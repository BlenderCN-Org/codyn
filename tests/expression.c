#include <stdlib.h>
#include <math.h>

#include "cpg-expression.h"
#include "utils.h"

static CpgExpression *expression;

static void
expression_initialize_context(char const *exp, CpgObject *context)
{
	expression = cpg_expression_new(exp);
	
	char *error;
	CpgContext ctx = {context, NULL};

	if (!cpg_expression_compile(expression, &ctx, &error))
	{
		fprintf(stderr, "Could not parse expression: %s\n", error);
		exit(1);
	}
}

static void
expression_initialize(char const *exp)
{
	expression_initialize_context(exp, NULL);
}

static double
expression_eval()
{
	return cpg_expression_evaluate(expression);
}

static int
test_operator_plus()
{
	expression_initialize("3 + 4");
	assert(expression_eval(), (3 + 4));
	
	return 1;
}

static int
test_operator_minus()
{
	expression_initialize("3 - 4");
	assert(expression_eval(), (3 - 4));
	
	return 1;
}

static int
test_operator_minus_unary()
{
	expression_initialize("3 + -4");
	assert(expression_eval(), (3 + -4));
	
	return 1;
}

static int
test_priority()
{
	expression_initialize("3 * 4 + 3");
	assert(expression_eval(), (3 * 4 + 3));
	
	expression_initialize("3 + 4 * 2");
	assert(expression_eval(), (3 + 4 * 2));
	
	return 1;
}

static int
test_function_sin()
{
	expression_initialize("sin(pi)");
	assert(expression_eval(), sin(M_PI));
	
	return 1;
}

static int
test_complex()
{
	CpgObject *obj = cpg_object_new();
	
	/*cpg_object_add_property(obj, "x", "1", 0);
	cpg_expression_compile(obj->properties[0]->initial, obj, NULL);
	
	cpg_object_add_property(obj,"phase", "2", 0);
	cpg_expression_compile(obj->properties[1]->initial, obj, NULL);
	
	cpg_object_add_property(obj, "y", "3", 0);
	cpg_expression_parse(obj->properties[2]->initial, obj, NULL);
	
	cpg_object_reset(obj);

	expression_initialize_context("x * sin(phase) + 2 * y * PI", obj);
	assert(expression_eval(), 1 * sin(2) + 2 * 3 * M_PI);*/
	
	return 1;
}

typedef int (*TestFunction)();

typedef struct
{
	TestFunction function;
	char const *name;
} Test;

static Test functions[] = {
	{test_operator_plus, "test_operator_plus"},
	{test_operator_minus, "test_operator_minus"},
	{test_operator_minus_unary, "test_operator_minus_unary"},
	{test_priority, "test_priority"},
	{test_function_sin, "test_function_sin"},
	{test_complex, "test_complex"}
};

int 
main (int argc, char *argv[])
{
	unsigned i;
	
	for (i = 0; i < sizeof(functions) / sizeof(Test); ++i)
	{
		if (functions[i].function())
			printf("Test `\e[;1m%s\e[0m' \e[32msuccessful\e[0m...\n", functions[i].name);
	}
	
	return 0;
}
