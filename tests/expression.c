#include <stdlib.h>
#include <math.h>

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>
#include "utils.h"

static CpgExpression *expression;

static void
expression_initialize_context(gchar const *exp, CpgObject *context)
{
	expression = cpg_expression_new(exp);
	
	GSList *ctx = g_slist_append(NULL, context);

	gboolean ret = cpg_expression_compile(expression, ctx, NULL);
	g_slist_free(ctx);
	
	if (!ret)
	{
		fprintf(stderr, "Could not parse expression: %s\n", exp);
		exit(1);
	}
}

static void
expression_initialize(gchar const *exp)
{
	expression_initialize_context(exp, NULL);
}

static gdouble
expression_eval()
{
	return cpg_expression_evaluate(expression);
}

static gboolean
test_operator_plus()
{
	expression_initialize("3 + 4");
	assert(expression_eval(), (3 + 4));
	
	return TRUE;
}

static gboolean
test_operator_minus()
{
	expression_initialize("3 - 4");
	assert(expression_eval(), (3 - 4));
	
	return TRUE;
}

static gboolean
test_operator_minus_unary()
{
	expression_initialize("3 + -4");
	assert(expression_eval(), (3 + -4));
	
	return TRUE;
}

static gboolean
test_priority()
{
	expression_initialize("3 * 4 + 3");
	assert(expression_eval(), (3 * 4 + 3));
	
	expression_initialize("3 + 4 * 2");
	assert(expression_eval(), (3 + 4 * 2));
	
	return TRUE;
}

static gboolean
test_function_sin()
{
	expression_initialize("sin(pi)");
	assert(expression_eval(), sin(M_PI));
	
	return TRUE;
}

static gboolean
test_function_varargs()
{
	expression_initialize("max(0, 2 * pi)");
	assert(expression_eval(), 2 * M_PI);
	
	return TRUE;
}

static gboolean
test_complex()
{
	CpgObject *obj = cpg_object_new(NULL);
	CpgProperty *prop;
	
	prop = cpg_object_add_property(obj, "x", "1", 0);
	
	cpg_expression_compile(cpg_property_get_value_expression(prop), NULL, NULL);
	
	prop = cpg_object_add_property(obj, "phase", "2", 0);
	cpg_expression_compile(cpg_property_get_value_expression(prop), NULL, NULL);
	
	prop = cpg_object_add_property(obj, "y", "3", 0);
	cpg_expression_compile(cpg_property_get_value_expression(prop), NULL, NULL);
	
	cpg_object_reset(obj);

	expression_initialize_context("x * sin(phase) + 2 * y * PI", obj);
	assert(expression_eval(), 1 * sin(2) + 2 * 3 * M_PI);
	
	g_object_unref(obj);
	return TRUE;
}

static gboolean
text_scientific_notation()
{
	expression_initialize("1e-2");
	assert(expression_eval(), 1e-2);
	
	expression_initialize("1e+20");
	assert(expression_eval(), 1e+20);
	
	expression_initialize("1.25e-5");
	assert(expression_eval(), 1.25e-5);
	
	expression_initialize("10.2523e+4");
	assert(expression_eval(), 10.2523e+4);
	
	return TRUE;
}

typedef gboolean (*TestFunction)();

typedef struct
{
	TestFunction function;
	gchar const *name;
} Test;

static Test functions[] = {
	{test_operator_plus, "test_operator_plus"},
	{test_operator_minus, "test_operator_minus"},
	{test_operator_minus_unary, "test_operator_minus_unary"},
	{test_priority, "test_priority"},
	{test_function_sin, "test_function_sin"},
	{test_complex, "test_complex"},
	{test_function_varargs, "test_function_varargs"},
	{text_scientific_notation, "test_scientific_notation"}
};

gint 
main (gint argc, gchar *argv[])
{
	guint i;
	
	g_type_init();
	for (i = 0; i < sizeof(functions) / sizeof(Test); ++i)
	{
		if (functions[i].function())
			printf("Test `\e[;1m%s\e[0m' \e[32msuccessful\e[0m...\n", functions[i].name);
	}
	
	return 0;
}
