#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cpg-math.h"
#include "cpg-expression.h"

typedef struct
{
	char const *name;
	CpgFunctionClosure closure;
	int arguments;
} FunctionEntry;

static void
op_sin(CpgExpression *expression)
{
	cpg_expression_push(expression, sin(cpg_expression_pop(expression)));
}

static void
op_cos(CpgExpression *expression)
{
	cpg_expression_push(expression, cos(cpg_expression_pop(expression)));
}

static void
op_tan(CpgExpression *expression)
{
	cpg_expression_push(expression, tan(cpg_expression_pop(expression)));
}

static void
op_sqrt(CpgExpression *expression)
{
	cpg_expression_push(expression, sqrt(cpg_expression_pop(expression)));
}

static void
op_asin(CpgExpression *expression)
{
	cpg_expression_push(expression, asin(cpg_expression_pop(expression)));
}

static void
op_acos(CpgExpression *expression)
{
	cpg_expression_push(expression, acos(cpg_expression_pop(expression)));
}

static void
op_atan(CpgExpression *expression)
{
	cpg_expression_push(expression, atan(cpg_expression_pop(expression)));
}

static void
op_floor(CpgExpression *expression)
{
	cpg_expression_push(expression, floor(cpg_expression_pop(expression)));
}

static void
op_ceil(CpgExpression *expression)
{
	cpg_expression_push(expression, ceil(cpg_expression_pop(expression)));
}

static void
op_round(CpgExpression *expression)
{
	cpg_expression_push(expression, round(cpg_expression_pop(expression)));
}

static void
op_abs(CpgExpression *expression)
{
	cpg_expression_push(expression, abs(cpg_expression_pop(expression)));
}

static double
min(double a, double b)
{
	return a < b ? a : b;
}

static double
max(double a, double b)
{
	return a > b ? a : b;
}

static void
op_nested(CpgExpression *expression, double(*func)(double, double))
{
	unsigned nargs = (unsigned)cpg_expression_pop(expression);
	double value = cpg_expression_pop(expression);
	unsigned i;
	
	for (i = 0; i < nargs - 1; ++i)
		value = func(value, cpg_expression_pop(expression));
	
	cpg_expression_push(expression, value);	
}

static void
op_min(CpgExpression *expression)
{
	op_nested(expression, min);
}

static void
op_max(CpgExpression *expression)
{
	op_nested(expression, max);
}

static void
op_exp(CpgExpression *expression)
{
	cpg_expression_push(expression, exp(cpg_expression_pop(expression)));
}

static void
op_pow(CpgExpression *expression)
{
	double second = cpg_expression_pop(expression);
	double first = cpg_expression_pop(expression);
	
	cpg_expression_push(expression, pow(first, second));
}

static FunctionEntry function_entries[] = {
	{"sin", op_sin, 1},
	{"cos", op_cos, 1},
	{"tan", op_tan, 1},
	{"asin", op_asin, 1},
	{"acos", op_acos, 1},
	{"atan", op_atan, 1},
	{"sqrt", op_sqrt, 1},
	{"min", op_min, -1},
	{"max", op_max, -1},
	{"exp", op_exp, 1},
	{"floor", op_floor, 1},
	{"ceil", op_ceil, 1},
	{"round", op_round, 1},
	{"abs", op_abs, 1},
	{"pow", op_pow, 2}
};

CpgFunctionClosure
cpg_math_function_lookup(char const *name, int *arguments)
{
	unsigned i;
	
	for (i = 0; i < sizeof(function_entries) / sizeof(FunctionEntry); ++i)
	{
		if (strcmp(function_entries[i].name, name) == 0)
		{
			*arguments = function_entries[i].arguments;
			return function_entries[i].closure;
		}
	}
	
	return NULL;
}

static void
op_time(CpgExpression *expression)
{
	// TODO
	cpg_expression_push(expression, 0.0);
}

static void
op_timestep(CpgExpression *expression)
{
	// TODO
	cpg_expression_push(expression, 0.0);
}

static CpgConstantEntry constant_entries[] = {
	{"pi", NULL, M_PI},
	{"PI", NULL, M_PI},
	{"e", NULL, M_E},
	{"E", NULL, M_E}
};

CpgConstantEntry const *
cpg_math_constant_lookup(char const *name)
{
	unsigned i;
	
	for (i = 0; i < sizeof(constant_entries) / sizeof(CpgConstantEntry); ++i)
	{
		if (strcmp(constant_entries[i].name, name) == 0)
			return &(constant_entries[i]);
	}
	
	return NULL;
}
