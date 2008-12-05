#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cpg-math.h"

typedef void (*FunctionClosure)(CpgStack *, void *);

static void
op_sin(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, sin(cpg_stack_pop(stack, data)), data);
}

static void
op_cos(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, cos(cpg_stack_pop(stack, data)), data);
}

static void
op_tan(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, tan(cpg_stack_pop(stack, data)), data);
}

static void
op_sqrt(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, sqrt(cpg_stack_pop(stack, data)), data);
}

static void
op_asin(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, asin(cpg_stack_pop(stack, data)), data);
}

static void
op_acos(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, acos(cpg_stack_pop(stack, data)), data);
}

static void
op_atan(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, atan(cpg_stack_pop(stack, data)), data);
}

static void
op_floor(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, floor(cpg_stack_pop(stack, data)), data);
}

static void
op_ceil(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, ceil(cpg_stack_pop(stack, data)), data);
}

static void
op_round(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, round(cpg_stack_pop(stack, data)), data);
}

static void
op_abs(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, abs(cpg_stack_pop(stack, data)), data);
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
op_nested(CpgStack *stack, double(*func)(double, double), void *data)
{
	unsigned nargs = (unsigned)cpg_stack_pop(stack, data);
	double value = cpg_stack_pop(stack, data);
	unsigned i;
	
	for (i = 0; i < nargs - 1; ++i)
		value = func(value, cpg_stack_pop(stack, data));
	
	cpg_stack_push(stack, value, data);	
}

static void
op_min(CpgStack *stack, void *data)
{
	op_nested(stack, min, data);
}

static void
op_max(CpgStack *stack, void *data)
{
	op_nested(stack, max, data);
}

static void
op_exp(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, exp(cpg_stack_pop(stack, data)), data);
}

static void
op_pow(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, pow(first, second), data);
}

static void
op_noop(CpgStack *stack, void *data)
{
}

typedef struct
{
	char const *name;
	FunctionClosure function;
	int arguments;
} FunctionEntry;

static FunctionEntry function_entries[] = {
	{NULL, op_noop, 0},
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

unsigned
cpg_math_function_lookup(char const *name, int *arguments)
{
	unsigned i;
	
	for (i = 1; i < sizeof(function_entries) / sizeof(FunctionEntry); ++i)
	{
		if (strcmp(function_entries[i].name, name) == 0)
		{
			*arguments = function_entries[i].arguments;
			return i;
		}
	}
	
	return 0;
}

void 
cpg_math_function_execute(unsigned id, CpgStack *stack, void *data)
{
	function_entries[id].function(stack, data);
}

/* operator functions */
static void
op_unary_minus(CpgStack *stack, void *data)
{
	cpg_stack_push(stack, -1 * cpg_stack_pop(stack, data), data);
}

static void
op_minus(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first - second, data);
}

static void
op_plus(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first + second, data);
}

static void
op_multiply(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first * second, data);
}

static void
op_divide(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, second == 0.0 ? 0.0 : first / second, data);
}

static double
my_fmod (double x, double y)
{
	double ans = fmod (x, y);
	return ans < 0 ? ans + y : ans;
}

static void
op_modulo(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, my_fmod(first, second), data);
}

static void
op_power(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, pow(first, second), data);
}

static void
op_greater(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first > second, data);
}

static void
op_less(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first < second, data);
}

static void
op_greater_or_equal(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first >= second, data);
}

static void
op_less_or_equal(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first < second, data);
}

static void
op_equal(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first == second, data);
}

static void
op_or(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first || second, data);
}

static void
op_and(CpgStack *stack, void *data)
{
	double second = cpg_stack_pop(stack, data);
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, first && second, data);
}

static void
op_negate(CpgStack *stack, void *data)
{
	double first = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, !first, data);
}

static void
op_ternary(CpgStack *stack, void *data)
{
	double falsepart = cpg_stack_pop(stack, data);
	double truepart = cpg_stack_pop(stack, data);
	double condition = cpg_stack_pop(stack, data);
	
	cpg_stack_push(stack, condition ? truepart : falsepart, data);
}

typedef struct
{
	char const *name;
	FunctionClosure function;
	int arguments;
} OperatorEntry;

static OperatorEntry operator_entries[] = {
	{NULL, op_noop, 0},
	{"--", op_unary_minus, 1},
	{"-", op_minus, 2},
	{"+", op_plus, 2},
	{"*", op_multiply, 2},
	{"/", op_divide, 2},
	{"%", op_modulo, 2},
	{"^", op_power, 2},
	{">", op_greater, 2},
	{"<", op_less, 2},
	{">=", op_greater_or_equal, 2},
	{"<=", op_less_or_equal, 2},
	{"==", op_equal, 2},
	{"||", op_or, 2},
	{"&&", op_and, 2},
	{"!", op_negate, 1},
	{"?:", op_ternary, 3}
};

unsigned
cpg_math_operator_lookup(CpgMathOperatorType type)
{
	return type;
}

void
cpg_math_operator_execute(unsigned id, CpgStack *stack, void *data)
{
	operator_entries[id].function(stack, data);
}

typedef struct
{
	char const *name;
	double (*function)(void);
	double value;
} CpgConstantEntry;

static CpgConstantEntry constant_entries[] = {
	{"pi", NULL, M_PI},
	{"PI", NULL, M_PI},
	{"e", NULL, M_E},
	{"E", NULL, M_E}
};

double
cpg_math_constant_lookup(char const *name, int *found)
{
	unsigned i;
	
	for (i = 0; i < sizeof(constant_entries) / sizeof(CpgConstantEntry); ++i)
	{
		if (strcmp(constant_entries[i].name, name) == 0)
		{
			*found = 1;

			if (constant_entries[i].function)
				return constant_entries[i].function();
			else
				return constant_entries[i].value;
		}
	}

	*found = 0;	
	return 0.0;
}
