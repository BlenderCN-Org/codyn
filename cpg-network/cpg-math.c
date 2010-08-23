/*
 * cpg-math.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cpg-math.h"

#define RAND(A, B)  ((A) + rand() * 1.0 / RAND_MAX * ((B) - (A)))

typedef void (*FunctionClosure)(CpgStack *);

static void
op_sin (CpgStack *stack)
{
	cpg_stack_push (stack, sin (cpg_stack_pop (stack)));
}

static void
op_cos (CpgStack *stack)
{
	cpg_stack_push (stack, cos (cpg_stack_pop (stack)));
}

static void
op_tan (CpgStack *stack)
{
	cpg_stack_push (stack, tan (cpg_stack_pop (stack)));
}

static void
op_sqrt (CpgStack *stack)
{
	cpg_stack_push (stack, sqrt (cpg_stack_pop (stack)));
}

static void
op_invsqrt (CpgStack *stack)
{
	cpg_stack_push (stack, 1.0 / sqrt (cpg_stack_pop (stack)));
}

static void
op_asin (CpgStack *stack)
{
	cpg_stack_push (stack, asin (cpg_stack_pop (stack)));
}

static void
op_acos (CpgStack *stack)
{
	cpg_stack_push (stack, acos (cpg_stack_pop (stack)));
}

static void
op_atan (CpgStack  *stack)
{
	cpg_stack_push (stack, atan (cpg_stack_pop (stack)));
}

static void
op_atan2 (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, atan2 (first, second));
}

static void
op_floor (CpgStack *stack)
{
	cpg_stack_push (stack, floor (cpg_stack_pop (stack)));
}

static void
op_ceil (CpgStack *stack)
{
	cpg_stack_push (stack, ceil (cpg_stack_pop (stack)));
}

static void
op_round (CpgStack *stack)
{
	cpg_stack_push (stack, round (cpg_stack_pop (stack)));
}

static void
op_abs (CpgStack *stack)
{
	cpg_stack_push (stack, fabs (cpg_stack_pop (stack)));
}

static double
min (double a,
     double b)
{
	return a < b ? a : b;
}

static double
max (double a,
     double b)
{
	return a > b ? a : b;
}

static void
op_nested (CpgStack  *stack, 
           double   (*func)(double, double))
{
	unsigned nargs = (unsigned)cpg_stack_pop (stack);
	double value = cpg_stack_pop (stack);
	unsigned i;
	
	for (i = 0; i < nargs - 1; ++i)
		value = func (value, cpg_stack_pop (stack));
	
	cpg_stack_push (stack, value);
}

static void
op_min (CpgStack *stack)
{
	op_nested (stack, min);
}

static void
op_max (CpgStack *stack)
{
	op_nested (stack, max);
}

static void
op_exp (CpgStack *stack)
{
	cpg_stack_push (stack, exp (cpg_stack_pop (stack)));
}

static void
op_pow (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, pow (first, second));
}

static void
op_log (CpgStack *stack)
{
	cpg_stack_push (stack, log (cpg_stack_pop (stack)));
}

static void
op_rand (CpgStack *stack)
{
	unsigned nargs = (unsigned)cpg_stack_pop (stack);
	double value;
	
	if (nargs == 0)
	{
		value = RAND (0, 1);
	}
	else if (nargs == 2)
	{
		double second = cpg_stack_pop (stack);
		double first = cpg_stack_pop (stack);

		value = RAND (first, second);
	}
	else
	{
		int i;
		
		for (i = 0; i < nargs - 1; ++i)
		{
			cpg_stack_pop (stack);
		}
		
		double first = cpg_stack_pop (stack);
		value = RAND (0, first);
	}

	cpg_stack_push (stack, value);
}

static void
op_hypot (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, hypot (first, second));
}

static void
op_exp2 (CpgStack *stack)
{
	cpg_stack_push (stack, exp2 (cpg_stack_pop (stack)));
}

static void
op_sinh (CpgStack *stack)
{
	cpg_stack_push (stack, sinh (cpg_stack_pop (stack)));
}

static void
op_cosh (CpgStack *stack)
{
	cpg_stack_push (stack, cosh (cpg_stack_pop (stack)));
}

static void
op_tanh (CpgStack *stack)
{
	cpg_stack_push (stack, tanh (cpg_stack_pop (stack)));
}

static void
op_lerp (CpgStack *stack)
{
	double third = cpg_stack_pop (stack);
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first + (second - first) * third);
}

static void
op_sqsum (CpgStack *stack)
{
	unsigned nargs = (unsigned)cpg_stack_pop (stack);
	unsigned i;
	double value = 0;

	for (i = 0; i < nargs; ++i)
	{
		double v = cpg_stack_pop (stack);
		value += v * v;
	}

	cpg_stack_push (stack, value);
}

static void
op_noop (CpgStack *stack)
{
}

typedef struct
{
	gchar const *name;
	FunctionClosure function;
	gint arguments;
	gboolean constant;
} FunctionEntry;

static FunctionEntry function_entries[] = {
	{NULL, op_noop, 0, TRUE},
	{"sin", op_sin, 1, TRUE},
	{"cos", op_cos, 1, TRUE},
	{"tan", op_tan, 1, TRUE},
	{"asin", op_asin, 1, TRUE},
	{"acos", op_acos, 1, TRUE},
	{"atan", op_atan, 1, TRUE},
	{"atan2", op_atan2, 2, TRUE},
	{"sqrt", op_sqrt, 1, TRUE},
	{"invsqrt", op_invsqrt, 1, TRUE},
	{"min", op_min, -1, TRUE},
	{"max", op_max, -1, TRUE},
	{"exp", op_exp, 1, TRUE},
	{"floor", op_floor, 1, TRUE},
	{"ceil", op_ceil, 1, TRUE},
	{"round", op_round, 1, TRUE},
	{"abs", op_abs, 1, TRUE},
	{"pow", op_pow, 2, TRUE},
	{"rand", op_rand, -1, FALSE},
	{"ln", op_log, 1, TRUE},
	{"log", op_log, 1, TRUE},
	{"hypot", op_hypot, 2, TRUE},
	{"exp2", op_exp2, 1, TRUE},
	{"sinh", op_sinh, 1, TRUE},
	{"cosh", op_cosh, 1, TRUE},
	{"tanh", op_tanh, 1, TRUE},
	{"lerp", op_lerp, 3, TRUE},
	{"sqsum", op_sqsum, -1, TRUE}
};

gchar const *
cpg_math_function_lookup_by_id (CpgMathFunctionType  id,
                                gint                *arguments) 
{
	*arguments = function_entries[id].arguments;
	return function_entries[id].name;
}

CpgMathFunctionType
cpg_math_function_lookup (gchar const  *name,
                          gint         *arguments)
{
	guint i;
	
	for (i = 1; i < CPG_FUNCTION_OPERATOR_NUM; ++i)
	{
		if (strcmp (function_entries[i].name, name) == 0)
		{
			*arguments = function_entries[i].arguments;
			return i;
		}
	}
	
	return 0;
}

gboolean
cpg_math_function_is_constant (CpgMathFunctionType type)
{
	return function_entries[type].constant;
}

gboolean
cpg_math_function_is_variable (CpgMathFunctionType type)
{
	return function_entries[type].arguments == -1;
}

void
cpg_math_function_execute (CpgMathFunctionType  id,
                           CpgStack            *stack)
{
	function_entries[id].function (stack);
}

/* operator functions */
static void
op_unary_minus (CpgStack *stack)
{
	cpg_stack_push (stack, -1 * cpg_stack_pop (stack));
}

static void
op_minus (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first - second);
}

static void
op_plus (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first + second);
}

static void
op_multiply (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first * second);
}

static void
op_divide (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, second == 0.0 ? 0.0 : first / second);
}

static double
my_fmod (double x,
         double y)
{
	double ans = fmod (x, y);
	return ans < 0 ? ans + y : ans;
}

static void
op_modulo (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, my_fmod (first, second));
}

static void
op_power (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, pow (first, second));
}

static void
op_greater (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first > second);
}

static void
op_less (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first < second);
}

static void
op_greater_or_equal (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first >= second);
}

static void
op_less_or_equal (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first < second);
}

static void
op_equal (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first == second);
}

static void
op_or (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first || second);
}

static void
op_and (CpgStack *stack)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, first && second);
}

static void
op_negate (CpgStack *stack)
{
	double first = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, !first);
}

static void
op_ternary (CpgStack *stack)
{
	double falsepart = cpg_stack_pop (stack);
	double truepart = cpg_stack_pop (stack);
	double condition = cpg_stack_pop (stack);
	
	cpg_stack_push (stack, condition ? truepart : falsepart);
}

typedef struct
{
	gchar const *name;
	FunctionClosure function;
	gint arguments;
	gboolean constant;
} OperatorEntry;

static OperatorEntry operator_entries[] = {
	{NULL, op_noop, 0, TRUE},
	{"--", op_unary_minus, 1, TRUE},
	{"-", op_minus, 2, TRUE},
	{"+", op_plus, 2, TRUE},
	{"*", op_multiply, 2, TRUE},
	{"/", op_divide, 2, TRUE},
	{"%", op_modulo, 2, TRUE},
	{"^", op_power, 2, TRUE},
	{">", op_greater, 2, TRUE},
	{"<", op_less, 2, TRUE},
	{">=", op_greater_or_equal, 2, TRUE},
	{"<=", op_less_or_equal, 2, TRUE},
	{"==", op_equal, 2, TRUE},
	{"||", op_or, 2, TRUE},
	{"&&", op_and, 2, TRUE},
	{"!", op_negate, 1, TRUE},
	{"?:", op_ternary, 3, TRUE}
};

CpgMathOperatorType
cpg_math_operator_lookup (CpgMathOperatorType type)
{
	return type;
}

void
cpg_math_operator_execute (CpgMathOperatorType  id,
                           CpgStack            *stack)
{
	operator_entries[id].function (stack);
}

gboolean
cpg_math_operator_is_constant (CpgMathOperatorType type)
{
	return operator_entries[type].constant;
}

gboolean
cpg_math_operator_is_variable (CpgMathOperatorType type)
{
	return operator_entries[type].arguments == -1;
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
	{"E", NULL, M_E},
	{"NAN", NULL, 0},
	{"nan", NULL, 0},
	{"NaN", NULL, 0},
	{"Inf", NULL, 0},
	{"INF", NULL, 0},
	{"inf", NULL, 0}
};

gdouble
cpg_math_constant_lookup (gchar const  *name,
                          gboolean     *found)
{
	guint i;
	
	for (i = 0; i < sizeof (constant_entries) / sizeof (CpgConstantEntry); ++i)
	{
		if (g_strcmp0 (constant_entries[i].name, name) == 0)
		{
			*found = TRUE;

			if (constant_entries[i].function)
				return constant_entries[i].function ();
			else
				return constant_entries[i].value;
		}
	}

	*found = FALSE;	
	return 0.0;
}
