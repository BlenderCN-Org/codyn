/*
 * cdn-math.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cdn-math.h"

/**
 * SECTION:cdn-math
 * @short_description: Math function/operators
 *
 * Math expression helper functions.
 *
 */

typedef void (*FunctionClosure)(CdnStack *stack,
                                gint      numargs,
                                gint     *argdim);

#define foreach_element(op)							\
gint i;										\
gint num;									\
gint start;									\
										\
num = argdim ? (argdim[0] * argdim[1]) : 1;					\
start = cdn_stack_count (stack) - 1;						\
										\
for (i = 0; i < num; ++i)							\
{										\
	cdn_stack_set_at (stack, start, op (cdn_stack_at (stack, start)));	\
	--start;								\
}

typedef void (*BinaryFunction)(gdouble a, gdouble b);

static void
foreach_element2 (CdnStack       *stack,
                  gint           *argdim,
                  BinaryFunction  op)
{
	gint num1;
	gint num2;

	num1 = argdim ? (argdim[0] * argdim[1]) : 1:
	num2 = argdim ? (argdim[2] * argdim[3]) : 1;

	if (num1 == 1 && num2 == 1)
	{
		gdouble second = cdn_stack_pop (stack);
		cdn_stack_set (stack, op (cdn_stack_peek (stack), second));
	}
	else if (num1 == 1)
	{	
		gdouble first;
		gint start;
		gint i;

		start = cdn_stack_count (stack) - 1 - num2;
		first = cdn_stack_at (stack, start);

		for (i = 0; i < num2; ++i)
		{
			cdn_stack_set_at (stack, start, op (first, cdn_stack_at (stack, start + 1)));
			++start;
		}

		cdn_stack_pop (stack);
	}
	else if (num2 == 1)
	{
		gdouble second;
		gint start;
		gint i;

		second = cdn_stack_at (stack, start);
		start = cdn_stack_count (stack) - 2 - num1;

		for (i = 0; i < num1; ++i)
		{
			cdn_stack_set_at (stack, start, op (cdn_stack_at (stack, start), second));
			++start;
		}

		cdn_stack_pop (stack);
	}
	else
	{
		gint start = cdn_stack_count (stack) - 1 - num2;
		gint i;

		for (i = 0; i < num1; ++i)
		{
			gdouble second = cdn_stack_pop (stack);
			cdn_stack_set_at (stack, start, op (cdn_stack_at (stack, start), second));

			--start;
		}
	}
}

static void
op_sin (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element (sin);
}

static void
op_cos (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element (cos);
}

static void
op_tan (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element (tan);
}

static void
op_sqrt (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (sqrt);
}

static void
op_invsqrt (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	foreach_element (1.0 / sqrt);
}

static void
op_asin (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (asin);
}

static void
op_acos (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (acos);
}

static void
op_atan (CdnStack  *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (atan);
}

static void
op_atan2 (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, atan2);
}

static void
op_floor (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element (floor);
}

static void
op_ceil (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (ceil);
}

static void
op_round (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element (round);
}

static void
op_abs (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element (fabs);
}

static gdouble
min (gdouble a,
     gdouble b)
{
	return a < b ? a : b;
}

static gdouble
max (gdouble a,
     gdouble b)
{
	return a > b ? a : b;
}

static void
op_nested (CdnStack   *stack,
           gint        numargs,
           gdouble   (*func)(gdouble, gdouble))
{
	gdouble value = cdn_stack_pop (stack);
	gint i;

	for (i = 0; i < numargs - 1; ++i)
	{
		value = func (value, cdn_stack_pop (stack));
	}

	cdn_stack_push (stack, value);
}

static void
op_min (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	op_nested (stack, numargs, min);
}

static void
op_max (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	op_nested (stack, numargs, max);
}

static void
op_exp (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element (exp);
}

static void
op_pow (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element2 (stack, argdim, pow);
}

static void
op_ln (CdnStack *stack,
       gint      numargs,
       gint     *argdim)
{
	foreach_element (log);
}


static void
op_log10 (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element (log10);
}

static void
op_hypot (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, hypot);
}

static void
op_exp2 (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (exp2);
}

static void
op_sinh (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (sinh);
}

static void
op_cosh (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (cosh);
}

static void
op_tanh (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (tanh);
}

static void
op_lerp (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	gdouble third = cdn_stack_pop (stack);
	gdouble second = cdn_stack_pop (stack);
	gdouble first = cdn_stack_pop (stack);

	cdn_stack_push (stack, first + (second - first) * third);
}

static void
op_sqsum (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	gint i;
	gdouble value = 0;

	for (i = 0; i < numargs; ++i)
	{
		gdouble v = cdn_stack_pop (stack);
		value += v * v;
	}

	cdn_stack_push (stack, value);
}

static gdouble
sign_value (gdouble value)
{
	return signbit (value) ? -1 : 1;
}

static void
op_sign (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element (sign_value);
}

static void
op_csign (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, copysign);
}

static void
op_clip (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	gdouble max = cdn_stack_pop (stack);
	gdouble min = cdn_stack_pop (stack);
	gdouble val = cdn_stack_pop (stack);

	cdn_stack_push (stack, val < min ? min : (val > max ? max : val));
}

static void
op_cycle (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	gdouble max = cdn_stack_pop (stack);
	gdouble min = cdn_stack_pop (stack);
	gdouble val = cdn_stack_pop (stack);

	if (val > max)
	{
		cdn_stack_push (stack, min + fmod (val - min, max - min));
	}
	else if (val < min)
	{
		cdn_stack_push (stack, max - fmod (min - val, max - min));
	}
	else
	{
		cdn_stack_push (stack, val);
	}
}

static void
op_noop (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
}

/* operator functions */
static void
op_unary_minus (CdnStack *stack,
                gint      numargs,
                gint     *argdim)
{
	foreach_element (-1 * );
}

static gdouble
op_minus_impl (gdouble a, gdouble b)
{
	return a - b;
}

static void
op_minus (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, op_minus_impl);
}

static gdouble
op_plus_impl (gdouble a, gdouble b)
{
	return a + b;
}

static void
op_plus (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element2 (stack, argdim, op_plus_impl);
}

static gdouble
op_emultiply_impl (gdouble a, gdouble b)
{
	return a * b;
}

static void
op_multiply (CdnStack *stack,
             gint      numargs,
             gint     *argdim)
{
	if (!argdim || (argdim[0] == 1 && argdim[1] == 1 && argdim[2] == 1 && argdim[3] == 1))
	{
		gdouble second = cdn_stack_pop (stack);
		cdn_stack_set (stack, cdn_stack_peek (stack) * second);
	}
	else if (argdim[0] == argdim[2])
	{
		
	}
	else
	{
		foreach_element2 (stack, argdim, op_emultiply_impl);
	}
}

static void
op_emultiply (CdnStack *stack,
              gint      numargs,
              gint     *argdim)
{
	foreach_element2 (stack, argdim, op_emultiply_impl);
}

static gdouble
op_edivide_impl (gdouble a, gdouble b)
{
	return a / b;
}

static void
op_divide (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	foreach_element2 (stack, argdim, op_edivide_impl);
}

static void
op_edivide (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	foreach_element2 (stack, argdim, op_edivide_impl);
}

static gdouble
my_fmod (gdouble x,
         gdouble y)
{
	gdouble ans = fmod (x, y);

	return ans < 0 ? ans + y : ans;
}

static void
op_modulo (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	foreach_element2 (stack, argdim, my_fmod);
}

static gdouble
op_power_impl (gdouble a, gdouble b)
{
	return pow(a, b);
}

static void
op_power (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, op_power_impl);
}

static gdouble
op_greater_impl (gdouble a, gdouble b)
{
	return a > b;
}

static void
op_greater (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	foreach_element2 (stack, argdim, op_greater_impl);
}

static gdouble
op_less_impl (gdouble a, gdouble b)
{
	return a < b;
}

static void
op_less (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	foreach_element2 (stack, argdim, op_less_impl);
}

static gdouble
op_greater_or_equal_impl (gdouble a, gdouble b)
{
	return a >= b;
}

static void
op_greater_or_equal (CdnStack *stack,
                     gint      numargs,
                     gint     *argdim)
{
	foreach_element2 (stack, argdim, op_greater_or_equal_impl);
}

static gdouble
op_less_or_equal_impl (gdouble a, gdouble b)
{
	return a <= b;
}

static void
op_less_or_equal (CdnStack *stack,
                  gint      numargs,
                  gint     *argdim)
{
	foreach_element2 (stack, argdim, op_less_or_equal_impl);
}

static gdouble
op_equal_impl (gdouble a, gdouble b)
{
	return fabs (a - b) < 10e-12;
}

static void
op_equal (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, op_equal_impl);
}

static gdouble
op_or_impl (gdouble a, gdouble b)
{
	return !op_equal_impl (a, 0) || !op_equal_impl (b, 0);
}

static void
op_or (CdnStack *stack,
       gint      numargs,
       gint     *argdim)
{
	foreach_element2 (stack, argdim, op_or_impl);
}

static gdouble
op_and_impl (gdouble a, gdouble b)
{
	return !op_equal_impl (a, 0) && !op_equal_impl (b, 0);
}

static void
op_and (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	foreach_element2 (stack, argdim, op_and_impl);
}

static gdouble
op_negate_impl (gdouble a)
{
	return op_equal_impl (a, 0) ? 1 : 0;
}

static void
op_negate (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	foreach_element (op_negate_impl);
}

static void
op_ternary (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	gdouble falsepart = cdn_stack_pop (stack);
	gdouble truepart = cdn_stack_pop (stack);
	gdouble condition = cdn_stack_pop (stack);

	cdn_stack_push (stack, condition ? truepart : falsepart);
}

typedef struct
{
	gchar const *name;
	FunctionClosure function;
	gint arguments;
	gboolean commutative;
} FunctionEntry;

static FunctionEntry function_entries[] = {
	{NULL, op_noop, 0, FALSE},
	{"--", op_unary_minus, 1, FALSE},
	{"-", op_minus, 2, FALSE},
	{"+", op_plus, 2, TRUE},
	{"*", op_multiply, 2, TRUE},
	{".*", op_emultiply, 2, TRUE},
	{"/", op_divide, 2, FALSE},
	{"./", op_edivide, 2, FALSE},
	{"%", op_modulo, 2, FALSE},
	{"^", op_power, 2, FALSE},
	{">", op_greater, 2, FALSE},
	{"<", op_less, 2, FALSE},
	{">=", op_greater_or_equal, 2, FALSE},
	{"<=", op_less_or_equal, 2, FALSE},
	{"==", op_equal, 2, TRUE},
	{"||", op_or, 2, TRUE},
	{"&&", op_and, 2, TRUE},
	{"!", op_negate, 1, FALSE},
	{"?:", op_ternary, 3, FALSE},
	{"sin", op_sin, 1, FALSE},
	{"cos", op_cos, 1, FALSE},
	{"tan", op_tan, 1, FALSE},
	{"asin", op_asin, 1, FALSE},
	{"acos", op_acos, 1, FALSE},
	{"atan", op_atan, 1, FALSE},
	{"atan2", op_atan2, 2, FALSE},
	{"sqrt", op_sqrt, 1, FALSE},
	{"invsqrt", op_invsqrt, 1, FALSE},
	{"min", op_min, -1, TRUE},
	{"max", op_max, -1, TRUE},
	{"exp", op_exp, 1, FALSE},
	{"floor", op_floor, 1, FALSE},
	{"ceil", op_ceil, 1, FALSE},
	{"round", op_round, 1, FALSE},
	{"abs", op_abs, 1, FALSE},
	{"pow", op_pow, 2, FALSE},
	{"ln", op_ln, 1, FALSE},
	{"log10", op_log10, 1, FALSE},
	{"hypot", op_hypot, 2, TRUE},
	{"exp2", op_exp2, 1, FALSE},
	{"sinh", op_sinh, 1, FALSE},
	{"cosh", op_cosh, 1, FALSE},
	{"tanh", op_tanh, 1, FALSE},
	{"lerp", op_lerp, 3, FALSE},
	{"sqsum", op_sqsum, -1, TRUE},
	{"sign", op_sign, 1, FALSE},
	{"csign", op_csign, 2, FALSE},
	{"clip", op_clip, 3, FALSE},
	{"cycle", op_cycle, 3, FALSE}
};

/**
 * cdn_math_function_lookup_by_id:
 * @type: A #CdnMathFunctionType
 * @arguments: (out): return value for the number of arguments
 *
 * Lookup the name of a function by its id.
 *
 * Returns: the name of the function, or %NULL if the function could not be
 *          found
 *
 **/
gchar const *
cdn_math_function_lookup_by_id (CdnMathFunctionType  type,
                                gint                *arguments)
{
	*arguments = function_entries[type].arguments;
	return function_entries[type].name;
}

/**
 * cdn_math_function_lookup:
 * @name: The function name
 * @arguments: (out): The number of arguments
 *
 * Lookup a math function given the name @name and number of arguments.
 *
 * Returns: A #CdnMathFunctionType
 *
 **/
CdnMathFunctionType
cdn_math_function_lookup (gchar const  *name,
                          gint         *arguments)
{
	guint i;

	for (i = 1; i < CDN_MATH_FUNCTION_TYPE_NUM; ++i)
	{
		if (strcmp (function_entries[i].name, name) == 0)
		{
			*arguments = function_entries[i].arguments;
			return i;
		}
	}

	return 0;
}

/**
 * cdn_math_function_is_variable:
 * @type: A #CdnMathFunctionType
 *
 * Get whether the math function accepts a variable number of arguments.
 *
 * Returns: %TRUE if the function accepts a variable number of arguments,
 *          %FALSE otherwise
 *
 **/
gboolean
cdn_math_function_is_variable (CdnMathFunctionType type)
{
	return function_entries[type].arguments == -1;
}

/**
 * cdn_math_function_is_commutative:
 * @type: A #CdnMathOperatorType
 *
 * Get whether an function is commutative.
 *
 * Returns: %TRUE if the function is commutative, %FALSE otherwise
 *
 **/
gboolean
cdn_math_function_is_commutative (CdnMathFunctionType type)
{
	return function_entries[type].commutative;
}

/**
 * cdn_math_function_execute:
 * @type: A #CdnMathFunctionType
 * @stack: A #CdnStack
 *
 * Execute a math function on the stack.
 *
 **/
void
cdn_math_function_execute (CdnMathFunctionType  type,
                           gint                 numargs,
                           gint                *argdim,
                           CdnStack            *stack)
{
	function_entries[type].function (stack, numargs, argdim);
}

typedef struct
{
	gchar const *name;
	gdouble (*function)(void);
	gdouble value;
} CdnConstantEntry;

static CdnConstantEntry constant_entries[] = {
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

/**
 * cdn_math_constant_lookup:
 * @name: The name of the constant
 * @found: Return value whether or not the constant could be found
 *
 * Get the value of a constant. Valid constants are: pi, PI, e, E, NAN, nan,
 * NaN, Inf, INF, inf.
 *
 * Returns: the value of a constant
 *
 **/
gdouble
cdn_math_constant_lookup (gchar const  *name,
                          gboolean     *found)
{
	guint i;

	for (i = 0; i < sizeof (constant_entries) / sizeof (CdnConstantEntry); ++i)
	{
		if (g_strcmp0 (constant_entries[i].name, name) == 0)
		{
			if (found)
			{
				*found = TRUE;
			}

			if (constant_entries[i].function)
			{
				return constant_entries[i].function ();
			}
			else
			{
				return constant_entries[i].value;
			}
		}
	}

	if (found)
	{
		*found = FALSE;
	}

	return 0.0;
}
