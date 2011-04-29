#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "cpg-math.h"

/**
 * SECTION:cpg-math
 * @short_description: Math function/operators
 *
 * Math expression helper functions.
 *
 */

#define RAND(A, B)  ((A) + rand() * 1.0 / RAND_MAX * ((B) - (A)))

typedef void (*FunctionClosure)(CpgStack *, gint numargs);

static void
op_sin (CpgStack *stack,
        gint      numargs)
{
	cpg_stack_push (stack, sin (cpg_stack_pop (stack)));
}

static void
op_cos (CpgStack *stack,
        gint      numargs)
{
	cpg_stack_push (stack, cos (cpg_stack_pop (stack)));
}

static void
op_tan (CpgStack *stack,
        gint      numargs)
{
	cpg_stack_push (stack, tan (cpg_stack_pop (stack)));
}

static void
op_sqrt (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, sqrt (cpg_stack_pop (stack)));
}

static void
op_invsqrt (CpgStack *stack,
            gint      numargs)
{
	cpg_stack_push (stack, 1.0 / sqrt (cpg_stack_pop (stack)));
}

static void
op_asin (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, asin (cpg_stack_pop (stack)));
}

static void
op_acos (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, acos (cpg_stack_pop (stack)));
}

static void
op_atan (CpgStack  *stack,
         gint      numargs)
{
	cpg_stack_push (stack, atan (cpg_stack_pop (stack)));
}

static void
op_atan2 (CpgStack *stack,
          gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, atan2 (first, second));
}

static void
op_floor (CpgStack *stack,
          gint      numargs)
{
	cpg_stack_push (stack, floor (cpg_stack_pop (stack)));
}

static void
op_ceil (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, ceil (cpg_stack_pop (stack)));
}

static void
op_round (CpgStack *stack,
          gint      numargs)
{
	cpg_stack_push (stack, round (cpg_stack_pop (stack)));
}

static void
op_abs (CpgStack *stack,
        gint      numargs)
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
           gint       numargs,
           double   (*func)(double, double))
{
	double value = cpg_stack_pop (stack);
	gint i;

	for (i = 0; i < numargs - 1; ++i)
	{
		value = func (value, cpg_stack_pop (stack));
	}

	cpg_stack_push (stack, value);
}

static void
op_min (CpgStack *stack,
        gint      numargs)
{
	op_nested (stack, numargs, min);
}

static void
op_max (CpgStack *stack,
        gint      numargs)
{
	op_nested (stack, numargs, max);
}

static void
op_exp (CpgStack *stack,
        gint      numargs)
{
	cpg_stack_push (stack, exp (cpg_stack_pop (stack)));
}

static void
op_pow (CpgStack *stack,
        gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, pow (first, second));
}

static void
op_ln (CpgStack *stack,
       gint      numargs)
{
	cpg_stack_push (stack, log (cpg_stack_pop (stack)));
}


static void
op_log10 (CpgStack *stack,
          gint      numargs)
{
	cpg_stack_push (stack, log10 (cpg_stack_pop (stack)));
}

static void
op_rand (CpgStack *stack,
         gint      numargs)
{
	double value;

	if (numargs == 0)
	{
		value = RAND (0, 1);
	}
	else if (numargs == 2)
	{
		double second = cpg_stack_pop (stack);
		double first = cpg_stack_pop (stack);

		value = RAND (first, second);
	}
	else
	{
		gint i;

		for (i = 0; i < numargs - 1; ++i)
		{
			cpg_stack_pop (stack);
		}

		double first = cpg_stack_pop (stack);
		value = RAND (0, first);
	}

	cpg_stack_push (stack, value);
}

static void
op_hypot (CpgStack *stack,
          gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, hypot (first, second));
}

static void
op_exp2 (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, exp2 (cpg_stack_pop (stack)));
}

static void
op_sinh (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, sinh (cpg_stack_pop (stack)));
}

static void
op_cosh (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, cosh (cpg_stack_pop (stack)));
}

static void
op_tanh (CpgStack *stack,
         gint      numargs)
{
	cpg_stack_push (stack, tanh (cpg_stack_pop (stack)));
}

static void
op_lerp (CpgStack *stack,
         gint      numargs)
{
	double third = cpg_stack_pop (stack);
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first + (second - first) * third);
}

static void
op_sqsum (CpgStack *stack,
          gint      numargs)
{
	gint i;
	double value = 0;

	for (i = 0; i < numargs; ++i)
	{
		double v = cpg_stack_pop (stack);
		value += v * v;
	}

	cpg_stack_push (stack, value);
}

static void
op_noop (CpgStack *stack,
         gint      numargs)
{
}

typedef struct
{
	gchar const *name;
	FunctionClosure function;
	gint arguments;
	gboolean constant;
	gboolean commutative;
} FunctionEntry;

static FunctionEntry function_entries[] = {
	{NULL, op_noop, 0, TRUE, FALSE},
	{"sin", op_sin, 1, TRUE, FALSE},
	{"cos", op_cos, 1, TRUE, FALSE},
	{"tan", op_tan, 1, TRUE, FALSE},
	{"asin", op_asin, 1, TRUE, FALSE},
	{"acos", op_acos, 1, TRUE, FALSE},
	{"atan", op_atan, 1, TRUE, FALSE},
	{"atan2", op_atan2, 2, TRUE, FALSE},
	{"sqrt", op_sqrt, 1, TRUE, FALSE},
	{"invsqrt", op_invsqrt, 1, TRUE, FALSE},
	{"min", op_min, -1, TRUE, TRUE},
	{"max", op_max, -1, TRUE, TRUE},
	{"exp", op_exp, 1, TRUE, FALSE},
	{"floor", op_floor, 1, TRUE, FALSE},
	{"ceil", op_ceil, 1, TRUE, FALSE},
	{"round", op_round, 1, TRUE, FALSE},
	{"abs", op_abs, 1, TRUE, FALSE},
	{"pow", op_pow, 2, TRUE, FALSE},
	{"rand", op_rand, -1, FALSE, FALSE},
	{"ln", op_ln, 1, TRUE, FALSE},
	{"log10", op_log10, 1, TRUE, FALSE},
	{"hypot", op_hypot, 2, TRUE, TRUE},
	{"exp2", op_exp2, 1, TRUE, FALSE},
	{"sinh", op_sinh, 1, TRUE, FALSE},
	{"cosh", op_cosh, 1, TRUE, FALSE},
	{"tanh", op_tanh, 1, TRUE, FALSE},
	{"lerp", op_lerp, 3, TRUE, FALSE},
	{"sqsum", op_sqsum, -1, TRUE, TRUE}
};

/**
 * cpg_math_function_lookup_by_id:
 * @type: A #CpgMathFunctionType
 * @arguments: return value for the number of arguments
 *
 * Lookup the name of a function by its id.
 *
 * Returns: the name of the function, or %NULL if the function could not be
 *          found
 *
 **/
gchar const *
cpg_math_function_lookup_by_id (CpgMathFunctionType  type,
                                gint                *arguments)
{
	*arguments = function_entries[type].arguments;
	return function_entries[type].name;
}

/**
 * cpg_math_function_lookup:
 * @name: The function name
 * @arguments: The number of arguments
 *
 * Lookup a math function given the name @name and number of arguments.
 *
 * Returns: A #CpgMathFunctionType
 *
 **/
CpgMathFunctionType
cpg_math_function_lookup (gchar const  *name,
                          gint         *arguments)
{
	guint i;

	for (i = 1; i < CPG_MATH_FUNCTION_TYPE_NUM; ++i)
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
 * cpg_math_function_is_constant:
 * @type: A #CpgMathFunctionType
 *
 * Get whether a math function is constant (i.e. if it is deterministic). An
 * example of a function that is not constant is 'rand'.
 *
 * Returns: %TRUE if the function is constant, %FALSE otherwise
 *
 **/
gboolean
cpg_math_function_is_constant (CpgMathFunctionType type)
{
	return function_entries[type].constant;
}

/**
 * cpg_math_function_is_variable:
 * @type: A #CpgMathFunctionType
 *
 * Get whether the math function accepts a variable number of arguments.
 *
 * Returns: %TRUE if the function accepts a variable number of arguments,
 *          %FALSE otherwise
 *
 **/
gboolean
cpg_math_function_is_variable (CpgMathFunctionType type)
{
	return function_entries[type].arguments == -1;
}

/**
 * cpg_math_function_is_commutative:
 * @type: A #CpgMathOperatorType
 *
 * Get whether an function is commutative.
 *
 * Returns: %TRUE if the function is commutative, %FALSE otherwise
 *
 **/
gboolean
cpg_math_function_is_commutative (CpgMathFunctionType type)
{
	return function_entries[type].commutative;
}

/**
 * cpg_math_function_execute:
 * @type: A #CpgMathFunctionType
 * @stack: A #CpgStack
 *
 * Execute a math function on the stack.
 *
 **/
void
cpg_math_function_execute (CpgMathFunctionType  type,
                           gint                 numargs,
                           CpgStack            *stack)
{
	function_entries[type].function (stack, numargs);
}

/* operator functions */
static void
op_unary_minus (CpgStack *stack,
                gint      numargs)
{
	cpg_stack_push (stack, -1 * cpg_stack_pop (stack));
}

static void
op_minus (CpgStack *stack,
          gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first - second);
}

static void
op_plus (CpgStack *stack,
         gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first + second);
}

static void
op_multiply (CpgStack *stack,
             gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first * second);
}

static void
op_divide (CpgStack *stack,
           gint      numargs)
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
op_modulo (CpgStack *stack,
           gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, my_fmod (first, second));
}

static void
op_power (CpgStack *stack,
          gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, pow (first, second));
}

static void
op_greater (CpgStack *stack,
            gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first > second);
}

static void
op_less (CpgStack *stack,
         gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first < second);
}

static void
op_greater_or_equal (CpgStack *stack,
                     gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first >= second);
}

static void
op_less_or_equal (CpgStack *stack,
                  gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first < second);
}

static void
op_equal (CpgStack *stack,
          gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first == second);
}

static void
op_or (CpgStack *stack,
       gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first || second);
}

static void
op_and (CpgStack *stack,
        gint      numargs)
{
	double second = cpg_stack_pop (stack);
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, first && second);
}

static void
op_negate (CpgStack *stack,
           gint      numargs)
{
	double first = cpg_stack_pop (stack);

	cpg_stack_push (stack, !first);
}

static void
op_ternary (CpgStack *stack,
            gint      numargs)
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
	gboolean commutative;
} OperatorEntry;

static OperatorEntry operator_entries[] = {
	{NULL, op_noop, 0, TRUE, FALSE},
	{"--", op_unary_minus, 1, TRUE, FALSE},
	{"-", op_minus, 2, TRUE, FALSE},
	{"+", op_plus, 2, TRUE, TRUE},
	{"*", op_multiply, 2, TRUE, TRUE},
	{"/", op_divide, 2, TRUE, FALSE},
	{"%", op_modulo, 2, TRUE, FALSE},
	{"^", op_power, 2, TRUE, FALSE},
	{">", op_greater, 2, TRUE, FALSE},
	{"<", op_less, 2, TRUE, FALSE},
	{">=", op_greater_or_equal, 2, TRUE, FALSE},
	{"<=", op_less_or_equal, 2, TRUE, FALSE},
	{"==", op_equal, 2, TRUE, TRUE},
	{"||", op_or, 2, TRUE, TRUE},
	{"&&", op_and, 2, TRUE, TRUE},
	{"!", op_negate, 1, TRUE, FALSE},
	{"?:", op_ternary, 3, TRUE, FALSE}
};

/**
 * cpg_math_operator_lookup:
 * @type: A #CpgMathOperatorType
 *
 * Lookup the operator type for a certain operator type.
 *
 * Returns: A #CpgMathOperatorType
 *
 **/
CpgMathOperatorType
cpg_math_operator_lookup (CpgMathOperatorType type)
{
	return type;
}

/**
 * cpg_math_operator_execute:
 * @type: A #CpgMathOperatorType
 * @stack: A #CpgStack
 *
 * Execute an operator on the stack.
 *
 **/
void
cpg_math_operator_execute (CpgMathOperatorType  type,
                           gint                 numargs,
                           CpgStack            *stack)
{
	operator_entries[type].function (stack, numargs);
}

/**
 * cpg_math_operator_is_constant:
 * @type: A #CpgMathOperatorType
 *
 * Get whether an operator is constant (i.e. if it is deterministic).
 *
 * Returns: %TRUE if the operator is constant, %FALSE otherwise
 *
 **/
gboolean
cpg_math_operator_is_constant (CpgMathOperatorType type)
{
	return operator_entries[type].constant;
}

/**
 * cpg_math_operator_is_variable:
 * @type: A #CpgMathOperatorType
 *
 * Get whether an operator accepts a variable number of arguments.
 *
 * Returns: %TRUE if the operator accepts a variable number of arguments,
 *          %FALSE otherwise
 *
 **/
gboolean
cpg_math_operator_is_variable (CpgMathOperatorType type)
{
	return operator_entries[type].arguments == -1;
}

/**
 * cpg_math_operator_is_commutative:
 * @type: A #CpgMathOperatorType
 *
 * Get whether an operator is commutative.
 *
 * Returns: %TRUE if the operator is commutative, %FALSE otherwise
 *
 **/
gboolean
cpg_math_operator_is_commutative (CpgMathOperatorType type)
{
	return operator_entries[type].commutative;
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

/**
 * cpg_math_constant_lookup:
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
cpg_math_constant_lookup (gchar const  *name,
                          gboolean     *found)
{
	guint i;

	for (i = 0; i < sizeof (constant_entries) / sizeof (CpgConstantEntry); ++i)
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
