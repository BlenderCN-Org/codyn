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
#include <clapack.h>
#include <cblas.h>

#include "cdn-compile-error.h"

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

typedef gdouble (*BinaryFunction)(gdouble a, gdouble b);

static void
foreach_element2 (CdnStack       *stack,
                  gint           *argdim,
                  BinaryFunction  op)
{
	gint num1;
	gint num2;

	num1 = argdim ? (argdim[2] * argdim[3]) : 1;
	num2 = argdim ? (argdim[0] * argdim[1]) : 1;

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

		second = cdn_stack_pop (stack);
		start = cdn_stack_count (stack) - num1;

		for (i = 0; i < num1; ++i)
		{
			cdn_stack_set_at (stack,
			                  start,
			                  op (cdn_stack_at (stack,
			                                    start),
			                      second));
			++start;
		}
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

static gdouble
sum (gdouble a,
     gdouble b)
{
	return a + b;
}

static gdouble
product (gdouble a,
         gdouble b)
{
	return a * b;
}

static void
op_nested (CdnStack   *stack,
           gint        numargs,
           gint       *argdim,
           gdouble   (*func)(gdouble, gdouble))
{
	gboolean first = TRUE;
	gdouble value = 0;
	gint i;

	for (i = 0; i < numargs; ++i)
	{
		gint n = argdim ? argdim[(numargs - 1 - i) * 2] * argdim[(numargs - i) * 2 - 1] : 1;

		while (n > 0)
		{
			if (first)
			{
				value = cdn_stack_pop (stack);
				first = FALSE;
			}
			else
			{
				value = func (value, cdn_stack_pop (stack));
			}

			--n;
		}
	}

	cdn_stack_push (stack, value);
}

static void
op_min (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	op_nested (stack, numargs, argdim, min);
}

static void
op_max (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	op_nested (stack, numargs, argdim, max);
}

static void
op_sum (CdnStack *stack,
        gint      numargs,
        gint     *argdim)
{
	op_nested (stack, numargs, argdim, sum);
}

static void
op_product (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	op_nested (stack, numargs, argdim, product);
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

	// TODO

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

	// TODO
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

	// TODO

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
matrix_multiply (CdnStack *stack,
                 gint     *argdim)
{
	gint i;
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrC;

	gint num1 = argdim[2] * argdim[3];
	gint num2 = argdim[0] * argdim[1];
	gint numend = argdim[2] * argdim[1];

	ptrC = cdn_stack_output_ptr (stack);
	ptrB = ptrC - num2;
	ptrA = ptrB - num1;

	cblas_dgemm (CblasRowMajor,
	             CblasNoTrans,
	             CblasNoTrans,
	             argdim[2],
	             argdim[1],
	             argdim[3],
	             1,
	             ptrA,
	             argdim[3],
	             ptrB,
	             argdim[1],
	             0,
	             ptrC,
	             argdim[1]);

	cdn_stack_popn (stack, num1 + num2);

	// Copy back from ptrC to ptrA
	for (i = 0; i < numend; ++i)
	{
		cdn_stack_push (stack, *(ptrC++));
	}
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
	else if ((argdim[0] == 1 && argdim[1] == 1) ||
	         (argdim[2] == 1 && argdim[3] == 1))
	{
		foreach_element2 (stack, argdim, op_emultiply_impl);
	}
	else if (argdim[3] == argdim[0])
	{
		matrix_multiply (stack, argdim);
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
op_divide_impl (gdouble a, gdouble b)
{
	return a / b;
}

static void
op_divide (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	foreach_element2 (stack, argdim, op_divide_impl);
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
	return fabs (a - b) < 10e-12 ? 1.0 : 0.0;
}

static void
op_equal (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, op_equal_impl);
}

static gdouble
op_nequal_impl (gdouble a, gdouble b)
{
	return fabs (a - b) >= 10e-12 ? 1.0 : 0.0;
}

static void
op_nequal (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	foreach_element2 (stack, argdim, op_nequal_impl);
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
	if (!argdim || (argdim[2] == 1 && argdim[3] == 1))
	{
		gdouble falsepart;
		gdouble truepart;
		gdouble condition;

		falsepart = cdn_stack_pop (stack);
		truepart = cdn_stack_pop (stack);
		condition = cdn_stack_pop (stack);

		cdn_stack_push (stack, condition ? truepart : falsepart);
	}
	else
	{
		gint n = cdn_stack_count (stack);
		gint nn = argdim[2] * argdim[3];

		if (!op_equal_impl (cdn_stack_at (stack, n - 1 - nn * 2), 0))
		{
			gint i;

			// pop off the false part
			cdn_stack_popn (stack, nn);

			n -= nn * 2;

			// copy the true part
			for (i = 0; i < nn; ++i)
			{
				cdn_stack_set_at (stack, n - 1 + i, cdn_stack_at (stack, n + i));
			}

			// Pop condition
			cdn_stack_pop (stack);
		}
		else
		{
			gint i;

			n -= nn;

			// copy the false part
			for (i = 0; i < nn; ++i)
			{
				cdn_stack_set_at (stack, n - nn - 1 + i, cdn_stack_at (stack, n + i));
			}

			cdn_stack_popn (stack, nn + 1);
		}
	}
}

static void
op_mindex (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	// Sample from the input
	gint rows;
	gint cols;
	gboolean a1 = FALSE;
	gboolean a2 = FALSE;
	gdouble *vptr;
	gint vrows;
	gint vcols;
	gint nv;
	gdouble *iptr1;
	gdouble *iptr2;
	gint n;

	if (!argdim)
	{
		rows = 1;
		cols = 1;

		vrows = 1;
		vcols = 1;
		nv = 1;
		n = 1;

		vptr = cdn_stack_output_ptr (stack) - 1;
		iptr2 = vptr - 1;
		iptr1 = iptr2 - 1;
	}
	else
	{
		a1 = (argdim[4] * argdim[5] == 1);
		a2 = (argdim[2] * argdim[3] == 1);

		rows = a1 ? argdim[2] : argdim[4];
		cols = a1 ? argdim[3] : argdim[5];

		vrows = argdim[0];
		vcols = argdim[1];

		vptr = cdn_stack_output_ptr (stack) - vrows * vcols;

		iptr2 = vptr - argdim[2] * argdim[3];
		iptr1 = iptr2 - argdim[4] * argdim[5];

		n = rows * cols;
		nv = vrows * vcols;
	}

	if (a1 && !a2)
	{
		gint r = (gint)(*iptr1 + 0.5) * vcols;
		gint i;

		for (i = 0; i < n; ++i)
		{
			gint c = (gint)(*iptr2++ + 0.5);
			gint idx = r + c;

			if (idx < 0 || idx >= nv)
			{
				*iptr1++ = 0;
			}
			else
			{
				*iptr1++ = vptr[idx];
			}
		}

		cdn_stack_popn (stack, nv + 1);
	}
	else if (a2 && !a1)
	{
		gint c = (gint)(*iptr2 + 0.5);
		gint i;

		for (i = 0; i < n; ++i)
		{
			gint r = (gint)(*iptr1 + 0.5);
			gint idx = r * vcols + c;

			if (idx < 0 || idx >= nv)
			{
				*iptr1++ = 0;
			}
			else
			{
				*iptr1++ = vptr[idx];
			}
		}

		cdn_stack_popn (stack, nv + 1);
	}
	else
	{
		gint i;

		for (i = 0; i < n; ++i)
		{
			gint c = (gint)(*iptr2++ + 0.5);
			gint r = (gint)(*iptr1 + 0.5);

			gint idx = r * vcols + c;

			if (idx < 0 || idx >= nv)
			{
				*iptr1++ = 0;
			}
			else
			{
				*iptr1++ = vptr[idx];
			}
		}

		cdn_stack_popn (stack, nv + n);
	}
}

static void
op_index (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	gint i;

	if (numargs == 3)
	{
		// row/column indexing
		return op_mindex (stack, numargs, argdim);
	}

	// Sample from the output
	gint num1 = argdim ? argdim[2] * argdim[3] : 1;
	gint num2 = argdim ? argdim[0] * argdim[1] : 1;

	gdouble *vptr = cdn_stack_output_ptr (stack) - num2;
	gdouble *iptr = vptr - num1;

	for (i = 0; i < num1; ++i)
	{
		gint idx = (gint)(*iptr + 0.5);

		if (idx < num2)
		{
			*iptr++ = vptr[idx];
		}
		else
		{
			*iptr++ = 0.0;
		}
	}

	cdn_stack_popn (stack, num2);
}

static void
op_lindex (CdnStack *stack,
           gint      numargs,
           gint     *argdim)
{
	gboolean a1 = (argdim[4] * argdim[5] == 1);
	gboolean a2 = (argdim[2] * argdim[3] == 1);
	gint n;
	gdouble *ptr1;
	gdouble *ptr2;
	gint si;

	n = a1 ? argdim[2] * argdim[3] : argdim[4] * argdim[5];

	gint w = (gint)(cdn_stack_pop (stack) + 0.5);

	ptr2 = cdn_stack_output_ptr (stack) - argdim[2] * argdim[3];
	ptr1 = ptr2 - argdim[4] * argdim[5];

	if (a1)
	{
		gint i;

		si = (gint)(*ptr1 + 0.5) * w;

		for (i = 0; i < n; ++i)
		{
			*ptr1++ = si + (gint)(*ptr2++ + 0.5);
		}

		cdn_stack_pop (stack);
	}
	else if (a2)
	{
		gint i;

		si = (gint)(*ptr2 + 0.5);

		for (i = 0; i < n; ++i)
		{
			*ptr1 = si + ((gint)(*ptr1 + 0.5)) * w;
			++ptr1;
		}

		cdn_stack_pop (stack);
	}
	else
	{
		gint i;

		for (i = 0; i < n; ++i)
		{
			*ptr1 = (gint)(*ptr2++ + 0.5) + ((gint)(*ptr1 + 0.5)) * w;
			++ptr1;
		}

		cdn_stack_popn (stack, n / 2);
	}
}

static void
op_transpose (CdnStack *stack,
              gint      numargs,
              gint     *argdim)
{
	gint numr = argdim ? argdim[0] : 1;
	gint numc = argdim ? argdim[1] : 1;

	gint start;

	gdouble *m = cdn_stack_ptr (stack) + cdn_stack_count (stack) - numr * numc;

	for (start = 0; start < numr * numc; ++start)
	{
		gint next = start;
		gint i = 0;
		gdouble tmp;

		do
		{
			++i;
			next = (next % numr) * numc + next / numr;
		} while (next > start);

		if (next < start || i == 1)
		{
			continue;
		}

		tmp = m[next = start];

		do
		{
			i = (next % numr) * numc + next / numr;
			m[next] = (i == start) ? tmp : m[i];
			next = i;
		} while (next > start);
	}
}

static void
op_inverse (CdnStack *stack,
            gint      numargs,
            gint     *argdim)
{
	gint n = argdim ? argdim[0] : 1;
	gdouble *ptr;
	gint *ipiv;
	gint nn = n * n;

	ptr = cdn_stack_output_ptr (stack) - nn;

	ipiv = (gint *)(cdn_stack_output_ptr (stack));

	clapack_dgetrf (CblasRowMajor,
	                n,
	                n,
	                ptr,
	                n,
	                ipiv);

	clapack_dgetri (CblasRowMajor,
	                n,
	                ptr,
	                n,
	                ipiv);
}

static void
op_linsolve (CdnStack *stack,
             gint      numargs,
             gint     *argdim)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gint *ptrIpv;
	gint numa = argdim[0] * argdim[1];
	gint numb = argdim[2] * argdim[3];

	ptrA = cdn_stack_output_ptr (stack) - numa;
	ptrB = ptrA - numb;

	// Use the extra space we allocated on the stack to write ipv
	ptrIpv = (gint *)cdn_stack_output_ptr (stack);

	clapack_dgesv (CblasRowMajor,
	               argdim[0],
	               argdim[3],
	               ptrA,
	               argdim[1],
	               ptrIpv,
	               ptrB,
	               argdim[0]);

	cdn_stack_popn (stack, numa);
}

static void
slinsolve_factorize (gdouble *A,
                     gdouble *L,
                     gint     n)
{
	gint k;

	// First perform LTDL factorization of A exploiting sparsity
	for (k = n - 1; k >= 0; --k)
	{
		gint i = (gint)L[k];

		while (i >= 0)
		{
			gint nr;
			gint nir;
			gint idx1;
			gint idx2;
			gint j;
			gdouble a;

			nr = k * n;
			idx1 = nr + i;
			idx2 = nr + k;

			a = A[idx1] / A[idx2];
			j = i;
			nir = i * n;

			while (j >= 0)
			{
				A[nir + j] -= a * A[nr + j];
				j = (gint)L[j];
			}

			A[idx1] = a;
			i = (gint)L[i];
		}
	}
}

static void
slinsolve_backsubs (gdouble *ptrA,
                    gdouble *ptrB,
                    gdouble *ptrL,
                    gint     n,
                    gint     numc,
                    gint     idx)
{
	gint i;
	gint diagidx;

	diagidx = 0;

	// First solve for b = L^-T b
	for (i = n - 1; i >= 0; --i)
	{
		gint iidx;
		gint j;

		j = (gint)ptrL[i];
		iidx = i * n;

		while (j >= 0)
		{
			ptrB[j] -= ptrA[iidx + j] * ptrB[i];
			j = (gint)ptrL[j];
		}
	}

	// Then finally solve for L^-1 b
	for (i = 0; i < n; ++i)
	{
		gint iidx;
		gint j;

		j = (gint)ptrL[i];
		iidx = i * n;

		ptrB[i] /= ptrA[diagidx];
		diagidx += n + 1;

		while (j >= 0)
		{
			ptrB[i] -= ptrA[iidx + j] * ptrB[j];
			j = (gint)ptrL[j];
		}
	}
}
static void
op_slinsolve (CdnStack *stack,
              gint      numargs,
              gint     *argdim)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrL;
	gint numa;
	gint numl;
	gint numb;
	gint i;
	gint n;

	numa = argdim[0] * argdim[1];
	numl = argdim[2] * argdim[3];
	numb = argdim[4] * argdim[5];

	ptrA = cdn_stack_output_ptr (stack) - numa;
	ptrL = ptrA - numl;
	ptrB = ptrL - numb;

	n = argdim[0];

	// Factorized A in place using LTDL factorization
	slinsolve_factorize (ptrA, ptrL, n);

	for (i = 0; i < argdim[5]; ++i)
	{
		slinsolve_backsubs (ptrA,
		                    ptrB,
		                    ptrL,
		                    n,
		                    argdim[5],
		                    i);
	}

	// Finally pop lambda and A
	cdn_stack_popn (stack, numa + numl);
}

static void
op_size (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	gint r = argdim ? argdim[0] : 1;
	gint c = argdim ? argdim[1] : 1;

	cdn_stack_popn (stack, r * c);

	cdn_stack_push (stack, r);
	cdn_stack_push (stack, c);
}

static void
op_length (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	gint r = argdim ? argdim[0] : 1;
	gint c = argdim ? argdim[1] : 1;
	gint n = r * c;

	cdn_stack_popn (stack, n);
	cdn_stack_push (stack, n);
}

static void
op_hcat (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	gint c1;
	gint c2;
	gint s1;
	gint s2;
	gint i;
	gdouble *ptr1;
	gdouble *ptr2;
	gdouble *ptr;

	// Horizontally cat two matrices together
	if (!argdim || argdim[0] == 1)
	{
		return;
	}

	c1 = argdim[3];
	c2 = argdim[1];

	s1 = sizeof (gdouble) * c1;
	s2 = sizeof (gdouble) * c2;

	ptr = cdn_stack_output_ptr (stack);
	ptr2 = ptr - argdim[0] * c2;
	ptr1 = ptr2 - argdim[2] * c1;

	for (i = 0; i < argdim[0]; ++i)
	{
		memcpy (ptr, ptr1, s1);

		ptr1 += c1;
		ptr += c1;

		memcpy (ptr, ptr2, s2);

		ptr2 += c2;
		ptr += c2;
	}

	ptr = cdn_stack_output_ptr (stack);

	memcpy (ptr - argdim[0] * c2 - argdim[2] * c1,
	        ptr,
	        sizeof (gdouble) * argdim[0] * (argdim[1] + argdim[3]));
}

static void
op_zeros (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_eye (CdnStack *stack,
         gint      numargs,
         gint     *argdim)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_block (CdnStack *stack,
          gint      numargs,
          gint     *argdim)
{
	gdouble *ptr;
	gint r;
	gint c;
	gint numr;
	gint numc;
	gdouble *ptrA;
	gdouble *ptrR;
	gdouble *ptrC;
	gint i;
	gint n;

	ptr = cdn_stack_output_ptr (stack);

	numr = argdim[3];
	numc = argdim[1];

	ptrC = ptr - numc;
	ptrR = ptrC - numr;
	ptrA = ptrR - argdim[4] * argdim[5];

	i = 0;
	n = numr * numc;

	for (r = 0; r < numr; ++r)
	{
		gint ridx = (gint)ptrR[r] * argdim[5];

		for (c = 0; c < numc; ++c)
		{
			gint cidx = (gint)ptrC[c];

			ptr[i] = ptrA[ridx + cidx];
		}
	}

	// Now copy back. Need a loop like this to not override ptr memory
	// before it's copied to ptrA
	for (i = 0; i < n; ++i)
	{
		ptrA[i] = ptr[i];
	}

	cdn_stack_set_output_ptr (stack, ptrA + n);
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
	{"%", op_modulo, 2, FALSE},
	{"^", op_power, 2, FALSE},
	{">", op_greater, 2, FALSE},
	{"<", op_less, 2, FALSE},
	{">=", op_greater_or_equal, 2, FALSE},
	{"<=", op_less_or_equal, 2, FALSE},
	{"==", op_equal, 2, TRUE},
	{"!=", op_nequal, 2, TRUE},
	{"||", op_or, 2, TRUE},
	{"&&", op_and, 2, TRUE},
	{"!", op_negate, 1, FALSE},
	{"?:", op_ternary, 3, FALSE},
	{NULL, op_noop, 0, FALSE},
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
	{"cycle", op_cycle, 3, FALSE},
	{"index", op_index, -1, FALSE},
	{"lindex", op_lindex, -1, FALSE},
	{"transpose", op_transpose, 1, FALSE},
	{"inverse", op_inverse, 1, FALSE},
	{"linsolve", op_linsolve, 2, FALSE},
	{"slinsolve", op_slinsolve, 3, FALSE},
	{"sum", op_sum, -1, FALSE},
	{"product", op_product, -1, FALSE},
	{"length", op_length, 1, FALSE},
	{"size", op_size, 1, FALSE},
	{"hcat", op_hcat, 2, FALSE},
	{"zeros", op_zeros, 2, FALSE},
	{"eye", op_eye, 1, FALSE},
	{"block", op_block, 3, FALSE}
};

typedef struct
{
	gchar const *name;
	CdnMathFunctionType type;
} FunctionEntryMap;

static FunctionEntryMap function_entry_mapping[] = {
	{"\xe2\x88\x9a", CDN_MATH_FUNCTION_TYPE_SQRT}, /* √ */
	{"\xe2\x88\x91", CDN_MATH_FUNCTION_TYPE_SUM}, /* ∑ */
	{"\xe2\x88\x8f", CDN_MATH_FUNCTION_TYPE_PRODUCT} /* ∏ */
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
	if (arguments)
	{
		*arguments = function_entries[type].arguments;
	}

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

	for (i = CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS + 1; i < CDN_MATH_FUNCTION_TYPE_NUM; ++i)
	{
		if (g_strcmp0 (function_entries[i].name, name) == 0)
		{
			*arguments = function_entries[i].arguments;
			return i;
		}
	}

	for (i = 0; i < sizeof (function_entry_mapping) / sizeof (FunctionEntryMap); ++i)
	{
		FunctionEntryMap *m = &(function_entry_mapping[i]);
		if (g_strcmp0 (m->name, name) == 0)
		{
			*arguments = function_entries[m->type].arguments;
			return m->type;
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
cdn_math_function_is_commutative (CdnMathFunctionType  type,
                                  gint                 numargs,
                                  gint                *argdim)
{
	if (!function_entries[type].commutative)
	{
		return FALSE;
	}

	switch (type)
	{
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			// Only element wise multiply is commutative
			if (!argdim)
			{
				return TRUE;
			}

			if (argdim[0] * argdim[1] == 1 ||
			    argdim[2] * argdim[3] == 1)
			{
				return TRUE;
			}

			if (argdim[3] != argdim[1])
			{
				return TRUE;
			}

			return FALSE;
		break;
		default:
			return TRUE;
	}
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
	{"\xcf\x80", NULL, M_PI},
	{"e", NULL, M_E},
	{"E", NULL, M_E},
	{"NAN", NULL, NAN},
	{"nan", NULL, NAN},
	{"NaN", NULL, NAN},
	{"Inf", NULL, INFINITY},
	{"INF", NULL, INFINITY},
	{"inf", NULL, INFINITY}
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

gboolean
cdn_math_function_get_stack_manipulation (CdnMathFunctionType    type,
                                          gint                   arguments,
                                          gint                  *argdim,
                                          gint                  *outargdim,
                                          gint                  *extra_space,
                                          GError               **error)
{
	*extra_space = 0;

	if (!argdim)
	{
		outargdim[0] = 1;
		outargdim[1] = 1;

		return TRUE;
	}

	// Get the stack manipulation of a particular function given the
	// particular arguments
	switch (type)
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
		case CDN_MATH_FUNCTION_TYPE_SIN:
		case CDN_MATH_FUNCTION_TYPE_COS:
		case CDN_MATH_FUNCTION_TYPE_TAN:
		case CDN_MATH_FUNCTION_TYPE_ASIN:
		case CDN_MATH_FUNCTION_TYPE_ACOS:
		case CDN_MATH_FUNCTION_TYPE_ATAN:
		case CDN_MATH_FUNCTION_TYPE_SQRT:
		case CDN_MATH_FUNCTION_TYPE_INVSQRT:
		case CDN_MATH_FUNCTION_TYPE_EXP:
		case CDN_MATH_FUNCTION_TYPE_FLOOR:
		case CDN_MATH_FUNCTION_TYPE_CEIL:
		case CDN_MATH_FUNCTION_TYPE_ROUND:
		case CDN_MATH_FUNCTION_TYPE_ABS:
		case CDN_MATH_FUNCTION_TYPE_LN:
		case CDN_MATH_FUNCTION_TYPE_LOG10:
		case CDN_MATH_FUNCTION_TYPE_EXP2:
		case CDN_MATH_FUNCTION_TYPE_SINH:
		case CDN_MATH_FUNCTION_TYPE_COSH:
		case CDN_MATH_FUNCTION_TYPE_TANH:
		case CDN_MATH_FUNCTION_TYPE_SIGN:
		case CDN_MATH_FUNCTION_TYPE_CSIGN:
			// Operators with one argument simply copy
			outargdim[0] = argdim[0];
			outargdim[1] = argdim[1];
			return TRUE;
		break;
		case CDN_MATH_FUNCTION_TYPE_MINUS:
		case CDN_MATH_FUNCTION_TYPE_PLUS:
		case CDN_MATH_FUNCTION_TYPE_EMULTIPLY:
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
		case CDN_MATH_FUNCTION_TYPE_MODULO:
		case CDN_MATH_FUNCTION_TYPE_POWER:
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_NEQUAL:
		case CDN_MATH_FUNCTION_TYPE_OR:
		case CDN_MATH_FUNCTION_TYPE_AND:
		case CDN_MATH_FUNCTION_TYPE_POW:
		case CDN_MATH_FUNCTION_TYPE_HYPOT:
		case CDN_MATH_FUNCTION_TYPE_SQSUM:
		case CDN_MATH_FUNCTION_TYPE_ATAN2:
			if (argdim[0] == 1 && argdim[1] == 1)
			{
				// Take second arg size
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[3];
				return TRUE;
			}
			else if (argdim[0] * argdim[1] != argdim[2] * argdim[3])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot perform element wise operation on arguments of %d-by%d and %d-by-%d",
				             argdim[2], argdim[3],
				             argdim[0], argdim[1]);

				return FALSE;
			}
			else
			{
				// Take first arg size
				outargdim[0] = argdim[0];
				outargdim[1] = argdim[1];
				return TRUE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			if (argdim[0] == 1 && argdim[1] == 1)
			{
				// element wise, take second dims
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[3];
				return TRUE;
			}
			else if (argdim[2] == 1 && argdim[3] == 1)
			{
				// element wise, take first dims
				outargdim[0] = argdim[0];
				outargdim[1] = argdim[1];
				return TRUE;
			}
			else if (argdim[3] == argdim[0])
			{
				// matrix multiplication
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[1];

				*extra_space = outargdim[0] * outargdim[1];
				return TRUE;
			}
			else if (argdim[0] * argdim[1] == argdim[2] * argdim[3])
			{
				// element wise, take first arg
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[3];
				return TRUE;
			}
			else
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot multiply matrices of size (%d, %d) and (%d, %d)",
				             argdim[2], argdim[3], argdim[0], argdim[1]);

				return FALSE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_TERNARY:
			if (argdim[4] != 1 || argdim[5] != 1 ||
			    argdim[0] != argdim[2] ||
			    argdim[1] != argdim[3])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The `true' and `false' parts of the ternary operator must have the same dimensions (got (%d, %d) and (%d, %d))",
				             argdim[2], argdim[3], argdim[0], argdim[1]);

				return FALSE;
			}
			else
			{
				outargdim[0] = argdim[0];
				outargdim[1] = argdim[1];
				return TRUE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_MIN:
		case CDN_MATH_FUNCTION_TYPE_MAX:
		case CDN_MATH_FUNCTION_TYPE_SUM:
		case CDN_MATH_FUNCTION_TYPE_PRODUCT:
			outargdim[0] = 1;
			outargdim[1] = 1;

			return TRUE;
		break;
		case CDN_MATH_FUNCTION_TYPE_LERP:
			if (argdim[2] != argdim[4] ||
			    argdim[3] != argdim[5])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Can only use `lerp' on arguments with the same dimensions (got (%d, %d) and (%d, %d))",
				             argdim[4], argdim[5], argdim[2], argdim[3]);

				return FALSE;
			}
			else if (argdim[0] == 1 && argdim[1] == 1)
			{
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[3];
				return TRUE;
			}
			else if (argdim[2] == 1 && argdim[0] == 1)
			{
				outargdim[0] = argdim[1];
				outargdim[1] = argdim[3];
				return TRUE;
			}
			else if (argdim[3] == 1 && argdim[1] == 1)
			{
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[0];
				return TRUE;
			}
			else
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of arguments for `lerp' (got (%d, %d) and (%d, %d) and (%d, %d))",
				             argdim[4], argdim[5], argdim[2], argdim[3], argdim[0], argdim[1]);
				return FALSE;
			}
		case CDN_MATH_FUNCTION_TYPE_CLIP:
		case CDN_MATH_FUNCTION_TYPE_CYCLE:
			if (argdim[0] == 1 && argdim[1] == 1 &&
			    argdim[2] == 1 && argdim[3] == 1 &&
			    argdim[4] == 1 && argdim[5])
			{
				outargdim[0] = 1;
				outargdim[1] = 1;
				return TRUE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_INDEX:
			if (arguments == 3)
			{
				/* Non linear index */
				gboolean a1;
				gboolean a2;

				a1 = (argdim[4] * argdim[5] == 1);
				a2 = (argdim[2] * argdim[3] == 1);

				if (!a1 && !a2 && (argdim[2] != argdim[4] || argdim[3] != argdim[5]))
				{
					g_set_error (error,
					             CDN_COMPILE_ERROR_TYPE,
					             CDN_COMPILE_ERROR_INVALID_DIMENSION,
					             "Dimensions of arguments for `index' operator must be the same (got (%d, %d) and (%d, %d))",
					             argdim[4], argdim[5], argdim[2], argdim[3]);

					return FALSE;
				}

				outargdim[0] = a1 ? argdim[2] : argdim[4];
				outargdim[1] = a1 ? argdim[3] : argdim[5];
			}
			else
			{
				/* Linear index */
				outargdim[0] = argdim[2];
				outargdim[1] = argdim[3];
			}

			return TRUE;
		break;
		case CDN_MATH_FUNCTION_TYPE_LINDEX:
		{
			gboolean a1 = (argdim[4] * argdim[5] == 1);
			gboolean a2 = (argdim[2] * argdim[3] == 1);

			if (!a1 && !a2 && (argdim[2] != argdim[4] || argdim[3] != argdim[5]))
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Dimensions of `lindex' arguments must be the same (got (%d, %d) and (%d, %d))",
				             argdim[4], argdim[5], argdim[2], argdim[3]);

				return FALSE;
			}

			outargdim[0] = 1;
			outargdim[1] = a1 ? argdim[2] * argdim[3] : argdim[4] * argdim[5];

			return TRUE;
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_TRANSPOSE:
			outargdim[0] = argdim[1];
			outargdim[1] = argdim[0];
		break;
		case CDN_MATH_FUNCTION_TYPE_INVERSE:
			if (argdim[0] != argdim[1])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot invert a non square matrix (%d, %d)",
				             argdim[0], argdim[1]);

				return FALSE;
			}

			outargdim[0] = argdim[0];
			outargdim[1] = argdim[1];

			*extra_space = argdim[0];
		break;
		case CDN_MATH_FUNCTION_TYPE_LINSOLVE:
			// A x = B with A the second arg and B the first
			if (argdim[0] != argdim[1])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot solve a system which is not square (%d, %d)",
				             argdim[0],
				             argdim[1]);

				return FALSE;
			}

			if (argdim[0] != argdim[2])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of B (in Ax = B), expected `%d' rows but got `%d' rows",
				             argdim[0], argdim[2]);

				return FALSE;
			}

			outargdim[0] = argdim[2];
			outargdim[1] = argdim[3];

			// Need extra space to store the pivoting coefficients
			*extra_space = argdim[0];
		break;
		case CDN_MATH_FUNCTION_TYPE_SLINSOLVE:
			// A x = B, λ
			// with order of arguments: B, λ, A
			if (argdim[0] != argdim[1])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot solve a system which is not square (%d, %d)",
				             argdim[0],
				             argdim[1]);

				return FALSE;
			}

			if (argdim[0] != argdim[4])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of B (in A(λ, λ)x = B(λ)), expected `%d' rows but got `%d' rows",
				             argdim[0], argdim[2]);

				return FALSE;
			}

			if (argdim[0] != argdim[2])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of λ (in A(λ, λ)x = B(λ)), expected `%d' rows but got `%d' rows",
				             argdim[0], argdim[4]);

				return FALSE;
			}

			if (argdim[5] != 1)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The number of columns of λ must 1 (got `%d')",
				             argdim[5]);

				return FALSE;
			}

			outargdim[0] = argdim[2];
			outargdim[1] = argdim[3];
		break;
		case CDN_MATH_FUNCTION_TYPE_LENGTH:
			outargdim[0] = 1;
			outargdim[1] = 1;
		break;
		case CDN_MATH_FUNCTION_TYPE_SIZE:
			outargdim[0] = 1;
			outargdim[1] = 2;
		break;
		case CDN_MATH_FUNCTION_TYPE_HCAT:
			if (argdim[0] != argdim[2])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot concat %d rows with %d rows",
				             argdim[0], argdim[1]);

				return FALSE;
			}

			outargdim[0] = argdim[0];
			outargdim[1] = argdim[1] + argdim[3];

			*extra_space = argdim[0] * (argdim[1] + argdim[3]);
		break;
		case CDN_MATH_FUNCTION_TYPE_BLOCK:
			if (argdim[0] != 1 || argdim[2] != 1 ||
			    argdim[1] != argdim[3])
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The row and column indices for the block function must be 1-by-N but got %d-by-%d rows and %d-by-%d columns",
				             argdim[2], argdim[3],
				             argdim[0], argdim[1]);

				return FALSE;
			}

			outargdim[0] = argdim[3];
			outargdim[1] = argdim[1];

			*extra_space = argdim[3] * argdim[1];
		break;
		default:
			return FALSE;
	}

	return FALSE;
}
