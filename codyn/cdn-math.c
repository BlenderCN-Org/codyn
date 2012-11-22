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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <math.h>
#include <string.h>

#ifdef HAVE_LAPACK
#include <clapack.h>
#endif

#ifdef HAVE_BLAS
#ifdef PLATFORM_OSX
#include <Accelerate/Accelerate.h>
#else
#include <cblas.h>
#endif
#endif

#include "cdn-compile-error.h"

#include "cdn-math.h"

/**
 * SECTION:cdn-math
 * @short_description: Math function/operators
 *
 * Math expression helper functions.
 *
 */

#define foreach_element(op)							\
gint i;										\
gint num;									\
gint start;									\
										\
num = argdim ? (cdn_stack_arg_size (argdim->args)) : 1;				\
start = cdn_stack_count (stack) - 1;						\
										\
for (i = 0; i < num; ++i)							\
{										\
	cdn_stack_set_at (stack, start, op (cdn_stack_at (stack, start)));	\
	--start;								\
}

#define SIMPLE_MATH_MAP(func)	SIMPLE_MATH_MAP_CODE(func, func)

#define SIMPLE_MATH_MAP_CODE(func, code)					\
static void									\
op_##func (CdnStack           *stack,						\
           CdnStackArgs const *argdim,						\
           gpointer            userdata)					\
{										\
	foreach_element (code);							\
}

#define BIN_MATH_MAP(func)	BIN_MATH_MAP_CODE(func, func)

#define BIN_MATH_MAP_CODE(func, code)						\
static void									\
op_##func (CdnStack           *stack,						\
           CdnStackArgs const *argdim,						\
           gpointer            userdata)					\
{										\
	foreach_element2 (stack, argdim, code);					\
}

typedef gdouble (*BinaryFunction)(gdouble a, gdouble b);

static void
foreach_element2 (CdnStack           *stack,
                  CdnStackArgs const *argdim,
                  BinaryFunction      op)
{
	gint num1;
	gint num2;

	num1 = argdim->args[1].rows * argdim->args[1].columns;
	num2 = argdim->args[0].rows * argdim->args[0].columns;

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
			cdn_stack_set_at (stack,
			                  start,
			                  op (first,
			                      cdn_stack_at (stack,
			                                    start + 1)));
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

			cdn_stack_set_at (stack,
			                  start,
			                  op (cdn_stack_at (stack,
			                                    start),
			                      second));

			--start;
		}
	}
}

typedef gdouble (*TernaryFunction)(gdouble a, gdouble b, gdouble c);

static void
foreach_element3 (CdnStack           *stack,
                  CdnStackArgs const *argdim,
                  TernaryFunction     op)
{
	gint num1;
	gint num2;
	gint num3;
	gdouble *ptr;
	gdouble *maxptr;
	gdouble *minptr;

	num1 = cdn_stack_arg_size (argdim->args + 2);
	num2 = cdn_stack_arg_size (argdim->args + 1);
	num3 = cdn_stack_arg_size (argdim->args + 0);

	maxptr = cdn_stack_output_ptr (stack) - num3;
	minptr = maxptr - num2;
	ptr = minptr - num1;

	if (num1 == 1 && num2 == 1 && num3 == 1)
	{
		*ptr = op (*ptr, *(ptr + 1), *(ptr + 2));
		++ptr;
	}
	else if (num1 == 1 && num2 == 1)
	{
		gdouble val;
		gdouble min;
		gint i;

		val = *ptr;
		min = *(ptr + 1);

		for (i = 0; i < num3; ++i)
		{
			*ptr = op (val, min, *(ptr + 2));
			++ptr;
		}
	}
	else if (num1 == 1)
	{
		gdouble val;
		gint i;

		val = *ptr;

		for (i = 0; i < num2; ++i)
		{
			*ptr++ = op (val, *minptr++, *maxptr);

			if (num3 > 1)
			{
				++maxptr;
			}
		}
	}
	else
	{
		gint i;

		for (i = 0; i < num1; ++i)
		{
			*ptr = op (*ptr, *minptr, *maxptr);
			++ptr;

			if (num2 > 1)
			{
				++minptr;
			}

			if (num3 > 1)
			{
				++maxptr;
			}
		}
	}

	cdn_stack_set_output_ptr (stack, ptr);
}

static gdouble
sign_value (gdouble value)
{
	return signbit (value) ? -1 : 1;
}

SIMPLE_MATH_MAP (sin)
SIMPLE_MATH_MAP (cos)
SIMPLE_MATH_MAP (tan)
SIMPLE_MATH_MAP (sqrt)
SIMPLE_MATH_MAP_CODE (invsqrt, 1.0 / sqrt)
SIMPLE_MATH_MAP (asin)
SIMPLE_MATH_MAP (acos)
SIMPLE_MATH_MAP (atan)
SIMPLE_MATH_MAP (floor)
SIMPLE_MATH_MAP (ceil)
SIMPLE_MATH_MAP (round)
SIMPLE_MATH_MAP_CODE (abs, fabs)
SIMPLE_MATH_MAP (exp)
SIMPLE_MATH_MAP (erf)
SIMPLE_MATH_MAP_CODE (ln, log)
SIMPLE_MATH_MAP (log10)
SIMPLE_MATH_MAP (exp2)
SIMPLE_MATH_MAP (sinh)
SIMPLE_MATH_MAP (cosh)
SIMPLE_MATH_MAP (tanh)
SIMPLE_MATH_MAP_CODE (sign, sign_value)

BIN_MATH_MAP (atan2)
BIN_MATH_MAP (pow)
BIN_MATH_MAP_CODE (csign, copysign)
BIN_MATH_MAP_CODE (hypotv, hypot)

static gdouble
min (gdouble  a,
     gdouble  b,
     gboolean initial)
{
	if (initial)
	{
		return b;
	}
	else
	{
		return a < b ? a : b;
	}
}

static gdouble
max (gdouble  a,
     gdouble  b,
     gboolean initial)
{
	if (initial)
	{
		return b;
	}
	else
	{
		return a > b ? a : b;
	}
}

static gdouble
sum (gdouble  a,
     gdouble  b,
     gboolean initial)
{
	return a + b;
}

static gdouble
product (gdouble  a,
         gdouble  b,
         gboolean initial)
{
	return a * b;
}

static void
op_nested (CdnStack            *stack,
           CdnStackArgs const  *argdim,
           gdouble              initial,
           gdouble            (*func)(gdouble, gdouble, gboolean))
{
	gboolean first = TRUE;
	gdouble value;
	gint i;

	value = initial;

	for (i = 0; i < argdim->num; ++i)
	{
		gint n = cdn_stack_arg_size (argdim->args + i);

		while (n > 0)
		{
			value = func (value, cdn_stack_pop (stack), first);
			first = FALSE;

			--n;
		}
	}

	cdn_stack_push (stack, value);
}

#define NESTED_MATH_MAP(func, initial) NESTED_MATH_MAP_CODE(func, func, initial)

#define NESTED_MATH_MAP_CODE(func, cb, initial)					\
static void									\
op_##func (CdnStack           *stack,						\
           CdnStackArgs const *argdim,						\
           gpointer            userdata)					\
{										\
	op_nested (stack, argdim, initial, cb);					\
}

static gdouble
sqsum (gdouble  a,
       gdouble  b,
       gboolean initial)
{
	return a + b * b;
}

NESTED_MATH_MAP (min, 0)
NESTED_MATH_MAP (max, 0)
NESTED_MATH_MAP (sum, 0)
NESTED_MATH_MAP (product, 1)
NESTED_MATH_MAP (sqsum, 0)

static void
op_hypot (CdnStack           *stack,
          CdnStackArgs const *argdim,
          gpointer            userdata)
{
	if (argdim->num == 2 &&
	    argdim->args[0].rows == 1 &&
	    argdim->args[0].columns == 1 &&
	    argdim->args[1].rows == 1 &&
	    argdim->args[1].columns == 1)
	{
		gdouble a;
		gdouble b;

		b = cdn_stack_pop (stack);
		a = cdn_stack_pop (stack);

		cdn_stack_push (stack, hypot (a, b));
	}
	else if (argdim->num == 2)
	{
		op_hypotv (stack, argdim, userdata);
	}
	else
	{
		op_nested (stack, argdim, 0, sqsum);
		cdn_stack_push (stack, sqrt (cdn_stack_pop (stack)));
	}
}

static gdouble
op_lerp_impl (gdouble val,
              gdouble min,
              gdouble max)
{
	return min + (max - min) * val;
}

static void
op_lerp (CdnStack           *stack,
         CdnStackArgs const *argdim,
         gpointer            userdata)
{
	foreach_element3 (stack, argdim, op_lerp_impl);
}

static gdouble
op_clip_impl (gdouble val,
              gdouble min,
              gdouble max)
{
	return val < min ? min : (val > max ? max : val);
}

static void
op_clip (CdnStack           *stack,
         CdnStackArgs const *argdim,
         gpointer            userdata)
{
	foreach_element3 (stack, argdim, op_clip_impl);
}

static gdouble
op_cycle_impl (gdouble val,
               gdouble min,
               gdouble max)
{
	if (val > max)
	{
		return min + fmod (val - min, max - min);
	}
	else if (val < min)
	{
		return max - fmod (min - val, max - min);
	}
	else
	{
		return val;
	}
}

static void
op_cycle (CdnStack           *stack,
          CdnStackArgs const *argdim,
          gpointer            userdata)
{
	foreach_element3 (stack, argdim, op_cycle_impl);
}

static void
op_noop (CdnStack           *stack,
         CdnStackArgs const *argdim,
         gpointer            userdata)
{
}

SIMPLE_MATH_MAP_CODE (unary_minus, -1 * )

static gdouble
minus_impl (gdouble a,
            gdouble b)
{
	return a - b;
}

static gdouble
plus_impl (gdouble a,
           gdouble b)
{
	return a + b;
}

static gdouble
emultiply_impl (gdouble a,
                gdouble b)
{
	return a * b;
}

static gdouble
divide_impl (gdouble a,
             gdouble b)
{
	return a / b;
}

static gdouble
modulo_impl (gdouble x,
             gdouble y)
{
	gdouble ans = fmod (x, y);

	return ans < 0 ? ans + y : ans;
}

static gdouble
greater_impl (gdouble a,
              gdouble b)
{
	return a > b;
}

static gdouble
less_impl (gdouble a,
           gdouble b)
{
	return a < b;
}

static gdouble
greater_or_equal_impl (gdouble a,
                       gdouble b)
{
	return a >= b;
}

static gdouble
less_or_equal_impl (gdouble a,
                    gdouble b)
{
	return a <= b;
}

static gdouble
equal_impl (gdouble a,
            gdouble b)
{
	return fabs (a - b) < 10e-12 ? 1.0 : 0.0;
}

static gdouble
nequal_impl (gdouble a,
             gdouble b)
{
	return fabs (a - b) >= 10e-12 ? 1.0 : 0.0;
}

static gdouble
or_impl (gdouble a,
         gdouble b)
{
	return !equal_impl (a, 0) || !equal_impl (b, 0);
}

static gdouble
and_impl (gdouble a,
          gdouble b)
{
	return !equal_impl (a, 0) && !equal_impl (b, 0);
}

static gdouble
negate_impl (gdouble a)
{
	return equal_impl (a, 0) ? 1 : 0;
}

BIN_MATH_MAP_CODE (minus, minus_impl)
BIN_MATH_MAP_CODE (plus, plus_impl)
BIN_MATH_MAP_CODE (emultiply, emultiply_impl)
BIN_MATH_MAP_CODE (divide, divide_impl)
BIN_MATH_MAP_CODE (modulo, modulo_impl)
BIN_MATH_MAP_CODE (power, pow)
BIN_MATH_MAP_CODE (greater, greater_impl)
BIN_MATH_MAP_CODE (less, less_impl)
BIN_MATH_MAP_CODE (greater_or_equal, greater_or_equal_impl)
BIN_MATH_MAP_CODE (less_or_equal, less_or_equal_impl)
BIN_MATH_MAP_CODE (equal, equal_impl)
BIN_MATH_MAP_CODE (nequal, nequal_impl)
BIN_MATH_MAP_CODE (or, or_impl)
BIN_MATH_MAP_CODE (and, and_impl)
SIMPLE_MATH_MAP_CODE (negate, negate_impl)

static void
matrix_multiply (CdnStack           *stack,
                 CdnStackArgs const *argdim)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrC;

	gint num1 = argdim->args[1].rows * argdim->args[1].columns;
	gint num2 = argdim->args[0].rows * argdim->args[0].columns;
	gint numend = argdim->args[1].rows * argdim->args[0].columns;

	ptrC = cdn_stack_output_ptr (stack);
	ptrB = ptrC - num2;
	ptrA = ptrB - num1;

#ifdef HAVE_BLAS
	cblas_dgemm (CblasRowMajor,
	             CblasNoTrans,
	             CblasNoTrans,
	             argdim->args[1].rows,
	             argdim->args[0].columns,
	             argdim->args[1].columns,
	             1,
	             ptrA,
	             argdim->args[1].columns,
	             ptrB,
	             argdim->args[0].columns,
	             0,
	             ptrC,
	             argdim->args[0].columns);
#else
{
	gint r;
	gint idx = 0;
	gint ar = 0;

	// Naive implementation
	for (r = 0; r < argdim->args[0].columns; ++r)
	{
		gint c;

		for (c = 0; c < argdim->args[1].columns; ++c)
		{
			gdouble s = 0;
			gint bc = 0;
			gint i;

			for (i = 0; i < argdim->args[0].rows; ++i)
			{
				s += ptrA[ar + i] * ptrB[c + bc];
				bc += argdim->args[0].columns;
			}

			ptrC[idx] = s;
			++idx;
		}

		ar += argdim->args[0].rows;
	}
}
#endif

	memmove (ptrA, ptrC, sizeof (gdouble) * numend);

	cdn_stack_set_output_ptr (stack,
	                          ptrA + numend);
}

static void
op_multiply (CdnStack           *stack,
             CdnStackArgs const *argdim,
             gpointer            userdata)
{
	gboolean n1;
	gboolean n2;

	n1 = argdim->args[0].rows == 1 &&
	     argdim->args[0].columns == 1;

	n2 = argdim->args[1].rows == 1 &&
	     argdim->args[1].columns == 1;

	if (n1 && n2)
	{
		gdouble second;

		second = cdn_stack_pop (stack);
		cdn_stack_set (stack, cdn_stack_peek (stack) * second);
	}
	else if (n1 || n2)
	{
		foreach_element2 (stack, argdim, emultiply_impl);
	}
	else if (argdim->args[1].columns == argdim->args[0].rows)
	{
		matrix_multiply (stack, argdim);
	}
	else
	{
		foreach_element2 (stack, argdim, emultiply_impl);
	}
}

static void
op_ternary (CdnStack           *stack,
            CdnStackArgs const *argdim,
            gpointer            userdata)
{
	if (argdim->args[1].rows == 1 &&
	    argdim->args[1].columns == 1)
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
		gint nn = argdim->args[1].rows * argdim->args[1].columns;

		if (!equal_impl (cdn_stack_at (stack, n - 1 - nn * 2), 0))
		{
			gint i;

			// pop off the false part
			cdn_stack_popn (stack, nn);

			n -= nn * 2;

			// copy the true part
			for (i = 0; i < nn; ++i)
			{
				cdn_stack_set_at (stack,
				                  n - 1 + i,
				                  cdn_stack_at (stack, n + i));
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
				cdn_stack_set_at (stack,
				                  n - nn - 1 + i,
				                  cdn_stack_at (stack, n + i));
			}

			cdn_stack_popn (stack, nn + 1);
		}
	}
}

static void
op_mindex (CdnStack           *stack,
           CdnStackArgs const *argdim,
           gpointer            userdata)
{
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

	a1 = (cdn_stack_arg_size (argdim->args + 2) == 1);
	a2 = (cdn_stack_arg_size (argdim->args + 1) == 1);

	rows = a1 ? argdim->args[1].rows : argdim->args[2].rows;
	cols = a1 ? argdim->args[1].columns : argdim->args[2].columns;

	vrows = argdim->args[0].rows;
	vcols = argdim->args[0].columns;

	vptr = cdn_stack_output_ptr (stack) - vrows * vcols;

	iptr2 = vptr - cdn_stack_arg_size (argdim->args + 1);
	iptr1 = iptr2 - cdn_stack_arg_size (argdim->args + 2);

	n = rows * cols;
	nv = vrows * vcols;

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
op_index (CdnStack           *stack,
          CdnStackArgs const *argdim,
          gpointer            userdata)
{
	gint i;

	if (argdim->num == 3)
	{
		// row/column indexing
		return op_mindex (stack, argdim, userdata);
	}

	// Sample from the output
	gint num1 = cdn_stack_arg_size (argdim->args + 1);
	gint num2 = cdn_stack_arg_size (argdim->args);

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
op_lindex (CdnStack           *stack,
           CdnStackArgs const *argdim,
           gpointer            userdata)
{
	gboolean a1;
	gboolean a2;
	gdouble *ptr1;
	gdouble *ptr2;
	gint si;
	gint w;

	a1 = (cdn_stack_arg_size (argdim->args + 2) == 1);
	a2 = (cdn_stack_arg_size (argdim->args + 1) == 1);

	w = (gint)(cdn_stack_pop (stack) + 0.5);

	ptr2 = cdn_stack_output_ptr (stack) - cdn_stack_arg_size (argdim->args + 1);
	ptr1 = ptr2 - cdn_stack_arg_size (argdim->args + 2);

	if (argdim->args[1].rows == argdim->args[2].rows &&
	    argdim->args[1].columns == argdim->args[2].columns)
	{
		// row/col matrices
		gint i;
		gint n;

		n = cdn_stack_arg_size (argdim->args + 1);

		for (i = 0; i < n; ++i)
		{
			*ptr1 = (gint)(*ptr2++ + 0.5) + ((gint)(*ptr1 + 0.5)) * w;
			++ptr1;
		}

		cdn_stack_popn (stack, n);
	}
	else if (a1)
	{
		gint i;
		gint n;

		n = cdn_stack_arg_size (argdim->args + 1);

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
		gint n;

		n = cdn_stack_arg_size (argdim->args + 2);

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
		gint nr;
		gint nc;
		gint r;
		gint n;
		gdouble *outptr;

		nr = cdn_stack_arg_size (argdim->args + 2);
		nc = cdn_stack_arg_size (argdim->args + 1);

		outptr = cdn_stack_output_ptr (stack);

		// single row and single column combined
		for (r = 0; r < nr; ++r)
		{
			gint c;
			gint si;

			si = (gint)(ptr1[r] + 0.5) * w;

			for (c = 0; c < nc; ++c)
			{
				*outptr++ = si + (gint)(ptr2[c] + 0.5);
			}
		}

		outptr = cdn_stack_output_ptr (stack);

		n = nr * nc;

		// copy back
		memmove (ptr1, outptr, sizeof (gdouble) * n);
		cdn_stack_set_output_ptr (stack, ptr1 + n);
	}
}

static void
op_transpose (CdnStack           *stack,
              CdnStackArgs const *argdim,
              gpointer            userdata)
{
	gint numr;
	gint numc;
	gint start;

	numr = argdim->args[0].rows;
	numc = argdim->args[0].columns;

	if (numr == 1 || numc == 1)
	{
		// noop
		return;
	}

	gdouble *m = cdn_stack_ptr (stack) +
	             cdn_stack_count (stack) -
	             numr * numc;

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

#ifdef HAVE_LAPACK
static void
op_inverse (CdnStack           *stack,
            CdnStackArgs const *argdim,
            gpointer            userdata)
{
	gint n;
	gdouble *ptr;
	gint *ipiv;
	gint nn;

	n = argdim->args[0].rows;
	nn = n * n;

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
op_linsolve (CdnStack           *stack,
             CdnStackArgs const *argdim,
             gpointer            userdata)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gint *ptrIpv;
	gint numa;
	gint numb;

	numa = cdn_stack_arg_size (argdim->args);
	numb = cdn_stack_arg_size (argdim->args + 1);

	ptrA = cdn_stack_output_ptr (stack) - numa;
	ptrB = ptrA - numb;

	// Use the extra space we allocated on the stack to write ipv
	ptrIpv = (gint *)cdn_stack_output_ptr (stack);

	clapack_dgesv (CblasRowMajor,
	               argdim->args[0].rows,
	               argdim->args[1].columns,
	               ptrA,
	               argdim->args[0].columns,
	               ptrIpv,
	               ptrB,
	               argdim->args[0].rows);

	cdn_stack_popn (stack, numa);
}
#endif

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
op_slinsolve (CdnStack           *stack,
              CdnStackArgs const *argdim,
              gpointer            userdata)
{
	gdouble *ptrA;
	gdouble *ptrB;
	gdouble *ptrL;
	gint numa;
	gint numl;
	gint numb;
	gint i;
	gint n;

	numa = cdn_stack_arg_size (argdim->args);
	numl = cdn_stack_arg_size (argdim->args + 1);
	numb = cdn_stack_arg_size (argdim->args + 2);

	ptrA = cdn_stack_output_ptr (stack) - numa;
	ptrL = ptrA - numl;
	ptrB = ptrL - numb;

	n = argdim->args[0].rows;

	// Factorized A in place using LTDL factorization
	slinsolve_factorize (ptrA, ptrL, n);

	for (i = 0; i < argdim->args[2].columns; ++i)
	{
		slinsolve_backsubs (ptrA,
		                    ptrB,
		                    ptrL,
		                    n,
		                    argdim->args[2].columns,
		                    i);
	}

	// Finally pop lambda and A
	cdn_stack_popn (stack, numa + numl);
}

static void
op_size (CdnStack           *stack,
         CdnStackArgs const *argdim,
         gpointer            userdata)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_length (CdnStack           *stack,
           CdnStackArgs const *argdim,
           gpointer            userdata)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_hcat (CdnStack           *stack,
         CdnStackArgs const *argdim,
         gpointer            userdata)
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
	if (argdim->args[0].rows == 1)
	{
		return;
	}

	c1 = argdim->args[1].columns;
	c2 = argdim->args[0].columns;

	s1 = sizeof (gdouble) * c1;
	s2 = sizeof (gdouble) * c2;

	ptr = cdn_stack_output_ptr (stack);
	ptr2 = ptr - argdim->args[0].rows * c2;
	ptr1 = ptr2 - argdim->args[1].rows * c1;

	for (i = 0; i < argdim->args[0].rows; ++i)
	{
		memcpy (ptr, ptr1, s1);

		ptr1 += c1;
		ptr += c1;

		memcpy (ptr, ptr2, s2);

		ptr2 += c2;
		ptr += c2;
	}

	ptr = cdn_stack_output_ptr (stack);

	memcpy (ptr - argdim->args[0].rows * c2 - argdim->args[1].rows * c1,
	        ptr,
	        sizeof (gdouble) *
	        argdim->args[0].rows *
	        (argdim->args[0].columns + argdim->args[1].columns));
}

static void
op_zeros (CdnStack           *stack,
          CdnStackArgs const *argdim,
          gpointer            userdata)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_eye (CdnStack           *stack,
        CdnStackArgs const *argdim,
        gpointer            userdata)
{
	// This will never be executed because it's compiled as a
	// kind of macro
}

static void
op_block (CdnStack           *stack,
          CdnStackArgs const *argdim,
          gpointer            userdata)
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

	numr = argdim->args[1].columns;
	numc = argdim->args[0].columns;

	ptrC = ptr - numc;
	ptrR = ptrC - numr;
	ptrA = ptrR - cdn_stack_arg_size (argdim->args + 2);

	i = 0;
	n = numr * numc;

	for (r = 0; r < numr; ++r)
	{
		gint ridx = (gint)ptrR[r] * argdim->args[2].columns;

		for (c = 0; c < numc; ++c)
		{
			gint cidx = (gint)ptrC[c];

			ptr[i] = ptrA[ridx + cidx];
		}
	}

	memmove (ptrA, ptr, sizeof (gdouble) * n);
	cdn_stack_set_output_ptr (stack, ptrA + n);
}

typedef struct
{
	gchar *name;
	CdnMathFunctionEvaluateFunc function;
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
	{"erf", op_erf, 1, FALSE},
	{"floor", op_floor, 1, FALSE},
	{"ceil", op_ceil, 1, FALSE},
	{"round", op_round, 1, FALSE},
	{"abs", op_abs, 1, FALSE},
	{"pow", op_pow, 2, FALSE},
	{"ln", op_ln, 1, FALSE},
	{"log10", op_log10, 1, FALSE},
	{"hypot", op_hypot, -1, TRUE},
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
	{"lindex", op_lindex, 3, FALSE},
	{"transpose", op_transpose, 1, FALSE},
#ifdef HAVE_LAPACK
	{"inverse", op_inverse, 1, FALSE},
	{"linsolve", op_linsolve, 2, FALSE},
#else
	{"inverse", op_noop, 1, FALSE},
	{"linsolve", op_noop, 2, FALSE},

#endif
	{"slinsolve", op_slinsolve, 3, FALSE},
	{"sum", op_sum, -1, FALSE},
	{"product", op_product, -1, FALSE},
	{"length", op_length, 1, FALSE},
	{"size", op_size, 1, FALSE},
	{"hcat", op_hcat, 2, FALSE},
	{"zeros", op_zeros, -1, FALSE},
	{"eye", op_eye, 1, FALSE},
	{"block", op_block, 3, FALSE}
};

typedef struct
{
	FunctionEntry entry;
	gpointer userdata;
	CdnMathStackManipulationFunc smanipfunc;
	GDestroyNotify destroy_notify;
} FunctionEntryExt;

static GPtrArray *external_function_entries = NULL;

typedef struct
{
	gchar const *name;
	CdnMathFunctionType type;
} FunctionEntryMap;

static FunctionEntryMap function_entry_mapping[] = {
	{"\xe2\x88\x9a", CDN_MATH_FUNCTION_TYPE_SQRT}, /* √ */
	{"\xe2\x88\x91", CDN_MATH_FUNCTION_TYPE_SUM}, /* ∑ */
	{"\xe2\x88\x8f", CDN_MATH_FUNCTION_TYPE_PRODUCT}, /* ∏ */
	{"\xc3\xb7", CDN_MATH_FUNCTION_TYPE_DIVIDE} /* ÷ */
};

static void
function_entry_ext_free (FunctionEntryExt *entry)
{
	g_free (entry->entry.name);

	if (entry->destroy_notify)
	{
		entry->destroy_notify (entry->userdata);
	}

	g_slice_free (FunctionEntryExt, entry);
}

/**
 * cdn_math_register_builtin_function:
 * @name: the name of the builtin function
 * @numargs: the number of arguments of the builtin function
 * @evaluate: (scope notified): a #CdnMathFunctionEvaluateFunc
 * @smanipcb: (scope notified): a #CdnMathStackManipulationFunc
 * @userdata: userdata provided to the evaluate func
 * @destroy_notify: a #GDestroyNotify
 *
 * Register a new builtin math function. After registration, any expression
 * can use the new builtin function.
 *
 * Returns: the id of the newly registered function
 *
 **/
guint
cdn_math_register_builtin_function (gchar const                   *name,
                                    gint                           numargs,
                                    CdnMathFunctionEvaluateFunc    evaluate,
                                    CdnMathStackManipulationFunc   smanipcb,
                                    gpointer                       userdata,
                                    GDestroyNotify                 destroy_notify)
{
	FunctionEntryExt *entry;

	entry = g_slice_new0 (FunctionEntryExt);

	entry->entry.name = g_strdup (name);
	entry->entry.arguments = numargs;
	entry->entry.function = evaluate;

	entry->userdata = userdata;
	entry->smanipfunc = smanipcb;
	entry->destroy_notify = destroy_notify;

	if (!external_function_entries)
	{
		external_function_entries =
			g_ptr_array_new_with_free_func ((GDestroyNotify)function_entry_ext_free);
	}

	g_ptr_array_add (external_function_entries, entry);

	return CDN_MATH_FUNCTION_TYPE_NUM + external_function_entries->len - 1;
}

static FunctionEntry *
lookup_function_entry (CdnMathFunctionType type)
{
	if (type >= CDN_MATH_FUNCTION_TYPE_NUM)
	{
		guint idx;

		idx = type - CDN_MATH_FUNCTION_TYPE_NUM;

		return external_function_entries->pdata[idx];
	}
	else
	{
		return &function_entries[type];
	}
}

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
	FunctionEntry *entry;

	entry = lookup_function_entry (type);

	if (!entry || entry->function == op_noop)
	{
		return NULL;
	}

	if (arguments)
	{
		*arguments = entry->arguments;
	}

	return entry->name;
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

	if (external_function_entries)
	{
		for (i = 0; i < external_function_entries->len; ++i)
		{
			FunctionEntryExt *entry;

			entry = external_function_entries->pdata[i];

			if (g_strcmp0 (entry->entry.name, name) == 0)
			{
				if (arguments)
				{
					*arguments = entry->entry.arguments;
				}

				return CDN_MATH_FUNCTION_TYPE_NUM + i;
			}
		}
	}

	for (i = CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS + 1; i < CDN_MATH_FUNCTION_TYPE_NUM; ++i)
	{
		if (g_strcmp0 (function_entries[i].name, name) == 0)
		{
			if (arguments)
			{
				*arguments = function_entries[i].arguments;
			}

			if (function_entries[i].function == op_noop)
			{
				return 0;
			}

			return i;
		}
	}

	for (i = 0; i < sizeof (function_entry_mapping) / sizeof (FunctionEntryMap); ++i)
	{
		FunctionEntryMap *m = &(function_entry_mapping[i]);

		if (g_strcmp0 (m->name, name) == 0)
		{
			if (arguments)
			{
				*arguments = function_entries[m->type].arguments;
			}

			if (function_entries[m->type].function == op_noop)
			{
				return 0;
			}

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
	FunctionEntry *entry;

	entry = lookup_function_entry (type);

	return entry->arguments == -1;
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
                                  CdnStackArgs const  *argdim)
{
	FunctionEntry *entry;

	entry = lookup_function_entry (type);

	if (!entry->commutative)
	{
		return FALSE;
	}

	switch (type)
	{
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			if ((argdim->args[0].rows == 1 &&
			     argdim->args[0].columns == 1) ||
			    (argdim->args[1].rows == 1 &&
			     argdim->args[1].columns == 1))
			{
				return TRUE;
			}

			if (argdim->args[1].columns != argdim->args[0].columns)
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
                           CdnStackArgs const  *argdim,
                           CdnStack            *stack)
{
	FunctionEntry *entry;
	gpointer userdata;

	if (type >= CDN_MATH_FUNCTION_TYPE_NUM)
	{
		guint idx;

		idx = type - CDN_MATH_FUNCTION_TYPE_NUM;

		entry = external_function_entries->pdata[idx];
		userdata = ((FunctionEntryExt *)entry)->userdata;
	}
	else
	{
		entry = &function_entries[type];
		userdata = NULL;
	}

	entry->function (stack, argdim, userdata);
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
	{"inf", NULL, INFINITY},
	{"\xe2\x88\x9e", NULL, INFINITY} /* ∞ */
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

static void
sparsity_intersect (CdnStackArg const *arg1,
                    CdnStackArg const *arg2,
                    CdnStackArg       *outarg)
{
	gint i1 = 0;
	gint i2 = 0;

	// Sets the sparsity in outarg to the intersection of the sparsity
	// in arg1 and arg2
	cdn_stack_arg_set_sparsity (outarg, NULL, 0);

	while (i1 < arg1->num_sparse && i2 < arg2->num_sparse)
	{
		guint s1;
		guint s2;

		s1 = arg1->sparsity[i1];
		s2 = arg2->sparsity[i2];

		if (s1 == s2)
		{
			if (!outarg->sparsity)
			{
				outarg->sparsity = g_new (guint, MIN (arg1->num_sparse - i1,
				                                      arg2->num_sparse - i2));
			}

			outarg->sparsity[outarg->num_sparse++] = s1;

			++i1;
			++i2;
		}
		else if (s1 > s2)
		{
			++i2;
		}
		else
		{
			++i1;
		}
	}
}

static void
sparsity_transpose (CdnStackArg const *inarg,
                    CdnStackArg       *outarg)
{
	guint *sparsity;
	guint i;

	if (!inarg->num_sparse)
	{
		return;
	}

	sparsity = g_new (guint, inarg->num_sparse);

	for (i = 0; i < inarg->num_sparse; ++i)
	{
		guint idx;
		guint r;
		guint c;

		idx = inarg->sparsity[i];

		r = idx / inarg->columns;
		c = idx % inarg->columns;

		sparsity[i] = c * inarg->columns + r;
	}

	cdn_stack_arg_set_sparsity (outarg,
	                            sparsity,
	                            inarg->num_sparse);

	g_free (sparsity);
}

static void
sparsity_first (CdnStackArg const *arg1,
                CdnStackArg const *arg2,
                CdnStackArg       *outarg)
{
	if (cdn_stack_arg_size (arg1) == 1 && arg1->num_sparse > 0)
	{
		guint *sparsity;
		guint num_sparse;
		guint i;

		num_sparse = cdn_stack_arg_size (outarg);
		sparsity = g_new (guint, num_sparse);

		for (i = 0; i < num_sparse; ++i)
		{
			sparsity[i] = i;
		}

		cdn_stack_arg_set_sparsity (outarg, sparsity, num_sparse);
	}
	else if (cdn_stack_arg_size (arg1) == cdn_stack_arg_size (outarg))
	{
		cdn_stack_arg_set_sparsity (outarg,
		                            g_memdup (arg1->sparsity,
		                                      sizeof (guint) * arg1->num_sparse),
		                            arg1->num_sparse);
	}
}

static guint *
sparsity_make_full (guint n)
{
	guint *ret;
	guint i;

	if (n == 0)
	{
		return NULL;
	}

	ret = g_new (guint, n);

	for (i = 0; i < n; ++i)
	{
		ret[i] = i;
	}

	return ret;
}

static void
sparsity_union (CdnStackArg const *arg1,
                CdnStackArg const *arg2,
                CdnStackArg       *outarg)
{
	if ((cdn_stack_arg_size (arg1) == 1 &&
	     (arg1->num_sparse > 0 || arg2->num_sparse == cdn_stack_arg_size (arg2))) ||
	    (cdn_stack_arg_size (arg2) == 1 &&
	      (arg2->num_sparse > 0 || arg1->num_sparse == cdn_stack_arg_size (arg1))))
	{
		guint n;

		n = cdn_stack_arg_size (outarg);

		cdn_stack_arg_set_sparsity (outarg,
		                            sparsity_make_full (n),
		                            n);
	}
	else if (cdn_stack_arg_size (arg1) == cdn_stack_arg_size (arg2))
	{
		gint i1 = 0;
		gint i2 = 0;

		// Sets the sparsity in outarg to the union of the sparsity
		// in arg1 and arg2
		cdn_stack_arg_set_sparsity (outarg, NULL, 0);

		while (i1 < arg1->num_sparse || i2 < arg2->num_sparse)
		{
			gint s1 = -1;
			gint s2 = -1;

			if (i1 < arg1->num_sparse)
			{
				s1 = arg1->sparsity[i1];
			}

			if (i2 < arg2->num_sparse)
			{
				s2 = arg2->sparsity[i2];
			}

			if (!outarg->sparsity)
			{
				gint num;

				num = MIN (arg1->num_sparse + arg2->num_sparse,
				           cdn_stack_arg_size (outarg));

				outarg->sparsity = g_new (guint, num);
				outarg->num_sparse = 0;
			}

			if (s1 == s2)
			{
				outarg->sparsity[outarg->num_sparse++] = s1;

				++i1;
				++i2;
			}
			else if (s1 == -1 || (s2 != -1 && s1 > s2))
			{
				outarg->sparsity[outarg->num_sparse++] = s2;
				++i2;
			}
			else if (s2 == -1 || s2 > s1)
			{
				outarg->sparsity[outarg->num_sparse++] = s1;
				++i1;
			}
		}
	}
}

static guchar *
make_sparse_matrix (CdnStackArg const *arg)
{
	guchar *ret;
	gint num;
	gint i;

	num = cdn_dimension_size (&arg->dimension);

	ret = g_new0 (guchar, num);

	for (i = 0; i < arg->num_sparse; ++i)
	{
		ret[arg->sparsity[i]] = 1;
	}

	return ret;
}

static void
sparsity_multiply (CdnStackArg const *arg1,
                   CdnStackArg const *arg2,
                   CdnStackArg       *outarg)
{
	if (arg1->columns == arg2->rows &&
	    !(cdn_stack_arg_size (arg1) == 1 && cdn_stack_arg_size (arg2) == 1))
	{
		guchar *s1;
		guchar *s2;
		guchar *s1ptr;
		guint sidx = 0;
		guint *sparsity;
		gint r;

		s1 = make_sparse_matrix (arg1);
		s2 = make_sparse_matrix (arg2);

		s1ptr = s1;
		sparsity = g_new0 (guint, arg1->rows * arg2->columns);

		for (r = 0; r < arg1->rows; ++r)
		{
			guchar *s2ptr;
			gint c;

			s2ptr = s2;

			for (c = 0; c < arg2->columns; ++c)
			{
				gboolean issparse = TRUE;
				gint k;

				for (k = 0; k < arg1->columns; ++k)
				{
					if (!s1ptr[k] && !s2ptr[k * arg2->columns])
					{
						issparse = FALSE;
						break;
					}
				}

				if (issparse)
				{
					sparsity[sidx++] = r * arg2->columns + c;
				}

				++s2ptr;
			}

			s1ptr += arg1->columns;
		}

		cdn_stack_arg_set_sparsity (outarg, sparsity, sidx);

		g_free (sparsity);
		g_free (s1);
		g_free (s2);
	}
	else
	{
		sparsity_union (arg1, arg2, outarg);
	}
}

void
cdn_math_compute_sparsity (CdnMathFunctionType  type,
                           CdnStackArgs const  *inargs,
                           CdnStackArg         *outarg)
{
	if (type >= CDN_MATH_FUNCTION_TYPE_NUM)
	{
		return;
	}

	switch (type)
	{
		case CDN_MATH_FUNCTION_TYPE_FLOOR:
		case CDN_MATH_FUNCTION_TYPE_CEIL:
		case CDN_MATH_FUNCTION_TYPE_ROUND:
		case CDN_MATH_FUNCTION_TYPE_ABS:
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		case CDN_MATH_FUNCTION_TYPE_SIN:
		case CDN_MATH_FUNCTION_TYPE_ASIN:
		case CDN_MATH_FUNCTION_TYPE_TAN:
		case CDN_MATH_FUNCTION_TYPE_ATAN:
		case CDN_MATH_FUNCTION_TYPE_SINH:
		case CDN_MATH_FUNCTION_TYPE_TANH:
			// Single argument functions which propagate sparsity
			cdn_stack_arg_set_sparsity (outarg,
			                            inargs->args[0].sparsity,
			                            inargs->args[0].num_sparse);
		break;
		case CDN_MATH_FUNCTION_TYPE_PLUS:
		case CDN_MATH_FUNCTION_TYPE_MINUS:
			// sparse OP sparse = sparse
			if (cdn_stack_arg_size (inargs->args) == 1 &&
			    inargs->args[0].num_sparse > 0)
			{
				cdn_stack_arg_set_sparsity (outarg,
				                            inargs->args[1].sparsity,
				                            inargs->args[1].num_sparse);
			}
			else if (cdn_stack_arg_size (inargs->args + 1) == 1 &&
			         inargs->args[1].num_sparse > 0)
			{
				cdn_stack_arg_set_sparsity (outarg,
				                            inargs->args[0].sparsity,
				                            inargs->args[0].num_sparse);
			}
			else if (cdn_stack_arg_size (inargs->args) ==
			         cdn_stack_arg_size (inargs->args + 1))
			{
				sparsity_intersect (inargs->args,
				                    inargs->args + 1,
				                    outarg);
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
		case CDN_MATH_FUNCTION_TYPE_POWER:
		case CDN_MATH_FUNCTION_TYPE_POW:
		case CDN_MATH_FUNCTION_TYPE_MODULO:
			// sparse OP N = sparse
			sparsity_first (inargs->args + 1, inargs->args, outarg);
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			sparsity_multiply (inargs->args + 1, inargs->args, outarg);
		break;
		case CDN_MATH_FUNCTION_TYPE_TRANSPOSE:
			sparsity_transpose (inargs->args, outarg);
		break;
		default:
		break;
	}
}

gboolean
cdn_math_function_get_stack_manipulation (CdnMathFunctionType    type,
                                          CdnStackArgs const    *inargs,
                                          CdnStackArg           *outarg,
                                          gint                  *extra_space,
                                          GError               **error)
{
	*extra_space = 0;

	if (type >= CDN_MATH_FUNCTION_TYPE_NUM)
	{
		guint idx;
		FunctionEntryExt *entry;

		idx = type - CDN_MATH_FUNCTION_TYPE_NUM;
		entry = external_function_entries->pdata[idx];

		return entry->smanipfunc (inargs,
		                          outarg,
		                          extra_space,
		                          error);
	}

	// Get the stack manipulation of a particular function given the
	// particular arguments
	switch (type)
	{
		case CDN_MATH_FUNCTION_TYPE_FLOOR:
		case CDN_MATH_FUNCTION_TYPE_CEIL:
		case CDN_MATH_FUNCTION_TYPE_ROUND:
		case CDN_MATH_FUNCTION_TYPE_ABS:
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		case CDN_MATH_FUNCTION_TYPE_SIN:
		case CDN_MATH_FUNCTION_TYPE_SQRT:
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
		case CDN_MATH_FUNCTION_TYPE_COS:
		case CDN_MATH_FUNCTION_TYPE_TAN:
		case CDN_MATH_FUNCTION_TYPE_ASIN:
		case CDN_MATH_FUNCTION_TYPE_ACOS:
		case CDN_MATH_FUNCTION_TYPE_ATAN:
		case CDN_MATH_FUNCTION_TYPE_INVSQRT:
		case CDN_MATH_FUNCTION_TYPE_EXP:
		case CDN_MATH_FUNCTION_TYPE_ERF:
		case CDN_MATH_FUNCTION_TYPE_LN:
		case CDN_MATH_FUNCTION_TYPE_LOG10:
		case CDN_MATH_FUNCTION_TYPE_EXP2:
		case CDN_MATH_FUNCTION_TYPE_SINH:
		case CDN_MATH_FUNCTION_TYPE_COSH:
		case CDN_MATH_FUNCTION_TYPE_TANH:
		case CDN_MATH_FUNCTION_TYPE_SIGN:
			// Operators with one argument simply copy
			cdn_stack_arg_copy (outarg, inargs->args);
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
		case CDN_MATH_FUNCTION_TYPE_ATAN2:
		case CDN_MATH_FUNCTION_TYPE_CSIGN:
			// Math functions with two arguments can operate
			// elementwise (i.e. both arguments are NxM), or
			// one argument is 1-by-1 and the other can be NxM
			if (cdn_stack_arg_size (inargs->args) == 1)
			{
				// Take second arg size
				cdn_stack_arg_copy (outarg, inargs->args + 1);
			}
			else if (cdn_stack_arg_size (inargs->args + 1) != 1 &&
			         cdn_stack_arg_size (inargs->args) != cdn_stack_arg_size (inargs->args + 1))
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot perform element wise operation on arguments of %d-by-%d and %d-by-%d",
				             inargs->args[1].rows, inargs->args[1].columns,
				             inargs->args[0].rows, inargs->args[1].columns);

				return FALSE;
			}
			else
			{
				// Take first arg size
				cdn_stack_arg_copy (outarg, inargs->args);
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			if (cdn_stack_arg_size (inargs->args) == 1)
			{
				// element wise, take second dims
				cdn_stack_arg_copy (outarg, inargs->args + 1);
			}
			else if (cdn_stack_arg_size (inargs->args + 1) == 1)
			{
				// element wise, take first dims
				cdn_stack_arg_copy (outarg, inargs->args);
			}
			else if (inargs->args[0].rows == inargs->args[1].columns)
			{
				// matrix multiplication
				outarg->rows = inargs->args[1].rows;
				outarg->columns = inargs->args[0].columns;

				*extra_space = cdn_stack_arg_size (outarg);
			}
			else if (cdn_stack_arg_size (inargs->args) == cdn_stack_arg_size (inargs->args + 1))
			{
				// element wise, take first arg dimensions
				cdn_stack_arg_copy (outarg, inargs->args + 1);
			}
			else
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot multiply matrices of size (%d, %d) and (%d, %d)",
				             inargs->args[1].rows, inargs->args[1].columns,
				             inargs->args[0].rows, inargs->args[1].columns);

				return FALSE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_TERNARY:
			if (cdn_stack_arg_size (inargs->args + 2) != 1 ||
			    inargs->args[0].rows != inargs->args[1].rows ||
			    inargs->args[0].columns != inargs->args[1].columns)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The `true' and `false' parts of the ternary operator must have the same dimensions (got (%d, %d) and (%d, %d))",
				             inargs->args[1].rows, inargs->args[1].columns,
				             inargs->args[0].rows, inargs->args[0].columns);

				return FALSE;
			}
			else
			{
				cdn_stack_arg_copy (outarg, inargs->args);
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_MIN:
		case CDN_MATH_FUNCTION_TYPE_MAX:
		case CDN_MATH_FUNCTION_TYPE_SUM:
		case CDN_MATH_FUNCTION_TYPE_PRODUCT:
		case CDN_MATH_FUNCTION_TYPE_SQSUM:
			outarg->rows = 1;
			outarg->columns = 1;
		break;
		case CDN_MATH_FUNCTION_TYPE_HYPOT:
			if (inargs->num == 2)
			{
				// Math functions with two arguments can operate
				// elementwise (i.e. both arguments are NxM), or
				// one argument is 1-by-1 and the other can be NxM
				if (cdn_stack_arg_size (inargs->args) == 1)
				{
					// Take second arg size
					cdn_stack_arg_copy (outarg, inargs->args + 1);
				}
				else if (cdn_stack_arg_size (inargs->args + 1) != 1 &&
					 cdn_stack_arg_size (inargs->args) != cdn_stack_arg_size (inargs->args + 1))
				{
					g_set_error (error,
						     CDN_COMPILE_ERROR_TYPE,
						     CDN_COMPILE_ERROR_INVALID_DIMENSION,
						     "Cannot perform element wise operation on arguments of %d-by-%d and %d-by-%d",
						     inargs->args[1].rows, inargs->args[1].columns,
						     inargs->args[0].rows, inargs->args[1].columns);

					return FALSE;
				}
				else
				{
					// Take first arg size
					cdn_stack_arg_copy (outarg, inargs->args);
				}
			}
			else
			{
				outarg->rows = 1;
				outarg->columns = 1;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_LERP:
		case CDN_MATH_FUNCTION_TYPE_CLIP:
		case CDN_MATH_FUNCTION_TYPE_CYCLE:
		{
			CdnDimension thesize = CDN_DIMENSION(1, 1);
			gint prevarg = 0;
			gint i;

			for (i = 0; i < 3; ++i)
			{
				gint argsize = cdn_stack_arg_size (inargs->args + i);

				if (argsize == 1)
				{
					continue;
				}

				if (cdn_dimension_size (&thesize) == 1 || cdn_dimension_size (&thesize) == argsize)
				{
					thesize = inargs->args[i].dimension;
					prevarg = i;
				}
				else
				{
					g_set_error (error,
					             CDN_COMPILE_ERROR_TYPE,
					             CDN_COMPILE_ERROR_INVALID_DIMENSION,
					             "Incompatible dimension for arguments %d and %d (got %d-by-%d and %d-by-%d)",
					             3 - i,
					             3 - prevarg,
					             inargs->args[i].rows,
					             inargs->args[i].columns,
					             inargs->args[prevarg].rows,
					             inargs->args[prevarg].columns);

					return FALSE;
				}
			}

			cdn_stack_arg_copy (outarg, inargs->args + prevarg);
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_INDEX:
			if (inargs->num == 3)
			{
				/* Non linear index */
				gboolean a1;
				gboolean a2;

				a1 = (cdn_stack_arg_size (inargs->args + 2) == 1);
				a2 = (cdn_stack_arg_size (inargs->args + 1) == 1);

				if (!a1 && !a2 &&
				    (inargs->args[1].rows != inargs->args[2].rows ||
				     inargs->args[1].columns != inargs->args[2].columns))
				{
					g_set_error (error,
					             CDN_COMPILE_ERROR_TYPE,
					             CDN_COMPILE_ERROR_INVALID_DIMENSION,
					             "Dimensions of arguments for `index' operator must be the same (got (%d, %d) and (%d, %d))",
					             inargs->args[2].rows,
					             inargs->args[2].columns,
					             inargs->args[1].rows,
					             inargs->args[1].columns);

					return FALSE;
				}

				outarg->rows = a1 ? inargs->args[1].rows : inargs->args[2].rows;
				outarg->columns = a1 ? inargs->args[1].columns : inargs->args[2].columns;
			}
			else
			{
				/* Linear index */
				cdn_stack_arg_copy (outarg, inargs->args + 1);
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_LINDEX:
		{
			gboolean a1;
			gboolean a2;

			a1 = (cdn_stack_arg_size (inargs->args + 2) == 1);
			a2 = (cdn_stack_arg_size (inargs->args + 1) == 1);

			/* acceptable lindex modes:
			 * ========================
			 * indices in matrix form:
			 *   dim(a) == dim(b) ()
			 *
			 * single index with vector
			 *   size(a) == [1, 1] ||
			 *   size(b) == [1, 1]
			 *
			 * indices in row/column form
			 *   size(a, 1) == 1 &&
			 *   size(b, 0) == 1
			 *
			 */

			if (a1 || a2)
			{
				outarg->rows = a1 ? inargs->args[1].rows : inargs->args[2].rows;
				outarg->columns = a1 ? inargs->args[1].columns : inargs->args[2].columns;
			}
			else if (inargs->args[1].rows == inargs->args[2].rows &&
			         inargs->args[1].columns == inargs->args[2].columns)
			{
				cdn_stack_arg_copy (outarg, inargs->args + 1);
			}
			else if (inargs->args[1].rows == 1 && inargs->args[2].columns == 1)
			{
				outarg->rows = inargs->args[2].rows;
				outarg->columns = inargs->args[1].columns;

				*extra_space = inargs->args[1].columns * inargs->args[2].rows;
			}
			else
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Dimensions of `lindex' arguments are inconsistent (got (%d, %d) and (%d, %d))",
				             inargs->args[2].rows, inargs->args[2].columns,
				             inargs->args[1].rows, inargs->args[1].columns);

				return FALSE;
			}
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_TRANSPOSE:
			outarg->rows = inargs->args[0].columns;
			outarg->columns = inargs->args[0].rows;
		break;
		case CDN_MATH_FUNCTION_TYPE_INVERSE:
			if (inargs->args->rows != inargs->args->columns)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot invert a non square matrix (%d, %d)",
				             inargs->args->rows, inargs->args->columns);

				return FALSE;
			}

			cdn_stack_arg_copy (outarg, inargs->args);
			*extra_space = inargs->args[0].rows;
		break;
		case CDN_MATH_FUNCTION_TYPE_LINSOLVE:
			// A x = B with A the second arg and B the first
			if (inargs->args[0].rows != inargs->args[0].columns)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot solve a system which is not square (%d, %d)",
				             inargs->args[0].rows, inargs->args[0].columns);

				return FALSE;
			}

			if (inargs->args[0].rows != inargs->args[1].rows)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of B (in Ax = B), expected `%d' rows but got `%d' rows",
				             inargs->args[0].rows, inargs->args[1].rows);

				return FALSE;
			}

			cdn_stack_arg_copy (outarg, inargs->args + 1);

			// Need extra space to store the pivoting coefficients
			*extra_space = inargs->args[0].rows;
		break;
		case CDN_MATH_FUNCTION_TYPE_SLINSOLVE:
			// A x = B, λ
			// with order of arguments: B, λ, A
			if (inargs->args[0].rows != inargs->args[0].columns)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot solve a system which is not square (%d, %d)",
				             inargs->args[0].rows, inargs->args[0].columns);

				return FALSE;
			}

			if (inargs->args[0].rows != inargs->args[2].rows)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of B (in A(λ, λ)x = B(λ)), expected `%d' rows but got `%d' rows",
				             inargs->args[0].rows, inargs->args[2].rows);

				return FALSE;
			}

			if (inargs->args[0].rows != inargs->args[1].rows)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Invalid dimensions of λ (in A(λ, λ)x = B(λ)), expected `%d' rows but got `%d' rows",
				             inargs->args[0].rows, inargs->args[1].rows);

				return FALSE;
			}

			if (inargs->args[2].columns != 1)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The number of columns of λ must 1 (got `%d')",
				             inargs->args[2].columns);

				return FALSE;
			}

			cdn_stack_arg_copy (outarg, inargs->args + 1);
		break;
		case CDN_MATH_FUNCTION_TYPE_LENGTH:
			outarg->rows = 1;
			outarg->columns = 1;
		break;
		case CDN_MATH_FUNCTION_TYPE_SIZE:
			outarg->rows = 1;
			outarg->columns = 2;
		break;
		case CDN_MATH_FUNCTION_TYPE_HCAT:
			if (inargs->args[0].rows != inargs->args[1].rows)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "Cannot concat %d rows with %d rows",
				             inargs->args[0].rows, inargs->args[0].columns);

				return FALSE;
			}

			outarg->rows = inargs->args[0].rows;
			outarg->columns = inargs->args[0].columns + inargs->args[1].columns;

			*extra_space = cdn_stack_arg_size (outarg);
		break;
		case CDN_MATH_FUNCTION_TYPE_BLOCK:
			if (inargs->args[0].rows != 1 || inargs->args[1].rows != 1 ||
			    inargs->args[0].columns != inargs->args[1].columns)
			{
				g_set_error (error,
				             CDN_COMPILE_ERROR_TYPE,
				             CDN_COMPILE_ERROR_INVALID_DIMENSION,
				             "The row and column indices for the block function must be 1-by-N but got %d-by-%d rows and %d-by-%d columns",
				             inargs->args[1].rows, inargs->args[1].columns,
				             inargs->args[0].rows, inargs->args[0].columns);

				return FALSE;
			}

			outarg->rows = inargs->args[1].columns;
			outarg->columns = inargs->args[0].columns;

			*extra_space = cdn_stack_arg_size (outarg);
		break;
		default:
			return FALSE;
	}

	cdn_math_compute_sparsity (type, inargs, outarg);
	return TRUE;
}
