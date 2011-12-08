/*
 * cdn-math.h
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

#ifndef __CDN_MATH_H__
#define __CDN_MATH_H__

#include <codyn/cdn-stack.h>

G_BEGIN_DECLS

/**
 * CdnMathFunctionType:
 * @CDN_MATH_FUNCTION_TYPE_NONE: none
 * @CDN_MATH_FUNCTION_TYPE_UNARY_MINUS: unary minus
 * @CDN_MATH_FUNCTION_TYPE_MINUS: minus
 * @CDN_MATH_FUNCTION_TYPE_PLUS: plus
 * @CDN_MATH_FUNCTION_TYPE_MULTIPLY: multiply
 * @CDN_MATH_FUNCTION_TYPE_EMULTIPLY: emultiply
 * @CDN_MATH_FUNCTION_TYPE_DIVIDE: divide
 * @CDN_MATH_FUNCTION_TYPE_EDIVIDE: edivide
 * @CDN_MATH_FUNCTION_TYPE_MODULO: modulo
 * @CDN_MATH_FUNCTION_TYPE_POWER: power
 * @CDN_MATH_FUNCTION_TYPE_GREATER: greater
 * @CDN_MATH_FUNCTION_TYPE_LESS: less
 * @CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL: greater or equal
 * @CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL: less or equal
 * @CDN_MATH_FUNCTION_TYPE_EQUAL: equal
 * @CDN_MATH_FUNCTION_TYPE_OR: or
 * @CDN_MATH_FUNCTION_TYPE_AND: and
 * @CDN_MATH_FUNCTION_TYPE_NEGATE: negate
 * @CDN_MATH_FUNCTION_TYPE_TERNARY: ternary
 * @CDN_MATH_FUNCTION_TYPE_TILDE: tilde
 * @CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS: num operators
 * @CDN_MATH_FUNCTION_TYPE_SIN: sine
 * @CDN_MATH_FUNCTION_TYPE_COS: cosine
 * @CDN_MATH_FUNCTION_TYPE_TAN: tangent
 * @CDN_MATH_FUNCTION_TYPE_ASIN: arc sine
 * @CDN_MATH_FUNCTION_TYPE_ACOS: arc cosine
 * @CDN_MATH_FUNCTION_TYPE_ATAN: arc tangent
 * @CDN_MATH_FUNCTION_TYPE_ATAN2: arc tangent 2
 * @CDN_MATH_FUNCTION_TYPE_SQRT: square root
 * @CDN_MATH_FUNCTION_TYPE_INVSQRT: inverse sequare root
 * @CDN_MATH_FUNCTION_TYPE_MIN: min
 * @CDN_MATH_FUNCTION_TYPE_MAX: max
 * @CDN_MATH_FUNCTION_TYPE_EXP: exponential
 * @CDN_MATH_FUNCTION_TYPE_FLOOR: floor
 * @CDN_MATH_FUNCTION_TYPE_CEIL: ceil
 * @CDN_MATH_FUNCTION_TYPE_ROUND: round
 * @CDN_MATH_FUNCTION_TYPE_ABS: absolute
 * @CDN_MATH_FUNCTION_TYPE_POW: power
 * @CDN_MATH_FUNCTION_TYPE_LN: natural logarithm
 * @CDN_MATH_FUNCTION_TYPE_LOG10: base 10 logarithm
 * @CDN_MATH_FUNCTION_TYPE_HYPOT: euclidean distance
 * @CDN_MATH_FUNCTION_TYPE_EXP2: base-2 exponential
 * @CDN_MATH_FUNCTION_TYPE_SINH: hyperbolic sine
 * @CDN_MATH_FUNCTION_TYPE_COSH: hyperbolic consine
 * @CDN_MATH_FUNCTION_TYPE_TANH: hyperbolic tangent
 * @CDN_MATH_FUNCTION_TYPE_LERP: linear interpolation
 * @CDN_MATH_FUNCTION_TYPE_SQSUM: squared sum
 * @CDN_MATH_FUNCTION_TYPE_SIGN: sign
 * @CDN_MATH_FUNCTION_TYPE_CSIGN: copy sign
 * @CDN_MATH_FUNCTION_TYPE_CLIP: clip
 * @CDN_MATH_FUNCTION_TYPE_CYCLE: cycle
 * @CDN_MATH_FUNCTION_TYPE_INDEX: index
 * @CDN_MATH_FUNCTION_TYPE_TRANSPOSE: transpose
 * @CDN_MATH_FUNCTION_TYPE_INVERSE: inverse
 * @CDN_MATH_FUNCTION_TYPE_LINSOLVE: linsolve
 * @CDN_MATH_FUNCTION_TYPE_CROSS: cross
 * @CDN_MATH_FUNCTION_TYPE_SUM: sum
 * @CDN_MATH_FUNCTION_TYPE_PRODUCT: product
 * @CDN_MATH_FUNCTION_TYPE_NUM: number of function types
 *
 * Function types.
 *
 */
typedef enum
{
	CDN_MATH_FUNCTION_TYPE_NONE = 0,
	CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	CDN_MATH_FUNCTION_TYPE_MINUS,
	CDN_MATH_FUNCTION_TYPE_PLUS,
	CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	CDN_MATH_FUNCTION_TYPE_EMULTIPLY,
	CDN_MATH_FUNCTION_TYPE_DIVIDE,
	CDN_MATH_FUNCTION_TYPE_MODULO,
	CDN_MATH_FUNCTION_TYPE_POWER,
	CDN_MATH_FUNCTION_TYPE_TILDE,
	CDN_MATH_FUNCTION_TYPE_GREATER,
	CDN_MATH_FUNCTION_TYPE_LESS,
	CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL,
	CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL,
	CDN_MATH_FUNCTION_TYPE_EQUAL,
	CDN_MATH_FUNCTION_TYPE_OR,
	CDN_MATH_FUNCTION_TYPE_AND,
	CDN_MATH_FUNCTION_TYPE_NEGATE,
	CDN_MATH_FUNCTION_TYPE_TERNARY,
	CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS,
	CDN_MATH_FUNCTION_TYPE_SIN,
	CDN_MATH_FUNCTION_TYPE_COS,
	CDN_MATH_FUNCTION_TYPE_TAN,
	CDN_MATH_FUNCTION_TYPE_ASIN,
	CDN_MATH_FUNCTION_TYPE_ACOS,
	CDN_MATH_FUNCTION_TYPE_ATAN,
	CDN_MATH_FUNCTION_TYPE_ATAN2,
	CDN_MATH_FUNCTION_TYPE_SQRT,
	CDN_MATH_FUNCTION_TYPE_INVSQRT,
	CDN_MATH_FUNCTION_TYPE_MIN,
	CDN_MATH_FUNCTION_TYPE_MAX,
	CDN_MATH_FUNCTION_TYPE_EXP,
	CDN_MATH_FUNCTION_TYPE_FLOOR,
	CDN_MATH_FUNCTION_TYPE_CEIL,
	CDN_MATH_FUNCTION_TYPE_ROUND,
	CDN_MATH_FUNCTION_TYPE_ABS,
	CDN_MATH_FUNCTION_TYPE_POW,
	CDN_MATH_FUNCTION_TYPE_LN,
	CDN_MATH_FUNCTION_TYPE_LOG10,
	CDN_MATH_FUNCTION_TYPE_HYPOT,
	CDN_MATH_FUNCTION_TYPE_EXP2,
	CDN_MATH_FUNCTION_TYPE_SINH,
	CDN_MATH_FUNCTION_TYPE_COSH,
	CDN_MATH_FUNCTION_TYPE_TANH,
	CDN_MATH_FUNCTION_TYPE_LERP,
	CDN_MATH_FUNCTION_TYPE_SQSUM,
	CDN_MATH_FUNCTION_TYPE_SIGN,
	CDN_MATH_FUNCTION_TYPE_CSIGN,
	CDN_MATH_FUNCTION_TYPE_CLIP,
	CDN_MATH_FUNCTION_TYPE_CYCLE,
	CDN_MATH_FUNCTION_TYPE_INDEX,
	CDN_MATH_FUNCTION_TYPE_TRANSPOSE,
	CDN_MATH_FUNCTION_TYPE_INVERSE,
	CDN_MATH_FUNCTION_TYPE_LINSOLVE,
	CDN_MATH_FUNCTION_TYPE_SUM,
	CDN_MATH_FUNCTION_TYPE_PRODUCT,
	CDN_MATH_FUNCTION_TYPE_NUM
} CdnMathFunctionType;

CdnMathFunctionType  cdn_math_function_lookup      (const gchar         *name,
                                                    gint                *arguments);
gdouble              cdn_math_constant_lookup      (const gchar         *name,
                                                    gboolean            *found);

void                 cdn_math_function_execute     (CdnMathFunctionType  type,
                                                    gint                 numargs,
                                                    gint                *argdim,
                                                    CdnStack            *stack);

gboolean             cdn_math_function_is_variable (CdnMathFunctionType  type);
gboolean             cdn_math_function_is_commutative (CdnMathFunctionType  type);

const gchar         *cdn_math_function_lookup_by_id (CdnMathFunctionType  type,
                                                     gint                *arguments);

gboolean             cdn_math_function_get_stack_manipulation (CdnMathFunctionType   type,
                                                               gint                  arguments,
                                                               gint                 *argdim,
                                                               gint                 *outargdim,
                                                               gint                 *extra_space,
                                                               GError              **error);

G_END_DECLS

#endif /* __CDN_MATH_H__ */

