/*
 * cpg-math.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_MATH_H__
#define __CPG_MATH_H__

#include <cpg-network/cpg-stack.h>

G_BEGIN_DECLS

/**
 * CpgMathOperatorType:
 * @CPG_MATH_OPERATOR_TYPE_NONE: none
 * @CPG_MATH_OPERATOR_TYPE_UNARY_MINUS: unary minus
 * @CPG_MATH_OPERATOR_TYPE_MINUS: minus
 * @CPG_MATH_OPERATOR_TYPE_PLUS: plus
 * @CPG_MATH_OPERATOR_TYPE_MULTIPLY: multiply
 * @CPG_MATH_OPERATOR_TYPE_DIVIDE: divide
 * @CPG_MATH_OPERATOR_TYPE_MODULO: modulo
 * @CPG_MATH_OPERATOR_TYPE_POWER: power
 * @CPG_MATH_OPERATOR_TYPE_GREATER: greater
 * @CPG_MATH_OPERATOR_TYPE_LESS: less
 * @CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL: greater or equal
 * @CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL: less or equal
 * @CPG_MATH_OPERATOR_TYPE_EQUAL: equal
 * @CPG_MATH_OPERATOR_TYPE_OR: or
 * @CPG_MATH_OPERATOR_TYPE_AND: and
 * @CPG_MATH_OPERATOR_TYPE_NEGATE: negate
 * @CPG_MATH_OPERATOR_TYPE_TERNARY: ternary
 * @CPG_MATH_OPERATOR_TYPE_NUM: number of operators
 *
 * Operator types.
 *
 */
typedef enum
{
	CPG_MATH_OPERATOR_TYPE_NONE = 0,
	CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	CPG_MATH_OPERATOR_TYPE_MINUS,
	CPG_MATH_OPERATOR_TYPE_PLUS,
	CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	CPG_MATH_OPERATOR_TYPE_DIVIDE,
	CPG_MATH_OPERATOR_TYPE_MODULO,
	CPG_MATH_OPERATOR_TYPE_POWER,
	CPG_MATH_OPERATOR_TYPE_GREATER,
	CPG_MATH_OPERATOR_TYPE_LESS,
	CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL,
	CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL,
	CPG_MATH_OPERATOR_TYPE_EQUAL,
	CPG_MATH_OPERATOR_TYPE_OR,
	CPG_MATH_OPERATOR_TYPE_AND,
	CPG_MATH_OPERATOR_TYPE_NEGATE,
	CPG_MATH_OPERATOR_TYPE_TERNARY,
	CPG_MATH_OPERATOR_TYPE_NUM
} CpgMathOperatorType;

/**
 * CpgMathFunctionType:
 * @CPG_MATH_FUNCTION_TYPE_NONE: none
 * @CPG_MATH_FUNCTION_TYPE_SIN: sine
 * @CPG_MATH_FUNCTION_TYPE_COS: cosine
 * @CPG_MATH_FUNCTION_TYPE_TAN: tangent
 * @CPG_MATH_FUNCTION_TYPE_ASIN: arc sine
 * @CPG_MATH_FUNCTION_TYPE_ACOS: arc cosine
 * @CPG_MATH_FUNCTION_TYPE_ATAN: arc tangent
 * @CPG_MATH_FUNCTION_TYPE_ATAN2: arc tangent 2
 * @CPG_MATH_FUNCTION_TYPE_SQRT: square root
 * @CPG_MATH_FUNCTION_TYPE_INVSQRT: inverse sequare root
 * @CPG_MATH_FUNCTION_TYPE_MIN: min
 * @CPG_MATH_FUNCTION_TYPE_MAX: max
 * @CPG_MATH_FUNCTION_TYPE_EXP: exponential
 * @CPG_MATH_FUNCTION_TYPE_FLOOR: floor
 * @CPG_MATH_FUNCTION_TYPE_CEIL: ceil
 * @CPG_MATH_FUNCTION_TYPE_ROUND: round
 * @CPG_MATH_FUNCTION_TYPE_ABS: absolute
 * @CPG_MATH_FUNCTION_TYPE_POW: power
 * @CPG_MATH_FUNCTION_TYPE_RAND: random
 * @CPG_MATH_FUNCTION_TYPE_LN: natural logarithm
 * @CPG_MATH_FUNCTION_TYPE_LOG10: base 10 logarithm
 * @CPG_MATH_FUNCTION_TYPE_HYPOT: euclidean distance
 * @CPG_MATH_FUNCTION_TYPE_EXP2: base-2 exponential
 * @CPG_MATH_FUNCTION_TYPE_SINH: hyperbolic sine
 * @CPG_MATH_FUNCTION_TYPE_COSH: hyperbolic consine
 * @CPG_MATH_FUNCTION_TYPE_TANH: hyperbolic tangent
 * @CPG_MATH_FUNCTION_TYPE_LERP: linear interpolation
 * @CPG_MATH_FUNCTION_TYPE_SQSUM: squared sum
 * @CPG_MATH_FUNCTION_TYPE_SIGN: sign
 * @CPG_MATH_FUNCTION_TYPE_CSIGN: copy sign
 * @CPG_MATH_FUNCTION_TYPE_CLIP: clip
 * @CPG_MATH_FUNCTION_TYPE_CYCLE: cycle
 * @CPG_MATH_FUNCTION_TYPE_NUM: number of function types
 *
 * Function types.
 *
 */
typedef enum
{
	CPG_MATH_FUNCTION_TYPE_NONE = 0,
	CPG_MATH_FUNCTION_TYPE_SIN,
	CPG_MATH_FUNCTION_TYPE_COS,
	CPG_MATH_FUNCTION_TYPE_TAN,
	CPG_MATH_FUNCTION_TYPE_ASIN,
	CPG_MATH_FUNCTION_TYPE_ACOS,
	CPG_MATH_FUNCTION_TYPE_ATAN,
	CPG_MATH_FUNCTION_TYPE_ATAN2,
	CPG_MATH_FUNCTION_TYPE_SQRT,
	CPG_MATH_FUNCTION_TYPE_INVSQRT,
	CPG_MATH_FUNCTION_TYPE_MIN,
	CPG_MATH_FUNCTION_TYPE_MAX,
	CPG_MATH_FUNCTION_TYPE_EXP,
	CPG_MATH_FUNCTION_TYPE_FLOOR,
	CPG_MATH_FUNCTION_TYPE_CEIL,
	CPG_MATH_FUNCTION_TYPE_ROUND,
	CPG_MATH_FUNCTION_TYPE_ABS,
	CPG_MATH_FUNCTION_TYPE_POW,
	CPG_MATH_FUNCTION_TYPE_RAND,
	CPG_MATH_FUNCTION_TYPE_LN,
	CPG_MATH_FUNCTION_TYPE_LOG10,
	CPG_MATH_FUNCTION_TYPE_HYPOT,
	CPG_MATH_FUNCTION_TYPE_EXP2,
	CPG_MATH_FUNCTION_TYPE_SINH,
	CPG_MATH_FUNCTION_TYPE_COSH,
	CPG_MATH_FUNCTION_TYPE_TANH,
	CPG_MATH_FUNCTION_TYPE_LERP,
	CPG_MATH_FUNCTION_TYPE_SQSUM,
	CPG_MATH_FUNCTION_TYPE_SIGN,
	CPG_MATH_FUNCTION_TYPE_CSIGN,
	CPG_MATH_FUNCTION_TYPE_CLIP,
	CPG_MATH_FUNCTION_TYPE_CYCLE,
	CPG_MATH_FUNCTION_TYPE_NUM
} CpgMathFunctionType;

CpgMathFunctionType  cpg_math_function_lookup      (const gchar         *name,
                                                    gint                *arguments);
gdouble              cpg_math_constant_lookup      (const gchar         *name,
                                                    gboolean            *found);
CpgMathOperatorType  cpg_math_operator_lookup      (CpgMathOperatorType  type);
void                 cpg_math_function_execute     (CpgMathFunctionType  type,
                                                    gint                 numargs,
                                                    CpgStack            *stack);
void                 cpg_math_operator_execute     (CpgMathOperatorType  type,
                                                    gint                 numargs,
                                                    CpgStack            *stack);
gboolean             cpg_math_function_is_constant (CpgMathFunctionType  type);
gboolean             cpg_math_operator_is_constant (CpgMathOperatorType  type);

gboolean             cpg_math_function_is_variable (CpgMathFunctionType  type);
gboolean             cpg_math_operator_is_variable (CpgMathOperatorType  type);

gboolean             cpg_math_function_is_commutative (CpgMathFunctionType  type);
gboolean             cpg_math_operator_is_commutative (CpgMathOperatorType  type);

const gchar        *cpg_math_function_lookup_by_id (CpgMathFunctionType  type,
                                                    gint                *arguments);

G_END_DECLS

#endif /* __CPG_MATH_H__ */

