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
 * @CPG_MATH_OPERATOR_NUM: number of operators
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
	CPG_MATH_OPERATOR_NUM
} CpgMathOperatorType;

/**
 * CpgMathFunctionType:
 * @CPG_FUNCTION_OPERATOR_TYPE_NONE: none
 * @CPG_FUNCTION_OPERATOR_TYPE_SIN: sine
 * @CPG_FUNCTION_OPERATOR_TYPE_COS: cosine
 * @CPG_FUNCTION_OPERATOR_TYPE_TAN: tangent
 * @CPG_FUNCTION_OPERATOR_TYPE_ASIN: arc sine
 * @CPG_FUNCTION_OPERATOR_TYPE_ACOS: arc cosine
 * @CPG_FUNCTION_OPERATOR_TYPE_ATAN: arc tangent
 * @CPG_FUNCTION_OPERATOR_TYPE_ATAN2: arc tangent 2
 * @CPG_FUNCTION_OPERATOR_TYPE_SQRT: square root
 * @CPG_FUNCTION_OPERATOR_TYPE_INVSQRT: inverse sequare root
 * @CPG_FUNCTION_OPERATOR_TYPE_MIN: min
 * @CPG_FUNCTION_OPERATOR_TYPE_MAX: max
 * @CPG_FUNCTION_OPERATOR_TYPE_EXP: exponential
 * @CPG_FUNCTION_OPERATOR_TYPE_FLOOR: floor
 * @CPG_FUNCTION_OPERATOR_TYPE_CEIL: ceil
 * @CPG_FUNCTION_OPERATOR_TYPE_ROUND: round
 * @CPG_FUNCTION_OPERATOR_TYPE_ABS: absolute
 * @CPG_FUNCTION_OPERATOR_TYPE_POW: power
 * @CPG_FUNCTION_OPERATOR_TYPE_RAND: random
 * @CPG_FUNCTION_OPERATOR_TYPE_LN: natural logarithm
 * @CPG_FUNCTION_OPERATOR_TYPE_LOG10: base 10 logarithm
 * @CPG_FUNCTION_OPERATOR_TYPE_HYPOT: euclidean distance
 * @CPG_FUNCTION_OPERATOR_TYPE_EXP2: base-2 exponential
 * @CPG_FUNCTION_OPERATOR_TYPE_SINH: hyperbolic sine
 * @CPG_FUNCTION_OPERATOR_TYPE_COSH: hyperbolic consine
 * @CPG_FUNCTION_OPERATOR_TYPE_TANH: hyperbolic tangent
 * @CPG_FUNCTION_OPERATOR_TYPE_LERP: linear interpolation
 * @CPG_FUNCTION_OPERATOR_TYPE_SQSUM: squared sum
 * @CPG_FUNCTION_OPERATOR_NUM: number of function types
 *
 * Function types.
 *
 */
typedef enum
{
	CPG_FUNCTION_OPERATOR_TYPE_NONE = 0,
	CPG_FUNCTION_OPERATOR_TYPE_SIN,
	CPG_FUNCTION_OPERATOR_TYPE_COS,
	CPG_FUNCTION_OPERATOR_TYPE_TAN,
	CPG_FUNCTION_OPERATOR_TYPE_ASIN,
	CPG_FUNCTION_OPERATOR_TYPE_ACOS,
	CPG_FUNCTION_OPERATOR_TYPE_ATAN,
	CPG_FUNCTION_OPERATOR_TYPE_ATAN2,
	CPG_FUNCTION_OPERATOR_TYPE_SQRT,
	CPG_FUNCTION_OPERATOR_TYPE_INVSQRT,
	CPG_FUNCTION_OPERATOR_TYPE_MIN,
	CPG_FUNCTION_OPERATOR_TYPE_MAX,
	CPG_FUNCTION_OPERATOR_TYPE_EXP,
	CPG_FUNCTION_OPERATOR_TYPE_FLOOR,
	CPG_FUNCTION_OPERATOR_TYPE_CEIL,
	CPG_FUNCTION_OPERATOR_TYPE_ROUND,
	CPG_FUNCTION_OPERATOR_TYPE_ABS,
	CPG_FUNCTION_OPERATOR_TYPE_POW,
	CPG_FUNCTION_OPERATOR_TYPE_RAND,
	CPG_FUNCTION_OPERATOR_TYPE_LN,
	CPG_FUNCTION_OPERATOR_TYPE_LOG10,
	CPG_FUNCTION_OPERATOR_TYPE_HYPOT,
	CPG_FUNCTION_OPERATOR_TYPE_EXP2,
	CPG_FUNCTION_OPERATOR_TYPE_SINH,
	CPG_FUNCTION_OPERATOR_TYPE_COSH,
	CPG_FUNCTION_OPERATOR_TYPE_TANH,
	CPG_FUNCTION_OPERATOR_TYPE_LERP,
	CPG_FUNCTION_OPERATOR_TYPE_SQSUM,
	CPG_FUNCTION_OPERATOR_NUM
} CpgMathFunctionType;

CpgMathFunctionType  cpg_math_function_lookup      (gchar const         *name,
                                                    gint                *arguments);
gdouble              cpg_math_constant_lookup      (gchar const         *name,
                                                    gboolean            *found);
CpgMathOperatorType  cpg_math_operator_lookup      (CpgMathOperatorType  type);
void                 cpg_math_function_execute     (CpgMathFunctionType  type,
                                                    CpgStack            *stack);
void                 cpg_math_operator_execute     (CpgMathOperatorType  type,
                                                    CpgStack            *stack);
gboolean             cpg_math_function_is_constant (CpgMathFunctionType  type);
gboolean             cpg_math_operator_is_constant (CpgMathOperatorType  type);
gboolean             cpg_math_function_is_variable (CpgMathFunctionType  type);
gboolean             cpg_math_operator_is_variable (CpgMathOperatorType  type);

gchar const        *cpg_math_function_lookup_by_id (CpgMathFunctionType  type,
                                                    gint                *arguments);

G_END_DECLS

#endif /* __CPG_MATH_H__ */

