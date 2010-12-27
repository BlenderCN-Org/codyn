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
	CPG_MATH_FUNCTION_TYPE_NUM
} CpgMathFunctionType;

CpgMathFunctionType  cpg_math_function_lookup      (const gchar         *name,
                                                    gint                *arguments);
gdouble              cpg_math_constant_lookup      (const gchar         *name,
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

const gchar        *cpg_math_function_lookup_by_id (CpgMathFunctionType  type,
                                                    gint                *arguments);

G_END_DECLS

#endif /* __CPG_MATH_H__ */

