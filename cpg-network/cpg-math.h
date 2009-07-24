#ifndef __CPG_MATH_H__
#define __CPG_MATH_H__

#include <cpg-network/cpg-stack.h>

G_BEGIN_DECLS

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
	CPG_MATH_OPERATOR_TYPE_TERNARY
} CpgMathOperatorType;

typedef enum
{
	CPG_FUNCTION_OPERATOR_TYPE_NONE = 0,
	CPG_FUNCTION_OPERATOR_TYPE_SIN,
	CPG_FUNCTION_OPERATOR_TYPE_COS,
	CPG_FUNCTION_OPERATOR_TYPE_TAN,
	CPG_FUNCTION_OPERATOR_TYPE_ASIN,
	CPG_FUNCTION_OPERATOR_TYPE_ACOS,
	CPG_FUNCTION_OPERATOR_TYPE_ATAN,
	CPG_FUNCTION_OPERATOR_TYPE_SQRT,
	CPG_FUNCTION_OPERATOR_TYPE_MIN,
	CPG_FUNCTION_OPERATOR_TYPE_MAX,
	CPG_FUNCTION_OPERATOR_TYPE_EXP,
	CPG_FUNCTION_OPERATOR_TYPE_FLOOR,
	CPG_FUNCTION_OPERATOR_TYPE_CEIL,
	CPG_FUNCTION_OPERATOR_TYPE_ROUND,
	CPG_FUNCTION_OPERATOR_TYPE_ABS,
	CPG_FUNCTION_OPERATOR_TYPE_POW,
	CPG_FUNCTION_OPERATOR_TYPE_RAND
} CpgMathFunctionType;

CpgMathFunctionType cpg_math_function_lookup (gchar const *name, gint *arguments);
gdouble cpg_math_constant_lookup (gchar const *name, gboolean *found);

CpgMathOperatorType cpg_math_operator_lookup(CpgMathOperatorType type);

void cpg_math_function_execute(CpgMathFunctionType id, CpgStack *stack, void *data);
void cpg_math_operator_execute(CpgMathOperatorType id, CpgStack *stack, void *data);

gboolean cpg_math_function_is_constant (CpgMathFunctionType id);
gboolean cpg_math_operator_is_constant (CpgMathOperatorType id);

gboolean cpg_math_function_is_variable (CpgMathFunctionType id);
gboolean cpg_math_operator_is_variable (CpgMathOperatorType id);

G_END_DECLS

#endif /* __CPG_MATH_H__ */

