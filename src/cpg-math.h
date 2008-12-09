#ifndef __CPG_MATH_H__
#define __CPG_MATH_H__

#include "cpg-stack.h"

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

#ifndef RTLINUX
unsigned cpg_math_function_lookup(char const *name, int *arguments);
double cpg_math_constant_lookup(char const *name, int *found);
#endif

unsigned cpg_math_operator_lookup(CpgMathOperatorType type);

void cpg_math_function_execute(unsigned id, CpgStack *stack, void *data);
void cpg_math_operator_execute(unsigned id, CpgStack *stack, void *data);

#endif /* __CPG_MATH_H__ */

