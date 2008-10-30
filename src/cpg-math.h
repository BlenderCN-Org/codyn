#ifndef __CPG_MATH_H__
#define __CPG_MATH_H__

#include "cpg-expression.h"

typedef struct
{
	char const *name;
	CpgFunctionClosure closure;
	double value;
} CpgConstantEntry;

CpgFunctionClosure cpg_math_function_lookup(char const *name, int *arguments);
CpgConstantEntry const *cpg_math_constant_lookup(char const *name);

#endif /* __CPG_MATH_H__ */

