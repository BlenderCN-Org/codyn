#ifndef __CPG_EXPRESSION_PRIVATE_H__
#define __CPG_EXPRESSION_PRIVATE_H__

#include "cpg-expression.h"

typedef struct _CpgInstruction CpgInstruction;

struct _CpgExpression
{	
	// Expression to evaluate
	char *expression;
	
	CpgInstruction *instructions;
	double *output_ptr;
	double *output;
	unsigned num_output;
	
	double cached_output;
	int has_cache;
};

void			cpg_expression_set				(CpgExpression *expression, char const *value);
void 			cpg_expression_reset_cache		(CpgExpression *expression);

#endif /* __CPG_EXPRESSION_PRIVATE_H__ */
