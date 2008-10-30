#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include "cpg-object.h"
#include "cpg-property.h"

struct _CpgLink;

typedef struct _CpgExpression CpgExpression;
typedef struct _CpgInstruction CpgInstruction;
typedef struct _CpgExpressionStack CpgExpressionStack;
typedef struct _CpgOutput CpgOutput;

struct _CpgExpression
{
	// Property we act on, easy reference, fast
	CpgProperty *destination;
	
	// The link
	struct _CpgLink *link;
	
	// Expression to evaluate
	char *expression;
	
	CpgInstruction *instructions;
	double *output_ptr;
	double *output;
	unsigned num_output;
};

typedef void (*CpgFunctionClosure)(CpgExpression *);

CpgExpression 	*cpg_expression_new				(char const *expression);
CpgExpression	*cpg_expression_new_for_link	(struct _CpgLink *link, CpgProperty *destination, char const *expression);
CpgExpression	*cpg_expression_copy			(CpgExpression *expression);
void 			 cpg_expression_free			(CpgExpression *expression);

int				 cpg_expression_parse			(CpgExpression *expression, CpgObject *context, char **error);
double 			 cpg_expression_evaluate		(CpgExpression *expression);
void			 cpg_expression_set_value		(CpgExpression *expression, double value);

double 			 cpg_expression_pop				(CpgExpression *expression);
void 			 cpg_expression_push			(CpgExpression *expression, double value);

void			 cpg_expression_print_instructions(CpgExpression *expression, FILE *f);
#endif /* __CPG_EXPRESSION_H__ */
