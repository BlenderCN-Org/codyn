#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include "cpg-object.h"

typedef struct _CpgExpression 		CpgExpression;
typedef struct _CpgContext			CpgContext;

struct _CpgContext
{
	CpgObject *object;
	CpgContext *next;
};

CpgExpression 	*cpg_expression_new				(char const *expression);
CpgExpression	*cpg_expression_copy			(CpgExpression *expression);
void 			 cpg_expression_free			(CpgExpression *expression);

char const 		*cpg_expression_get				(CpgExpression *expression);
int				 cpg_expression_compile			(CpgExpression *expression, CpgContext *context, char **error);

double 			 cpg_expression_evaluate		(CpgExpression *expression);
void			 cpg_expression_set_value		(CpgExpression *expression, double value);

/* convenient function */
void			 cpg_expression_print_instructions(CpgExpression *expression, FILE *f);

#endif /* __CPG_EXPRESSION_H__ */
