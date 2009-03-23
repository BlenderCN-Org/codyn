#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>

typedef enum
{
	CPG_EXPRESSION_FLAG_NONE = 0,
	CPG_EXPRESSION_FLAG_CACHED = 1 << 0,
	CPG_EXPRESSION_FLAG_INSTANT = 1 << 1
} CpgExpressionFlag;

typedef struct _CpgExpression 		CpgExpression;

GType			  cpg_expression_get_type			(void);
CpgExpression 	 *cpg_expression_new				(gchar const    *expression);
CpgExpression	 *cpg_expression_copy				(CpgExpression  *expression);

GSList		 	 *cpg_expression_get_dependencies	(CpgExpression  *expression);
gchar const		 *cpg_expression_get				(CpgExpression  *expression);
gint			  cpg_expression_compile			(CpgExpression  *expression, 
													 GSList         *context, 
													 gchar         **error);

GSList 			 *cpg_expression_get_instructions	(CpgExpression  *expression);

gdouble 		  cpg_expression_evaluate			(CpgExpression  *expression);
void			  cpg_expression_set_value			(CpgExpression  *expression, 
													 gdouble         value);


/* convenient function */
void			  cpg_expression_print_instructions	(CpgExpression  *expression, 
													 FILE           *f);

void			  cpg_expression_set				(CpgExpression  *expression, 
													 gchar const    *value);
void 			  cpg_expression_reset_cache		(CpgExpression  *expression);

#endif /* __CPG_EXPRESSION_H__ */
