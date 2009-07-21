#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _CpgExpression 		CpgExpression;

GType			  cpg_expression_get_type			(void);
CpgExpression 	 *cpg_expression_new				(gchar const    *expression);

GSList		 	 *cpg_expression_get_dependencies	(CpgExpression  *expression);
const gchar      *cpg_expression_get_as_string		(CpgExpression  *expression);
gint			  cpg_expression_compile			(CpgExpression  *expression, 
													 GSList         *context, 
													 GError        **error);

GSList 			 *cpg_expression_get_instructions	(CpgExpression  *expression);

gdouble 		  cpg_expression_evaluate			(CpgExpression  *expression);
void			  cpg_expression_set_value			(CpgExpression  *expression, 
													 gdouble         value);
void			  cpg_expression_reset				(CpgExpression  *expression);

gboolean		  cpg_expression_equal				(CpgExpression  *expression,
													 CpgExpression  *other);

void			  cpg_expression_set_from_string	(CpgExpression  *expression, 
													 gchar const    *value);
void 			  cpg_expression_reset_cache		(CpgExpression  *expression);

G_END_DECLS

#endif /* __CPG_EXPRESSION_H__ */
