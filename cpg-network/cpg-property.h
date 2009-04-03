#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include "cpg-ref-counted.h"
#include "cpg-expression.h"

typedef struct _CpgProperty CpgProperty;

/* forward declaration */
struct _CpgObject;

GType			   cpg_property_get_type				(void);
CpgProperty 	  *cpg_property_new						(gchar const        *name, 
														 gchar const        *expression, 
														 gboolean            integrated, 
														 struct _CpgObject  *object);

gchar const 	  *cpg_property_get_name				(CpgProperty        *property);

struct _CpgObject *cpg_property_get_object				(CpgProperty        *property);
gboolean		   cpg_property_get_integrated			(CpgProperty        *property);
void			   cpg_property_set_integrated			(CpgProperty		*property,
														 gboolean			 integrated);

gboolean		   cpg_property_compile					(CpgProperty        *property, 
														 GSList 			*context, 
														 gchar             **error);

void			   cpg_property_reset					(CpgProperty		*property);
void 			   cpg_property_reset_cache				(CpgProperty		*property);

gdouble			   cpg_property_get_value				(CpgProperty        *property);
CpgExpression 	  *cpg_property_get_value_expression	(CpgProperty        *property);

void			   cpg_property_set_value				(CpgProperty        *property, 
														 gdouble             value);
void			   cpg_property_set_value_expression	(CpgProperty        *property, 
														 gchar const 		*expression);

void			   _cpg_property_set_update				(CpgProperty	    *property,
														 gdouble             value);
gdouble			   _cpg_property_get_update				(CpgProperty	    *property);

#endif /* __CPG_PROPERTY_H__ */
