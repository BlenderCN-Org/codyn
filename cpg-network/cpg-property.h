#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include "cpg-ref-counted.h"
#include "cpg-expression.h"

G_BEGIN_DECLS

typedef struct _CpgProperty CpgProperty;

/* forward declaration */
struct _CpgObject;

GType			   cpg_property_get_type				(void);
CpgProperty 	  *cpg_property_new						(gchar const        *name, 
														 gchar const        *expression, 
														 gboolean            integrated, 
														 struct _CpgObject  *object);

CpgProperty       *_cpg_property_copy                   (CpgProperty        *property);
gchar const 	  *cpg_property_get_name				(CpgProperty        *property);

struct _CpgObject *cpg_property_get_object				(CpgProperty        *property);
void               _cpg_property_set_object             (CpgProperty        *property,
                                                         struct _CpgObject  *object);

gboolean		   cpg_property_get_integrated			(CpgProperty        *property);
void			   cpg_property_set_integrated			(CpgProperty		*property,
														 gboolean			 integrated);

void 			   cpg_property_reset_cache				(CpgProperty		*property);

gdouble			   cpg_property_get_value				(CpgProperty        *property);
CpgExpression 	  *cpg_property_get_value_expression	(CpgProperty        *property);

void			   cpg_property_set_value				(CpgProperty        *property, 
														 gdouble             value);
void			   cpg_property_set_value_expression	(CpgProperty        *property, 
														 gchar const 		*expression);

gboolean		   cpg_property_equal					(CpgProperty        *property,
														 CpgProperty        *other);

void			   _cpg_property_set_update				(CpgProperty	    *property,
														 gdouble             value);
gdouble			   _cpg_property_get_update				(CpgProperty	    *property);

void 			   _cpg_property_use                    (CpgProperty        *property);
gboolean           _cpg_property_unuse                  (CpgProperty        *property);

G_END_DECLS

#endif /* __CPG_PROPERTY_H__ */
