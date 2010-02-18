#ifndef __CPG_PROPERTY_H__
#define __CPG_PROPERTY_H__

#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

typedef enum
{
	CPG_PROPERTY_HINT_NONE = 0,
	CPG_PROPERTY_HINT_IN = 1 << 0,
	CPG_PROPERTY_HINT_OUT = 1 << 1
} CpgPropertyHint;

typedef struct _CpgProperty CpgProperty;

/* forward declaration */
CPG_FORWARD_DECL (CpgObject);

GType			   cpg_property_get_type				(void);
CpgProperty 	  *cpg_property_new						(const gchar        *name, 
														 const gchar        *expression, 
														 gboolean            integrated, 
														 struct _CpgObject  *object);

CpgProperty       *_cpg_property_copy                   (CpgProperty        *property);
const gchar 	  *cpg_property_get_name				(CpgProperty        *property);

struct _CpgObject *cpg_property_get_object				(CpgProperty        *property);
void               _cpg_property_set_object             (CpgProperty        *property,
                                                         struct _CpgObject  *object);

gboolean		   cpg_property_get_integrated			(CpgProperty        *property);
void			   cpg_property_set_integrated			(CpgProperty		*property,
														 gboolean			 integrated);

CpgPropertyHint	   cpg_property_get_hint				(CpgProperty        *property);
void			   cpg_property_set_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void			   cpg_property_add_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void			   cpg_property_remove_hint				(CpgProperty		*property,
														 CpgPropertyHint     hint);
void 			   cpg_property_reset_cache				(CpgProperty		*property);

gdouble			   cpg_property_get_value				(CpgProperty        *property);
CpgExpression 	  *cpg_property_get_value_expression	(CpgProperty        *property);

void			   cpg_property_set_value				(CpgProperty        *property, 
														 gdouble             value);
void			   cpg_property_set_value_expression	(CpgProperty        *property, 
														 const gchar 		*expression);

gboolean		   cpg_property_equal					(CpgProperty        *property,
														 CpgProperty        *other);

void			   _cpg_property_set_update				(CpgProperty	    *property,
														 gdouble             value);
gdouble			   _cpg_property_get_update				(CpgProperty	    *property);

void 			   _cpg_property_use                    (CpgProperty        *property);
gboolean           _cpg_property_unuse                  (CpgProperty        *property);
guint	            cpg_property_get_used               (CpgProperty        *property);

G_END_DECLS

#endif /* __CPG_PROPERTY_H__ */
