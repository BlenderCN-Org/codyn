#ifndef __CPG_OBJECT_H__
#define __CPG_OBJECT_H__

#include "cpg-property.h"

struct _CpgLink;

#define CPG_OBJECT_IS_STATE(x) (((CpgObject *)x)->type == CPG_OBJECT_TYPE_STATE)
#define CPG_OBJECT_IS_LINK(x) (((CpgObject *)x)->type == CPG_OBJECT_TYPE_LINK)

typedef enum
{
	CPG_OBJECT_TYPE_NONE,
	CPG_OBJECT_TYPE_STATE,
	CPG_OBJECT_TYPE_LINK
} CpgObjectType;

typedef struct
{
	CpgObjectType type;

	// Properties
	CpgProperty **properties;
	unsigned num_properties;
	
	CpgProperty **actors;
	unsigned num_actors;
	
	// Links
	struct _CpgLink **links;
	unsigned num_links;
} CpgObject;

CpgObject 		*cpg_object_new				();
void 			 cpg_object_initialize		(CpgObject *object, CpgObjectType type);
void 			 cpg_object_add_property	(CpgObject *object, char const *name, char const *expression, char integrated);
void 			 cpg_object_free			(CpgObject *object);
void 			 cpg_object_destroy			(CpgObject *object);

CpgProperty 	*cpg_object_get_property	(CpgObject *object, char const *name);
void 			 cpg_object_link			(CpgObject *object, struct _CpgLink *link);
void			 cpg_object_update_link		(CpgObject *object, struct _CpgLink *link);

void			 cpg_object_set_value		(CpgObject *object, CpgProperty *property, char const *expression);
void			 cpg_object_set_initial		(CpgObject *object, CpgProperty *property, char const *expression);

void			 cpg_object_update			(CpgObject *object, float timestep);
void			 cpg_object_evaluate		(CpgObject *object, float timestep);
void			 cpg_object_reset			(CpgObject *object);

#endif /* __CPG_OBJECT_H__ */
