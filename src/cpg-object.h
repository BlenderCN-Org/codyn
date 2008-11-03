#ifndef __CPG_OBJECT_H__
#define __CPG_OBJECT_H__

#include "cpg-property.h"

#define CPG_OBJECT_IS_STATE(x) (cpg_object_type((CpgObject *)x) == CPG_OBJECT_TYPE_STATE)
#define CPG_OBJECT_IS_LINK(x) (cpg_object_type((CpgObject *)x) == CPG_OBJECT_TYPE_LINK)

typedef enum
{
	CPG_OBJECT_TYPE_NONE,
	CPG_OBJECT_TYPE_STATE,
	CPG_OBJECT_TYPE_LINK
} CpgObjectType;

typedef struct _CpgObject CpgObject;

CpgObject 		 *cpg_object_new				();
void 			  cpg_object_free			(CpgObject *object);

CpgObjectType 	  cpg_object_type			(CpgObject *object);
void			  cpg_object_reset			(CpgObject *object);

CpgProperty 	 *cpg_object_add_property	(CpgObject *object, char const *name, char const *expression, char integrated);
CpgProperty 	 *cpg_object_property		(CpgObject *object, char const *name);

CpgProperty		**cpg_object_properties		(CpgObject *object, unsigned *size);

#endif /* __CPG_OBJECT_H__ */
