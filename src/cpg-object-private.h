#ifndef __CPG_OBJECT_PRIVATE_H__
#define __CPG_OBJECT_PRIVATE_H__

#include "cpg-link.h"
#include "cpg-object.h"

struct _CpgObject
{
	CpgObjectType type;
	char *id;

	// Properties
	CpgProperty **properties;
	unsigned num_properties;
	
	CpgProperty **actors;
	unsigned num_actors;
	
	// Links
	CpgLink **links;
	unsigned num_links;
	
	void (*compile)		(CpgObject *object);
	void (*reset)		(CpgObject *object);
	void (*update)		(CpgObject *object, float timestep);
	void (*evaluate)	(CpgObject *object, float timestep);
};

/* used for inheritance of CpgState and CpgLink */
void 			 cpg_object_initialize		(CpgObject *object, CpgObjectType type);
void 			 cpg_object_destroy			(CpgObject *object);
char const  	*cpg_object_id				(CpgObject *object);

/* used for referencing links */
void 			 cpg_object_link			(CpgObject *object, CpgLink *link);
void			 cpg_object_update_link		(CpgObject *object, CpgLink *link);

/* evaluation */
void			 cpg_object_update			(CpgObject *object, float timestep);
void			 cpg_object_evaluate		(CpgObject *object, float timestep);

#endif /* __CPG_OBJECT_PRIVATE_H__ */
