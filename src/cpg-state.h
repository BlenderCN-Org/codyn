#ifndef __CPG_STATE_H__
#define __CPG_STATE_H__

#include "cpg-object.h"

struct _CpgLink;

typedef struct
{
	CpgObject parent;

	char *name;
} CpgState;

CpgState 	*cpg_state_new		(char const *name);
void		 cpg_state_free		();

#endif /* __CPG_STATE_H__ */
