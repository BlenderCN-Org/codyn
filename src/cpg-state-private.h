#ifndef __CPG_STATE_PRIVATE_H__
#define __CPG_STATE_PRIVATE_H__

#include "cpg-object-private.h"
#include "cpg-state.h"

struct _CpgState
{
	CpgObject parent;

	char *name;
};

#endif /* __CPG_STATE_PRIVATE_H__ */
