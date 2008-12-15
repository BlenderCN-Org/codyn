#ifndef __CPG_RELAY_PRIVATE_H__
#define __CPG_RELAY_PRIVATE_H__

#include "cpg-object-private.h"
#include "cpg-relay.h"

struct _CpgRelay
{
	CpgObject parent;
	unsigned done;
};

#endif /* __CPG_RELAY_PRIVATE_H__ */
