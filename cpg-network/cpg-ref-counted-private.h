#ifndef __CPG_REF_COUNTED_PRIVATE_H__
#define __CPG_REF_COUNTED_PRIVATE_H__

#include "cpg-ref-counted.h"

struct _CpgRefCounted
{
	gulong ref_count;
	GDestroyNotify destroy_func;
};

#endif /* __CPG_REF_COUNTED_PRIVATE_H__ */

