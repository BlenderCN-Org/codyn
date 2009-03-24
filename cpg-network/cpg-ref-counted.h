#ifndef __CPG_REF_COUNTED_H__
#define __CPG_REF_COUNTED_H__

#include <glib-object.h>

typedef struct _CpgRefCounted CpgRefCounted;

void		cpg_ref_counted_init				(gpointer		 ref_counted,
												 GDestroyNotify  destroy_func);
gpointer 	cpg_ref_counted_ref					(gpointer        ref_counted);
void 		cpg_ref_counted_unref				(gpointer        ref_counted);

#endif /* __CPG_REF_COUNTED_H__ */

