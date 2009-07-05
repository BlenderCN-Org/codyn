#ifndef __CPG_REF_COUNTED_H__
#define __CPG_REF_COUNTED_H__

#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _CpgRefCounted CpgRefCounted;

void		cpg_ref_counted_init				(gpointer		 ref_counted,
												 GDestroyNotify  destroy_func);
gpointer 	cpg_ref_counted_ref					(gpointer        ref_counted);
void 		cpg_ref_counted_unref				(gpointer        ref_counted);

G_END_DECLS

#endif /* __CPG_REF_COUNTED_H__ */

