#ifndef __CPG_USABLE_H__
#define __CPG_USABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_USABLE			(cpg_usable_get_type ())
#define CPG_USABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_USABLE, CpgUsable))
#define CPG_IS_USABLE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_USABLE))
#define CPG_USABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_USABLE, CpgUsableInterface))

typedef struct _CpgUsable		CpgUsable;
typedef struct _CpgUsableInterface	CpgUsableInterface;

struct _CpgUsableInterface
{
	GTypeInterface parent;

	guint    (*use_count) (CpgUsable *self);
	void     (*use)       (CpgUsable *self);
	gboolean (*unuse)     (CpgUsable *self);
};

GType cpg_usable_get_type (void) G_GNUC_CONST;

guint    cpg_usable_use_count (CpgUsable *self);
void     cpg_usable_use       (CpgUsable *self);
gboolean cpg_usable_unuse     (CpgUsable *self);

G_END_DECLS

#endif /* __CPG_USABLE_H__ */
