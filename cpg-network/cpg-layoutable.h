#ifndef __CPG_LAYOUTABLE_H__
#define __CPG_LAYOUTABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_LAYOUTABLE			(cpg_layoutable_get_type ())
#define CPG_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LAYOUTABLE, CpgLayoutable))
#define CPG_IS_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_LAYOUTABLE))
#define CPG_LAYOUTABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_LAYOUTABLE, CpgLayoutableInterface))

typedef struct _CpgLayoutable		CpgLayoutable;
typedef struct _CpgLayoutableInterface	CpgLayoutableInterface;

struct _CpgLayoutableInterface
{
	GTypeInterface parent;

	void (*get_location) (CpgLayoutable *layoutable,
	                          gint          *x,
	                          gint          *y);

	void (*set_location) (CpgLayoutable *layoutable,
	                      gint           x,
	                      gint           y);

	gboolean (*supports_location) (CpgLayoutable *layoutable);
};

GType cpg_layoutable_get_type (void) G_GNUC_CONST;

void cpg_layoutable_get_location (CpgLayoutable *self,
                                  gint          *x,
                                  gint          *y);

void cpg_layoutable_set_location (CpgLayoutable *self,
                                  gint           x,
                                  gint           y);

gboolean cpg_layoutable_supports_location (CpgLayoutable *self);

G_END_DECLS

#endif /* __CPG_LAYOUTABLE_H__ */
