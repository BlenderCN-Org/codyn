#ifndef __CPG_MODIFIABLE_H__
#define __CPG_MODIFIABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_MODIFIABLE			(cpg_modifiable_get_type ())
#define CPG_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MODIFIABLE, CpgModifiable))
#define CPG_IS_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_MODIFIABLE))
#define CPG_MODIFIABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_MODIFIABLE, CpgModifiableInterface))

typedef struct _CpgModifiable		CpgModifiable;
typedef struct _CpgModifiableInterface	CpgModifiableInterface;

struct _CpgModifiableInterface
{
	GTypeInterface parent;

	gboolean (*get_modified) (CpgModifiable *modifiable);
	void     (*set_modified) (CpgModifiable *modifiable,
	                          gboolean       modified);
};

GType cpg_modifiable_get_type (void) G_GNUC_CONST;

gboolean cpg_modifiable_get_modified (CpgModifiable *modifiable);
void     cpg_modifiable_set_modified (CpgModifiable *modifiable,
                                      gboolean       modified);

G_END_DECLS

#endif /* __CPG_MODIFIABLE_H__ */
