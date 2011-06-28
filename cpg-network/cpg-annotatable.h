#ifndef __CPG_ANNOTATABLE_H__
#define __CPG_ANNOTATABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_ANNOTATABLE			(cpg_annotatable_get_type ())
#define CPG_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ANNOTATABLE, CpgAnnotatable))
#define CPG_IS_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_ANNOTATABLE))
#define CPG_ANNOTATABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_ANNOTATABLE, CpgAnnotatableInterface))

typedef struct _CpgAnnotatable		CpgAnnotatable;
typedef struct _CpgAnnotatableInterface	CpgAnnotatableInterface;

struct _CpgAnnotatableInterface
{
	GTypeInterface parent;

	gchar *(*get_title) (CpgAnnotatable *annotatable);

	void (*set_annotation) (CpgAnnotatable *annotatable,
	                        gchar const    *annotation);

	gchar *(*get_annotation) (CpgAnnotatable *annotatable);
};

GType cpg_annotatable_get_type (void) G_GNUC_CONST;

gchar *cpg_annotatable_get_title (CpgAnnotatable *self);

gchar *cpg_annotatable_get_annotation (CpgAnnotatable *self);

void cpg_annotatable_set_annotation (CpgAnnotatable *self,
                                     gchar const    *annotation);

G_END_DECLS

#endif /* __CPG_ANNOTATABLE_H__ */
