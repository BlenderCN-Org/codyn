#ifndef __CPG_ATTRIBUTE_H__
#define __CPG_ATTRIBUTE_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-string.h>

G_BEGIN_DECLS

#define CPG_TYPE_ATTRIBUTE		(cpg_attribute_get_type ())
#define CPG_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ATTRIBUTE, CpgAttribute))
#define CPG_ATTRIBUTE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ATTRIBUTE, CpgAttribute const))
#define CPG_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_ATTRIBUTE, CpgAttributeClass))
#define CPG_IS_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_ATTRIBUTE))
#define CPG_IS_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_ATTRIBUTE))
#define CPG_ATTRIBUTE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_ATTRIBUTE, CpgAttributeClass))

typedef struct _CpgAttribute		CpgAttribute;
typedef struct _CpgAttributeClass	CpgAttributeClass;
typedef struct _CpgAttributePrivate	CpgAttributePrivate;

struct _CpgAttribute
{
	/*< private >*/
	GObject parent;

	CpgAttributePrivate *priv;
};

struct _CpgAttributeClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_attribute_get_type (void) G_GNUC_CONST;

CpgAttribute *cpg_attribute_new (gchar const *id);
CpgAttribute *cpg_attribute_newv (gchar const *id,
                                  ...) G_GNUC_NULL_TERMINATED;

void cpg_attribute_set_arguments (CpgAttribute *attr, GSList *arguments);

gchar const *cpg_attribute_get_id (CpgAttribute *attr);
GSList *cpg_attribute_get_arguments (CpgAttribute *attr);

gpointer cpg_attribute_get_argument (CpgAttribute *attr, gint i);
gint cpg_attribute_num_arguments (CpgAttribute *attr);

G_END_DECLS

#endif /* __CPG_ATTRIBUTE_H__ */
