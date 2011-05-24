#ifndef __CPG_SELECTION_H__
#define __CPG_SELECTION_H__

#include <glib-object.h>
#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_SELECTION		(cpg_selection_get_type ())
#define CPG_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTION, CpgSelection))
#define CPG_SELECTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTION, CpgSelection const))
#define CPG_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_SELECTION, CpgSelectionClass))
#define CPG_IS_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_SELECTION))
#define CPG_IS_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_SELECTION))
#define CPG_SELECTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_SELECTION, CpgSelectionClass))

typedef struct _CpgSelection		CpgSelection;
typedef struct _CpgSelectionClass	CpgSelectionClass;
typedef struct _CpgSelectionPrivate	CpgSelectionPrivate;

struct _CpgSelection
{
	GObject parent;

	CpgSelectionPrivate *priv;
};

struct _CpgSelectionClass
{
	GObjectClass parent_class;
};

GType cpg_selection_get_type (void) G_GNUC_CONST;

CpgSelection *cpg_selection_new              (gpointer                object,
                                              GSList                 *expansions);

CpgSelection *cpg_selection_copy             (CpgSelection           *selection);

gpointer      cpg_selection_get_object       (CpgSelection           *selection);
GSList       *cpg_selection_get_expansions   (CpgSelection           *selection);



G_END_DECLS

#endif /* __CPG_SELECTION_H__ */

