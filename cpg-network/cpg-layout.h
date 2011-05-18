#ifndef __CPG_LAYOUT_H__
#define __CPG_LAYOUT_H__

#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_LAYOUT			(cpg_layout_get_type ())
#define CPG_LAYOUT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LAYOUT, CpgLayout))
#define CPG_LAYOUT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LAYOUT, CpgLayout const))
#define CPG_LAYOUT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_LAYOUT, CpgLayoutClass))
#define CPG_IS_LAYOUT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_LAYOUT))
#define CPG_IS_LAYOUT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_LAYOUT))
#define CPG_LAYOUT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_LAYOUT, CpgLayoutClass))

typedef struct _CpgLayout		CpgLayout;
typedef struct _CpgLayoutClass		CpgLayoutClass;
typedef struct _CpgLayoutPrivate	CpgLayoutPrivate;

typedef enum
{
	CPG_LAYOUT_RELATION_ABOVE = 1 << 0,
	CPG_LAYOUT_RELATION_BELOW = 1 << 1,
	CPG_LAYOUT_RELATION_LEFT_OF = 1 << 2,
	CPG_LAYOUT_RELATION_RIGHT_OF = 1 << 3
} CpgLayoutRelation;

struct _CpgLayout
{
	/*< private >*/
	GObject parent;

	CpgLayoutPrivate *priv;
};

struct _CpgLayoutClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType      cpg_layout_get_type (void) G_GNUC_CONST;

CpgLayout *cpg_layout_new      (CpgNetwork        *network);

void       cpg_layout_add      (CpgLayout         *layout,
                                CpgObject         *left,
                                CpgObject         *right,
                                CpgLayoutRelation  relation);

G_END_DECLS

#endif /* __CPG_LAYOUT_H__ */
