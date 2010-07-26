#ifndef __CPG_OPERATOR_LASTOF_H__
#define __CPG_OPERATOR_LASTOF_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_LASTOF		(cpg_operator_lastof_get_type ())
#define CPG_OPERATOR_LASTOF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_LASTOF, CpgOperatorLastof))
#define CPG_OPERATOR_LASTOF_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_LASTOF, CpgOperatorLastof const))
#define CPG_OPERATOR_LASTOF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_LASTOF, CpgOperatorLastofClass))
#define CPG_IS_OPERATOR_LASTOF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_LASTOF))
#define CPG_IS_OPERATOR_LASTOF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_LASTOF))
#define CPG_OPERATOR_LASTOF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_LASTOF, CpgOperatorLastofClass))

typedef struct _CpgOperatorLastof		CpgOperatorLastof;
typedef struct _CpgOperatorLastofClass		CpgOperatorLastofClass;
typedef struct _CpgOperatorLastofPrivate	CpgOperatorLastofPrivate;

struct _CpgOperatorLastof
{
	/*< private >*/
	GObject parent;

	CpgOperatorLastofPrivate *priv;
};

struct _CpgOperatorLastofClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType              cpg_operator_lastof_get_type (void) G_GNUC_CONST;
CpgOperatorLastof *cpg_operator_lastof_new      (void);

G_END_DECLS

#endif /* __CPG_OPERATOR_LASTOF_H__ */
