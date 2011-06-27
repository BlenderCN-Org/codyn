#ifndef __CPG_OPERATOR_DELAYED_H__
#define __CPG_OPERATOR_DELAYED_H__

#include <cpg-network/cpg-operator.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_DELAYED		(cpg_operator_delayed_get_type ())
#define CPG_OPERATOR_DELAYED(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayed))
#define CPG_OPERATOR_DELAYED_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayed const))
#define CPG_OPERATOR_DELAYED_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayedClass))
#define CPG_IS_OPERATOR_DELAYED(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_DELAYED))
#define CPG_IS_OPERATOR_DELAYED_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_DELAYED))
#define CPG_OPERATOR_DELAYED_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayedClass))

typedef struct _CpgOperatorDelayed		CpgOperatorDelayed;
typedef struct _CpgOperatorDelayedClass		CpgOperatorDelayedClass;
typedef struct _CpgOperatorDelayedPrivate	CpgOperatorDelayedPrivate;

struct _CpgOperatorDelayed
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorDelayedPrivate *priv;
};

struct _CpgOperatorDelayedClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType              cpg_operator_delayed_get_type (void) G_GNUC_CONST;
CpgOperatorDelayed *cpg_operator_delayed_new      (void);

G_END_DECLS

#endif /* __CPG_OPERATOR_DELAYED_H__ */
