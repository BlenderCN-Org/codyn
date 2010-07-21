#ifndef __CPG_LINK_ACTION_H__
#define __CPG_LINK_ACTION_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-expression.h>

G_BEGIN_DECLS

#define CPG_TYPE_LINK_ACTION		(cpg_link_action_get_type ())
#define CPG_LINK_ACTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LINK_ACTION, CpgLinkAction))
#define CPG_LINK_ACTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LINK_ACTION, CpgLinkAction const))
#define CPG_LINK_ACTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_LINK_ACTION, CpgLinkActionClass))
#define CPG_IS_LINK_ACTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_LINK_ACTION))
#define CPG_IS_LINK_ACTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_LINK_ACTION))
#define CPG_LINK_ACTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_LINK_ACTION, CpgLinkActionClass))

typedef struct _CpgLinkAction		CpgLinkAction;
typedef struct _CpgLinkActionClass	CpgLinkActionClass;
typedef struct _CpgLinkActionPrivate	CpgLinkActionPrivate;

struct _CpgLinkAction
{
	/*< private >*/
	GObject parent;

	CpgLinkActionPrivate *priv;
};

struct _CpgLinkActionClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_link_action_get_type (void) G_GNUC_CONST;
CpgLinkAction *cpg_link_action_new (CpgProperty *target, CpgExpression *equation);

CpgProperty *cpg_link_action_get_target (CpgLinkAction *action);
void cpg_link_action_set_target (CpgLinkAction *action, CpgProperty *target);

CpgExpression *cpg_link_action_get_equation (CpgLinkAction *action);
void cpg_link_action_set_equation (CpgLinkAction *action, CpgExpression *equation);

gboolean cpg_link_action_depends (CpgLinkAction *action,
                                  CpgProperty   *property);

G_END_DECLS

#endif /* __CPG_LINK_ACTION_H__ */
