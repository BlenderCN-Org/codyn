#ifndef __CPG_LINK_ACTION_H__
#define __CPG_LINK_ACTION_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-modifiable.h>

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
	GInitiallyUnowned parent;

	CpgLinkActionPrivate *priv;
};

struct _CpgLinkActionClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;
};

GType          cpg_link_action_get_type      (void) G_GNUC_CONST;

CpgLinkAction *cpg_link_action_new           (const gchar   *target,
                                              CpgExpression *equation);

CpgLinkAction *cpg_link_action_copy          (CpgLinkAction *action);

const gchar   *cpg_link_action_get_target    (CpgLinkAction *action);
void           cpg_link_action_set_target    (CpgLinkAction *action,
                                              const gchar   *target);

CpgExpression  *cpg_link_action_get_equation (CpgLinkAction *action);
void            cpg_link_action_set_equation (CpgLinkAction *action,
                                              CpgExpression *equation);

gboolean        cpg_link_action_depends      (CpgLinkAction *action,
                                              CpgProperty   *property);

gboolean        cpg_link_action_equal        (CpgLinkAction *action,
                                              CpgLinkAction *other);

void            cpg_link_action_set_enabled  (CpgLinkAction *action,
                                              gboolean       enabled);

gboolean        cpg_link_action_get_enabled  (CpgLinkAction *action);

CpgProperty    *cpg_link_action_get_target_property  (CpgLinkAction *action);
void            _cpg_link_action_set_target_property (CpgLinkAction *action,
                                                      CpgProperty   *property);

G_END_DECLS

#endif /* __CPG_LINK_ACTION_H__ */
