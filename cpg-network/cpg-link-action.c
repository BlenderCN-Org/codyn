#include "cpg-link-action.h"

/**
 * SECTION:cpg-link-action
 * @short_description: Link action equation
 *
 * A #CpgLinkAction is an action inside a link which sets a target
 * #CpgProperty to the value of a particular #CpgExpression equation.
 */

#define CPG_LINK_ACTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_LINK_ACTION, CpgLinkActionPrivate))

struct _CpgLinkActionPrivate
{
	CpgProperty *target;
	CpgExpression *equation;

	guint equation_proxy_id;
};

/* Properties */
enum
{
	PROP_0,
	PROP_TARGET,
	PROP_EQUATION
};

G_DEFINE_TYPE (CpgLinkAction, cpg_link_action, G_TYPE_OBJECT)

static void
set_target (CpgLinkAction *action,
            CpgProperty   *target)
{
	if (action->priv->target == target)
	{
		return;
	}

	if (action->priv->target)
	{
		_cpg_property_unuse (action->priv->target);
		g_object_unref (action->priv->target);
		action->priv->target = NULL;
	}

	if (target)
	{
		action->priv->target = g_object_ref (target);
		_cpg_property_use (action->priv->target);
	}
}

static void
on_expression_changed (CpgLinkAction *action)
{
	g_object_notify (G_OBJECT (action), "equation");
}

static void
set_equation (CpgLinkAction *action,
              CpgExpression *equation)
{
	if (action->priv->equation == equation)
	{
		return;
	}

	if (action->priv->equation)
	{
		g_signal_handler_disconnect (action->priv->equation,
		                             action->priv->equation_proxy_id);

		g_object_unref (action->priv->equation);
		action->priv->equation = NULL;
	}

	if (equation)
	{
		action->priv->equation = g_object_ref (equation);

		action->priv->equation_proxy_id =
			g_signal_connect_swapped (action->priv->equation,
			                          "notify::expression",
			                          G_CALLBACK (on_expression_changed),
			                          action);
	}
}

static void
cpg_link_action_finalize (GObject *object)
{
	CpgLinkAction *action = CPG_LINK_ACTION (object);

	set_target (action, NULL);
	set_equation (action, NULL);

	G_OBJECT_CLASS (cpg_link_action_parent_class)->finalize (object);
}

static void
cpg_link_action_set_property (GObject      *object,
                              guint         prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
	CpgLinkAction *self = CPG_LINK_ACTION (object);

	switch (prop_id)
	{
		case PROP_TARGET:
			set_target (self,
			            CPG_PROPERTY (g_value_get_object (value)));
		break;
		case PROP_EQUATION:
			set_equation (self,
			              CPG_EXPRESSION (g_value_get_object (value)));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_action_get_property (GObject    *object,
                              guint       prop_id,
                              GValue     *value,
                              GParamSpec *pspec)
{
	CpgLinkAction *self = CPG_LINK_ACTION (object);
	
	switch (prop_id)
	{
		case PROP_TARGET:
			g_value_set_object (value, self->priv->target);
		break;
		case PROP_EQUATION:
			g_value_set_object (value, self->priv->equation);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_link_action_class_init (CpgLinkActionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_link_action_finalize;
	object_class->set_property = cpg_link_action_set_property;
	object_class->get_property = cpg_link_action_get_property;

	/**
	 * CpgLinkAction:target:
	 *
	 * The target #CpgProperty
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TARGET,
	                                 g_param_spec_object ("target",
	                                                      "Target",
	                                                      "Target",
	                                                      CPG_TYPE_PROPERTY,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgLinkAction:equation:
	 *
	 * The equation
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_EQUATION,
	                                 g_param_spec_object ("equation",
	                                                      "Equation",
	                                                      "Equation",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_type_class_add_private (object_class, sizeof(CpgLinkActionPrivate));
}

static void
cpg_link_action_init (CpgLinkAction *self)
{
	self->priv = CPG_LINK_ACTION_GET_PRIVATE (self);
}

/**
 * cpg_link_action_new:
 * @target: A #CpgProperty
 * @equation: A #CpgExpression
 * 
 * Create a new #CpgLinkAction.
 *
 * Returns: A new #CpgLinkAction
 *
 **/
CpgLinkAction *
cpg_link_action_new (CpgProperty   *target,
                     CpgExpression *equation)
{
	return g_object_new (CPG_TYPE_LINK_ACTION,
	                     "target", target,
	                     "equation", equation,
	                     NULL);
}

/**
 * cpg_link_action_get_target:
 * @action: A #CpgLinkAction
 *
 * Get the target of the action.
 *
 * Returns: A #CpgProperty
 *
 **/
CpgProperty *
cpg_link_action_get_target (CpgLinkAction *action)
{
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), NULL);

	return action->priv->target;
}

/**
 * cpg_link_action_set_target:
 * @action: A #CpgLinkAction
 * @target: A #CpgProperty
 *
 * Set the target of the action.
 *
 **/
void
cpg_link_action_set_target (CpgLinkAction *action,
                            CpgProperty   *target)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));
	g_return_if_fail (target == NULL || CPG_IS_PROPERTY (target));

	set_target (action, target);

	if (action->priv->target == target)
	{
		g_object_notify (G_OBJECT (action), "target");
	}
}

/**
 * cpg_link_action_get_equation:
 * @action: A #CpgLinkAction
 *
 * Get the equation of the action.
 *
 * Returns: A #CpgExpression
 *
 **/
CpgExpression *
cpg_link_action_get_equation (CpgLinkAction *action)
{
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), NULL);

	return action->priv->equation;
}

/**
 * cpg_link_action_set_equation:
 * @action: A #CpgLinkAction
 * @equation: A #CpgExpression
 *
 * Set the equation of the action.
 *
 **/
void
cpg_link_action_set_equation (CpgLinkAction *action,
                              CpgExpression *equation)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));
	g_return_if_fail (equation == NULL || CPG_IS_EXPRESSION (equation));

	set_equation (action, equation);

	if (action->priv->equation == equation)
	{
		g_object_notify (G_OBJECT (action), "equation");
	}
}

/**
 * cpg_link_action_depends:
 * @action: A #CpgLinkAction
 * @property: A #CpgProperty
 *
 * Check whether the action depends on a certain property.
 *
 * Returns: %TRUE if the action depends on @property, %FALSE otherwise
 *
 **/
gboolean
cpg_link_action_depends (CpgLinkAction *action,
                         CpgProperty   *property)
{
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), FALSE);
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);

	return g_slist_find ((GSList *)cpg_expression_get_dependencies (action->priv->equation),
	                     property) != NULL;
}

