/*
 * cpg-link-action.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-link-action.h"
#include "cpg-modifiable.h"
#include "cpg-usable.h"
#include "cpg-annotatable.h"
#include "cpg-link.h"

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
	gchar *target;
	CpgExpression *equation;
	CpgProperty *property;
	CpgLink *link;

	guint equation_proxy_id;

	gchar *annotation;

	guint modified : 1;
	guint enabled : 1;
	guint disposing : 1;
};

/* Properties */
enum
{
	PROP_0,
	PROP_TARGET,
	PROP_EQUATION,
	PROP_TARGET_PROPERTY,
	PROP_MODIFIED,
	PROP_ENABLED,
	PROP_LINK,
	PROP_ANNOTATION,
};

static void cpg_modifiable_iface_init (gpointer iface);
static void cpg_annotatable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgLinkAction,
                         cpg_link_action,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_MODIFIABLE,
                                                cpg_modifiable_iface_init);
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_ANNOTATABLE,
                                                cpg_annotatable_iface_init));

static void
cpg_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static gchar *
cpg_link_action_annotatable_get_title (CpgAnnotatable *annotatable)
{
	CpgLinkAction *action;
	gchar *ret = NULL;

	action = CPG_LINK_ACTION (annotatable);

	if (action->priv->link)
	{
		gchar *id;

		id = cpg_annotatable_get_title (CPG_ANNOTATABLE (action->priv->link));

		ret = g_strconcat (id, " (", action->priv->target, ")", NULL);

		g_free (id);
	}
	else
	{
		ret = g_strdup (action->priv->target);
	}

	return ret;
}

static void
cpg_annotatable_iface_init (gpointer iface)
{
	CpgAnnotatableInterface *annotatable = iface;

	annotatable->get_title = cpg_link_action_annotatable_get_title;
}

static void
set_property (CpgLinkAction *action,
              CpgProperty   *property)
{
	if (action->priv->property == property)
	{
		return;
	}

	if (action->priv->property)
	{
		cpg_usable_unuse (CPG_USABLE (action->priv->property));
		g_object_unref (action->priv->property);
		action->priv->property = NULL;
	}

	if (property)
	{
		action->priv->property = g_object_ref_sink (property);
		cpg_usable_use (CPG_USABLE (action->priv->property));
	}
}

static void
set_link (CpgLinkAction *action,
          CpgLink       *link)
{
	if (action->priv->link == link)
	{
		return;
	}

	if (action->priv->link)
	{
		g_object_remove_weak_pointer (G_OBJECT (action->priv->link),
		                              (gpointer *)&action->priv->link);
	}

	action->priv->link = link;

	if (action->priv->link)
	{
		g_object_add_weak_pointer (G_OBJECT (action->priv->link),
		                           (gpointer *)&action->priv->link);
	}

	if (!action->priv->disposing)
	{
		g_object_notify (G_OBJECT (action), "link");
	}
}

static void
set_target (CpgLinkAction *action,
            gchar const   *target)
{
	if (g_strcmp0 (action->priv->target, target) == 0)
	{
		return;
	}

	g_free (action->priv->target);
	action->priv->target = g_strdup (target);

	g_object_notify (G_OBJECT (action), "target");
}

static void
on_expression_changed (CpgLinkAction *action)
{
	g_object_notify (G_OBJECT (action), "equation");
	cpg_modifiable_set_modified (CPG_MODIFIABLE (action), TRUE);
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
		action->priv->equation = g_object_ref_sink (equation);

		action->priv->equation_proxy_id =
			g_signal_connect_swapped (action->priv->equation,
			                          "notify::expression",
			                          G_CALLBACK (on_expression_changed),
			                          action);
	}

	g_object_notify (G_OBJECT (action), "equation");
	cpg_modifiable_set_modified (CPG_MODIFIABLE (action), TRUE);
}

static void
cpg_link_action_dispose (GObject *object)
{
	CpgLinkAction *action = CPG_LINK_ACTION (object);

	action->priv->disposing = TRUE;

	set_property (action, NULL);
	set_target (action, NULL);
	set_equation (action, NULL);
	set_link (action, NULL);

	G_OBJECT_CLASS (cpg_link_action_parent_class)->dispose (object);
}

static void
cpg_link_action_finalize (GObject *object)
{
	CpgLinkAction *action = CPG_LINK_ACTION (object);

	g_free (action->priv->annotation);

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
		case PROP_LINK:
			set_link (self, g_value_get_object (value));
		break;
		case PROP_TARGET:
			set_target (self, g_value_get_string (value));
		break;
		case PROP_EQUATION:
			set_equation (self,
			              CPG_EXPRESSION (g_value_get_object (value)));
		break;
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
		break;
		case PROP_ENABLED:
			cpg_link_action_set_enabled (self,
			                             g_value_get_boolean (value));
		break;
		case PROP_ANNOTATION:
			g_free (self->priv->annotation);
			self->priv->annotation = g_value_dup_string (value);
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
		case PROP_LINK:
			g_value_set_object (value, self->priv->link);
		break;
		case PROP_TARGET:
			g_value_set_object (value, self->priv->target);
		break;
		case PROP_EQUATION:
			g_value_set_object (value, self->priv->equation);
		break;
		case PROP_TARGET_PROPERTY:
			g_value_set_object (value, self->priv->property);
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
		break;
		case PROP_ENABLED:
			g_value_set_boolean (value, self->priv->enabled);
		break;
		case PROP_ANNOTATION:
			g_value_set_string (value, self->priv->annotation);
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
	object_class->dispose = cpg_link_action_dispose;

	object_class->set_property = cpg_link_action_set_property;
	object_class->get_property = cpg_link_action_get_property;

	/**
	 * CpgLinkAction:link:
	 *
	 * The link
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_LINK,
	                                 g_param_spec_object ("link",
	                                                      "Link",
	                                                      "Link",
	                                                      CPG_TYPE_LINK,
	                                                      G_PARAM_READABLE));

	/**
	 * CpgLinkAction:target:
	 *
	 * The target #CpgProperty
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TARGET,
	                                 g_param_spec_string ("target",
	                                                      "Target",
	                                                      "Target",
	                                                      NULL,
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

	/**
	 * CpgLinkAction:target-property:
	 *
	 * The target property
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TARGET_PROPERTY,
	                                 g_param_spec_object ("target-property",
	                                                      "Target property",
	                                                      "Target Property",
	                                                      CPG_TYPE_PROPERTY,
	                                                      G_PARAM_READABLE));

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");

	g_object_class_override_property (object_class,
	                                  PROP_ANNOTATION,
	                                  "annotation");

	g_type_class_add_private (object_class, sizeof(CpgLinkActionPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_ENABLED,
	                                 g_param_spec_boolean ("enabled",
	                                                       "Enabled",
	                                                       "Enabled",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
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
cpg_link_action_new (const gchar   *target,
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
 * Returns: (transfer none): the action target
 *
 **/
const gchar *
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
                            const gchar   *target)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));

	set_target (action, target);
}

/**
 * cpg_link_action_get_equation:
 * @action: A #CpgLinkAction
 *
 * Get the equation of the action.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_link_action_get_equation (CpgLinkAction *action)
{
	/* Omit type check to increase speed */
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

/**
 * cpg_link_action_copy:
 * @action: A #CpgLinkAction
 *
 * Create a copy of a #CpgLinkAction.
 *
 * Returns: (transfer full): A #CpgLinkAction
 *
 **/
CpgLinkAction *
cpg_link_action_copy (CpgLinkAction *action)
{
	CpgLinkAction *newaction;

	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), NULL);

	newaction = cpg_link_action_new (g_strdup (action->priv->target),
	                                 cpg_expression_copy (action->priv->equation));

	cpg_annotatable_set_annotation (CPG_ANNOTATABLE (newaction),
	                                action->priv->annotation);

	return newaction;
}

/**
 * cpg_link_action_get_target_property:
 * @action: A #CpgLinkAction
 *
 * Get the target property of the link action.
 *
 * Returns: (transfer none): A #CpgProperty
 *
 **/
CpgProperty *
cpg_link_action_get_target_property (CpgLinkAction *action)
{
	/* Omit type check to increase speed */
	return action->priv->property;
}

gboolean
cpg_link_action_equal (CpgLinkAction *action,
                       CpgLinkAction *other)
{
	g_return_val_if_fail (action == NULL || CPG_IS_LINK_ACTION (action), FALSE);
	g_return_val_if_fail (other == NULL || CPG_IS_LINK_ACTION (other), FALSE);

	if (action == NULL || other == NULL)
	{
		return action == other;
 	}

	if (g_strcmp0 (action->priv->target, other->priv->target) != 0)
	{
		return FALSE;
	}

	if (!action->priv->equation || !other->priv->equation)
	{
		return action->priv->equation == other->priv->equation;
	}

	return cpg_expression_equal (action->priv->equation,
	                             other->priv->equation);
}

void
cpg_link_action_set_enabled (CpgLinkAction *action,
                             gboolean       enabled)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));

	if (action->priv->enabled != enabled)
	{
		action->priv->enabled = enabled;
		g_object_notify (G_OBJECT (action), "enabled");
	}
}

gboolean
cpg_link_action_get_enabled (CpgLinkAction *action)
{
	/* Omit type check to increase speed */
	return action->priv->enabled;
}

void
_cpg_link_action_set_target_property (CpgLinkAction *action,
                                      CpgProperty   *property)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));
	g_return_if_fail (property == NULL || CPG_IS_PROPERTY (property));

	set_property (action, property);
}

/**
 * cpg_link_action_get_link:
 * @action: the #CpgLinkAction
 *
 * Get the link associated with the action
 *
 * Returns: (type CpgLink) (transfer none): the link associated with the action
 **/
CpgLink *
cpg_link_action_get_link (CpgLinkAction *action)
{
	g_return_val_if_fail (CPG_IS_LINK_ACTION (action), NULL);

	return action->priv->link;
}

void
_cpg_link_action_set_link (CpgLinkAction *action,
                           CpgLink       *link)
{
	g_return_if_fail (CPG_IS_LINK_ACTION (action));
	g_return_if_fail (link == NULL || CPG_IS_LINK (link));

	set_link (action, link);
}
