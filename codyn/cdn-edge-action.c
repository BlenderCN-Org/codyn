/*
 * cdn-edge-action.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-edge-action.h"
#include "cdn-modifiable.h"
#include "cdn-usable.h"
#include "cdn-annotatable.h"
#include "cdn-edge.h"
#include "cdn-enum-types.h"
#include "cdn-phaseable.h"
#include "cdn-compile-error.h"

#include <math.h>

/**
 * SECTION:cdn-edge-action
 * @short_description: Link action equation
 *
 * A #CdnEdgeAction is an action inside a link which sets a target
 * #CdnVariable to the value of a particular #CdnExpression equation.
 */

#define CDN_EDGE_ACTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EDGE_ACTION, CdnEdgeActionPrivate))

struct _CdnEdgeActionPrivate
{
	gchar *target;
	CdnExpression *equation;
	CdnVariable *property;
	CdnEdge *link;
	CdnExpression *index;
	gint *indices;
	gint num_indices;

	guint equation_proxy_id;
	guint index_proxy_id;

	gchar *annotation;
	GHashTable *phases;

	guint modified : 1;
	guint enabled : 1;
	guint disposing : 1;
	guint integrated : 1;
	guint integrated_set : 1;
};

/* Properties */
enum
{
	PROP_0,
	PROP_TARGET,
	PROP_EQUATION,
	PROP_TARGET_PROPERTY,
	PROP_MODIFIED,
	PROP_LINK,
	PROP_ANNOTATION,
	PROP_INDEX,
	PROP_INTEGRATED
};

static void cdn_modifiable_iface_init (gpointer iface);
static void cdn_annotatable_iface_init (gpointer iface);
static void cdn_phaseable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnEdgeAction,
                         cdn_edge_action,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_MODIFIABLE,
                                                cdn_modifiable_iface_init);
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_ANNOTATABLE,
                                                cdn_annotatable_iface_init);
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_PHASEABLE,
                                                cdn_phaseable_iface_init));

static GHashTable *
cdn_phaseable_get_phase_table_impl (CdnPhaseable *phaseable)
{
	return CDN_EDGE_ACTION (phaseable)->priv->phases;
}

static void
cdn_phaseable_set_phase_table_impl (CdnPhaseable *phaseable,
                                    GHashTable   *table)
{
	CdnEdgeAction *action;

	action = CDN_EDGE_ACTION (phaseable);

	if (action->priv->phases)
	{
		g_hash_table_unref (action->priv->phases);
		action->priv->phases = NULL;
	}

	if (table)
	{
		action->priv->phases = table;
		g_hash_table_ref (table);
	}
}

static void
cdn_phaseable_iface_init (gpointer iface)
{
	CdnPhaseableInterface *phaseable;

	phaseable = iface;

	phaseable->get_phase_table = cdn_phaseable_get_phase_table_impl;
	phaseable->set_phase_table = cdn_phaseable_set_phase_table_impl;
}

static void
cdn_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static gchar *
cdn_edge_action_annotatable_get_title (CdnAnnotatable *annotatable)
{
	CdnEdgeAction *action;
	gchar *ret = NULL;

	action = CDN_EDGE_ACTION (annotatable);

	if (action->priv->link)
	{
		gchar *id;

		id = cdn_annotatable_get_title (CDN_ANNOTATABLE (action->priv->link));

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
cdn_annotatable_iface_init (gpointer iface)
{
	CdnAnnotatableInterface *annotatable = iface;

	annotatable->get_title = cdn_edge_action_annotatable_get_title;
}

static void
set_property (CdnEdgeAction *action,
              CdnVariable   *property)
{
	if (action->priv->property == property)
	{
		return;
	}

	if (action->priv->property)
	{
		cdn_usable_unuse (CDN_USABLE (action->priv->property));
		g_object_unref (action->priv->property);
		action->priv->property = NULL;
	}

	if (property)
	{
		action->priv->property = g_object_ref_sink (property);
		cdn_usable_use (CDN_USABLE (action->priv->property));
	}
}

static void
set_edge (CdnEdgeAction *action,
          CdnEdge       *link)
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
set_target (CdnEdgeAction *action,
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
on_expression_changed (CdnEdgeAction *action)
{
	g_object_notify (G_OBJECT (action), "equation");
	cdn_modifiable_set_modified (CDN_MODIFIABLE (action), TRUE);
}

static void
set_equation (CdnEdgeAction *action,
              CdnExpression *equation)
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
	cdn_modifiable_set_modified (CDN_MODIFIABLE (action), TRUE);
}

static void
on_index_changed (CdnEdgeAction *action)
{
	g_object_notify (G_OBJECT (action), "index");
	cdn_modifiable_set_modified (CDN_MODIFIABLE (action), TRUE);
}

static void
index_cache_notify (CdnExpression *expression,
                    CdnEdgeAction *action)
{
	g_free (action->priv->indices);
	action->priv->indices = NULL;
}

static void
set_index (CdnEdgeAction *action,
           CdnExpression *index)
{
	if (action->priv->index == index)
	{
		return;
	}

	if (action->priv->index)
	{
		cdn_expression_set_cache_notify (action->priv->index,
		                                 NULL,
		                                 NULL,
		                                 NULL);

		g_signal_handler_disconnect (action->priv->index,
		                             action->priv->index_proxy_id);

		g_object_unref (action->priv->index);
		action->priv->index = NULL;
	}

	if (index)
	{
		action->priv->index = g_object_ref_sink (index);

		action->priv->index_proxy_id =
			g_signal_connect_swapped (action->priv->index,
			                          "notify::expression",
			                          G_CALLBACK (on_index_changed),
			                          action);

		cdn_expression_set_cache_notify (action->priv->index,
		                                 (CdnExpressionCacheNotify)index_cache_notify,
		                                 action,
		                                 NULL);
	}

	g_object_notify (G_OBJECT (action), "index");
	cdn_modifiable_set_modified (CDN_MODIFIABLE (action), TRUE);
}

static void
cdn_edge_action_dispose (GObject *object)
{
	CdnEdgeAction *action = CDN_EDGE_ACTION (object);

	action->priv->disposing = TRUE;

	set_property (action, NULL);
	set_target (action, NULL);
	set_equation (action, NULL);
	set_edge (action, NULL);
	set_index (action, NULL);

	G_OBJECT_CLASS (cdn_edge_action_parent_class)->dispose (object);
}

static void
cdn_edge_action_finalize (GObject *object)
{
	CdnEdgeAction *action = CDN_EDGE_ACTION (object);

	g_free (action->priv->annotation);

	if (action->priv->phases)
	{
		g_hash_table_destroy (action->priv->phases);
	}

	G_OBJECT_CLASS (cdn_edge_action_parent_class)->finalize (object);
}

static void
cdn_edge_action_set_property (GObject      *object,
                              guint         prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
	CdnEdgeAction *self = CDN_EDGE_ACTION (object);

	switch (prop_id)
	{
		case PROP_LINK:
			set_edge (self, g_value_get_object (value));
		break;
		case PROP_TARGET:
			set_target (self, g_value_get_string (value));
		break;
		case PROP_EQUATION:
			set_equation (self,
			              CDN_EXPRESSION (g_value_get_object (value)));
		break;
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
		break;
		case PROP_ANNOTATION:
			g_free (self->priv->annotation);
			self->priv->annotation = g_value_dup_string (value);
		break;
		case PROP_INDEX:
			set_index (self,
			           CDN_EXPRESSION (g_value_get_object (value)));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_edge_action_get_property (GObject    *object,
                              guint       prop_id,
                              GValue     *value,
                              GParamSpec *pspec)
{
	CdnEdgeAction *self = CDN_EDGE_ACTION (object);
	
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
		case PROP_ANNOTATION:
			g_value_set_string (value, self->priv->annotation);
		break;
		case PROP_INDEX:
			g_value_set_object (value, self->priv->index);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_edge_action_class_init (CdnEdgeActionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_edge_action_finalize;
	object_class->dispose = cdn_edge_action_dispose;

	object_class->set_property = cdn_edge_action_set_property;
	object_class->get_property = cdn_edge_action_get_property;

	/**
	 * CdnEdgeAction:link:
	 *
	 * The link
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_LINK,
	                                 g_param_spec_object ("link",
	                                                      "Link",
	                                                      "Link",
	                                                      CDN_TYPE_EDGE,
	                                                      G_PARAM_READABLE));

	/**
	 * CdnEdgeAction:target:
	 *
	 * The target #CdnVariable
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
	 * CdnEdgeAction:equation:
	 *
	 * The equation
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_EQUATION,
	                                 g_param_spec_object ("equation",
	                                                      "Equation",
	                                                      "Equation",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CdnEdgeAction:target-property:
	 *
	 * The target property
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TARGET_PROPERTY,
	                                 g_param_spec_object ("target-property",
	                                                      "Target property",
	                                                      "Target Property",
	                                                      CDN_TYPE_VARIABLE,
	                                                      G_PARAM_READABLE));

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");

	g_object_class_override_property (object_class,
	                                  PROP_ANNOTATION,
	                                  "annotation");

	g_type_class_add_private (object_class, sizeof(CdnEdgeActionPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_INDEX,
	                                 g_param_spec_object ("index",
	                                                      "Index",
	                                                      "Index",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE));
}

static void
cdn_edge_action_init (CdnEdgeAction *self)
{
	self->priv = CDN_EDGE_ACTION_GET_PRIVATE (self);
}

/**
 * cdn_edge_action_new:
 * @target: A #CdnVariable
 * @equation: A #CdnExpression
 * 
 * Create a new #CdnEdgeAction.
 *
 * Returns: A new #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_edge_action_new (const gchar   *target,
                     CdnExpression *equation)
{
	return g_object_new (CDN_TYPE_EDGE_ACTION,
	                     "target", target,
	                     "equation", equation,
	                     NULL);
}

/**
 * cdn_edge_action_get_target:
 * @action: A #CdnEdgeAction
 *
 * Get the target of the action.
 *
 * Returns: (transfer none): the action target
 *
 **/
const gchar *
cdn_edge_action_get_target (CdnEdgeAction *action)
{
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	return action->priv->target;
}

/**
 * cdn_edge_action_set_target:
 * @action: A #CdnEdgeAction
 * @target: A #CdnVariable
 *
 * Set the target of the action.
 *
 **/
void
cdn_edge_action_set_target (CdnEdgeAction *action,
                            const gchar   *target)
{
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));

	set_target (action, target);
}

/**
 * cdn_edge_action_get_equation:
 * @action: A #CdnEdgeAction
 *
 * Get the equation of the action.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_edge_action_get_equation (CdnEdgeAction *action)
{
	/* Omit type check to increase speed */
	return action->priv->equation;
}

/**
 * cdn_edge_action_set_equation:
 * @action: A #CdnEdgeAction
 * @equation: A #CdnExpression
 *
 * Set the equation of the action.
 *
 **/
void
cdn_edge_action_set_equation (CdnEdgeAction *action,
                              CdnExpression *equation)
{
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));
	g_return_if_fail (equation == NULL || CDN_IS_EXPRESSION (equation));

	set_equation (action, equation);
}

/**
 * cdn_edge_action_copy:
 * @action: A #CdnEdgeAction
 *
 * Create a copy of a #CdnEdgeAction.
 *
 * Returns: (transfer full): A #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_edge_action_copy (CdnEdgeAction *action)
{
	CdnEdgeAction *newaction;

	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	newaction = cdn_edge_action_new (g_strdup (action->priv->target),
	                                 cdn_expression_copy (action->priv->equation));

	cdn_annotatable_set_annotation (CDN_ANNOTATABLE (newaction),
	                                action->priv->annotation);

	cdn_phaseable_copy_to (CDN_PHASEABLE (action),
	                       CDN_PHASEABLE (newaction));

	if (action->priv->index)
	{
		set_index (newaction,
		           cdn_expression_copy (action->priv->index));
	}

	newaction->priv->integrated = action->priv->integrated;
	newaction->priv->integrated_set = action->priv->integrated_set;

	return newaction;
}

/**
 * cdn_edge_action_get_target_variable:
 * @action: A #CdnEdgeAction
 *
 * Get the target property of the link action.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_edge_action_get_target_variable (CdnEdgeAction *action)
{
	/* Omit type check to increase speed */
	return action->priv->property;
}

gboolean
cdn_edge_action_equal (CdnEdgeAction *action,
                       CdnEdgeAction *other)
{
	g_return_val_if_fail (action == NULL || CDN_IS_EDGE_ACTION (action), FALSE);
	g_return_val_if_fail (other == NULL || CDN_IS_EDGE_ACTION (other), FALSE);

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

	return cdn_expression_equal (action->priv->equation,
	                             other->priv->equation,
	                             TRUE);
}

void
_cdn_edge_action_set_target_variable (CdnEdgeAction *action,
                                      CdnVariable   *property)
{
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));
	g_return_if_fail (property == NULL || CDN_IS_VARIABLE (property));

	set_property (action, property);

	if (action->priv->integrated_set)
	{
		CdnVariableFlags flags;

		flags = cdn_variable_get_flags (property);

		if (action->priv->integrated)
		{
			flags |= CDN_VARIABLE_FLAG_INTEGRATED;
		}
		else
		{
			flags &= ~CDN_VARIABLE_FLAG_INTEGRATED;
		}

		cdn_variable_set_flags (property, flags);
	}
}

/**
 * cdn_edge_action_get_edge:
 * @action: the #CdnEdgeAction
 *
 * Get the link associated with the action
 *
 * Returns: (type CdnEdge) (transfer none): the link associated with the action
 **/
CdnEdge *
cdn_edge_action_get_edge (CdnEdgeAction *action)
{
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	return action->priv->link;
}

void
_cdn_edge_action_set_edge (CdnEdgeAction *action,
                           CdnEdge       *link)
{
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));
	g_return_if_fail (link == NULL || CDN_IS_EDGE (link));

	set_edge (action, link);
}

void
cdn_edge_action_set_index (CdnEdgeAction *action,
                           CdnExpression *expression)
{
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));

	set_index (action, expression);
}

static void
get_indices (CdnEdgeAction *action)
{
	CdnDimension dim;
	gdouble const *values;
	gint i;

	values = cdn_expression_evaluate_values (action->priv->index,
	                                         &dim);

	action->priv->num_indices = cdn_dimension_size (&dim);

	g_free (action->priv->indices);
	action->priv->indices = g_new (gint, action->priv->num_indices);

	for (i = 0; i < action->priv->num_indices; ++i)
	{
		action->priv->indices[i] = (gint)rint (values[i]);
	}
}

/**
 * cdn_edge_action_get_indices:
 * @action: a #CdnEdgeAction.
 * @num_indices: (out): return value for the number of indices.
 *
 * Get the indices for this action.
 *
 * Returns: (allow-none) (array length=num_indices): the indices for this action.
 *
 **/
gint const *
cdn_edge_action_get_indices (CdnEdgeAction *action,
                             gint          *num_indices)
{
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	if (!action->priv->index)
	{
		if (num_indices)
		{
			*num_indices = 0;
		}

		return NULL;
	}

	if (!action->priv->indices)
	{
		get_indices (action);
	}

	if (num_indices)
	{
		*num_indices = action->priv->num_indices;
	}

	return action->priv->indices;
}

/**
 * cdn_edge_action_get_index:
 * @action: A #CdnEdgeAction
 *
 * Get the index expression for the action (or %NULL if the action does not
 * have an associated index).
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_edge_action_get_index (CdnEdgeAction *action)
{
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	return action->priv->index;
}

gboolean
cdn_edge_action_compile (CdnEdgeAction     *action,
                         CdnCompileContext *context,
                         CdnCompileError   *error)
{
	CdnVariable *v;

	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), FALSE);
	g_return_val_if_fail (error == NULL || CDN_IS_COMPILE_ERROR (error), FALSE);

	v = cdn_edge_action_get_target_variable (action);

	if (v == NULL)
	{
		if (error)
		{
			GError *gerror;

			gerror = g_error_new (CDN_COMPILE_ERROR_TYPE,
			                      CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
			                      "The variable `%s' for an edge action of `%s' could not be found",
			                      cdn_edge_action_get_target (action),
			                      cdn_object_get_id (CDN_OBJECT (action->priv->link)));

			cdn_compile_error_set (error,
			                       gerror,
			                       CDN_OBJECT (action->priv->link),
			                       NULL,
			                       action,
			                       NULL);

			g_error_free (gerror);
		}

		return FALSE;
	}

	// Compile the target initial value
	if (!cdn_variable_compile (v, error))
	{
		return FALSE;
	}

	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (action->priv->equation)))
	{
		gboolean ret;
		CdnDimension edim;
		CdnDimension dim;

		if (context == NULL)
		{
			context = cdn_object_get_compile_context (CDN_OBJECT (action->priv->link),
			                                          NULL);
		}
		else
		{
			g_object_ref (context);
		}

		ret = cdn_expression_compile (action->priv->equation, context, error);

		if (!ret && error)
		{
			cdn_compile_error_set (error,
			                       NULL,
			                       CDN_OBJECT (action->priv->link),
			                       NULL,
			                       action,
			                       NULL);
		}

		if (!ret)
		{
			g_object_unref (context);
			return ret;
		}

		if (action->priv->index)
		{
			if (!cdn_expression_compile (action->priv->index,
			                             context,
			                             error))
			{
				if (error)
				{
					cdn_compile_error_set (error,
					                       NULL,
					                       CDN_OBJECT (action->priv->link),
					                       NULL,
					                       action,
					                       NULL);
				}

				g_object_unref (context);
				return FALSE;
			}
		}

		cdn_expression_get_dimension (action->priv->equation,
		                              &edim);

		cdn_expression_get_dimension (cdn_variable_get_expression (action->priv->property),
		                              &dim);

		if (!action->priv->index && (dim.rows != edim.rows || dim.columns != edim.columns))
		{
			if (error)
			{
				GError *gerror;

				gerror = g_error_new (CDN_COMPILE_ERROR_TYPE,
				                      CDN_COMPILE_ERROR_INVALID_DIMENSION,
				                      "The dimensions of the edge action (%d-by-%d) and the initial value of `%s' (%d-by-%d) must be the same",
				                      edim.rows,
				                      edim.columns,
				                      cdn_variable_get_name (action->priv->property),
				                      dim.rows,
				                      dim.columns);

				cdn_compile_error_set (error,
				                       gerror,
				                       CDN_OBJECT (action->priv->link),
				                       NULL,
				                       action,
				                       NULL);

				g_error_free (gerror);
			}

			g_object_unref (context);
			return FALSE;
		}

		g_object_unref (context);
	}

	return TRUE;
}

void
_cdn_edge_action_set_integrated (CdnEdgeAction *action,
                                 gboolean       integrated)
{
	action->priv->integrated_set = TRUE;
	action->priv->integrated = integrated;
}

gboolean
_cdn_edge_action_get_integrated (CdnEdgeAction *action,
                                 gboolean      *integrated)
{
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), FALSE);

	if (integrated)
	{
		*integrated = action->priv->integrated;
	}

	return action->priv->integrated_set;
}
