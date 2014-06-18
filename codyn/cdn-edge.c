/*
 * cdn-edge.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write output the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include "cdn-edge.h"
#include "cdn-compile-error.h"
#include <string.h>
#include "cdn-node.h"
#include "cdn-layoutable.h"
#include "cdn-annotatable.h"
#include "cdn-phaseable.h"
#include "cdn-function.h"

#define CDN_EDGE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CDN_TYPE_EDGE, CdnEdgePrivate))

enum
{
	EXT_PROPERTY_ADDED,
	EXT_PROPERTY_REMOVED,
	NUM_EXT_SIGNALS
};

struct _CdnEdgePrivate
{
	// input and output objects
	CdnNode *input;
	CdnNode *output;

	// list of expressions output evaluate
	GSList *actions;
	GHashTable *phases;

	CdnNode *prev_parent;

	guint ext_signals[NUM_EXT_SIGNALS];
};

/* Properties */
enum
{
	PROP_0,
	PROP_OUTPUT,
	PROP_INPUT
};

/* Signals */
enum
{
	ACTION_ADDED,
	ACTION_REMOVED,
	NUM_SIGNALS
};

guint signals[NUM_SIGNALS] = {0,};

static void cdn_layoutable_iface_init (gpointer iface);
static void cdn_phaseable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnEdge,
                         cdn_edge,
                         CDN_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_LAYOUTABLE,
                                                cdn_layoutable_iface_init);
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_PHASEABLE,
                                                cdn_phaseable_iface_init))

static gboolean
cdn_layoutable_supports_location_impl (CdnLayoutable *layoutable)
{
	CdnEdge *edge;

	edge = CDN_EDGE (layoutable);

	return (edge->priv->input == NULL || edge->priv->output == NULL);
}

static GHashTable *
cdn_phaseable_get_phase_table_impl (CdnPhaseable *phaseable)
{
	return CDN_EDGE (phaseable)->priv->phases;
}

static void
cdn_phaseable_set_phase_table_impl (CdnPhaseable *phaseable,
                                    GHashTable   *table)
{
	CdnEdge *edge;

	edge = CDN_EDGE (phaseable);

	if (edge->priv->phases)
	{
		g_hash_table_unref (edge->priv->phases);
		edge->priv->phases = NULL;
	}

	if (table)
	{
		edge->priv->phases = table;
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
cdn_layoutable_iface_init (gpointer iface)
{
	CdnLayoutableInterface *layout;

	layout = iface;

	layout->supports_location = cdn_layoutable_supports_location_impl;
}

static void
cdn_edge_finalize (GObject *object)
{
	CdnEdge *edge;

	edge = CDN_EDGE (object);

	if (edge->priv->phases)
	{
		g_hash_table_destroy (edge->priv->phases);
		edge->priv->phases = NULL;
	}

	G_OBJECT_CLASS (cdn_edge_parent_class)->finalize (object);
}

static void
cdn_edge_get_property (GObject     *object,
                       guint        prop_id,
                       GValue      *value,
                       GParamSpec  *pspec)
{
	CdnEdge *edge = CDN_EDGE (object);

	switch (prop_id)
	{
		case PROP_OUTPUT:
			g_value_set_object (value, edge->priv->output);
		break;
		case PROP_INPUT:
			g_value_set_object (value, edge->priv->input);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
update_action_property (CdnEdge       *edge,
                        CdnEdgeAction *action)
{
	gchar const *target = cdn_edge_action_get_target (action);
	CdnVariable *prop = NULL;

	if (edge->priv->output)
	{
		prop = cdn_object_get_variable (CDN_OBJECT (edge->priv->output),
		                                target);
	}

	_cdn_edge_action_set_target_variable (action, prop);
}


static void
resolve_edge_actions (CdnEdge *edge)
{
	GSList *item;
	GSList *copy = g_slist_copy (edge->priv->actions);

	for (item = copy; item; item = g_slist_next (item))
	{
		update_action_property (edge, item->data);
	}

	g_slist_free (copy);

	cdn_object_taint (CDN_OBJECT (edge));
}

static void
on_variable_added_removed (CdnEdge *edge)
{
	resolve_edge_actions (edge);
}

static void
set_output (CdnEdge  *edge,
        CdnNode *target)
{
	if (edge->priv->output)
	{
		if (edge->priv->input != edge->priv->output)
		{
			_cdn_node_unlink (edge->priv->output, edge);
		}

		g_signal_handler_disconnect (edge->priv->output,
		                             edge->priv->ext_signals[EXT_PROPERTY_ADDED]);

		g_signal_handler_disconnect (edge->priv->output,
		                             edge->priv->ext_signals[EXT_PROPERTY_REMOVED]);

		g_object_unref (edge->priv->output);

		edge->priv->output = NULL;
	}

	if (target)
	{
		edge->priv->output = g_object_ref (target);

		if (edge->priv->input != edge->priv->output)
		{
			_cdn_node_link (target, edge);
		}

		edge->priv->ext_signals[EXT_PROPERTY_ADDED] =
			g_signal_connect_swapped (edge->priv->output,
			                          "variable-added",
			                          G_CALLBACK (on_variable_added_removed),
			                          edge);

		edge->priv->ext_signals[EXT_PROPERTY_REMOVED] =
			g_signal_connect_swapped (edge->priv->output,
			                          "variable-removed",
			                          G_CALLBACK (on_variable_added_removed),
			                          edge);
	}

	resolve_edge_actions (edge);

	cdn_object_taint (CDN_OBJECT (edge));
}

static void
set_input (CdnEdge  *edge,
          CdnNode *target)
{
	if (edge->priv->input)
	{
		if (edge->priv->input != edge->priv->output)
		{
			_cdn_node_unlink (edge->priv->input, edge);
		}

		g_object_unref (edge->priv->input);
		edge->priv->input = NULL;
	}

	if (target)
	{
		edge->priv->input = g_object_ref (target);

		if (edge->priv->input != edge->priv->output)
		{
			_cdn_node_link (target, edge);
		}
	}

	cdn_object_taint (CDN_OBJECT (edge));
}

static void
cdn_edge_set_property (GObject       *object,
                       guint          prop_id,
                       GValue const  *value,
                       GParamSpec    *pspec)
{
	CdnEdge *edge = CDN_EDGE (object);

	switch (prop_id)
	{
		case PROP_OUTPUT:
		{
			set_output (edge, g_value_get_object (value));
		}
		break;
		case PROP_INPUT:
		{
			set_input (edge, g_value_get_object (value));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
check_modified_for_template (CdnEdge       *edge,
                             CdnEdgeAction *action)
{
	CdnEdge *templ;

	templ = cdn_edge_get_action_template (edge,
	                                      action,
	                                      TRUE);

	if (templ != NULL)
	{
		cdn_modifiable_set_modified (CDN_MODIFIABLE (action), FALSE);
	}
}

static void
on_template_action_equation_changed (CdnEdgeAction *action,
                                     GParamSpec    *spec,
                                     CdnEdge       *edge)
{
	CdnEdgeAction *orig = cdn_edge_get_action (edge,
	                                           cdn_edge_action_get_target (action));

	if (!orig)
	{
		return;
	}

	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (orig)))
	{
		return;
	}

	CdnEdge *templ = cdn_edge_get_action_template (edge, orig, FALSE);

	if (templ == NULL)
	{
		return;
	}

	CdnEdgeAction *over = cdn_edge_get_action (templ,
	                                           cdn_edge_action_get_target (action));

	if (over != action)
	{
		return;
	}

	cdn_edge_action_set_equation (orig,
	                              cdn_expression_copy (cdn_edge_action_get_equation (action)));

	cdn_modifiable_set_modified (CDN_MODIFIABLE (orig), FALSE);
}

static void
on_action_target_changed (CdnEdgeAction *action,
                          GParamSpec    *spec,
                          CdnEdge       *edge)
{
	update_action_property (edge, action);

	cdn_object_taint (CDN_OBJECT (edge));
}

static void
on_action_equation_changed (CdnEdgeAction *action,
                            GParamSpec    *spec,
                            CdnEdge       *edge)
{
	check_modified_for_template (edge, action);

	cdn_object_taint (CDN_OBJECT (edge));
}

static void
on_action_modified (CdnEdge       *edge,
                    GParamSpec    *spec,
                    CdnEdgeAction *action)
{
	check_modified_for_template (edge, action);
}

static void
remove_action (CdnEdge       *edge,
               CdnEdgeAction *action)
{
	_cdn_edge_action_set_target_variable (action, NULL);
	_cdn_edge_action_set_edge (action, NULL);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_target_changed,
	                                      edge);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_equation_changed,
	                                      edge);

	g_signal_handlers_disconnect_by_func (action,
	                                      on_action_modified,
	                                      edge);
}

static void
disconnect_template_action (CdnEdge       *edge,
                            CdnEdge       *templ,
                            CdnEdgeAction *action)
{
	g_signal_handlers_disconnect_by_func (action,
	                                      on_template_action_equation_changed,
	                                      edge);
}

static void
on_template_action_added (CdnEdge       *templ,
                          CdnEdgeAction *action,
                          CdnEdge       *edge)
{
	CdnEdgeAction *orig =
		cdn_edge_get_action (edge,
		                     cdn_edge_action_get_target (action));

	if (orig == NULL ||
	    cdn_edge_get_action_template (edge, orig, TRUE))
	{
		if (cdn_edge_add_action (edge,
		                         cdn_edge_action_copy (action)))
		{
			orig = cdn_edge_get_action (edge,
			                            cdn_edge_action_get_target (action));

			if (orig)
			{
				cdn_modifiable_set_modified (CDN_MODIFIABLE (orig), FALSE);
			}
		}
		else
		{
			return;
		}
	}

	g_signal_connect (action,
	                  "notify::equation",
	                  G_CALLBACK (on_template_action_equation_changed),
	                  edge);
}

static void
on_template_action_removed (CdnEdge       *templ,
                            CdnEdgeAction *action,
                            CdnEdge       *edge)
{
	CdnEdgeAction *orig =
		cdn_edge_get_action (edge,
		                     cdn_edge_action_get_target (action));

	if (orig && !cdn_modifiable_get_modified (CDN_MODIFIABLE (orig)) &&
	    cdn_edge_get_action_template (edge, orig, TRUE) == templ)
	{
		/* Remove the original property as well */
		cdn_edge_remove_action (edge, orig);
	}

	disconnect_template_action (edge, templ, action);
}

static CdnEdge *
find_template_for_attachments (CdnEdge *edge)
{
	GSList const *templates;
	CdnEdge *ret = NULL;

	templates = cdn_object_get_applied_templates (CDN_OBJECT (edge));

	/* Find the last template that has both output and input set */
	while (templates)
	{
		if (CDN_IS_EDGE (templates->data))
		{
			CdnEdge *templ = templates->data;

			if (templ->priv->output != NULL && templ->priv->input != NULL)
			{
				ret = templ;
			}
		}

		templates = g_slist_next (templates);
	}

	return ret;
}

static CdnNode *
find_in_parent (CdnEdge  *edge,
                CdnNode *obj)
{
	if (obj == NULL)
	{
		return NULL;
	}

	CdnNode *parent;

	parent = CDN_NODE (cdn_object_get_parent (CDN_OBJECT (edge)));

	if (parent)
	{
		CdnObject *child;

		child = cdn_node_get_child (parent,
		                             cdn_object_get_id (CDN_OBJECT (obj)));

		if (child && CDN_IS_NODE (child))
		{
			return CDN_NODE (child);
		}
		else
		{
			return NULL;
		}
	}
	else
	{
		return NULL;
	}
}

static void
attach_from_template (CdnEdge *edge)
{
	CdnEdge *ret = find_template_for_attachments (edge);
	CdnNode *input;
	CdnNode *output;

	if (ret == NULL)
	{
		return;
	}

	input = find_in_parent (edge, ret->priv->input);
	output = find_in_parent (edge, ret->priv->output);

	if (input == NULL || output == NULL)
	{
		return;
	}

	/* Find the corresponding child in the parent */
	cdn_edge_attach (edge, input, output);
}

static void
on_template_to_changed (CdnEdge    *edge,
                        GParamSpec *spec,
                        CdnEdge    *templ)
{
	attach_from_template (edge);
}

static void
on_template_from_changed (CdnEdge    *edge,
                          GParamSpec *spec,
                          CdnEdge    *templ)
{
	attach_from_template (edge);
}

static void
disconnect_template (CdnEdge   *edge,
                     CdnObject *templ,
                     gboolean   disconnect_actions)
{
	if (disconnect_actions && CDN_IS_EDGE (templ))
	{
		CdnEdge *templ_edge = CDN_EDGE (templ);
		GSList *item;

		for (item = templ_edge->priv->actions; item; item = g_slist_next (item))
		{
			disconnect_template_action (edge, templ_edge, item->data);
		}
	}

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_action_added,
	                                      edge);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_action_removed,
	                                      edge);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_to_changed,
	                                      edge);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_from_changed,
	                                      edge);
}

static void
on_parent_child_removed (CdnNode  *parent,
                         CdnObject *child,
                         CdnEdge   *self)
{
	if (child == CDN_OBJECT (self->priv->output) ||
	    child == CDN_OBJECT (self->priv->input))
	{
		cdn_edge_attach (self, NULL, NULL);
	}
}

static void
cdn_edge_dispose (GObject *object)
{
	CdnEdge *edge = CDN_EDGE (object);

	set_output (edge, NULL);
	set_input (edge, NULL);

	GSList *item;

	for (item = edge->priv->actions; item; item = g_slist_next (item))
	{
		remove_action (edge, item->data);
		g_object_unref (item->data);
	}

	g_slist_free (edge->priv->actions);
	edge->priv->actions = NULL;

	GSList const *templates = cdn_object_get_applied_templates (CDN_OBJECT (object));

	while (templates)
	{
		disconnect_template (edge, templates->data, TRUE);

		templates = g_slist_next (templates);
	}

	if (edge->priv->prev_parent != NULL)
	{
		g_object_remove_weak_pointer (G_OBJECT (edge->priv->prev_parent),
		                              (gpointer *)&(edge->priv->prev_parent));

		g_signal_handlers_disconnect_by_func (edge->priv->prev_parent,
		                                      on_parent_child_removed,
		                                      edge);

		edge->priv->prev_parent = NULL;
	}

	G_OBJECT_CLASS (cdn_edge_parent_class)->dispose (object);
}

static void
cdn_edge_foreach_expression_impl (CdnObject                *object,
                                  CdnForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_edge_parent_class)->foreach_expression != NULL)
	{
		CDN_OBJECT_CLASS (cdn_edge_parent_class)->foreach_expression (object,
		                                                              func,
		                                                              userdata);
	}

	/* Reset action expressions */
	GSList *item;

	for (item = CDN_EDGE (object)->priv->actions; item; item = g_slist_next (item))
	{
		func (cdn_edge_action_get_equation (item->data), userdata);
	}
}

static void
copy_edge_actions (CdnEdge *dest,
                   CdnEdge *source)
{
	GSList *item;

	for (item = source->priv->actions; item; item = g_slist_next (item))
	{
		cdn_edge_add_action (dest,
		                     cdn_edge_action_copy (item->data));
	}
}

static void
cdn_edge_copy_impl (CdnObject *object,
                    CdnObject *source)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_edge_parent_class)->copy != NULL)
	{
		CDN_OBJECT_CLASS (cdn_edge_parent_class)->copy (object, source);
	}

	/* Copy over edge actions */
	copy_edge_actions (CDN_EDGE (object), CDN_EDGE (source));

	// Copy phases
	cdn_phaseable_copy_to (CDN_PHASEABLE (source),
	                       CDN_PHASEABLE (object));
}

static void
prepend_functions (CdnNode          *obj,
                   CdnCompileContext *context)
{
	GSList const *item;

	if (!obj)
	{
		return;
	}

	item = cdn_node_get_children (obj);

	while (item)
	{
		if (CDN_IS_FUNCTION (item->data))
		{
			cdn_compile_context_prepend_function (context,
			                                      item->data);
		}

		item = g_slist_next (item);
	}
}

static CdnCompileContext *
cdn_edge_get_compile_context_impl (CdnObject         *object,
                                   CdnCompileContext *context)
{
	CdnEdge *edge;

	edge = CDN_EDGE (object);

	/* Note: we repeat this logic here input cdn-object because we need
	   output prepend the 'input' object before the real object... */
	if (!context)
	{
		if (cdn_object_get_parent (object))
		{
			context = cdn_object_get_compile_context (CDN_OBJECT (cdn_object_get_parent (object)),
			                                          NULL);
		}
		else
		{
			context = cdn_compile_context_new ();
		}
	}

	cdn_compile_context_prepend_object (context,
	                                    CDN_OBJECT (edge->priv->input));

	prepend_functions (edge->priv->input, context);

	CDN_OBJECT_CLASS (cdn_edge_parent_class)->get_compile_context (object, context);

	return context;
}

static gboolean
cdn_edge_compile_impl (CdnObject         *object,
                       CdnCompileContext *context,
                       CdnCompileError   *error)
{
	CdnEdge *edge = CDN_EDGE (object);

	if (cdn_object_is_compiled (object))
	{
		return TRUE;
	}

	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	context = cdn_edge_get_compile_context_impl (object, context);

	/* Chain up, compile object */
	if (CDN_OBJECT_CLASS (cdn_edge_parent_class)->compile)
	{
		if (!CDN_OBJECT_CLASS (cdn_edge_parent_class)->compile (object, context, error))
		{
			cdn_compile_context_restore (context);
			g_object_unref (context);
			return FALSE;
		}
	}

	/* Compile all actions */
	GSList const *actions = cdn_edge_get_actions (edge);
	gboolean ret = TRUE;

	while (actions)
	{
		if (!cdn_edge_action_compile (actions->data, context, error))
		{
			ret = FALSE;
			break;
		}

		actions = g_slist_next (actions);
	}

	cdn_compile_context_restore (context);
	g_object_unref (context);

	return ret;
}

static gboolean
cdn_edge_equal_impl (CdnObject *first,
                     CdnObject *second)
{
	if (!CDN_OBJECT_CLASS (cdn_edge_parent_class)->equal (first, second))
	{
		return FALSE;
	}

	CdnEdge *edge1 = CDN_EDGE (first);
	CdnEdge *edge2 = CDN_EDGE (second);

	if ((edge1->priv->input == NULL && edge2->priv->input != NULL) ||
	    (edge2->priv->input == NULL && edge1->priv->input != NULL) ||
	    (edge1->priv->output == NULL && edge2->priv->output != NULL) ||
	    (edge2->priv->output == NULL && edge1->priv->output != NULL))
	{
		return FALSE;
	}

	if (edge1->priv->input &&
	    g_strcmp0 (cdn_object_get_id (CDN_OBJECT (edge1->priv->input)),
	               cdn_object_get_id (CDN_OBJECT (edge2->priv->input))) != 0)
	{
		return FALSE;
	}

	if (edge1->priv->output &&
	    g_strcmp0 (cdn_object_get_id (CDN_OBJECT (edge1->priv->output)),
	               cdn_object_get_id (CDN_OBJECT (edge2->priv->output))) != 0)
	{
		return FALSE;
	}

	if (g_slist_length (edge1->priv->actions) != g_slist_length (edge2->priv->actions))
	{
		return FALSE;
	}

	GSList const *actions1 = cdn_edge_get_actions (edge1);

	while (actions1)
	{
		CdnEdgeAction *ac1 = actions1->data;
		CdnExpression *index;

		index = cdn_edge_action_get_index (ac1);

		CdnEdgeAction *ac2 = cdn_edge_get_action_with_index (edge2,
		                                                     cdn_edge_action_get_target (ac1),
		                                                     index);

		if (!ac2 || !cdn_edge_action_equal (ac1, ac2))
		{
			return FALSE;
		}

		actions1 = g_slist_next (actions1);
	}

	return TRUE;
}

static gboolean
cdn_edge_unapply_template_impl (CdnObject  *object,
                                CdnObject  *templ,
                                GError    **error)
{
	if (CDN_IS_EDGE (templ))
	{
		GSList *item;
		CdnEdge *templ_edge = CDN_EDGE (templ);
		CdnEdge *edge = CDN_EDGE (object);

		for (item = templ_edge->priv->actions; item; item = g_slist_next (item))
		{
			on_template_action_removed (templ_edge, item->data, edge);
		}

		disconnect_template (edge, templ, FALSE);

		attach_from_template (edge);
	}

	/* Chain up */
	return CDN_OBJECT_CLASS (cdn_edge_parent_class)->unapply_template (object, templ, error);
}

static void
connect_template (CdnEdge *edge,
                  CdnEdge *templ)
{
	g_signal_connect (templ,
	                  "action-added",
	                  G_CALLBACK (on_template_action_added),
	                  edge);

	g_signal_connect (templ,
	                  "action-removed",
	                  G_CALLBACK (on_template_action_removed),
	                  edge);

	g_signal_connect_swapped (templ,
	                          "notify::output",
	                          G_CALLBACK (on_template_to_changed),
	                          edge);

	g_signal_connect_swapped (templ,
	                          "notify::input",
	                          G_CALLBACK (on_template_from_changed),
	                          edge);
}

static gboolean
cdn_edge_apply_template_impl (CdnObject  *object,
                              CdnObject  *templ,
                              GError    **error)
{
	/* Chain up first, transfer properties and such */
	if (!CDN_OBJECT_CLASS (cdn_edge_parent_class)->apply_template (object, templ, error))
	{
		return FALSE;
	}

	if (CDN_IS_EDGE (templ))
	{
		CdnEdge *templ_edge = CDN_EDGE (templ);
		GSList *item;

		for (item = templ_edge->priv->actions; item; item = g_slist_next (item))
		{
			on_template_action_added (templ_edge, item->data, CDN_EDGE (object));
		}

		attach_from_template (CDN_EDGE (object));

		connect_template (CDN_EDGE (object),
		                  templ_edge);
	}

	return TRUE;
}

static void
action_removed_impl (CdnEdge   *edge,
                     CdnEdgeAction *action)
{
	edge->priv->actions = g_slist_remove (edge->priv->actions, action);

	remove_action (edge, action);
}

static void
cdn_edge_class_init (CdnEdgeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdnobject_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_edge_finalize;
	object_class->dispose = cdn_edge_dispose;

	object_class->get_property = cdn_edge_get_property;
	object_class->set_property = cdn_edge_set_property;

	klass->action_removed = action_removed_impl;

	cdnobject_class->foreach_expression = cdn_edge_foreach_expression_impl;
	cdnobject_class->copy = cdn_edge_copy_impl;
	cdnobject_class->compile = cdn_edge_compile_impl;
	cdnobject_class->get_compile_context = cdn_edge_get_compile_context_impl;
	cdnobject_class->equal = cdn_edge_equal_impl;
	cdnobject_class->apply_template = cdn_edge_apply_template_impl;
	cdnobject_class->unapply_template = cdn_edge_unapply_template_impl;

	/**
	 * CdnEdge::action-added:
	 * @object: a #CdnObject
	 * @action: the added #CdnEdgeAction
	 *
	 * Emitted when a edge action is added output the edge
	 *
	 **/
	signals[ACTION_ADDED] =
		g_signal_new ("action-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnEdgeClass,
		                               action_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_EDGE_ACTION);

	/**
	 * CdnEdge::action-removed:
	 * @object: a #CdnObject
	 * @action: the removed #CdnEdgeAction
	 *
	 * Emitted when a edge action is removed input the edge
	 *
	 **/
	signals[ACTION_REMOVED] =
		g_signal_new ("action-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnEdgeClass,
		                               action_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_EDGE_ACTION);

	/**
	 * CdnEdge:input:
	 *
	 * The input #CdnObject
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_INPUT,
	                                 g_param_spec_object ("input",
	                                                      "INPUT",
	                                                      "The edge input object",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CdnEdge:output:
	 *
	 * The output #CdnObject
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OUTPUT,
	                                 g_param_spec_object ("output",
	                                                      "OUTPUT",
	                                                      "The edge output object",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_type_class_add_private (object_class, sizeof (CdnEdgePrivate));
}

static void
on_parent_changed (CdnEdge *self)
{
	if (self->priv->prev_parent != NULL)
	{
		g_object_remove_weak_pointer (G_OBJECT (self->priv->prev_parent),
		                              (gpointer *)&(self->priv->prev_parent));

		g_signal_handlers_disconnect_by_func (self->priv->prev_parent,
		                                      on_parent_child_removed,
		                                      self);

		cdn_edge_attach (self, NULL, NULL);
		self->priv->prev_parent = NULL;
	}

	self->priv->prev_parent = cdn_object_get_parent (CDN_OBJECT (self));

	if (self->priv->prev_parent != NULL)
	{
		g_object_add_weak_pointer (G_OBJECT (self->priv->prev_parent),
		                           (gpointer *)&(self->priv->prev_parent));

		g_signal_connect (self->priv->prev_parent,
		                  "child-removed",
		                  G_CALLBACK (on_parent_child_removed),
		                  self);
	}
}

static void
cdn_edge_init (CdnEdge *self)
{
	self->priv = CDN_EDGE_GET_PRIVATE (self);

	g_signal_connect (self, "notify::parent", G_CALLBACK (on_parent_changed), NULL);
}

/**
 * cdn_edge_new:
 * @id: the object id
 * @input: (allow-none): a #CdnObject
 * @output: (allow-none): a #CdnObject
 *
 * Create a new #CdnEdge
 *
 * Returns: a new #CdnEdge
 *
 **/
CdnEdge *
cdn_edge_new (gchar const *id,
              CdnNode    *input,
              CdnNode    *output)
{
	return g_object_new (CDN_TYPE_EDGE,
	                     "id", id,
	                     "input", input,
	                     "output", output, NULL);
}

/**
 * cdn_edge_add_action:
 * @edge: the #CdnEdge
 * @action: the #CdnEdgeAction
 *
 * Add a new action output be performed when the edge is evaluated during
 * simulation. Note that if an action with the same
 * target already exists, the action information is transfered output the existing
 * action instance. This means that the specified @action might not actually
 * be added output the object. Also, since a #CdnEdgeAction is a #GInitiallyUnowned,
 * @action will be destroyed after the call output #cdn_edge_add_action in
 * the above described case, unless you explicitly sink the floating reference.
 *
 * In the case that you can not know whether an action is overriding an
 * existing action in @edge, never use @action after a call output
 * #cdn_edge_add_action. Instead, retrieve the corresponding action
 * using #cdn_edge_get_action after the call output #cdn_edge_add_action.
 *
 * Returns: %TRUE if @action could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cdn_edge_add_action (CdnEdge       *edge,
                     CdnEdgeAction *action)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), FALSE);
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), FALSE);

	gchar const *target;
	CdnEdgeAction *orig;
	CdnExpression *index;

	target = cdn_edge_action_get_target (action);
	index = cdn_edge_action_get_index (action);

	orig = cdn_edge_get_action_with_index (edge,
	                                       target,
	                                       index);

	if (orig != NULL && cdn_phaseable_equal (CDN_PHASEABLE (orig),
	                                         CDN_PHASEABLE (action)))
	{
		cdn_edge_action_set_equation (orig,
		                              cdn_edge_action_get_equation (action));

		// Copy the annotation
		gchar *an = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (action));

		if (an && *an)
		{
			cdn_annotatable_set_annotation (CDN_ANNOTATABLE (orig),
			                                an);
		}

		g_free (an);

		if (g_object_is_floating (action))
		{
			g_object_unref (action);
		}

		return TRUE;
	}

	edge->priv->actions = g_slist_append (edge->priv->actions,
	                                      action);

	g_object_ref_sink (action);
	update_action_property (edge, action);

	g_signal_connect (action,
	                  "notify::target",
	                  G_CALLBACK (on_action_target_changed),
	                  edge);

	g_signal_connect (action,
	                  "notify::equation",
	                  G_CALLBACK (on_action_equation_changed),
	                  edge);

	g_signal_connect_swapped (action,
	                          "notify::modified",
	                          G_CALLBACK (on_action_modified),
	                          edge);

	_cdn_edge_action_set_edge (action, edge);

	cdn_object_taint (CDN_OBJECT (edge));

	g_signal_emit (edge, signals[ACTION_ADDED], 0, action);

	return TRUE;
}

/**
 * cdn_edge_remove_action:
 * @edge: the #CdnEdge
 * @action: the #CdnEdgeAction
 *
 * Removes an action input the edge.
 *
 * Returns: %TRUE if the action was successfully removed
 *
 **/
gboolean
cdn_edge_remove_action (CdnEdge       *edge,
                        CdnEdgeAction *action)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), FALSE);
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), FALSE);

	GSList *item = g_slist_find (edge->priv->actions, action);

	if (item != NULL)
	{
		g_signal_emit (edge, signals[ACTION_REMOVED], 0, action);
		g_object_unref (action);

		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_edge_get_input:
 * @edge: the #CdnEdge
 *
 * Returns the input #CdnNode of the edge
 *
 * Returns: (transfer none): the input #CdnNode
 *
 **/
CdnNode *
cdn_edge_get_input (CdnEdge *edge)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);

	return edge->priv->input;
}

/**
 * cdn_edge_get_output:
 * @edge: the #CdnEdge
 *
 * Returns the output #CdnNode of the edge
 *
 * Returns: (transfer none): the output #CdnNode
 *
 **/
CdnNode *
cdn_edge_get_output (CdnEdge *edge)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);

	return edge->priv->output;
}

/**
 * cdn_edge_get_actions:
 * @edge: the #CdnEdge
 *
 * Get edge actions
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): list of #CdnEdgeAction. The list is
 *          owned by the edge and should not be freed
 *
 **/
const GSList *
cdn_edge_get_actions (CdnEdge *edge)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);

	return edge->priv->actions;
}

/**
 * cdn_edge_get_action:
 * @edge: A #CdnEdge
 * @target: The target property name
 *
 * Get a #CdnEdgeAction targetting the property @target.
 *
 * Returns: (transfer none): A #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_edge_get_action (CdnEdge     *edge,
                     gchar const *target)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);
	g_return_val_if_fail (target != NULL, NULL);

	return cdn_edge_get_action_with_index (edge,
	                                       target,
	                                       NULL);
}

static CdnEdgeAction *
edge_get_action_intern (CdnEdge       *edge,
                        gchar const   *target,
                        CdnExpression *index,
                        GSList const  *phases,
                        gboolean       compare_phases)
{
	GSList *actions = edge->priv->actions;

	while (actions)
	{
		CdnEdgeAction *action = actions->data;
		CdnExpression *oidx;

		actions = g_slist_next (actions);

		oidx = cdn_edge_action_get_index (action);

		if ((index != NULL) != (oidx != NULL))
		{
			continue;
		}

		if (g_strcmp0 (cdn_edge_action_get_target (action), target) != 0)
		{
			continue;
		}

		if (oidx && !cdn_expression_equal (oidx, index, TRUE))
		{
			continue;
		}

		if (compare_phases)
		{
			GHashTable *table;
			GSList const *item;
			gint num = 0;

			table = cdn_phaseable_get_phase_table (CDN_PHASEABLE (action));

			if ((!table || g_hash_table_size (table) == 0))
			{
				if (!phases)
				{
					return action;
				}
				else
				{
					continue;
				}
			}

			for (item = phases; item; item = g_slist_next (item))
			{
				if (!g_hash_table_lookup (table, item->data))
				{
					continue;
				}

				++num;
			}

			if (num != g_hash_table_size (table))
			{
				continue;
			}
		}

		return action;
	}

	return NULL;
}

/**
 * cdn_edge_get_action_with_index:
 * @edge: A #CdnEdge
 * @target: The action target
 * @index: A #CdnExpression
 *
 * Get the action for a target with a specific index.
 *
 * Returns: (transfer none): A #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_edge_get_action_with_index (CdnEdge       *edge,
                                gchar const   *target,
                                CdnExpression *index)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (index == NULL || CDN_IS_EXPRESSION (index), NULL);

	return edge_get_action_intern (edge, target, index, NULL, FALSE);
}

/**
 * cdn_edge_get_action_with_index_and_phases:
 * @edge: A #CdnEdge
 * @target: The action target
 * @index: A #CdnExpression
 * @phases: (element-type utf8) (transfer none): a list of phases
 *
 * Get the action for a target with a specific index and set of phases.
 *
 * Returns: (transfer none): A #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_edge_get_action_with_index_and_phases (CdnEdge       *edge,
                                           gchar const   *target,
                                           CdnExpression *index,
                                           GSList const  *phases)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);
	g_return_val_if_fail (target != NULL, NULL);
	g_return_val_if_fail (index == NULL || CDN_IS_EXPRESSION (index), NULL);

	return edge_get_action_intern (edge, target, index, phases, TRUE);
}

/**
 * cdn_edge_attach:
 * @edge: (allow-none): A #CdnEdge
 * @input: (allow-none): A #CdnNode
 * @output: A #CdnNode
 *
 * Attach @edge output the objects @input and @output. This is equivalent to:
 * <informalexample>
 * <programlisting>
 * g_object_set (edge, "input", input, "output", output);
 * </programlisting>
 * </informalexample>
 *
 **/
void
cdn_edge_attach (CdnEdge *edge,
                 CdnNode *input,
                 CdnNode *output)
{
	g_return_if_fail (CDN_IS_EDGE (edge));
	g_return_if_fail ((input == NULL) == (output == NULL));
	g_return_if_fail (input == NULL || CDN_IS_OBJECT (input));
	g_return_if_fail (output == NULL || CDN_IS_OBJECT (output));

	g_object_set (edge, "input", input, "output", output, NULL);
}

/**
 * cdn_edge_get_action_template:
 * @edge: A #CdnEdge
 * @action: A #CdnEdgeAction
 * @match_full: How output match the action
 *
 * Get the template on which @action is defined, if any. If @match_full is
 * %TRUE, the template will only be possitively matched if both actions are
 * equal (i.e. if an action originated input a template, but was later modified,
 * this function will not return the original template object).
 *
 * Returns: (transfer none): A #CdnEdge or %NULL if the template could not be found
 *
 **/
CdnEdge *
cdn_edge_get_action_template (CdnEdge       *edge,
                              CdnEdgeAction *action,
                              gboolean       match_full)
{
	g_return_val_if_fail (CDN_IS_EDGE (edge), NULL);
	g_return_val_if_fail (CDN_IS_EDGE_ACTION (action), NULL);

	GSList *templates = g_slist_copy ((GSList *)cdn_object_get_applied_templates (CDN_OBJECT (edge)));
	templates = g_slist_reverse (templates);
	GSList *item;

	gchar const *target = cdn_edge_action_get_target (action);

	for (item = templates; item; item = g_slist_next (item))
	{
		if (!CDN_IS_EDGE (item->data))
		{
			continue;
		}

		CdnEdgeAction *taction;
		CdnEdge *templ = item->data;

		taction = cdn_edge_get_action (templ, target);

		if (taction && (!match_full || cdn_edge_action_equal (action, taction)))
		{
			g_slist_free (templates);
			return templ;
		}
	}

	g_slist_free (templates);

	return NULL;
}
