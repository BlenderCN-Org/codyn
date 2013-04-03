/*
 * cdn-node.c
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

#include "cdn-node.h"
#include "cdn-edge.h"
#include "cdn-compile-error.h"
#include "cdn-compile-context.h"
#include "cdn-marshal.h"
#include "cdn-utils.h"
#include "cdn-selector.h"
#include "cdn-layoutable.h"
#include "cdn-io.h"

#define CDN_NODE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_NODE, CdnNodePrivate))

enum
{
	EXT_PROPERTY_ADDED,
	EXT_PROPERTY_REMOVED,
	EXT_INTERFACE_PROPERTY_ADDED,
	EXT_INTERFACE_PROPERTY_REMOVED,
	NUM_EXT_SIGNALS
};

struct _CdnNodePrivate
{
	GSList *children;
	GHashTable *child_hash;

	CdnVariableInterface *variable_interface;

	GSList *edges;
	GSList *actors;
	CdnEdge *self_edge;

	gchar *state;
};

G_DEFINE_TYPE (CdnNode, cdn_node, CDN_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_STATE
};

enum
{
	CHILD_ADDED,
	CHILD_REMOVED,
	VERIFY_ADD_CHILD,
	VERIFY_REMOVE_CHILD,
	NUM_SIGNALS
};

static guint node_signals[NUM_SIGNALS] = {0,};

static void remove_object (CdnNode *node, CdnObject *object);

GQuark
cdn_node_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_node_error");
	}

	return quark;
}

static void
edge_destroyed (CdnNode  *node,
                CdnEdge   *edge,
                gboolean   is_last_ref)
{
	if (!is_last_ref)
	{
		return;
	}

	/* Remove edge, and toggle ref */
	node->priv->edges = g_slist_remove (node->priv->edges, edge);

	g_object_remove_toggle_ref (G_OBJECT (edge),
	                            (GToggleNotify)edge_destroyed,
	                            node);
}


static void
cdn_node_finalize (GObject *object)
{
	CdnNode *node = CDN_NODE (object);

	g_hash_table_destroy (node->priv->child_hash);

	g_slist_free (node->priv->edges);
	g_slist_free (node->priv->actors);

	G_OBJECT_CLASS (cdn_node_parent_class)->finalize (object);
}

static CdnObject *
get_child_from_template (CdnNode  *node,
                         CdnObject *templ)
{
	GSList const *children;

	children = node->priv->children;

	while (children)
	{
		CdnObject *obj = children->data;
		GSList const *templates = cdn_object_get_applied_templates (obj);

		if (g_slist_find ((GSList *)templates, obj))
		{
			return obj;
		}

		children = g_slist_next (children);
	}

	return NULL;
}

static CdnLayoutable *
last_layoutable_template (CdnObject *child)
{
	CdnLayoutable *last = NULL;
	GSList const *templates;

	templates = cdn_object_get_applied_templates (child);

	while (templates)
	{
		if (CDN_IS_LAYOUTABLE (templates->data) &&
		    cdn_layoutable_supports_location (templates->data) &&
		    cdn_layoutable_get_has_location (templates->data))
		{
			last = templates->data;
		}

		templates = g_slist_next (templates);
	}

	return last;
}

static gboolean
should_propagate_layout (CdnNode  *node,
                         CdnObject *child,
                         CdnObject *templ)
{
	CdnLayoutable *last;

	if (!CDN_IS_LAYOUTABLE (templ) ||
	    !cdn_layoutable_supports_location (CDN_LAYOUTABLE (templ)) ||
	    !cdn_layoutable_get_has_location (CDN_LAYOUTABLE (templ)))
	{
		return FALSE;
	}

	if (!cdn_layoutable_get_has_location (CDN_LAYOUTABLE (child)))
	{
		return TRUE;
	}

	last = last_layoutable_template (child);

	if (last)
	{
		gint lx;
		gint ly;
		gint x;
		gint y;

		cdn_layoutable_get_location (last, &lx, &ly);
		cdn_layoutable_get_location (CDN_LAYOUTABLE (child), &x, &y);

		return x == lx && y == ly;
	}

	return TRUE;
}

static void
layout_from_template (CdnLayoutable *layoutable)
{
	CdnLayoutable *templ;
	gint x;
	gint y;

	cdn_layoutable_set_location (layoutable, 0, 0);
	cdn_layoutable_set_has_location (layoutable, FALSE);

	templ = last_layoutable_template (CDN_OBJECT (layoutable));

	if (!templ)
	{
		return;
	}

	cdn_layoutable_get_location (templ, &x, &y);
	cdn_layoutable_set_location (layoutable, x, y);
}


static void
on_template_child_added (CdnNode  *templ,
                         CdnObject *child,
                         CdnNode  *node)
{
	CdnObject *obj;
	gboolean layout;

	obj = cdn_node_get_child (node, cdn_object_get_id (child));
	layout = should_propagate_layout (node, obj, child);

	if (obj != NULL && G_TYPE_FROM_INSTANCE (child) == G_TYPE_FROM_INSTANCE (obj))
	{
		cdn_object_apply_template (obj, child, NULL);
	}
	else
	{
		obj = cdn_object_new_from_template (child, NULL);
		cdn_node_add (node, obj, NULL);
		g_object_unref (obj);
	}

	if (layout)
	{
		layout_from_template (CDN_LAYOUTABLE (child));
	}
}

static void
on_template_child_removed (CdnNode  *templ,
                           CdnObject *child,
                           CdnNode  *node)
{
	CdnObject *obj;

	obj = get_child_from_template (node, child);

	if (obj)
	{
		GSList *variables;
		gboolean relayout;

		relayout = should_propagate_layout (node, obj, child) &&
		           last_layoutable_template (obj) == CDN_LAYOUTABLE (child);

		cdn_object_unapply_template (obj, child, NULL);

		variables = cdn_object_get_variables (obj);

		if (cdn_object_get_applied_templates (obj) == NULL &&
		    variables == NULL &&
		    (!CDN_IS_NODE (obj) || cdn_node_get_edges (CDN_NODE (obj)) == NULL))
		{
			cdn_node_remove (templ, child, NULL);
		}

		if (relayout)
		{
			layout_from_template (CDN_LAYOUTABLE (child));
		}

		g_slist_free (variables);
	}
}

static gboolean
interface_should_override (CdnNode     *node,
                           CdnNode     *template,
                           gchar const  *name,
                           CdnNode    **last_templ)
{
	GSList const *templates;
	gboolean found = FALSE;

	/* Check if the interface 'name' should override the already existing
	 * interface item (with the same name)
	 *
	 * Conditions:
	 *
	 * 1) No template applied after 'template' adds this interface name
	 *
	 */

	templates = cdn_object_get_applied_templates (CDN_OBJECT (node));

	while (templates)
	{
		CdnNode *tmpl;
		CdnVariableInterface *iface;
		gboolean templprop;

		tmpl = templates->data;
		templates = g_slist_next (templates);

		if (!CDN_IS_NODE (tmpl))
		{
			continue;
		}

		iface = cdn_node_get_variable_interface (tmpl);
		templprop = cdn_variable_interface_implements (iface, name);

		if (!found && templprop && last_templ && tmpl != template)
		{
			*last_templ = tmpl;
		}

		if (found && templprop)
		{
			return FALSE;
		}

		if (tmpl == template)
		{
			found = TRUE;
		}
	}

	return TRUE;
}

static gboolean
add_template_interface (CdnNode     *node,
                        CdnNode     *source,
                        gchar const  *name,
                        gchar const  *child_name,
                        gchar const  *property_name,
                        GError      **error)
{
	CdnVariableInterface *iface;

	iface = cdn_node_get_variable_interface (node);

	if (cdn_variable_interface_implements (iface, name))
	{
		if (interface_should_override (node, source, name, NULL))
		{
			if (!cdn_variable_interface_remove (iface, name, error))
			{
				return FALSE;
			}

			if (!cdn_variable_interface_add (iface,
			                                 name,
			                                 child_name,
			                                 property_name,
			                                 error))
			{
				return FALSE;
			}
		}
	}
	else
	{
		if (!cdn_variable_interface_add (iface,
		                                 name,
		                                 child_name,
		                                 property_name,
		                                 NULL))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
remove_template_interface (CdnNode    *node,
                           CdnNode    *source,
                           gchar const *name,
                           gchar const *child_name,
                           gchar const *property_name)
{
	CdnVariableInterface *iface;
	GError *error = NULL;
	CdnNode *last = NULL;

	/* Check if this interface actually owns that property */
	if (!interface_should_override (node, source, name, &last))
	{
		return;
	}

	/* Remove the mapping */
	iface = cdn_node_get_variable_interface (node);

	if (!cdn_variable_interface_remove (iface, name, &error))
	{
		g_warning ("Could not remove interface property `%s' from template `%s': %s",
		           name,
		           cdn_object_get_id (CDN_OBJECT (source)),
		           error->message);

		g_error_free (error);
		return;
	}

	if (last)
	{
		/* Lookup new child and property name */
		iface = cdn_node_get_variable_interface (last);
		child_name = cdn_variable_interface_lookup_child_name (iface, name);
		property_name = cdn_variable_interface_lookup_variable_name (iface, name);

		add_template_interface (node,
		                        last,
		                        name,
		                        child_name,
		                        property_name,
		                        &error);
	}
}

static void
on_template_interface_variable_added (CdnVariableInterface *templ_iface,
                                      gchar const          *name,
                                      gchar const          *child_name,
                                      gchar const          *property_name,
                                      CdnNode             *node)
{
	add_template_interface (node,
	                        cdn_variable_interface_get_node (templ_iface),
	                        name,
	                        child_name,
	                        property_name,
	                        NULL);
}

static void
on_template_interface_variable_removed (CdnVariableInterface *templ_iface,
                                        gchar const          *name,
                                        gchar const          *child_name,
                                        gchar const          *property_name,
                                        CdnNode             *node)
{
	remove_template_interface (node,
	                           cdn_variable_interface_get_node (templ_iface),
	                           name,
	                           child_name,
	                           property_name);
}

static void
disconnect_template (CdnNode  *node,
                     CdnObject *templ)
{
	CdnVariableInterface *piface;

	piface = cdn_node_get_variable_interface (CDN_NODE (templ));

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_variable_added,
	                                      node);

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_variable_removed,
	                                      node);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_added,
	                                      node);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_removed,
	                                      node);
}

static void
cdn_node_dispose (GObject *object)
{
	CdnNode *node = CDN_NODE (object);
	GSList *copy;
	GSList *item;
	GSList const *templates;

	if (node->priv->children)
	{
		GSList *children = node->priv->children;
		GSList *item;

		node->priv->children = NULL;

		for (item = children; item; item = g_slist_next (item))
		{
			remove_object (node, item->data);
		}

		g_slist_free (children);
	}

	templates = cdn_object_get_applied_templates (CDN_OBJECT (object));

	while (templates)
	{
		disconnect_template (node, templates->data);
		templates = g_slist_next (templates);
	}

	if (node->priv->variable_interface)
	{
		g_object_unref (node->priv->variable_interface);
		node->priv->variable_interface = NULL;
	}

	/* Untoggle ref all edges, because we need them destroyed! */
	copy = g_slist_copy (node->priv->edges);

	for (item = copy; item; item = g_slist_next (item))
	{
		if (item->data != node->priv->self_edge)
		{
			edge_destroyed (node, item->data, TRUE);
		}
	}

	g_slist_free (copy);
	g_slist_free (node->priv->edges);
	node->priv->edges = NULL;

	if (node->priv->self_edge)
	{
		g_object_unref (node->priv->self_edge);
		node->priv->self_edge = NULL;
	}

	G_OBJECT_CLASS (cdn_node_parent_class)->dispose (object);
}

static CdnVariable *
cdn_node_cdn_get_property (CdnObject   *object,
                            gchar const *name)
{
	CdnVariable *prop;
	CdnNode *node = CDN_NODE (object);

	prop = CDN_OBJECT_CLASS (cdn_node_parent_class)->get_variable (object,
	                                                                name);

	if (!prop)
	{
		prop = cdn_variable_interface_lookup (node->priv->variable_interface,
		                                      name);
	}

	return prop;
}

static void
prepend_functions (CdnNode          *node,
                   CdnCompileContext *context)
{
	GSList *item;

	if (!node)
	{
		return;
	}

	item = node->priv->children;

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
cdn_node_cdn_get_compile_context (CdnObject         *object,
                                   CdnCompileContext *context)
{
	CdnCompileContext *ret;
	CdnNode *node;

	node = CDN_NODE (object);

	ret = CDN_OBJECT_CLASS (cdn_node_parent_class)->get_compile_context (object, context);

	prepend_functions (node, ret);
	return ret;
}

static gboolean
cdn_node_cdn_compile (CdnObject         *object,
                       CdnCompileContext *context,
                       CdnCompileError   *error)
{
	CdnNode *node = CDN_NODE (object);
	GSList *item;
	gboolean ret;
	GSList *others = NULL;
	GSList *othersl = NULL;

	if (cdn_object_is_compiled (object))
	{
		return TRUE;
	}

	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	/* Prepend all the defined functions in the instances */
	context = cdn_node_cdn_get_compile_context (object, context);

	item = node->priv->children;

	// First compile inputs and filter out functions (they get compiled on
	// the fly)
	while (item)
	{
		if (!CDN_IS_IO (item->data) && !CDN_IS_FUNCTION (item->data))
		{
			others = g_slist_prepend (others,
			                          item->data);
		}
		else if (CDN_IS_IO (item->data) &&
		         !cdn_object_compile (item->data, context, error))
		{
			cdn_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	others = g_slist_reverse (others);
	item = others;

	while (item)
	{
		if (!CDN_IS_EDGE (item->data))
		{
			othersl = g_slist_prepend (othersl,
			                           item->data);
		}
		else if (!cdn_object_compile (item->data, context, error))
		{
			cdn_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	if (node->priv->self_edge)
	{
		if (!cdn_object_compile (CDN_OBJECT (node->priv->self_edge), context, error))
		{
			cdn_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}
	}

	g_slist_free (others);
	othersl = g_slist_reverse (othersl);
	item = othersl;

	while (item)
	{
		if (!cdn_object_compile (item->data, context, error))
		{
			cdn_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	g_slist_free (othersl);

	ret = CDN_OBJECT_CLASS (cdn_node_parent_class)->compile (object,
	                                                          context,
	                                                          error);

	cdn_compile_context_restore (context);
	g_object_unref (context);

	return ret;
}

static void
cdn_node_cdn_reset (CdnObject *object)
{
	CdnNode *node;

	CDN_OBJECT_CLASS (cdn_node_parent_class)->reset (object);

	node = CDN_NODE (object);

	/* And then also the children! */
	cdn_node_foreach (node,
	                   (GFunc)cdn_object_reset,
	                   NULL);
}

static void
cdn_node_cdn_foreach_expression (CdnObject                *object,
                                  CdnForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	CdnNode *node;
	GSList *item;

	node = CDN_NODE (object);

	CDN_OBJECT_CLASS (cdn_node_parent_class)->foreach_expression (object,
	                                                               func,
	                                                               userdata);

	/* And then also the children! */
	for (item = node->priv->children; item; item = g_slist_next (item))
	{
		cdn_object_foreach_expression (item->data, func, userdata);
	}

	// And the self edge!
	if (node->priv->self_edge)
	{
		cdn_object_foreach_expression (CDN_OBJECT (node->priv->self_edge),
		                               func,
		                               userdata);
	}
}

static void
reconnect_children (CdnNode   *node,
                    CdnNode   *source,
                    GHashTable *mapping)
{
	GSList const *children;

	children = cdn_node_get_children (source);

	/* Reconnect all the edges */
	while (children)
	{
		CdnObject *child = children->data;

		if (CDN_IS_EDGE (child))
		{
			CdnEdge *orig_edge = CDN_EDGE (child);
			CdnEdge *copied_edge;

			CdnNode *copied_from;
			CdnNode *copied_to;

			copied_edge = g_hash_table_lookup (mapping,
			                                   child);

			copied_from = g_hash_table_lookup (mapping,
			                                   cdn_edge_get_input (orig_edge));

			copied_to = g_hash_table_lookup (mapping,
			                                 cdn_edge_get_output (orig_edge));

			cdn_edge_attach (copied_edge, copied_from, copied_to);
		}

		children = g_slist_next (children);
	}
}

static void
copy_children (CdnNode *node,
               CdnNode *source)
{
	GSList const *children = cdn_node_get_children (source);

	GHashTable *hash_table = g_hash_table_new (g_direct_hash,
	                                           g_direct_equal);

	while (children)
	{
		CdnObject *child = children->data;
		CdnObject *copied = cdn_object_copy (child);

		/* Store map from the original to the copy */
		g_hash_table_insert (hash_table, child, copied);

		cdn_node_add (node, copied, NULL);

		children = g_slist_next (children);
	}

	reconnect_children (node, source, hash_table);
	g_hash_table_destroy (hash_table);
}

static void
copy_interface (CdnNode *node,
                CdnNode *source)
{
	CdnVariableInterface *iface;
	CdnVariableInterface *source_iface;
	gchar **names;
	gchar **ptr;

	iface = cdn_node_get_variable_interface (node);
	source_iface = cdn_node_get_variable_interface (source);

	names = cdn_variable_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;
		GError *error = NULL;

		child_name = cdn_variable_interface_lookup_child_name (source_iface, *ptr);
		property_name = cdn_variable_interface_lookup_variable_name (source_iface, *ptr);

		if (!cdn_variable_interface_add (iface,
		                                 *ptr,
		                                 child_name,
		                                 property_name,
		                                 &error))
		{
			g_warning ("Could not copy interface property: %s",
			           error->message);

			g_error_free (error);
		}
	}

	g_strfreev (names);
}

static gboolean
cdn_node_cdn_unapply_template (CdnObject  *object,
                                CdnObject  *templ,
                                GError    **error)
{
	gchar **names;
	gchar **ptr;
	CdnVariableInterface *source_iface;
	CdnNode *node;
	CdnNode *source;
	GSList const *children;

	node = CDN_NODE (object);
	source = CDN_NODE (templ);

	if (!CDN_OBJECT_CLASS (cdn_node_parent_class)->unapply_template (object, templ, error))
	{
		return FALSE;
	}

	if (!CDN_IS_NODE (templ))
	{
		return TRUE;
	}

	disconnect_template (node, templ);

	if (cdn_node_has_self_edge (source))
	{
		if (!cdn_object_unapply_template (CDN_OBJECT (cdn_node_get_self_edge (node)),
		                                  CDN_OBJECT (cdn_node_get_self_edge (source)),
		                                  error))
		{
			return FALSE;
		}
	}

	/* Remove interface */
	source_iface = cdn_node_get_variable_interface (source);

	names = cdn_variable_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;

		child_name = cdn_variable_interface_lookup_child_name (source_iface, *ptr);
		property_name = cdn_variable_interface_lookup_variable_name (source_iface, *ptr);

		remove_template_interface (node, source, *ptr, child_name, property_name);
	}

	/* Remove children */
	children = cdn_node_get_children (source);

	while (children)
	{
		CdnObject *child;
		CdnObject *orig;

		child = children->data;

		orig = cdn_node_get_child (node, cdn_object_get_id (child));

		if (orig != NULL)
		{
			GSList *variables;
			gboolean layout;

			layout = should_propagate_layout (node,
			                                  orig,
			                                  child) &&
			         last_layoutable_template (orig) == CDN_LAYOUTABLE (child);

			/* TODO: make atomic */
			if (!cdn_object_unapply_template (orig, child, error))
			{
				return FALSE;
			}

			variables = cdn_object_get_variables (orig);

			/* Check if object is now empty */
			if (!cdn_object_get_applied_templates (orig) &&
			    !variables &&
			    !cdn_node_get_edges (CDN_NODE (orig)))
			{
				/* Then also remove it because it was introduced
				   by this template */
				if (!cdn_node_remove (node,
				                       orig,
				                       error))
				{
					/* TODO: make atomic */
					g_slist_free (variables);
					return FALSE;
				}
			}

			if (layout)
			{
				layout_from_template (CDN_LAYOUTABLE (orig));
			}

			g_slist_free (variables);
		}

		children = g_slist_next (children);
	}

	return TRUE;
}

static gboolean
cdn_node_cdn_apply_template (CdnObject  *object,
                              CdnObject  *templ,
                              GError    **error)
{
	CdnVariableInterface *source_iface;
	CdnNode *node;
	CdnNode *source;
	GSList const *children;
	gchar **names;
	gchar **ptr;

	if (!CDN_OBJECT_CLASS (cdn_node_parent_class)->apply_template (object, templ, error))
	{
		return FALSE;
	}

	if (!CDN_IS_NODE (templ))
	{
		return TRUE;
	}

	node = CDN_NODE (object);
	source = CDN_NODE (templ);

	children = cdn_node_get_children (source);

	/* Copy in children */
	while (children)
	{
		CdnObject *child = children->data;
		gboolean layout;

		/* Check to find existing one */
		CdnObject *new_child;

		new_child = cdn_node_get_child (node,
		                                 cdn_object_get_id (child));

		if (!new_child)
		{
			new_child = g_object_new (G_TYPE_FROM_INSTANCE (child),
			                          "id", cdn_object_get_id (child),
			                          NULL);

			cdn_node_add (node, new_child, NULL);
			g_object_unref (new_child);
		}

		layout = should_propagate_layout (node, new_child, child);

		if (!cdn_object_apply_template (new_child,
		                                child,
		                                error))
		{
			/* TODO: make atomic */
			return FALSE;
		}

		if (layout)
		{
			layout_from_template (CDN_LAYOUTABLE (new_child));
		}

		children = g_slist_next (children);
	}

	/* Apply interfaces from template */
	source_iface = cdn_node_get_variable_interface (source);

	names = cdn_variable_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;

		child_name = cdn_variable_interface_lookup_child_name (source_iface, *ptr);
		property_name = cdn_variable_interface_lookup_variable_name (source_iface, *ptr);

		if (!add_template_interface (node,
		                             source,
		                             *ptr,
		                             child_name,
		                             property_name,
		                             error))
		{
			/* TODO: make atomic */
			return FALSE;
		}
	}

	if (cdn_node_has_self_edge (source))
	{
		if (!cdn_object_apply_template (CDN_OBJECT (cdn_node_get_self_edge (node)),
		                                CDN_OBJECT (cdn_node_get_self_edge (source)),
		                                error))
		{
			return FALSE;
		}
	}

	g_strfreev (names);

	g_signal_connect (source_iface,
	                  "added",
	                  G_CALLBACK (on_template_interface_variable_added),
	                  node);

	g_signal_connect (source_iface,
	                  "removed",
	                  G_CALLBACK (on_template_interface_variable_removed),
	                  node);

	g_signal_connect (source,
	                  "child-added",
	                  G_CALLBACK (on_template_child_added),
	                  node);

	g_signal_connect (source,
	                  "child-removed",
	                  G_CALLBACK (on_template_child_removed),
	                  node);

	return TRUE;
}

static void
cdn_node_cdn_copy (CdnObject *object,
                    CdnObject *source)
{
	CdnNode *g = CDN_NODE (object);

	CDN_OBJECT_CLASS (cdn_node_parent_class)->copy (object, source);

	/* Copy over children */
	copy_children (g, CDN_NODE (source));

	/* Copy over interface */
	copy_interface (g, CDN_NODE (source));

	if (cdn_node_has_self_edge (CDN_NODE (source)))
	{
		CdnEdge *sl;

		sl = cdn_node_get_self_edge (CDN_NODE (source));
		sl = CDN_EDGE (cdn_object_copy (CDN_OBJECT (sl)));

		g->priv->self_edge = sl;
		g->priv->edges = g_slist_prepend (g->priv->edges,
		                                  sl);
	}
}

static gchar *
unique_id (CdnNode  *node,
           CdnObject *object)
{
	gchar const *id = cdn_object_get_id (object);
	gchar *newid = g_strdup (id);
	gint cnt = 0;

	while (TRUE)
	{
		CdnObject *orig = g_hash_table_lookup (node->priv->child_hash,
		                                       newid);

		if (orig == NULL || orig == object)
		{
			if (cnt == 0)
			{
				g_free (newid);
				newid = NULL;
			}

			break;
		}

		g_free (newid);
		newid = g_strdup_printf ("%s_%d", id, ++cnt);
	}

	return newid;
}

static void
register_id (CdnNode  *node,
             CdnObject *object)
{
	gchar *newid = unique_id (node, object);

	if (newid == NULL)
	{
		g_hash_table_insert (node->priv->child_hash,
		                     g_strdup (cdn_object_get_id (object)),
		                     object);
	}
	else
	{
		cdn_object_set_id (object, newid);
		g_free (newid);
	}
}

typedef struct
{
	CdnObject *find;
	const gchar *id;
} FindInfo;

static gboolean
find_object (const gchar  *id,
             CdnObject    *object,
             FindInfo     *info)
{
	if (object == info->find)
	{
		info->id = id;
		return TRUE;
	}

	return FALSE;
}

static void
update_object_id (CdnObject  *object,
                  GParamSpec *spec,
                  CdnNode   *node)
{
	FindInfo info = {object, NULL};

	g_hash_table_find (node->priv->child_hash,
	                   (GHRFunc)find_object,
	                   &info);

	/* Remove old id */
	if (info.id != NULL)
	{
		g_hash_table_remove (node->priv->child_hash,
		                     info.id);
	}

	register_id (node, object);
}

static void
unregister_object (CdnNode  *node,
                   CdnObject *object)
{
	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (cdn_object_taint),
	                                      node);

	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (update_object_id),
	                                      node);

	g_hash_table_remove (node->priv->child_hash,
	                     cdn_object_get_id (object));
}

static void
register_object (CdnNode  *node,
                 CdnObject *object)
{
	g_signal_connect_swapped (object,
	                          "tainted",
	                          G_CALLBACK (cdn_object_taint),
	                          node);

	g_signal_connect (object,
	                  "notify::id",
	                  G_CALLBACK (update_object_id),
	                  node);

	register_id (node, object);
}

static gboolean
cdn_node_add_impl (CdnNode   *node,
                    CdnObject  *object,
                    GError    **error)
{
	CdnObject *other;

	other = g_hash_table_lookup (node->priv->child_hash,
	                             cdn_object_get_id (object));

	if (other == object)
	{
		g_set_error (error,
		             CDN_NODE_ERROR,
		             CDN_NODE_ERROR_CHILD_ALREADY_EXISTS,
		             "The child `%s' already exists in the node `%s'",
		             cdn_object_get_id (object),
		             cdn_object_get_id (CDN_OBJECT (node)));

		return FALSE;
	}

	gboolean ret = FALSE;

	g_signal_emit (node,
	               node_signals[VERIFY_ADD_CHILD],
	               0,
	               object,
	               error,
	               &ret);

	if (ret)
	{
		return FALSE;
	}

	node->priv->children = g_slist_append (node->priv->children,
	                                        g_object_ref (object));

	register_object (node, object);

	_cdn_object_set_parent (object, node);

	cdn_object_taint (CDN_OBJECT (node));
	g_signal_emit (node, node_signals[CHILD_ADDED], 0, object);

	return TRUE;
}

static void
remove_object (CdnNode  *node,
               CdnObject *object)
{
	unregister_object (node, object);

	if (cdn_object_get_parent (object) == node)
	{
		_cdn_object_set_parent (object, NULL);
	}

	g_signal_emit (node, node_signals[CHILD_REMOVED], 0, object);
	g_object_unref (object);
}

gboolean
cdn_node_verify_remove_child (CdnNode   *node,
                               CdnObject  *child,
                               GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (node), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (child), FALSE);

	gboolean ret = FALSE;

	g_signal_emit (node,
	               node_signals[VERIFY_REMOVE_CHILD],
	               0,
	               child,
	               error,
	               &ret);

	return !ret;
}

static gboolean
verify_all (CdnNode   *node,
            CdnObject  *object,
            GError    **error)
{
	if (!cdn_node_verify_remove_child (node, object, error))
	{
		return FALSE;
	}

	if (CDN_IS_NODE (object))
	{
		CdnNode *other = CDN_NODE (object);
		GSList const *children = cdn_node_get_children (other);

		while (children)
		{
			if (!verify_all (other, children->data, error))
			{
				return FALSE;
			}

			children = g_slist_next (children);
		}
	}

	return TRUE;
}

static gboolean
cdn_node_remove_impl (CdnNode   *node,
                       CdnObject  *object,
                       GError    **error)
{
	GSList *item;

	item = g_slist_find (node->priv->children, object);

	if (!item)
	{
		g_set_error (error,
		             CDN_NODE_ERROR,
		             CDN_NODE_ERROR_CHILD_DOES_NOT_EXIST,
		             "The child `%s' does not exist in the node `%s'",
		             cdn_object_get_id (object),
		             cdn_object_get_id (CDN_OBJECT (node)));

		return FALSE;
	}

	if (!verify_all (node, object, error))
	{
		return FALSE;
	}

	node->priv->children = g_slist_remove_link (node->priv->children,
	                                            item);

	remove_object (node, object);
	return TRUE;
}

static void
cdn_node_cdn_clear (CdnObject *object)
{
	CdnNode *node = CDN_NODE (object);

	GSList *children = g_slist_copy (node->priv->children);
	GSList *child;

	for (child = children; child; child = g_slist_next (child))
	{
		cdn_node_remove (node, child->data, NULL);
	}

	g_slist_free (children);

	CDN_OBJECT_CLASS (cdn_node_parent_class)->clear (object);
}

static gboolean
cdn_node_cdn_equal (CdnObject *first,
                     CdnObject *second)
{
	if (!CDN_OBJECT_CLASS (cdn_node_parent_class)->equal (first, second))
	{
		return FALSE;
	}

	/* Same proxies */
	CdnNode *node1 = CDN_NODE (first);
	CdnNode *node2 = CDN_NODE (second);

	GSList const *children1 = cdn_node_get_children (node1);
	GSList const *children2 = cdn_node_get_children (node2);

	if (g_slist_length ((GSList *)children1) != g_slist_length ((GSList *)children2))
	{
		return FALSE;
	}

	while (children1)
	{
		CdnObject *child1 = children1->data;
		CdnObject *child2 = cdn_node_get_child (node2,
		                                         cdn_object_get_id (child1));

		if (!child2 || !cdn_object_equal (child1, child2))
		{
			return FALSE;
		}

		children1 = g_slist_next (children1);
	}

	return TRUE;
}

static GSList const *
cdn_node_get_children_impl (CdnNode *node)
{
	return node->priv->children;
}

static gboolean
cdn_node_verify_remove_child_impl (CdnNode   *node,
                                    CdnObject  *object,
                                    GError    **error)
{
	/* TODO: Check if there are any objects that use the child as a template
	   while this template is not inherited from applying a template to
	   the parent of the child */
	return TRUE;
}

static void
cdn_node_cdn_taint (CdnObject *object)
{
	CdnNode *node;

	node = CDN_NODE (object);

	g_slist_free (node->priv->actors);
	node->priv->actors = NULL;

	CDN_OBJECT_CLASS (cdn_node_parent_class)->taint (object);
}

static void
cdn_node_set_property (GObject      *object,
                       guint         prop_id,
                       const GValue *value,
                       GParamSpec   *pspec)
{
	CdnNode *self = CDN_NODE (object);

	switch (prop_id)
	{
		case PROP_STATE:
			g_free (self->priv->state);
			self->priv->state = g_value_dup_string (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_node_get_property (GObject    *object,
                       guint       prop_id,
                       GValue     *value,
                       GParamSpec *pspec)
{
	CdnNode *self = CDN_NODE (object);

	switch (prop_id)
	{
		case PROP_STATE:
			g_value_set_string (value, self->priv->state);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_node_class_init (CdnNodeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_node_finalize;

	object_class->get_property = cdn_node_get_property;
	object_class->set_property = cdn_node_set_property;

	object_class->dispose = cdn_node_dispose;

	cdn_class->get_variable = cdn_node_cdn_get_property;

	cdn_class->compile = cdn_node_cdn_compile;
	cdn_class->get_compile_context = cdn_node_cdn_get_compile_context;
	cdn_class->reset = cdn_node_cdn_reset;
	cdn_class->foreach_expression = cdn_node_cdn_foreach_expression;
	cdn_class->clear = cdn_node_cdn_clear;
	cdn_class->equal = cdn_node_cdn_equal;
	cdn_class->taint = cdn_node_cdn_taint;

	cdn_class->copy = cdn_node_cdn_copy;
	cdn_class->apply_template = cdn_node_cdn_apply_template;
	cdn_class->unapply_template = cdn_node_cdn_unapply_template;

	klass->add = cdn_node_add_impl;
	klass->remove = cdn_node_remove_impl;
	klass->get_children = cdn_node_get_children_impl;
	klass->verify_remove_child = cdn_node_verify_remove_child_impl;

	g_type_class_add_private (object_class, sizeof(CdnNodePrivate));

	/**
	 * CdnNode::child-added:
	 * @object: a #CdnObject
	 * @action: the added #CdnObject
	 *
	 * Emitted when a child object is added to the node
	 *
	 **/
	node_signals[CHILD_ADDED] =
		g_signal_new ("child-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnNodeClass,
		                               child_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_OBJECT);

	/**
	 * CdnNode::child-removed:
	 * @object: a #CdnObject
	 * @action: the removed #CdnObject
	 *
	 * Emitted when a child object is removed from the node
	 *
	 **/
	node_signals[CHILD_REMOVED] =
		g_signal_new ("child-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnNodeClass,
		                               child_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_OBJECT);

	/**
	 * CdnNode::verify-remove-child:
	 * @object: a #CdnObject
	 * @child: the child to be removed
	 * @error: the error
	 *
	 * Returns: %TRUE if the child can be removed, %FALSE otherwise
	 *
	 **/
	node_signals[VERIFY_REMOVE_CHILD] =
		g_signal_new ("verify-remove-child",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnNodeClass,
		                               verify_remove_child),
		              cdn_signal_accumulator_false_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__OBJECT_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              CDN_TYPE_OBJECT,
		              G_TYPE_POINTER);


	/**
	 * CdnNode::verify-add-child:
	 * @object: a #CdnObject
	 * @child: the child to be removed
	 * @error: the error
	 *
	 * Returns: %TRUE if the child can be added, %FALSE otherwise
	 *
	 **/
	node_signals[VERIFY_ADD_CHILD] =
		g_signal_new ("verify-add-child",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              0,
		              cdn_signal_accumulator_false_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__OBJECT_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              CDN_TYPE_OBJECT,
		              G_TYPE_POINTER);

	g_object_class_install_property (object_class,
	                                 PROP_STATE,
	                                 g_param_spec_string ("state",
	                                                      "State",
	                                                      "State",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS));
}

static void
on_variable_interface_changed (CdnObject *object)
{
	cdn_object_taint (object);
}

static gboolean
on_variable_interface_verify_remove (CdnVariableInterface  *iface,
                                     gchar const           *name,
                                     gchar const           *child_name,
                                     gchar const           *property_name,
                                     GError               **error,
                                     CdnNode              *node)
{
	return TRUE;
}

static void
cdn_node_init (CdnNode *self)
{
	self->priv = CDN_NODE_GET_PRIVATE (self);

	self->priv->child_hash = g_hash_table_new_full (g_str_hash,
	                                                g_str_equal,
	                                                (GDestroyNotify)g_free,
	                                                NULL);

	self->priv->variable_interface = cdn_variable_interface_new (self);

	g_signal_connect_swapped (self->priv->variable_interface,
	                          "added",
	                          G_CALLBACK (on_variable_interface_changed),
	                          self);

	g_signal_connect_swapped (self->priv->variable_interface,
	                          "removed",
	                          G_CALLBACK (on_variable_interface_changed),
	                          self);

	g_signal_connect (self->priv->variable_interface,
	                  "verify-remove",
	                  G_CALLBACK (on_variable_interface_verify_remove),
	                  self);
}

/**
 * cdn_node_new:
 * @id: the node id
 *
 * Create a new node.
 *
 * Returns: A #CdnNode
 *
 **/
CdnNode *
cdn_node_new (gchar const *id)
{
	return g_object_new (CDN_TYPE_NODE,
	                     "id", id,
	                     NULL);
}

/**
 * cdn_node_get_children:
 * @node: A #CdnNode
 *
 * Get a list of the children in @node.
 *
 * Returns: (element-type CdnObject) (transfer none): A #GSList of #CdnObject
 *
 **/
const GSList *
cdn_node_get_children (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	return CDN_NODE_GET_CLASS (node)->get_children (node);
}

/**
 * cdn_node_add:
 * @node: A #CdnNode
 * @object: A #CdnObject
 *
 * Add a child object to the node.
 *
 * Returns: %TRUE if the child could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cdn_node_add (CdnNode   *node,
               CdnObject  *object,
               GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (node), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	if (CDN_NODE_GET_CLASS (node)->add)
	{
		return CDN_NODE_GET_CLASS (node)->add (node, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_node_remove:
 * @node: A #CdnNode
 * @object: A #CdnObject
 * @error: A #GError
 *
 * Remove a child object from the node.
 *
 * Returns: %TRUE if the child was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cdn_node_remove (CdnNode   *node,
                  CdnObject  *object,
                  GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (node), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	if (CDN_NODE_GET_CLASS (node)->remove)
	{
		return CDN_NODE_GET_CLASS (node)->remove (node, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_node_foreach:
 * @node: A #CdnNode
 * @func: (scope call): A #GFunc
 * @data: User data
 *
 * Call @func for each child object in the node.
 *
 **/
void
cdn_node_foreach (CdnNode *node,
                   GFunc     func,
                   gpointer  data)
{
	g_return_if_fail (CDN_IS_NODE (node));

	g_slist_foreach ((GSList *)cdn_node_get_children (node), func, data);
}

/**
 * cdn_node_get_child:
 * @node: A #CdnNode
 * @name: The child name
 *
 * Get a child from the node by name.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_node_get_child (CdnNode    *node,
                     const gchar *name)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	return g_hash_table_lookup (node->priv->child_hash,
	                            name);
}

/**
 * cdn_node_find_objects:
 * @node: A #CdnNode
 * @selector: The selector
 *
 * Find objects by specifying a selector. For example, if there is
 * another node "g" containing a state "s", you can use
 * #cdn_node_find_object (node, "g.s") to get the object.
 *
 * Returns: (transfer container) (element-type CdnObject): A #CdnObject
 *
 **/
GSList *
cdn_node_find_objects (CdnNode     *node,
                       const gchar *selector)
{
	CdnSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cdn_selector_parse (CDN_OBJECT (node), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);

		return NULL;
	}

	all = cdn_selector_select (sel,
	                           G_OBJECT (node),
	                           CDN_SELECTOR_TYPE_OBJECT,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_get_object (all->data));

		cdn_selection_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cdn_node_find_object:
 * @node: A #CdnNode
 * @selector: The selector
 *
 * Find object by specifying a selector. For example, if there is
 * another node "g" containing a node "s", you can use
 * #cdn_node_find_object (node, "g.s") to get the object.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_node_find_object (CdnNode    *node,
                       const gchar *selector)
{
	GSList *all;
	CdnObject *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cdn_node_find_objects (node, selector);

	if (all)
	{
		ret =  all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cdn_node_find_variables:
 * @node: A #CdnNode
 * @selector: The selector
 *
 * Find variables by specifying a selector. For example, if there is
 * another node "g" containing a node "s" with variable "x", you can use
 * #cdn_node_find_variables (node, "g.s.x") to remove the variable.
 *
 * Returns: (transfer container) (element-type CdnVariable): A list of #CdnVariable
 *
 **/
GSList *
cdn_node_find_variables (CdnNode    *node,
                         gchar const *selector)
{
	CdnSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cdn_selector_parse (CDN_OBJECT (node), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);
		return NULL;
	}

	all = cdn_selector_select (sel,
	                           G_OBJECT (node),
	                           CDN_SELECTOR_TYPE_VARIABLE,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_get_object (all->data));

		cdn_selection_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cdn_node_find_variable:
 * @node: A #CdnNode
 * @selector: The selector
 *
 * Find a variable by specifying a selector. For example, if there is
 * another node "g" containing a node "s" with a variable "x", you can use
 * #cdn_node_find_variable (node, "g.s.x") to get the variable.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_node_find_variable (CdnNode      *node,
                         gchar const *selector)
{
	GSList *all;
	CdnVariable *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cdn_node_find_variables (node, selector);

	if (all)
	{
		ret = all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cdn_node_get_variable_interface:
 * @node: A #CdnNode
 *
 * Get the property interface of the node.
 *
 * Returns: (transfer none): A #CdnVariableInterface
 *
 **/
CdnVariableInterface *
cdn_node_get_variable_interface (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	return node->priv->variable_interface;
}

/**
 * cdn_node_get_auto_templates_for_child:
 * @node: A #CdnNode
 * @child: A #CdnObject
 *
 * Get the templates that were automatically applied from the node to the
 * child.
 *
 * Returns: (transfer container) (element-type CdnObject) (allow-none): A #GSList of #CdnObject
 *
 **/
GSList *
cdn_node_get_auto_templates_for_child (CdnNode  *node,
                                        CdnObject *child)
{
	GSList const *templates;
	GSList *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (node), NULL);
	g_return_val_if_fail (CDN_IS_OBJECT (child), NULL);

	templates = cdn_object_get_applied_templates (CDN_OBJECT (node));

	while (templates)
	{
		if (CDN_IS_NODE (templates->data))
		{
			CdnNode *grp = templates->data;
			CdnObject *tchild;

			tchild = cdn_node_get_child (grp,
			                              cdn_object_get_id (child));

			if (tchild)
			{
				ret = g_slist_prepend (ret, tchild);
			}
		}

		templates = g_slist_next (templates);
	}

	return g_slist_reverse (ret);
}

/**
 * _cdn_object_link:
 * @object: the #CdnObject
 * @edge: the #CdnEdge which edges to this object
 *
 * Adds @edge as a edge which targets the object.
 *
 **/
void
_cdn_node_link (CdnNode *node,
                CdnEdge  *edge)
{
	g_return_if_fail (CDN_IS_NODE (node));
	g_return_if_fail (CDN_IS_EDGE (edge));

	node->priv->edges = g_slist_append (node->priv->edges, edge);

	g_slist_free (node->priv->actors);
	node->priv->actors = NULL;

	g_object_add_toggle_ref (G_OBJECT (edge),
	                         (GToggleNotify)edge_destroyed,
	                         node);
}

/**
 * _cdn_node_unlink:
 * @node: the #CdnObject
 * @edge: the #CdnEdge which unedges from this node
 *
 * Removes @edge as a edge which targets the node.
 *
 **/
void
_cdn_node_unlink (CdnNode *node,
                  CdnEdge  *edge)
{
	g_return_if_fail (CDN_IS_NODE (node));
	g_return_if_fail (CDN_IS_EDGE (edge));

	GSList *item = g_slist_find (node->priv->edges, edge);

	if (!item)
	{
		return;
	}

	node->priv->edges = g_slist_remove_link (node->priv->edges,
	                                           item);

	g_slist_free (node->priv->actors);
	node->priv->actors = NULL;

	g_object_remove_toggle_ref (G_OBJECT (edge),
	                            (GToggleNotify)edge_destroyed,
	                            node);
}

/**
 * cdn_node_get_actors:
 * @node: A #CdnNode
 *
 * Get the variables which are acted upon by edges.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable.
 *
 **/
GSList const *
cdn_node_get_actors (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	if (node->priv->actors != NULL)
	{
		return node->priv->actors;
	}

	GSList *ret = NULL;
	GSList *item;

	for (item = node->priv->edges; item; item = g_slist_next (item))
	{
		GSList const *actions;

		actions = cdn_edge_get_actions (CDN_EDGE (item->data));

		while (actions)
		{
			CdnEdgeAction *a = actions->data;
			CdnVariable *target = cdn_edge_action_get_target_variable (a);

			if (!g_slist_find (ret, target))
			{
				ret = g_slist_prepend (ret, target);
			}

			actions = g_slist_next (actions);
		}
	}

	node->priv->actors = g_slist_reverse (ret);
	return node->priv->actors;
}

/**
 * cdn_node_get_edges:
 * @node: A #CdnNode
 *
 * Get a list of edges that act on this object.
 *
 * Returns: (element-type CdnEdge): A list of #CdnEdge
 *
 */
GSList const *
cdn_node_get_edges (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	return node->priv->edges;
}

gboolean
cdn_node_has_self_edge (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), FALSE);

	return node->priv->self_edge != NULL;
}


/**
 * cdn_node_get_self_edge:
 * @node: A #CdnNode
 *
 * Get the self edge of this node. Note that the self edge will be automatically
 * created if it does not exist yet. When this is undesired, use
 * @cdn_node_has_self_edge first.
 *
 * Returns: (transfer none): A #CdnEdge
 *
 **/
CdnEdge *
cdn_node_get_self_edge (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	if (!node->priv->self_edge)
	{
		node->priv->self_edge = cdn_edge_new ("integrate",
		                                       node,
		                                       node);

		_cdn_object_set_parent (CDN_OBJECT (node->priv->self_edge),
		                        node);
	}

	return node->priv->self_edge;
}


gchar const *
cdn_node_get_state (CdnNode *node)
{
	g_return_val_if_fail (CDN_IS_NODE (node), NULL);

	return node->priv->state;
}

void
cdn_node_set_state (CdnNode     *node,
                    gchar const *state)
{
	g_return_if_fail (CDN_IS_NODE (node));

	g_free (node->priv->state);
	node->priv->state = g_strdup (state);

	g_object_notify (G_OBJECT (node), "state");
}
