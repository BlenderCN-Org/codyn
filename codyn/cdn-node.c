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

/**
 * SECTION:cdn-node
 * @short_description: Group object grouping many objects together
 *
 * The #CdnNode is a special #CdnObject that contains many objects as children.
 * This can be used make sub-networks that can be easily instantiated,
 * providing some common functionality.
 *
 * Each group can have a proxy object associated which serves as a bridge
 * between the outside and the inside of the group. In addition, each group
 * has an interface (#CdnVariableInterface) which allows the designer of the
 * group to expose a certain subset of its child variables on the group.
 *
 * <refsect2 id="CdnNode-COPY">
 * <title>CdnNode Copy Semantics</title>
 * When a group is copied with #cdn_object_copy, all the children are
 * recursively copied as well. If a group has a proxy, the new group will
 * have its proxy set to the new copy of the original proxy.
 * </refsect2>
 */

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
	CdnObject *proxy;

	GSList *children;
	GHashTable *child_hash;

	CdnVariableInterface *variable_interface;

	/* Links */
	GSList *links;
	GSList *actors;
	CdnEdge *self_link;

	guint proxy_signals[NUM_EXT_SIGNALS];
};

G_DEFINE_TYPE (CdnNode, cdn_node, CDN_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_PROXY
};

enum
{
	CHILD_ADDED,
	CHILD_REMOVED,
	VERIFY_ADD_CHILD,
	VERIFY_REMOVE_CHILD,
	NUM_SIGNALS
};

static guint group_signals[NUM_SIGNALS] = {0,};

static void remove_object (CdnNode *group, CdnObject *object);

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
link_destroyed (CdnNode  *group,
                CdnEdge   *link,
                gboolean   is_last_ref)
{
	if (!is_last_ref)
	{
		return;
	}

	/* Remove link, and toggle ref */
	group->priv->links = g_slist_remove (group->priv->links, link);

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            group);
}


static void
cdn_node_finalize (GObject *object)
{
	CdnNode *group = CDN_NODE (object);

	g_hash_table_destroy (group->priv->child_hash);

	g_slist_free (group->priv->links);
	g_slist_free (group->priv->actors);

	G_OBJECT_CLASS (cdn_node_parent_class)->finalize (object);
}

static CdnObject *
get_child_from_template (CdnNode  *group,
                         CdnObject *templ)
{
	GSList const *children;

	children = group->priv->children;

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
should_propagate_layout (CdnNode  *group,
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
                         CdnNode  *group)
{
	CdnObject *obj;
	gboolean layout;

	obj = cdn_node_get_child (group, cdn_object_get_id (child));
	layout = should_propagate_layout (group, obj, child);

	if (obj != NULL && G_TYPE_FROM_INSTANCE (child) == G_TYPE_FROM_INSTANCE (obj))
	{
		cdn_object_apply_template (obj, child, NULL);
	}
	else
	{
		obj = cdn_object_new_from_template (child, NULL);
		cdn_node_add (group, obj, NULL);
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
                           CdnNode  *group)
{
	CdnObject *obj;

	obj = get_child_from_template (group, child);

	if (obj)
	{
		GSList *variables;
		gboolean relayout;

		relayout = should_propagate_layout (group, obj, child) &&
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
interface_is_from_proxy (CdnNode    *group,
                         gchar const *name)
{
	gchar const *child_name;

	if (!group->priv->proxy)
	{
		return FALSE;
	}

	child_name = cdn_variable_interface_lookup_child_name (group->priv->variable_interface,
	                                                       name);

	return g_strcmp0 (cdn_object_get_id (group->priv->proxy), child_name) == 0;
}

static gboolean
interface_should_override (CdnNode     *group,
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
	 * 1) Interface was not generated by a proxy (proxies have priority)
	 * 2) No template applied after 'template' adds this interface name
	 *
	 */

	if (interface_is_from_proxy (group, name))
	{
		return FALSE;
	}

	templates = cdn_object_get_applied_templates (CDN_OBJECT (group));

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
add_template_interface (CdnNode     *group,
                        CdnNode     *source,
                        gchar const  *name,
                        gchar const  *child_name,
                        gchar const  *property_name,
                        GError      **error)
{
	CdnVariableInterface *iface;

	iface = cdn_node_get_variable_interface (group);

	if (cdn_variable_interface_implements (iface, name))
	{
		if (interface_should_override (group, source, name, NULL))
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
remove_template_interface (CdnNode    *group,
                           CdnNode    *source,
                           gchar const *name,
                           gchar const *child_name,
                           gchar const *property_name)
{
	CdnVariableInterface *iface;
	GError *error = NULL;
	CdnNode *last = NULL;

	/* Check if this interface actually owns that property */
	if (!interface_should_override (group, source, name, &last))
	{
		return;
	}

	/* Remove the mapping */
	iface = cdn_node_get_variable_interface (group);

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

		add_template_interface (group,
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
                                      CdnNode             *group)
{
	add_template_interface (group,
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
                                        CdnNode             *group)
{
	remove_template_interface (group,
	                           cdn_variable_interface_get_node (templ_iface),
	                           name,
	                           child_name,
	                           property_name);
}

static void
disconnect_template (CdnNode  *group,
                     CdnObject *templ)
{
	CdnVariableInterface *piface;

	piface = cdn_node_get_variable_interface (CDN_NODE (templ));

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_variable_added,
	                                      group);

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_variable_removed,
	                                      group);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_added,
	                                      group);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_removed,
	                                      group);
}

static void
cdn_node_dispose (GObject *object)
{
	CdnNode *group = CDN_NODE (object);
	GSList *copy;
	GSList *item;
	GSList const *templates;

	if (group->priv->children)
	{
		GSList *children = group->priv->children;
		GSList *item;

		group->priv->children = NULL;

		for (item = children; item; item = g_slist_next (item))
		{
			remove_object (group, item->data);
		}

		g_slist_free (children);
	}

	if (group->priv->proxy)
	{
		CdnObject *proxy = group->priv->proxy;
		group->priv->proxy = NULL;

		g_object_unref (proxy);
	}

	templates = cdn_object_get_applied_templates (CDN_OBJECT (object));

	while (templates)
	{
		disconnect_template (group, templates->data);
		templates = g_slist_next (templates);
	}

	if (group->priv->variable_interface)
	{
		g_object_unref (group->priv->variable_interface);
		group->priv->variable_interface = NULL;
	}

	/* Untoggle ref all links, because we need them destroyed! */
	copy = g_slist_copy (group->priv->links);

	for (item = copy; item; item = g_slist_next (item))
	{
		if (item->data != group->priv->self_link)
		{
			link_destroyed (group, item->data, TRUE);
		}
	}

	g_slist_free (copy);
	g_slist_free (group->priv->links);
	group->priv->links = NULL;

	if (group->priv->self_link)
	{
		g_object_unref (group->priv->self_link);
		group->priv->self_link = NULL;
	}

	G_OBJECT_CLASS (cdn_node_parent_class)->dispose (object);
}

static void
proxy_add_variable (CdnNode    *group,
                    gchar const *name,
                    gchar const *child_name)
{
	if (cdn_variable_interface_implements (group->priv->variable_interface,
	                                       name))
	{
		cdn_variable_interface_remove (group->priv->variable_interface,
		                               name,
		                               NULL);
	}

	cdn_variable_interface_add (group->priv->variable_interface,
	                            name,
	                            child_name,
	                            name,
	                            NULL);
}

static void
add_interface_after_proxy_remove (CdnNode    *group,
                                  gchar const *name)
{
	GSList const *templates;
	CdnNode *templ = NULL;

	templates = cdn_object_get_applied_templates (CDN_OBJECT (group));

	while (templates)
	{
		CdnObject *temp = templates->data;

		if (CDN_IS_NODE (temp))
		{
			CdnVariableInterface *iface;

			iface = cdn_node_get_variable_interface (CDN_NODE (temp));

			if (cdn_variable_interface_implements (iface, name))
			{
				templ = CDN_NODE (temp);
			}
		}

		templates = g_slist_next (templates);
	}

	if (templ != NULL)
	{
		CdnVariableInterface *iface;

		iface = cdn_node_get_variable_interface (templ);

		add_template_interface (group,
		                        templ,
		                        name,
		                        cdn_variable_interface_lookup_child_name (iface, name),
		                        cdn_variable_interface_lookup_variable_name (iface, name),
		                        NULL);
	}
}

static void
proxy_remove_variable (CdnNode    *group,
                       gchar const *name)
{
	if (cdn_variable_interface_implements (group->priv->variable_interface,
	                                       name))
	{
		cdn_variable_interface_remove (group->priv->variable_interface,
		                               name,
		                               NULL);

		/* Then maybe generate property for interfaces */
		add_interface_after_proxy_remove (group, name);
	}
}

static void
on_proxy_variable_added (CdnObject   *proxy,
                         CdnVariable *property,
                         CdnNode    *group)
{
	proxy_add_variable (group,
	                    cdn_variable_get_name (property),
	                    cdn_object_get_id (proxy));
}

static void
on_proxy_variable_removed (CdnObject   *proxy,
                           CdnVariable *property,
                           CdnNode    *group)
{
	proxy_remove_variable (group,
	                       cdn_variable_get_name (property));
}

static void
on_proxy_interface_variable_added (CdnVariableInterface *iface,
                                   gchar const          *name,
                                   gchar const          *child_name,
                                   gchar const          *property_name,
                                   CdnNode             *group)
{
	proxy_add_variable (group, name, child_name);
}

static void
on_proxy_interface_variable_removed (CdnVariableInterface *iface,
                                     gchar const          *name,
                                     gchar const          *child_name,
                                     gchar const          *property_name,
                                     CdnNode             *group)
{
	proxy_remove_variable (group, name);
}

static gboolean
set_proxy (CdnNode  *group,
           CdnObject *proxy)
{
	if (group->priv->proxy == proxy)
	{
		return TRUE;
	}

	if (group->priv->proxy)
	{
		g_signal_handler_disconnect (group->priv->proxy,
		                             group->priv->proxy_signals[EXT_PROPERTY_ADDED]);

		g_signal_handler_disconnect (group->priv->proxy,
		                             group->priv->proxy_signals[EXT_PROPERTY_REMOVED]);

		if (CDN_IS_NODE (group->priv->proxy))
		{
			CdnVariableInterface *iface;

			iface = cdn_node_get_variable_interface (CDN_NODE (group->priv->proxy));

			g_signal_handler_disconnect (iface,
			                             group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_ADDED]);

			g_signal_handler_disconnect (iface,
			                             group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_REMOVED]);
		}

		GSList *variables = cdn_object_get_variables (group->priv->proxy);
		GSList *item;

		CdnObject *pr = group->priv->proxy;
		group->priv->proxy = NULL;

		/* Remove automatically mapped variables from interface */
		for (item = variables; item; item = g_slist_next (item))
		{
			proxy_remove_variable (group,
			                       cdn_variable_get_name (item->data));
		}

		g_slist_free (variables);

		if (CDN_IS_NODE (pr))
		{
			CdnVariableInterface *iface;
			gchar **names;
			gchar **ptr;

			iface = cdn_node_get_variable_interface (CDN_NODE (pr));

			names = cdn_variable_interface_get_names (iface);

			for (ptr = names; ptr && *ptr; ++ptr)
			{
				proxy_remove_variable (group, *ptr);
			}

			g_strfreev (names);
		}

		g_object_unref (pr);
	}

	if (proxy)
	{
		group->priv->proxy = g_object_ref (proxy);
		cdn_node_add (group, proxy, NULL);

		GSList *variables = cdn_object_get_variables (group->priv->proxy);
		GSList *item;

		for (item = variables; item; item = g_slist_next (item))
		{
			proxy_add_variable (group,
			                    cdn_variable_get_name (item->data),
			                    cdn_object_get_id (proxy));
		}

		g_slist_free (variables);

		group->priv->proxy_signals[EXT_PROPERTY_ADDED] =
			g_signal_connect_after (group->priv->proxy,
			                        "variable-added",
			                        G_CALLBACK (on_proxy_variable_added),
			                        group);

		group->priv->proxy_signals[EXT_PROPERTY_REMOVED] =
			g_signal_connect (group->priv->proxy,
			                  "variable-removed",
			                  G_CALLBACK (on_proxy_variable_removed),
			                  group);

		if (CDN_IS_NODE (group->priv->proxy))
		{
			CdnVariableInterface *iface;
			gchar **names;
			gchar **ptr;

			iface = cdn_node_get_variable_interface (CDN_NODE (group->priv->proxy));
			names = cdn_variable_interface_get_names (iface);

			for (ptr = names; ptr && *ptr; ++ptr)
			{
				proxy_add_variable (group,
				                    *ptr,
				                    cdn_object_get_id (proxy));
			}

			g_strfreev (names);

			group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_ADDED] =
				g_signal_connect_after (iface,
				                        "added",
				                        G_CALLBACK (on_proxy_interface_variable_added),
				                        group);

			group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_REMOVED] =
				g_signal_connect (iface,
				                  "removed",
				                  G_CALLBACK (on_proxy_interface_variable_removed),
				                  group);
		}
	}

	g_object_notify (G_OBJECT (group), "proxy");

	cdn_object_taint (CDN_OBJECT (group));
	return TRUE;
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
		case PROP_PROXY:
			set_proxy (self, g_value_get_object (value));
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
		case PROP_PROXY:
			g_value_set_object (value, self->priv->proxy);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static CdnVariable *
cdn_node_cdn_get_property (CdnObject   *object,
                            gchar const *name)
{
	CdnVariable *prop;
	CdnNode *group = CDN_NODE (object);

	prop = CDN_OBJECT_CLASS (cdn_node_parent_class)->get_variable (object,
	                                                                name);

	if (!prop)
	{
		prop = cdn_variable_interface_lookup (group->priv->variable_interface,
		                                      name);
	}

	return prop;
}

static void
prepend_functions (CdnNode          *group,
                   CdnCompileContext *context)
{
	GSList *item;

	if (!group)
	{
		return;
	}

	item = group->priv->children;

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
	CdnNode *group;

	group = CDN_NODE (object);

	ret = CDN_OBJECT_CLASS (cdn_node_parent_class)->get_compile_context (object, context);

	prepend_functions (group, ret);
	return ret;
}

static gboolean
cdn_node_cdn_compile (CdnObject         *object,
                       CdnCompileContext *context,
                       CdnCompileError   *error)
{
	CdnNode *group = CDN_NODE (object);
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

	item = group->priv->children;

	while (item)
	{
		if (!CDN_IS_FUNCTION (item->data))
		{
			others = g_slist_prepend (others,
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

	if (group->priv->self_link)
	{
		if (!cdn_object_compile (CDN_OBJECT (group->priv->self_link), context, error))
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
	CdnNode *group;

	CDN_OBJECT_CLASS (cdn_node_parent_class)->reset (object);

	group = CDN_NODE (object);

	/* And then also the children! */
	cdn_node_foreach (group,
	                   (GFunc)cdn_object_reset,
	                   NULL);
}

static void
cdn_node_cdn_foreach_expression (CdnObject                *object,
                                  CdnForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	CdnNode *group;
	GSList *item;

	group = CDN_NODE (object);

	CDN_OBJECT_CLASS (cdn_node_parent_class)->foreach_expression (object,
	                                                               func,
	                                                               userdata);

	/* And then also the children! */
	for (item = group->priv->children; item; item = g_slist_next (item))
	{
		cdn_object_foreach_expression (item->data, func, userdata);
	}
}

static void
reconnect_children (CdnNode   *group,
                    CdnNode   *source,
                    GHashTable *mapping)
{
	GSList const *children;

	children = cdn_node_get_children (source);

	/* Reconnect all the links */
	while (children)
	{
		CdnObject *child = children->data;

		if (CDN_IS_EDGE (child))
		{
			CdnEdge *orig_link = CDN_EDGE (child);
			CdnEdge *copied_link;

			CdnNode *copied_from;
			CdnNode *copied_to;

			copied_link = g_hash_table_lookup (mapping,
			                                   child);

			copied_from = g_hash_table_lookup (mapping,
			                                   cdn_edge_get_from (orig_link));

			copied_to = g_hash_table_lookup (mapping,
			                                 cdn_edge_get_to (orig_link));

			cdn_edge_attach (copied_link, copied_from, copied_to);
		}

		children = g_slist_next (children);
	}
}

static void
copy_children (CdnNode *group,
               CdnNode *source)
{
	CdnObject *proxy = cdn_node_get_proxy (source);
	GSList const *children = cdn_node_get_children (source);

	GHashTable *hash_table = g_hash_table_new (g_direct_hash,
	                                           g_direct_equal);

	while (children)
	{
		CdnObject *child = children->data;
		CdnObject *copied = cdn_object_copy (child);

		/* Store map from the original to the copy */
		g_hash_table_insert (hash_table, child, copied);

		cdn_node_add (group, copied, NULL);

		if (child == proxy)
		{
			group->priv->proxy = g_object_ref (copied);
		}

		children = g_slist_next (children);
	}

	reconnect_children (group, source, hash_table);
	g_hash_table_destroy (hash_table);
}

static void
copy_interface (CdnNode *group,
                CdnNode *source)
{
	CdnVariableInterface *iface;
	CdnVariableInterface *source_iface;
	gchar **names;
	gchar **ptr;

	iface = cdn_node_get_variable_interface (group);
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

static CdnObject *
get_template_proxy (CdnNode *group)
{
	if (group->priv->proxy == NULL)
	{
		return NULL;
	}

	GSList *templates = g_slist_copy ((GSList *)cdn_object_get_applied_templates (CDN_OBJECT (group)));
	templates = g_slist_reverse (templates);

	GSList *item;
	GSList const *proxy_templates = cdn_object_get_applied_templates (group->priv->proxy);

	for (item = templates; item; item = g_slist_next (item))
	{
		if (!CDN_IS_NODE (item->data))
		{
			continue;
		}

		CdnObject *proxy = cdn_node_get_proxy (item->data);

		if (proxy && g_slist_find ((GSList *)proxy_templates, proxy))
		{
			CdnObject *ret;

			ret = item->data;
			g_slist_free (templates);
			return ret;
		}
	}

	g_slist_free (templates);
	return NULL;
}

static gboolean
cdn_node_cdn_unapply_template (CdnObject  *object,
                                CdnObject  *templ,
                                GError    **error)
{
	gchar **names;
	gchar **ptr;
	CdnVariableInterface *source_iface;
	CdnNode *group;
	CdnNode *source;
	gboolean hadproxy;
	GSList const *children;

	group = CDN_NODE (object);
	source = CDN_NODE (templ);

	hadproxy = (get_template_proxy (group) == templ);

	if (!CDN_OBJECT_CLASS (cdn_node_parent_class)->unapply_template (object, templ, error))
	{
		return FALSE;
	}

	if (!CDN_IS_NODE (templ))
	{
		return TRUE;
	}

	disconnect_template (group, templ);

	if (cdn_node_has_self_edge (source))
	{
		if (!cdn_object_apply_template (CDN_OBJECT (cdn_node_get_self_edge (group)),
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

		remove_template_interface (group, source, *ptr, child_name, property_name);
	}

	if (hadproxy)
	{
		/* Set to next proxy */
		GSList const *templates;
		CdnObject *proxy = NULL;

		templates = cdn_object_get_applied_templates (object);

		while (templates)
		{
			CdnObject *pr;

			if (CDN_IS_NODE (templates->data))
			{
				pr = cdn_node_get_proxy (templates->data);

				if (pr != NULL)
				{
					proxy = pr;
				}
			}

			templates = g_slist_next (templates);
		}

		set_proxy (group, proxy);
	}

	/* Remove children */
	children = cdn_node_get_children (source);

	while (children)
	{
		CdnObject *child;
		CdnObject *orig;

		child = children->data;

		orig = cdn_node_get_child (group, cdn_object_get_id (child));

		if (orig != NULL)
		{
			GSList *variables;
			gboolean layout;

			layout = should_propagate_layout (group,
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
				if (!cdn_node_remove (group,
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
	CdnNode *group;
	CdnNode *source;
	CdnObject *proxy;
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

	group = CDN_NODE (object);
	source = CDN_NODE (templ);

	proxy = cdn_node_get_proxy (source);
	children = cdn_node_get_children (source);

	/* Copy in children */
	while (children)
	{
		CdnObject *child = children->data;
		gboolean layout;

		/* Check to find existing one */
		CdnObject *new_child;

		new_child = cdn_node_get_child (group,
		                                 cdn_object_get_id (child));

		if (!new_child)
		{
			new_child = g_object_new (G_TYPE_FROM_INSTANCE (child),
			                          "id", cdn_object_get_id (child),
			                          NULL);

			cdn_node_add (group, new_child, NULL);
			g_object_unref (new_child);
		}

		layout = should_propagate_layout (group, new_child, child);

		if (!cdn_object_apply_template (new_child,
		                                child,
		                                error))
		{
			/* TODO: make atomic */
			return FALSE;
		}

		if (child == proxy)
		{
			if (group->priv->proxy == NULL ||
			    get_template_proxy (group))
			{
				set_proxy (group, new_child);
			}
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

		if (!add_template_interface (group,
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
		if (!cdn_object_apply_template (CDN_OBJECT (cdn_node_get_self_edge (group)),
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
	                  group);

	g_signal_connect (source_iface,
	                  "removed",
	                  G_CALLBACK (on_template_interface_variable_removed),
	                  group);

	g_signal_connect (source,
	                  "child-added",
	                  G_CALLBACK (on_template_child_added),
	                  group);

	g_signal_connect (source,
	                  "child-removed",
	                  G_CALLBACK (on_template_child_removed),
	                  group);

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

		g->priv->self_link = sl;
		g->priv->links = g_slist_prepend (g->priv->links,
		                                  sl);
	}
}

static gchar *
unique_id (CdnNode  *group,
           CdnObject *object)
{
	gchar const *id = cdn_object_get_id (object);
	gchar *newid = g_strdup (id);
	gint cnt = 0;

	while (TRUE)
	{
		CdnObject *orig = g_hash_table_lookup (group->priv->child_hash,
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
register_id (CdnNode  *group,
             CdnObject *object)
{
	gchar *newid = unique_id (group, object);

	if (newid == NULL)
	{
		g_hash_table_insert (group->priv->child_hash,
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
                  CdnNode   *group)
{
	FindInfo info = {object, NULL};

	g_hash_table_find (group->priv->child_hash,
	                   (GHRFunc)find_object,
	                   &info);

	/* Remove old id */
	if (info.id != NULL)
	{
		g_hash_table_remove (group->priv->child_hash,
		                     info.id);
	}

	register_id (group, object);
}

static void
unregister_object (CdnNode  *group,
                   CdnObject *object)
{
	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (cdn_object_taint),
	                                      group);

	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (update_object_id),
	                                      group);

	g_hash_table_remove (group->priv->child_hash,
	                     cdn_object_get_id (object));
}

static void
register_object (CdnNode  *group,
                 CdnObject *object)
{
	g_signal_connect_swapped (object,
	                          "tainted",
	                          G_CALLBACK (cdn_object_taint),
	                          group);

	g_signal_connect (object,
	                  "notify::id",
	                  G_CALLBACK (update_object_id),
	                  group);

	register_id (group, object);
}

static gboolean
cdn_node_add_impl (CdnNode   *group,
                    CdnObject  *object,
                    GError    **error)
{
	CdnObject *other;

	other = g_hash_table_lookup (group->priv->child_hash,
	                             cdn_object_get_id (object));

	if (other == object)
	{
		g_set_error (error,
		             CDN_NODE_ERROR,
		             CDN_NODE_ERROR_CHILD_ALREADY_EXISTS,
		             "The child `%s' already exists in the group `%s'",
		             cdn_object_get_id (object),
		             cdn_object_get_id (CDN_OBJECT (group)));

		return FALSE;
	}

	gboolean ret = FALSE;

	g_signal_emit (group,
	               group_signals[VERIFY_ADD_CHILD],
	               0,
	               object,
	               error,
	               &ret);

	if (ret)
	{
		return FALSE;
	}

	group->priv->children = g_slist_append (group->priv->children,
	                                        g_object_ref (object));

	register_object (group, object);

	_cdn_object_set_parent (object, group);

	cdn_object_taint (CDN_OBJECT (group));
	g_signal_emit (group, group_signals[CHILD_ADDED], 0, object);

	return TRUE;
}

static void
remove_object (CdnNode  *group,
               CdnObject *object)
{
	unregister_object (group, object);

	if (cdn_object_get_parent (object) == group)
	{
		_cdn_object_set_parent (object, NULL);
	}

	g_signal_emit (group, group_signals[CHILD_REMOVED], 0, object);
	g_object_unref (object);
}

gboolean
cdn_node_verify_remove_child (CdnNode   *group,
                               CdnObject  *child,
                               GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (group), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (child), FALSE);

	gboolean ret = FALSE;

	g_signal_emit (group,
	               group_signals[VERIFY_REMOVE_CHILD],
	               0,
	               child,
	               error,
	               &ret);

	return !ret;
}

static gboolean
verify_all (CdnNode   *group,
            CdnObject  *object,
            GError    **error)
{
	if (!cdn_node_verify_remove_child (group, object, error))
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
cdn_node_remove_impl (CdnNode   *group,
                       CdnObject  *object,
                       GError    **error)
{
	GSList *item;

	item = g_slist_find (group->priv->children, object);

	if (!item)
	{
		g_set_error (error,
		             CDN_NODE_ERROR,
		             CDN_NODE_ERROR_CHILD_DOES_NOT_EXIST,
		             "The child `%s' does not exist in the group `%s'",
		             cdn_object_get_id (object),
		             cdn_object_get_id (CDN_OBJECT (group)));

		return FALSE;
	}

	if (!verify_all (group, object, error))
	{
		return FALSE;
	}

	if (object == group->priv->proxy)
	{
		set_proxy (group, NULL);
	}

	group->priv->children = g_slist_remove_link (group->priv->children,
	                                             item);

	remove_object (group, object);
	return TRUE;
}

static void
cdn_node_cdn_clear (CdnObject *object)
{
	CdnNode *group = CDN_NODE (object);

	set_proxy (group, NULL);

	GSList *children = g_slist_copy (group->priv->children);
	GSList *child;

	for (child = children; child; child = g_slist_next (child))
	{
		cdn_node_remove (group, child->data, NULL);
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
	CdnNode *group1 = CDN_NODE (first);
	CdnNode *group2 = CDN_NODE (second);

	if ((group1->priv->proxy == NULL && group2->priv->proxy != NULL) ||
	    (group2->priv->proxy == NULL && group1->priv->proxy != NULL))
	{
		return FALSE;
	}

	if (group1->priv->proxy &&
	    g_strcmp0 (cdn_object_get_id (group1->priv->proxy),
	               cdn_object_get_id (group2->priv->proxy)) != 0)
	{
		return FALSE;
	}

	GSList const *children1 = cdn_node_get_children (group1);
	GSList const *children2 = cdn_node_get_children (group2);

	if (g_slist_length ((GSList *)children1) != g_slist_length ((GSList *)children2))
	{
		return FALSE;
	}

	while (children1)
	{
		CdnObject *child1 = children1->data;
		CdnObject *child2 = cdn_node_get_child (group2,
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
cdn_node_get_children_impl (CdnNode *group)
{
	return group->priv->children;
}

static gboolean
cdn_node_verify_remove_child_impl (CdnNode   *group,
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
	CdnNode *group;

	group = CDN_NODE (object);

	g_slist_free (group->priv->actors);
	group->priv->actors = NULL;

	CDN_OBJECT_CLASS (cdn_node_parent_class)->taint (object);
}

static void
cdn_node_class_init (CdnNodeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_node_finalize;
	object_class->dispose = cdn_node_dispose;

	object_class->get_property = cdn_node_get_property;
	object_class->set_property = cdn_node_set_property;

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
	 * CdnNode:proxy:
	 *
	 * The group proxy object
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_PROXY,
	                                 g_param_spec_object ("proxy",
	                                                      "Proxy",
	                                                      "Proxy",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * CdnNode::child-added:
	 * @object: a #CdnObject
	 * @action: the added #CdnObject
	 *
	 * Emitted when a child object is added to the group
	 *
	 **/
	group_signals[CHILD_ADDED] =
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
	 * Emitted when a child object is removed from the group
	 *
	 **/
	group_signals[CHILD_REMOVED] =
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
	group_signals[VERIFY_REMOVE_CHILD] =
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
	group_signals[VERIFY_ADD_CHILD] =
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
                                     CdnNode              *group)
{
	/* If it comes from a proxy, we deny */
	if (interface_is_from_proxy (group, name))
	{
		g_set_error (error,
		             CDN_NODE_ERROR,
		             CDN_NODE_ERROR_INTERFACE_IS_PROXY,
		             "The interface `%s' is automatically generated from the proxy `%s' and cannot be removed manually",
		             name,
		             cdn_object_get_id (group->priv->proxy));

		return FALSE;
	}

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
 * @id: the group id
 * @proxy: A #CdnObject
 *
 * Create a new group.
 *
 * Returns: A #CdnNode
 *
 **/
CdnNode *
cdn_node_new (gchar const *id,
               CdnObject   *proxy)
{
	return g_object_new (CDN_TYPE_NODE,
	                     "id", id,
	                     "proxy", proxy,
	                     NULL);
}

/**
 * cdn_node_get_children:
 * @group: A #CdnNode
 *
 * Get a list of the children in @group.
 *
 * Returns: (element-type CdnObject) (transfer none): A #GSList of #CdnObject
 *
 **/
const GSList *
cdn_node_get_children (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	return CDN_NODE_GET_CLASS (group)->get_children (group);
}

/**
 * cdn_node_add:
 * @group: A #CdnNode
 * @object: A #CdnObject
 *
 * Add a child object to the group.
 *
 * Returns: %TRUE if the child could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cdn_node_add (CdnNode   *group,
               CdnObject  *object,
               GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (group), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	if (CDN_NODE_GET_CLASS (group)->add)
	{
		return CDN_NODE_GET_CLASS (group)->add (group, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_node_remove:
 * @group: A #CdnNode
 * @object: A #CdnObject
 * @error: A #GError
 *
 * Remove a child object from the group.
 *
 * Returns: %TRUE if the child was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cdn_node_remove (CdnNode   *group,
                  CdnObject  *object,
                  GError    **error)
{
	g_return_val_if_fail (CDN_IS_NODE (group), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	if (CDN_NODE_GET_CLASS (group)->remove)
	{
		return CDN_NODE_GET_CLASS (group)->remove (group, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_node_set_proxy:
 * @group: A #CdnNode
 * @proxy: A #CdnObject
 *
 * Set the proxy object of @group to @proxy.
 *
 * Returns: %TRUE if the proxy could be successfully changed, %FALSE otherwise
 *
 **/
gboolean
cdn_node_set_proxy (CdnNode  *group,
                     CdnObject *proxy)
{
	g_return_val_if_fail (CDN_IS_NODE (group), FALSE);
	g_return_val_if_fail (proxy == NULL || CDN_IS_OBJECT (proxy), FALSE);

	return set_proxy (group, proxy);
}

/**
 * cdn_node_get_proxy:
 * @group: A #CdnNode
 *
 * Get the proxy object of @group.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_node_get_proxy (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	return group->priv->proxy;
}

/**
 * cdn_node_foreach:
 * @group: A #CdnNode
 * @func: (scope call): A #GFunc
 * @data: User data
 *
 * Call @func for each child object in the group.
 *
 **/
void
cdn_node_foreach (CdnNode *group,
                   GFunc     func,
                   gpointer  data)
{
	g_return_if_fail (CDN_IS_NODE (group));

	g_slist_foreach ((GSList *)cdn_node_get_children (group), func, data);
}

/**
 * cdn_node_get_child:
 * @group: A #CdnNode
 * @name: The child name
 *
 * Get a child from the group by name.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_node_get_child (CdnNode    *group,
                     const gchar *name)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	return g_hash_table_lookup (group->priv->child_hash,
	                            name);
}

/**
 * cdn_node_find_objects:
 * @group: A #CdnNode
 * @path: The object path
 *
 * Find objects by specifying a path. For example, if there is
 * another group "g" containing a state "s", you can use
 * cdn_node_find_object (group, "g.s") to get the object.
 *
 * Returns: (transfer container) (element-type CdnObject): A #CdnObject
 *
 **/
GSList *
cdn_node_find_objects (CdnNode    *group,
                        const gchar *selector)
{
	CdnSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cdn_selector_parse (CDN_OBJECT (group), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);

		return NULL;
	}

	all = cdn_selector_select (sel,
	                           G_OBJECT (group),
	                           CDN_SELECTOR_TYPE_OBJECT,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_get_object (all->data));

		g_object_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cdn_node_find_object:
 * @group: A #CdnNode
 * @path: The object path
 *
 * Find object by specifying a path. For example, if there is
 * another group "g" containing a state "s", you can use
 * cdn_node_find_object (group, "g.s") to get the object.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_node_find_object (CdnNode    *group,
                       const gchar *selector)
{
	GSList *all;
	CdnObject *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cdn_node_find_objects (group, selector);

	if (all)
	{
		ret =  all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cdn_node_remove_variables:
 * @group: A #CdnNode
 * @path: The object path
 *
 * Find variables by specifying a path. For example, if there is
 * another group "g" containing a state "s" with property "x", you can use
 * cdn_node_remove_variables (group, "g.s.x") to get the property.
 *
 * Returns: (transfer container) (element-type CdnVariable): A list of #CdnVariable
 *
 **/
GSList *
cdn_node_remove_variables (CdnNode    *group,
                           gchar const *selector)
{
	CdnSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cdn_selector_parse (CDN_OBJECT (group), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);
		return NULL;
	}

	all = cdn_selector_select (sel,
	                           G_OBJECT (group),
	                           CDN_SELECTOR_TYPE_VARIABLE,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cdn_selection_get_object (all->data));

		g_object_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cdn_node_find_variable:
 * @group: A #CdnNode
 * @path: The property path
 *
 * Find a property by specifying an object path. For example, if there is
 * another group "g" containing a state "s" with a property "x", you can use
 * cdn_node_find_variable (group, "g.s.x") to get the property.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_node_find_variable (CdnNode    *group,
                         gchar const *selector)
{
	GSList *all;
	CdnVariable *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cdn_node_remove_variables (group, selector);

	if (all)
	{
		ret = all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cdn_node_get_variable_interface:
 * @group: A #CdnNode
 *
 * Get the property interface of the group.
 *
 * Returns: (transfer none): A #CdnVariableInterface
 *
 **/
CdnVariableInterface *
cdn_node_get_variable_interface (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	return group->priv->variable_interface;
}

/**
 * cdn_node_get_auto_templates_for_child:
 * @group: A #CdnNode
 * @child: A #CdnObject
 *
 * Get the templates that were automatically applied from the group to the
 * child.
 *
 * Returns: (transfer container) (allow-none): A #GSList of #CdnObject
 *
 **/
GSList *
cdn_node_get_auto_templates_for_child (CdnNode  *group,
                                        CdnObject *child)
{
	GSList const *templates;
	GSList *ret = NULL;

	g_return_val_if_fail (CDN_IS_NODE (group), NULL);
	g_return_val_if_fail (CDN_IS_OBJECT (child), NULL);

	templates = cdn_object_get_applied_templates (CDN_OBJECT (group));

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
 * @link: the #CdnEdge which links to this object
 *
 * Adds @link as a link which targets the object.
 *
 **/
void
_cdn_node_link (CdnNode *group,
                 CdnEdge  *link)
{
	g_return_if_fail (CDN_IS_NODE (group));
	g_return_if_fail (CDN_IS_EDGE (link));

	group->priv->links = g_slist_append (group->priv->links, link);

	g_slist_free (group->priv->actors);
	group->priv->actors = NULL;

	g_object_add_toggle_ref (G_OBJECT (link),
	                         (GToggleNotify)link_destroyed,
	                         group);
}

/**
 * _cdn_node_unlink:
 * @group: the #CdnObject
 * @link: the #CdnEdge which unlinks from this group
 *
 * Removes @link as a link which targets the group.
 *
 **/
void
_cdn_node_unlink (CdnNode *group,
                   CdnEdge  *link)
{
	g_return_if_fail (CDN_IS_NODE (group));
	g_return_if_fail (CDN_IS_EDGE (link));

	GSList *item = g_slist_find (group->priv->links, link);

	if (!item)
	{
		return;
	}

	group->priv->links = g_slist_remove_link (group->priv->links,
	                                           item);

	g_slist_free (group->priv->actors);
	group->priv->actors = NULL;

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            group);
}

/**
 * cdn_node_get_actors:
 * @group: A #CdnNode
 *
 * Get the variables which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable.
 *
 **/
GSList const *
cdn_node_get_actors (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	if (group->priv->actors != NULL)
	{
		return group->priv->actors;
	}

	GSList *ret = NULL;
	GSList *item;

	for (item = group->priv->links; item; item = g_slist_next (item))
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

	group->priv->actors = g_slist_reverse (ret);
	return group->priv->actors;
}

/**
 * cdn_node_get_edges:
 * @group: A #CdnNode
 *
 * Get a list of links that act on this object.
 *
 * Returns: (element-type CdnEdge): A list of #CdnEdge
 *
 */
GSList const *
cdn_node_get_edges (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	return group->priv->links;
}

gboolean
cdn_node_has_self_edge (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), FALSE);

	return group->priv->self_link != NULL;
}


CdnEdgeForward *
cdn_node_get_self_edge (CdnNode *group)
{
	g_return_val_if_fail (CDN_IS_NODE (group), NULL);

	if (!group->priv->self_link)
	{
		group->priv->self_link = cdn_edge_new ("integrate",
		                                       group,
		                                       group);

		group->priv->links = g_slist_prepend (group->priv->links,
		                                      group->priv->self_link);
	}

	return group->priv->self_link;
}
