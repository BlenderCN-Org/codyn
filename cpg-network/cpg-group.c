/*
 * cpg-group.c
 * This file is part of cpg-network
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

#include "cpg-group.h"
#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-compile-context.h"
#include "cpg-marshal.h"
#include "cpg-utils.h"
#include "cpg-selector.h"
#include "cpg-layoutable.h"

/**
 * SECTION:cpg-group
 * @short_description: Group object grouping many objects together
 *
 * The #CpgGroup is a special #CpgObject that contains many objects as children.
 * This can be used make sub-networks that can be easily instantiated,
 * providing some common functionality.
 *
 * Each group can have a proxy object associated which serves as a bridge
 * between the outside and the inside of the group. In addition, each group
 * has an interface (#CpgPropertyInterface) which allows the designer of the
 * group to expose a certain subset of its child properties on the group.
 *
 * <refsect2 id="CpgGroup-COPY">
 * <title>CpgGroup Copy Semantics</title>
 * When a group is copied with #cpg_object_copy, all the children are
 * recursively copied as well. If a group has a proxy, the new group will
 * have its proxy set to the new copy of the original proxy.
 * </refsect2>
 */

#define CPG_GROUP_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_GROUP, CpgGroupPrivate))

enum
{
	EXT_PROPERTY_ADDED,
	EXT_PROPERTY_REMOVED,
	EXT_INTERFACE_PROPERTY_ADDED,
	EXT_INTERFACE_PROPERTY_REMOVED,
	NUM_EXT_SIGNALS
};

struct _CpgGroupPrivate
{
	CpgObject *proxy;

	GSList *children;
	GHashTable *child_hash;

	CpgPropertyInterface *property_interface;

	/* Links */
	GSList *links;
	GSList *actors;
	CpgLink *self_link;

	guint proxy_signals[NUM_EXT_SIGNALS];
};

G_DEFINE_TYPE (CpgGroup, cpg_group, CPG_TYPE_OBJECT)

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

static void remove_object (CpgGroup *group, CpgObject *object);

GQuark
cpg_group_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_group_error");
	}

	return quark;
}

static void
link_destroyed (CpgGroup  *group,
                CpgLink   *link,
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
cpg_group_finalize (GObject *object)
{
	CpgGroup *group = CPG_GROUP (object);

	g_hash_table_destroy (group->priv->child_hash);

	g_slist_free (group->priv->links);
	g_slist_free (group->priv->actors);

	G_OBJECT_CLASS (cpg_group_parent_class)->finalize (object);
}

static CpgObject *
get_child_from_template (CpgGroup  *group,
                         CpgObject *templ)
{
	GSList const *children;

	children = group->priv->children;

	while (children)
	{
		CpgObject *obj = children->data;
		GSList const *templates = cpg_object_get_applied_templates (obj);

		if (g_slist_find ((GSList *)templates, obj))
		{
			return obj;
		}

		children = g_slist_next (children);
	}

	return NULL;
}

static CpgLayoutable *
last_layoutable_template (CpgObject *child)
{
	CpgLayoutable *last = NULL;
	GSList const *templates;

	templates = cpg_object_get_applied_templates (child);

	while (templates)
	{
		if (CPG_IS_LAYOUTABLE (templates->data) &&
		    cpg_layoutable_supports_location (templates->data) &&
		    cpg_layoutable_get_has_location (templates->data))
		{
			last = templates->data;
		}

		templates = g_slist_next (templates);
	}

	return last;
}

static gboolean
should_propagate_layout (CpgGroup  *group,
                         CpgObject *child,
                         CpgObject *templ)
{
	CpgLayoutable *last;

	if (!CPG_IS_LAYOUTABLE (templ) ||
	    !cpg_layoutable_supports_location (CPG_LAYOUTABLE (templ)) ||
	    !cpg_layoutable_get_has_location (CPG_LAYOUTABLE (templ)))
	{
		return FALSE;
	}

	if (!cpg_layoutable_get_has_location (CPG_LAYOUTABLE (child)))
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

		cpg_layoutable_get_location (last, &lx, &ly);
		cpg_layoutable_get_location (CPG_LAYOUTABLE (child), &x, &y);

		return x == lx && y == ly;
	}

	return TRUE;
}

static void
layout_from_template (CpgLayoutable *layoutable)
{
	CpgLayoutable *templ;
	gint x;
	gint y;

	cpg_layoutable_set_location (layoutable, 0, 0);
	cpg_layoutable_set_has_location (layoutable, FALSE);

	templ = last_layoutable_template (CPG_OBJECT (layoutable));

	if (!templ)
	{
		return;
	}

	cpg_layoutable_get_location (templ, &x, &y);
	cpg_layoutable_set_location (layoutable, x, y);
}


static void
on_template_child_added (CpgGroup  *templ,
                         CpgObject *child,
                         CpgGroup  *group)
{
	CpgObject *obj;
	gboolean layout;

	obj = cpg_group_get_child (group, cpg_object_get_id (child));
	layout = should_propagate_layout (group, obj, child);

	if (obj != NULL && G_TYPE_FROM_INSTANCE (child) == G_TYPE_FROM_INSTANCE (obj))
	{
		cpg_object_apply_template (obj, child, NULL);
	}
	else
	{
		obj = cpg_object_new_from_template (child, NULL);
		cpg_group_add (group, obj, NULL);
		g_object_unref (obj);
	}

	if (layout)
	{
		layout_from_template (CPG_LAYOUTABLE (child));
	}
}

static void
on_template_child_removed (CpgGroup  *templ,
                           CpgObject *child,
                           CpgGroup  *group)
{
	CpgObject *obj;

	obj = get_child_from_template (group, child);

	if (obj)
	{
		GSList *properties;
		gboolean relayout;

		relayout = should_propagate_layout (group, obj, child) &&
		           last_layoutable_template (obj) == CPG_LAYOUTABLE (child);

		cpg_object_unapply_template (obj, child, NULL);

		properties = cpg_object_get_properties (obj);

		if (cpg_object_get_applied_templates (obj) == NULL &&
		    properties == NULL &&
		    (!CPG_IS_GROUP (obj) || cpg_group_get_links (CPG_GROUP (obj)) == NULL))
		{
			cpg_group_remove (templ, child, NULL);
		}

		if (relayout)
		{
			layout_from_template (CPG_LAYOUTABLE (child));
		}

		g_slist_free (properties);
	}
}

static gboolean
interface_is_from_proxy (CpgGroup    *group,
                         gchar const *name)
{
	gchar const *child_name;

	if (!group->priv->proxy)
	{
		return FALSE;
	}

	child_name = cpg_property_interface_lookup_child_name (group->priv->property_interface,
	                                                       name);

	return g_strcmp0 (cpg_object_get_id (group->priv->proxy), child_name) == 0;
}

static gboolean
interface_should_override (CpgGroup     *group,
                           CpgGroup     *template,
                           gchar const  *name,
                           CpgGroup    **last_templ)
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

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	while (templates)
	{
		CpgGroup *tmpl;
		CpgPropertyInterface *iface;
		gboolean templprop;

		tmpl = templates->data;
		templates = g_slist_next (templates);

		if (!CPG_IS_GROUP (tmpl))
		{
			continue;
		}

		iface = cpg_group_get_property_interface (tmpl);
		templprop = cpg_property_interface_implements (iface, name);

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
add_template_interface (CpgGroup     *group,
                        CpgGroup     *source,
                        gchar const  *name,
                        gchar const  *child_name,
                        gchar const  *property_name,
                        GError      **error)
{
	CpgPropertyInterface *iface;

	iface = cpg_group_get_property_interface (group);

	if (cpg_property_interface_implements (iface, name))
	{
		if (interface_should_override (group, source, name, NULL))
		{
			if (!cpg_property_interface_remove (iface, name, error))
			{
				return FALSE;
			}

			if (!cpg_property_interface_add (iface,
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
		if (!cpg_property_interface_add (iface,
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
remove_template_interface (CpgGroup    *group,
                           CpgGroup    *source,
                           gchar const *name,
                           gchar const *child_name,
                           gchar const *property_name)
{
	CpgPropertyInterface *iface;
	GError *error = NULL;
	CpgGroup *last = NULL;

	/* Check if this interface actually owns that property */
	if (!interface_should_override (group, source, name, &last))
	{
		return;
	}

	/* Remove the mapping */
	iface = cpg_group_get_property_interface (group);

	if (!cpg_property_interface_remove (iface, name, &error))
	{
		g_warning ("Could not remove interface property `%s' from template `%s': %s",
		           name,
		           cpg_object_get_id (CPG_OBJECT (source)),
		           error->message);

		g_error_free (error);
		return;
	}

	if (last)
	{
		/* Lookup new child and property name */
		iface = cpg_group_get_property_interface (last);
		child_name = cpg_property_interface_lookup_child_name (iface, name);
		property_name = cpg_property_interface_lookup_property_name (iface, name);

		add_template_interface (group,
		                        last,
		                        name,
		                        child_name,
		                        property_name,
		                        &error);
	}
}

static void
on_template_interface_property_added (CpgPropertyInterface *templ_iface,
                                      gchar const          *name,
                                      gchar const          *child_name,
                                      gchar const          *property_name,
                                      CpgGroup             *group)
{
	add_template_interface (group,
	                        cpg_property_interface_get_group (templ_iface),
	                        name,
	                        child_name,
	                        property_name,
	                        NULL);
}

static void
on_template_interface_property_removed (CpgPropertyInterface *templ_iface,
                                        gchar const          *name,
                                        gchar const          *child_name,
                                        gchar const          *property_name,
                                        CpgGroup             *group)
{
	remove_template_interface (group,
	                           cpg_property_interface_get_group (templ_iface),
	                           name,
	                           child_name,
	                           property_name);
}

static void
disconnect_template (CpgGroup  *group,
                     CpgObject *templ)
{
	CpgPropertyInterface *piface;

	piface = cpg_group_get_property_interface (CPG_GROUP (templ));

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_property_added,
	                                      group);

	g_signal_handlers_disconnect_by_func (piface,
	                                      on_template_interface_property_removed,
	                                      group);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_added,
	                                      group);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_child_removed,
	                                      group);
}

static void
cpg_group_dispose (GObject *object)
{
	CpgGroup *group = CPG_GROUP (object);
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
		CpgObject *proxy = group->priv->proxy;
		group->priv->proxy = NULL;

		g_object_unref (proxy);
	}

	templates = cpg_object_get_applied_templates (CPG_OBJECT (object));

	while (templates)
	{
		disconnect_template (group, templates->data);
		templates = g_slist_next (templates);
	}

	if (group->priv->property_interface)
	{
		g_object_unref (group->priv->property_interface);
		group->priv->property_interface = NULL;
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

	G_OBJECT_CLASS (cpg_group_parent_class)->dispose (object);
}

static void
proxy_add_property (CpgGroup    *group,
                    gchar const *name,
                    gchar const *child_name)
{
	if (cpg_property_interface_implements (group->priv->property_interface,
	                                       name))
	{
		cpg_property_interface_remove (group->priv->property_interface,
		                               name,
		                               NULL);
	}

	cpg_property_interface_add (group->priv->property_interface,
	                            name,
	                            child_name,
	                            name,
	                            NULL);
}

static void
add_interface_after_proxy_remove (CpgGroup    *group,
                                  gchar const *name)
{
	GSList const *templates;
	CpgGroup *templ = NULL;

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	while (templates)
	{
		CpgObject *temp = templates->data;

		if (CPG_IS_GROUP (temp))
		{
			CpgPropertyInterface *iface;

			iface = cpg_group_get_property_interface (CPG_GROUP (temp));

			if (cpg_property_interface_implements (iface, name))
			{
				templ = CPG_GROUP (temp);
			}
		}

		templates = g_slist_next (templates);
	}

	if (templ != NULL)
	{
		CpgPropertyInterface *iface;

		iface = cpg_group_get_property_interface (templ);

		add_template_interface (group,
		                        templ,
		                        name,
		                        cpg_property_interface_lookup_child_name (iface, name),
		                        cpg_property_interface_lookup_property_name (iface, name),
		                        NULL);
	}
}

static void
proxy_remove_property (CpgGroup    *group,
                       gchar const *name)
{
	if (cpg_property_interface_implements (group->priv->property_interface,
	                                       name))
	{
		cpg_property_interface_remove (group->priv->property_interface,
		                               name,
		                               NULL);

		/* Then maybe generate property for interfaces */
		add_interface_after_proxy_remove (group, name);
	}
}

static void
on_proxy_property_added (CpgObject   *proxy,
                         CpgProperty *property,
                         CpgGroup    *group)
{
	proxy_add_property (group,
	                    cpg_property_get_name (property),
	                    cpg_object_get_id (proxy));
}

static void
on_proxy_property_removed (CpgObject   *proxy,
                           CpgProperty *property,
                           CpgGroup    *group)
{
	proxy_remove_property (group,
	                       cpg_property_get_name (property));
}

static void
on_proxy_interface_property_added (CpgPropertyInterface *iface,
                                   gchar const          *name,
                                   gchar const          *child_name,
                                   gchar const          *property_name,
                                   CpgGroup             *group)
{
	proxy_add_property (group, name, child_name);
}

static void
on_proxy_interface_property_removed (CpgPropertyInterface *iface,
                                     gchar const          *name,
                                     gchar const          *child_name,
                                     gchar const          *property_name,
                                     CpgGroup             *group)
{
	proxy_remove_property (group, name);
}

static gboolean
set_proxy (CpgGroup  *group,
           CpgObject *proxy)
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

		if (CPG_IS_GROUP (group->priv->proxy))
		{
			CpgPropertyInterface *iface;

			iface = cpg_group_get_property_interface (CPG_GROUP (group->priv->proxy));

			g_signal_handler_disconnect (iface,
			                             group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_ADDED]);

			g_signal_handler_disconnect (iface,
			                             group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_REMOVED]);
		}

		GSList *properties = cpg_object_get_properties (group->priv->proxy);
		GSList *item;

		CpgObject *pr = group->priv->proxy;
		group->priv->proxy = NULL;

		/* Remove automatically mapped properties from interface */
		for (item = properties; item; item = g_slist_next (item))
		{
			proxy_remove_property (group,
			                       cpg_property_get_name (item->data));
		}

		g_slist_free (properties);

		if (CPG_IS_GROUP (pr))
		{
			CpgPropertyInterface *iface;
			gchar **names;
			gchar **ptr;

			iface = cpg_group_get_property_interface (CPG_GROUP (pr));

			names = cpg_property_interface_get_names (iface);

			for (ptr = names; ptr && *ptr; ++ptr)
			{
				proxy_remove_property (group, *ptr);
			}

			g_strfreev (names);
		}

		g_object_unref (pr);
	}

	if (proxy)
	{
		group->priv->proxy = g_object_ref (proxy);
		cpg_group_add (group, proxy, NULL);

		GSList *properties = cpg_object_get_properties (group->priv->proxy);
		GSList *item;

		for (item = properties; item; item = g_slist_next (item))
		{
			proxy_add_property (group,
			                    cpg_property_get_name (item->data),
			                    cpg_object_get_id (proxy));
		}

		g_slist_free (properties);

		group->priv->proxy_signals[EXT_PROPERTY_ADDED] =
			g_signal_connect_after (group->priv->proxy,
			                        "property-added",
			                        G_CALLBACK (on_proxy_property_added),
			                        group);

		group->priv->proxy_signals[EXT_PROPERTY_REMOVED] =
			g_signal_connect (group->priv->proxy,
			                  "property-removed",
			                  G_CALLBACK (on_proxy_property_removed),
			                  group);

		if (CPG_IS_GROUP (group->priv->proxy))
		{
			CpgPropertyInterface *iface;
			gchar **names;
			gchar **ptr;

			iface = cpg_group_get_property_interface (CPG_GROUP (group->priv->proxy));
			names = cpg_property_interface_get_names (iface);

			for (ptr = names; ptr && *ptr; ++ptr)
			{
				proxy_add_property (group,
				                    *ptr,
				                    cpg_object_get_id (proxy));
			}

			g_strfreev (names);

			group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_ADDED] =
				g_signal_connect_after (iface,
				                        "added",
				                        G_CALLBACK (on_proxy_interface_property_added),
				                        group);

			group->priv->proxy_signals[EXT_INTERFACE_PROPERTY_REMOVED] =
				g_signal_connect (iface,
				                  "removed",
				                  G_CALLBACK (on_proxy_interface_property_removed),
				                  group);
		}
	}

	g_object_notify (G_OBJECT (group), "proxy");

	cpg_object_taint (CPG_OBJECT (group));
	return TRUE;
}

static void
cpg_group_set_property (GObject      *object,
                        guint         prop_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
	CpgGroup *self = CPG_GROUP (object);

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
cpg_group_get_property (GObject    *object,
                        guint       prop_id,
                        GValue     *value,
                        GParamSpec *pspec)
{
	CpgGroup *self = CPG_GROUP (object);

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

static CpgProperty *
cpg_group_cpg_get_property (CpgObject   *object,
                            gchar const *name)
{
	CpgProperty *prop;
	CpgGroup *group = CPG_GROUP (object);

	prop = CPG_OBJECT_CLASS (cpg_group_parent_class)->get_property (object,
	                                                                name);

	if (!prop)
	{
		prop = cpg_property_interface_lookup (group->priv->property_interface,
		                                      name);
	}

	return prop;
}

static void
prepend_functions (CpgGroup          *group,
                   CpgCompileContext *context)
{
	GSList *item;

	if (!group)
	{
		return;
	}

	item = group->priv->children;

	while (item)
	{
		if (CPG_IS_FUNCTION (item->data))
		{
			cpg_compile_context_prepend_function (context,
			                                      item->data);
		}

		item = g_slist_next (item);
	}
}

static CpgCompileContext *
cpg_group_cpg_get_compile_context (CpgObject         *object,
                                   CpgCompileContext *context)
{
	CpgCompileContext *ret;
	CpgGroup *group;

	group = CPG_GROUP (object);

	ret = CPG_OBJECT_CLASS (cpg_group_parent_class)->get_compile_context (object, context);

	prepend_functions (group, ret);
	return ret;
}

static gboolean
cpg_group_cpg_compile (CpgObject         *object,
                       CpgCompileContext *context,
                       CpgCompileError   *error)
{
	CpgGroup *group = CPG_GROUP (object);
	GSList *item;
	gboolean ret;
	GSList *others = NULL;
	GSList *othersl = NULL;

	if (cpg_object_is_compiled (object))
	{
		return TRUE;
	}

	if (context)
	{
		cpg_compile_context_save (context);
		g_object_ref (context);
	}

	/* Prepend all the defined functions in the instances */
	context = cpg_group_cpg_get_compile_context (object, context);

	item = group->priv->children;

	while (item)
	{
		if (!CPG_IS_FUNCTION (item->data))
		{
			others = g_slist_prepend (others,
			                          item->data);
		}
		else if (!cpg_object_compile (item->data, context, error))
		{
			cpg_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	others = g_slist_reverse (others);
	item = others;

	while (item)
	{
		if (!CPG_IS_LINK (item->data))
		{
			othersl = g_slist_prepend (othersl,
			                           item->data);
		}
		else if (!cpg_object_compile (item->data, context, error))
		{
			cpg_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	if (group->priv->self_link)
	{
		if (!cpg_object_compile (CPG_OBJECT (group->priv->self_link), context, error))
		{
			cpg_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}
	}

	g_slist_free (others);
	othersl = g_slist_reverse (othersl);
	item = othersl;

	while (item)
	{
		if (!cpg_object_compile (item->data, context, error))
		{
			cpg_compile_context_restore (context);
			g_object_unref (context);

			return FALSE;
		}

		item = g_slist_next (item);
	}

	g_slist_free (othersl);

	ret = CPG_OBJECT_CLASS (cpg_group_parent_class)->compile (object,
	                                                          context,
	                                                          error);

	cpg_compile_context_restore (context);
	g_object_unref (context);

	return ret;
}

static void
cpg_group_cpg_reset (CpgObject *object)
{
	CpgGroup *group;

	CPG_OBJECT_CLASS (cpg_group_parent_class)->reset (object);

	group = CPG_GROUP (object);

	/* And then also the children! */
	cpg_group_foreach (group,
	                   (GFunc)cpg_object_reset,
	                   NULL);
}

static void
cpg_group_cpg_foreach_expression (CpgObject                *object,
                                  CpgForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	CpgGroup *group;
	GSList *item;

	group = CPG_GROUP (object);

	CPG_OBJECT_CLASS (cpg_group_parent_class)->foreach_expression (object,
	                                                               func,
	                                                               userdata);

	/* And then also the children! */
	for (item = group->priv->children; item; item = g_slist_next (item))
	{
		cpg_object_foreach_expression (item->data, func, userdata);
	}
}

static void
reconnect_children (CpgGroup   *group,
                    CpgGroup   *source,
                    GHashTable *mapping)
{
	GSList const *children;

	children = cpg_group_get_children (source);

	/* Reconnect all the links */
	while (children)
	{
		CpgObject *child = children->data;

		if (CPG_IS_LINK (child))
		{
			CpgLink *orig_link = CPG_LINK (child);
			CpgLink *copied_link;

			CpgGroup *copied_from;
			CpgGroup *copied_to;

			copied_link = g_hash_table_lookup (mapping,
			                                   child);

			copied_from = g_hash_table_lookup (mapping,
			                                   cpg_link_get_from (orig_link));

			copied_to = g_hash_table_lookup (mapping,
			                                 cpg_link_get_to (orig_link));

			cpg_link_attach (copied_link, copied_from, copied_to);
		}

		children = g_slist_next (children);
	}
}

static void
copy_children (CpgGroup *group,
               CpgGroup *source)
{
	CpgObject *proxy = cpg_group_get_proxy (source);
	GSList const *children = cpg_group_get_children (source);

	GHashTable *hash_table = g_hash_table_new (g_direct_hash,
	                                           g_direct_equal);

	while (children)
	{
		CpgObject *child = children->data;
		CpgObject *copied = cpg_object_copy (child);

		/* Store map from the original to the copy */
		g_hash_table_insert (hash_table, child, copied);

		cpg_group_add (group, copied, NULL);

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
copy_interface (CpgGroup *group,
                CpgGroup *source)
{
	CpgPropertyInterface *iface;
	CpgPropertyInterface *source_iface;
	gchar **names;
	gchar **ptr;

	iface = cpg_group_get_property_interface (group);
	source_iface = cpg_group_get_property_interface (source);

	names = cpg_property_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;
		GError *error = NULL;

		child_name = cpg_property_interface_lookup_child_name (source_iface, *ptr);
		property_name = cpg_property_interface_lookup_property_name (source_iface, *ptr);

		if (!cpg_property_interface_add (iface,
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

static CpgObject *
get_template_proxy (CpgGroup *group)
{
	if (group->priv->proxy == NULL)
	{
		return NULL;
	}

	GSList *templates = g_slist_copy ((GSList *)cpg_object_get_applied_templates (CPG_OBJECT (group)));
	templates = g_slist_reverse (templates);

	GSList *item;
	GSList const *proxy_templates = cpg_object_get_applied_templates (group->priv->proxy);

	for (item = templates; item; item = g_slist_next (item))
	{
		if (!CPG_IS_GROUP (item->data))
		{
			continue;
		}

		CpgObject *proxy = cpg_group_get_proxy (item->data);

		if (proxy && g_slist_find ((GSList *)proxy_templates, proxy))
		{
			CpgObject *ret;

			ret = item->data;
			g_slist_free (templates);
			return ret;
		}
	}

	g_slist_free (templates);
	return NULL;
}

static gboolean
cpg_group_cpg_unapply_template (CpgObject  *object,
                                CpgObject  *templ,
                                GError    **error)
{
	gchar **names;
	gchar **ptr;
	CpgPropertyInterface *source_iface;
	CpgGroup *group;
	CpgGroup *source;
	gboolean hadproxy;
	GSList const *children;

	group = CPG_GROUP (object);
	source = CPG_GROUP (templ);

	hadproxy = (get_template_proxy (group) == templ);

	if (!CPG_OBJECT_CLASS (cpg_group_parent_class)->unapply_template (object, templ, error))
	{
		return FALSE;
	}

	if (!CPG_IS_GROUP (templ))
	{
		return TRUE;
	}

	disconnect_template (group, templ);

	if (cpg_group_has_self_link (source))
	{
		if (!cpg_object_apply_template (CPG_OBJECT (cpg_group_get_self_link (group)),
		                                CPG_OBJECT (cpg_group_get_self_link (source)),
		                                error))
		{
			return FALSE;
		}
	}

	/* Remove interface */
	source_iface = cpg_group_get_property_interface (source);

	names = cpg_property_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;

		child_name = cpg_property_interface_lookup_child_name (source_iface, *ptr);
		property_name = cpg_property_interface_lookup_property_name (source_iface, *ptr);

		remove_template_interface (group, source, *ptr, child_name, property_name);
	}

	if (hadproxy)
	{
		/* Set to next proxy */
		GSList const *templates;
		CpgObject *proxy = NULL;

		templates = cpg_object_get_applied_templates (object);

		while (templates)
		{
			CpgObject *pr;

			if (CPG_IS_GROUP (templates->data))
			{
				pr = cpg_group_get_proxy (templates->data);

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
	children = cpg_group_get_children (source);

	while (children)
	{
		CpgObject *child;
		CpgObject *orig;

		child = children->data;

		orig = cpg_group_get_child (group, cpg_object_get_id (child));

		if (orig != NULL)
		{
			GSList *properties;
			gboolean layout;

			layout = should_propagate_layout (group,
			                                  orig,
			                                  child) &&
			         last_layoutable_template (orig) == CPG_LAYOUTABLE (child);

			/* TODO: make atomic */
			if (!cpg_object_unapply_template (orig, child, error))
			{
				return FALSE;
			}

			properties = cpg_object_get_properties (orig);

			/* Check if object is now empty */
			if (!cpg_object_get_applied_templates (orig) &&
			    !properties &&
			    !cpg_group_get_links (CPG_GROUP (orig)))
			{
				/* Then also remove it because it was introduced
				   by this template */
				if (!cpg_group_remove (group,
				                       orig,
				                       error))
				{
					/* TODO: make atomic */
					g_slist_free (properties);
					return FALSE;
				}
			}

			if (layout)
			{
				layout_from_template (CPG_LAYOUTABLE (orig));
			}

			g_slist_free (properties);
		}

		children = g_slist_next (children);
	}

	return TRUE;
}

static gboolean
cpg_group_cpg_apply_template (CpgObject  *object,
                              CpgObject  *templ,
                              GError    **error)
{
	CpgPropertyInterface *source_iface;
	CpgGroup *group;
	CpgGroup *source;
	CpgObject *proxy;
	GSList const *children;
	gchar **names;
	gchar **ptr;

	if (!CPG_OBJECT_CLASS (cpg_group_parent_class)->apply_template (object, templ, error))
	{
		return FALSE;
	}

	if (!CPG_IS_GROUP (templ))
	{
		return TRUE;
	}

	group = CPG_GROUP (object);
	source = CPG_GROUP (templ);

	proxy = cpg_group_get_proxy (source);
	children = cpg_group_get_children (source);

	/* Copy in children */
	while (children)
	{
		CpgObject *child = children->data;
		gboolean layout;

		/* Check to find existing one */
		CpgObject *new_child;

		new_child = cpg_group_get_child (group,
		                                 cpg_object_get_id (child));

		if (!new_child)
		{
			new_child = g_object_new (G_TYPE_FROM_INSTANCE (child),
			                          "id", cpg_object_get_id (child),
			                          NULL);

			cpg_group_add (group, new_child, NULL);
			g_object_unref (new_child);
		}

		layout = should_propagate_layout (group, new_child, child);

		if (!cpg_object_apply_template (new_child,
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
			layout_from_template (CPG_LAYOUTABLE (new_child));
		}

		children = g_slist_next (children);
	}

	/* Apply interfaces from template */
	source_iface = cpg_group_get_property_interface (source);

	names = cpg_property_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		gchar const *child_name;
		gchar const *property_name;

		child_name = cpg_property_interface_lookup_child_name (source_iface, *ptr);
		property_name = cpg_property_interface_lookup_property_name (source_iface, *ptr);

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

	if (cpg_group_has_self_link (source))
	{
		if (!cpg_object_apply_template (CPG_OBJECT (cpg_group_get_self_link (group)),
		                                CPG_OBJECT (cpg_group_get_self_link (source)),
		                                error))
		{
			return FALSE;
		}
	}

	g_strfreev (names);

	g_signal_connect (source_iface,
	                  "added",
	                  G_CALLBACK (on_template_interface_property_added),
	                  group);

	g_signal_connect (source_iface,
	                  "removed",
	                  G_CALLBACK (on_template_interface_property_removed),
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
cpg_group_cpg_copy (CpgObject *object,
                    CpgObject *source)
{
	CpgGroup *g = CPG_GROUP (object);

	CPG_OBJECT_CLASS (cpg_group_parent_class)->copy (object, source);

	/* Copy over children */
	copy_children (g, CPG_GROUP (source));

	/* Copy over interface */
	copy_interface (g, CPG_GROUP (source));

	if (cpg_group_has_self_link (CPG_GROUP (source)))
	{
		CpgLink *sl;

		sl = cpg_group_get_self_link (CPG_GROUP (source));
		sl = CPG_LINK (cpg_object_copy (CPG_OBJECT (sl)));

		g->priv->self_link = sl;
		g->priv->links = g_slist_prepend (g->priv->links,
		                                  sl);
	}
}

static gchar *
unique_id (CpgGroup  *group,
           CpgObject *object)
{
	gchar const *id = cpg_object_get_id (object);
	gchar *newid = g_strdup (id);
	gint cnt = 0;

	while (TRUE)
	{
		CpgObject *orig = g_hash_table_lookup (group->priv->child_hash,
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
register_id (CpgGroup  *group,
             CpgObject *object)
{
	gchar *newid = unique_id (group, object);

	if (newid == NULL)
	{
		g_hash_table_insert (group->priv->child_hash,
		                     g_strdup (cpg_object_get_id (object)),
		                     object);
	}
	else
	{
		cpg_object_set_id (object, newid);
		g_free (newid);
	}
}

typedef struct
{
	CpgObject *find;
	const gchar *id;
} FindInfo;

static gboolean
find_object (const gchar  *id,
             CpgObject    *object,
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
update_object_id (CpgObject  *object,
                  GParamSpec *spec,
                  CpgGroup   *group)
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
unregister_object (CpgGroup  *group,
                   CpgObject *object)
{
	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (cpg_object_taint),
	                                      group);

	g_signal_handlers_disconnect_by_func (object,
	                                      G_CALLBACK (update_object_id),
	                                      group);

	g_hash_table_remove (group->priv->child_hash,
	                     cpg_object_get_id (object));
}

static void
register_object (CpgGroup  *group,
                 CpgObject *object)
{
	g_signal_connect_swapped (object,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          group);

	g_signal_connect (object,
	                  "notify::id",
	                  G_CALLBACK (update_object_id),
	                  group);

	register_id (group, object);
}

static gboolean
cpg_group_add_impl (CpgGroup   *group,
                    CpgObject  *object,
                    GError    **error)
{
	CpgObject *other;

	other = g_hash_table_lookup (group->priv->child_hash,
	                             cpg_object_get_id (object));

	if (other == object)
	{
		g_set_error (error,
		             CPG_GROUP_ERROR,
		             CPG_GROUP_ERROR_CHILD_ALREADY_EXISTS,
		             "The child `%s' already exists in the group `%s'",
		             cpg_object_get_id (object),
		             cpg_object_get_id (CPG_OBJECT (group)));

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

	_cpg_object_set_parent (object, group);

	cpg_object_taint (CPG_OBJECT (group));
	g_signal_emit (group, group_signals[CHILD_ADDED], 0, object);

	return TRUE;
}

static void
remove_object (CpgGroup  *group,
               CpgObject *object)
{
	unregister_object (group, object);

	if (cpg_object_get_parent (object) == group)
	{
		_cpg_object_set_parent (object, NULL);
	}

	g_signal_emit (group, group_signals[CHILD_REMOVED], 0, object);
	g_object_unref (object);
}

gboolean
cpg_group_verify_remove_child (CpgGroup   *group,
                               CpgObject  *child,
                               GError    **error)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (child), FALSE);

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
verify_all (CpgGroup   *group,
            CpgObject  *object,
            GError    **error)
{
	if (!cpg_group_verify_remove_child (group, object, error))
	{
		return FALSE;
	}

	if (CPG_IS_GROUP (object))
	{
		CpgGroup *other = CPG_GROUP (object);
		GSList const *children = cpg_group_get_children (other);

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
cpg_group_remove_impl (CpgGroup   *group,
                       CpgObject  *object,
                       GError    **error)
{
	GSList *item;

	item = g_slist_find (group->priv->children, object);

	if (!item)
	{
		g_set_error (error,
		             CPG_GROUP_ERROR,
		             CPG_GROUP_ERROR_CHILD_DOES_NOT_EXIST,
		             "The child `%s' does not exist in the group `%s'",
		             cpg_object_get_id (object),
		             cpg_object_get_id (CPG_OBJECT (group)));

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
cpg_group_cpg_clear (CpgObject *object)
{
	CpgGroup *group = CPG_GROUP (object);

	set_proxy (group, NULL);

	GSList *children = g_slist_copy (group->priv->children);
	GSList *child;

	for (child = children; child; child = g_slist_next (child))
	{
		cpg_group_remove (group, child->data, NULL);
	}

	g_slist_free (children);

	CPG_OBJECT_CLASS (cpg_group_parent_class)->clear (object);
}

static gboolean
cpg_group_cpg_equal (CpgObject *first,
                     CpgObject *second)
{
	if (!CPG_OBJECT_CLASS (cpg_group_parent_class)->equal (first, second))
	{
		return FALSE;
	}

	/* Same proxies */
	CpgGroup *group1 = CPG_GROUP (first);
	CpgGroup *group2 = CPG_GROUP (second);

	if ((group1->priv->proxy == NULL && group2->priv->proxy != NULL) ||
	    (group2->priv->proxy == NULL && group1->priv->proxy != NULL))
	{
		return FALSE;
	}

	if (group1->priv->proxy &&
	    g_strcmp0 (cpg_object_get_id (group1->priv->proxy),
	               cpg_object_get_id (group2->priv->proxy)) != 0)
	{
		return FALSE;
	}

	GSList const *children1 = cpg_group_get_children (group1);
	GSList const *children2 = cpg_group_get_children (group2);

	if (g_slist_length ((GSList *)children1) != g_slist_length ((GSList *)children2))
	{
		return FALSE;
	}

	while (children1)
	{
		CpgObject *child1 = children1->data;
		CpgObject *child2 = cpg_group_get_child (group2,
		                                         cpg_object_get_id (child1));

		if (!child2 || !cpg_object_equal (child1, child2))
		{
			return FALSE;
		}

		children1 = g_slist_next (children1);
	}

	return TRUE;
}

static GSList const *
cpg_group_get_children_impl (CpgGroup *group)
{
	return group->priv->children;
}

static gboolean
cpg_group_verify_remove_child_impl (CpgGroup   *group,
                                    CpgObject  *object,
                                    GError    **error)
{
	/* TODO: Check if there are any objects that use the child as a template
	   while this template is not inherited from applying a template to
	   the parent of the child */
	return TRUE;
}

static void
cpg_group_cpg_taint (CpgObject *object)
{
	CpgGroup *group;

	group = CPG_GROUP (object);

	g_slist_free (group->priv->actors);
	group->priv->actors = NULL;

	CPG_OBJECT_CLASS (cpg_group_parent_class)->taint (object);
}

static void
cpg_group_class_init (CpgGroupClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	object_class->finalize = cpg_group_finalize;
	object_class->dispose = cpg_group_dispose;

	object_class->get_property = cpg_group_get_property;
	object_class->set_property = cpg_group_set_property;

	cpg_class->get_property = cpg_group_cpg_get_property;

	cpg_class->compile = cpg_group_cpg_compile;
	cpg_class->get_compile_context = cpg_group_cpg_get_compile_context;
	cpg_class->reset = cpg_group_cpg_reset;
	cpg_class->foreach_expression = cpg_group_cpg_foreach_expression;
	cpg_class->clear = cpg_group_cpg_clear;
	cpg_class->equal = cpg_group_cpg_equal;
	cpg_class->taint = cpg_group_cpg_taint;

	cpg_class->copy = cpg_group_cpg_copy;
	cpg_class->apply_template = cpg_group_cpg_apply_template;
	cpg_class->unapply_template = cpg_group_cpg_unapply_template;

	klass->add = cpg_group_add_impl;
	klass->remove = cpg_group_remove_impl;
	klass->get_children = cpg_group_get_children_impl;
	klass->verify_remove_child = cpg_group_verify_remove_child_impl;

	g_type_class_add_private (object_class, sizeof(CpgGroupPrivate));

	/**
	 * CpgGroup:proxy:
	 *
	 * The group proxy object
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_PROXY,
	                                 g_param_spec_object ("proxy",
	                                                      "Proxy",
	                                                      "Proxy",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	/**
	 * CpgGroup::child-added:
	 * @object: a #CpgObject
	 * @action: the added #CpgObject
	 *
	 * Emitted when a child object is added to the group
	 *
	 **/
	group_signals[CHILD_ADDED] =
		g_signal_new ("child-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgGroupClass,
		                               child_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_OBJECT);

	/**
	 * CpgGroup::child-removed:
	 * @object: a #CpgObject
	 * @action: the removed #CpgObject
	 *
	 * Emitted when a child object is removed from the group
	 *
	 **/
	group_signals[CHILD_REMOVED] =
		g_signal_new ("child-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgGroupClass,
		                               child_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_OBJECT);

	/**
	 * CpgGroup::verify-remove-child:
	 * @object: a #CpgObject
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
		              G_STRUCT_OFFSET (CpgGroupClass,
		                               verify_remove_child),
		              cpg_signal_accumulator_false_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__OBJECT_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              CPG_TYPE_OBJECT,
		              G_TYPE_POINTER);


	/**
	 * CpgGroup::verify-add-child:
	 * @object: a #CpgObject
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
		              cpg_signal_accumulator_false_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__OBJECT_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              CPG_TYPE_OBJECT,
		              G_TYPE_POINTER);
}

static void
on_property_interface_changed (CpgObject *object)
{
	cpg_object_taint (object);
}

static gboolean
on_property_interface_verify_remove (CpgPropertyInterface  *iface,
                                     gchar const           *name,
                                     gchar const           *child_name,
                                     gchar const           *property_name,
                                     GError               **error,
                                     CpgGroup              *group)
{
	/* If it comes from a proxy, we deny */
	if (interface_is_from_proxy (group, name))
	{
		g_set_error (error,
		             CPG_GROUP_ERROR,
		             CPG_GROUP_ERROR_INTERFACE_IS_PROXY,
		             "The interface `%s' is automatically generated from the proxy `%s' and cannot be removed manually",
		             name,
		             cpg_object_get_id (group->priv->proxy));

		return FALSE;
	}

	return TRUE;
}

static void
cpg_group_init (CpgGroup *self)
{
	self->priv = CPG_GROUP_GET_PRIVATE (self);

	self->priv->child_hash = g_hash_table_new_full (g_str_hash,
	                                                g_str_equal,
	                                                (GDestroyNotify)g_free,
	                                                NULL);

	self->priv->property_interface = cpg_property_interface_new (self);

	g_signal_connect_swapped (self->priv->property_interface,
	                          "added",
	                          G_CALLBACK (on_property_interface_changed),
	                          self);

	g_signal_connect_swapped (self->priv->property_interface,
	                          "removed",
	                          G_CALLBACK (on_property_interface_changed),
	                          self);

	g_signal_connect (self->priv->property_interface,
	                  "verify-remove",
	                  G_CALLBACK (on_property_interface_verify_remove),
	                  self);
}

/**
 * cpg_group_new:
 * @id: the group id
 * @proxy: A #CpgObject
 *
 * Create a new group.
 *
 * Returns: A #CpgGroup
 *
 **/
CpgGroup *
cpg_group_new (gchar const *id,
               CpgObject   *proxy)
{
	return g_object_new (CPG_TYPE_GROUP,
	                     "id", id,
	                     "proxy", proxy,
	                     NULL);
}

/**
 * cpg_group_get_children:
 * @group: A #CpgGroup
 *
 * Get a list of the children in @group.
 *
 * Returns: (element-type CpgObject) (transfer none): A #GSList of #CpgObject
 *
 **/
const GSList *
cpg_group_get_children (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	return CPG_GROUP_GET_CLASS (group)->get_children (group);
}

/**
 * cpg_group_add:
 * @group: A #CpgGroup
 * @object: A #CpgObject
 *
 * Add a child object to the group.
 *
 * Returns: %TRUE if the child could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cpg_group_add (CpgGroup   *group,
               CpgObject  *object,
               GError    **error)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_GROUP_GET_CLASS (group)->add)
	{
		return CPG_GROUP_GET_CLASS (group)->add (group, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_group_remove:
 * @group: A #CpgGroup
 * @object: A #CpgObject
 * @error: A #GError
 *
 * Remove a child object from the group.
 *
 * Returns: %TRUE if the child was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cpg_group_remove (CpgGroup   *group,
                  CpgObject  *object,
                  GError    **error)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_GROUP_GET_CLASS (group)->remove)
	{
		return CPG_GROUP_GET_CLASS (group)->remove (group, object, error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_group_set_proxy:
 * @group: A #CpgGroup
 * @proxy: A #CpgObject
 *
 * Set the proxy object of @group to @proxy.
 *
 * Returns: %TRUE if the proxy could be successfully changed, %FALSE otherwise
 *
 **/
gboolean
cpg_group_set_proxy (CpgGroup  *group,
                     CpgObject *proxy)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (proxy == NULL || CPG_IS_OBJECT (proxy), FALSE);

	return set_proxy (group, proxy);
}

/**
 * cpg_group_get_proxy:
 * @group: A #CpgGroup
 *
 * Get the proxy object of @group.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_group_get_proxy (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	return group->priv->proxy;
}

/**
 * cpg_group_foreach:
 * @group: A #CpgGroup
 * @func: (scope call): A #GFunc
 * @data: User data
 *
 * Call @func for each child object in the group.
 *
 **/
void
cpg_group_foreach (CpgGroup *group,
                   GFunc     func,
                   gpointer  data)
{
	g_return_if_fail (CPG_IS_GROUP (group));

	g_slist_foreach ((GSList *)cpg_group_get_children (group), func, data);
}

/**
 * cpg_group_get_child:
 * @group: A #CpgGroup
 * @name: The child name
 *
 * Get a child from the group by name.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_group_get_child (CpgGroup    *group,
                     const gchar *name)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	return g_hash_table_lookup (group->priv->child_hash,
	                            name);
}

/**
 * cpg_group_find_objects:
 * @group: A #CpgGroup
 * @path: The object path
 *
 * Find objects by specifying a path. For example, if there is
 * another group "g" containing a state "s", you can use
 * cpg_group_find_object (group, "g.s") to get the object.
 *
 * Returns: (transfer container) (element-type CpgObject): A #CpgObject
 *
 **/
GSList *
cpg_group_find_objects (CpgGroup    *group,
                        const gchar *selector)
{
	CpgSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cpg_selector_parse (CPG_OBJECT (group), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);

		return NULL;
	}

	all = cpg_selector_select (sel,
	                           G_OBJECT (group),
	                           CPG_SELECTOR_TYPE_OBJECT,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_get_object (all->data));

		g_object_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cpg_group_find_object:
 * @group: A #CpgGroup
 * @path: The object path
 *
 * Find object by specifying a path. For example, if there is
 * another group "g" containing a state "s", you can use
 * cpg_group_find_object (group, "g.s") to get the object.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_group_find_object (CpgGroup    *group,
                       const gchar *selector)
{
	GSList *all;
	CpgObject *ret = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cpg_group_find_objects (group, selector);

	if (all)
	{
		ret =  all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cpg_group_find_properties:
 * @group: A #CpgGroup
 * @path: The object path
 *
 * Find properties by specifying a path. For example, if there is
 * another group "g" containing a state "s" with property "x", you can use
 * cpg_group_find_properties (group, "g.s.x") to get the property.
 *
 * Returns: (transfer container) (element-type CpgProperty): A list of #CpgProperty
 *
 **/
GSList *
cpg_group_find_properties (CpgGroup    *group,
                           gchar const *selector)
{
	CpgSelector *sel;
	GSList *all;
	GSList *ret = NULL;
	GError *error = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cpg_selector_parse (CPG_OBJECT (group), selector, &error);

	if (!sel)
	{
		g_warning ("Failed to parse selector: %s", error->message);
		g_error_free (error);
		return NULL;
	}

	all = cpg_selector_select (sel,
	                           G_OBJECT (group),
	                           CPG_SELECTOR_TYPE_PROPERTY,
	                           NULL);

	while (all)
	{
		ret = g_slist_prepend (ret,
		                       cpg_selection_get_object (all->data));

		g_object_unref (all->data);
		all = g_slist_delete_link (all, all);
	}

	g_object_unref (sel);

	return g_slist_reverse (ret);
}

/**
 * cpg_group_find_property:
 * @group: A #CpgGroup
 * @path: The property path
 *
 * Find a property by specifying an object path. For example, if there is
 * another group "g" containing a state "s" with a property "x", you can use
 * cpg_group_find_property (group, "g.s.x") to get the property.
 *
 * Returns: (transfer none): A #CpgProperty
 *
 **/
CpgProperty *
cpg_group_find_property (CpgGroup    *group,
                         gchar const *selector)
{
	GSList *all;
	CpgProperty *ret = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	all = cpg_group_find_properties (group, selector);

	if (all)
	{
		ret = all->data;
	}

	g_slist_free (all);

	return ret;
}

/**
 * cpg_group_get_property_interface:
 * @group: A #CpgGroup
 *
 * Get the property interface of the group.
 *
 * Returns: (transfer none): A #CpgPropertyInterface
 *
 **/
CpgPropertyInterface *
cpg_group_get_property_interface (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	return group->priv->property_interface;
}

/**
 * cpg_group_get_auto_templates_for_child:
 * @group: A #CpgGroup
 * @child: A #CpgObject
 *
 * Get the templates that were automatically applied from the group to the
 * child.
 *
 * Returns: (transfer container) (allow-none): A #GSList of #CpgObject
 *
 **/
GSList *
cpg_group_get_auto_templates_for_child (CpgGroup  *group,
                                        CpgObject *child)
{
	GSList const *templates;
	GSList *ret = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (child), NULL);

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	while (templates)
	{
		if (CPG_IS_GROUP (templates->data))
		{
			CpgGroup *grp = templates->data;
			CpgObject *tchild;

			tchild = cpg_group_get_child (grp,
			                              cpg_object_get_id (child));

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
 * _cpg_object_link:
 * @object: the #CpgObject
 * @link: the #CpgLink which links to this object
 *
 * Adds @link as a link which targets the object.
 *
 **/
void
_cpg_group_link (CpgGroup *group,
                 CpgLink  *link)
{
	g_return_if_fail (CPG_IS_GROUP (group));
	g_return_if_fail (CPG_IS_LINK (link));

	group->priv->links = g_slist_append (group->priv->links, link);

	g_slist_free (group->priv->actors);
	group->priv->actors = NULL;

	g_object_add_toggle_ref (G_OBJECT (link),
	                         (GToggleNotify)link_destroyed,
	                         group);
}

/**
 * _cpg_group_unlink:
 * @group: the #CpgObject
 * @link: the #CpgLink which unlinks from this group
 *
 * Removes @link as a link which targets the group.
 *
 **/
void
_cpg_group_unlink (CpgGroup *group,
                   CpgLink  *link)
{
	g_return_if_fail (CPG_IS_GROUP (group));
	g_return_if_fail (CPG_IS_LINK (link));

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
 * cpg_group_get_actors:
 * @group: A #CpgGroup
 *
 * Get the properties which are acted upon by links.
 *
 * Returns: (element-type CpgProperty) (transfer none): A #GSList of #CpgProperty.
 *
 **/
GSList const *
cpg_group_get_actors (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	if (group->priv->actors != NULL)
	{
		return group->priv->actors;
	}

	GSList *ret = NULL;
	GSList *item;

	for (item = group->priv->links; item; item = g_slist_next (item))
	{
		GSList const *actions;

		actions = cpg_link_get_actions (CPG_LINK (item->data));

		while (actions)
		{
			CpgLinkAction *a = actions->data;
			CpgProperty *target = cpg_link_action_get_target_property (a);

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
 * cpg_group_get_links:
 * @group: A #CpgGroup
 *
 * Get a list of links that act on this object.
 *
 * Returns: (element-type CpgLink): A list of #CpgLink
 *
 */
GSList const *
cpg_group_get_links (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	return group->priv->links;
}

gboolean
cpg_group_has_self_link (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);

	return group->priv->self_link != NULL;
}


CpgLinkForward *
cpg_group_get_self_link (CpgGroup *group)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);

	if (!group->priv->self_link)
	{
		group->priv->self_link = cpg_link_new ("integrate",
		                                       group,
		                                       group);

		group->priv->links = g_slist_prepend (group->priv->links,
		                                      group->priv->self_link);
	}

	return group->priv->self_link;
}
