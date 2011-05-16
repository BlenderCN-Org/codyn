#include "cpg-group.h"
#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-compile-context.h"
#include "cpg-marshal.h"
#include "cpg-utils.h"
#include "cpg-selector.h"

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
	NUM_EXT_SIGNALS
};

struct _CpgGroupPrivate
{
	CpgObject *proxy;

	GSList *children;
	GHashTable *child_hash;

	CpgPropertyInterface *property_interface;

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
cpg_group_finalize (GObject *object)
{
	CpgGroup *group = CPG_GROUP (object);

	g_hash_table_destroy (group->priv->child_hash);

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

static void
on_template_child_added (CpgGroup  *templ,
                         CpgObject *child,
                         CpgGroup  *group)
{
	CpgObject *obj;

	obj = cpg_group_get_child (group, cpg_object_get_id (child));

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

		cpg_object_unapply_template (obj, child, NULL);

		properties = cpg_object_get_properties (obj);

		if (cpg_object_get_applied_templates (obj) == NULL &&
		    properties == NULL && cpg_object_get_links (obj) == NULL)
		{
			cpg_group_remove (templ, child, NULL);
		}

		g_slist_free (properties);
	}
}

static gboolean
interface_should_override (CpgGroup     *group,
                           CpgGroup     *template,
                           gchar const  *name,
                           CpgGroup    **last_templ)
{
	GSList const *templates;
	gboolean found = FALSE;

	/* Check if it's not automatically generated from the proxy, we don't
	   override that */
	if (group->priv->proxy)
	{
		CpgProperty *prop;

		prop = cpg_property_interface_lookup (group->priv->property_interface,
		                                      name);

		if (prop && cpg_property_get_object (prop) == group->priv->proxy)
		{
			return FALSE;
		}
	}

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	while (templates)
	{
		CpgGroup *tmpl;
		CpgPropertyInterface *iface;
		CpgProperty *templprop;

		tmpl = templates->data;

		iface = cpg_group_get_property_interface (tmpl);

		templprop = cpg_property_interface_lookup (iface, name);

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

		templates = g_slist_next (templates);
	}

	return TRUE;
}

static void
add_template_interface (CpgGroup    *group,
                        CpgGroup    *source,
                        gchar const *name,
                        CpgProperty *property)
{
	gchar *path;
	gchar *proppath;
	CpgPropertyInterface *iface;

	iface = cpg_group_get_property_interface (group);

	if (property == NULL)
	{
		CpgPropertyInterface *source_iface;

		source_iface = cpg_group_get_property_interface (source);
		property = cpg_property_interface_lookup (source_iface, name);
	}

	path = cpg_object_get_relative_id (cpg_property_get_object (property),
	                                   CPG_OBJECT (source));

	if (path && *path)
	{
		proppath = g_strconcat (path, ".", cpg_property_get_name (property), NULL);
	}
	else
	{
		proppath = g_strdup (cpg_property_get_name (property));
	}

	property = cpg_group_find_property (group, proppath);

	if (property)
	{
		CpgProperty *origprop;

		origprop = cpg_property_interface_lookup (iface, name);

		if (origprop)
		{
			/* See if this template should override this interface */
			if (interface_should_override (group, source, name, NULL))
			{
				cpg_property_interface_remove (iface, name, NULL);
				cpg_property_interface_add (iface, name, property, NULL);
			}
		}
		else
		{
			cpg_property_interface_add (iface, name, property, NULL);
		}
	}
	else
	{
		g_warning ("Could not find interface property `%s' for applied template `%s'",
		           proppath,
		           cpg_object_get_id (CPG_OBJECT (source)));
	}

	g_free (path);
	g_free (proppath);
}

static void
remove_template_interface (CpgGroup    *group,
                           CpgGroup    *source,
                           gchar const *name,
                           CpgProperty *property)
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
		add_template_interface (group, last, name, NULL);
	}
}

static void
on_template_interface_property_added (CpgPropertyInterface *templ_iface,
                                      gchar const          *name,
                                      CpgProperty          *property,
                                      CpgGroup             *group)
{
	add_template_interface (group,
	                        CPG_GROUP (cpg_property_interface_get_object (templ_iface)),
	                        name,
	                        property);
}

static void
on_template_interface_property_removed (CpgPropertyInterface *templ_iface,
                                        gchar const          *name,
                                        CpgProperty          *property,
                                        CpgGroup             *group)
{
	remove_template_interface (group,
	                           CPG_GROUP (cpg_property_interface_get_object (templ_iface)),
	                           name,
	                           property);
}

static void
disconnect_template (CpgGroup  *group,
                     CpgObject *templ)
{
	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_interface_property_added,
	                                      group);

	g_signal_handlers_disconnect_by_func (templ,
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

	GSList const *templates = cpg_object_get_applied_templates (CPG_OBJECT (object));

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

	G_OBJECT_CLASS (cpg_group_parent_class)->dispose (object);
}

static void
proxy_add_property (CpgGroup    *group,
                    CpgProperty *property)
{
	gchar const *name;

	name = cpg_property_get_name (property);

	if (cpg_property_interface_lookup (group->priv->property_interface,
	                                   name))
	{
		cpg_property_interface_remove (group->priv->property_interface,
		                               name,
		                               NULL);
	}

	cpg_property_interface_add (group->priv->property_interface,
	                            name,
	                            property,
	                            NULL);
}

static void
add_interface_after_proxy_remove (CpgGroup    *group,
                                  gchar const *name)
{
	GSList const *templates;
	CpgProperty *last = NULL;
	CpgGroup *templ = NULL;

	templates = cpg_object_get_applied_templates (CPG_OBJECT (group));

	while (templates)
	{
		CpgObject *temp = templates->data;

		if (CPG_IS_GROUP (temp))
		{
			CpgPropertyInterface *iface;
			CpgProperty *look;

			iface = cpg_group_get_property_interface (CPG_GROUP (temp));

			look = cpg_property_interface_lookup (iface, name);

			if (look)
			{
				templ = CPG_GROUP (temp);
				last = look;
			}
		}

		templates = g_slist_next (templates);
	}

	if (last != NULL)
	{
		add_template_interface (group,
		                        templ,
		                        name,
		                        last);
	}
}

static void
proxy_remove_property (CpgGroup    *group,
                       CpgProperty *property)
{
	CpgProperty *prop;
	gchar const *name;

	name = cpg_property_get_name (property);

	prop = cpg_property_interface_lookup (group->priv->property_interface,
	                                      name);

	if (prop == property)
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
	proxy_add_property (group, property);
}

static void
on_proxy_property_removed (CpgObject   *proxy,
                           CpgProperty *property,
                           CpgGroup    *group)
{
	proxy_remove_property (group, property);
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

		GSList const *properties = cpg_object_get_properties (group->priv->proxy);

		CpgObject *pr = group->priv->proxy;
		group->priv->proxy = NULL;

		/* Remove automatically mapped properties from interface */
		while (properties)
		{
			proxy_remove_property (group, properties->data);

			properties = g_slist_next (properties);
		}

		g_object_unref (pr);
	}

	if (proxy)
	{
		group->priv->proxy = g_object_ref (proxy);
		cpg_group_add (group, proxy, NULL);

		GSList const *properties = cpg_object_get_properties (group->priv->proxy);

		while (properties)
		{
			proxy_add_property (group, properties->data);

			properties = g_slist_next (properties);
		}

		group->priv->proxy_signals[EXT_PROPERTY_ADDED] =
			g_signal_connect (group->priv->proxy,
			                  "property-added",
			                  G_CALLBACK (on_proxy_property_added),
			                  group);

		group->priv->proxy_signals[EXT_PROPERTY_REMOVED] =
			g_signal_connect (group->priv->proxy,
			                  "property-removed",
			                  G_CALLBACK (on_proxy_property_removed),
			                  group);
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

static gboolean
cpg_group_cpg_compile (CpgObject         *object,
                       CpgCompileContext *context,
                       CpgCompileError   *error)
{
	/* And then also the children! */
	CpgGroup *group = CPG_GROUP (object);
	GSList *item = group->priv->children;

	while (item)
	{
		if (!cpg_object_compile (item->data,
		                         context,
		                         error))
		{
			return FALSE;
		}

		item = g_slist_next (item);
	}

	if (!CPG_OBJECT_CLASS (cpg_group_parent_class)->compile (object,
	                                                         context,
	                                                         error))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cpg_group_cpg_reset (CpgObject *object)
{
	CPG_OBJECT_CLASS (cpg_group_parent_class)->reset (object);

	/* And then also the children! */
	cpg_group_foreach (CPG_GROUP (object),
	                   (GFunc)cpg_object_reset,
	                   NULL);
}

static void
cpg_group_cpg_foreach_expression (CpgObject                *object,
                                  CpgForeachExpressionFunc  func,
                                  gpointer                  userdata)
{
	if (CPG_OBJECT_CLASS (cpg_group_parent_class)->foreach_expression)
	{
		CPG_OBJECT_CLASS (cpg_group_parent_class)->foreach_expression (object,
		                                                               func,
		                                                               userdata);
	}

	/* And then also the children! */
	GSList *item;

	for (item = CPG_GROUP (object)->priv->children; item; item = g_slist_next (item))
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

			CpgObject *copied_from;
			CpgObject *copied_to;

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
		CpgProperty *prop;
		gchar *path;
		gchar *proppath;

		prop = cpg_property_interface_lookup (source_iface, *ptr);

		path = cpg_object_get_relative_id (cpg_property_get_object (prop),
		                                   CPG_OBJECT (source));

		if (path && *path)
		{
			proppath = g_strconcat (path, ".", cpg_property_get_name (prop), NULL);
		}
		else
		{
			proppath = g_strdup (cpg_property_get_name (prop));
		}

		prop = cpg_group_find_property (group, proppath);

		if (prop != NULL)
		{
			GError *error = NULL;

			if (!cpg_property_interface_add (iface,
			                                 *ptr,
			                                 prop,
			                                 &error))
			{
				g_warning ("Could not copy interface property: %s",
				           error->message);

				g_error_free (error);
			}
		}

		g_free (path);
		g_free (proppath);
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
			g_slist_free (templates);
			return item->data;
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
	/* TODO */

	/* Remove interface */
	source_iface = cpg_group_get_property_interface (source);

	names = cpg_property_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		CpgProperty *prop;

		prop = cpg_property_interface_lookup (source_iface, *ptr);

		remove_template_interface (group, source, *ptr, prop);
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
	GHashTable *hash_table;
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

	hash_table = g_hash_table_new (g_direct_hash,
	                               g_direct_equal);

	/* Copy in children */
	while (children)
	{
		CpgObject *child = children->data;

		/* Check to find existing one */
		CpgObject *new_child;

		new_child = cpg_group_get_child (group,
		                                 cpg_object_get_id (child));

		if (new_child)
		{
			if (!cpg_object_apply_template (new_child,
			                                child,
			                                error))
			{
				/* TODO: make atomic */
				return FALSE;
			}
		}
		else
		{
			new_child = cpg_object_new_from_template (child, error);

			if (!new_child)
			{
				/* TODO: make atomic */
				return FALSE;
			}

			cpg_group_add (group, new_child, NULL);
			g_object_unref (new_child);
		}

		/* Store map from the original to the copy */
		g_hash_table_insert (hash_table, child, new_child);

		if (child == proxy)
		{
			if (group->priv->proxy == NULL ||
			    get_template_proxy (group))
			{
				set_proxy (group, new_child);
			}
		}

		children = g_slist_next (children);
	}

	reconnect_children (group, source, hash_table);
	g_hash_table_destroy (hash_table);

	/* Apply interfaces from template */
	source_iface = cpg_group_get_property_interface (source);

	names = cpg_property_interface_get_names (source_iface);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		CpgProperty *prop;

		prop = cpg_property_interface_lookup (source_iface, *ptr);

		add_template_interface (group, source, *ptr, prop);
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
	CPG_OBJECT_CLASS (cpg_group_parent_class)->copy (object, source);

	/* Copy over children */
	copy_children (CPG_GROUP (object), CPG_GROUP (source));

	/* Copy over interface */
	copy_interface (CPG_GROUP (object), CPG_GROUP (source));
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
	CpgObject *other = g_hash_table_lookup (group->priv->child_hash,
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

	_cpg_object_set_parent (object, CPG_OBJECT (group));

	cpg_object_taint (CPG_OBJECT (group));
	g_signal_emit (group, group_signals[CHILD_ADDED], 0, object);

	return TRUE;
}

static void
remove_object (CpgGroup  *group,
               CpgObject *object)
{
	unregister_object (group, object);

	if (cpg_object_get_parent (object) == CPG_OBJECT (group))
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
	GSList *item = g_slist_find (group->priv->children, object);

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
	cpg_class->reset = cpg_group_cpg_reset;
	cpg_class->foreach_expression = cpg_group_cpg_foreach_expression;
	cpg_class->clear = cpg_group_cpg_clear;
	cpg_class->equal = cpg_group_cpg_equal;

	cpg_class->copy = cpg_group_cpg_copy;
	cpg_class->apply_template = cpg_group_cpg_apply_template;
	cpg_class->unapply_template = cpg_group_cpg_unapply_template;

	klass->add = cpg_group_add_impl;
	klass->remove = cpg_group_remove_impl;
	klass->get_children = cpg_group_get_children_impl;

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
		              0,
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
                                     GError               **error,
                                     CpgGroup              *group)
{
	/* If it comes from a proxy, we deny */
	CpgProperty *property;

	property = cpg_property_interface_lookup (iface, name);

	if (property &&
	    cpg_property_get_object (property) == group->priv->proxy &&
	    g_strcmp0 (cpg_property_get_name (property), name) == 0)
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

	self->priv->property_interface = cpg_property_interface_new (CPG_OBJECT (self));

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
 * cpg_group_find_object:
 * @group: A #CpgGroup
 * @path: The object path
 *
 * Find an object by specifying a path. For example, if there is
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
	CpgSelector *sel;
	GSList *all;
	CpgObject *ret = NULL;

	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (selector != NULL, NULL);

	sel = cpg_selector_parse (selector);
	all = cpg_selector_select (sel, CPG_OBJECT (group));

	if (all)
	{
		ret = all->data;
	}

	g_slist_free (all);
	g_object_unref (sel);

	return ret;
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
                         gchar const *path)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (path != NULL, NULL);

	gchar *copy = g_strdup (path);
	gchar *ptr = g_utf8_strrchr (copy, -1, '.');

	if (!ptr)
	{
		g_free (copy);
		return NULL;
	}

	*ptr = '\0';
	CpgObject *object;

	object = cpg_group_find_object (group, copy);

	if (!object)
	{
		g_free (copy);
		return NULL;
	}

	CpgProperty *ret = cpg_object_get_property (object, ptr + 1);
	g_free (copy);

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
