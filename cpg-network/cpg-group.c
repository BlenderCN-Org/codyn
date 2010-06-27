#include "cpg-group.h"
#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-compile-context.h"

/**
 * SECTION:cpg-group
 * @short_description: Group object grouping many objects together
 *
 * The #CpgGroup is a special #CpgObject that contains many objects as children.
 * This can be used make sub-networks that can be easily instantiated,
 * providing some common functionality.
 *
 * Each group can have a proxy object associated which serves as a bridge
 * between the outside and the inside of the group.
 *
 */

#define CPG_GROUP_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_GROUP, CpgGroupPrivate))

struct _CpgGroupPrivate
{
	CpgObject *proxy;

	GSList *children;
	GHashTable *child_hash;
};

G_DEFINE_TYPE (CpgGroup, cpg_group, CPG_TYPE_STATE)

enum
{
	PROP_0,
	PROP_PROXY
};

enum
{
	CHILD_ADDED,
	CHILD_REMOVED,
	NUM_SIGNALS
};

static guint group_signals[NUM_SIGNALS] = {0,};

static void remove_object (CpgGroup *group, CpgObject *object);

static void
cpg_group_finalize (GObject *object)
{
	CpgGroup *group = CPG_GROUP (object);

	g_hash_table_destroy (group->priv->child_hash);

	G_OBJECT_CLASS (cpg_group_parent_class)->finalize (object);
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

	G_OBJECT_CLASS (cpg_group_parent_class)->dispose (object);
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
		GSList const *actors = cpg_object_get_actors (CPG_OBJECT (group));

		// Check if there are still actors on this proxy
		while (actors)
		{
			CpgProperty *prop = actors->data;

			if (cpg_property_get_object (prop) == group->priv->proxy)
			{
				return FALSE;
			}
		}

		CpgObject *pr = group->priv->proxy;
		group->priv->proxy = NULL;

		cpg_group_remove (group, pr);
		g_object_unref (pr);
	}

	if (proxy)
	{
		group->priv->proxy = g_object_ref (proxy);
		cpg_group_add (group, proxy);
	}

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

	if (!prop && group->priv->proxy)
	{
		prop = cpg_object_get_property (group->priv->proxy,
		                                name);
	}

	return prop;
}

static GSList *
cpg_group_cpg_get_properties (CpgObject *object)
{
	CpgGroup *group = CPG_GROUP (object);
	GSList *properties;

	properties = CPG_OBJECT_CLASS (cpg_group_parent_class)->get_properties (object);

	if (group->priv->proxy)
	{
		properties = g_slist_concat (properties,
		                             cpg_object_get_properties (group->priv->proxy));
	}

	return properties;

}

static gboolean
cpg_group_cpg_remove_property (CpgObject    *object,
                               gchar const  *name,
                               GError      **error)
{
	CpgGroup *group = CPG_GROUP (object);
	CpgProperty *prop;

	prop = CPG_OBJECT_CLASS (cpg_group_parent_class)->get_property (object,
	                                                                name);

	if (prop)
	{
		return CPG_OBJECT_CLASS (cpg_group_parent_class)->remove_property (object,
		                                                                   name,
		                                                                   error);
	}
	else if (group->priv->proxy)
	{
		return cpg_object_remove_property (group->priv->proxy,
		                                   name,
		                                   error);
	}

	return FALSE;
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
cpg_group_cpg_reset_cache (CpgObject *object)
{
	CPG_OBJECT_CLASS (cpg_group_parent_class)->reset_cache (object);

	/* And then also the children! */
	cpg_group_foreach (CPG_GROUP (object),
	                   (GFunc)cpg_object_reset_cache,
	                   NULL);
}

static void
copy_children (CpgGroup *group,
               CpgGroup *source)
{
	CpgObject *proxy = cpg_group_get_proxy (source);
	GSList const *children = cpg_group_get_children (source);

	while (children)
	{
		CpgObject *child = children->data;
		CpgObject *copied = _cpg_object_copy (child);

		cpg_group_add (group, copied);

		if (child == proxy)
		{
			group->priv->proxy = g_object_ref (copied);
		}

		children = g_slist_next (children);
	}
}

static void
cpg_group_cpg_apply_template (CpgObject *object,
                              CpgObject *templ)
{
	CPG_OBJECT_CLASS (cpg_group_parent_class)->apply_template (object, templ);

	if (CPG_IS_GROUP (templ))
	{
		copy_children (CPG_GROUP (object), CPG_GROUP (templ));
	}
}

static void
cpg_group_cpg_copy (CpgObject *object,
                    CpgObject *source)
{
	CPG_OBJECT_CLASS (cpg_group_parent_class)->copy (object, source);

	/* Copy over children */
	copy_children (CPG_GROUP (object), CPG_GROUP (source));
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
		newid = g_strdup_printf ("%s (%d)", id, ++cnt);
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
cpg_group_add_impl (CpgGroup  *group,
                    CpgObject *object)
{
	CpgObject *other = g_hash_table_lookup (group->priv->child_hash,
	                                        cpg_object_get_id (object));

	if (other == object)
	{
		return FALSE;
	}

	group->priv->children = g_slist_append (group->priv->children,
	                                        g_object_ref (object));

	register_object (group, object);

	if (CPG_IS_LINK (object))
	{
		_cpg_link_resolve_actions (CPG_LINK (object));
	}

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

static gboolean
cpg_group_remove_impl (CpgGroup  *group,
                       CpgObject *object)
{
	if (object == group->priv->proxy)
	{
		return set_proxy (group, NULL);
	}

	GSList *item = g_slist_find (group->priv->children, object);

	if (item)
	{
		group->priv->children = g_slist_remove_link (group->priv->children,
		                                             item);

		remove_object (group, object);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
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
		cpg_group_remove (group, child->data);
	}

	g_slist_free (children);

	CPG_OBJECT_CLASS (cpg_group_parent_class)->clear (object);
}

static CpgProperty *
cpg_group_cpg_add_property (CpgObject        *object,
                            gchar const      *name,
                            gchar const      *expression,
                            CpgPropertyFlags  flags)
{
	CpgGroup *group = CPG_GROUP (object);
	CpgProperty *property;

	property = cpg_object_get_property (object, name);

	if (property && cpg_property_get_object (property) != object)
	{
		CpgObject *owner = cpg_property_get_object (property);

		if (owner == group->priv->proxy)
		{
			return cpg_object_add_property (group->priv->proxy,
			                                name,
			                                expression,
			                                flags);
		}
	}

	if (CPG_OBJECT_CLASS (cpg_group_parent_class)->add_property)
	{
		return CPG_OBJECT_CLASS (cpg_group_parent_class)->add_property (object,
		                                                                name,
		                                                                expression,
		                                                                flags);
	}
	else
	{
		return NULL;
	}
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

	cpg_class->add_property = cpg_group_cpg_add_property;
	cpg_class->get_property = cpg_group_cpg_get_property;
	cpg_class->get_properties = cpg_group_cpg_get_properties;
	cpg_class->remove_property = cpg_group_cpg_remove_property;

	cpg_class->compile = cpg_group_cpg_compile;
	cpg_class->reset = cpg_group_cpg_reset;
	cpg_class->reset_cache = cpg_group_cpg_reset_cache;
	cpg_class->clear = cpg_group_cpg_clear;
	cpg_class->equal = cpg_group_cpg_equal;

	cpg_class->copy = cpg_group_cpg_copy;
	cpg_class->apply_template = cpg_group_cpg_apply_template;

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
}

static void
cpg_group_init (CpgGroup *self)
{
	self->priv = CPG_GROUP_GET_PRIVATE (self);

	self->priv->child_hash = g_hash_table_new_full (g_str_hash,
	                                                g_str_equal,
	                                                (GDestroyNotify)g_free,
	                                                NULL);
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
 * Returns: A #GSList of #CpgObject
 *
 **/
GSList const *
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
cpg_group_add (CpgGroup  *group,
               CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_GROUP_GET_CLASS (group)->add)
	{
		return CPG_GROUP_GET_CLASS (group)->add (group, object);
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
 *
 * Remove a child object from the group.
 *
 * Returns: %TRUE if the child was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cpg_group_remove (CpgGroup  *group,
                  CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_GROUP_GET_CLASS (group)->remove)
	{
		return CPG_GROUP_GET_CLASS (group)->remove (group, object);
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
	g_return_val_if_fail (CPG_IS_OBJECT (proxy), FALSE);

	return set_proxy (group, proxy);
}

/**
 * cpg_group_get_proxy:
 * @group: A #CpgGroup
 *
 * Get the proxy object of @group.
 *
 * Returns: A #CpgObject
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
 * @func: A #GFunc
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
 * Returns: A #CpgObject
 *
 **/
CpgObject *
cpg_group_get_child (CpgGroup    *group,
                     gchar const *name)
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
 * Returns: A #CpgObject
 *
 **/
CpgObject *
cpg_group_find_object (CpgGroup    *group,
                       gchar const *path)
{
	g_return_val_if_fail (CPG_IS_GROUP (group), NULL);
	g_return_val_if_fail (path != NULL, NULL);

	gchar **parts = g_strsplit (path, ".", -1);
	gchar **ptr = parts;

	CpgObject *ret = NULL;
	CpgGroup *parent = group;

	while (ptr && *ptr)
	{
		if (!parent)
		{
			ret = NULL;
			break;
		}

		ret = cpg_group_get_child (parent, *ptr);

		if (!ret)
		{
			break;
		}

		if (CPG_IS_GROUP (ret))
		{
			parent = CPG_GROUP (ret);
		}
		else
		{
			parent = NULL;
		}

		++ptr;
	}

	g_strfreev (parts);
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
 * Returns: A #CpgProperty
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

