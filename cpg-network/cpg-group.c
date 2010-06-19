#include "cpg-group.h"
#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-compile-context.h"

/**
 * SECTION:group
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
};

G_DEFINE_TYPE (CpgGroup, cpg_group, CPG_TYPE_STATE)

enum
{
	PROP_0,
	PROP_PROXY
};

static void
cpg_group_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_group_parent_class)->finalize (object);
}

static void
cpg_group_dispose (GObject *object)
{
	CpgGroup *group = CPG_GROUP (object);

	if (group->priv->children)
	{
		GSList *children = group->priv->children;
		group->priv->children = NULL;

		g_slist_foreach (children, (GFunc)g_object_unref, NULL);
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

		g_object_unref (group->priv->proxy);
		group->priv->proxy = NULL;

		group->priv->children = g_slist_remove (group->priv->children,
		                                        group->priv->proxy);
	}

	if (proxy)
	{
		group->priv->proxy = g_object_ref (proxy);

		group->priv->children = g_slist_prepend (group->priv->children,
		                                         g_object_ref (group->priv->proxy));
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

static void
cpg_group_cpg_evaluate (CpgObject *object)
{
	CPG_OBJECT_CLASS (cpg_group_parent_class)->evaluate (object);

	/* And then also the children! */
	cpg_group_foreach (CPG_GROUP (object), (GFunc)cpg_object_evaluate, NULL);
}

static gboolean
cpg_group_cpg_compile (CpgObject         *object,
                       CpgCompileContext *context,
                       CpgCompileError   *error)
{
	if (!CPG_OBJECT_CLASS (cpg_group_parent_class)->compile (object,
	                                                         context,
	                                                         error))
	{
		return FALSE;
	}

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
cpg_group_cpg_copy (CpgObject *object,
                    CpgObject *source)
{
	/* Copy all the normal stuff */
	CPG_OBJECT_CLASS (cpg_group_parent_class)->copy (object, source);

	/* And then recursive copy the children */
	CpgGroup *group = CPG_GROUP (object);
	CpgGroup *source_group = CPG_GROUP (source);
	CpgObject *source_proxy = cpg_group_get_proxy (source_group);

	GSList const *children = cpg_group_get_children (source_group);

	while (children)
	{
		CpgObject *child = _cpg_object_copy (children->data);

		if (CPG_OBJECT (children->data) == source_proxy)
		{
			group->priv->proxy = g_object_ref (child);
		}

		group->priv->children = g_slist_prepend (group->priv->children,
		                                         child);

		children = g_slist_next (children);
	}

	group->priv->children = g_slist_reverse (group->priv->children);
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
	cpg_class->get_properties = cpg_group_cpg_get_properties;
	cpg_class->remove_property = cpg_group_cpg_remove_property;

	cpg_class->compile = cpg_group_cpg_compile;
	cpg_class->reset = cpg_group_cpg_reset;
	cpg_class->reset_cache = cpg_group_cpg_reset_cache;
	cpg_class->evaluate = cpg_group_cpg_evaluate;
	cpg_class->copy = cpg_group_cpg_copy;

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
}

static void
cpg_group_init (CpgGroup *self)
{
	self->priv = CPG_GROUP_GET_PRIVATE (self);
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

	return group->priv->children;
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

	if (g_slist_find (group->priv->children, object))
	{
		return FALSE;
	}

	group->priv->children = g_slist_append (group->priv->children,
	                                        g_object_ref (object));

	cpg_object_taint (CPG_OBJECT (group));
	return TRUE;
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

	if (object == group->priv->proxy)
	{
		return set_proxy (group, NULL);
	}

	GSList *item = g_slist_find (group->priv->children, object);

	if (item)
	{
		group->priv->children = g_slist_remove_link (group->priv->children,
		                                             item);

		g_object_unref (object);
		return TRUE;
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

void
cpg_group_foreach (CpgGroup *group,
                   GFunc     func,
                   gpointer  data)
{
	g_return_if_fail (CPG_IS_GROUP (group));

	g_slist_foreach (group->priv->children, func, data);
}

