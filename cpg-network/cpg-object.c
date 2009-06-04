#include <stdlib.h>
#include <string.h>

#include "cpg-object.h"

#include "cpg-link.h"
#include "cpg-state.h"
#include "cpg-expression.h"

#include "cpg-debug.h"

#define CPG_OBJECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_OBJECT, CpgObjectPrivate))

struct _CpgObjectPrivate
{
	gchar *id;

	// Properties
	GSList *properties;	
	GSList *actors;
	
	// Links
	GSList *links;
};

/* Properties */
enum
{
	PROP_0,
	PROP_ID,
	PROP_LOCAL_ID
};

/* Signals */
enum
{
	TAINTED,
	NUM_SIGNALS
};

G_DEFINE_TYPE (CpgObject, cpg_object, G_TYPE_OBJECT)

static guint object_signals[NUM_SIGNALS] = {0,};

static void
cpg_object_finalize (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);
	
	g_slist_foreach (obj->priv->properties, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (obj->priv->properties);

	g_slist_free (obj->priv->links);
	g_slist_free (obj->priv->actors);
	
	g_free (obj->priv->id);
		
	G_OBJECT_CLASS (cpg_object_parent_class)->finalize (object);
}

/* interface implementations */
static void
property_reset (CpgProperty  *property,
                gpointer      data)
{
	cpg_expression_reset (cpg_property_get_value_expression (property));
}

static void
cpg_object_reset_impl (CpgObject *object)
{
	g_slist_foreach (object->priv->properties, (GFunc)property_reset, NULL);
}

static void
set_id (CpgObject   *object,
        gchar const *id)
{
	g_free (object->priv->id);
	object->priv->id = g_strdup (id);
}

static void
get_property (GObject     *object,
              guint        prop_id,
              GValue      *value,
              GParamSpec  *pspec)
{
	CpgObject *obj = CPG_OBJECT (object);

	switch (prop_id)
	{
		case PROP_ID:
			g_value_set_string (value, obj->priv->id);
		break;
		case PROP_LOCAL_ID:
			g_value_take_string (value, cpg_object_get_local_id (obj));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
set_property (GObject       *object,
              guint          prop_id,
              GValue const  *value,
              GParamSpec    *pspec)
{
	CpgObject *obj = CPG_OBJECT (object);

	switch (prop_id)
	{
		case PROP_ID:
		{
			set_id (obj, g_value_get_string (value));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
link_destroyed (CpgObject  *object,
                CpgLink    *link,
                gboolean    is_last_ref)
{
	if (!is_last_ref)
		return;

	/* Remove link, actors and toggle ref */
	object->priv->links = g_slist_remove (object->priv->links, link);
	
	GSList *item;
	
	for (item = cpg_link_get_actions (link); item; item = g_slist_next (item))
	{
		CpgLinkAction *action = (CpgLinkAction *)item->data;
		object->priv->actors = g_slist_remove (object->priv->actors, cpg_link_action_get_target (action));
	}
	
	g_object_remove_toggle_ref (G_OBJECT (link), (GToggleNotify)link_destroyed, object);
}

static void
cpg_object_dispose (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);
	
	/* Untoggle ref all links, because we need them destroyed! */
	GSList *item;
	GSList *copy = g_slist_copy (obj->priv->links);
	
	for (item = copy; item; item = g_slist_next (item))
		link_destroyed (obj, CPG_LINK (item->data), TRUE);
	
	g_slist_free (copy);
}

static void
property_reset_cache (CpgProperty  *property,
                      gpointer      data)
{
	cpg_expression_reset_cache (cpg_property_get_value_expression (property));
}

void
cpg_object_reset_cache_impl (CpgObject *object)
{
	g_slist_foreach (object->priv->properties, (GFunc)property_reset_cache, NULL);
}

static void
cpg_object_class_init (CpgObjectClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_object_finalize;
	object_class->dispose = cpg_object_dispose;
	object_class->get_property = get_property;
	object_class->set_property = set_property;

	klass->reset = cpg_object_reset_impl;
	klass->reset_cache = cpg_object_reset_cache_impl;
	
	g_object_class_install_property (object_class, PROP_ID,
				 g_param_spec_string ("id",
						      "ID",
						      "The object's id",
						      NULL,
						      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class, PROP_LOCAL_ID,
				 g_param_spec_string ("local-id",
						      "LOCAL_ID",
						      "The object's local id",
						      NULL,
						      G_PARAM_READABLE));

	object_signals[TAINTED] = 
	   		g_signal_new ("tainted",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (CpgObjectClass, tainted),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	g_type_class_add_private (object_class, sizeof (CpgObjectPrivate));
}

static void
cpg_object_init (CpgObject *self)
{
	self->priv = CPG_OBJECT_GET_PRIVATE (self);
}

/**
 * cpg_object_new:
 * @id: the object id
 *
 * Creates a new #CpgObject.
 *
 * Return value: the newly created #CpgObject
 *
 **/
CpgObject *
cpg_object_new (gchar const *id)
{
	return g_object_new (CPG_TYPE_OBJECT, "id", id, NULL);
}

/**
 * cpg_object_add_property:
 * @object: the #CpgObject
 * @name: the property name
 * @expression: the properties initial value
 * @integrated: whether or not the update values should be integrated when
 * a link acts on the property
 *
 * Returns the new property added to the object
 *
 * Return value: the new #CpgProperty. The returned object is owned by
 * @object and should not be freed
 *
 **/
CpgProperty *
cpg_object_add_property (CpgObject    *object,
                         gchar const  *name,
                         gchar const  *expression,
                         gboolean      integrated)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expression != NULL, NULL);

	CpgProperty *property = cpg_property_new (name, expression, integrated, object);
	
	object->priv->properties = g_slist_append (object->priv->properties, property);	
	return property;
}

CpgProperty *
cpg_object_get_property (CpgObject    *object,
                         gchar const  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	GSList *item;
	
	for (item = object->priv->properties; item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		
		if (strcmp (cpg_property_get_name (property), name) == 0)
			return property;
	}
	
	return NULL;
}

gboolean
cpg_object_has_property (CpgObject    *object,
                         gchar const  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);
	
	return cpg_object_get_property (object, name) != NULL;
}

void
cpg_object_remove_property (CpgObject    *object,
                            gchar const  *name)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (name != NULL);
	
	CpgProperty *property = cpg_object_get_property (object, name);
	
	if (property)
	{
		object->priv->properties = g_slist_remove (object->priv->properties, property);
		cpg_ref_counted_unref (property);
	}
}

/**
 * cpg_object_get_properties:
 * @object: a #CpgObject
 *
 * Gets the object properties.
 *
 * Returns: a list of #CpgProperty. The list is owned by the object and should
 *          not be freed.
 *
 **/
GSList *
cpg_object_get_properties (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->properties;
}

/**
 * _cpg_object_link:
 * @object: the #CpgObject
 * @link: the #CpgLink which links to this object
 *
 * Adds @link as a link which targets the object (link will be evaluated in
 * #cpg_object_evaluate).
 *
 **/
void
_cpg_object_link (CpgObject  *object,
                  CpgLink    *link)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_LINK (link));
	
	object->priv->links = g_slist_append (object->priv->links, link);
	
	g_object_add_toggle_ref (G_OBJECT (link), (GToggleNotify)link_destroyed, object);
}

GSList *
_cpg_object_get_actors (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	
	GSList *ret = NULL;
	GSList *item;
	
	for (item = object->priv->links; item; item = g_slist_next (item))
	{
		GSList *action;
		GSList *actions;
		
		actions = cpg_link_get_actions (CPG_LINK (item->data));
		
		for (action = actions; action; action = g_slist_next (action))
		{
			CpgLinkAction *a = (CpgLinkAction *)action->data;
			ret = g_slist_prepend (ret, cpg_link_action_get_target (a));
		}
	}
	
	return g_slist_reverse (ret);
}

/**
 * cpg_object_update:
 * @object: the #CpgObject
 * @timestep: the timestep to use
 *
 * Update property values using the values previously calculated in 
 * #cpg_object_evaluate.
 *
 **/
void
cpg_object_update (CpgObject  *object,
                   gdouble     timestep)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (timestep > 0);

	if (CPG_OBJECT_GET_CLASS (object)->update)
	{
		CPG_OBJECT_GET_CLASS (object)->update (object, timestep);
	}
}

/**
 * cpg_object_evaluate:
 * @object: the #CpgObject
 * @timestep: the timestep to use
 *
 * Calculates update values for all the properties acted on by links
 *
 **/
void
cpg_object_evaluate (CpgObject  *object,
                     gdouble     timestep)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (timestep > 0);

	if (CPG_OBJECT_GET_CLASS (object)->evaluate)
	{
		CPG_OBJECT_GET_CLASS (object)->evaluate (object, timestep);
	}
}

/**
 * cpg_object_reset:
 * @object: the #CpgObject
 *
 * Reset all properties to their initial values
 *
 **/
void
cpg_object_reset (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->reset)
	{
		CPG_OBJECT_GET_CLASS (object)->reset (object);
	}
}

/**
 * cpg_object_get_id:
 * @object: a #CpgObject
 *
 * Gets the object id
 *
 * Returns: the object id
 **/
gchar const *
cpg_object_get_id (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->id;
}

/**
 * cpg_object_set_id:
 * @object: a #CpgObject
 * @id: the new object id
 *
 * Sets the object id
 *
 **/
void 
cpg_object_set_id (CpgObject    *object,
                   gchar const  *id)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (id != NULL);
	
	set_id (object, id);	
	g_object_notify (G_OBJECT (object), "id");
}

gchar *
cpg_object_get_local_id (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	if (!object->priv->id)
		return NULL;

	gchar *last = strrchr (object->priv->id, '.');
	
	if (!last)
		return g_strdup (object->priv->id);
	else
		return g_strdup (last + 1);
}

GSList *
_cpg_object_get_links (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	
	return object->priv->links;
}

void
cpg_object_reset_cache (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	
	if (CPG_OBJECT_GET_CLASS (object)->reset_cache)
	{
		CPG_OBJECT_GET_CLASS (object)->reset_cache (object);
	}
}

void
cpg_object_taint (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	
	g_signal_emit (object, object_signals[TAINTED], 0);
}
