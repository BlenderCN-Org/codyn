#include <stdlib.h>
#include <string.h>

#include "cpg-object.h"

#include "cpg-link.h"
#include "cpg-state.h"
#include "cpg-expression.h"

#include "cpg-debug.h"
#include "cpg-compile-error.h"

/**
 * SECTION:object
 * @short_description: Basis for all cpg objects
 *
 * #CpgObject is a base class for all the objects which can be added to a
 * network. It provides property storage and virtual methods which can be
 * implemented that drive the simulation process.
 *
 */

#define CPG_OBJECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_OBJECT, CpgObjectPrivate))

struct _CpgObjectPrivate
{
	gchar *id;

	// Properties
	GSList *properties;
	GHashTable *property_hash;

	// Links
	GSList *links;

	// Actors
	GSList *actors;

	CpgObject *template;
};

/* Properties */
enum
{
	PROP_0,
	PROP_ID,
	PROP_LOCAL_ID,
	PROP_TEMPLATE
};

/* Signals */
enum
{
	TAINTED,
	NUM_SIGNALS
};

G_DEFINE_TYPE (CpgObject, cpg_object, G_TYPE_OBJECT)

static guint object_signals[NUM_SIGNALS] = {0,};

GQuark
cpg_object_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_object_error");
	}

	return quark;
}

static void
free_property (CpgProperty *property,
               CpgObject   *object)
{
	_cpg_property_unuse (property);

	g_signal_handlers_disconnect_by_func (property,
	                                      cpg_object_taint,
	                                      object);

	g_object_unref (property);
}

static void
cpg_object_finalize (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);

	g_slist_foreach (obj->priv->properties, (GFunc)free_property, object);
	g_slist_free (obj->priv->properties);

	g_slist_free (obj->priv->links);
	g_slist_free (obj->priv->actors);

	g_free (obj->priv->id);

	g_hash_table_destroy (obj->priv->property_hash);

	G_OBJECT_CLASS (cpg_object_parent_class)->finalize (object);
}

/* interface implementations */
static void
property_reset (CpgProperty  *property,
                gpointer      data)
{
	cpg_expression_reset (cpg_property_get_expression (property));
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
		case PROP_TEMPLATE:
			g_value_set_object (value, obj->priv->template);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
template_toggled (CpgObject *object,
                  CpgObject *template,
                  gboolean   is_last_ref)
{
	if (is_last_ref)
	{
		object->priv->template = NULL;

		g_object_remove_toggle_ref (G_OBJECT (template),
		                            (GToggleNotify)template_toggled,
		                            object);
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
			set_id (obj, g_value_get_string (value));
		break;
		case PROP_TEMPLATE:
			if (obj->priv->template)
			{
				g_object_remove_toggle_ref (G_OBJECT (obj->priv->template),
				                            (GToggleNotify)template_toggled,
				                            obj);
			}

			obj->priv->template = g_value_get_object (value);

			if (obj->priv->template)
			{
				g_object_add_toggle_ref (G_OBJECT (obj->priv->template),
				                         (GToggleNotify)template_toggled,
				                         obj);
			}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object,
			                                   prop_id,
			                                   pspec);
		break;
	}
}

static void
link_destroyed (CpgObject  *object,
                CpgLink    *link,
                gboolean    is_last_ref)
{
	if (!is_last_ref)
	{
		return;
	}

	/* Remove link, and toggle ref */
	object->priv->links = g_slist_remove (object->priv->links, link);

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            object);
}

static void
cpg_object_dispose (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);

	if (obj->priv->template)
	{
		g_object_remove_toggle_ref (G_OBJECT (obj->priv->template),
		                            (GToggleNotify)template_toggled,
		                            obj);
	}

	/* Untoggle ref all links, because we need them destroyed! */
	GSList *item;
	GSList *copy = g_slist_copy (obj->priv->links);

	for (item = copy; item; item = g_slist_next (item))
	{
		link_destroyed (obj, item->data, TRUE);
	}

	g_slist_free (copy);

	G_OBJECT_CLASS (cpg_object_parent_class)->dispose (object);
}

static void
cpg_object_reset_cache_impl (CpgObject *object)
{
	g_slist_foreach (object->priv->properties,
	                 (GFunc)cpg_property_reset_cache,
	                 NULL);
}

static void
cpg_object_copy_impl (CpgObject *object,
                      CpgObject *source)
{
	// Copy id
	object->priv->id = g_strdup (source->priv->id);

	// Copy properties
	GSList *item;

	for (item = source->priv->properties; item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		CpgProperty *cpy = _cpg_property_copy (property);

		_cpg_property_set_object (cpy, object);
		_cpg_property_use (cpy);

		object->priv->properties = g_slist_prepend (object->priv->properties,
		                                            cpy);
	}

	object->priv->properties = g_slist_reverse (object->priv->properties);
}

static gboolean
cpg_object_compile_impl (CpgObject         *object,
                         CpgCompileContext *context,
                         CpgCompileError   *error)
{
	/* Compile all the property expressions */
	GSList *properties = object->priv->properties;
	gboolean ret = TRUE;

	/* Prepend the object in the context */
	cpg_compile_context_save (context);
	cpg_compile_context_prepend_object (context, object);

	while (properties)
	{
		CpgProperty *property = (CpgProperty *)properties->data;
		CpgExpression *expr = cpg_property_get_expression (property);
		GError *gerror = NULL;

		if (!cpg_expression_compile (expr,
		                             context,
		                             &gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s].%s<%s>: %s",
			                 cpg_object_get_id (object),
			                 cpg_property_get_name (property),
			                 cpg_expression_get_as_string (expr),
			                 gerror->message);


			if (error)
			{
				cpg_compile_error_set (error,
				                       gerror,
				                       object,
				                       property,
				                       NULL);
			}

			g_error_free (gerror);

			ret = FALSE;
			break;
		}

		properties = g_slist_next (properties);
	}

	if (ret)
	{
		cpg_object_reset (object);
	}

	cpg_compile_context_restore (context);
	return ret;
}

static GSList *
cpg_object_get_properties_impl (CpgObject *object)
{
	return g_slist_copy (object->priv->properties);
}

static CpgProperty *
cpg_object_get_property_impl (CpgObject   *object,
                              gchar const *name)
{
	return g_hash_table_lookup (object->priv->property_hash, name);
}

static gboolean
cpg_object_has_property_impl (CpgObject *object,
                              gchar const *name)
{
	return cpg_object_get_property (object, name) != NULL;
}

static gboolean
cpg_object_remove_property_impl (CpgObject    *object,
                                 gchar const  *name,
                                 GError      **error)
{
	if (error)
	{
		*error = NULL;
	}

	CpgProperty *property = cpg_object_get_property (object, name);

	if (property)
	{
		if (!_cpg_property_unuse (property))
		{
			if (error)
			{
				g_set_error (error,
				             CPG_OBJECT_ERROR,
				             CPG_OBJECT_ERROR_PROP_IN_USE,
				             "Property %s is still in use and can not be removed",
				             name);
			}

			_cpg_property_use (property);
			return FALSE;
		}

		object->priv->properties = g_slist_remove (object->priv->properties,
		                                           property);

		g_hash_table_remove (object->priv->property_hash,
		                     cpg_property_get_name (property));

		g_signal_handlers_disconnect_by_func (property,
		                                      cpg_object_taint,
		                                      object);
		g_object_unref (property);

		cpg_object_taint (object);
	}
	else
	{
		if (error)
		{
			g_set_error (error,
			             CPG_OBJECT_ERROR,
			             CPG_OBJECT_ERROR_PROP_NOT_FOUND,
			             "Property %s could not be found for %s",
			             name,
			             cpg_object_get_id (object));
		}
		return FALSE;
	}

	return TRUE;
}

static CpgProperty *
cpg_object_add_property_impl (CpgObject   *object,
                              gchar const *name,
                              gchar const *expression,
                              gboolean     integrated)
{
	// Check if property already set
	CpgProperty *property;

	property = cpg_object_get_property (object, name);

	if (property)
	{
		if (!cpg_object_remove_property (object, name, NULL))
		{
			return NULL;
		}
	}

	property = cpg_property_new (name, expression, integrated, object);
	object->priv->properties = g_slist_append (object->priv->properties,
	                                           property);

	g_hash_table_insert (object->priv->property_hash,
	                     g_strdup (cpg_property_get_name (property)),
	                     property);

	_cpg_property_use (property);
	cpg_object_taint (object);

	g_signal_connect_swapped (property,
	                          "notify::expression",
	                          G_CALLBACK (cpg_object_taint),
	                          object);

	return property;
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
	klass->copy = cpg_object_copy_impl;
	klass->compile = cpg_object_compile_impl;

	klass->get_property = cpg_object_get_property_impl;
	klass->get_properties = cpg_object_get_properties_impl;
	klass->has_property = cpg_object_has_property_impl;
	klass->remove_property = cpg_object_remove_property_impl;
	klass->add_property = cpg_object_add_property_impl;

	/**
	 * CpgObject:id:
	 *
	 * The #CpgObject id
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_ID,
	                                 g_param_spec_string ("id",
	                                                      "ID",
	                                                      "The object's id",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgObject:local-id:
	 *
	 * The #CpgObject local id
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_LOCAL_ID,
	                                 g_param_spec_string ("local-id",
	                                                      "LOCAL_ID",
	                                                      "The object's local id",
	                                                      NULL,
	                                                      G_PARAM_READABLE));

	/**
	 * CpgObject:template:
	 *
	 * The #CpgObject template on which the object is based
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TEMPLATE,
	                                 g_param_spec_object ("template",
	                                                      "TEMPLATE",
	                                                      "The object's template",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgObject::tainted:
	 * @object: a #CpgObject
	 *
	 * Emitted when the object is tainted
	 *
	 **/
	object_signals[TAINTED] =
		g_signal_new ("tainted",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               tainted),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	g_type_class_add_private (object_class, sizeof (CpgObjectPrivate));
}

static void
cpg_object_init (CpgObject *self)
{
	self->priv = CPG_OBJECT_GET_PRIVATE (self);

	self->priv->property_hash = g_hash_table_new_full (g_str_hash,
	                                                   g_str_equal,
	                                                   (GDestroyNotify)g_free,
	                                                   NULL);
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

	if (CPG_OBJECT_GET_CLASS (object)->add_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->add_property (object,
		                                                    name,
		                                                    expression,
		                                                    integrated);
	}
	else
	{
		return NULL;
	}
}

/**
 * cpg_object_get_property:
 * @object: a #CpgObject
 * @name: a property name
 *
 * Get a #CpgProperty from the object by name
 *
 * Returns: the #CpgProperty with name @name, or %NULL if no such property could
 *          be found
 *
 **/
CpgProperty *
cpg_object_get_property (CpgObject    *object,
                         gchar const  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (CPG_OBJECT_GET_CLASS (object)->get_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->get_property (object,
		                                                    name);
	}
	else
	{
		return NULL;
	}
}

/**
 * cpg_object_has_property:
 * @object: a #CpgObject
 * @name: a property name
 *
 * Get whether @object has a property with name @name
 *
 * Returns: %TRUE if @object has a property with name @name, %FALSE otherwise
 *
 **/
gboolean
cpg_object_has_property (CpgObject    *object,
                         gchar const  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return cpg_object_get_property (object, name) != NULL;
}

/**
 * cpg_object_remove_property:
 * @object: a #CpgObject
 * @name: a property name
 * @error: a #GError
 *
 * Remove the property @name from @object. If the property was not found or
 * could not be removed, @error will be appropriately set
 *
 * Returns: %TRUE if the property could be removed, %FALSE otherwise
 *
 **/
gboolean
cpg_object_remove_property (CpgObject    *object,
                            gchar const  *name,
                            GError      **error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	if (CPG_OBJECT_GET_CLASS (object)->remove_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->remove_property (object,
		                                                       name,
		                                                       error);
	}
	else
	{
		return FALSE;
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

	if (CPG_OBJECT_GET_CLASS (object)->get_properties)
	{
		return CPG_OBJECT_GET_CLASS (object)->get_properties (object);
	}
	else
	{
		return NULL;
	}
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

	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	g_object_add_toggle_ref (G_OBJECT (link),
	                         (GToggleNotify)link_destroyed,
	                         object);
}

/**
 * _cpg_object_unlink:
 * @object: the #CpgObject
 * @link: the #CpgLink which unlinks from this object
 *
 * Removes @link as a link which targets the object.
 *
 **/
void
_cpg_object_unlink (CpgObject  *object,
                    CpgLink    *link)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_LINK (link));

	GSList *item = g_slist_find (object->priv->links, link);

	if (!item)
	{
		return;
	}

	object->priv->links = g_slist_remove_link (object->priv->links,
	                                           item);

	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            object);
}

/**
 * cpg_object_get_actors:
 * @object: A #CpgObject
 *
 * Get the properties which are acted upon by links.
 *
 * Returns: A #GSList of #CpgProperty. This list is used internally and should
 *          not be freed.
 *
 **/
GSList const *
cpg_object_get_actors (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	if (object->priv->actors != NULL)
	{
		return object->priv->actors;
	}

	GSList *ret = NULL;
	GSList *item;

	for (item = object->priv->links; item; item = g_slist_next (item))
	{
		GSList const *actions;

		actions = cpg_link_get_actions (CPG_LINK (item->data));

		while (actions)
		{
			CpgLinkAction *a = actions->data;
			CpgProperty *target = cpg_link_action_get_target (a);

			if (!g_slist_find (ret, target))
			{
				ret = g_slist_prepend (ret, target);
			}

			actions = g_slist_next (actions);
		}
	}

	object->priv->actors = g_slist_reverse (ret);
	return object->priv->actors;
}

/**
 * cpg_object_evaluate:
 * @object: the #CpgObject
 *
 * Calculates update values for all the properties acted on by links
 *
 **/
void
cpg_object_evaluate (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->evaluate)
	{
		CPG_OBJECT_GET_CLASS (object)->evaluate (object);
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
 *
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

/**
 * cpg_object_get_local_id:
 * @object: a #CpgObject
 *
 * Gets the object's local id. The local id is the last part of a dot-namespaced
 * object name
 *
 * Returns: the object local id
 *
 **/
gchar *
cpg_object_get_local_id (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	if (!object->priv->id)
	{
		return NULL;
	}

	gchar *last = strrchr (object->priv->id, '.');

	if (!last)
	{
		return g_strdup (object->priv->id);
	}
	else
	{
		return g_strdup (last + 1);
	}
}

GSList const *
_cpg_object_get_links (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->links;
}

/**
 * cpg_object_reset_cache:
 * @object: a #CpgObject
 *
 * Reset object expression caches. This will reset all expressions (such as
 * within properties) within the object
 *
 **/
void
cpg_object_reset_cache (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->reset_cache)
	{
		CPG_OBJECT_GET_CLASS (object)->reset_cache (object);
	}
}

/**
 * cpg_object_compile:
 * @object: A #CpgObject
 * @context: A #CpgCompileContext
 * @error: A #CpgCompileError
 *
 * Compile the object.
 *
 * Returns: %TRUE if the object compiled successfully, %FALSE otherwise. If the
 *          compilation failed and @error was set, the reason for the compile
 *          failure is set in @error.
 *
 **/
gboolean
cpg_object_compile (CpgObject         *object,
                    CpgCompileContext *context,
                    CpgCompileError   *error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_OBJECT_GET_CLASS (object)->compile)
	{
		return CPG_OBJECT_GET_CLASS (object)->compile (object,
		                                               context,
		                                               error);
	}

	return TRUE;
}

/**
 * cpg_object_taint:
 * @object: a #CpgObject
 *
 * Mark the object as tainted. This emits the "tainted" signal. The #CpgNetwork
 * in which the object is added acts on this signal to mark the network tainted
 * and as such the object will be properly recompiled when the network needs
 * to be simulated
 *
 **/
void
cpg_object_taint (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	g_signal_emit (object, object_signals[TAINTED], 0);
}

CpgObject *
_cpg_object_copy (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	CpgObject *ret = g_object_new (G_OBJECT_TYPE (object),
	                               "template",
	                               object,
	                               NULL);

	if (CPG_OBJECT_GET_CLASS (object)->copy)
	{
		CPG_OBJECT_GET_CLASS (object)->copy (ret, object);
	}

	return ret;
}
