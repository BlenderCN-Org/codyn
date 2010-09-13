#include <string.h>

#include "cpg-property.h"
#include "cpg-expression.h"
#include "cpg-utils.h"
#include "cpg-enum-types.h"
#include "cpg-object.h"
#include "cpg-marshal.h"
#include "cpg-usable.h"
#include "cpg-modifiable.h"

#define CPG_PROPERTY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PROPERTY, CpgPropertyPrivate))

/* signals */
enum
{
	INVALIDATE_NAME,
	EXPRESSION_CHANGED,
	FLAGS_CHANGED,
	NUM_SIGNALS
};

struct _CpgPropertyPrivate
{
	guint use_count;
	gchar *name;

	CpgExpression *expression;
	CpgPropertyFlags flags;

	gdouble update;
	CpgObject *object;

	gdouble last_value;
	gboolean modified : 1;
};

static void cpg_usable_iface_init (gpointer iface);
static void cpg_modifiable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgProperty,
                         cpg_property,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_USABLE,
                                                cpg_usable_iface_init);
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_MODIFIABLE,
                                                cpg_modifiable_iface_init));

static guint signals[NUM_SIGNALS] = {0,};

enum
{
	PROP_0,
	PROP_NAME,
	PROP_OBJECT,
	PROP_FLAGS,
	PROP_EXPRESSION,
	PROP_USE_COUNT,
	PROP_MODIFIED
};

/**
 * SECTION:cpg-property
 * @short_description: Property container
 *
 * A #CpgProperty is a container for a specific variable in an object. It
 * consists of a name and a mathematical expression describing its contents.
 *
 */

static void
cpg_property_use (CpgUsable *usable)
{
	CpgProperty *prop = CPG_PROPERTY (usable);
	++prop->priv->use_count;
}

static gboolean
cpg_property_unuse (CpgUsable *usable)
{
	CpgProperty *prop = CPG_PROPERTY (usable);

	if (prop->priv->use_count == 0)
	{
		return TRUE;
	}

	return (--(prop->priv->use_count) == 0);
}

static void
cpg_usable_iface_init (gpointer iface)
{
	CpgUsableInterface *usable = iface;

	usable->use = cpg_property_use;
	usable->unuse = cpg_property_unuse;
}

static void
cpg_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static void
set_object (CpgProperty *property,
            CpgObject   *object)
{
	if (property->priv->object == object)
	{
		return;
	}

	if (property->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (property->priv->object),
		                              (gpointer *)&property->priv->object);
	}

	property->priv->object = object;

	if (property->priv->object)
	{
		g_object_add_weak_pointer (G_OBJECT (property->priv->object),
		                           (gpointer *)&property->priv->object);
	}

	g_object_notify (G_OBJECT (property), "object");
}

static void
on_expression_changed (CpgProperty *property)
{
	cpg_modifiable_set_modified (CPG_MODIFIABLE (property), TRUE);

	g_object_notify (G_OBJECT (property), "expression");
}

static void
set_expression (CpgProperty *property,
                CpgExpression *expression)
{
	if (property->priv->expression == expression ||
	    (expression && property->priv->expression &&
	     cpg_expression_equal (property->priv->expression,
	                           expression)))
	{
		if (expression && g_object_is_floating (expression))
		{
			g_object_unref (expression);
		}

		return;
	}

	if (property->priv->expression)
	{
		g_signal_handlers_disconnect_by_func (property->priv->expression,
		                                      on_expression_changed,
		                                      property);

		g_object_unref (property->priv->expression);
		property->priv->expression = NULL;
	}

	if (expression)
	{
		property->priv->expression = g_object_ref_sink (expression);

		g_signal_connect_swapped (expression,
		                          "notify::expression",
		                          G_CALLBACK (on_expression_changed),
		                          property);
	}

	g_object_notify (G_OBJECT (property), "expression");
	cpg_modifiable_set_modified (CPG_MODIFIABLE (property), TRUE);
}

static void
cpg_property_finalize (GObject *object)
{
	CpgProperty *property;

	property = CPG_PROPERTY (object);

	g_free (property->priv->name);

	G_OBJECT_CLASS (cpg_property_parent_class)->finalize (object);
}

static void
cpg_property_dispose (GObject *object)
{
	CpgProperty *property = CPG_PROPERTY (object);

	g_object_freeze_notify (object);

	set_expression (property, NULL);
	set_object (property, NULL);

	g_object_thaw_notify (object);

	G_OBJECT_CLASS (cpg_property_parent_class)->dispose (object);
}

static void
set_flags (CpgProperty      *property,
           CpgPropertyFlags  flags)
{
	if (flags != property->priv->flags)
	{
		gboolean wasonce = property->priv->flags & CPG_PROPERTY_FLAG_ONCE;
		property->priv->flags = flags;

		if (flags & CPG_PROPERTY_FLAG_ONCE)
		{
			if (!wasonce)
			{
				cpg_expression_reset_cache (property->priv->expression);
				cpg_expression_set_once (property->priv->expression,
				                         TRUE);
			}
		}
		else if (wasonce)
		{
			cpg_expression_set_once (property->priv->expression,
			                         FALSE);
			cpg_expression_reset_cache (property->priv->expression);
		}

		g_object_notify (G_OBJECT (property), "flags");
		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), TRUE);
	}
}

static gboolean
set_name (CpgProperty *property,
          gchar const *name)
{
	if (g_strcmp0 (property->priv->name, name) == 0)
	{
		return TRUE;
	}

	gboolean invalid = FALSE;

	g_signal_emit (property, signals[INVALIDATE_NAME], 0, name, &invalid);

	if (!invalid)
	{
		g_free (property->priv->name);
		property->priv->name = g_strdup (name);

		g_object_notify (G_OBJECT (property), "name");
	}

	return !invalid;
}

static void
cpg_property_set_property (GObject      *object,
                           guint         prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
	CpgProperty *self = CPG_PROPERTY (object);

	switch (prop_id)
	{
		case PROP_NAME:
			set_name (self, g_value_get_string (value));
		break;
		case PROP_OBJECT:
			set_object (self, g_value_get_object (value));
		break;
		case PROP_FLAGS:
			set_flags (self, g_value_get_flags (value));
		break;
		case PROP_EXPRESSION:
			set_expression (self, g_value_get_object (value));
		break;
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_property_get_property (GObject    *object,
                           guint       prop_id,
                           GValue     *value,
                           GParamSpec *pspec)
{
	CpgProperty *self = CPG_PROPERTY (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_value_set_string (value, self->priv->name);
		break;
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_FLAGS:
			g_value_set_flags (value, self->priv->flags);
		break;
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
		break;
		case PROP_USE_COUNT:
			g_value_set_uint (value, self->priv->use_count);
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_property_class_init (CpgPropertyClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_property_finalize;
	object_class->dispose = cpg_property_dispose;

	object_class->get_property = cpg_property_get_property;
	object_class->set_property = cpg_property_set_property;

	g_type_class_add_private (object_class, sizeof(CpgPropertyPrivate));

	/**
	 * CpgProperty:name:
	 *
	 * The property name
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_NAME,
	                                 g_param_spec_string ("name",
	                                                      "Name",
	                                                      "Name",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	/**
	 * CpgProperty:object:
	 *
	 * The object on which the property is defined
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	/**
	 * CpgProperty:flags:
	 *
	 * The property flags
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FLAGS,
	                                 g_param_spec_flags ("flags",
	                                                     "Flags",
	                                                     "Flags",
	                                                     CPG_TYPE_PROPERTY_FLAGS,
	                                                     CPG_PROPERTY_FLAG_NONE,
	                                                     G_PARAM_READWRITE |
	                                                     G_PARAM_CONSTRUCT));

	/**
	 * CpgProperty:expression:
	 *
	 * The property expression
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_override_property (object_class,
	                                  PROP_USE_COUNT,
	                                  "use-count");

	/**
	 * CpgProperty::invalidate-name:
	 * @property: a #CpgProperty
	 * @name: the new property name
	 *
	 * This signal is emitted to validate (or rather, invalidate) a new
	 * name for a property. When a signal handler returns %TRUE,
	 * the new name is rejected.
	 *
	 * Returns: %TRUE if the new name should be rejected, %FALSE otherwise
	 *
	 **/
	signals[INVALIDATE_NAME] =
		g_signal_new ("invalidate-name",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgPropertyClass, invalidate_name),
		              g_signal_accumulator_true_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__STRING,
		              G_TYPE_BOOLEAN,
		              1,
		              G_TYPE_STRING);

	signals[EXPRESSION_CHANGED] =
		g_signal_new ("expression-changed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_EXPRESSION);

	signals[FLAGS_CHANGED] =
		g_signal_new ("flags-changed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__FLAGS,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_PROPERTY_FLAGS);

	g_object_class_override_property (object_class,
	                                 PROP_MODIFIED,
	                                 "modified");
}

static void
cpg_property_init (CpgProperty *self)
{
	self->priv = CPG_PROPERTY_GET_PRIVATE (self);
}

/**
 * cpg_property_new:
 * @name: the property name
 * @expression: the value expression
 * @flags: the property flags
 *
 * Create a new property object. Property objects are assigned to #CpgObject
 * objects and are of little use on their own. The provided expression will
 * not be parsed initially.
 *
 * Returns: the new #CpgProperty
 *
 **/
CpgProperty *
cpg_property_new (gchar const      *name,
                  gchar const      *expression,
                  CpgPropertyFlags  flags)
{
	return g_object_new (CPG_TYPE_PROPERTY,
	                     "name", name,
	                     "expression", cpg_expression_new (expression),
	                     "flags", flags,
	                     NULL);
}

/**
 * cpg_property_get_object:
 * @property: the #CpgProperty
 *
 * Get the object associated with the property
 *
 * Returns: (type CpgObject) (transfer none): the object associated with the property
 **/
CpgObject *
cpg_property_get_object (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->object;
}

/**
 * cpg_property_set_value:
 * @property: the #CpgProperty
 * @value: the new value
 *
 * Change the value to a specific number.
 *
 **/
void
cpg_property_set_value (CpgProperty  *property,
                        gdouble       value)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	cpg_expression_set_value (property->priv->expression, value);
}

/**
 * cpg_property_get_value:
 * @property: the #CpgProperty
 *
 * Get the numerical value of the current value of the property
 *
 * Return value: the numerical value of the property's current value
 *
 **/
gdouble
cpg_property_get_value (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	if (property->priv->expression)
	{
		return cpg_expression_evaluate (property->priv->expression);
	}
	else
	{
		return 0;
	}
}

gdouble
cpg_property_get_last_value (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	return property->priv->last_value;
}

void
cpg_property_update_last_value (CpgProperty *property)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	property->priv->last_value = cpg_property_get_value (property);
}

/**
 * cpg_property_get_expression:
 * @property: a #CpgProperty
 *
 * Get the property value expression
 *
 * Returns: (transfer none): a #CpgExpression. The expression is owned by the
 *                          property and should not be freed
 *
 **/
CpgExpression *
cpg_property_get_expression (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->expression;
}

/**
 * cpg_property_set_expression:
 * @property: a #CpgProperty
 * @expression: the expression
 *
 * Set the property value from an expression.
 *
 **/
void
cpg_property_set_expression (CpgProperty   *property,
                             CpgExpression *expression)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	set_expression (property, expression);
}

/**
 * cpg_property_get_name:
 * @property: a #CpgProperty
 *
 * Get the property name
 *
 * Returns: (transfer none): the property name
 *
 **/
gchar const *
cpg_property_get_name (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->name;
}

/**
 * cpg_property_set_name:
 * @property: A #CpgProperty
 * @name: The new property name
 *
 * Set a new name for a property.
 *
 * Returns: %TRUE if the name could be successfully changed, %FALSE otherwise
 *
 **/
gboolean
cpg_property_set_name (CpgProperty *property,
                       gchar const *name)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return set_name (property, name);
}

/**
 * cpg_property_get_integrated:
 * @property: a #CpgProperty
 *
 * Get whether the property should be integrated during evaluation or not. This
 * is a convenience function that simply checks if the
 * CPG_PROPERTY_FLAG_INTEGRATED flag is set.
 *
 * Returns: %TRUE if the property will be integrated, %FALSE otherwise
 *
 **/
gboolean
cpg_property_get_integrated (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);

	return property->priv->flags & CPG_PROPERTY_FLAG_INTEGRATED;
}

/**
 * cpg_property_set_integrated:
 * @property: a #CpgProperty
 * @integrated: integrate the property
 *
 * Set whether the property should be integrated during evaluation or not. This
 * is a convenience function that simply sets or unsets the
 * CPG_PROPERTY_FLAG_INTEGRATED flag.
 *
 **/
void
cpg_property_set_integrated (CpgProperty  *property,
                             gboolean      integrated)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	if (integrated)
	{
		cpg_property_add_flags (property, CPG_PROPERTY_FLAG_INTEGRATED);
	}
	else
	{
		cpg_property_remove_flags (property, CPG_PROPERTY_FLAG_INTEGRATED);
	}
}

/**
 * cpg_property_reset:
 * @property: A #CpgProperty
 *
 * Reset the property. This will reset the value of the property to the
 * stored string representation.
 *
 **/
void
cpg_property_reset (CpgProperty *property)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	cpg_expression_reset (property->priv->expression);
	cpg_expression_set_once (property->priv->expression,
	                         (property->priv->flags & CPG_PROPERTY_FLAG_ONCE) != 0);
}

/**
 * cpg_property_equal:
 * @property: a #CpgProperty
 * @other: a #CpgProperty
 *
 * Compare two properties for equal values/expressions
 *
 * Returns: %TRUE if the properties are equal, %FALSE otherwise
 *
 **/
gboolean
cpg_property_equal (CpgProperty *property,
                    CpgProperty *other)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);
	g_return_val_if_fail (CPG_IS_PROPERTY (other), FALSE);

	return property->priv->flags == other->priv->flags &&
	       cpg_expression_equal (cpg_property_get_expression (property),
	                             cpg_property_get_expression (other));
}

/**
 * cpg_property_get_flags:
 * @property: A #CpgProperty
 * 
 * The property flags. The flags contains information on the type of property (
 * such as in, or out).
 *
 * Returns: A #CpgPropertyFlags
 *
 **/
CpgPropertyFlags
cpg_property_get_flags (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), CPG_PROPERTY_FLAG_NONE);

	return property->priv->flags;
}

/**
 * cpg_property_set_flags:
 * @property: A #CpgProperty
 * @flags: A #CpgPropertyFlags
 * 
 * Set the property flags.
 *
 **/
void
cpg_property_set_flags (CpgProperty      *property,
                        CpgPropertyFlags  flags)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_flags (property, flags);
}

/**
 * cpg_property_add_flags:
 * @property: A #CpgProperty
 * @flags: A #CpgPropertyFlags
 * 
 * Add a flags flag to the property flagss.
 *
 **/
void
cpg_property_add_flags (CpgProperty      *property,
                        CpgPropertyFlags  flags)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_flags (property, property->priv->flags | flags);
}

/**
 * cpg_property_remove_flags:
 * @property: A #CpgProperty
 * @flags: A #CpgPropertyFlags
 * 
 * Remove a flags flag from the property flagss.
 *
 **/
void
cpg_property_remove_flags (CpgProperty      *property,
                           CpgPropertyFlags  flags)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_flags (property, property->priv->flags & ~flags);
}

/**
 * cpg_property_set_update:
 * @property: A #CpgProperty
 * @value: The update value
 * 
 * Set the update value of the property. The update value is used to store the
 * result of differential equations on the property/ You normally do not need
 * to use this function.
 *
 **/
void
cpg_property_set_update (CpgProperty  *property,
                         gdouble       value)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	property->priv->update = value;
}

/**
 * cpg_property_get_update:
 * @property: A #CpgProperty
 * 
 * Get the update value of a property. The update value is used to store the
 * result of differential equations on the property. You normally do not need
 * to use this function.
 *
 * Returns: The update value
 *
 **/
gdouble
cpg_property_get_update (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	return property->priv->update;
}

/**
 * cpg_property_flags_to_string:
 * @flags: A #CpgPropertyFlags
 *
 * Convert flags to a string representation.
 *
 * Returns: the string representation of the flags
 *
 **/
gchar *
cpg_property_flags_to_string (CpgPropertyFlags flags)
{
	GFlagsClass *klass;
	guint i;

	klass = g_type_class_ref (CPG_TYPE_PROPERTY_FLAGS);
	GPtrArray *attrs;

	attrs = g_ptr_array_new ();

	for (i = 0; i < klass->n_values; ++i)
	{
		GFlagsValue *value = &(klass->values[i]);

		if (flags & value->value)
		{
			g_ptr_array_add (attrs, (gpointer)value->value_nick);
		}
	}

	g_ptr_array_add (attrs, NULL);

	gchar **vals = (gchar **)g_ptr_array_free (attrs, FALSE);
	gchar *ret = g_strjoinv (" | ", vals);
	g_free (vals);

	g_type_class_unref (klass);

	return ret;
}

/**
 * cpg_property_flags_from_string:
 * @flags: The flags to parse
 *
 * Parse a string into a set of property flags. The flags can be specified
 * by their nicks (none, in, out, once, integrated) and separated by any
 * combination of spaces, comma's and/or pipes.
 *
 * Returns: A #CpgPropertyFlags
 *
 **/
CpgPropertyFlags
cpg_property_flags_from_string (gchar const *flags)
{
	CpgPropertyFlags ret = CPG_PROPERTY_FLAG_NONE;
	GFlagsClass *klass = g_type_class_ref (CPG_TYPE_PROPERTY_FLAGS);

	gchar **parts = g_strsplit_set (flags, ",| ", -1);
	gchar **ptr = parts;

	while (ptr && *ptr)
	{
		GFlagsValue *value = g_flags_get_value_by_nick (klass, *ptr);

		if (value)
		{
			ret |= value->value;
		}

		++ptr;
	}

	g_strfreev (parts);
	g_type_class_unref (klass);

	return ret;
}

/**
 * cpg_property_get_full_name:
 * @property: A #CpgProperty
 *
 * Get the full name of the property. This is the name that can be used in the
 * outer most parent to refer to this property (i.e.
 * <code>cpg_group_find_property (top_parent, cpg_property_get_full_name (deep_property)) == deep_property</code>)
 *
 * Returns: The full name of the property. This is a newly allocated string that
 *          should be freed with g_free.
 *
 **/
gchar *
cpg_property_get_full_name (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	if (!property->priv->object)
	{
		return g_strdup (property->priv->name);
	}

	gchar *objid = cpg_object_get_full_id (property->priv->object);
	gchar *ret = g_strconcat (objid, ".", property->priv->name, NULL);
	g_free (objid);

	return ret;
}

/**
 * cpg_property_copy:
 * @property: A #CpgProperty
 *
 * Make a copy of @property.
 *
 * Returns: (transfer full): A #CpgProperty
 *
 **/
CpgProperty *
cpg_property_copy (CpgProperty *property)
{
	CpgProperty *ret;

	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	ret = cpg_property_new (property->priv->name,
	                        cpg_expression_get_as_string (property->priv->expression),
	                        property->priv->flags);

	ret->priv->update = property->priv->update;

	cpg_modifiable_set_modified (CPG_MODIFIABLE (ret),
	                             property->priv->modified);

	return ret;
}

void
_cpg_property_set_object (CpgProperty *property,
                          CpgObject   *object)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));
	g_return_if_fail (object == NULL || CPG_IS_OBJECT (object));

	set_object (property, object);
}
