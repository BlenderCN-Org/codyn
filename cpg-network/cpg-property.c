#include <string.h>

#include "cpg-property.h"
#include "cpg-ref-counted-private.h"
#include "cpg-expression.h"
#include "cpg-utils.h"

#include "cpg-object.h"

typedef enum
{
	ATTRIBUTE_INTEGRATED = 1 << 0,
	ATTRIBUTE_VARIANT = 1 << 1,
	ATTRIBUTE_OUT = 1 << 2
} PropertyAttribute;

/**
 * SECTION:cpg-property
 * @short_description: Property container
 *
 * A #CpgProperty is a container for a specific variable in an object. It
 * consists of a name and a mathematical expression describing its contents.
 *
 */
 
struct _CpgProperty
{
	CpgRefCounted parent;
	guint use_count;
	gchar *name;
	
	CpgExpression *value;
	PropertyAttribute attributes;
	
	gdouble update;
	CpgObject *object;
};

GType 
cpg_property_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
		type_id = g_boxed_type_register_static ("CpgProperty", cpg_ref_counted_ref, cpg_ref_counted_unref);
	
	return type_id;
}

CpgProperty *
_cpg_property_copy (CpgProperty *property)
{
	CpgProperty *ret;
	
	ret = cpg_property_new (property->name,
	                        cpg_expression_get_as_string (property->value),
	                        FALSE,
	                        NULL);

	ret->attributes = property->attributes;
	return ret;
}

static void
cpg_property_free (CpgProperty *property)
{
	cpg_ref_counted_unref (property->value);

	g_free (property->name);
	g_slice_free (CpgProperty, property);
}

/**
 * cpg_property_new:
 * @name: the property name
 * @expression: the value expression
 * @integrated: whether this property should be integated during the simulation
 * @object: the #CpgObject to which the property belongs
 *
 * Create a new property object. Property objects are assigned to #CpgObject
 * objects and are of little use on their own. The provided expression will
 * not be parsed initially.
 *
 * Returns: the new #CpgProperty
 *
 **/
CpgProperty *
cpg_property_new (gchar const  *name,
                  gchar const  *expression,
                  gboolean      integrated,
                  CpgObject    *object)
{
	CpgProperty *res = g_slice_new0(CpgProperty);
	cpg_ref_counted_init (res, (GDestroyNotify)cpg_property_free);
	
	res->name = g_strdup (name);
	res->object = object;
	res->use_count = 0;

	res->attributes = integrated ? ATTRIBUTE_INTEGRATED : 0;
	res->value = cpg_expression_new (expression);
	
	return res;
}

/**
 * cpg_property_get_object:
 * @property: the #CpgProperty
 *
 * Get the object associated with the property
 *
 * Returns: the object associated with the property
 **/
CpgObject *
cpg_property_get_object (CpgProperty *property)
{
	return property->object;
}

void
_cpg_property_set_object (CpgProperty *property,
                          CpgObject   *object)
{
	property->object = object;
}

/**
 * cpg_property_set_value:
 * @property: the #CpgProperty
 * @value: the new value
 *
 * Change the value to a specific number. To set the value
 * to any expression, use #cpg_object_set_value.
 *
 **/
void
cpg_property_set_value (CpgProperty  *property,
                        gdouble       value)
{
	cpg_expression_set_value (property->value, value);
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
	return property->value ? cpg_expression_evaluate (property->value) : 0.0;
}

/**
 * cpg_property_get_value_expression:
 * @property: a #CpgProperty
 *
 * Get the property value expression
 *
 * Returns: a #CpgExpression. The expression is owned by the property and
 *          should not be freed
 *
 **/
CpgExpression *
cpg_property_get_value_expression (CpgProperty *property)
{
	return property->value;
}

/**
 * cpg_property_set_value_expression:
 * @property: a #CpgProperty
 * @expression: the expression
 *
 * Set the property value from an expression. This will mark the associated
 * #CpgObject as tainted so the expression will be recompiled accordingly.
 *
 **/
void
cpg_property_set_value_expression (CpgProperty  *property,
                                   gchar const  *expression)
{
	cpg_expression_set_from_string (property->value, expression);
	
	if (property->object)
	{
		cpg_object_taint (property->object);
	}
}

/**
 * cpg_property_get_name:
 * @property: a #CpgProperty
 *
 * Get the property name
 *
 * Returns: the property name
 *
 **/
gchar const *
cpg_property_get_name (CpgProperty *property)
{
	return property->name;
}

/**
 * cpg_property_get_integrated:
 * @property: a #CpgProperty
 *
 * Get whether the property should be integrated during evaluation or not
 *
 * Returns: %TRUE if the property will be integrated, %FALSE otherwise
 *
 **/
gboolean
cpg_property_get_integrated (CpgProperty *property)
{
	return property->attributes & ATTRIBUTE_INTEGRATED;
}

/**
 * cpg_property_set_integrated:
 * @property: a #CpgProperty
 * @integrated: integrate the property
 *
 * Set whether the property should be integrated during evaluation or not
 *
 **/
void
cpg_property_set_integrated (CpgProperty  *property,
                             gboolean      integrated)
{
	if (integrated)
	{
		property->attributes |= ATTRIBUTE_INTEGRATED;
	}
	else
	{
		property->attributes &= ~ATTRIBUTE_INTEGRATED;
	}
}

/**
 * cpg_property_get_variant:
 * @property: a #CpgProperty
 *
 * Get whether the property is variant. When optimizing the network, variant
 * properties are not optimized away and can thus be modified at run time
 *
 * Returns: %TRUE if the property is variant, %FALSE otherwise
 *
 **/
gboolean
cpg_property_get_variant (CpgProperty *property)
{
	return property->attributes & ATTRIBUTE_VARIANT;
}

/**
 * cpg_property_set_variant:
 * @property: a #CpgProperty
 * @variant: whether the property is variant
 *
 * Set to %TRUE when the property may not be optimized when the network is
 * optimized.
 *
 **/
void
cpg_property_set_variant (CpgProperty  *property,
                          gboolean      variant)
{
	if (variant)
	{
		property->attributes |= ATTRIBUTE_VARIANT;
	}
	else
	{
		property->attributes &= ~ATTRIBUTE_VARIANT;
	}
}


/**
 * cpg_property_get_out:
 * @property: a #CpgProperty
 *
 * Get whether the property is an output. When optimizing the network, out
 * properties are preserved and can be used to read data back externally
 *
 * Returns: %TRUE if the property is an out, %FALSE otherwise
 *
 **/
gboolean
cpg_property_get_out (CpgProperty *property)
{
	return property->attributes & ATTRIBUTE_OUT;
}

/**
 * cpg_property_set_variant:
 * @property: a #CpgProperty
 * @variant: whether the property is out
 *
 * Set to %TRUE when the property may not be optimized when the network is
 * optimized and should be available externally to read back data
 *
 **/
void
cpg_property_set_out (CpgProperty  *property,
                      gboolean      out)
{
	if (out)
	{
		property->attributes |= ATTRIBUTE_OUT;
	}
	else
	{
		property->attributes &= ~ATTRIBUTE_OUT;
	}
}

void
_cpg_property_set_update (CpgProperty  *property,
                          gdouble       value)
{
	property->update = value;
}

gdouble
_cpg_property_get_update (CpgProperty *property)
{
	return property->update;
}

/**
 * cpg_property_reset_cache:
 * @property: a #CpgProperty
 *
 * Reset the cached value of the property expression
 *
 **/
void
cpg_property_reset_cache (CpgProperty *property)
{
	cpg_expression_reset_cache (property->value);
}

void
_cpg_property_use (CpgProperty *property)
{
	++(property->use_count);
}

gboolean
_cpg_property_unuse (CpgProperty *property)
{
	if (property->use_count == 0)
		return TRUE;	

	return (--(property->use_count) == 0);
}

gboolean 
cpg_property_get_used (CpgProperty *property)
{
	return property->use_count == 0;
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
	return property->attributes == other->attributes &&
	       cpg_expression_equal (cpg_property_get_value_expression (property),
	                             cpg_property_get_value_expression (other));
}
