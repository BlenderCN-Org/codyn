#include <string.h>

#include "cpg-property.h"
#include "cpg-ref-counted-private.h"
#include "cpg-expression.h"
#include "cpg-utils.h"
#include "cpg-object.h"

struct _CpgProperty
{
	CpgRefCounted parent;
	guint use_count;
	gchar *name;
	
	CpgExpression *value;
	gboolean integrated;
	
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
	return cpg_property_new (property->name,
	                         cpg_expression_get_as_string (property->value),
	                         property->integrated,
	                         NULL);
}

static void
cpg_property_free (CpgProperty *property)
{
	cpg_ref_counted_unref (property->value);

	g_free (property->name);
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

	res->integrated = integrated;	
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

CpgExpression *
cpg_property_get_value_expression (CpgProperty *property)
{
	return property->value;
}

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

gchar const *
cpg_property_get_name (CpgProperty *property)
{
	return property->name;
}

gboolean
cpg_property_get_integrated (CpgProperty *property)
{
	return property->integrated;
}

void
cpg_property_set_integrated (CpgProperty  *property,
                             gboolean      integrated)
{
	property->integrated = integrated;
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
