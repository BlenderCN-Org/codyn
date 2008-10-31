#include <string.h>

#include "cpg-property.h"
#include "cpg-expression.h"
#include "cpg-utils.h"

/**
 * cpg_property_new:
 * @name: the property name
 * @expression: the initial value expression
 * @integrated: whether this property should be integated during the simulation
 *
 * Create a new property object. Property objects are assigned to #CpgObject
 * objects and are of little use on their own. The provided expression will
 * not be parsed initially.
 *
 **/
CpgProperty *
cpg_property_new(char const *name, char const *expression, char integrated)
{
	CpgProperty *res = cpg_new1(CpgProperty);
	
	res->name = strdup(name);

	res->value = NULL;
	res->update = 0.0;

	res->integrated = integrated;	
	res->initial = cpg_expression_new(expression);
	
	// set current value to copy of initial value
	res->value = cpg_expression_copy(res->initial);
	
	return res;
}

/**
 * cpg_property_free:
 * @property: the #CpgProperty
 *
 * Destroy the #CpgProperty object
 *
 **/
void
cpg_property_free(CpgProperty *property)
{
	if (!property)
		return;

	cpg_expression_free(property->value);
	cpg_expression_free(property->initial);

	free(property->name);
	free(property);
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
cpg_property_set_value(CpgProperty *property, double value)
{
	if (!property->value)
		return;
	
	cpg_expression_set_value(property->value, value);
}

/**
 * cpg_property_set_initial:
 * @property: the #CpgProperty
 * @value: the new initial value
 *
 * Change the initial value to a specific number. To set the initial value
 * to any expression, use #cpg_object_set_initial.
 *
 **/
void
cpg_property_set_initial(CpgProperty *property, double value)
{
	if (!property->initial)
		return;
	
	cpg_expression_set_value(property->initial, value);
}
