#include <stdlib.h>
#include <string.h>

#include "cpg-object-private.h"
#include "cpg-link-private.h"
#include "cpg-state-private.h"
#include "cpg-expression-private.h"

#include "cpg-utils.h"
#include "cpg-debug.h"

/* interface implementations */
static void
cpg_object_update_impl(CpgObject *object, float timestep)
{
	unsigned i;

	// Copy from update to actual
	for (i = 0; i < object->num_actors; ++i)
	{
		CpgProperty *property = object->actors[i];
		double value;
			
		if (property->integrated)
			value = cpg_expression_evaluate(property->value) + property->update * timestep;
		else
			value = property->update;

		cpg_debug_evaluate("Updating %s.%s (%d) = %f (from %f)", object->id, property->name, property->integrated, value, cpg_expression_evaluate(property->value));
		cpg_expression_set_value(property->value, value);
	}
}

static void
cpg_object_evaluate_impl(CpgObject *object, float timestep)
{
	unsigned i;

	// Prepare update values (ready for accumulation)
	for (i = 0; i < object->num_actors; ++i)
		object->actors[i]->update = 0.0;
	
	// Iterate over all the links
	for (i = 0; i < object->num_links; ++i)
	{
		CpgLink *link = object->links[i];
		unsigned e;
		unsigned size;
		
		CpgLinkAction **actions = cpg_link_actions(link, &size);
		
		// Iterate over all the expressions in the link
		for (e = 0; e < size; ++e)
		{
			CpgExpression *expression = cpg_link_action_expression(actions[e]);
			
			// Evaluate expression and add value to the update
			double val = cpg_expression_evaluate(expression);
			cpg_link_action_target(actions[e])->update += val;
		}
	}
}

static void
cpg_object_reset_impl(CpgObject *object)
{
	unsigned i;
	
	for (i = 0; i < object->num_properties; ++i)
	{
		CpgProperty *property = object->properties[i];
		
		// Free current value
		if (property->value)
			cpg_expression_free(property->value);

		// Copy initial expression back to current value
		property->value = cpg_expression_copy(property->initial);
	}
}

/**
 * cpg_object_initialize:
 * @object: the #CpgObject
 * @type: type object type
 *
 * Initializes the #CpgObject. This is a private function and is used by
 * subclasses of #CpgObject such as #CpgState and #CpgLink.
 *
 **/
CpgObject *
cpg_object_initialize(CpgObject *object, CpgObjectType type, char const *id)
{
	object->id = id ? cpg_strdup(id) : NULL;
	object->type = type;
	object->properties = NULL;
	object->num_properties = 0;
	
	object->actors = NULL;
	object->num_actors = 0;
	
	object->links = NULL;
	object->num_links = 0;
	
	object->evaluate = cpg_object_evaluate_impl;
	object->update = cpg_object_update_impl;
	object->reset = cpg_object_reset_impl;
	
	return object;
}

/**
 * cpg_object_new:
 *
 * Create new empty #CpgObject
 *
 * Return value: a newly created #CpgObject
 *
 **/
CpgObject *
cpg_object_new(char const *id)
{
	CpgObject *res = cpg_new1(CpgObject);
	cpg_object_initialize(res, CPG_OBJECT_TYPE_NONE, id);

	return res;
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
cpg_object_add_property(CpgObject *object, char const *name, char const *expression, char integrated)
{
	CpgProperty *property = cpg_property_new(name, expression, integrated, object);
	
	array_resize(object->properties, CpgProperty *, ++object->num_properties);
	object->properties[object->num_properties - 1] = property;
	
	return property;
}

void
cpg_object_destroy(CpgObject *object)
{
	if (!object)
		return;

	unsigned i;

	for (i = 0; i < object->num_properties; ++i)
		cpg_property_free(object->properties[i]);

	free(object->properties);
	free(object->links);
	free(object->actors);
	
	if (object->id)
		free(object->id);
}

void
cpg_object_free(CpgObject *object)
{
	if (!object)
		return;

	cpg_object_destroy(object);
	free(object);
}

CpgProperty *
cpg_object_property(CpgObject *object, char const *name)
{
	unsigned i;
	
	for (i = 0; i < object->num_properties; ++i)
	{
		if (strcmp(object->properties[i]->name, name) == 0)
			return object->properties[i];
	}
	
	return NULL;
}

CpgProperty **
cpg_object_properties(CpgObject *object, unsigned *size)
{
	*size = object->num_properties;
	
	return object->properties;
}

static void
add_actor(CpgObject *object, CpgProperty *property)
{
	unsigned i;
	
	for (i = 0; i < object->num_actors; ++i)
		if (object->actors[i] == property)
			return;
	
	array_resize(object->actors, CpgProperty *, ++object->num_actors);
	object->actors[object->num_actors - 1] = property;
}

/**
 * cpg_object_update_link:
 * @object: the #CpgObject
 * @link: the #CpgLink which links to this object
 *
 * Registers new actors for the object
 *
 **/
void
cpg_object_update_link(CpgObject *object, CpgLink *link)
{
	unsigned i;
	unsigned size;
	
	CpgLinkAction **actions = cpg_link_actions(link, &size);
	
	for (i = 0; i < size; ++i)
		add_actor(object, cpg_link_action_target(actions[i]));
}

/**
 * cpg_object_link:
 * @object: the #CpgObject
 * @link: the #CpgLink which links to this object
 *
 * Adds @link as a link which targets the object (link will be evaluated in
 * #cpg_object_evaluate). If the link introduces new actors, they will be
 * automatically registered.
 *
 **/
void
cpg_object_link(CpgObject *object, CpgLink *link)
{
	array_resize(object->links, CpgLink *, ++object->num_links);
	object->links[object->num_links - 1] = link;
	
	// register possible new actors
	cpg_object_update_link(object, link);
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
cpg_object_update(CpgObject *object, float timestep)
{
	if (object->update)
		object->update(object, timestep);
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
cpg_object_evaluate(CpgObject *object, float timestep)
{
	if (object->evaluate)
		object->evaluate(object, timestep);
}

/**
 * cpg_object_reset:
 * @object: the #CpgObject
 *
 * Reset all properties to their initial values
 *
 **/
void
cpg_object_reset(CpgObject *object)
{
	if (object->reset)
		object->reset(object);
}

CpgObjectType
cpg_object_type(CpgObject *object)
{
	return object->type;
}

char const *
cpg_object_id(CpgObject *object)
{
	return object->id;
}

char *
cpg_object_local_id(CpgObject *object)
{
	if (!object->id)
		return NULL;

	char *last = strrchr(object->id, '.');
	
	if (!last)
		return cpg_strdup(object->id);
	else
		return cpg_strdup(last + 1);
}
