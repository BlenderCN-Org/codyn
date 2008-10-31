#include <stdlib.h>
#include <string.h>

#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-debug.h"
#include "cpg-state.h"

void
cpg_object_initialize(CpgObject *object, CpgObjectType type)
{
	object->type = type;
	object->properties = NULL;
	object->num_properties = 0;
	
	object->actors = NULL;
	object->num_actors = 0;
	
	object->links = NULL;
	object->num_links = 0;
}

CpgObject *
cpg_object_new()
{
	CpgObject *res = cpg_new1(CpgObject);
	cpg_object_initialize(res, CPG_OBJECT_TYPE_NONE);
	
	return res;
}

void
cpg_object_add_property(CpgObject *object, char const *name, char const *expression, char integrated)
{
	CpgProperty *property = cpg_property_new(name, expression, integrated);
	
	if (CPG_OBJECT_IS_LINK(object))
		property->initial->link = (CpgLink *)object;
	
	object->properties = (CpgProperty **)realloc(object->properties, sizeof(CpgProperty *) * ++object->num_properties);
	object->properties[object->num_properties - 1] = property;
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
cpg_object_get_property(CpgObject *object, char const *name)
{
	unsigned i;
	
	for (i = 0; i < object->num_properties; ++i)
	{
		if (strcmp(object->properties[i]->name, name) == 0)
			return object->properties[i];
	}
	
	return NULL;
}

static void
add_actor(CpgObject *object, CpgProperty *property)
{
	unsigned i;
	
	for (i = 0; i < object->num_actors; ++i)
		if (object->actors[i] == property)
			return;
	
	object->actors = (CpgProperty **)realloc(object->actors, sizeof(CpgProperty) * ++object->num_actors);
	object->actors[object->num_actors - 1] = property;
}

void
cpg_object_update_link(CpgObject *object, CpgLink *link)
{
	unsigned i;
	for (i = 0; i < link->num_expressions; ++i)
	{
		CpgExpression *exp = link->expressions[i];
		
		add_actor(object, exp->destination);
	}
}

void
cpg_object_link(CpgObject *object, CpgLink *link)
{
	object->links = (CpgLink **)realloc(object->links, sizeof(CpgLink) * ++object->num_links);
	object->links[object->num_links - 1] = link;
	
	cpg_object_update_link(object, link);
}

void
cpg_object_update(CpgObject *object, float timestep)
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

		cpg_debug_evaluate("Updating %s.%s (%d) = %f (from %f)", CPG_OBJECT_IS_STATE(object) ? ((CpgState *)object)->name : "link", property->name, property->integrated, value, cpg_expression_evaluate(property->value));
		cpg_expression_set_value(property->value, value);
	}
}

void
cpg_object_evaluate(CpgObject *object, float timestep)
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
		
		// Iterate over all the expressions in the link
		for (e = 0; e < link->num_expressions; ++e)
		{
			CpgExpression *expression = link->expressions[e];
			
			// Evaluate expression and add value to the update
			double val = cpg_expression_evaluate(expression);
			
			cpg_debug_evaluate("Update val for %s.%s: %f", CPG_OBJECT_IS_STATE(object) ? ((CpgState *)object)->name : "link", expression->destination->name, val);
			expression->destination->update += val;
		}
	}
}

void
cpg_object_reset(CpgObject *object)
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

void 
cpg_object_set_value(CpgObject *object, CpgProperty *property, char const *expression)
{
	if (!property->value)
		return;
	
	cpg_expression_set(property->value, expression);
	
	char *error;
	
	if (!cpg_expression_parse(property->value, object, &error))
	{
		fprintf(stderr, "Unable to parse new property value: %s\n", error);
		free(error);
	}
}

void
cpg_object_set_initial(CpgObject *object, CpgProperty *property, char const *expression)
{
	if (!property->initial)
		return;
	
	cpg_expression_set(property->initial, expression);
	
	char *error;
	
	if (!cpg_expression_parse(property->initial, object, &error))
	{
		fprintf(stderr, "Unable to parse new property value: %s\n", error);
		free(error);
	}
}
