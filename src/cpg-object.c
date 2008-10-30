#include <stdlib.h>
#include <string.h>

#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-utils.h"

void
cpg_object_initialize(CpgObject *object, CpgObjectType type)
{
	object->type = type;
	object->properties = NULL;
	object->num_properties = 0;
	
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

void
cpg_object_link(CpgObject *object, CpgLink *link)
{
	object->links = (CpgLink **)realloc(object->links, sizeof(CpgLink) * ++object->num_links);
	object->links[object->num_links - 1] = link;
}

void
cpg_object_update(CpgObject *object, float timestep)
{
	unsigned i;

	// Copy from update to actual
	for (i = 0; i < object->num_links; ++i)
	{
		CpgLink *link = object->links[i];
		unsigned e;
		
		// Iterate over all the expressions in the link and initialize
		// the update value
		for (e = 0; e < link->num_expressions; ++e)
		{
			CpgProperty *property = link->expressions[e]->destination;
			double value;
			
			if (property->integrated)
				value = cpg_expression_evaluate(property->value) + property->update * timestep;
			else
				value = property->update;

			cpg_expression_set_value(property->value, value);
		}
	}
}

void
cpg_object_evaluate(CpgObject *object, float timestep)
{
	unsigned i;

	// Prepare update values
	for (i = 0; i < object->num_links; ++i)
	{
		CpgLink *link = object->links[i];
		unsigned e;
		
		// Iterate over all the expressions in the link and initialize
		// the update value
		for (e = 0; e < link->num_expressions; ++e)
		{
			CpgProperty *property = link->expressions[e]->destination;
			
			property->update = 0.0;
		}
	}
	
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
		
		// Make sure to copy the expression to the actual value after parsing
		if (property->value)
			cpg_expression_free(property->value);

		property->value = cpg_expression_copy(property->initial);
	}
}
