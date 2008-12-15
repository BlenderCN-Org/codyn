#include <string.h>

#include "cpg-object-private.h"
#include "cpg-relay-private.h"
#include "cpg-utils.h"
#include "cpg-debug.h"

static void
ensure_dependencies(CpgRelay *relay, CpgExpression *expression, float timestep)
{
	unsigned i;
	unsigned size;
			
	CpgProperty **dependencies = cpg_expression_dependencies(expression, &size);
			
	for (i = 0; i < size; ++i)
	{
		CpgObject *obj = cpg_property_object(dependencies[i]);
		
		if (obj->type == CPG_OBJECT_TYPE_RELAY)
			cpg_object_evaluate(obj, timestep);
	}
}

static void
cpg_relay_evaluate_impl(CpgObject *object, float timestep)
{
	CpgRelay *relay = (CpgRelay *)object;
	unsigned i;
	
	if (relay->done)
		return;
	
	// Set this first to avoid cyclic loops
	relay->done = 1;

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
			
			// Ensure relay dependencies
			ensure_dependencies(relay, expression, timestep);				
			
			// Evaluate expression and add value to the update
			double val = cpg_expression_evaluate(expression);
			cpg_link_action_target(actions[e])->update += val;
		}
	}
	
	// instantly set values, that's what the relay does
	for (i = 0; i < object->num_actors; ++i)
		cpg_property_set_value(object->actors[i], object->actors[i]->update);
}

/**
 * cpg_relay_new:
 * @name: the name of the relay
 *
 * Returns a newly created #CpgRelay object
 *
 * Return value: a new #CpgRelay object
 *
 **/
CpgRelay *
cpg_relay_new(char const *id)
{
	CpgRelay *relay = cpg_object_create(CpgRelay, CPG_OBJECT_TYPE_RELAY, id);
	
	((CpgObject *)relay)->evaluate = cpg_relay_evaluate_impl;
	return relay;
}

/**
 * cpg_relay_free:
 * @state: the #CpgRelay
 *
 * Destroy the #CpgRelay object
 *
 **/
void
cpg_relay_free(CpgRelay *relay)
{
	if (!relay)
		return;

	cpg_object_destroy(&relay->parent);
	free(relay);
}
