#include "shared/cpg-shared-network.h"
#include "shared/cpg-shared-link.h"
#include "cpg-debug.h"

static void
simulation_evaluate(CpgSharedNetwork *network,
					void			 *base)
{
	unsigned i;
	
	// prepare values
	for (i = 0; i < network->num_actors; ++i)
	{
		CpgSharedProperty *property = cpg_shared_array_base_type(base, network->actors, i, CpgSharedProperty);
		property->update = 0.0;
	}
	
	for (i = 0; i < network->num_links; ++i)
	{
		CpgSharedLink *link = cpg_shared_array_base_type(base, network->links, i, CpgSharedLink);
		unsigned a;
		
		for (a = 0; a < link->num_actions; ++a)
		{
			CpgSharedLinkAction *action = cpg_shared_array_base_type(base, link->actions, a, CpgSharedLinkAction);
			CpgSharedProperty *property = cpg_shared_pointer_base_type(base, action->target, CpgSharedProperty);
			
			property->update += cpg_shared_expression_evaluate(&(action->expression), base);			
		}
	}
}

static void
simulation_update(CpgSharedNetwork *network,
				  void			   *base)
{
	unsigned i;
	
	for (i = 0; i < network->num_actors; ++i)
	{
		CpgSharedProperty *property = cpg_shared_array_base_type(base, network->actors, i, CpgSharedProperty);
		double value;
			
		if (property->integrated)
			value = cpg_shared_expression_evaluate(&(property->value), base) + property->update * network->timestep;
		else
			value = property->update;

		cpg_shared_property_set_value(property, value, base);
	}
}

void
cpg_shared_network_simulation_step(CpgSharedNetwork *network, 
								   float			 timestep,
								   void				*base)
{
	network->timestep = timestep;
	cpg_shared_property_set_value(&(network->timestepprop), network->timestep, base);
	
	cpg_debug_evaluate("Simulation step");

	simulation_evaluate(network, base);
	simulation_update(network, base);
}

void
cpg_shared_network_simulation_run(CpgSharedNetwork *network, 
								  float				from, 
								  float				timestep, 
								  float				to,
								  void			   *base)
{
	if (from >= to)
	{
		cpg_debug_error("Invalid range specified, from has to be smaller than to");
		return;
	}
	
	if (timestep <= 0)
	{
		cpg_debug_error("Timestep has to be larger than 0");
		return;
	}
	
	network->time = from;
	cpg_shared_property_set_value(&(network->timeprop), network->time, base);
	
	while (network->time < to - 0.5 * timestep)
		cpg_shared_network_simulation_step(network, timestep, base);
}
