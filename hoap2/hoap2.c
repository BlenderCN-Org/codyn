#include <stdlib.h>
#include <stdio.h>
#include "cpg-network.h"
#include "cpg-debug.h"
#include "cpg-expression.h"

void
print_instructions(CpgNetwork *network)
{
	unsigned i;
	for (i = 0; i < network->num_states + network->num_links; ++i)
	{
		CpgObject *obj = network->objects[i];
		
		if (CPG_OBJECT_IS_STATE(obj))
			printf("State: %s\n", ((CpgState *)obj)->name);
		else
			printf("Link\n");
		
		unsigned p;
		
		for (p = 0; p < obj->num_properties; ++p)
		{
			CpgProperty *prop = obj->properties[p];
			
			printf("Property: %s, ", prop->name);
			
			if (prop->initial)
				cpg_expression_print_instructions(prop->initial, stdout);
		}
		
		printf("\n");
	}
}

typedef struct
{
	char const *name;
	CpgObject *object;
	double *values;
} Oscillator;

static Oscillator oscillators[] =
{
	{"lankle", NULL, NULL},
	{"lknee", NULL, NULL},
	{"lhip", NULL, NULL},
	{"lankle_l", NULL, NULL},
	{"rankle", NULL, NULL},
	{"rknee", NULL, NULL},
	{"rhip", NULL, NULL},
	{"rankle_l", NULL, NULL}
};

int 
main (int argc, char *argv[])
{
	//cpg_debug_add(CPG_DEBUG_TYPE_EXPRESSION);
	CpgNetwork *network = cpg_network_new_from_file("hoap2.txt");
	
	if (!network)
	{
		printf("Network construction failed!\n");
		exit(1);
	}
	
	unsigned i;
	float ts = 2.0;
	float timestep = 0.001;
	unsigned steps = (unsigned)(ts / timestep);
	
	unsigned o;
	for (o = 0; o < sizeof(oscillators) / sizeof(Oscillator); ++o)
	{
		oscillators[o].values = (double *)malloc(sizeof(double) * steps);
		oscillators[o].object = (CpgObject *)cpg_network_get_state_by_name(network, oscillators[o].name);
	}

	for (i = 0; i < steps; ++i)
	{
		for (o = 0; o < sizeof(oscillators) / sizeof(Oscillator); ++o)
		{
			CpgProperty *property = cpg_object_get_property(oscillators[o].object, "x");
			oscillators[o].values[i] = cpg_expression_evaluate(property->value ? property->value : property->initial);
		}

		cpg_network_simulation_step(network, timestep);		
	}
	
	for (o = 0; o < sizeof(oscillators) / sizeof(Oscillator); ++o)
	{
		Oscillator *osc = &(oscillators[o]);
		char filename[50];
		
		snprintf(filename, 50, "%s.txt", osc->name);
		FILE *f = fopen(filename, "w");
		
		// write all values
		for (i = 0; i < steps; ++i)
			fprintf(f, "%f\t%f\n", i * timestep, osc->values[i]);
		
		fclose(f);
	}

	cpg_network_free(network);
	return 0;
}
