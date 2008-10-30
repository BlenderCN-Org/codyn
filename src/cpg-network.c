#include <stdio.h>
#include <string.h>

#include "cpg-network.h"
#include "cpg-utils.h"
#include "cpg-state.h"
#include "cpg-link.h"
#include "cpg-debug.h"

#define BUFFER_SIZE 4096
#define MONITOR_GROW_SIZE 1000

typedef struct _CpgMonitor CpgMonitor;

struct _CpgMonitor
{
	CpgObject *object;
	CpgProperty *property;
	
	double *values;
	unsigned value_ptr;
	unsigned num_values;

	CpgMonitor *next;
};

struct _CpgNetwork
{
	char *filename;
	
	/* states */
	CpgState **states;
	unsigned num_states;
	
	/* links */
	CpgLink **links;
	unsigned num_links;
	
	/* simulation */
	float timestep;
	float time;
	int compiled;
	
	/* monitors */
	CpgMonitor *monitors;
};


static char *
read_line(FILE *f)
{
	char *buffer = cpg_new(char, BUFFER_SIZE);
	char *ret = fgets(buffer, BUFFER_SIZE, f);
	
	if (!ret)
	{
		free(buffer);
	}
	else
	{
		int len = strlen(buffer);
		
		if (buffer[len - 1] == '\n')
			buffer[len - 1] = '\0';
	}
	
	return ret;
}

static void
cpg_monitor_grow(CpgMonitor *monitor)
{
	monitor->num_values += MONITOR_GROW_SIZE;
	monitor->values = (double *)realloc(monitor->values, sizeof(double) * monitor->num_values);
}

static CpgMonitor *
cpg_monitor_new(CpgObject *object, CpgProperty *property)
{
	CpgMonitor *res = cpg_new1(CpgMonitor);
	
	res->object = object;
	res->property = property;

	res->num_values = 0;
	res->values = NULL;
	res->value_ptr = 0;
	
	// initialize values list
	cpg_monitor_grow(res);
	
	return res;
}

static void
cpg_monitor_free(CpgMonitor *monitor)
{
	if (monitor->values)
		free(monitor->values);
	
	free(monitor);
}

static void
read_headers(CpgNetwork *network, FILE *f)
{
	char *buffer;

	while ((buffer = read_line(f)))
	{
		if (*buffer != '#')
		{
			fseek(f, -strlen(buffer) - 1, SEEK_CUR);
			free(buffer);

			break;
		}
		
		// just ignore headers for now
		free(buffer);
	}
}

static char *
read_tab_sep(FILE *f, char **first, char **second)
{
	if (feof(f))
		return NULL;
	
	char *buffer = read_line(f);
	
	if (!buffer)
		return NULL;
	
	char *pos = strchr(buffer, '\t');
	
	if (pos)
	{
		*pos = '\0';
		*first = buffer;
		*second = pos + 1;
	}
	else
	{
		*first = NULL;
		*second = NULL;
	}

	return buffer;
}

static void
read_properties(CpgObject *object, FILE *f)
{
	char *buffer;
	char *first;
	char *second;
	
	while ((buffer = read_tab_sep(f, &first, &second)))
	{
		if (*buffer == '\0')
		{
			free(buffer);
			break;
		}
		
		char *pos = strchr(second, '\t');
		char integrated = 0;
		
		if (pos)
		{
			*pos = '\0';
			integrated = (*(pos + 1) != '0');
		}
		
		cpg_object_add_property(object, first, second, integrated);
		free(buffer);
	}
}

static void
read_expressions(CpgLink *link, FILE *f)
{
	char *buffer;
	char *first;
	char *second;
	
	while ((buffer = read_tab_sep(f, &first, &second)))
	{
		if (*buffer == '\0')
		{
			free(buffer);
			break;
		}
		
		CpgProperty *property = cpg_object_get_property(link->to, first);
		
		if (property)
			cpg_link_add_action(link, property, second);
		else
			fprintf(stderr, "Could not find property `%s' to act on", first);

		free(buffer);
	}
}

static CpgObject *
read_state(CpgNetwork *network, FILE *f)
{
	char *buffer;
	
	// read in the state object id
	buffer = read_line(f);
	
	CpgState *state = cpg_state_new(buffer);
	free(buffer);
	
	// read in properties
	read_properties((CpgObject *)state, f);
	
	return (CpgObject *)state;
}

static CpgObject *
read_link(CpgNetwork *network, FILE *f)
{
	char *from;
	char *to;
	
	// read from
	from = read_line(f);
	to = read_line(f);
	
	CpgState *fromobj = cpg_network_get_state_by_name(network, from);
	CpgState *toobj = cpg_network_get_state_by_name(network, to);
	
	if (!fromobj || !toobj)
	{
		fprintf(stderr, "Could not find object `%s' for link\n", !fromobj ? from : to);

		if (from)
			free(from);
		
		if (to)
			free(to);

		return NULL;
	}
	
	CpgLink *link = cpg_link_new((CpgObject *)fromobj, (CpgObject *)toobj);
	
	// read properties
	read_properties((CpgObject *)link, f);

	// read expressions
	read_expressions(link, f);
	
	free(from);
	free(to);
	
	return (CpgObject *)link;
}

static void
skip_until_separator(FILE *f)
{
	char *buffer;
	int nl = 0;
	
	while ((buffer = read_line(f)))
	{
		if (*buffer == '\0')
			++nl;
		else
			nl = 0;
		
		free(buffer);
		
		if (nl == 2)
			break;
	}
}

static int
parse_expressions(CpgObject *object)
{
	unsigned i;
	
	// Parse all property value expressions
	for (i = 0; i < object->num_properties; ++i)
	{
		CpgProperty *property = object->properties[i];
		char *error;

		if (!cpg_expression_parse(property->initial, object, &error))
		{
			fprintf(stderr, "Error while parsing expression: %s\n", error);
			free(error);
			return 0;
		}
	}
	
	cpg_object_reset(object);
	
	if (!CPG_OBJECT_IS_LINK(object))
		return 1;

	// Parse all link expressions	
	CpgLink *link = (CpgLink *)object;
	unsigned e;
	
	for (e = 0; e < link->num_expressions; ++e)
	{
		char *error;
		
		if (!cpg_expression_parse(link->expressions[e], (CpgObject *)link, &error))
		{
			fprintf(stderr, "Error while parsing expression: %s\n", error);
			free(error);
			return 0;
		}
	}
	
	return 1;
}

static void
add_state(CpgNetwork *network, CpgState *state)
{
	network->states = (CpgState **)realloc(network->states, sizeof(CpgState *) * (++network->num_states));
	network->states[network->num_states - 1] = state;
}

static void
add_link(CpgNetwork *network, CpgLink *link)
{
	network->links = (CpgLink **)realloc(network->links, sizeof(CpgLink *) * (++network->num_links));
	network->links[network->num_links - 1] = link;
}

/**
 * cpg_network_add_object:
 * @network: the #CpgNetwork
 * @object: the #CpgObject to add
 *
 * Adds a new object to the network (either #CpgLink or #CpgState). Make sure
 * to recompile the network after the object is added.
 *
 */
void
cpg_network_add_object(CpgNetwork *network, CpgObject *object)
{
	// add object to the network
	if (CPG_OBJECT_IS_STATE(object))
		add_state(network, (CpgState *)object);
	else
		add_link(network, (CpgLink *)object);
	
	network->compiled = 0;
}

static int
read_object(CpgNetwork *network, FILE *f)
{
	// read in type of object
	char *buffer = read_line(f);
	CpgObject *object = NULL;
	
	if (!buffer)
		return 0;
	
	if (strcmp(buffer, "state") == 0)
		object = read_state(network, f);
	else if (strcmp(buffer, "link") == 0)
		object = read_link(network, f);
	else
		skip_until_separator(f);
	
	free(buffer);

	if (object)
		cpg_network_add_object(network, object);
	
	return 1;
}

CpgState *
cpg_network_get_state_by_name(CpgNetwork *network, char const *name)
{
	int i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgState *state = network->states[i];
		
		if (strcmp(state->name, name) == 0)
			return state;
	}
	
	return NULL;
}

/**
 * cpg_network_states:
 * @network: the #CpgNetwork
 * @size: return value for the size of the list of states
 *
 * Retrieves the list of states. This list is managed internally by the network
 * and should therefore not be changed or freed
 *
 * Return value: a list of #CpgState
 *
 **/
CpgState * const *
cpg_network_states(CpgNetwork *network, unsigned *size)
{
	if (size)
		*size = network->num_states;
	
	return network->states;
}

/**
 * cpg_network_links:
 * @network: the #CpgNetwork
 * @size: return value for the size of the list of links
 *
 * Retrieves the list of links. This list is managed internally by the network
 * and should therefore not be changed or freed
 *
 * Return value: a list of #CpgLink
 *
 **/
CpgLink * const *
cpg_network_links(CpgNetwork *network, unsigned *size)
{
	if (size)
		*size = network->num_links;
	
	return network->links;
}

/**
 * cpg_network_taint:
 * @network: the #CpgNetwork
 *
 * Set the network in an uncompiled state, forcing it to recompile at the next
 * simulation step or run (or the network can be recompiled manually with 
 * #cpg_network_compile)
 *
 **/
void
cpg_network_taint(CpgNetwork *network)
{
	network->compiled = 0;
}

/**
 * cpg_network_compile:
 * @network: the #CpgNetwork
 *
 * Recompile all expressions for all states and links. You should do this
 * after you've added new objects to the network. If a simulation is ran while
 * the network is in an uncompiled state, it will be compiled first.
 *
 * Return value: 1 if compilation was successful, 0 otherwise
 *
 **/
int
cpg_network_compile(CpgNetwork *network)
{
	unsigned i;
	
	network->compiled = 0;

	for (i = 0; i < network->num_states; ++i)
	{
		if (!parse_expressions((CpgObject *)(network->states[i])))
			return 0;
	}
	
	for (i = 0; i < network->num_links; ++i)
	{
		if (!parse_expressions((CpgObject *)(network->links[i])))
			return 0;
	}
	
	network->compiled = 1;
	return 1;
}

/**
 * cpg_network_new:
 * 
 * Create a new empty CPG network
 *
 * Return value: the newly created CPG network
 **/
CpgNetwork *
cpg_network_new()
{
	CpgNetwork *network = cpg_new1(CpgNetwork);

	network->filename = NULL;
	network->states = NULL;
	network->links = NULL;

	network->num_states = 0;
	network->num_links = 0;
	
	network->time = 0;
	network->timestep = 0;
	network->compiled = 0;
	
	network->monitors = NULL;
}

/**
 * cpg_network_new_from_file:
 * @filename: the filename of the file containing the network definition
 * 
 * Create a new CPG network by reading the network definition from file
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 **/
CpgNetwork *
cpg_network_new_from_file(char const *filename)
{
	FILE *f = fopen(filename, "r");
	
	if (!f)
		return NULL;

	CpgNetwork *network = cpg_network_new();

	network->filename = strdup(filename);
	
	// read in headers
	read_headers(network, f);
	
	// rest of the file consists of objects
	while (!feof(f))
	{
		// read object
		if (!read_object(network, f))
		{
			cpg_network_free(network);
			fclose(f);
			return NULL;
		}
	}
	
	fclose(f);
	
	if (!cpg_network_compile(network))
	{
		cpg_network_free(network);
		return NULL;
	}
	
	return network;
}

/**
 * cpg_network_clear:
 * @network: the #CpgNetwork
 *
 * Clears the network (removes all objects). Any monitors still active are
 * also removed.
 *
 **/
void
cpg_network_clear(CpgNetwork *network)
{
	// remove all states
	unsigned i;
	for (i = 0; i < network->num_states; ++i)
		cpg_state_free(network->states[i]);
	
	// remove all links
	for (i = 0; i < network->num_links; ++i)
		cpg_link_free(network->links[i]);
	
	if (network->states)
		free(network->states);
	
	if (network->links)
		free(network->links);
	
	network->states = NULL;
	network->links = NULL;

	network->num_states = 0;
	network->num_links = 0;

	CpgMonitor *monitor = network->monitors;
	
	while (monitor)
	{
		CpgMonitor *next = monitor->next;
		cpg_monitor_free(monitor);
		
		monitor = next;
	}
	
	network->monitors = NULL;
}

/**
 * cpg_network_free:
 * @network: the #CpgNetwork
 *
 * Destroy CPG network
 *
 **/
void
cpg_network_free(CpgNetwork *network)
{
	if (network->filename)
		free(network->filename);
	
	cpg_network_clear(network);

	free(network);	
}

/* simulation functions */
static void
simulation_evaluate(CpgNetwork *network)
{
	unsigned i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgObject *object = (CpgObject *)(network->states[i]);
		cpg_object_evaluate(object, network->timestep);
	}
}

static void
simulation_update(CpgNetwork *network)
{
	unsigned i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgObject *object = (CpgObject *)(network->states[i]);
		cpg_object_update(object, network->timestep);
	}
}

/**
 * cpg_network_simulation_step:
 * @network: the #CpgNetwork
 * @timestep: the integration timestep
 * 
 * Perform one step of simulation given the specified @timestep
 *
 **/
void
cpg_network_simulation_step(CpgNetwork *network, float timestep)
{
	if (!network->compiled)
		cpg_network_compile(network);

	network->timestep = timestep;
	
	cpg_debug_evaluate("%s", "Simulation step");
	simulation_evaluate(network);
	simulation_update(network);
	
	network->time += timestep;
}

/**
 * cpg_network_simulation_run:
 * @network: the #CpgNetwork
 * @from: the simulation start time
 * @timestep: the integration time step to simulate with
 * @to: the simulation end time
 *
 * Perform a period of simulation. The period is determined by from, timestep
 * and to as described above.
 *
 **/
void
cpg_network_simulation_run(CpgNetwork *network, float from, float timestep, float to)
{
	if (!network->compiled && !cpg_network_compile(network))
		return;

	if (from >= to)
	{
		fprintf(stderr, "** Error: Invalid range specified, from has to be smaller than to\n");
		return;
	}
	
	if (timestep <= 0)
	{
		fprintf(stderr, "** Error: Timestep has to be larger than 0\n");
		return;
	}
	
	network->time = from;
	
	while (network->time < to)
		cpg_network_simulation_step(network, timestep);
}

/**
 * cpg_network_simulation_reset:
 * @network: the #CpgNetwork
 *
 * Reset the CPG network to its original values. This will reset the time
 * to 0 and for all objects in the network will reset all properties to the
 * initial value.
 *
 **/
void
cpg_network_simulation_reset(CpgNetwork *network)
{
	// set time back to 0
	network->time = 0;
	
	// reset all objects
	unsigned i;
	for (i = 0; i < network->num_states; ++i)
		cpg_object_reset((CpgObject *)(network->states[i]));

	for (i = 0; i < network->num_links; ++i)
		cpg_object_reset((CpgObject *)(network->links[i]));

}

/* monitoring */
static CpgMonitor *
monitor_find(CpgNetwork *network, CpgObject *object, CpgProperty *property)
{
	CpgMonitor *monitor;
	
	for (monitor = network->monitors; monitor; monitor = monitor->next)
		if (monitor->object == object && monitor->property == property)
			return monitor;
	
	return NULL;
}

static CpgMonitor *
monitor_add(CpgNetwork *network, CpgObject *object, CpgProperty *property)
{
	// see if monitor is already present
	CpgMonitor *monitor = monitor_find(network, object, property);
	
	if (monitor)
		return;

	monitor = cpg_monitor_new(object, property);

	monitor->next = network->monitors;
	network->monitors = monitor;
	
	return monitor;
}

void
cpg_network_set_monitor(CpgNetwork *network, CpgObject *object, char const *propname)
{
	CpgProperty *property = cpg_object_get_property(object, propname);
	
	if (!property)
		return;
	
	monitor_add(network, object, property);	
}

void
cpg_network_unset_monitor(CpgNetwork *network, CpgObject *object, char const *propname)
{
	CpgProperty *property = cpg_object_get_property(object, propname);
	
	if (!property)
		return;

	CpgMonitor *monitor = monitor_find(network, object, property);
	
	if (!monitor)
		return;
	
	CpgMonitor *other;
	CpgMonitor *prev = NULL;

	for (other = network->monitors; other; ++other)
	{
		if (other != monitor)
			continue;
			
		if (prev)
			prev->next = monitor->next;
		else
			network->monitors = monitor->next;

		break;
	}
	
	cpg_monitor_free(monitor);
}

double const *
cpg_network_monitor_data(CpgNetwork *network, CpgObject  *object, char const *propname, unsigned *size)
{
	CpgProperty *property = cpg_object_get_property(object, propname);
	
	if (size)
		*size = 0;

	if (!property)
		return NULL;

	CpgMonitor *monitor = monitor_find(network, object, property);
	
	if (!monitor)
		return NULL;
	
	if (size)	
		*size = monitor->value_ptr;

	return monitor->values;
}
