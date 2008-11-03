#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "cpg-object-private.h"
#include "cpg-link-private.h"
#include "cpg-state-private.h"
#include "cpg-expression-private.h"

#include "cpg-network.h"
#include "cpg-utils.h"
#include "cpg-debug.h"

#define BUFFER_SIZE 4096
#define MONITOR_GROW_SIZE 1000

typedef struct _CpgMonitor CpgMonitor;

struct _CpgMonitor
{
	CpgObject *object;
	CpgProperty *property;
	
	double *values;
	double *sites;
	unsigned num_values;
	unsigned size;

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
	
	/* context */
	CpgContext context[3];
	CpgObject *constants;
	CpgProperty *timeprop;
	CpgProperty *timestepprop;
};


static char *
read_line_real(FILE *f, int skip_comments)
{
	char *buffer = cpg_new(char, BUFFER_SIZE);
	char *ret;
	
	while ((ret = fgets(buffer, BUFFER_SIZE, f)) && (skip_comments && *ret == '#'))
		;

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

static char *
read_line(FILE *f)
{
	return read_line_real(f, 1);
}

static void
cpg_monitor_grow(CpgMonitor *monitor)
{
	monitor->size += MONITOR_GROW_SIZE;
	
	array_resize(monitor->values, double, monitor->size);
	array_resize(monitor->sites, double, monitor->size);
}

static CpgMonitor *
cpg_monitor_new(CpgObject *object, CpgProperty *property)
{
	CpgMonitor *res = cpg_new1(CpgMonitor);
	
	res->object = object;
	res->property = property;

	res->num_values = 0;
	res->values = NULL;
	res->size = 0;
	res->sites = NULL;
	
	// initialize values list
	cpg_monitor_grow(res);
	
	return res;
}

static void
cpg_monitor_free_data(CpgMonitor *monitor)
{
	if (monitor->values)
		free(monitor->values);
	
	if (monitor->sites)
		free(monitor->sites);
	
	monitor->values = NULL;
	monitor->sites = NULL;
	monitor->size = 0;
	monitor->num_values = 0;
}	
static void
cpg_monitor_free(CpgMonitor *monitor)
{
	cpg_monitor_free_data(monitor);	
	free(monitor);
}

static void
read_headers(CpgNetwork *network, FILE *f)
{
	char *buffer;

	while ((buffer = read_line_real(f, 0)))
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
		
		CpgProperty *property = cpg_object_property(link->to, first);
		
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

static void
set_context(CpgNetwork *network, CpgObject *first, CpgObject *second)
{
	network->context[0].object = first;
	network->context[1].object = second;
}

static int
parse_expressions(CpgNetwork *network, CpgObject *object)
{
	unsigned i;
	
	set_context(network, object, CPG_OBJECT_IS_LINK(object) ? ((CpgLink *)object)->from : NULL);
	
	// Parse all property value expressions
	for (i = 0; i < object->num_properties; ++i)
	{
		CpgProperty *property = object->properties[i];
		char *error;

		if (!cpg_expression_compile(property->initial, network->context, &error))
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
	unsigned size;
	
	CpgLinkAction **actions = cpg_link_actions(link, &size);
	
	for (e = 0; e < size; ++e)
	{
		char *error;
		
		if (!cpg_expression_compile(cpg_link_action_expression(actions[e]), network->context, &error))
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
	array_resize(network->states, CpgState *, ++network->num_states);
	network->states[network->num_states - 1] = state;
}

static void
add_link(CpgNetwork *network, CpgLink *link)
{
	array_resize(network->links, CpgLink *, ++network->num_links);
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
	char *buffer;
	
	// skip empty lines
	while ((buffer = read_line(f)) && !*buffer)
		free(buffer);
	
	CpgObject *object = NULL;
	
	if (!buffer)
		return feof(f); // return 1 if end of file, cause that's ok

	// read in type of object	
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
		if (!parse_expressions(network, (CpgObject *)(network->states[i])))
			return 0;
	}
	
	for (i = 0; i < network->num_links; ++i)
	{
		if (!parse_expressions(network, (CpgObject *)(network->links[i])))
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
 *
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
	
	unsigned i;
	unsigned size = sizeof(network->context) / sizeof(CpgContext);
	for (i = 0; i < size; ++i)
	{
		network->context[i].next = (i == size - 1 ? NULL : &(network->context[i + 1]));
		network->context[i].object = NULL;
	}
	
	network->constants = cpg_object_new();
	network->timeprop = cpg_object_add_property(network->constants, "t", "0", 0);
	network->timestepprop = cpg_object_add_property(network->constants, "dt", "0", 0);
	
	network->context[2].object = network->constants;
	
	return network;
}

/**
 * cpg_network_new_from_file:
 * @filename: the filename of the file containing the network definition
 * 
 * Create a new CPG network by reading the network definition from file
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_file(char const *filename)
{
	FILE *f = fopen(filename, "r");
	
	if (!f)
	{
		fprintf(stderr, "Could not open network file: %s\n", strerror(errno));
		return NULL;
	}

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
	
	cpg_object_free(network->constants);
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

static void
update_monitors(CpgNetwork *network)
{
	CpgMonitor *monitor;
	for (monitor = network->monitors; monitor; monitor = monitor->next)
	{
		if (monitor->size == 0 || monitor->num_values >= monitor->size - 1)
			cpg_monitor_grow(monitor);
		
		monitor->values[monitor->num_values] = cpg_expression_evaluate(monitor->property->value);
		monitor->sites[monitor->num_values++] = network->time;
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
		
	update_monitors(network);

	network->timestep = timestep;
	cpg_property_set_value(network->timestepprop, timestep);
	
	cpg_debug_evaluate("%s", "Simulation step");
	simulation_evaluate(network);
	simulation_update(network);
	
	network->time += timestep;
	cpg_property_set_value(network->timeprop, network->time);
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
	cpg_property_set_value(network->timeprop, network->time);
	
	while (network->time < to - 0.5 * timestep)
		cpg_network_simulation_step(network, timestep);
}

/**
 * cpg_network_simulation_reset:
 * @network: the #CpgNetwork
 *
 * Reset the CPG network to its original values. This will reset the time
 * to 0 and for all objects in the network will reset all properties to the
 * initial value. This also will reset any active monitors.
 *
 **/
void
cpg_network_simulation_reset(CpgNetwork *network)
{
	// set time back to 0
	network->time = 0;
	cpg_property_set_value(network->timeprop, 0);
	
	// reset all objects
	unsigned i;
	for (i = 0; i < network->num_states; ++i)
		cpg_object_reset((CpgObject *)(network->states[i]));

	for (i = 0; i < network->num_links; ++i)
		cpg_object_reset((CpgObject *)(network->links[i]));
	
	// reset monitors
	CpgMonitor *monitor;
	for (monitor = network->monitors; monitor; monitor = monitor->next)
		cpg_monitor_free_data(monitor);
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
		return monitor;

	monitor = cpg_monitor_new(object, property);

	monitor->next = network->monitors;
	network->monitors = monitor;
	
	return monitor;
}

/**
 * cpg_network_set_monitor:
 * @network: the #CpgNetwork
 * @object: the #CpgObject to monitor
 * @propname: the name of the property to monitor on @object
 *
 * Starts a monitor on the specified object and property. The value of that
 * property is collected for every simulation step until 
 * #cpg_network_unset_monitor is called. The collected data can be retrieved
 * at any time with #cpg_network_monitor_data or 
 * #cpg_network_monitor_data_resampled.
 *
 **/
void
cpg_network_set_monitor(CpgNetwork *network, CpgObject *object, char const *propname)
{
	CpgProperty *property = cpg_object_property(object, propname);
	
	if (!property)
		return;
	
	monitor_add(network, object, property);	
}

/**
 * cpg_network_unset_monitor:
 * @network: the #CpgNetwork
 * @object: the monitored #CpgObject
 * @propname: the name of the monitored property
 *
 * Removes the monitor specified by the object and property name. The property
 * value will no longer be collected and previously collected data is
 * freed. Note that data previously requested with 
 * #cpg_network_monitor_data is now no longer valid.
 *
 **/
void
cpg_network_unset_monitor(CpgNetwork *network, CpgObject *object, char const *propname)
{
	CpgProperty *property = cpg_object_property(object, propname);
	
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

/**
 * cpg_network_monitor_data:
 * @network: the #CpgNetwork
 * @object: the monitored #CpgObject
 * @propname: the monitored property name
 * @size: return pointer value for the size of the returned array
 *
 * Returns the data as monitored during the simulation. See also
 * #cpg_network_monitor_data_resampled for retrieving a resampled version
 * of the monitor data
 *
 * Return value: internal array of monitored values. The double pointer should
 * not be freed
 *
 **/
double const *
cpg_network_monitor_data(CpgNetwork *network, CpgObject *object, char const *propname, unsigned *size)
{
	CpgProperty *property = cpg_object_property(object, propname);
	
	if (size)
		*size = 0;

	if (!property)
		return NULL;

	CpgMonitor *monitor = monitor_find(network, object, property);
	
	if (!monitor)
		return NULL;
	
	if (size)	
		*size = monitor->num_values;

	return monitor->values;
}

static int
bsearch_find(double const *list, int size, double value)
{
	int left = 0;
	int right = size;
	
	while (right > left)
	{
		int probe = (left + right) / 2;
		
		if (list[probe] > value)
			right = probe - 1;
		else if (list[probe] < value)
			left = probe + 1;
		else
			return probe;
	}
	
	return right + (right < size && list[right] < value ? 1 : 0);
}

/**
 * cpg_network_monitor_data_resampled:
 * @network: the #CpgNetwork
 * @object: the monitored #CpgObject
 * @propname: the monitored property name
 * @sites: the data sites at which to resample the data
 * @size: the size of the data sites array
 *
 * Returns the data as monitored during the simulation, but resampled at
 * sepcific data sites
 *
 * Return value: newly allocated array of monitored values. The returned pointer
 * should be freed when no longer used
 *
 **/
double *
cpg_network_monitor_data_resampled(CpgNetwork *network, CpgObject *object, char const *propname, double const *sites, unsigned size)
{
	if (!sites || size == 0)
		return NULL;

	CpgProperty *property = cpg_object_property(object, propname);

	if (!property)
		return NULL;

	CpgMonitor *monitor = monitor_find(network, object, property);
	
	if (!monitor)
		return NULL;
	
	double const *data = monitor->values;
	double *ret = cpg_new(double, size);
	unsigned i;
	
	double const *monsites = monitor->sites;
	
	for (i = 0; i < size; ++i)
	{
		unsigned idx = bsearch_find(monsites, (int)monitor->num_values, sites[i]);
		unsigned fidx = idx > 0 ? idx - 1 : 0;
		unsigned sidx = idx < monitor->num_values ? idx : monitor->num_values - 1;
		
		// interpolate between the values
		double factor = monsites[sidx] == monsites[fidx] ? 1 : (monsites[sidx] - sites[i]) / (monsites[sidx] - monsites[fidx]);
		ret[i] = data[fidx] * factor + (data[sidx] * (1 - factor));
	}
	
	return ret;
}

int
cpg_network_set_value(CpgNetwork *network, CpgObject *object, CpgProperty *property, char const *expression)
{
	if (!property->value)
		property->value = cpg_expression_new(expression);
	else
		cpg_expression_set(property->value, expression);
	
	if (CPG_OBJECT_IS_LINK(object))
		set_context(network, object, ((CpgLink *)object)->from);
	else
		set_context(network, object, NULL);

	return cpg_expression_compile(property->value, network->context, NULL);
}

int
cpg_network_set_initial(CpgNetwork *network, CpgObject *object, CpgProperty *property, char const *expression)
{
	if (!property->initial)
		property->initial = cpg_expression_new(expression);
	else
		cpg_expression_set(property->initial, expression);
	
	if (CPG_OBJECT_IS_LINK(object))
		set_context(network, object, ((CpgLink *)object)->from);
	else
		set_context(network, object, NULL);

	return cpg_expression_compile(property->initial, network->context, NULL);
}
