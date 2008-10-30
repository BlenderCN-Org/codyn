#include <stdio.h>
#include <string.h>

#include "cpg-network.h"
#include "cpg-utils.h"
#include "cpg-state.h"
#include "cpg-link.h"

#define BUFFER_SIZE 4096

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

/*static int
read_skip(FILE *f, int c)
{
	int ret = 0;

	// read as long as current character is 'c'
	while (fgetc(f) == c)
		++ret;
	
	// go one back
	fseek(f, -1, SEEK_CUR);
	return ret;
}*/

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
			cpg_link_add_expression(link, property, second);
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
	{
		// Make sure to parse all expressions in the object after it has been
		// fully constructed so that references are all present
		if (!parse_expressions(object))
		{
			if (CPG_OBJECT_IS_STATE(object))
				cpg_state_free((CpgState *)object);
			else
				cpg_link_free((CpgLink *)object);
			
			return 0;
		}

		cpg_object_reset(object);
		
		network->objects = (CpgObject **)realloc(network->objects, sizeof(CpgObject *) * (network->num_states + network->num_links + 1));
		network->objects[network->num_states + network->num_links] = object;
		
		if (CPG_OBJECT_IS_STATE(object))
			++network->num_states;
		else
			++network->num_links;
	}
	
	return 1;
}

CpgState *
cpg_network_get_state_by_name(CpgNetwork *network, char const *name)
{
	int i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgState *state = (CpgState *)(network->objects[i]);
		
		if (strcmp(state->name, name) == 0)
			return state;
	}
	
	return NULL;
}

CpgNetwork *
cpg_network_new_from_file(char const *filename)
{
	FILE *f = fopen(filename, "r");
	
	if (!f)
		return NULL;

	CpgNetwork *network = cpg_new1(CpgNetwork);

	network->filename = strdup(filename);
	network->objects = NULL;

	network->num_states = 0;
	network->num_links = 0;
	
	read_headers(network, f);
	
	while (!feof(f))
	{
		if (!read_object(network, f))
		{
			cpg_network_free(network);
			fclose(f);
			return NULL;
		}
	}
	
	fclose(f);
	
	return network;
}

void
cpg_network_free(CpgNetwork *network)
{
	free(network->filename);
	
	// free all objects
	unsigned i;
	for (i = 0; i < network->num_states; ++i)
		cpg_state_free((CpgState *)network->objects[i]);
	
	for (i = network->num_states; i < network->num_states + network->num_links; ++i)
		cpg_link_free((CpgLink *)network->objects[i]);
	
	free(network->objects);
	free(network);
}

/* simulation functions */
static void
simulation_evaluate(CpgNetwork *network)
{
	unsigned i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgObject *object = (CpgObject *)(network->objects[i]);
		cpg_object_evaluate(object, network->timestep);
	}
}

static void
simulation_update(CpgNetwork *network)
{
	unsigned i;
	
	for (i = 0; i < network->num_states; ++i)
	{
		CpgObject *object = (CpgObject *)(network->objects[i]);
		cpg_object_update(object, network->timestep);
	}
}

void
cpg_network_simulation_step(CpgNetwork *network, float step)
{
	network->timestep = step;
	
	simulation_evaluate(network);
	simulation_update(network);
}
