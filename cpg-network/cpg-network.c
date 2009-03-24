#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <sys/sysinfo.h>

#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-relay.h"
#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-debug.h"

#define BUFFER_SIZE 4096

#define CPG_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_NETWORK, CpgNetworkPrivate))

typedef struct
{
	CpgNetwork *network;

	guint from;
	guint to;
	
	gboolean running;
} CpgSimulationWorker;

struct _CpgNetworkPrivate
{
	gchar *filename;
	
	GSList *states;
	GSList *links;
	
	/* simulation */
	gdouble timestep;
	gdouble time;
	gboolean compiled;
	
	/* context */
	GSList *context;
	CpgObject *constants;
	CpgProperty *timeprop;
	CpgProperty *timestepprop;
};

enum
{
	RESET,
	UPDATE,
	NUM_SIGNALS
};

#define NUM_CONTEXT 3

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE(CpgNetwork, cpg_network, G_TYPE_OBJECT)

static void
cpg_network_finalize(GObject *object)
{
	CpgNetwork *network = CPG_NETWORK(object);

	g_free(network->priv->filename);
	
	g_object_unref(network->priv->constants);
	g_slist_free(network->priv->context);

	cpg_network_clear(network);
	
	G_OBJECT_CLASS(cpg_network_parent_class)->finalize(object);
}

static void
cpg_network_class_init(CpgNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);
	
	object_class->finalize = cpg_network_finalize;

	network_signals[RESET] =
   		g_signal_new("reset",
			      G_OBJECT_CLASS_TYPE(object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(CpgNetworkClass, reset),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	network_signals[UPDATE] =
   		g_signal_new("update",
			      G_OBJECT_CLASS_TYPE(object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET(CpgNetworkClass, update),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__DOUBLE,
			      G_TYPE_NONE,
			      1,
				  G_TYPE_DOUBLE);
		
	g_type_class_add_private(object_class, sizeof(CpgNetworkPrivate));
}

static void
cpg_network_init(CpgNetwork *network)
{
	network->priv = CPG_NETWORK_GET_PRIVATE(network);

	guint i;
	
	for (i = 0; i < NUM_CONTEXT; ++i)
		network->priv->context = g_slist_prepend(network->priv->context, NULL);
	
	network->priv->constants = cpg_object_new(NULL);
	network->priv->timeprop = cpg_object_add_property(network->priv->constants, "t", "0", 0);
	network->priv->timestepprop = cpg_object_add_property(network->priv->constants, "dt", "0", 0);
	
	g_slist_nth(network->priv->context, NUM_CONTEXT - 1)->data = network->priv->constants;
}

/**
 * cpg_network_new:
 * 
 * Create a new empty CPG network
 *
 * Return value: the newly created CPG network
 *
 **/
CpgNetwork*
cpg_network_new()
{
	return g_object_new(CPG_TYPE_NETWORK, NULL);
}

static gchar *
read_line_real(FILE *f, 
			   gint  skip_comments)
{
	gchar *buffer = g_new(gchar, BUFFER_SIZE);
	gchar *ret;
	
	while ((ret = fgets(buffer, BUFFER_SIZE, f)) && (skip_comments && *ret == '#'))
		;

	if (!ret)
	{
		g_free(buffer);
	}
	else
	{
		gint len = strlen(buffer);
		
		if (buffer[len - 1] == '\n')
			buffer[len - 1] = '\0';
	}
	
	return ret;
}

static gchar *
read_line(FILE *f)
{
	return read_line_real(f, 1);
}

static void
read_headers(CpgNetwork *network, 
			 FILE       *f)
{
	gchar *buffer;

	while ((buffer = read_line_real(f, 0)))
	{
		if (*buffer != '#')
		{
			fseek(f, -strlen(buffer) - 1, SEEK_CUR);
			g_free(buffer);

			break;
		}
		
		// just ignore headers for now
		g_free(buffer);
	}
}

static gchar *
read_tab_sep(FILE   *f, 
			 gchar **first, 
			 gchar **second)
{
	if (feof(f))
		return NULL;
	
	gchar *buffer = read_line(f);
	
	if (!buffer)
		return NULL;
	
	gchar *pos = strchr(buffer, '\t');
	
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
read_properties(CpgObject *object, 
				FILE      *f)
{
	gchar *buffer;
	gchar *first;
	gchar *second;
	
	while ((buffer = read_tab_sep(f, &first, &second)))
	{
		if (*buffer == '\0')
		{
			g_free(buffer);
			break;
		}
		
		gchar *pos = strchr(second, '\t');
		gboolean integrated = FALSE;
		
		if (pos)
		{
			*pos = '\0';
			integrated = (*(pos + 1) != '0');
		}
		
		cpg_object_add_property(object, first, second, integrated);
		g_free(buffer);
	}
}

static void
read_expressions(CpgLink *link, 
				 FILE    *f)
{
	gchar *buffer;
	gchar *first;
	gchar *second;
	
	while ((buffer = read_tab_sep(f, &first, &second)))
	{
		if (*buffer == '\0')
		{
			g_free(buffer);
			break;
		}
		
		CpgProperty *property = cpg_object_get_property(cpg_link_get_to(link), first);
		
		if (property)
			cpg_link_add_action(link, property, second);
		else
			cpg_debug_error("Could not find property `%s' to act on", first);

		g_free(buffer);
	}
}

static CpgObject *
read_object_type(CpgNetwork *network, 
				 FILE       *f, 
				 GType       gtype)
{
	gchar *buffer;
	
	// read in the state object id
	buffer = read_line(f);
	
	CpgObject *object = g_object_new(gtype, "id", buffer, NULL);
	g_free(buffer);
	
	// read in properties
	read_properties(object, f);
	
	return object;
}

static CpgObject *
read_state(CpgNetwork *network, 
		   FILE       *f)
{
	return read_object_type(network, f, CPG_TYPE_STATE);
}

static CpgObject *
read_relay(CpgNetwork *network, 
		   FILE       *f)
{
	return read_object_type(network, f, CPG_TYPE_RELAY);
}

static CpgObject *
read_link(CpgNetwork *network, 
		  FILE       *f)
{
	gchar *from;
	gchar *to;
	gchar *id;

	// read from
	id = read_line(f);
	from = read_line(f);
	to = read_line(f);
	
	CpgObject *fromobj = cpg_network_get_object(network, from);
	CpgObject *toobj = cpg_network_get_object(network, to);
	
	if (!fromobj || !CPG_IS_STATE(fromobj) || !toobj || !CPG_IS_STATE(toobj))
	{
		cpg_debug_error("Could not find state `%s' for link", !fromobj ? from : to);

		g_free(from);
		g_free(to);
		g_free(id);

		return NULL;
	}
	
	CpgLink *link = cpg_link_new(id, fromobj, toobj);
	
	// read properties
	read_properties((CpgObject *)link, f);

	// read expressions
	read_expressions(link, f);
	
	g_free(from);
	g_free(to);
	g_free(id);
	
	return (CpgObject *)link;
}

static void
skip_until_separator(FILE *f)
{
	gchar *buffer;
	gint nl = 0;
	
	while ((buffer = read_line(f)))
	{
		if (*buffer == '\0')
			++nl;
		else
			nl = 0;
		
		g_free(buffer);
		
		if (nl == 2)
			break;
	}
}

static void
set_context(CpgNetwork *network, 
		    CpgObject  *first, 
			CpgObject  *second)
{
	g_slist_nth(network->priv->context, 0)->data = first;
	g_slist_nth(network->priv->context, 1)->data = second;
}

static gboolean
parse_expressions(CpgNetwork *network, 
				  CpgObject  *object)
{
	set_context(network, object, CPG_IS_LINK(object) ? cpg_link_get_from(CPG_LINK(object)) : NULL);
	
	// Parse all property value expressions
	GSList *properties = cpg_object_get_properties(object);
	
	while (properties)
	{
		CpgProperty *property = (CpgProperty *)properties->data;
		gchar *error;

		if (!cpg_expression_compile(cpg_property_get_value_expression(property), network->priv->context, &error))
		{
			cpg_debug_error("Error while parsing expression: %s for [%s].%s", error, cpg_object_get_id(object), cpg_property_get_name(property));
			g_free(error);
			return FALSE;
		}
		
		properties = g_slist_next(properties);
	}
	
	cpg_object_reset(object);
	
	if (!CPG_IS_LINK(object))
		return TRUE;

	// Parse all link expressions	
	CpgLink *link = CPG_LINK(object);	
	GSList *actions = cpg_link_get_actions(link);
	
	while (actions)
	{
		CpgLinkAction *action = (CpgLinkAction *)actions->data;
		gchar *error;
		
		if (!cpg_expression_compile(cpg_link_action_get_expression(action), network->priv->context, &error))
		{
			cpg_debug_error("Error while parsing expression: %s for [%s]", error, cpg_object_get_id(object));
			g_free(error);
			return FALSE;
		}
		
		actions = g_slist_next(actions);
	}
	
	return TRUE;
}

static void
add_state(CpgNetwork *network, 
		  gpointer    state)
{
	network->priv->states = g_slist_append(network->priv->states, state);
}

static void
add_link(CpgNetwork *network, 
		 gpointer    link)
{
	network->priv->links = g_slist_append(network->priv->links, link);
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
cpg_network_add_object(CpgNetwork *network, 
					   CpgObject  *object)
{
	g_return_if_fail(CPG_IS_NETWORK(network));
	g_return_if_fail(CPG_IS_OBJECT(object));

	// add object to the network
	if (CPG_IS_LINK(object))
		add_link(network, object);
	else if (CPG_IS_STATE(object) || CPG_IS_RELAY(object))
		add_state(network, object);
	else
		return;
	
	network->priv->compiled = FALSE;
}

static gboolean
read_object(CpgNetwork *network, 
			FILE       *f)
{
	gchar *buffer;
	
	// skip empty lines
	while ((buffer = read_line(f)) && !*buffer)
		g_free(buffer);
	
	CpgObject *object = NULL;
	
	if (!buffer)
		return feof(f); // return TRUE if end of file, cause that's ok

	// read in type of object	
	if (strcmp(buffer, "state") == 0)
		object = read_state(network, f);
	else if (strcmp(buffer, "relay") == 0)
		object = read_relay(network, f);
	else if (strcmp(buffer, "link") == 0)
		object = read_link(network, f);
	else
		skip_until_separator(f);
	
	g_free(buffer);

	if (object)
		cpg_network_add_object(network, object);
	
	return TRUE;
}

static CpgObject *
get_object_from_list(GSList      *objects,
					 gchar const *id)
{
	while (objects)
	{
		if (strcmp(cpg_object_get_id(CPG_OBJECT(objects->data)), id) == 0)
			return CPG_OBJECT(objects->data);

		objects = g_slist_next(objects);
	}
	
	return NULL;
}

CpgObject *
cpg_network_get_object(CpgNetwork  *network, 
					   gchar const *id)
{
	g_return_val_if_fail(CPG_IS_NETWORK(network), NULL);
	g_return_val_if_fail(id != NULL, NULL);
	
	CpgObject *ret;
	
	if ((ret = get_object_from_list(network->priv->states, id)))
		return ret;

	if ((ret = get_object_from_list(network->priv->links, id)))
		return ret;

	return NULL;
}

/**
 * cpg_network_get_states:
 * @network: the #CpgNetwork
 * @size: return value for the size of the list of states
 *
 * Retrieves the list of states. This list is managed internally by the network
 * and should therefore not be changed or freed
 *
 * Return value: a list of #CpgState
 *
 **/
GSList *
cpg_network_get_states(CpgNetwork *network)
{
	g_return_val_if_fail(CPG_IS_NETWORK(network), NULL);
	
	return network->priv->states;
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
GSList *
cpg_network_get_links(CpgNetwork *network)
{
	g_return_val_if_fail(CPG_IS_NETWORK(network), NULL);

	return network->priv->links;
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
	g_return_if_fail(CPG_IS_NETWORK(network));

	network->priv->compiled = FALSE;
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
gboolean
cpg_network_compile(CpgNetwork *network)
{
	g_return_val_if_fail(CPG_IS_NETWORK(network), FALSE);
	
	network->priv->compiled = FALSE;
	
	GSList *item;
	
	for (item = network->priv->states; item; item = g_slist_next(item))
	{
		if (!parse_expressions(network, CPG_OBJECT(item->data)))
			return FALSE;
	}
	
	for (item = network->priv->links; item; item = g_slist_next(item))
	{
		if (!parse_expressions(network, CPG_OBJECT(item->data)))
			return FALSE;
	}
	
	network->priv->compiled = TRUE;
	cpg_network_reset(network);

	return TRUE;
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
cpg_network_new_from_file(gchar const *filename)
{
	g_return_val_if_fail(filename != NULL, NULL);

	FILE *f = fopen(filename, "r");
	
	if (!f)
	{
		cpg_debug_error("Could not open network file: %s", strerror(errno));
		return NULL;
	}

	CpgNetwork *network = cpg_network_new();

	network->priv->filename = strdup(filename);
	
	// read in headers
	read_headers(network, f);
	
	// rest of the file consists of objects
	while (!feof(f))
	{
		// read object
		if (!read_object(network, f))
		{
			g_object_unref(network);
			fclose(f);
			return NULL;
		}
	}
	
	fclose(f);
	
	if (!cpg_network_compile(network))
	{
		g_object_unref(network);
		return NULL;
	}
	
	return network;
}

/**
 * cpg_network_clear:
 * @network: the #CpgNetwork
 *
 * Clears the network (removes all objects).
 *
 **/
void
cpg_network_clear(CpgNetwork *network)
{
	g_return_if_fail(CPG_IS_NETWORK(network));

	// remove all states
	g_slist_foreach(network->priv->states, (GFunc)g_object_unref, NULL);
	g_slist_foreach(network->priv->links, (GFunc)g_object_unref, NULL);
	
	g_slist_free(network->priv->states);
	g_slist_free(network->priv->links);
	
	network->priv->states = NULL;
	network->priv->links = NULL;
}

/* simulation functions */
static void
evaluate_objects(CpgNetwork *network,
				 GType       type)
{
	GSList *item;
	
	for (item = network->priv->states; item; item = g_slist_next(item))
	{
		CpgObject *obj = CPG_OBJECT(item->data);
		
		if (g_type_is_a(G_TYPE_FROM_INSTANCE(obj), type))
			cpg_object_evaluate(obj, network->priv->timestep);
	}
}

static void
simulation_evaluate_relays(CpgNetwork *network)
{
	evaluate_objects(network, CPG_TYPE_RELAY);
}

static void
simulation_evaluate_states(CpgNetwork *network)
{
	evaluate_objects(network, CPG_TYPE_STATE);
}

static void
simulation_update(CpgNetwork *network)
{
	GSList *item;
	
	/* update all objects */
	for (item = network->priv->states; item; item = g_slist_next(item))
		cpg_object_update(CPG_OBJECT(item->data), network->priv->timestep);
	
	for (item = network->priv->links; item; item = g_slist_next(item))
		cpg_object_update(CPG_OBJECT(item->data), network->priv->timestep);
}

/**
 * cpg_network_step:
 * @network: the #CpgNetwork
 * @timestep: the integration timestep
 * 
 * Perform one step of simulation given the specified @timestep.
 *
 **/
void
cpg_network_step(CpgNetwork *network, 
				 gdouble     timestep)
{
	g_return_if_fail(CPG_IS_NETWORK(network));
	g_return_if_fail(timestep > 0);

	if (!network->priv->compiled)
		cpg_network_compile(network);

	g_signal_emit(network, network_signals[UPDATE], 0, timestep);

	network->priv->timestep = timestep;
	cpg_property_set_value(network->priv->timestepprop, timestep);
	
	cpg_debug_evaluate("Simulation step");
	
	// first evaluate all the relays
	simulation_evaluate_relays(network);
	
	// then evaluate the network
	simulation_evaluate_states(network);
	simulation_update(network);
	
	network->priv->time += timestep;
	cpg_property_set_value(network->priv->timeprop, network->priv->time);
}

/**
 * cpg_network_run:
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
cpg_network_run(CpgNetwork *network, 
				gdouble     from, 
				gdouble     timestep, 
				gdouble     to)
{
	g_return_if_fail(CPG_IS_NETWORK(network));
	g_return_if_fail(from < to);
	g_return_if_fail(timestep > 0);
	g_return_if_fail(to - (from + timestep) < to - from);

	if (!network->priv->compiled && !cpg_network_compile(network))
		return;

	network->priv->time = from;
	cpg_property_set_value(network->priv->timeprop, network->priv->time);
		
	while (network->priv->time < to - 0.5 * timestep)
		cpg_network_step(network, timestep);
}

/**
 * cpg_network_reset:
 * @network: the #CpgNetwork
 *
 * Reset the CPG network to its original values. This will reset the time
 * to 0 and for all objects in the network will reset all properties to the
 * initial value.
 *
 **/
void
cpg_network_reset(CpgNetwork *network)
{
	g_return_if_fail(CPG_IS_NETWORK(network));

	// set time back to 0
	network->priv->time = 0;
	cpg_property_set_value(network->priv->timeprop, 0);
	
	// reset all objects
	g_slist_foreach(network->priv->states, (GFunc)cpg_object_reset, NULL);
	g_slist_foreach(network->priv->links, (GFunc)cpg_object_reset, NULL);

	g_signal_emit(network, network_signals[RESET], 0);
}

gboolean
cpg_network_set_expression(CpgNetwork  *network, 
					       CpgProperty *property, 
					       gchar const *expression)
{
	g_return_val_if_fail(CPG_IS_NETWORK(network), FALSE);
	g_return_val_if_fail(property != NULL, FALSE);
	g_return_val_if_fail(expression != NULL, FALSE);
	
	CpgObject *object = cpg_property_get_object(property);
	
	if (CPG_IS_LINK(object))
		set_context(network, object, cpg_link_get_from(CPG_LINK(object)));
	else
		set_context(network, object, NULL);

	CpgExpression *expr = cpg_property_get_value_expression(property);
	cpg_expression_set_from_string(expr, expression);
	
	return cpg_expression_compile(expr, network->priv->context, NULL);
}
