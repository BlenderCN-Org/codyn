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
#include "cpg-network-reader.h"

#define CPG_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_NETWORK, CpgNetworkPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_TIME,
	PROP_TIMESTEP,
	PROP_COMPILED
};

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
	
	GHashTable *object_map;
	
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
	COMPILE_ERROR,
	NUM_SIGNALS
};

#define NUM_CONTEXT 3

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgNetwork, cpg_network, G_TYPE_OBJECT)

static void
cpg_network_finalize (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	g_free (network->priv->filename);
	
	g_object_unref (network->priv->constants);
	g_slist_free (network->priv->context);

	cpg_network_clear (network);
	
	g_hash_table_destroy (network->priv->object_map);
	
	G_OBJECT_CLASS (cpg_network_parent_class)->finalize (object);
}

static void
cpg_network_get_property (GObject     *object,
                          guint        prop_id,
                          GValue      *value,
                          GParamSpec  *pspec)
{
	CpgNetwork *self = CPG_NETWORK (object);
	
	switch (prop_id)
	{
		case PROP_TIME:
			g_value_set_double (value, self->priv->time);
		break;
		case PROP_TIMESTEP:
			g_value_set_double (value, self->priv->timestep);
		break;
		case PROP_COMPILED:
			g_value_set_boolean (value, self->priv->compiled);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_set_property (GObject       *object,
                          guint          prop_id,
                          const GValue  *value,
                          GParamSpec    *pspec)
{
	CpgNetwork *self = CPG_NETWORK (object);
	
	switch (prop_id)
	{
		case PROP_TIME:
			self->priv->time = g_value_get_double (value);
		break;
		case PROP_TIMESTEP:
			self->priv->timestep = g_value_get_double (value);
		break;
		case PROP_COMPILED:
			self->priv->compiled = g_value_get_boolean (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_class_init (CpgNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_network_finalize;
	object_class->get_property = cpg_network_get_property;
	object_class->set_property = cpg_network_set_property;

	g_object_class_install_property (object_class, PROP_TIME,
				 g_param_spec_double ("time",
							  "TIME",
							  "Current simulatio time",
							  0,
							  G_MAXDOUBLE,
							  0,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class, PROP_TIMESTEP,
				 g_param_spec_double ("timestep",
							  "TIMESTEP",
							  "Current simulation timestep",
							  0,
							  G_MAXDOUBLE,
							  0,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class, PROP_COMPILED,
				 g_param_spec_boolean ("compiled",
							  "COMPILED",
							  "Whether the network is currently compiled",
							  FALSE,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	network_signals[RESET] =
   		g_signal_new ("reset",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (CpgNetworkClass, reset),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	network_signals[UPDATE] =
   		g_signal_new ("update",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (CpgNetworkClass, update),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__DOUBLE,
			      G_TYPE_NONE,
			      1,
				  G_TYPE_DOUBLE);

	network_signals[COMPILE_ERROR] =
   		g_signal_new ("compile-error",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (CpgNetworkClass, compile_error),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__BOXED,
			      G_TYPE_NONE,
			      1,
				  CPG_TYPE_COMPILE_ERROR);
		
	g_type_class_add_private (object_class, sizeof (CpgNetworkPrivate));
}

static void
cpg_network_init (CpgNetwork *network)
{
	network->priv = CPG_NETWORK_GET_PRIVATE (network);

	network->priv->object_map = g_hash_table_new_full (g_str_hash,
	                                                   g_str_equal,
	                                                   (GDestroyNotify)g_free,
	                                                   NULL);
	guint i;
	
	for (i = 0; i < NUM_CONTEXT; ++i)
	{
		network->priv->context = g_slist_prepend (network->priv->context, NULL);
	}
	
	network->priv->constants = cpg_object_new (NULL);
	network->priv->timeprop = cpg_object_add_property (network->priv->constants, "t", "0", 0);
	network->priv->timestepprop = cpg_object_add_property (network->priv->constants, "dt", "0", 0);
	
	g_slist_nth (network->priv->context, NUM_CONTEXT - 1)->data = network->priv->constants;
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
cpg_network_new ()
{
	return g_object_new (CPG_TYPE_NETWORK, NULL);
}

static void
set_context (CpgNetwork  *network,
             CpgObject   *first,
             CpgObject   *second)
{
	g_slist_nth (network->priv->context, 0)->data = first;
	g_slist_nth (network->priv->context, 1)->data = second;
}

static void
compile_error_set (CpgCompileError *error,
                   CpgObject       *object,
                   CpgProperty     *property,
                   CpgLinkAction   *action)
{
	if (error == NULL)
	{
		return;
	}

	_cpg_compile_error_set (error, object, property, action);
}

static gboolean
parse_expressions (CpgNetwork      *network,
                   CpgObject       *object,
                   CpgCompileError *error)
{
	set_context (network, object, CPG_IS_LINK (object) ? cpg_link_get_from (CPG_LINK (object)) : NULL);
	
	// Parse all property value expressions
	GSList *properties = cpg_object_get_properties (object);
	
	while (properties)
	{
		CpgProperty *property = (CpgProperty *)properties->data;
		CpgExpression *expr = cpg_property_get_value_expression (property);
		GError **gerror = cpg_compile_error_get_error (error);
		
		if (!cpg_expression_compile (expr, 
		                             network->priv->context, 
		                             gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s].%s<%s>: %s",
			                 cpg_object_get_id (object), 
			                 cpg_property_get_name (property), 
			                 cpg_expression_get_as_string (expr),
			                 (*gerror)->message);
			
			compile_error_set (error, object, property, NULL);
			return FALSE;
		}
		
		properties = g_slist_next (properties);
	}
	
	cpg_object_reset (object);
	
	if (!CPG_IS_LINK (object))
		return TRUE;

	// Parse all link expressions	
	CpgLink *link = CPG_LINK (object);	
	GSList *actions = cpg_link_get_actions (link);
	
	while (actions)
	{
		CpgLinkAction *action = (CpgLinkAction *)actions->data;
		CpgExpression *expr = cpg_link_action_get_expression (action);
		GError **gerror = cpg_compile_error_get_error (error);
		
		if (!cpg_expression_compile (expr, network->priv->context, gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s]<%s>: %s", 
			                 cpg_object_get_id (object), 
			                 cpg_expression_get_as_string (expr),
			                 (*gerror)->message);
			
			compile_error_set (error, object, NULL, action);
			return FALSE;
		}
		
		actions = g_slist_next (actions);
	}
	
	return TRUE;
}

static void
add_state (CpgNetwork *network, 
		  gpointer    state)
{
	network->priv->states = g_slist_append (network->priv->states, 
	                                        g_object_ref (state));
}

static void
add_link (CpgNetwork  *network,
          gpointer     link)
{
	network->priv->links = g_slist_append (network->priv->links, 
	                                       g_object_ref (link));
}

static gchar *
unique_id (CpgNetwork *network,
           CpgObject  *object)
{
	gchar const *id = cpg_object_get_id (object);
	gchar *newid = g_strdup (id);
	gint cnt = 0;
	
	while (TRUE)
	{
		CpgObject *orig = g_hash_table_lookup (network->priv->object_map,
	    	                                   newid);

		if (orig == NULL || orig == object)
		{
			if (cnt == 0)
			{
				g_free (newid);
				newid = NULL;
			}
			
			break;
		}
		
		g_free (newid);
		newid = g_strdup_printf ("%s (%d)", id, ++cnt);
	}
	
	return newid;
}

static void
register_id (CpgNetwork *network,
             CpgObject  *object)
{
	gchar *newid = unique_id (network, object);
	
	if (newid == NULL)
	{
		g_hash_table_insert (network->priv->object_map,
		                     g_strdup (cpg_object_get_id (object)),
		                     object);
	}
	else
	{
		cpg_object_set_id (object, newid);
		g_free (newid);
	}
}

typedef struct
{
	CpgObject *find;
	const gchar *id;
} FindInfo;

static gboolean
find_object (const gchar  *id,
             CpgObject    *object,
             FindInfo     *info)
{
	if (object == info->find)
	{
		info->id = id;
		return TRUE;
	}
	
	return FALSE;
}

static void
update_object_id (CpgObject   *object,
                  GParamSpec  *spec,
                  CpgNetwork  *network)
{
	FindInfo info = {object, NULL};
	
	g_hash_table_find (network->priv->object_map,
	                   (GHRFunc)find_object,
	                   &info);
	
	/* Remove old id */
	if (info.id != NULL)
	{
		g_hash_table_remove (network->priv->object_map,
		                     info.id);
	}

	register_id (network, object);
}

static void
unregister_object (CpgNetwork *network,
                   CpgObject  *object)
{
	g_signal_handlers_disconnect_by_func (object, 
	                                      G_CALLBACK (cpg_network_taint), 
	                                      network);
	g_signal_handlers_disconnect_by_func (object, 
	                                      G_CALLBACK (update_object_id), 
	                                      network);

	g_hash_table_remove (network->priv->object_map, cpg_object_get_id (object));
}

static void
register_object (CpgNetwork *network,
                 CpgObject  *object)
{
	g_signal_connect_swapped (object, 
	                          "tainted", 
	                          G_CALLBACK (cpg_network_taint), 
	                          network);

	g_signal_connect (object, 
	                  "notify::id", 
	                  G_CALLBACK (update_object_id), 
	                  network);

	register_id (network, object);
}

static void
set_compiled (CpgNetwork *network,
              gboolean    compiled)
{
	if (network->priv->compiled != compiled)
	{
		network->priv->compiled = compiled;
		g_object_notify (G_OBJECT (network), "compiled");
	}
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
cpg_network_add_object (CpgNetwork  *network,
                        CpgObject   *object)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_LINK (object) || CPG_IS_STATE (object) || CPG_IS_RELAY (object));
	
	CpgObject *other = g_hash_table_lookup (network->priv->object_map, 
	                                        cpg_object_get_id (object));

	if (other == object)
	{
		return;
	}
	
	register_object (network, object);

	if (CPG_IS_LINK (object))
		add_link (network, object);
	else if (CPG_IS_STATE (object) || CPG_IS_RELAY (object))
		add_state (network, object);
	
	set_compiled (network, FALSE);
}

/**
 * cpg_network_get_object:
 * @network: the #CpgNetwork
 * @id: the object id
 *
 * Find object @id in @network
 *
 * Return value: a #CpgObject or %NULL if there is no object with id @id
 *
 **/
CpgObject *
cpg_network_get_object (CpgNetwork   *network,
                        gchar const  *id)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (id != NULL, NULL);
	
	return g_hash_table_lookup (network->priv->object_map, id);
}

/**
 * cpg_network_get_states:
 * @network: the #CpgNetwork
 *
 * Retrieves the list of states. This list is managed internally by the network
 * and should therefore not be changed or freed
 *
 * Return value: a list of #CpgState
 *
 **/
GSList *
cpg_network_get_states (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	
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
cpg_network_get_links (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

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
cpg_network_taint (CpgNetwork *network)
{
	g_return_if_fail (CPG_IS_NETWORK (network));

	set_compiled (network, FALSE);
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
cpg_network_compile (CpgNetwork      *network,
                     CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	
	set_compiled (network, FALSE);
	
	GSList *item;
	
	for (item = network->priv->states; item; item = g_slist_next (item))
	{
		if (!parse_expressions (network, CPG_OBJECT (item->data), error))
			return FALSE;
	}
	
	for (item = network->priv->links; item; item = g_slist_next (item))
	{
		if (!parse_expressions (network, CPG_OBJECT (item->data), error))
			return FALSE;
	}
	
	set_compiled (network, TRUE);
	cpg_network_reset (network);

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
cpg_network_new_from_file (gchar const *filename)
{
	g_return_val_if_fail (filename != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();
	network->priv->filename = strdup (filename);
	
	if (!cpg_network_reader_xml (network, filename))
	{
		cpg_debug_error ("Could not read network from xml file: %s", filename);
		
		g_object_unref (network);
		network = NULL;
	}

	set_compiled (network, FALSE);
	return network;
}

/**
 * cpg_network_new_from_xml:
 * @xml: xml definition of the network
 * 
 * Create a new CPG network from the network xml definition
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_xml (gchar const *xml)
{
	g_return_val_if_fail (xml != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();
	
	if (!cpg_network_reader_xml_string (network, xml))
	{
		cpg_debug_error ("Could not read network from xml");
		
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

static void
remove_object (CpgObject   *object,
               CpgNetwork  *network)
{
	unregister_object (network, object);
	g_object_unref (object);	
}

/**
 * cpg_network_remove_object:
 * @network: the #CpgNetwork
 * @object: the #CpgObject to remove
 *
 * Removes @object from the network
 *
 **/
void
cpg_network_remove_object (CpgNetwork  *network,
                           CpgObject   *object)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_OBJECT (object));

	remove_object (object, network);

	if (CPG_IS_LINK (object))
		network->priv->links = g_slist_remove (network->priv->links, object);
	else
		network->priv->states = g_slist_remove (network->priv->states, object);
}

/**
 * cpg_network_clear:
 * @network: the #CpgNetwork
 *
 * Clears the network (removes all objects).
 *
 **/
void
cpg_network_clear (CpgNetwork *network)
{
	g_return_if_fail (CPG_IS_NETWORK (network));

	// remove all states
	g_slist_foreach (network->priv->states, (GFunc)remove_object, network);
	g_slist_foreach (network->priv->links, (GFunc)remove_object, network);
	
	g_slist_free (network->priv->states);
	g_slist_free (network->priv->links);
	
	network->priv->states = NULL;
	network->priv->links = NULL;
}

/* simulation functions */
static void
evaluate_objects (CpgNetwork  *network,
                  GType        type)
{
	GSList *item;
	
	for (item = network->priv->states; item; item = g_slist_next (item))
	{
		CpgObject *obj = CPG_OBJECT (item->data);
		
		if (g_type_is_a (G_TYPE_FROM_INSTANCE (obj), type))
			cpg_object_evaluate (obj, network->priv->timestep);
	}
}

static void
simulation_evaluate_relays (CpgNetwork *network)
{
	cpg_debug_evaluate ("Evaluate relays");
	evaluate_objects (network, CPG_TYPE_RELAY);
}

static void
simulation_evaluate_states (CpgNetwork *network)
{
	cpg_debug_evaluate ("Evaluate states");
	evaluate_objects (network, CPG_TYPE_STATE);
}

static void
simulation_update (CpgNetwork *network)
{
	GSList *item;
	
	cpg_debug_evaluate ("Simulation update");
	
	/* update all objects */
	for (item = network->priv->states; item; item = g_slist_next (item))
		cpg_object_update (CPG_OBJECT (item->data), network->priv->timestep);
	
	for (item = network->priv->links; item; item = g_slist_next (item))
		cpg_object_update (CPG_OBJECT (item->data), network->priv->timestep);
}

static void
reset_cache (CpgNetwork *network)
{
	g_slist_foreach (network->priv->states, (GFunc)cpg_object_reset_cache, NULL);
	g_slist_foreach (network->priv->links, (GFunc)cpg_object_reset_cache, NULL);
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
cpg_network_step (CpgNetwork  *network,
                  gdouble      timestep)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (timestep > 0);

	if (!network->priv->compiled)
	{
		CpgCompileError *error = cpg_compile_error_new ();
		
		if (!cpg_network_compile (network, error))
		{
			g_signal_emit (network, network_signals[COMPILE_ERROR], 0, error);
			cpg_ref_counted_unref (error);
			
			return;
		}
		
		cpg_ref_counted_unref (error);
	}

	g_signal_emit (network, network_signals[UPDATE], 0, timestep);
	reset_cache (network);	

	network->priv->timestep = timestep;
	g_object_notify (G_OBJECT (network), "timestep");

	cpg_property_set_value (network->priv->timestepprop, timestep);
	
	cpg_debug_evaluate ("Simulation step");
	
	// first evaluate all the relays
	simulation_evaluate_relays (network);
	
	// then evaluate the network
	simulation_evaluate_states (network);
	simulation_update (network);
	
	network->priv->time += timestep;
	cpg_property_set_value (network->priv->timeprop, network->priv->time);
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
cpg_network_run (CpgNetwork  *network,
                 gdouble      from,
                 gdouble      timestep,
                 gdouble      to)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (from < to);
	g_return_if_fail (timestep > 0);
	g_return_if_fail (to - (from + timestep) < to - from);

	if (!network->priv->compiled)
	{
		CpgCompileError *error = cpg_compile_error_new ();
		
		if (!cpg_network_compile (network, error))
		{
			g_signal_emit (network, network_signals[COMPILE_ERROR], 0, error);
			cpg_ref_counted_unref (error);
			
			return;
		}
		
		cpg_ref_counted_unref (error);
	}

	network->priv->time = from;
	g_object_notify (G_OBJECT (network), "time");
	
	cpg_property_set_value (network->priv->timeprop, network->priv->time);
	
	if (network->priv->time >= to - 0.5 * timestep)
		return;
	
	while (network->priv->time < to - 0.5 * timestep)
		cpg_network_step (network, timestep);
	
	g_signal_emit (network, network_signals[UPDATE], 0, timestep);
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
cpg_network_reset (CpgNetwork *network)
{
	g_return_if_fail (CPG_IS_NETWORK (network));

	// set time back to 0
	network->priv->time = 0;
	g_object_notify (G_OBJECT (network), "time");
	
	cpg_property_set_value (network->priv->timeprop, 0);
	
	// reset all objects
	g_slist_foreach (network->priv->states, (GFunc)cpg_object_reset, NULL);
	g_slist_foreach (network->priv->links, (GFunc)cpg_object_reset, NULL);

	g_signal_emit (network, network_signals[RESET], 0);
}

gboolean
cpg_network_set_expression (CpgNetwork   *network,
                            CpgProperty  *property,
                            gchar const  *expression)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (property != NULL, FALSE);
	g_return_val_if_fail (expression != NULL, FALSE);
	
	CpgObject *object = cpg_property_get_object (property);
	
	if (CPG_IS_LINK (object))
		set_context (network, object, cpg_link_get_from (CPG_LINK (object)));
	else
		set_context (network, object, NULL);

	CpgExpression *expr = cpg_property_get_value_expression (property);
	cpg_expression_set_from_string (expr, expression);
	
	return cpg_expression_compile (expr, network->priv->context, NULL);
}

void             
cpg_network_merge (CpgNetwork  *network,
                   CpgNetwork  *other)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_NETWORK (other));
	
	GSList *item;
	
	for (item = other->priv->states; item; item = g_slist_next (item))
		cpg_network_add_object (network, CPG_OBJECT (item->data));

	for (item = other->priv->links; item; item = g_slist_next (item))
		cpg_network_add_object (network, CPG_OBJECT (item->data));
}
