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
#include "cpg-network-writer.h"

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
	
	GHashTable *templates;
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
	g_hash_table_destroy (network->priv->templates);
	
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

	/**
	 * CpgNetwork:time:
	 *
	 * Current simulation time
	 *
	 **/
	g_object_class_install_property (object_class, PROP_TIME,
				 g_param_spec_double ("time",
							  "TIME",
							  "Current simulatio time",
							  0,
							  G_MAXDOUBLE,
							  0,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgNetwork:timestep:
	 *
	 * Current simulation timestep
	 *
	 **/
	g_object_class_install_property (object_class, PROP_TIMESTEP,
				 g_param_spec_double ("timestep",
							  "TIMESTEP",
							  "Current simulation timestep",
							  0,
							  G_MAXDOUBLE,
							  0,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgNetwork:compiled:
	 *
	 * Whether the network is currently compiled
	 *
	 **/
	g_object_class_install_property (object_class, PROP_COMPILED,
				 g_param_spec_boolean ("compiled",
							  "COMPILED",
							  "Whether the network is currently compiled",
							  FALSE,
							  G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgNetwork::reset:
	 * @network: a #CpgNetwork
	 *
	 * Emitted when the network is reset
	 *
	 **/
	network_signals[RESET] =
   		g_signal_new ("reset",
			      G_OBJECT_CLASS_TYPE (object_class),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (CpgNetworkClass, reset),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE,
			      0);

	/**
	 * CpgNetwork::update:
	 * @network: a #CpgNetwork
	 * @timestep: the timestep
	 *
	 * Emitted when the network is updated (one step)
	 *
	 **/
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

	/**
	 * CpgNetwork::compile-error:
	 * @network: a #CpgNetwork
	 * @error: a #CpgCompileError
	 *
	 * Emitted when there is a compile error
	 *
	 **/
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
	
	network->priv->constants = cpg_object_new ("Globals");
	network->priv->timeprop = cpg_object_add_property (network->priv->constants, "t", "0", 0);
	network->priv->timestepprop = cpg_object_add_property (network->priv->constants, "dt", "0", 0);
	
	g_signal_connect_swapped (network->priv->constants, 
	                          "tainted", 
	                          G_CALLBACK (cpg_network_taint), 
	                          network);
	
	g_slist_nth (network->priv->context, NUM_CONTEXT - 1)->data = network->priv->constants;
	
	network->priv->templates = g_hash_table_new_full (g_str_hash, 
	                                                  g_str_equal,
	                                                  (GDestroyNotify)g_free,
	                                                  (GDestroyNotify)g_object_unref);
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
		GError *gerror = NULL;
		
		if (!cpg_expression_compile (expr, 
		                             network->priv->context, 
		                             &gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s].%s<%s>: %s",
			                 cpg_object_get_id (object), 
			                 cpg_property_get_name (property), 
			                 cpg_expression_get_as_string (expr),
			                 gerror->message);
			
			if (error)
			{
				g_propagate_error (cpg_compile_error_get_error (error), gerror);
			}
			else
			{
				g_error_free (gerror);
			}

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
		GError *gerror = NULL;
		
		if (!cpg_expression_compile (expr, network->priv->context, &gerror))
		{
			cpg_debug_error ("Error while parsing expression [%s]<%s>: %s", 
			                 cpg_object_get_id (object), 
			                 cpg_expression_get_as_string (expr),
			                 gerror->message);
			
			if (error)
			{
				g_propagate_error (cpg_compile_error_get_error (error), gerror);
			}
			else
			{
				g_error_free (gerror);
			}
			
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
	_cpg_link_resolve_actions (CPG_LINK (link));

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
 * @network: a #CpgNetwork
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
	
	// Check if object has a template, and the network owns it
	CpgObject *template = NULL;
	g_object_get (G_OBJECT (object), "template", &template, NULL);
	
	if (template)
	{
		CpgObject *other = g_hash_table_lookup (network->priv->templates,
		                                        cpg_object_get_id (template));

		gboolean eq = other == template;
		g_object_unref (template);
		
		if (!eq)
		{
			return;
		}
	}
	
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
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
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
 * cpg_network_get_links:
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
 * @error: return location for compiler error
 *
 * Recompile all expressions for all states and links. You should do this
 * after you've added new objects to the network. If a simulation is ran while
 * the network is in an uncompiled state, it will be compiled first.
 *
 * If an error occurs while compiling, this function returns %FALSE and sets
 * @error accordingly.
 *
 * Return value: %TRUE if compilation was successful, %FALSE otherwise
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
	
	if (!parse_expressions (network, network->priv->constants, error))
		return FALSE;
	
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

	if (network)
	{
		set_compiled (network, FALSE);
	}

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

	if (network)
	{
		set_compiled (network, FALSE);
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
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
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
	
	cpg_object_reset (network->priv->constants);
}

/**
 * cpg_network_step:
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
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
 * @network: a #CpgNetwork
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
	
	cpg_object_reset (network->priv->constants);

	g_signal_emit (network, network_signals[RESET], 0);
}

static void
merge_templates (gchar const *key,
                 CpgObject   *template,
                 CpgNetwork  *network)
{
	CpgObject *orig = g_hash_table_lookup (network->priv->templates,
	                                       key);

	if (!orig)
	{
		cpg_network_add_template (network, key, template);
	}
}

/**
 * cpg_network_merge:
 * @network: a #CpgNetwork
 * @other: a #CpgNetwork to merge
 *
 * Merges all the globals, templates and objects from @other into @network.
 *
 **/
void             
cpg_network_merge (CpgNetwork  *network,
                   CpgNetwork  *other)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_NETWORK (other));
	
	// Copy over globals
	GSList *item;
	
	for (item = cpg_object_get_properties (other->priv->constants); item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		
		if (!cpg_object_get_property (network->priv->constants,
		                              cpg_property_get_name (property)))
		{
			cpg_object_add_property (network->priv->constants,
			                         cpg_property_get_name (property),
			                         cpg_expression_get_as_string (cpg_property_get_value_expression (property)),
			                         FALSE);
		}
	}
		
	// Copy over templates
	g_hash_table_foreach (other->priv->templates,
	                      (GHFunc)merge_templates,
	                      network);
	
	// Copy over states/relays
	for (item = other->priv->states; item; item = g_slist_next (item))
	{
		cpg_network_add_object (network, CPG_OBJECT (item->data));
	}

	// Copy over links
	for (item = other->priv->links; item; item = g_slist_next (item))
	{
		cpg_network_add_object (network, CPG_OBJECT (item->data));
	}
}

/**
 * cpg_network_merge_from_file:
 * @network: a #CpgNetwork
 * @filename: network filename
 *
 * Merges the network defined in the file @filename into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
void
cpg_network_merge_from_file (CpgNetwork  *network,
                             gchar const *filename)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (filename != NULL);

	CpgNetwork *other;
	
	other = cpg_network_new_from_file (filename);
	
	if (other != NULL)
	{
		cpg_network_merge (network, other);
		g_object_unref (other);
	}
}

/**
 * cpg_network_merge_from_xml:
 * @network: a #CpgNetwork
 * @xml: a xml string describing the network
 *
 * Merges the network defined in @xml into @network. This is
 * similar to creating a network from xml and merging it with @network.
 *
 **/
void
cpg_network_merge_from_xml (CpgNetwork  *network,
                            gchar const *xml)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (xml != NULL);
	
	CpgNetwork *other;
	
	other = cpg_network_new_from_xml (xml);
	
	if (other != NULL)
	{
		cpg_network_merge (network, other);
		g_object_unref (other);
	}
}

/**
 * cpg_network_set_global_constant:
 * @network: a #CpgNetwork
 * @name: the constant name
 * @constant: the constant value
 *
 * Sets a constant in the network. Constants can be used to set global settings
 * for the whole network (e.g. oscillator frequency)
 *
 **/
void
cpg_network_set_global_constant (CpgNetwork   *network,
                                 gchar const  *name,
                                 gdouble       constant)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (name != NULL);
	
	CpgProperty *property = cpg_object_get_property (network->priv->constants,
	                                                 name);

	if (property == NULL)
	{
		property = cpg_object_add_property (network->priv->constants,
		                                    name,
		                                    "",
		                                    FALSE);
	}

	cpg_property_set_value (property, constant);
}

/**
 * cpg_network_get_globals:
 * @network: a #CpgNetwork
 *
 * Get the #CpgObject containing all the global constants
 *
 * Returns: the #CpgObject containing the global constants
 *
 **/
CpgObject *
cpg_network_get_globals (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	
	return network->priv->constants;
}

/**
 * cpg_network_write_to_xml:
 * @network: a #CpgNetwork
 *
 * Get xml representation of the network
 *
 * Returns: a string containing the xml representation of the network
 *
 **/
gchar *
cpg_network_write_to_xml (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return cpg_network_writer_xml_string (network);
}

/**
 * cpg_network_write_to_file:
 * @network: a #CpgNetwork
 * @filename: a filename
 *
 * Write the xml representation of the network to file
 *
 **/
void			 
cpg_network_write_to_file (CpgNetwork  *network,
                           gchar const *filename)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (filename != NULL);
	
	cpg_network_writer_xml (network, filename);
}

static void
fill_templates (gchar const  *key,
                CpgObject    *template,
                GSList      **list)
{
	*list = g_slist_prepend (*list, g_strdup (key));
}

/**
 * cpg_network_get_templates:
 * @network: a #CpgNetwork
 *
 * Get a list of template names for @network. The names in the list are owned
 * by the caller and need to be freed accordingly:
 *
 * g_slist_foreach (templates, (GFunc)g_free, NULL);
 *
 * Returns: a list of template names
 *
 **/
GSList *
cpg_network_get_templates (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	
	GSList *list = NULL;
	
	g_hash_table_foreach (network->priv->templates, 
	                      (GHFunc)fill_templates,
	                      &list);

	return g_slist_reverse (list);
}

/**
 * cpg_network_add_template:
 * @network: a #CpgNetwork
 * @name: the template name
 * @object: the template object
 *
 * Adds a new template to the network. Templates can be used to define a
 * basis for constructing new states/links. This can be very useful to keep
 * the xml representation of the network small.
 *
 **/
void
cpg_network_add_template (CpgNetwork  *network,
                          gchar const *name,
                          CpgObject   *object)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (name != NULL);
	g_return_if_fail (CPG_IS_OBJECT (object));
	
	g_hash_table_insert (network->priv->templates,
	                     g_strdup (name),
	                     g_object_ref (object));
}

/**
 * cpg_network_get_template:
 * @network: a #CpgNetwork
 * @name: the template name
 *
 * Get a registered template object
 *
 * Returns: a template object or %NULL if the template could not be found
 *
 **/
CpgObject *
cpg_network_get_template (CpgNetwork  *network,
                          gchar const *name)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	return g_hash_table_lookup (network->priv->templates, name);
}

/**
 * cpg_network_remove_template:
 * @network: a #CpgNetwork
 * @name: the template name
 *
 * Remove a registered template object. Any objects based on this template
 * will become standalone objects.
 *
 **/
void
cpg_network_remove_template (CpgNetwork  *network,
                             gchar const *name)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (name != NULL);
	
	g_hash_table_remove (network->priv->templates, name);	
}

/**
 * cpg_network_add_from_template:
 * @network: a #CpgNetwork
 * @name: template name
 *
 * Add a new object to the network based on a template. Do not use this for
 * constructing links from templates. To construct links, use 
 * #cpg_network_add_link_from_template instead.
 * 
 * Returns: a new #CpgObject. The object is already added to the network
 *
 **/
CpgObject *
cpg_network_add_from_template (CpgNetwork  *network,
                               gchar const *name)

{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	
	CpgObject *template = cpg_network_get_template (network, name);
	
	if (template == NULL || CPG_IS_LINK (template))
	{
		return NULL;
	}
	
	// Make copy of the object and insert it
	CpgObject *ret = _cpg_object_copy (template);
	
	cpg_network_add_object (network, ret);
	g_object_unref (ret);

	return ret;
}

/**
 * cpg_network_add_link_from_template:
 * @network: a #CpgNetwork
 * @name: template name
 * @from: a #CpgObject
 * @to: a #CpgObject
 *
 * Add a new link to the network based on a template.
 * 
 * Returns: a new #CpgObject. The object is already added to the network
 *
 **/
CpgObject *
cpg_network_add_link_from_template (CpgNetwork  *network,
                                    gchar const *name,
                                    CpgObject   *from,
                                    CpgObject   *to)

{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (name != NULL, NULL);
	
	CpgObject *template = cpg_network_get_template (network, name);
	
	if (template == NULL || !CPG_IS_LINK (template))
	{
		return NULL;
	}
	
	CpgObject *object = _cpg_object_copy (template);
	g_object_set (G_OBJECT (object), "from", from, "to", to, NULL);

	cpg_network_add_object (network, object);
	return object;
}
