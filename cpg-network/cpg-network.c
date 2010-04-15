#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-relay.h"
#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-debug.h"
#include "cpg-network-reader.h"
#include "cpg-network-writer.h"
#include "cpg-integrator-euler.h"

/**
 * SECTION:network
 * @short_description: The main CPG network object
 *
 * The cpg network is the main component of the cpg-network library. The network
 * consists of #CpgState, #CpgRelay and #CpgLink objects which combined make
 * up the network.
 *
 * The easiest way of using the library is to write the network using the 
 * XML representation (see #xml-specification). You then create the network
 * from file using #cpg_network_new_from_file. To simulate the network, use
 * #cpg_network_run or for running single steps #cpg_network_step.
 * 
 * For more information, see 
 * <link linkend='using-the-network'>Using the network</link>.
 *
 */

#define CPG_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_NETWORK, CpgNetworkPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_COMPILED,
	PROP_INTEGRATOR
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
	gboolean compiled;
	
	CpgObject *globals;

	GHashTable *templates;

	GSList *functions;
	CpgIntegrator *integrator;

	GSList *state;
};

enum
{
	RESET,
	COMPILE_ERROR,
	NUM_SIGNALS
};

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgNetwork, cpg_network, G_TYPE_OBJECT)

GQuark
cpg_network_load_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
		quark = g_quark_from_static_string ("cpg_network_load_error");

	return quark;
}

static void
cpg_network_finalize (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	g_free (network->priv->filename);
	
	cpg_network_clear (network);

	g_hash_table_destroy (network->priv->object_map);
	g_hash_table_destroy (network->priv->templates);

	g_object_unref (network->priv->globals);
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
		case PROP_COMPILED:
			g_value_set_boolean (value, self->priv->compiled);
		break;
		case PROP_INTEGRATOR:
			g_value_set_object (value, self->priv->integrator);
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
		case PROP_COMPILED:
			self->priv->compiled = g_value_get_boolean (value);
		break;
		case PROP_INTEGRATOR:
			if (self->priv->integrator == g_value_get_object (value))
			{
				return;
			}

			if (self->priv->integrator)
			{
				g_object_unref (self->priv->integrator);
			}

			self->priv->integrator = g_value_dup_object (value);
			g_object_set (self->priv->integrator, "network", self, NULL);

			cpg_network_taint (self);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_dispose (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (network->priv->integrator)
	{
		g_object_unref (network->priv->integrator);
		network->priv->integrator = NULL;
	}

	G_OBJECT_CLASS (cpg_network_parent_class)->dispose (object);
}

static void
cpg_network_class_init (CpgNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_network_finalize;
	object_class->dispose = cpg_network_dispose;
	object_class->get_property = cpg_network_get_property;
	object_class->set_property = cpg_network_set_property;

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

	g_object_class_install_property (object_class,
	                                 PROP_INTEGRATOR,
	                                 g_param_spec_object ("integrator",
	                                                      "Integrator",
	                                                      "Integrator",
	                                                      CPG_TYPE_INTEGRATOR,
	                                                      G_PARAM_READWRITE));
	

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
	network->priv->globals = cpg_object_new ("Globals");
	
	g_signal_connect_swapped (network->priv->globals, 
	                          "tainted", 
	                          G_CALLBACK (cpg_network_taint), 
	                          network);
	
	network->priv->templates = g_hash_table_new_full (g_str_hash, 
	                                                  g_str_equal,
	                                                  (GDestroyNotify)g_free,
	                                                  (GDestroyNotify)g_object_unref);

	/* Create default integrator */
	CpgIntegratorEuler *integrator = cpg_integrator_euler_new ();
	cpg_network_set_integrator (network, CPG_INTEGRATOR (integrator));
	g_object_unref (integrator);
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

	if (!CPG_IS_FUNCTION (object))
	{
		g_signal_handlers_disconnect_by_func (object, 
			                                  G_CALLBACK (update_object_id), 
			                                  network);

		g_hash_table_remove (network->priv->object_map, cpg_object_get_id (object));
	}
}

static void
register_object (CpgNetwork *network,
                 CpgObject  *object)
{
	g_signal_connect_swapped (object, 
	                          "tainted", 
	                          G_CALLBACK (cpg_network_taint), 
	                          network);

	if (!CPG_IS_FUNCTION (object))
	{
		g_signal_connect (object, 
			              "notify::id", 
			              G_CALLBACK (update_object_id), 
			              network);

		register_id (network, object);
	}
}

static void
update_state (CpgNetwork *network)
{
	g_slist_foreach (network->priv->state, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (network->priv->state);

	network->priv->state = NULL;

	/* Collect all the states in the network */
	GSList *item;

	for (item = network->priv->states; item; item = g_slist_next (item))
	{
		CpgObject *object = CPG_OBJECT (item->data);

		GSList *actors = cpg_object_get_actors (object);

		while (actors)
		{
			network->priv->state = g_slist_prepend (network->priv->state,
			                                        cpg_integrator_state_new ((CpgProperty *)actors->data));

			actors = g_slist_next (actors);
		}
	}

	network->priv->state = g_slist_reverse (network->priv->state);
}

static void
set_compiled (CpgNetwork *network,
              gboolean    compiled)
{
	if (network->priv->compiled != compiled)
	{
		network->priv->compiled = compiled;
		g_object_notify (G_OBJECT (network), "compiled");

		if (compiled)
		{
			update_state (network);
		}
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
	{
		add_link (network, object);
	}
	else if (CPG_IS_STATE (object) || CPG_IS_RELAY (object))
	{
		add_state (network, object);
	}
	
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

typedef struct
{
	CpgNetwork *network;
	CpgCompileContext *context;
	CpgCompileError *error;
	gboolean failed;
} CompileInfo;

static void
compile_each_object (CpgObject   *object,
                     CompileInfo *info)
{
	if (info->failed)
	{
		return;
	}

	info->failed = !cpg_object_compile (object, info->context, info->error);
}

static gboolean
cpg_network_compile_each (CpgNetwork        *network,
                          CpgCompileContext *context,
                          CpgCompileError   *error,
                          GSList            *objects)
{
	CompileInfo info = {network, context, error, FALSE};

	g_slist_foreach (objects, (GFunc)compile_each_object, &info);
	return !info.failed;
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
	gboolean ret = TRUE;

	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);

	set_compiled (network, FALSE);

	CpgCompileContext *context = cpg_compile_context_new ();

	cpg_compile_context_prepend_object (context, CPG_OBJECT (network->priv->integrator));
	cpg_compile_context_prepend_object (context, network->priv->globals);
	cpg_compile_context_set_functions (context, network->priv->functions);

	/* Compile functions */
	if (!cpg_network_compile_each (network, context, error, network->priv->functions))
	{
		/* Functions */
		cpg_ref_counted_unref (context);
		ret = FALSE;
	}
	else if (!cpg_network_compile_each (network, context, error, network->priv->states))
	{
		/* States */
		cpg_ref_counted_unref (context);
		ret = FALSE;
	}
	else if (!cpg_network_compile_each (network, context, error, network->priv->links))
	{
		/* Links */
		cpg_ref_counted_unref (context);
		ret = FALSE;
	}
	else if (!cpg_object_compile (network->priv->globals, context, error))
	{
		/* Globals */
		cpg_ref_counted_unref (context);
		ret = FALSE;
	}

	if (!ret)
	{
		if (error)
		{
			g_signal_emit (network, network_signals[COMPILE_ERROR], 0, error);
		}
	}
	else
	{
		set_compiled (network, TRUE);
		cpg_network_reset (network);
	}

	return ret;
}

/**
 * cpg_network_new_from_file:
 * @filename: the filename of the file containing the network definition
 * @error: error return value
 * 
 * Create a new CPG network by reading the network definition from file
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_file (gchar const *filename, GError **error)
{
	g_return_val_if_fail (filename != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();
	network->priv->filename = strdup (filename);
	
	if (!cpg_network_reader_xml (network, filename, error))
	{
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
 * @error: error return value
 * 
 * Create a new CPG network from the network xml definition
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_xml (gchar const *xml, GError **error)
{
	g_return_val_if_fail (xml != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();
	
	if (!cpg_network_reader_xml_string (network, xml, error))
	{
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

static gint
compare_property_dependencies (CpgProperty *prop1,
                               CpgProperty *prop2)
{
	CpgExpression *e1 = cpg_property_get_value_expression (prop1);
	CpgExpression *e2 = cpg_property_get_value_expression (prop2);
	GSList *d1 = cpg_expression_get_dependencies (e1);
	GSList *d2 = cpg_expression_get_dependencies (e2);
	
	if (g_slist_find (d1, prop2) != NULL)
	{
		return 1;
	}
	else if (g_slist_find (d2, prop1) != NULL)
	{
		return -1;
	}
	else
	{
		return 0;
	}
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

	g_slist_foreach (network->priv->links, (GFunc)remove_object, network);
	g_slist_foreach (network->priv->states, (GFunc)remove_object, network);
	
	g_slist_free (network->priv->states);
	g_slist_free (network->priv->links);
	
	network->priv->states = NULL;
	network->priv->links = NULL;

	GSList *props = g_slist_copy (cpg_object_get_properties (network->priv->globals));
	GSList *item;

	props = g_slist_sort (props, (GCompareFunc)compare_property_dependencies);
	
	for (item = props; item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;

		cpg_object_remove_property (network->priv->globals, 
		                            cpg_property_get_name (property),
		                            NULL);
	}
	
	g_slist_free (props);
	
	/* Clear templates */
	g_hash_table_remove_all (network->priv->templates);

	/* Clear functions */
	g_slist_foreach (network->priv->functions, (GFunc)g_object_unref, NULL);
	g_slist_free (network->priv->functions);

	network->priv->functions = NULL;

	g_slist_foreach (network->priv->state, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (network->priv->state);

	network->priv->state = NULL;
}

static gboolean
ensure_compiled (CpgNetwork *network)
{
	if (network->priv->compiled)
	{
		return TRUE;
	}

	CpgCompileError *error = cpg_compile_error_new ();

	if (!cpg_network_compile (network, error))
	{
		cpg_ref_counted_unref (error);
		return FALSE;
	}

	cpg_ref_counted_unref (error);
	return TRUE;
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

	if (ensure_compiled (network))
	{
		cpg_integrator_step (network->priv->integrator,
			                 network->priv->state,
			                 cpg_integrator_get_time (network->priv->integrator),
			                 timestep);
	}
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

	if (ensure_compiled (network))
	{
		cpg_integrator_run (network->priv->integrator,
			                network->priv->state,
			                from,
			                timestep,
			                to);
	}
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

	// reset all objects
	g_slist_foreach (network->priv->states, (GFunc)cpg_object_reset, NULL);
	g_slist_foreach (network->priv->links, (GFunc)cpg_object_reset, NULL);
	
	cpg_object_reset (network->priv->globals);

	g_slist_foreach (network->priv->functions, (GFunc)cpg_object_reset, NULL);

	if (network->priv->integrator)
	{
		cpg_integrator_reset (network->priv->integrator,
		                      cpg_network_get_integration_state (network));
	}

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
	
	for (item = cpg_object_get_properties (other->priv->globals); item; item = g_slist_next (item))
	{
		CpgProperty *property = (CpgProperty *)item->data;
		
		if (!cpg_object_get_property (network->priv->globals,
		                              cpg_property_get_name (property)))
		{
			cpg_object_add_property (network->priv->globals,
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

	// Copy over functions
	for (item = other->priv->functions; item; item = g_slist_next (item))
	{
		cpg_network_add_function (network, CPG_FUNCTION (item->data));
	}
}

/**
 * cpg_network_merge_from_file:
 * @network: a #CpgNetwork
 * @filename: network filename
 * @error: error return value
 *
 * Merges the network defined in the file @filename into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
void
cpg_network_merge_from_file (CpgNetwork   *network,
                             gchar const  *filename,
                             GError      **error)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (filename != NULL);

	CpgNetwork *other;
	
	other = cpg_network_new_from_file (filename, error);
	
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
 * @error: error return value
 *
 * Merges the network defined in @xml into @network. This is
 * similar to creating a network from xml and merging it with @network.
 *
 **/
void
cpg_network_merge_from_xml (CpgNetwork   *network,
                            gchar const  *xml,
                            GError      **error)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (xml != NULL);
	
	CpgNetwork *other;
	
	other = cpg_network_new_from_xml (xml, error);
	
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
 * Sets a constant in the network. Globals can be used to set global settings
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
	
	CpgProperty *property = cpg_object_get_property (network->priv->globals,
	                                                 name);

	if (property == NULL)
	{
		property = cpg_object_add_property (network->priv->globals,
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
 * Get the #CpgObject containing all the global globals
 *
 * Returns: the #CpgObject containing the global globals
 *
 **/
CpgObject *
cpg_network_get_globals (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	
	return network->priv->globals;
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

static gboolean
check_template (GSList *templates, gchar const *name)
{
	while (templates)
	{
		if (g_strcmp0 (name, (gchar const *)templates->data) == 0)
		{
			return TRUE;
		}

		templates = g_slist_next (templates);
	}
	
	return FALSE;
}

static GSList *
sort_templates (CpgNetwork *network,
               GSList      *templates)
{
	GSList *sorted = NULL;
	GSList *ptr = templates;
	CpgObject *seen = NULL;
	
	while (ptr)
	{
		gchar *name = (gchar *)ptr->data;
		CpgObject *orig = cpg_network_get_template (network, name);
		CpgObject *template = NULL;
		g_object_get (G_OBJECT (orig), "template", &template, NULL);
		
		if (seen == orig || !template || check_template (sorted, cpg_object_get_id (template)))
		{
			sorted = g_slist_prepend (sorted, name);
			ptr = g_slist_next (ptr);
			
			seen = NULL;
		}
		else
		{
			// Template not yet added, so cycle it
			ptr = g_slist_next (ptr);
			ptr = g_slist_append (ptr, name);
			
			if (seen == NULL)
			{
				seen = orig;
			}
		}
	}
	
	g_slist_free (templates);
	return g_slist_reverse (sorted);
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

	return sort_templates (network, g_slist_reverse (list));
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
	g_object_unref (object);

	return object;
}

/**
 * cpg_network_add_function:
 * @network: A #CpgNetwork
 * @function: A #CpgFunction
 * 
 * Add a new custom user function to the network.
 *
 **/
void
cpg_network_add_function (CpgNetwork  *network,
                          CpgFunction *function)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_FUNCTION (function));

	if (cpg_network_get_function (network, cpg_object_get_id (CPG_OBJECT (function))))
	{
		return;
	}

	network->priv->functions = g_slist_append (network->priv->functions,
	                                           g_object_ref (function));

	register_object (network, CPG_OBJECT (function));
	set_compiled (network, FALSE);
}

/**
 * cpg_network_remove_function:
 * @network: A #CpgNetwork
 * @function: A #CpgFunction
 * 
 * Remove a custom user function from the network.
 *
 **/
void
cpg_network_remove_function (CpgNetwork  *network,
                             CpgFunction *function)
{
	GSList *item;

	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_FUNCTION (function));

	item = g_slist_find (network->priv->functions, function);

	if (item)
	{
		unregister_object (network, CPG_OBJECT (function));
		g_object_unref (item->data);
		network->priv->functions = g_slist_delete_link (network->priv->functions, item);
	}

	set_compiled (network, FALSE);
}

/**
 * cpg_network_get_functions:
 * @network: A #CpgNetwork
 * 
 * Get the custom user functions defined in the network.
 *
 * Returns: A #GSList of #CpgFunction.
 *
 **/
GSList *
cpg_network_get_functions (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->functions;
}

/**
 * cpg_network_get_function:
 * @network: A #CpgNetwork
 * @name: The function name
 * 
 * Get a custom user function defined in the network.
 *
 * Returns: A #CpgFunction if a function with @name could be found, %NULL
 *          otherwise
 *
 **/
CpgFunction *
cpg_network_get_function (CpgNetwork  *network,
                          gchar const *name)
{
	GSList *item;

	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (item = network->priv->functions; item; item = g_slist_next (item))
	{
		CpgFunction *func = CPG_FUNCTION (item->data);

		if (g_strcmp0 (cpg_object_get_id (CPG_OBJECT (func)), name) == 0)
		{
			return func;
		}
	}

	return NULL;
}

/**
 * cpg_network_get_compiled:
 * @network: A #CpgNetwork
 * 
 * Get whether the network is currently compiled.
 *
 * Returns: %TRUE if the network is compiled, %FALSE otherwise
 *
 **/
gboolean
cpg_network_get_compiled (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);

	return network->priv->compiled;
}

/**
 * cpg_network_set_integrator:
 * @network: A #CpgNetwork
 * @integrator: A #CpgIntegrator
 * 
 * Set the integrator used to integrate the network.
 *
 **/
void
cpg_network_set_integrator (CpgNetwork    *network,
                            CpgIntegrator *integrator)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	g_object_set (network, "integrator", integrator, NULL);
}

/**
 * cpg_network_get_integrator:
 * @network: A #CpgNetwork
 * 
 * Get the integrator currently associated with the network.
 *
 * Returns: A #CpgIntegrator
 *
 **/
CpgIntegrator *
cpg_network_get_integrator (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->integrator;
}

/**
 * cpg_network_get_integration_state:
 * @network: A #CpgNetwork
 * 
 * Get the list of properties that need to be integrated.
 *
 * Returns: A #GSList of #CpgProperty. The list is owned by the network and
 * should not be freed.
 *
 **/
GSList *
cpg_network_get_integration_state (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	if (!network->priv->state)
	{
		update_state (network);
	}

	return network->priv->state;
}
