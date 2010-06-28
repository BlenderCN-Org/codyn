#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-debug.h"
#include "cpg-integrator-euler.h"
#include "cpg-network-deserializer.h"

/**
 * SECTION:cpg-network
 * @short_description: The main CPG network object
 *
 * The cpg network is the main component of the cpg-network library. The network
 * consists of #CpgState and #CpgLink objects which combined make
 * up the network.
 *
 * The easiest way of using the library is to write the network using the
 * XML representation (see #xml-specification). You then create the network
 * from file using #cpg_network_new_from_file. To simulate the network, use
 * #cpg_network_run or for running single steps #cpg_network_step.
 *
 * For more information, see
 * <link linkend='making-a-network'>Making a network</link>.
 *
 */

#define CPG_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_NETWORK, CpgNetworkPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_INTEGRATOR,
	PROP_FILE,
	PROP_FILENAME
};

struct _CpgNetworkPrivate
{
	GFile *file;

	CpgIntegrator *integrator;
	CpgIntegratorState *integrator_state;

	CpgGroup *template_group;
	CpgGroup *function_group;
};

enum
{
	COMPILE_ERROR,
	NUM_SIGNALS
};

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgNetwork, cpg_network, CPG_TYPE_GROUP)

GQuark
cpg_network_load_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_network_load_error");
	}

	return quark;
}

static void
cpg_network_finalize (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	g_object_unref (network->priv->file);

	cpg_object_clear (CPG_OBJECT (network));

	g_object_unref (network->priv->template_group);
	g_object_unref (network->priv->function_group);

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
		case PROP_INTEGRATOR:
			g_value_set_object (value, self->priv->integrator);
		break;
		case PROP_FILE:
			g_value_set_object (value, self->priv->file);
		break;
		case PROP_FILENAME:
			if (self->priv->file)
			{
				g_value_take_string (value, g_file_get_path (self->priv->file));
			}
			else
			{
				g_value_set_string (value, NULL);
			}
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
			g_object_set (self->priv->integrator, "object", self, NULL);

			cpg_object_taint (CPG_OBJECT (self));
		break;
		case PROP_FILE:
		{
			GFile *file = g_value_get_object (value);
			self->priv->file = file ? g_file_dup (file) : NULL;
		}
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

	if (network->priv->integrator_state)
	{
		g_object_unref (network->priv->integrator_state);
		network->priv->integrator_state = NULL;
	}

	G_OBJECT_CLASS (cpg_network_parent_class)->dispose (object);
}

static gboolean
cpg_network_add_impl (CpgGroup  *group,
                      CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (group);

	/* Check if the network owns all the templates */
	GSList const *templates = cpg_object_get_applied_templates (object);

	while (templates)
	{
		CpgObject *template = templates->data;
		CpgObject *other = cpg_group_get_child (network->priv->template_group,
		                                        cpg_object_get_id (template));

		gboolean eq = other == template;

		if (!eq)
		{
			return FALSE;
		}

		templates = g_slist_next (templates);
	}

	if (CPG_GROUP_CLASS (cpg_network_parent_class)->add (group, object))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static void
cpg_network_reset_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	cpg_object_reset (CPG_OBJECT (network->priv->function_group));

	CPG_OBJECT_CLASS (cpg_network_parent_class)->reset (object);
}

typedef struct
{
	CpgNetwork *network;
	CpgCompileContext *context;
	CpgCompileError *error;
	gboolean failed;
} CompileInfo;

static gboolean
cpg_network_compile_impl (CpgObject         *object,
                          CpgCompileContext *context,
                          CpgCompileError   *error)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (!context)
	{
		context = cpg_compile_context_new ();
	}
	else
	{
		cpg_ref_counted_ref (context);
	}

	cpg_compile_context_prepend_object (context, CPG_OBJECT (network->priv->integrator));
	cpg_compile_context_prepend_object (context, object);

	cpg_compile_context_set_functions (context,
	                                   cpg_group_get_children (network->priv->function_group));

	gboolean ret = cpg_object_compile (CPG_OBJECT (network->priv->function_group),
	                                   context,
	                                   error);

	if (ret)
	{
		ret = CPG_OBJECT_CLASS (cpg_network_parent_class)->compile (object,
		                                                            context,
		                                                            error);
	}

	cpg_ref_counted_unref (context);

	if (!ret)
	{
		if (error)
		{
			g_signal_emit (network,
			               network_signals[COMPILE_ERROR],
			               0,
			               error);
		}
	}

	return ret;
}

static void
cpg_network_clear_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	CPG_OBJECT_CLASS (cpg_network_parent_class)->clear (object);

	cpg_object_clear (CPG_OBJECT (network->priv->template_group));
	cpg_object_clear (CPG_OBJECT (network->priv->function_group));
}

static void
cpg_network_reset_cache_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	CPG_OBJECT_CLASS (cpg_network_parent_class)->reset_cache (object);

	cpg_object_reset_cache (CPG_OBJECT (network->priv->function_group));
}

static void
cpg_network_class_init (CpgNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgGroupClass *group_class = CPG_GROUP_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	object_class->finalize = cpg_network_finalize;
	object_class->dispose = cpg_network_dispose;
	object_class->get_property = cpg_network_get_property;
	object_class->set_property = cpg_network_set_property;

	cpg_class->reset = cpg_network_reset_impl;
	cpg_class->compile = cpg_network_compile_impl;
	cpg_class->clear = cpg_network_clear_impl;
	cpg_class->reset_cache = cpg_network_reset_cache_impl;

	group_class->add = cpg_network_add_impl;

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
		              G_STRUCT_OFFSET (CpgNetworkClass,
		                               compile_error),
		              NULL,
		              NULL,
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


	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));


	g_object_class_install_property (object_class,
	                                 PROP_FILENAME,
	                                 g_param_spec_string ("filename",
	                                                      "Filename",
	                                                      "Filename",
	                                                      NULL,
	                                                      G_PARAM_READABLE));
}

static void
cpg_network_init (CpgNetwork *network)
{
	network->priv = CPG_NETWORK_GET_PRIVATE (network);

	network->priv->template_group = cpg_group_new ("templates", NULL);
	network->priv->function_group = cpg_group_new ("functions", NULL);

	g_signal_connect_swapped (network->priv->template_group,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          network);

	g_signal_connect_swapped (network->priv->function_group,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          network);

	network->priv->integrator_state = cpg_integrator_state_new (CPG_OBJECT (network));

	/* Create default integrator */
	CpgIntegratorEuler *integrator = cpg_integrator_euler_new ();
	cpg_network_set_integrator (network, CPG_INTEGRATOR (integrator));
	g_object_unref (integrator);

	cpg_integrator_set_state (CPG_INTEGRATOR (integrator),
	                          network->priv->integrator_state);
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
cpg_network_new ()
{
	return g_object_new (CPG_TYPE_NETWORK, NULL);
}

/**
 * cpg_network_new_from_file:
 * @file: the file containing the network definition
 * @error: error return value
 *
 * Create a new CPG network by reading the network definition from file
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_file (GFile   *file,
                           GError **error)
{
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	CpgNetwork *network = g_object_new (CPG_TYPE_NETWORK, "file", file, NULL);
	CpgNetworkDeserializer *deserializer;

	deserializer = cpg_network_deserializer_new (network,
	                                             NULL);

	if (!cpg_network_deserializer_deserialize_file (deserializer,
	                                                file,
	                                                error))
	{
		g_object_unref (network);
		network = NULL;
	}

	g_object_unref (deserializer);

	return network;
}

/**
 * cpg_network_new_from_path:
 * @path: The network file path
 * @error: A #GError
 * 
 * Create a new CPG network by reading the network definition from a file path.
 * See #cpg_network_new_from_file for more information.
 *
 * Returns: A #CpgNetwork
 *
 **/
CpgNetwork *
cpg_network_new_from_path (gchar const  *path,
                           GError      **error)
{
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CpgNetwork *network = cpg_network_new_from_file (file, error);
	g_object_unref (file);

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
cpg_network_new_from_xml (gchar const  *xml,
                          GError      **error)
{
	g_return_val_if_fail (xml != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();

	CpgNetworkDeserializer *deserializer;

	deserializer = cpg_network_deserializer_new (network,
	                                             NULL);

	GInputStream *stream = g_memory_input_stream_new_from_data (xml,
	                                                            -1,
	                                                            NULL);

	if (!stream)
	{
		g_object_unref (deserializer);
		return NULL;
	}

	gboolean ret;

	ret = cpg_network_deserializer_deserialize (deserializer,
	                                            stream,
	                                            error);

	g_object_unref (stream);

	if (!ret)
	{
		g_object_unref (network);
		network = NULL;
	}

	g_object_unref (deserializer);

	return network;
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

	cpg_integrator_step (network->priv->integrator,
	                     cpg_integrator_get_time (network->priv->integrator),
	                     timestep);
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

	cpg_integrator_run (network->priv->integrator,
	                    from,
	                    timestep,
	                    to);
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

	GSList *props = cpg_object_get_properties (CPG_OBJECT (other));
	GSList *item;

	for (item = props; item; item = g_slist_next (item))
	{
		CpgProperty *property = item->data;

		if (!cpg_object_get_property (CPG_OBJECT (network),
		                              cpg_property_get_name (property)))
		{
			cpg_object_add_property (CPG_OBJECT (network),
			                         cpg_property_get_name (property),
			                         cpg_expression_get_as_string (cpg_property_get_expression (property)),
			                         FALSE);
		}
	}

	g_slist_free (props);

	/* Copy over templates */
	CpgGroup *template_group = cpg_network_get_template_group (other);
	GSList const *templates = cpg_group_get_children (template_group);

	while (templates)
	{
		CpgObject *template = templates->data;

		if (!cpg_group_get_child (network->priv->template_group,
		                          cpg_object_get_id (template)))
		{
			cpg_group_add (network->priv->template_group,
			               template);
		}
	}

	/* Copy over children */
	GSList const *children = cpg_group_get_children (CPG_GROUP (other));

	while (children)
	{
		cpg_group_add (CPG_GROUP (network), children->data);
		children = g_slist_next (children);
	}

	CpgGroup *function_group = cpg_network_get_template_group (other);
	GSList const *functions = cpg_group_get_children (function_group);

	/* Copy over functions */
	while (functions)
	{
		CpgObject *function = functions->data;

		if (!cpg_group_get_child (network->priv->function_group,
		                          cpg_object_get_id (function)))
		{
			cpg_group_add (network->priv->function_group,
			               function);
		}
	}
}

/**
 * cpg_network_merge_from_file:
 * @network: a #CpgNetwork
 * @file: network file
 * @error: error return value
 *
 * Merges the network defined in the file @file into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
void
cpg_network_merge_from_file (CpgNetwork  *network,
                             GFile       *file,
                             GError     **error)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (G_IS_FILE (file));

	CpgNetwork *other;

	other = cpg_network_new_from_file (file, error);

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

	CpgObject *template = cpg_group_get_child (network->priv->template_group,
	                                           name);

	if (template == NULL || CPG_IS_LINK (template))
	{
		return NULL;
	}

	// Make copy of the object and insert it
	CpgObject *ret = _cpg_object_copy (template);

	cpg_group_add (CPG_GROUP (network), ret);
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

	CpgObject *template = cpg_group_get_child (network->priv->template_group,
	                                           name);

	if (template == NULL || !CPG_IS_LINK (template))
	{
		return NULL;
	}

	CpgObject *object = _cpg_object_copy (template);
	g_object_set (G_OBJECT (object), "from", from, "to", to, NULL);

	cpg_group_add (CPG_GROUP (network), object);
	g_object_unref (object);

	return object;
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
 * cpg_network_get_template_group:
 * @network: A #CpgNetwork
 * 
 * Get the group containing the templates.
 *
 * Returns: A #CpgGroup
 *
 **/
CpgGroup *
cpg_network_get_template_group (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->template_group;
}

/**
 * cpg_network_get_function_group:
 * @network: A #CpgNetwork
 * 
 * Get the group containing the user defined functions.
 *
 * Returns: A #CpgGroup
 *
 **/
CpgGroup *
cpg_network_get_function_group (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->function_group;
}

GFile *
cpg_network_get_file (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->file ? g_file_dup (network->priv->file) : NULL;
}
