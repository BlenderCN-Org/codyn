#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-debug.h"
#include "cpg-network-reader.h"
#include "cpg-network-writer.h"
#include "cpg-integrator-euler.h"

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
	PROP_INTEGRATOR
};

struct _CpgNetworkPrivate
{
	gchar *filename;

	CpgObject *globals;

	GSList *templates;
	GHashTable *template_hash;

	GSList *functions;
	CpgIntegrator *integrator;

	CpgIntegratorState *integrator_state;
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

	g_free (network->priv->filename);

	cpg_object_clear (CPG_OBJECT (network));

	g_hash_table_destroy (network->priv->template_hash);
	g_slist_free (network->priv->templates);

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
		CpgObject *other = g_hash_table_lookup (network->priv->template_hash,
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

	g_slist_foreach (network->priv->functions, (GFunc)cpg_object_reset, NULL);

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

	cpg_compile_context_set_functions (context, network->priv->functions);

	GSList *item;

	gboolean ret = TRUE;

	for (item = network->priv->functions; item; item = g_slist_next (item))
	{
		if (!cpg_object_compile (CPG_OBJECT (item->data), context, error))
		{
			cpg_ref_counted_unref (context);
			ret = FALSE;
		}
	}

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

	/* Clear templates */
	g_hash_table_remove_all (network->priv->template_hash);

	g_slist_free (network->priv->templates);
	network->priv->templates = NULL;

	/* Clear functions */
	g_slist_foreach (network->priv->functions, (GFunc)g_object_unref, NULL);
	g_slist_free (network->priv->functions);

	network->priv->functions = NULL;
}

static void
cpg_network_reset_cache_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	CPG_OBJECT_CLASS (cpg_network_parent_class)->reset_cache (object);

	g_slist_foreach (network->priv->functions,
	                 (GFunc)cpg_object_reset_cache,
	                 NULL);
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
}

static void
cpg_network_init (CpgNetwork *network)
{
	network->priv = CPG_NETWORK_GET_PRIVATE (network);

	network->priv->template_hash =
		g_hash_table_new_full (g_str_hash,
	                               g_str_equal,
	                               (GDestroyNotify)g_free,
	                               (GDestroyNotify)g_object_unref);

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
	GSList const *templates = cpg_network_get_templates (other);

	while (templates)
	{
		CpgObject *template = templates->data;

		if (!g_hash_table_lookup (network->priv->template_hash,
		                          cpg_object_get_id (template)))
		{
			cpg_network_add_template (network,
			                          cpg_object_get_id (template),
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

	/* Copy over functions */
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
GSList const *
cpg_network_get_templates (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->templates;
}

static gint
compare_templates (CpgObject *t1,
                   CpgObject *t2)
{
	GSList const *temp1 = cpg_object_get_applied_templates (t1);
	GSList const *temp2 = cpg_object_get_applied_templates (t2);

	if (g_slist_find ((GSList *)temp1, t2))
	{
		return 1;
	}
	else if (g_slist_find ((GSList *)temp2, t1))
	{
		return -1;
	}
	else
	{
		return 0;
	}
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

	if (g_hash_table_lookup (network->priv->template_hash,
	                         name))
	{
		return;
	}

	g_hash_table_insert (network->priv->template_hash,
	                     g_strdup (name),
	                     g_object_ref (object));

	network->priv->templates =
		g_slist_insert_sorted (network->priv->templates,
		                       object,
		                       (GCompareFunc)compare_templates);
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

	return g_hash_table_lookup (network->priv->template_hash, name);
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

	CpgObject *obj = cpg_network_get_template (network, name);

	if (obj)
	{
		g_hash_table_remove (network->priv->template_hash, name);
		network->priv->templates = g_slist_remove (network->priv->templates,
		                                           obj);
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

	CpgObject *template = cpg_network_get_template (network, name);

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

	CpgObject *template = cpg_network_get_template (network, name);

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

	g_signal_connect_swapped (function,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          network);

	cpg_object_taint (CPG_OBJECT (network));
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
		g_signal_handlers_disconnect_by_func (function,
		                                      cpg_object_taint,
		                                      network);

		g_object_unref (item->data);
		network->priv->functions = g_slist_delete_link (network->priv->functions, item);
	}

	cpg_object_taint (CPG_OBJECT (network));
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
