#include "cpg-monitor.h"

#include "cpg-monitor.h"
#include "cpg-property.h"
#include "cpg-utils.h"
#include "cpg-network.h"
#include "cpg-expression.h"

#include <string.h>

/**
 * SECTION:cpg-monitor
 * @short_description: Property value monitor
 *
 * A #CpgMonitor can be used to monitor the value of a certain #CpgProperty
 * while simulating. The monitor will collect the value of the property at
 * each simulation step and provides methods to access these values.
 * Particularly useful is #cpg_monitor_get_data_resampled which retrieves the
 * data resampled at specific times.
 *
 */

#define CPG_MONITOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_MONITOR, CpgMonitorPrivate))

#define MONITOR_GROW_SIZE 1000

enum
{
	RESETTED,
	NOTIFY_INTEGRATOR,
	STEP,
	BEGIN,
	NUM_SIGNALS
};

struct _CpgMonitorPrivate
{
	CpgNetwork  *network;
	CpgProperty *property;
	CpgIntegrator *integrator;

	gdouble *values;
	gdouble *sites;
	guint num_values;
	guint size;

	guint signals[NUM_SIGNALS];
};

G_DEFINE_TYPE (CpgMonitor, cpg_monitor, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_PROPERTY
};

static void
reset_monitor (CpgMonitor *monitor)
{
	g_free (monitor->priv->values);
	g_free (monitor->priv->sites);

	monitor->priv->values = NULL;
	monitor->priv->sites = NULL;
	monitor->priv->size = 0;
	monitor->priv->num_values = 0;
}

static void
disconnect_integrator (CpgMonitor *monitor)
{
	if (!monitor->priv->integrator)
	{
		return;
	}

	g_signal_handler_disconnect (monitor->priv->integrator,
	                             monitor->priv->signals[STEP]);

	g_signal_handler_disconnect (monitor->priv->integrator,
	                             monitor->priv->signals[BEGIN]);

	g_object_remove_weak_pointer (G_OBJECT (monitor->priv->integrator),
	                              (gpointer *)(&monitor->priv->integrator));

	monitor->priv->integrator = NULL;
}

static void
disconnect_network (CpgMonitor *monitor)
{
	if (!monitor->priv->network)
	{
		return;
	}

	g_signal_handler_disconnect (monitor->priv->network,
	                             monitor->priv->signals[RESETTED]);

	g_signal_handler_disconnect (monitor->priv->network,
	                             monitor->priv->signals[NOTIFY_INTEGRATOR]);

	g_object_remove_weak_pointer (G_OBJECT (monitor->priv->network),
	                              (gpointer *)(&monitor->priv->network));

	monitor->priv->network = NULL;
}

static void
cpg_monitor_grow (CpgMonitor *monitor)
{
	monitor->priv->size += MONITOR_GROW_SIZE;

	array_resize (monitor->priv->values, double, monitor->priv->size);
	array_resize (monitor->priv->sites, double, monitor->priv->size);
}

static void
cpg_monitor_update (CpgMonitor    *monitor,
                    CpgIntegrator *integrator)
{
	if (monitor->priv->size == 0 ||
	    monitor->priv->num_values >= monitor->priv->size - 1)
	{
		cpg_monitor_grow (monitor);
	}

	monitor->priv->values[monitor->priv->num_values] = cpg_property_get_value (monitor->priv->property);
	monitor->priv->sites[monitor->priv->num_values++] = integrator ? cpg_integrator_get_time (integrator) : 0;
}

static void
cpg_monitor_begin (CpgMonitor    *monitor,
                   CpgIntegrator *integrator)
{
	/* Record first value */
	cpg_monitor_update (monitor, integrator);
}

static void
connect_integrator (CpgMonitor *monitor)
{
	disconnect_integrator (monitor);

	CpgIntegrator *integrator = cpg_network_get_integrator (monitor->priv->network);

	if (integrator)
	{
		monitor->priv->signals[STEP] =
			g_signal_connect_swapped (integrator,
			                          "step",
			                          G_CALLBACK (cpg_monitor_update),
			                          monitor);

		monitor->priv->signals[BEGIN] =
			g_signal_connect_swapped (integrator,
			                          "begin",
			                          G_CALLBACK (cpg_monitor_begin),
			                          monitor);
	}

	monitor->priv->integrator = integrator;

	g_object_add_weak_pointer (G_OBJECT (monitor->priv->integrator),
	                           (gpointer *)&monitor->priv->integrator);
}

static void
cpg_monitor_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_monitor_parent_class)->finalize (object);
}

static void
cpg_monitor_dispose (GObject *object)
{
	CpgMonitor *monitor = CPG_MONITOR (object);

	disconnect_integrator (monitor);
	disconnect_network (monitor);
	reset_monitor (monitor);

	if (monitor->priv->property)
	{
		g_object_unref (monitor->priv->property);
		monitor->priv->property = NULL;
	}

	G_OBJECT_CLASS (cpg_monitor_parent_class)->dispose (object);
}

static void
monitor_get_property (GObject    *object,
                      guint       prop_id,
                      GValue     *value,
                      GParamSpec *pspec)
{
	CpgMonitor *self = CPG_MONITOR (object);
	
	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
		break;
		case PROP_PROPERTY:
			g_value_set_object (value, self->priv->property);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
on_network_resetted (CpgMonitor *monitor)
{
	reset_monitor (monitor);
}

static void
on_integrator_changed (CpgMonitor *monitor,
                       GParamSpec *spec,
                       CpgNetwork *network)
{
	connect_integrator (monitor);
}

static void
set_network (CpgMonitor *monitor,
             CpgNetwork *network)
{
	monitor->priv->network = network;

	g_object_add_weak_pointer (G_OBJECT (monitor->priv->network),
	                           (gpointer *)&monitor->priv->network);

	monitor->priv->signals[RESETTED] =
		g_signal_connect_swapped (network,
		                          "resetted",
		                          G_CALLBACK (on_network_resetted),
		                          monitor);

	monitor->priv->signals[NOTIFY_INTEGRATOR] =
		g_signal_connect_swapped (network,
		                          "notify::integrator",
		                          G_CALLBACK (on_integrator_changed),
		                          monitor);

	connect_integrator (monitor);
}
static void
monitor_set_property (GObject      *object,
                      guint         prop_id,
                      const GValue *value,
                      GParamSpec   *pspec)
{
	CpgMonitor *self = CPG_MONITOR (object);
	
	switch (prop_id)
	{
		case PROP_NETWORK:
			set_network (self, g_value_get_object (value));
		break;
		case PROP_PROPERTY:
			self->priv->property = g_value_dup_object (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_monitor_class_init (CpgMonitorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_monitor_finalize;
	object_class->dispose = cpg_monitor_dispose;

	object_class->get_property = monitor_get_property;
	object_class->set_property = monitor_set_property;

	g_type_class_add_private (object_class, sizeof(CpgMonitorPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));


	g_object_class_install_property (object_class,
	                                 PROP_PROPERTY,
	                                 g_param_spec_object ("property",
	                                                      "Property",
	                                                      "Property",
	                                                      CPG_TYPE_PROPERTY,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_monitor_init (CpgMonitor *self)
{
	self->priv = CPG_MONITOR_GET_PRIVATE (self);

	cpg_monitor_grow (self);
}

/**
 * cpg_monitor_new:
 * @network: (type CpgNetwork): a #CpgNetwork
 * @property: a #CpgProperty
 *
 * Create a new monitor for monitoring property @property.
 *
 * Returns: a new #CpgMonitor
 *
 **/
CpgMonitor *
cpg_monitor_new (CpgNetwork  *network,
                 CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return g_object_new (CPG_TYPE_MONITOR,
	                     "network", network,
	                     "property", property,
	                     NULL);
}

/**
 * cpg_monitor_get_data:
 * @monitor: a #CpgMonitor
 * @size: (out caller-allocates): return value for number of values
 *
 * Returns the data as monitored during the simulation. See also
 * #cpg_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data
 *
 * Returns: (array type=double length=size): internal array of monitored values. The pointer should
 * not be freed
 *
 **/
gdouble const *
cpg_monitor_get_data (CpgMonitor *monitor,
                      guint      *size)
{
	g_return_val_if_fail (CPG_IS_MONITOR (monitor), NULL);

	if (size)
	{
		*size = 0;
	}

	if (!monitor || !monitor->priv->property)
	{
		return NULL;
	}

	if (size)
	{
		*size = monitor->priv->num_values;
	}

	return monitor->priv->values;
}

/**
 * cpg_monitor_get_sites:
 * @monitor: a #CpgMonitor
 * @size: (out caller-allocates): return value for number of values
 *
 * Returns the data sites as monitored during the simulation. See also
 * #cpg_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data
 *
 * Returns: (array type=double length=size): internal array of monitored sites. The pointer should
 * not be freed
 *
 **/
gdouble const *
cpg_monitor_get_sites (CpgMonitor *monitor,
                       guint      *size)
{
	g_return_val_if_fail (CPG_IS_MONITOR (monitor), NULL);

	if (size)
	{
		*size = 0;
	}

	if (!monitor || !monitor->priv->property)
	{
		return NULL;
	}

	if (size)
	{
		*size = monitor->priv->num_values;
	}

	return monitor->priv->sites;
}

static int
bsearch_find (gdouble const  *list,
              gint            size,
              gdouble         value)
{
	gint left = 0;
	gint right = size;

	while (right > left)
	{
		gint probe = (left + right) / 2;

		if (list[probe] > value)
		{
			right = probe - 1;
		}
		else if (list[probe] < value)
		{
			left = probe + 1;
		}
		else
		{
			return probe;
		}
	}

	return right + (right < size && list[right] < value ? 1 : 0);
}

/**
 * cpg_monitor_get_data_resampled:
 * @monitor: a #CpgMonitor
 * @sites: (array type=double length=size): the data sites at which to resample the data
 * @size: the size of the data sites array
 * @ret: (out callee-allocates): the return location for the resampled data
 *
 * Returns the data as monitored during the simulation, but resampled at
 * specific data sites. @ret will have to be already allocated and large
 * enough to hold @size values.
 *
 * Returns: %TRUE if @ret was successfully filled with data, %FALSE otherwise
 *
 **/
gboolean
cpg_monitor_get_data_resampled (CpgMonitor     *monitor,
                                gdouble const  *sites,
                                guint           size,
                                gdouble        *ret)
{
	g_return_val_if_fail (CPG_IS_MONITOR (monitor), FALSE);
	
	if (!sites || size == 0 || !monitor || !monitor->priv->property)
	{
		memset (ret, 0, sizeof (double) * size);
		return FALSE;
	}

	gdouble const *data = monitor->priv->values;
	guint i;

	gdouble const *monsites = monitor->priv->sites;

	for (i = 0; i < size; ++i)
	{
		guint idx = bsearch_find (monsites,
		                          (gint)monitor->priv->num_values,
		                          sites[i]);

		guint fidx = idx > 0 ? idx - 1 : 0;
		guint sidx = idx < monitor->priv->num_values ? idx : monitor->priv->num_values - 1;

		if (fidx >= monitor->priv->num_values ||
		    sidx >= monitor->priv->num_values)
		{
			ret[i] = 0.0;
		}
		else
		{
			// interpolate between the values
			gdouble factor = monsites[sidx] == monsites[fidx] ? 1 : (monsites[sidx] - sites[i]) / (monsites[sidx] - monsites[fidx]);
			ret[i] = data[fidx] * factor + (data[sidx] * (1 - factor));
		}
	}

	return TRUE;
}

/**
 * cpg_monitor_get_property:
 * @monitor: a #CpgMonitor
 *
 * Returns the property which is being monitored.
 *
 * Returns: a pointer to the #CpgProperty
 *
 **/
CpgProperty *
cpg_monitor_get_property (CpgMonitor *monitor)
{
	g_return_val_if_fail (CPG_IS_MONITOR (monitor), NULL);
	return monitor->priv->property;
}
