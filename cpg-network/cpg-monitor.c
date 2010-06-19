#include "cpg-monitor.h"
#include "cpg-property.h"
#include "cpg-utils.h"
#include "cpg-ref-counted-private.h"
#include "cpg-network.h"
#include "cpg-expression.h"

#include <string.h>

/**
 * SECTION:monitor
 * @short_description: Property value monitor
 *
 * A #CpgMonitor can be used to monitor the value of a certain #CpgProperty
 * while simulating. The monitor will collect the value of the property at
 * each simulation step and provides methods to access these values.
 * Particularly useful is #cpg_monitor_get_data_resampled which retrieves the
 * data resampled at specific times.
 *
 */

#define MONITOR_GROW_SIZE 1000

enum
{
	RESET,
	STEP,
	NOTIFY_INTEGRATOR,
	BEGIN,
	NUM_SIGNALS
};

struct _CpgMonitor
{
	CpgRefCounted parent;

	CpgNetwork  *network;
	CpgObject   *object;
	CpgProperty *property;
	CpgIntegrator *integrator;

	gdouble *values;
	gdouble *sites;
	guint num_values;
	guint size;

	guint signals[NUM_SIGNALS];
};

static void
cpg_monitor_free_data (CpgMonitor *monitor)
{
	g_free (monitor->values);
	g_free (monitor->sites);

	monitor->values = NULL;
	monitor->sites = NULL;
	monitor->size = 0;
	monitor->num_values = 0;
}

static void
disconnect_integrator (CpgMonitor *monitor)
{
	if (monitor->integrator)
	{
		g_signal_handler_disconnect (monitor->integrator, monitor->signals[STEP]);
		g_signal_handler_disconnect (monitor->integrator, monitor->signals[BEGIN]);

		g_object_remove_weak_pointer (G_OBJECT (monitor->integrator),
		                              (gpointer *)(&monitor->integrator));

		monitor->integrator = NULL;
	}
}

static void
cpg_monitor_free (CpgMonitor *monitor)
{
	g_object_unref (monitor->property);

	if (monitor->network)
	{
		g_signal_handler_disconnect (monitor->network, monitor->signals[RESET]);
		g_signal_handler_disconnect (monitor->network, monitor->signals[NOTIFY_INTEGRATOR]);
	}

	disconnect_integrator (monitor);

	cpg_monitor_free_data (monitor);
	g_slice_free (CpgMonitor, monitor);
}

GType
cpg_monitor_get_type ()
{
	static GType type_id = 0;

	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgMonitor",
		                                        cpg_ref_counted_ref,
		                                        cpg_ref_counted_unref);
	}

	return type_id;
}

static void
cpg_monitor_grow (CpgMonitor *monitor)
{
	monitor->size += MONITOR_GROW_SIZE;

	array_resize (monitor->values, double, monitor->size);
	array_resize (monitor->sites, double, monitor->size);
}

static void
cpg_monitor_update (CpgMonitor    *monitor,
                    CpgIntegrator *integrator)
{
	if (monitor->size == 0 || monitor->num_values >= monitor->size - 1)
	{
		cpg_monitor_grow (monitor);
	}

	monitor->values[monitor->num_values] = cpg_property_get_value (monitor->property);
	monitor->sites[monitor->num_values++] = integrator ? cpg_integrator_get_time (integrator) : 0;
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

	CpgIntegrator *integrator = cpg_network_get_integrator (monitor->network);

	if (integrator)
	{
		monitor->signals[STEP] = g_signal_connect_swapped (integrator,
			                                               "step",
			                                               G_CALLBACK (cpg_monitor_update),
			                                               monitor);

		monitor->signals[BEGIN] = g_signal_connect_swapped (integrator,
		                                                    "begin",
		                                                    G_CALLBACK (cpg_monitor_begin),
		                                                    monitor);

		monitor->integrator = integrator;

		g_object_add_weak_pointer (G_OBJECT (integrator),
		                           (gpointer *)(&monitor->integrator));
	}
}

static void
network_reset_cb (CpgMonitor *monitor)
{
	cpg_monitor_free_data (monitor);
}

static void
integrator_changed_cb (CpgMonitor *monitor,
                       GParamSpec *spec,
                       CpgNetwork *network)
{
	connect_integrator (monitor);
}

/**
 * cpg_monitor_new:
 * @network: a #CpgNetwork
 * @object: a #CpgObject
 * @property_name: a property name
 *
 * Create a new monitor for monitoring property @property_name of object
 * @object in the network @network. The monitor will attach itself to the
 * life time of the network and object.
 *
 * Returns: a new #CpgMonitor
 *
 **/
CpgMonitor *
cpg_monitor_new (CpgNetwork   *network,
                 CpgObject    *object,
                 gchar const  *property_name)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (property_name != NULL, NULL);

	CpgProperty *property;

	property = cpg_object_get_property (object, property_name);
	g_return_val_if_fail (property != NULL, NULL);

	CpgMonitor *monitor = g_slice_new0 (CpgMonitor);

	cpg_ref_counted_init (monitor, (GDestroyNotify)cpg_monitor_free);

	monitor->network = network;
	monitor->object = object;
	monitor->property = g_object_ref (property);

	g_object_add_weak_pointer (G_OBJECT (object), (gpointer *)&(monitor->object));
	g_object_add_weak_pointer (G_OBJECT (network), (gpointer *)&(monitor->network));

	// initialize values list
	cpg_monitor_grow (monitor);
	connect_integrator (monitor);

	monitor->signals[RESET] = g_signal_connect_swapped (network,
	                                                    "reset",
	                                                    G_CALLBACK (network_reset_cb), monitor);

	monitor->signals[NOTIFY_INTEGRATOR] = g_signal_connect_swapped (network,
	                                                                "notify::integrator",
	                                                                G_CALLBACK (integrator_changed_cb),
	                                                                monitor);

	return monitor;
}

/**
 * cpg_monitor_get_data:
 * @monitor: a #CpgMonitor
 * @size: return value for number of values
 *
 * Returns the data as monitored during the simulation. See also
 * #cpg_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data
 *
 * Returns: internal array of monitored values. The pointer should
 * not be freed
 *
 **/
gdouble const *
cpg_monitor_get_data (CpgMonitor *monitor,
                      guint      *size)
{
	if (size)
		*size = 0;

	if (!monitor || !monitor->object || !monitor->property)
		return NULL;

	if (size)
	{
		*size = monitor->num_values;
	}

	return monitor->values;
}

/**
 * cpg_monitor_get_sites:
 * @monitor: a #CpgMonitor
 * @size: return value for number of values
 *
 * Returns the data sites as monitored during the simulation. See also
 * #cpg_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data
 *
 * Returns: internal array of monitored sites. The pointer should
 * not be freed
 *
 **/
gdouble const *
cpg_monitor_get_sites (CpgMonitor *monitor,
                       guint      *size)
{
	if (size)
		*size = 0;

	if (!monitor || !monitor->object || !monitor->property)
		return NULL;

	if (size)
		*size = monitor->num_values;

	return monitor->sites;
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
 * @sites: the data sites at which to resample the data
 * @size: the size of the data sites array
 * @ret: the return location for the resampled data
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
	if (!sites || size == 0 || !monitor || !monitor->object || !monitor->property)
	{
		memset (ret, 0, sizeof (double) * size);
		return FALSE;
	}

	gdouble const *data = monitor->values;
	guint i;

	gdouble const *monsites = monitor->sites;

	for (i = 0; i < size; ++i)
	{
		guint idx = bsearch_find (monsites, (gint)monitor->num_values, sites[i]);
		guint fidx = idx > 0 ? idx - 1 : 0;
		guint sidx = idx < monitor->num_values ? idx : monitor->num_values - 1;

		if (fidx >= monitor->num_values || sidx >= monitor->num_values)
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
 * cpg_monitor_get_object:
 * @monitor: a #CpgMonitor
 *
 * Returns the object from which a property is being monitored.
 *
 * Returns: a pointer to the #CpgObject
 *
 **/
CpgObject *
cpg_monitor_get_object (CpgMonitor *monitor)
{
	if (!monitor)
	{
		return NULL;
	}
	else
	{
		return monitor->object;
	}
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
	if (!monitor)
	{
		return NULL;
	}
	else
	{
		return monitor->property;
	}
}
