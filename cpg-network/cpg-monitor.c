#include "cpg-monitor.h"
#include "cpg-property.h"
#include "cpg-utils.h"

#define MONITOR_GROW_SIZE 1000

enum
{
	RESET,
	UPDATE,
	NUM_SIGNALS
};

struct _CpgMonitor
{
	CpgRefCounted parent;
	
	CpgNetwork  *network;
	CpgObject   *object;
	CpgProperty *property;

	gdouble *values;
	gdouble *sites;
	guint num_values;
	guint size;
	
	guint signals[NUM_SIGNALS];
};

static void
cpg_monitor_free_data(CpgMonitor *monitor)
{
	g_free(monitor->values);
	g_free(monitor->sites);
	
	monitor->values = NULL;
	monitor->sites = NULL;
	monitor->size = 0;
	monitor->num_values = 0;
}

static void
cpg_monitor_free(CpgMonitor *monitor)
{
	if (monitor->object)
		g_object_unref(monitor->object);
	
	cpg_ref_counted_unref(monitor->property);
	
	if (monitor->network)
	{
		g_signal_handler_disconnect(monitor->network, monitor->signals[RESET]);
		g_signal_handler_disconnect(monitor->network, monitor->signals[UPDATE]);
	}

	cpg_monitor_free_data(monitor);	
	g_slice_free(CpgMonitor, monitor);
}

GType 
cpg_monitor_get_type()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY(type_id == 0))
		type_id = cpg_ref_counted_register_static("CpgMonitor");
	
	return type_id;
}

static void
cpg_monitor_grow(CpgMonitor *monitor)
{
	monitor->size += MONITOR_GROW_SIZE;
	
	array_resize(monitor->values, double, monitor->size);
	array_resize(monitor->sites, double, monitor->size);
}

static void
cpg_monitor_update(CpgMonitor *monitor, gdouble timestep, CpgNetwork *network)
{
	if (monitor->size == 0 || monitor->num_values >= monitor->size - 1)
		cpg_monitor_grow(monitor);
	
	monitor->values[monitor->num_values] = cpg_expression_evaluate(monitor->property->value);
	monitor->sites[monitor->num_values++] = network->time;
}

CpgMonitor *
cpg_monitor_new(CpgNetwork  *network,
				CpgObject   *object, 
				gchar const *property_name)
{
	CpgMonitor *monitor = g_slice_new0(CpgMonitor);

	cpg_ref_counted_init(monitor, (GDestroyFunc)cpg_monitor_free);
	
	monitor->network = network;
	monitor->object = object;
	monitor->property = cpg_ref_counted_ref(cpg_object_property(object, property_name));
	
	g_object_add_weak_pointer(object, &(monitor->object));
	g_object_add_weak_pointer(network, &(monitor->network));
	
	// initialize values list
	cpg_monitor_grow(monitor);
	
	monitor->signals[RESET] = g_signal_connect_swapped(network, "reset", G_CALLBACK(cpg_monitor_free_data), monitor);
	monitor->signals[UPDATE] = g_signal_connect_swapped(network, "update", G_CALLBACK(cpg_monitor_update), monitor);

	return monitor;
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
gdouble const *
cpg_monitor_get_data(CpgMonitor *monitor,
				     guint      *size)
{
	if (size)
		*size = 0;

	if (!monitor || !monitor->object || !monitor->property)
		return NULL;

	if (size)	
		*size = monitor->num_values;

	return monitor->values;
}

static int
bsearch_find(gdouble const *list, gint size, gdouble value)
{
	gint left = 0;
	gint right = size;
	
	while (right > left)
	{
		gint probe = (left + right) / 2;
		
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
gdouble	*
cpg_monitor_get_data_resampled(CpgMonitor    *monitor,
							   gdouble const *sites,
							   guint          size)
{
	if (!sites || size == 0 || !monitor || !monitor->object || !monitor->property)
		return NULL;

	gdouble const *data = monitor->values;
	gdouble *ret = cpg_new(gdouble, size);
	guint i;
	
	gdouble const *monsites = monitor->sites;
	
	for (i = 0; i < size; ++i)
	{
		guint idx = bsearch_find(monsites, (gint)monitor->num_values, sites[i]);
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
	
	return ret;
}
