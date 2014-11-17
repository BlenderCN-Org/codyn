/*
 * cdn-monitor.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include "cdn-monitor.h"

#include "cdn-monitor.h"
#include "cdn-variable.h"
#include "cdn-utils.h"
#include "cdn-network.h"
#include "cdn-expression.h"

#include <string.h>
#include <math.h>

#define CDN_MONITOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_MONITOR, CdnMonitorPrivate))

#define MONITOR_GROW_SIZE 1000

enum
{
	RESETTED,
	NOTIFY_INTEGRATOR,
	STEP,
	BEGIN,
	NUM_SIGNALS
};

struct _CdnMonitorPrivate
{
	CdnNetwork  *network;
	CdnVariable *property;
	CdnIntegrator *integrator;

	gdouble *values;
	guint num_values;
	guint size_values;

	gdouble *sites;
	guint num_sites;
	guint size_sites;

	guint signals[NUM_SIGNALS];
};

G_DEFINE_TYPE (CdnMonitor, cdn_monitor, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_PROPERTY
};

static void
reset_monitor (CdnMonitor *monitor)
{
	g_free (monitor->priv->values);
	g_free (monitor->priv->sites);

	monitor->priv->values = NULL;
	monitor->priv->num_values = 0;
	monitor->priv->size_values = 0;

	monitor->priv->sites = NULL;
	monitor->priv->num_sites = 0;;
	monitor->priv->size_sites = 0;

}

static void
disconnect_integrator (CdnMonitor *monitor)
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
disconnect_network (CdnMonitor *monitor)
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
cdn_monitor_grow (CdnMonitor *monitor)
{
	CdnDimension dim;

	cdn_variable_get_dimension (monitor->priv->property, &dim);

	monitor->priv->size_values += MONITOR_GROW_SIZE * cdn_dimension_size (&dim);
	monitor->priv->size_sites += MONITOR_GROW_SIZE;

	array_resize (monitor->priv->values, gdouble, monitor->priv->size_values);
	array_resize (monitor->priv->sites, gdouble, monitor->priv->size_sites);
}

static void
cdn_monitor_update (CdnMonitor    *monitor,
                    gdouble        time,
                    gdouble        timestep,
                    CdnIntegrator *integrator)
{
	CdnMatrix const *vals;
	gdouble const *v;
	gint i;

	if (monitor->priv->size_sites == 0 ||
	    monitor->priv->num_sites >= monitor->priv->size_sites - 1)
	{
		cdn_monitor_grow (monitor);
	}

	vals = cdn_variable_get_values (monitor->priv->property);
	v = cdn_matrix_get (vals);

	for (i = 0; i < cdn_matrix_size (vals); ++i)
	{
		monitor->priv->values[monitor->priv->num_values++] = v[i];
	}

	monitor->priv->sites[monitor->priv->num_sites++] = time;
}

static void
cdn_monitor_begin (CdnMonitor    *monitor,
                   gdouble        from,
                   gdouble        step,
                   gdouble        to,
                   CdnIntegrator *integrator)
{
	/* Record first value */
	reset_monitor (monitor);

	cdn_monitor_update (monitor, from, step, integrator);
}

static void
connect_integrator (CdnMonitor *monitor)
{
	disconnect_integrator (monitor);

	CdnIntegrator *integrator = cdn_network_get_integrator (monitor->priv->network);

	if (integrator)
	{
		monitor->priv->signals[STEP] =
			g_signal_connect_swapped (integrator,
			                          "step",
			                          G_CALLBACK (cdn_monitor_update),
			                          monitor);

		monitor->priv->signals[BEGIN] =
			g_signal_connect_data (integrator,
			                        "begin",
			                        G_CALLBACK (cdn_monitor_begin),
			                        monitor,
			                        NULL,
			                        G_CONNECT_AFTER | G_CONNECT_SWAPPED);
	}

	monitor->priv->integrator = integrator;

	g_object_add_weak_pointer (G_OBJECT (monitor->priv->integrator),
	                           (gpointer *)&monitor->priv->integrator);
}

static void
cdn_monitor_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_monitor_parent_class)->finalize (object);
}

static void
cdn_monitor_dispose (GObject *object)
{
	CdnMonitor *monitor = CDN_MONITOR (object);

	disconnect_integrator (monitor);
	disconnect_network (monitor);
	reset_monitor (monitor);

	if (monitor->priv->property)
	{
		g_object_unref (monitor->priv->property);
		monitor->priv->property = NULL;
	}

	G_OBJECT_CLASS (cdn_monitor_parent_class)->dispose (object);
}

static void
monitor_get_variable (GObject    *object,
                      guint       prop_id,
                      GValue     *value,
                      GParamSpec *pspec)
{
	CdnMonitor *self = CDN_MONITOR (object);

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
on_network_resetted (CdnMonitor *monitor)
{
	reset_monitor (monitor);
}

static void
on_integrator_changed (CdnMonitor *monitor,
                       GParamSpec *spec,
                       CdnNetwork *network)
{
	connect_integrator (monitor);
}

static void
set_network (CdnMonitor *monitor,
             CdnNetwork *network)
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
	CdnMonitor *self = CDN_MONITOR (object);

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
cdn_monitor_class_init (CdnMonitorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_monitor_finalize;
	object_class->dispose = cdn_monitor_dispose;

	object_class->get_property = monitor_get_variable;
	object_class->set_property = monitor_set_property;

	g_type_class_add_private (object_class, sizeof(CdnMonitorPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CDN_TYPE_NETWORK,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));


	g_object_class_install_property (object_class,
	                                 PROP_PROPERTY,
	                                 g_param_spec_object ("property",
	                                                      "Property",
	                                                      "Property",
	                                                      CDN_TYPE_VARIABLE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_monitor_init (CdnMonitor *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_monitor_get_instance_private (self);
	self->priv = CDN_MONITOR_GET_PRIVATE (self);
}

/**
 * cdn_monitor_new:
 * @network: (type CdnNetwork): a #CdnNetwork
 * @property: a #CdnVariable
 *
 * Create a new monitor for monitoring property @property.
 *
 * Returns: a new #CdnMonitor
 *
 **/
CdnMonitor *
cdn_monitor_new (CdnNetwork  *network,
                 CdnVariable *property)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);
	g_return_val_if_fail (CDN_IS_VARIABLE (property), NULL);

	return g_object_new (CDN_TYPE_MONITOR,
	                     "network", network,
	                     "property", property,
	                     NULL);
}

/**
 * cdn_monitor_get_data:
 * @monitor: a #CdnMonitor
 * @size: (out caller-allocates): return value for number of values
 *
 * Returns the data as monitored during the simulation. See also
 * #cdn_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data. Note that the data returned is N-x-M values where
 * N is the number of sampled data points and M the size of the variable.
 *
 * Returns: (array length=size): internal array of monitored values. The pointer should
 * not be freed
 *
 **/
gdouble const *
cdn_monitor_get_data (CdnMonitor *monitor,
                      guint      *size)
{
	g_return_val_if_fail (CDN_IS_MONITOR (monitor), NULL);

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
 * cdn_monitor_get_sites:
 * @monitor: a #CdnMonitor
 * @size: (out caller-allocates): return value for number of sites
 *
 * Returns the data sites as monitored during the simulation. See also
 * #cdn_monitor_get_data_resampled for retrieving a resampled version
 * of the monitor data. Note that the size of the sites is always equal to the
 * number of sampled data points, but not necessarily equal to the number of
 * values returned by #cdn_monitor_get_data (since values might be vectors/matrices).
 *
 * Returns: (array length=size): internal array of monitored sites. The pointer should
 * not be freed
 *
 **/
gdouble const *
cdn_monitor_get_sites (CdnMonitor *monitor,
                       guint      *size)
{
	g_return_val_if_fail (CDN_IS_MONITOR (monitor), NULL);

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
		*size = monitor->priv->num_sites;
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
 * cdn_monitor_get_data_resampled:
 * @monitor: a #CdnMonitor
 * @sites: (array length=size): the data sites at which to resample the data
 * @size: the size of the data sites array
 * @ret: (out caller-allocates): the return location for the resampled data
 *
 * Returns the data as monitored during the simulation, but resampled at
 * specific data sites. @ret will have to be already allocated and large
 * enough to hold @size values of the dimension of the monitored variable.
 *
 * Returns: %TRUE if @ret was successfully filled with data, %FALSE otherwise
 *
 **/
gboolean
cdn_monitor_get_data_resampled (CdnMonitor     *monitor,
                                gdouble const  *sites,
                                guint           size,
                                gdouble        *ret)
{
	gint stride;
	gdouble const *data;
	guint i;
	gdouble const *monsites;
	CdnDimension dim;

	g_return_val_if_fail (CDN_IS_MONITOR (monitor), FALSE);

	if (!sites || size == 0 || !monitor || !monitor->priv->property)
	{
		memset (ret, 0, sizeof (double) * size);
		return FALSE;
	}

	data = monitor->priv->values;
	monsites = monitor->priv->sites;

	cdn_variable_get_dimension (monitor->priv->property, &dim);
	stride = cdn_dimension_size (&dim);

	for (i = 0; i < size; ++i)
	{
		guint idx = bsearch_find (monsites,
		                          (gint)monitor->priv->num_sites,
		                          sites[i]);

		guint fidx = idx > 0 ? idx - 1 : 0;
		guint sidx = idx < monitor->priv->num_sites ? idx : monitor->priv->num_sites - 1;

		if (fidx >= monitor->priv->num_sites ||
		    sidx >= monitor->priv->num_sites)
		{
			memcpy (ret + i * stride, data + (monitor->priv->num_sites - 1) * stride, sizeof (gdouble) * stride);
		}
		else
		{
			// interpolate between the values
			gdouble factor;
			gint j;

			if (fabs(monsites[sidx] - monsites[fidx]) < 0.00000001)
			{
				factor = 1;
			}
			else
			{
				factor = (monsites[sidx] - sites[i]) / (monsites[sidx] - monsites[fidx]);
			}

			for (j = 0; j < stride; ++j)
			{
				ret[i * stride + j] = data[fidx * stride + j] * factor + (data[sidx * stride + j] * (1 - factor));
			}
		}
	}

	return TRUE;
}

/**
 * cdn_monitor_get_variable:
 * @monitor: a #CdnMonitor
 *
 * Returns the property which is being monitored.
 *
 * Returns: (transfer none): a pointer to the #CdnVariable
 *
 **/
CdnVariable *
cdn_monitor_get_variable (CdnMonitor *monitor)
{
	g_return_val_if_fail (CDN_IS_MONITOR (monitor), NULL);
	return monitor->priv->property;
}
