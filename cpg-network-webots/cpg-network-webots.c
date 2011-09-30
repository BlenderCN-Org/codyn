/*
 * cpg-network-webots.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include <webots/robot.h>
#include <webots/servo.h>
#include <webots/touch_sensor.h>
#include <webots/light_sensor.h>
#include <webots/compass.h>
#include <webots/accelerometer.h>
#include <webots/gps.h>
#include <webots/gyro.h>

#include <cpg-network/cpg-expression.h>
#include <cpg-network/instructions/cpg-instruction-number.h>

#include <stdlib.h>
#include <string.h>

#include "cpg-network-webots.h"

#define CPG_NETWORK_WEBOTS_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_NETWORK_WEBOTS, CpgNetworkWebotsPrivate))

typedef struct _CpgWebotsBinding CpgWebotsBinding;

typedef void (*CpgWebotsBindingFunc)(CpgNetworkWebots *webots, CpgWebotsBinding *binding);

typedef enum
{
	CPG_WEBOTS_BINDING_TYPE_SERVO,
	CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR,
	CPG_WEBOTS_BINDING_TYPE_LIGHT_SENSOR,
	CPG_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
	CPG_WEBOTS_BINDING_TYPE_GPS_SENSOR,
	CPG_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
	CPG_WEBOTS_BINDING_TYPE_COMPASS_SENSOR
} CpgWebotsBindingType;

typedef enum
{
	ACCESS_TYPE_NONE = 0,
	ACCESS_TYPE_READ,
	ACCESS_TYPE_WRITE
} AccessType;

struct _CpgWebotsBinding
{
	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;

	gpointer data;

	WbDeviceTag device;
	CpgProperty *property;

	gchar *name;
	AccessType access;
};

struct _CpgNetworkWebotsPrivate
{
	CpgNetwork *network;
	CpgIntegrator *integrator;

	guint basic_time_step;

	GSList *inputs;
	GSList *outputs;

	guint resolved : 1;
};

G_DEFINE_TYPE (CpgNetworkWebots, cpg_network_webots, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_BASIC_TIME_STEP
};

typedef void (*CpgWebotsEnableFunc) (WbDeviceTag device, int ms);

typedef struct
{
	gchar const *template;
	gchar const *propname;

	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;
	CpgWebotsEnableFunc enable;

	gpointer data;
} BindingDefinition;

static void
binding_handler_servo_position (CpgNetworkWebots *webots,
                                CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		if (binding->access == ACCESS_TYPE_WRITE)
		{
			wb_servo_set_position (binding->device,
			                       cpg_property_get_value (binding->property));
		}
		else
		{
			cpg_property_set_value (binding->property,
			                        wb_servo_get_position (binding->device));
		}
	}
}

static void
binding_handler_servo_force (CpgNetworkWebots *webots,
                             CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		if (binding->access == ACCESS_TYPE_WRITE)
		{
			wb_servo_set_force (binding->device,
			                    cpg_property_get_value (binding->property));
		}
		else
		{
			cpg_property_set_value (binding->property,
			                        wb_servo_get_motor_force_feedback (binding->device));
		}
	}
}

static void
binding_handler_touch_sensor (CpgNetworkWebots *webots,
                              CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		cpg_property_set_value (binding->property,
		                        (gdouble)wb_touch_sensor_get_value (binding->device));
	}
}

static void
binding_handler_light_sensor (CpgNetworkWebots *webots,
                              CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		cpg_property_set_value (binding->property,
		                        (gdouble)wb_light_sensor_get_value (binding->device));
	}
}

static void
binding_handler_gyro_sensor (CpgNetworkWebots *webots,
                             CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_gyro_get_values (binding->device);
		cpg_property_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_gps_sensor (CpgNetworkWebots *webots,
                            CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_gps_get_values (binding->device);
		cpg_property_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_accelerometer_sensor (CpgNetworkWebots *webots,
                                      CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_accelerometer_get_values (binding->device);
		cpg_property_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_compass_sensor (CpgNetworkWebots *webots,
                                CpgWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_compass_get_values (binding->device);
		cpg_property_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static BindingDefinition binding_definitions[] =
{
	{
		"servo",
		"position",
		CPG_WEBOTS_BINDING_TYPE_SERVO,
		binding_handler_servo_position,
		wb_servo_enable_position,
		NULL
	},
	{
		"servo",
		"force",
		CPG_WEBOTS_BINDING_TYPE_SERVO,
		binding_handler_servo_force,
		wb_servo_enable_motor_force_feedback,
		NULL
	},
	{
		"touch",
		"value",
		CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR,
		binding_handler_touch_sensor,
		wb_touch_sensor_enable,
		NULL
	},
	{
		"light",
		"value",
		CPG_WEBOTS_BINDING_TYPE_LIGHT_SENSOR,
		binding_handler_light_sensor,
		wb_light_sensor_enable,
		NULL
	},
	{
		"gyro",
		"x",
		CPG_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(0)
	},
	{
		"gyro",
		"y",
		CPG_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(1)
	},
	{
		"gyro",
		"z",
		CPG_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(2)
	},
	{
		"gps",
		"x",
		CPG_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(0)
	},
	{
		"gps",
		"y",
		CPG_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(1)
	},
	{
		"gps",
		"z",
		CPG_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(2)
	},
	{
		"accelerometer",
		"x",
		CPG_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(0)
	},
	{
		"accelerometer",
		"y",
		CPG_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(1)
	},
	{
		"accelerometer",
		"z",
		CPG_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(2)
	},
	{
		"compass",
		"x",
		CPG_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(0)
	},
	{
		"compass",
		"y",
		CPG_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(1)
	},
	{
		"compass",
		"z",
		CPG_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(2)
	},
};

static CpgWebotsBinding *
webots_binding_new (CpgWebotsBindingType  type,
                    CpgWebotsBindingFunc  func,
                    gpointer              data,
                    gchar const          *device,
                    CpgProperty          *property,
                    AccessType            access)
{
	CpgWebotsBinding *binding;

	binding = g_slice_new (CpgWebotsBinding);

	binding->type = type;
	binding->func = func;
	binding->device = wb_robot_get_device (device);
	binding->property = property;
	binding->name = g_strdup (device);
	binding->data = data;
	binding->access = access;

	return binding;
}

static void
webots_binding_free (CpgWebotsBinding *binding)
{
	g_free (binding->name);
	g_slice_free (CpgWebotsBinding, binding);
}

static void
bindings_free (CpgNetworkWebots *webots)
{
	g_slist_foreach (webots->priv->inputs, (GFunc)webots_binding_free, NULL);
	g_slist_free (webots->priv->inputs);
	webots->priv->inputs = NULL;

	g_slist_foreach (webots->priv->outputs, (GFunc)webots_binding_free, NULL);
	g_slist_free (webots->priv->outputs);
	webots->priv->outputs = NULL;

	webots->priv->resolved = FALSE;
}

static void
cpg_network_webots_finalize (GObject *object)
{
	CpgNetworkWebots *self;

	self = CPG_NETWORK_WEBOTS (object);

	G_OBJECT_CLASS (cpg_network_webots_parent_class)->finalize (object);
}

static AccessType
property_access_type (CpgProperty *property)
{
	CpgPropertyFlags flags;

	flags = cpg_property_get_flags (property);

	if (flags & CPG_PROPERTY_FLAG_OUT)
	{
		return ACCESS_TYPE_WRITE;
	}

	return ACCESS_TYPE_READ;
}

static gchar const *
webots_template_name (CpgObject *obj)
{
	CpgObject *parent;

	if (!obj)
	{
		return NULL;
	}

	parent = cpg_object_get_parent (obj);

	if (!parent)
	{
		return NULL;
	}

	if (g_strcmp0 (cpg_object_get_id (parent), "webots") != 0)
	{
		return NULL;
	}

	return cpg_object_get_id (obj);
}

static gboolean
needs_initial_value (CpgProperty *prop)
{
	GSList const *instructions;

	if (!(cpg_property_get_flags (prop) & CPG_PROPERTY_FLAG_IN))
	{
		return FALSE;
	}

	instructions = cpg_expression_get_instructions (cpg_property_get_expression (prop));

	if (!instructions || instructions->next || !CPG_IS_INSTRUCTION_NUMBER (instructions->data))
	{
		return FALSE;
	}

	return TRUE;
}

static void
binding_for (CpgNetworkWebots *webots,
             CpgObject        *object,
             gchar const      *tname)
{
	guint d;

	for (d = 0; d < sizeof (binding_definitions) / sizeof (BindingDefinition); ++d)
	{
		BindingDefinition *def;
		CpgProperty *prop;
		CpgWebotsBinding *binding;

		def = &(binding_definitions[d]);

		if (g_strcmp0 (tname, def->template) != 0)
		{
			continue;
		}

		prop = cpg_object_get_property (object, def->propname);

		if (prop == NULL)
		{
			continue;
		}

		binding = webots_binding_new (def->type,
		                              def->func,
		                              def->data,
		                              cpg_object_get_id (object),
		                              prop,
		                              property_access_type (prop));

		if (binding->access == ACCESS_TYPE_READ)
		{
			def->enable (binding->device, webots->priv->basic_time_step);

			webots->priv->inputs =
				g_slist_prepend (webots->priv->inputs,
				                 binding);
		}
		else
		{
			if (needs_initial_value (prop))
			{
				/* Initial value... */
				binding->access = ACCESS_TYPE_READ;
				def->func (webots, binding);
				binding->access = ACCESS_TYPE_WRITE;
			}

			webots->priv->outputs =
				g_slist_prepend (webots->priv->outputs,
				                 binding);
		}
	}
}

static void
resolve_bindings (CpgNetworkWebots *webots,
                  CpgGroup         *group)
{
	GSList const *children;

	children = cpg_group_get_children (group ? group : CPG_GROUP (webots->priv->network));

	while (children)
	{
		CpgObject *object;
		GSList const *templates;

		object = children->data;
		templates = cpg_object_get_applied_templates (object);

		while (templates)
		{
			gchar const *name;

			name = webots_template_name (templates->data);

			binding_for (webots, object, name);

			templates = g_slist_next (templates);
		}

		if (CPG_IS_GROUP (object))
		{
			resolve_bindings (webots, CPG_GROUP (object));
		}

		children = g_slist_next (children);
	}

	if (group == NULL)
	{
		webots->priv->inputs = g_slist_reverse (webots->priv->inputs);
		webots->priv->outputs = g_slist_reverse (webots->priv->outputs);
	}
}

static void
on_network_tainted (CpgNetworkWebots *webots)
{
	bindings_free (webots);
}

static void
on_network_compiled (CpgNetworkWebots *webots)
{
	if (!webots->priv->resolved)
	{
		resolve_bindings (webots, NULL);
	}
}

static void
on_integrator_step (CpgNetworkWebots *webots)
{
	cpg_network_webots_write_outputs (webots);
}

static void
on_integrator_step_prepare (CpgNetworkWebots *webots)
{
	cpg_network_webots_read_inputs (webots);
}

static void
unset_integrator (CpgNetworkWebots *webots)
{
	if (webots->priv->integrator)
	{
		g_signal_handlers_disconnect_by_func (webots->priv->integrator,
		                                      on_integrator_step_prepare,
		                                      webots);

		g_signal_handlers_disconnect_by_func (webots->priv->integrator,
		                                      on_integrator_step,
		                                      webots);

		g_object_unref (webots->priv->integrator);
		webots->priv->integrator = NULL;
	}
}

static void
on_integrator_changed (CpgNetworkWebots *webots)
{
	unset_integrator (webots);

	webots->priv->integrator = cpg_network_get_integrator (webots->priv->network);

	if (webots->priv->integrator)
	{
		g_object_ref (webots->priv->integrator);

		g_signal_connect_swapped (webots->priv->integrator,
		                          "step",
		                          G_CALLBACK (on_integrator_step),
		                          webots);

		g_signal_connect_swapped (webots->priv->integrator,
		                          "step-prepare",
		                          G_CALLBACK (on_integrator_step_prepare),
		                          webots);
	}
}

static void
unset_network (CpgNetworkWebots *webots)
{
	if (webots->priv->network)
	{
		g_signal_handlers_disconnect_by_func (webots->priv->network,
		                                      on_network_tainted,
		                                      webots);

		g_signal_handlers_disconnect_by_func (webots->priv->network,
		                                      on_network_compiled,
		                                      webots);

		g_signal_handlers_disconnect_by_func (webots->priv->network,
		                                      on_integrator_changed,
		                                      webots);

		g_object_unref (webots->priv->network);
		webots->priv->network = NULL;

		bindings_free (webots);

		unset_integrator (webots);
	}
}

static void
cpg_network_webots_dispose (GObject *object)
{
	CpgNetworkWebots *self;

	self = CPG_NETWORK_WEBOTS (object);

	unset_network (self);

	G_OBJECT_CLASS (cpg_network_webots_parent_class)->dispose (object);
}

static void
set_network (CpgNetworkWebots *webots,
             CpgNetwork       *network)
{
	unset_network (webots);

	if (network)
	{
		webots->priv->network = g_object_ref (network);

		g_signal_connect_swapped (webots->priv->network,
		                          "tainted",
		                          G_CALLBACK (on_network_tainted),
		                          webots);

		g_signal_connect_swapped (webots->priv->network,
		                          "compiled",
		                          G_CALLBACK (on_network_compiled),
		                          webots);

		g_signal_connect_swapped (webots->priv->network,
		                          "notify::integrator",
		                          G_CALLBACK (on_integrator_changed),
		                          webots);

		if (cpg_object_is_compiled (CPG_OBJECT (webots->priv->network)))
		{
			resolve_bindings (webots, NULL);
		}

		on_integrator_changed (webots);
	}
}

static void
cpg_network_webots_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgNetworkWebots *self = CPG_NETWORK_WEBOTS (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			set_network (self, g_value_get_object (value));
			break;
		case PROP_BASIC_TIME_STEP:
			self->priv->basic_time_step = g_value_get_uint (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_webots_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgNetworkWebots *self = CPG_NETWORK_WEBOTS (object);

	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
			break;
		case PROP_BASIC_TIME_STEP:
			g_value_set_uint (value, self->priv->basic_time_step);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_webots_class_init (CpgNetworkWebotsClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_network_webots_finalize;
	object_class->dispose = cpg_network_webots_dispose;

	object_class->get_property = cpg_network_webots_get_property;
	object_class->set_property = cpg_network_webots_set_property;

	g_type_class_add_private (object_class, sizeof(CpgNetworkWebotsPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_BASIC_TIME_STEP,
	                                 g_param_spec_uint ("basic-time-step",
	                                                    "Basic Time Step",
	                                                    "Basic time step",
	                                                    0,
	                                                    G_MAXUINT,
	                                                    0,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_network_webots_init (CpgNetworkWebots *self)
{
	self->priv = CPG_NETWORK_WEBOTS_GET_PRIVATE (self);
}

CpgNetworkWebots *
cpg_network_webots_new (CpgNetwork *network,
                        guint       basic_time_step)
{
	CpgNetworkWebots *ret;

	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	ret = g_object_new (CPG_TYPE_NETWORK_WEBOTS,
	                    "network", network,
	                    "basic-time-step", basic_time_step);

	return ret;
}

void
cpg_network_webots_read_inputs (CpgNetworkWebots *webots)
{
	GSList *item;

	g_return_if_fail (CPG_IS_NETWORK_WEBOTS (webots));

	for (item = webots->priv->inputs; item; item = g_slist_next (item))
	{
		CpgWebotsBinding *binding;

		binding = item->data;
		binding->func (webots, binding);
	}
}

void
cpg_network_webots_write_outputs (CpgNetworkWebots *webots)
{
	GSList *item;

	g_return_if_fail (CPG_IS_NETWORK_WEBOTS (webots));

	for (item = webots->priv->outputs; item; item = g_slist_next (item))
	{
		CpgWebotsBinding *binding;

		binding = item->data;
		binding->func (webots, binding);
	}
}
