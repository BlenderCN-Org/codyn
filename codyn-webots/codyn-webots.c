/*
 * codyn-webots.c
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

#include <webots/robot.h>
#include <webots/servo.h>
#include <webots/touch_sensor.h>
#include <webots/light_sensor.h>
#include <webots/compass.h>
#include <webots/accelerometer.h>
#include <webots/gps.h>
#include <webots/gyro.h>

#include <codyn/cdn-expression.h>
#include <codyn/instructions/cdn-instruction-number.h>

#include <stdlib.h>
#include <string.h>

#include "codyn-webots.h"

#define CDN_NETWORK_WEBOTS_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_NETWORK_WEBOTS, CdnNetworkWebotsPrivate))

typedef struct _CdnWebotsBinding CdnWebotsBinding;

typedef void (*CdnWebotsBindingFunc)(CdnNetworkWebots *webots, CdnWebotsBinding *binding);

typedef enum
{
	CDN_WEBOTS_BINDING_TYPE_SERVO,
	CDN_WEBOTS_BINDING_TYPE_TOUCH_SENSOR,
	CDN_WEBOTS_BINDING_TYPE_LIGHT_SENSOR,
	CDN_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
	CDN_WEBOTS_BINDING_TYPE_GPS_SENSOR,
	CDN_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
	CDN_WEBOTS_BINDING_TYPE_COMPASS_SENSOR
} CdnWebotsBindingType;

typedef enum
{
	ACCESS_TYPE_NONE = 0,
	ACCESS_TYPE_READ,
	ACCESS_TYPE_WRITE
} AccessType;

struct _CdnWebotsBinding
{
	CdnWebotsBindingType type;
	CdnWebotsBindingFunc func;

	gpointer data;

	WbDeviceTag device;
	CdnVariable *property;

	gchar *name;
	AccessType access;
};

struct _CdnNetworkWebotsPrivate
{
	CdnNetwork *network;
	CdnIntegrator *integrator;

	guint basic_time_step;

	GSList *inputs;
	GSList *outputs;

	guint resolved : 1;
};

G_DEFINE_TYPE (CdnNetworkWebots, cdn_network_webots, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NETWORK,
	PROP_BASIC_TIME_STEP
};

typedef void (*CdnWebotsEnableFunc) (WbDeviceTag device, int ms);

typedef struct
{
	gchar const *template;
	gchar const *propname;

	CdnWebotsBindingType type;
	CdnWebotsBindingFunc func;
	CdnWebotsEnableFunc enable;

	gpointer data;
} BindingDefinition;

static void
binding_handler_servo_position (CdnNetworkWebots *webots,
                                CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		if (binding->access == ACCESS_TYPE_WRITE)
		{
			wb_servo_set_position (binding->device,
			                       cdn_variable_get_value (binding->property));
		}
		else
		{
			cdn_variable_set_value (binding->property,
			                        wb_servo_get_position (binding->device));
		}
	}
}

static void
binding_handler_servo_force (CdnNetworkWebots *webots,
                             CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		if (binding->access == ACCESS_TYPE_WRITE)
		{
			wb_servo_set_force (binding->device,
			                    cdn_variable_get_value (binding->property));
		}
		else
		{
			cdn_variable_set_value (binding->property,
			                        wb_servo_get_motor_force_feedback (binding->device));
		}
	}
}

static void
binding_handler_touch_sensor (CdnNetworkWebots *webots,
                              CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		cdn_variable_set_value (binding->property,
		                        (gdouble)wb_touch_sensor_get_value (binding->device));
	}
}

static void
binding_handler_light_sensor (CdnNetworkWebots *webots,
                              CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		cdn_variable_set_value (binding->property,
		                        (gdouble)wb_light_sensor_get_value (binding->device));
	}
}

static void
binding_handler_gyro_sensor (CdnNetworkWebots *webots,
                             CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_gyro_get_values (binding->device);
		cdn_variable_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_gps_sensor (CdnNetworkWebots *webots,
                            CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_gps_get_values (binding->device);
		cdn_variable_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_accelerometer_sensor (CdnNetworkWebots *webots,
                                      CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_accelerometer_get_values (binding->device);
		cdn_variable_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static void
binding_handler_compass_sensor (CdnNetworkWebots *webots,
                                CdnWebotsBinding *binding)
{
	if (binding->device)
	{
		gdouble const *values;

		values = wb_compass_get_values (binding->device);
		cdn_variable_set_value (binding->property,
		                        values[GPOINTER_TO_INT (binding->data)]);
	}
}

static BindingDefinition binding_definitions[] =
{
	{
		"servo",
		"position",
		CDN_WEBOTS_BINDING_TYPE_SERVO,
		binding_handler_servo_position,
		wb_servo_enable_position,
		NULL
	},
	{
		"servo",
		"force",
		CDN_WEBOTS_BINDING_TYPE_SERVO,
		binding_handler_servo_force,
		wb_servo_enable_motor_force_feedback,
		NULL
	},
	{
		"touch",
		"value",
		CDN_WEBOTS_BINDING_TYPE_TOUCH_SENSOR,
		binding_handler_touch_sensor,
		wb_touch_sensor_enable,
		NULL
	},
	{
		"light",
		"value",
		CDN_WEBOTS_BINDING_TYPE_LIGHT_SENSOR,
		binding_handler_light_sensor,
		wb_light_sensor_enable,
		NULL
	},
	{
		"gyro",
		"x",
		CDN_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(0)
	},
	{
		"gyro",
		"y",
		CDN_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(1)
	},
	{
		"gyro",
		"z",
		CDN_WEBOTS_BINDING_TYPE_GYRO_SENSOR,
		binding_handler_gyro_sensor,
		wb_gyro_enable,
		GINT_TO_POINTER(2)
	},
	{
		"gps",
		"x",
		CDN_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(0)
	},
	{
		"gps",
		"y",
		CDN_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(1)
	},
	{
		"gps",
		"z",
		CDN_WEBOTS_BINDING_TYPE_GPS_SENSOR,
		binding_handler_gps_sensor,
		wb_gps_enable,
		GINT_TO_POINTER(2)
	},
	{
		"accelerometer",
		"x",
		CDN_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(0)
	},
	{
		"accelerometer",
		"y",
		CDN_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(1)
	},
	{
		"accelerometer",
		"z",
		CDN_WEBOTS_BINDING_TYPE_ACCELEROMETER_SENSOR,
		binding_handler_accelerometer_sensor,
		wb_accelerometer_enable,
		GINT_TO_POINTER(2)
	},
	{
		"compass",
		"x",
		CDN_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(0)
	},
	{
		"compass",
		"y",
		CDN_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(1)
	},
	{
		"compass",
		"z",
		CDN_WEBOTS_BINDING_TYPE_COMPASS_SENSOR,
		binding_handler_compass_sensor,
		wb_compass_enable,
		GINT_TO_POINTER(2)
	},
};

static CdnWebotsBinding *
webots_binding_new (CdnWebotsBindingType  type,
                    CdnWebotsBindingFunc  func,
                    gpointer              data,
                    gchar const          *device,
                    CdnVariable          *property,
                    AccessType            access)
{
	CdnWebotsBinding *binding;

	binding = g_slice_new (CdnWebotsBinding);

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
webots_binding_free (CdnWebotsBinding *binding)
{
	g_free (binding->name);
	g_slice_free (CdnWebotsBinding, binding);
}

static void
bindings_free (CdnNetworkWebots *webots)
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
cdn_network_webots_finalize (GObject *object)
{
	CdnNetworkWebots *self;

	self = CDN_NETWORK_WEBOTS (object);

	G_OBJECT_CLASS (cdn_network_webots_parent_class)->finalize (object);
}

static AccessType
property_access_type (CdnVariable *property)
{
	CdnVariableFlags flags;

	flags = cdn_variable_get_flags (property);

	if (flags & CDN_VARIABLE_FLAG_OUT)
	{
		return ACCESS_TYPE_WRITE;
	}

	return ACCESS_TYPE_READ;
}

static gchar const *
webots_template_name (CdnObject *obj)
{
	CdnNode *parent;

	if (!obj)
	{
		return NULL;
	}

	parent = cdn_object_get_parent (obj);

	if (!parent)
	{
		return NULL;
	}

	if (g_strcmp0 (cdn_object_get_id (CDN_OBJECT (parent)), "webots") != 0)
	{
		return NULL;
	}

	return cdn_object_get_id (obj);
}

static gboolean
needs_initial_value (CdnVariable *prop)
{
	GSList const *instructions;

	if (!(cdn_variable_get_flags (prop) & CDN_VARIABLE_FLAG_IN))
	{
		return FALSE;
	}

	instructions = cdn_expression_get_instructions (cdn_variable_get_expression (prop));

	if (!instructions || instructions->next || !CDN_IS_INSTRUCTION_NUMBER (instructions->data))
	{
		return FALSE;
	}

	return TRUE;
}

static void
binding_for (CdnNetworkWebots *webots,
             CdnObject        *object,
             gchar const      *tname)
{
	guint d;

	for (d = 0; d < sizeof (binding_definitions) / sizeof (BindingDefinition); ++d)
	{
		BindingDefinition *def;
		CdnVariable *prop;
		CdnWebotsBinding *binding;

		def = &(binding_definitions[d]);

		if (g_strcmp0 (tname, def->template) != 0)
		{
			continue;
		}

		prop = cdn_object_get_variable (object, def->propname);

		if (prop == NULL)
		{
			continue;
		}

		binding = webots_binding_new (def->type,
		                              def->func,
		                              def->data,
		                              cdn_object_get_id (object),
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
resolve_bindings (CdnNetworkWebots *webots,
                  CdnNode         *group)
{
	GSList const *children;

	children = cdn_node_get_children (group ? group : CDN_NODE (webots->priv->network));

	while (children)
	{
		CdnObject *object;
		GSList const *templates;

		object = children->data;
		templates = cdn_object_get_applied_templates (object);

		while (templates)
		{
			gchar const *name;

			name = webots_template_name (templates->data);

			binding_for (webots, object, name);

			templates = g_slist_next (templates);
		}

		if (CDN_IS_NODE (object))
		{
			resolve_bindings (webots, CDN_NODE (object));
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
on_network_tainted (CdnNetworkWebots *webots)
{
	bindings_free (webots);
}

static void
on_network_compiled (CdnNetworkWebots *webots)
{
	if (!webots->priv->resolved)
	{
		resolve_bindings (webots, NULL);
	}
}

static void
on_integrator_step (CdnNetworkWebots *webots)
{
	cdn_network_webots_write_outputs (webots);
}

static void
on_integrator_step_prepare (CdnNetworkWebots *webots)
{
	cdn_network_webots_read_inputs (webots);
}

static void
unset_integrator (CdnNetworkWebots *webots)
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
on_integrator_changed (CdnNetworkWebots *webots)
{
	unset_integrator (webots);

	webots->priv->integrator = cdn_network_get_integrator (webots->priv->network);

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
unset_network (CdnNetworkWebots *webots)
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
cdn_network_webots_dispose (GObject *object)
{
	CdnNetworkWebots *self;

	self = CDN_NETWORK_WEBOTS (object);

	unset_network (self);

	G_OBJECT_CLASS (cdn_network_webots_parent_class)->dispose (object);
}

static void
set_network (CdnNetworkWebots *webots,
             CdnNetwork       *network)
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

		if (cdn_object_is_compiled (CDN_OBJECT (webots->priv->network)))
		{
			resolve_bindings (webots, NULL);
		}

		on_integrator_changed (webots);
	}
}

static void
cdn_network_webots_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CdnNetworkWebots *self = CDN_NETWORK_WEBOTS (object);

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
cdn_network_webots_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CdnNetworkWebots *self = CDN_NETWORK_WEBOTS (object);

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
cdn_network_webots_class_init (CdnNetworkWebotsClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_network_webots_finalize;
	object_class->dispose = cdn_network_webots_dispose;

	object_class->get_property = cdn_network_webots_get_property;
	object_class->set_property = cdn_network_webots_set_property;

	g_type_class_add_private (object_class, sizeof(CdnNetworkWebotsPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      CDN_TYPE_OBJECT,
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
cdn_network_webots_init (CdnNetworkWebots *self)
{
	self->priv = CDN_NETWORK_WEBOTS_GET_PRIVATE (self);
}

CdnNetworkWebots *
cdn_network_webots_new (CdnNetwork *network,
                        guint       basic_time_step)
{
	CdnNetworkWebots *ret;

	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	ret = g_object_new (CDN_TYPE_NETWORK_WEBOTS,
	                    "network", network,
	                    "basic-time-step", basic_time_step);

	return ret;
}

void
cdn_network_webots_read_inputs (CdnNetworkWebots *webots)
{
	GSList *item;

	g_return_if_fail (CDN_IS_NETWORK_WEBOTS (webots));

	for (item = webots->priv->inputs; item; item = g_slist_next (item))
	{
		CdnWebotsBinding *binding;

		binding = item->data;
		binding->func (webots, binding);
	}
}

void
cdn_network_webots_write_outputs (CdnNetworkWebots *webots)
{
	GSList *item;

	g_return_if_fail (CDN_IS_NETWORK_WEBOTS (webots));

	for (item = webots->priv->outputs; item; item = g_slist_next (item))
	{
		CdnWebotsBinding *binding;

		binding = item->data;
		binding->func (webots, binding);
	}
}
