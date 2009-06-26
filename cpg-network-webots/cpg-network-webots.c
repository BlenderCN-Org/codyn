#include <webots/robot.h>
#include <webots/servo.h>
#include <webots/touch_sensor.h>

#include "cpg-network-webots-private.h"
#include "cpg-network/cpg-expression.h"
#include "cpg-network/cpg-types.h"

#include <stdlib.h>
#include <string.h>

static CpgWebotsBinding *
webots_binding_new(CpgWebotsBindingType type, CpgWebotsBindingFunc func, gchar const *device, CpgProperty *property)
{
	CpgWebotsBinding *binding = g_slice_new(CpgWebotsBinding);

	binding->type = type;
	binding->func = func;
	binding->device = wb_robot_get_device(device);
	binding->property = property;
	binding->initial = 0;
	binding->name = g_strdup(device);
	
	return binding;
}

static void
webots_binding_free(CpgWebotsBinding *binding)
{
	g_free(binding->name);
	g_slice_free(CpgWebotsBinding, binding);
}

static void
binding_handler_servo(CpgNetworkWebots *webots, CpgWebotsBinding *binding)
{
	if (binding->device)
		wb_servo_set_position(binding->device, cpg_property_get_value(binding->property));
}

static void
binding_handler_servo_force(CpgNetworkWebots *webots, CpgWebotsBinding *binding)
{
	if (binding->device)
		wb_servo_set_force(binding->device, cpg_property_get_value(binding->property));
}

static void
binding_handler_touch_sensor(CpgNetworkWebots *webots, CpgWebotsBinding *binding)
{
	if (binding->device)
		cpg_property_set_value(binding->property, (gdouble)wb_touch_sensor_get_value(binding->device));
}

typedef enum
{
	ACCESS_TYPE_NONE = 0,
	ACCESS_TYPE_READ,
	ACCESS_TYPE_WRITE
} AccessType;

typedef struct
{
	gchar const *propname;
	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;
	AccessType access;
} BindingDefinition;

static BindingDefinition binding_definitions[] = 
{
	{"wb_servo_pos", CPG_WEBOTS_BINDING_TYPE_SERVO, binding_handler_servo, ACCESS_TYPE_WRITE},
	{"wb_servo_force", CPG_WEBOTS_BINDING_TYPE_SERVO, binding_handler_servo_force, ACCESS_TYPE_WRITE},
	{"wb_sensor_touch", CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR, binding_handler_touch_sensor, ACCESS_TYPE_READ}
};

static CpgProperty *
resolve_property(CpgProperty *property)
{
	CpgExpression *expr = cpg_property_get_value_expression(property);
	GSList *instructions = cpg_expression_get_instructions(expr);

	if (instructions == NULL || instructions->next != NULL)
		return property;
	
	CpgInstruction *instruction = (CpgInstruction *)instructions->data;
	if (instruction->type != CPG_INSTRUCTION_TYPE_PROPERTY)
		return property;
	
	return resolve_property(((CpgInstructionProperty *)instruction)->property);
}

static void
resolve_bindings(CpgNetworkWebots *webots)
{
	GSList *item;

	for (item = cpg_network_get_states(webots->network); item; item = g_slist_next(item))
	{
		guint d;
		CpgObject *object = CPG_OBJECT(item->data);
		
		for (d = 0; d < sizeof(binding_definitions) / sizeof(BindingDefinition); ++d)
		{
			CpgProperty *property = cpg_object_get_property(object, binding_definitions[d].propname);
			CpgWebotsBinding *binding;
			gchar *id;
			
			if (!property)
				continue;
				
			if (binding_definitions[d].access == ACCESS_TYPE_READ)
				property = resolve_property(property);

			id = cpg_object_get_local_id(object);
			binding = webots_binding_new(binding_definitions[d].type, 
										 binding_definitions[d].func,
										 id,
										 property);
			g_free(id);

			webots->bindings = g_slist_append(webots->bindings, binding);
		}
	}
}

CpgNetworkWebots *
cpg_network_webots_new(CpgNetwork *network)
{
	CpgNetworkWebots *webots = g_slice_new0(CpgNetworkWebots);
	
	webots->network = network;
	resolve_bindings(webots);
	
	return webots;
}

void
cpg_network_webots_initial(CpgNetworkWebots *webots, guint ms)
{
	GSList *item;

	for (item = webots->bindings; item; item = g_slist_next(item))
	{
		CpgWebotsBinding *binding = (CpgWebotsBinding *)item->data;
		
		switch (binding->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_SERVO:
			{
				if (!binding->device)
					continue;

				wb_servo_enable_position(binding->device, ms);
				wb_robot_step(ms);
				
				binding->initial = wb_servo_get_position(binding->device);
				wb_servo_disable_position(binding->device);
			}
			default:
			break;
		}
	}
}

void 
cpg_network_webots_free(CpgNetworkWebots *webots)
{
	g_slist_foreach(webots->bindings, (GFunc)webots_binding_free, NULL);
	g_slist_free(webots->bindings);

	g_slice_free(CpgNetworkWebots, webots);
}

void
cpg_network_webots_enable(CpgNetworkWebots *webots, guint ms)
{
	GSList *item;
	
	for (item = webots->bindings; item; item = g_slist_next(item))
	{
		CpgWebotsBinding *binding = (CpgWebotsBinding *)item->data;
		
		switch (binding->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR:
				if (binding->device)
					wb_touch_sensor_enable(binding->device, ms);
			break;
			default:
			break;
		}
	}
}

void
cpg_network_webots_disable(CpgNetworkWebots *webots)
{
	GSList *item;
	
	for (item = webots->bindings; item; item = g_slist_next(item))
	{
		CpgWebotsBinding *binding = (CpgWebotsBinding *)item->data;
		
		switch (binding->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR:
				if (binding->device)
					wb_touch_sensor_disable(binding->device);
			break;
			default:
			break;
		}
	}
}

void 
cpg_network_webots_update(CpgNetworkWebots *webots)
{
	GSList *item;
	
	for (item = webots->bindings; item; item = g_slist_next(item))
	{
		CpgWebotsBinding *binding = (CpgWebotsBinding *)item->data;
		binding->func(webots, binding);
	}
}

guint
cpg_network_webots_size(CpgNetworkWebots *webots)
{
	return g_slist_length(webots->bindings);
}

void
cpg_network_webots_scale_initial(CpgNetworkWebots *webots, gdouble fraction)
{
	GSList *item;
	
	/* Scale from initial webots values, to initial value of network, according
	   to fraction */

	for (item = webots->bindings; item; item = g_slist_next(item))
	{
		CpgWebotsBinding *binding = (CpgWebotsBinding *)item->data;
		
		switch (binding->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_SERVO:
			{
				if (!binding->device)
					continue;
					
				gdouble p = fraction * (cpg_property_get_value(binding->property) - binding->initial);
				wb_servo_set_position(binding->device, binding->initial + p);
			}
			break;
			default:
			break;
		}
	}
}
