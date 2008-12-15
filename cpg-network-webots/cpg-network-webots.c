#include <device/robot.h>
#include <device/servo.h>
#include <device/touch_sensor.h>

#include "cpg-network-webots-private.h"
#include "cpg-network/cpg-expression-private.h"
#include "cpg-network/cpg-types.h"
#include "cpg-network/cpg-utils.h"

#include <stdlib.h>

static CpgWebotsBinding *
webots_binding_new(CpgWebotsBindingType type, CpgWebotsBindingFunc func, char const *device, CpgProperty *property)
{
	CpgWebotsBinding *binding = (CpgWebotsBinding *)malloc(sizeof(CpgWebotsBinding));

	binding->type = type;
	binding->func = func;
	binding->device = robot_get_device(device);
	binding->property = property;
	
	return binding;
}

static void
webots_binding_free(CpgWebotsBinding *binding)
{
	free(binding);
}

static void
binding_handler_servo(CpgNetworkWebots *webots, CpgWebotsBinding *binding)
{
	if (binding->device)
		servo_set_position(binding->device, cpg_property_value(binding->property));
}

static void
binding_handler_touch_sensor(CpgNetworkWebots *webots, CpgWebotsBinding *binding)
{
	if (binding->device)
		cpg_property_set_value(binding->property, (double)touch_sensor_get_value(binding->device));
}

typedef enum
{
	ACCESS_TYPE_NONE = 0,
	ACCESS_TYPE_READ,
	ACCESS_TYPE_WRITE
} AccessType;

typedef struct
{
	char const *propname;
	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;
	AccessType access;
} BindingDefinition;

static BindingDefinition binding_definitions[] = 
{
	{"webots_servo", CPG_WEBOTS_BINDING_TYPE_SERVO, binding_handler_servo, ACCESS_TYPE_WRITE},
	{"webots_touch", CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR, binding_handler_touch_sensor, ACCESS_TYPE_READ}
};

static CpgProperty *
resolve_property(CpgProperty *property)
{
	CpgExpression *expr = property->value;
	
	if (expr->instructions == NULL ||
	    expr->instructions->next != NULL)
		return property;
	
	if (expr->instructions->type != CPG_INSTRUCTION_TYPE_PROPERTY)
		return property;
	
	return resolve_property(((CpgInstructionProperty *)expr->instructions)->property);
}

static void
resolve_bindings(CpgNetworkWebots *webots)
{
	unsigned num;
	unsigned i;
	CpgState * const *states = cpg_network_states(webots->network, &num);

	for (i = 0; i < num; ++i)
	{
		unsigned d;
		CpgObject *object = (CpgObject *)states[i];
		
		for (d = 0; d < sizeof(binding_definitions) / sizeof(BindingDefinition); ++d)
		{
			CpgProperty *property = cpg_object_property(object, binding_definitions[d].propname);
			CpgWebotsBinding *binding;
			char *id;
			
			if (!property)
				continue;
				
			if (binding_definitions[d].access == ACCESS_TYPE_READ)
				property = resolve_property(property);

			id = cpg_object_local_id(object);
			binding = webots_binding_new(binding_definitions[d].type, 
										 binding_definitions[d].func,
										 id,
										 property);
			cpg_free(id);

			webots->bindings = (CpgWebotsBinding **)realloc(webots->bindings, sizeof(CpgWebotsBinding *) * (webots->num_bindings + 1));
			webots->bindings[webots->num_bindings] = binding;
			++(webots->num_bindings);
		}
	}
}

CpgNetworkWebots *
cpg_network_webots_new(CpgNetwork *network)
{
	CpgNetworkWebots *webots = (CpgNetworkWebots *)malloc(sizeof(CpgNetworkWebots));
	
	webots->network = network;
	webots->num_bindings = 0;
	webots->bindings = NULL;
	
	resolve_bindings(webots);
	
	return webots;
}

void 
cpg_network_webots_free(CpgNetworkWebots *webots)
{
	unsigned i;
	for (i = 0; i < webots->num_bindings; ++i)
		webots_binding_free(webots->bindings[i]);
	
	free(webots->bindings);
	free(webots);
}

void
cpg_network_webots_enable(CpgNetworkWebots *webots, unsigned ms)
{
	unsigned i;
	for (i = 0; i < webots->num_bindings; ++i)
	{
		switch (webots->bindings[i]->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR:
				if (webots->bindings[i]->device)
					touch_sensor_enable(webots->bindings[i]->device, ms);
			break;
			default:
			break;
		}
	}
}

void
cpg_network_webots_disable(CpgNetworkWebots *webots)
{
	unsigned i;
	for (i = 0; i < webots->num_bindings; ++i)
	{
		switch (webots->bindings[i]->type)
		{
			case CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR:
				if (webots->bindings[i]->device)
					touch_sensor_disable(webots->bindings[i]->device);
			break;
			default:
			break;
		}
	}
}

void 
cpg_network_webots_update(CpgNetworkWebots *webots, float timestep)
{
	unsigned i;
	
	for (i = 0; i < webots->num_bindings; ++i)
		webots->bindings[i]->func(webots, webots->bindings[i]);
}
