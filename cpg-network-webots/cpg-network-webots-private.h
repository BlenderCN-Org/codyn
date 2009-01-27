#ifndef __CPG_NETWORK_WEBOTS_PRIVATE_H__
#define __CPG_NETWORK_WEBOTS_PRIVATE_H__

#include <device/robot_types.h>
#include "cpg-network-webots.h"

typedef struct _CpgWebotsBinding CpgWebotsBinding;

typedef void (*CpgWebotsBindingFunc)(CpgNetworkWebots *webots, CpgWebotsBinding *binding);

typedef enum
{
	CPG_WEBOTS_BINDING_TYPE_SERVO,
	CPG_WEBOTS_BINDING_TYPE_TOUCH_SENSOR
} CpgWebotsBindingType;

struct _CpgWebotsBinding
{
	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;
	DeviceTag device;
	CpgProperty *property;
	float initial;
	char *name;
};

struct _CpgNetworkWebots
{
	CpgNetwork *network;
	CpgWebotsBinding **bindings;
	unsigned num_bindings;
};

#endif /* __CPG_NETWORK_WEBOTS_PRIVATE_H__ */

