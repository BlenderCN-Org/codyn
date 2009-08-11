#ifndef __CPG_NETWORK_WEBOTS_PRIVATE_H__
#define __CPG_NETWORK_WEBOTS_PRIVATE_H__

#include <webots/types.h>
#include <glib.h>
#include "cpg-network-webots.h"

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

struct _CpgWebotsBinding
{
	CpgWebotsBindingType type;
	CpgWebotsBindingFunc func;
	gpointer data;
	WbDeviceTag device;
	CpgProperty *property;
	gdouble initial;
	gchar *name;
};

struct _CpgNetworkWebots
{
	CpgNetwork *network;
	GSList *bindings;
};

#endif /* __CPG_NETWORK_WEBOTS_PRIVATE_H__ */

