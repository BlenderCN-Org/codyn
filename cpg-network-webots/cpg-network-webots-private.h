/*
 * cpg-network-webots-private.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

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

