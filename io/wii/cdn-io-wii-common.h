/*
 * cdn-io-wii-common.h
 * This file is part of codyn
 *
 * Copyright (C) 2012 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_IO_WII_COMMON_H__
#define __CDN_IO_WII_COMMON_H__

#include <glib-object.h>
#include <codyn/cdn-variable.h>

G_BEGIN_DECLS

typedef struct
{
	gchar const *name;
	CdnVariableFlags flags;
	gint dimension[2];
} CdnIoWiiVariableInfo;

typedef enum
{
	CDN_IO_WII_VARIABLE_ID,
	CDN_IO_WII_VARIABLE_LED,
	CDN_IO_WII_VARIABLE_RUMBLE,
	CDN_IO_WII_VARIABLE_BATTERY,
	CDN_IO_WII_VARIABLE_BUTTON_A,
	CDN_IO_WII_VARIABLE_BUTTON_B,
	CDN_IO_WII_VARIABLE_BUTTON_1,
	CDN_IO_WII_VARIABLE_BUTTON_2,
	CDN_IO_WII_VARIABLE_BUTTON_MIN,
	CDN_IO_WII_VARIABLE_BUTTON_PLUS,
	CDN_IO_WII_VARIABLE_BUTTON_HOME,
	CDN_IO_WII_VARIABLE_BUTTON_LEFT,
	CDN_IO_WII_VARIABLE_BUTTON_RIGHT,
	CDN_IO_WII_VARIABLE_BUTTON_UP,
	CDN_IO_WII_VARIABLE_BUTTON_DOWN,
	CDN_IO_WII_VARIABLE_ACCELERATION,
	CDN_IO_WII_VARIABLE_IR,
	CDN_IO_WII_VARIABLE_IR_ACTIVE,
	CDN_IO_WII_VARIABLE_IR_SIZE,
	CDN_IO_WII_VARIABLE_NUM
} CdnIoWiiVariable;

CdnIoWiiVariableInfo const *cdn_io_wii_common_get_variable_info (CdnIoWiiVariable variable);

G_END_DECLS

#endif /* __CDN_IO_WII_COMMON_H__ */

