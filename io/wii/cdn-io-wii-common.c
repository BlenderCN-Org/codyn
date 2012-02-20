/*
 * cdn-io-wii-common.c
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

#include "cdn-io-wii-common.h"
#include <cwiid.h>

static CdnIoWiiVariableInfo infos[] =
{
	{"id", CDN_VARIABLE_FLAG_NONE, {1, 1}},
	{"led", CDN_VARIABLE_FLAG_OUT, {1, 1}},
	{"rumble", CDN_VARIABLE_FLAG_OUT, {1, 1}},
	{"battery", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_a", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_b", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_1", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_2", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_min", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_plus", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_home", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_left", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_right", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_up", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"button_down", CDN_VARIABLE_FLAG_IN, {1, 1}},
	{"accel", CDN_VARIABLE_FLAG_OUT, {3, 1}},
	{"ir", CDN_VARIABLE_FLAG_NONE, {CWIID_IR_SRC_COUNT, 2}},
	{"ir_active", CDN_VARIABLE_FLAG_NONE, {CWIID_IR_SRC_COUNT, 1}},
	{"ir_size", CDN_VARIABLE_FLAG_NONE, {CWIID_IR_SRC_COUNT, 1}}
};

CdnIoWiiVariableInfo const *
cdn_io_wii_common_get_variable_info (CdnIoWiiVariable variable)
{
	if (variable < 0 || variable >= CDN_IO_WII_VARIABLE_NUM)
	{
		return NULL;
	}

	return &infos[variable];
}
