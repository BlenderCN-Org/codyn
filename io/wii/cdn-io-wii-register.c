/*
 * cdn-io-wii-register.c
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

#include "cdn-io-wii-register.h"
#include "cdn-io-wii.h"
#include "cdn-io-wii-manager.h"

void
cdn_io_register_types (GTypeModule *module)
{
	cdn_io_wii_manager_register (module);
	cdn_io_wii_register (module);
}
