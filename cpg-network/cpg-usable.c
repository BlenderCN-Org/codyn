/*
 * cpg-usable.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-usable.h"

G_DEFINE_INTERFACE (CpgUsable, cpg_usable, G_TYPE_OBJECT)

/**
 * SECTION:cpg-usable
 * @short_description: Interface for counting the uses of an object
 *
 * This interface can be implemented when an object provides a use count.
 *
 **/
static guint
cpg_usable_use_count_default (CpgUsable *self)
{
	guint use_count;

	g_object_get (self, "use-count", &use_count, NULL);
	return use_count;
}

static void
cpg_usable_use_default (CpgUsable *self)
{
	g_return_if_reached ();
}

static gboolean
cpg_usable_unuse_default (CpgUsable *self)
{
	g_return_val_if_reached (FALSE);
}

static void
cpg_usable_default_init (CpgUsableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->use_count = cpg_usable_use_count_default;
	iface->use = cpg_usable_use_default;
	iface->unuse = cpg_usable_unuse_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_uint ("use-count",
		                                                        "Use Count",
		                                                        "The use count",
		                                                        0,
		                                                        G_MAXUINT,
		                                                        0,
		                                                        G_PARAM_READABLE));

		initialized = TRUE;
	}
}

/**
 * cpg_usable_use_count:
 * @self: A #CpgUsable
 *
 * Get the use count of the usable.
 *
 * Returns: The use count
 *
 **/
guint
cpg_usable_use_count (CpgUsable *self)
{
	g_return_val_if_fail (CPG_USABLE (self), 0);

	return CPG_USABLE_GET_INTERFACE (self)->use_count (self);
}

/**
 * cpg_usable_use:
 * @self: A #CpgUsable
 *
 * Increase the use count.
 *
 **/
void
cpg_usable_use (CpgUsable *self)
{
	g_return_if_fail (CPG_USABLE (self));

	CPG_USABLE_GET_INTERFACE (self)->use (self);
}

/**
 * cpg_usable_unuse:
 * @self: A #CpgUsable
 *
 * Decrease the use count.
 *
 * Returns: %TRUE if the use count dropped to 0, %FALSE otherwise
 *
 **/
gboolean
cpg_usable_unuse (CpgUsable *self)
{
	g_return_val_if_fail (CPG_USABLE (self), FALSE);

	return CPG_USABLE_GET_INTERFACE (self)->unuse (self);
}
