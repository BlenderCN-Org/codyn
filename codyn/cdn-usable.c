/*
 * cdn-usable.c
 * This file is part of codyn
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

#include "cdn-usable.h"

G_DEFINE_INTERFACE (CdnUsable, cdn_usable, G_TYPE_OBJECT)

static guint
cdn_usable_use_count_default (CdnUsable *self)
{
	guint use_count;

	g_object_get (self, "use-count", &use_count, NULL);
	return use_count;
}

static void
cdn_usable_use_default (CdnUsable *self)
{
	g_return_if_reached ();
}

static gboolean
cdn_usable_unuse_default (CdnUsable *self)
{
	g_return_val_if_reached (FALSE);
}

static void
cdn_usable_default_init (CdnUsableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->use_count = cdn_usable_use_count_default;
	iface->use = cdn_usable_use_default;
	iface->unuse = cdn_usable_unuse_default;

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
 * cdn_usable_use_count:
 * @self: A #CdnUsable
 *
 * Get the use count of the usable.
 *
 * Returns: The use count
 *
 **/
guint
cdn_usable_use_count (CdnUsable *self)
{
	g_return_val_if_fail (CDN_USABLE (self), 0);

	return CDN_USABLE_GET_INTERFACE (self)->use_count (self);
}

/**
 * cdn_usable_use:
 * @self: A #CdnUsable
 *
 * Increase the use count.
 *
 **/
void
cdn_usable_use (CdnUsable *self)
{
	g_return_if_fail (CDN_USABLE (self));

	CDN_USABLE_GET_INTERFACE (self)->use (self);
}

/**
 * cdn_usable_unuse:
 * @self: A #CdnUsable
 *
 * Decrease the use count.
 *
 * Returns: %TRUE if the use count dropped to 0, %FALSE otherwise
 *
 **/
gboolean
cdn_usable_unuse (CdnUsable *self)
{
	g_return_val_if_fail (CDN_USABLE (self), FALSE);

	return CDN_USABLE_GET_INTERFACE (self)->unuse (self);
}
