/*
 * cdn-modifiable.c
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

#include "cdn-modifiable.h"

G_DEFINE_INTERFACE (CdnModifiable, cdn_modifiable, G_TYPE_OBJECT);

/* Default implementation */
static gboolean
cdn_modifiable_get_modified_default (CdnModifiable *modifiable)
{
	gboolean ret = FALSE;

	g_object_get (modifiable, "modified", &ret, NULL);
	return ret;
}

static void
cdn_modifiable_set_modified_default (CdnModifiable *modifiable,
                                     gboolean       modified)
{
	gboolean orig = cdn_modifiable_get_modified (modifiable);

	if (orig != modified)
	{
		g_object_set (modifiable, "modified", modified, NULL);
	}
}

static void
cdn_modifiable_default_init (CdnModifiableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_modified = cdn_modifiable_get_modified_default;
	iface->set_modified = cdn_modifiable_set_modified_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_boolean ("modified",
		                                                           "Modified",
		                                                           "Whether the object is modified",
		                                                           FALSE,
		                                                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

/**
 * cdn_modifiable_get_modified:
 * @modifiable: A #CdnModifiable
 *
 * Get the modified state.
 *
 * Returns: %TRUE if the object has been modified, %FALSE otherwise
 *
 **/
gboolean
cdn_modifiable_get_modified (CdnModifiable *modifiable)
{
	g_return_val_if_fail (CDN_MODIFIABLE (modifiable), FALSE);

	return CDN_MODIFIABLE_GET_INTERFACE (modifiable)->get_modified (modifiable);
}


/**
 * cdn_modifiable_set_modified:
 * @modifiable: A #CdnModifiable
 * @modified: Modified state
 *
 * Set the modified state of the object.
 *
 **/
void
cdn_modifiable_set_modified (CdnModifiable *modifiable,
                             gboolean       modified)
{
	g_return_if_fail (CDN_MODIFIABLE (modifiable));

	CDN_MODIFIABLE_GET_INTERFACE (modifiable)->set_modified (modifiable,
	                                                         modified ? TRUE : FALSE);
}
