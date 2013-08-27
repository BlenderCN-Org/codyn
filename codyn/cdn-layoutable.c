/*
 * cdn-layoutable.c
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

#include "cdn-layoutable.h"
#include <codyn/cdn-network.h>

G_DEFINE_INTERFACE (CdnLayoutable, cdn_layoutable, G_TYPE_OBJECT)

/* Default implementation */
static void
cdn_layoutable_get_location_default (CdnLayoutable *layoutable,
                                     gint          *x,
                                     gint          *y)
{
	g_object_get (layoutable, "x", x, "y", y, NULL);
}

static void
cdn_layoutable_set_location_default (CdnLayoutable *layoutable,
                                     gint           x,
                                     gint           y)
{
	g_object_set (layoutable, "x", x, "y", y, NULL);
}

static gboolean
cdn_layoutable_supports_location_default (CdnLayoutable *layoutable)
{
	return TRUE;
}

static gboolean
cdn_layoutable_get_has_location_default (CdnLayoutable *layoutable)
{
	gboolean ret;

	ret = FALSE;

	g_object_get (layoutable, "has-location", &ret, NULL);

	return ret;
}

static void
cdn_layoutable_set_has_location_default (CdnLayoutable *layoutable,
                                         gboolean       has_location)
{
	g_object_set (layoutable, "has-location", has_location, NULL);
}

static void
cdn_layoutable_default_init (CdnLayoutableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_location = cdn_layoutable_get_location_default;
	iface->set_location = cdn_layoutable_set_location_default;
	iface->supports_location = cdn_layoutable_supports_location_default;
	iface->get_has_location = cdn_layoutable_get_has_location_default;
	iface->set_has_location = cdn_layoutable_set_has_location_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("x",
		                                                       "X",
		                                                       "X location",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("y",
		                                                       "Y",
		                                                       "Y location",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_boolean ("has-location",
		                                                           "Has Location",
		                                                           "Has location",
		                                                           FALSE,
		                                                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

/**
 * cdn_layoutable_get_location:
 * @layoutable: A #CdnLayoutable
 * @x: (out): x
 * @y: (out): y
 *
 * Get the location of the layoutable.
 *
 **/
void
cdn_layoutable_get_location (CdnLayoutable *layoutable,
                             gint          *x,
                             gint          *y)
{
	g_return_if_fail (CDN_IS_LAYOUTABLE (layoutable));

	if (cdn_layoutable_supports_location (layoutable) &&
	    cdn_layoutable_get_has_location (layoutable))
	{
		CDN_LAYOUTABLE_GET_INTERFACE (layoutable)->get_location (layoutable,
		                                                                x,
		                                                                y);
	}
	else
	{
		if (x)
		{
			*x = 0;
		}

		if (y)
		{
			*y = 0;
		}
	}
}

/**
 * cdn_layoutable_set_location:
 * @layoutable: A #CdnLayoutable
 * @x: x
 * @y: y
 *
 * Set the location of the layoutable.
 *
 **/
void
cdn_layoutable_set_location (CdnLayoutable *layoutable,
                             gint           x,
                             gint           y)
{
	g_return_if_fail (CDN_IS_LAYOUTABLE (layoutable));

	if (cdn_layoutable_supports_location (layoutable))
	{
		cdn_layoutable_set_has_location (layoutable, TRUE);

		CDN_LAYOUTABLE_GET_INTERFACE (layoutable)->set_location (layoutable,
		                                                         x,
		                                                         y);
	}
}

/**
 * cdn_layoutable_supports_location:
 * @layoutable: A #CdnLayoutable
 *
 * Whether the layoutable supports a location.
 *
 * Returns: %TRUE if the layoutable supports location, %FALSE otherwise
 *
 **/
gboolean
cdn_layoutable_supports_location (CdnLayoutable *layoutable)
{
	g_return_val_if_fail (CDN_IS_LAYOUTABLE (layoutable), FALSE);

	return CDN_LAYOUTABLE_GET_INTERFACE (layoutable)->supports_location (layoutable);
}

gboolean
cdn_layoutable_get_has_location (CdnLayoutable *layoutable)
{
	g_return_val_if_fail (CDN_IS_LAYOUTABLE (layoutable), FALSE);

	return CDN_LAYOUTABLE_GET_INTERFACE (layoutable)->get_has_location (layoutable);
}

void
cdn_layoutable_set_has_location (CdnLayoutable *layoutable,
                                 gboolean       has_location)
{
	g_return_if_fail (CDN_IS_LAYOUTABLE (layoutable));

	CDN_LAYOUTABLE_GET_INTERFACE (layoutable)->set_has_location (layoutable,
	                                                             has_location);
}
