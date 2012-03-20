/*
 * cdn-annotatable.c
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

#include "cdn-annotatable.h"

/**
 * SECTION:cdn-annotatable
 * @short_description: Interface for annotating objects
 *
 * This interface can be implemented when an object supports annotations.
 *
 **/

G_DEFINE_INTERFACE (CdnAnnotatable, cdn_annotatable, G_TYPE_OBJECT);

static gchar *
cdn_annotatable_get_title_default (CdnAnnotatable *annotatable)
{
	return NULL;
}

static gchar *
cdn_annotatable_get_annotation_default (CdnAnnotatable *annotatable)
{
	gchar *annotation;

	g_object_get (annotatable, "annotation", &annotation, NULL);

	return annotation;
}

static void
cdn_annotatable_set_annotation_default (CdnAnnotatable *annotatable,
                                        gchar const    *annotation)
{
	g_object_set (annotatable, "annotation", annotation, NULL);
}

static void
cdn_annotatable_default_init (CdnAnnotatableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_title = cdn_annotatable_get_title_default;
	iface->set_annotation = cdn_annotatable_set_annotation_default;
	iface->get_annotation = cdn_annotatable_get_annotation_default;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_string ("annotation",
		                                                          "Annotation",
		                                                          "Object annotation",
		                                                          NULL,
		                                                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

/**
 * cdn_annotatable_get_title:
 * @annotatable: A #CdnAnnotatable
 *
 * Get the title (display name) of the annotatable object.
 *
 * Returns: (transfer full): the title
 *
 **/
gchar *
cdn_annotatable_get_title (CdnAnnotatable *annotatable)
{
	g_return_val_if_fail (CDN_IS_ANNOTATABLE (annotatable), NULL);

	return CDN_ANNOTATABLE_GET_INTERFACE (annotatable)->get_title (annotatable);
}

/**
 * cdn_annotatable_set_annotation:
 * @annotatable: A #CdnAnnotatable
 * @annotation: The annotation
 *
 * Set the object annotation.
 *
 **/
void
cdn_annotatable_set_annotation (CdnAnnotatable *annotatable,
                                gchar const    *annotation)
{
	g_return_if_fail (CDN_IS_ANNOTATABLE (annotatable));

	CDN_ANNOTATABLE_GET_INTERFACE (annotatable)->set_annotation (annotatable,
	                                                             annotation);
}

/**
 * cdn_annotatable_get_annotation:
 * @annotatable: A #CdnAnnotatable
 *
 * Get the annotation.
 *
 * Returns: (transfer full): Get the object annotation
 *
 **/
gchar *
cdn_annotatable_get_annotation (CdnAnnotatable *annotatable)
{
	g_return_val_if_fail (CDN_IS_ANNOTATABLE (annotatable), NULL);

	return CDN_ANNOTATABLE_GET_INTERFACE (annotatable)->get_annotation (annotatable);
}
