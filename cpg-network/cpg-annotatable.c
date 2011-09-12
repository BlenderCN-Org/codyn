/*
 * cpg-annotatable.c
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

#include "cpg-annotatable.h"

/**
 * SECTION:cpg-annotatable
 * @short_description: Interface for annotating objects
 *
 * This interface can be implemented when an object supports annotations.
 *
 **/

G_DEFINE_INTERFACE (CpgAnnotatable, cpg_annotatable, G_TYPE_OBJECT);

static gchar *
cpg_annotatable_get_title_default (CpgAnnotatable *annotatable)
{
	return NULL;
}

static gchar *
cpg_annotatable_get_annotation_default (CpgAnnotatable *annotatable)
{
	gchar *annotation;

	g_object_get (annotatable, "annotation", &annotation, NULL);

	return annotation;
}

static void
cpg_annotatable_set_annotation_default (CpgAnnotatable *annotatable,
                                        gchar const    *annotation)
{
	g_object_set (annotatable, "annotation", annotation, NULL);
}

static void
cpg_annotatable_default_init (CpgAnnotatableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_title = cpg_annotatable_get_title_default;
	iface->set_annotation = cpg_annotatable_set_annotation_default;
	iface->get_annotation = cpg_annotatable_get_annotation_default;

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
 * cpg_annotatable_get_title:
 * @annotatable: A #CpgAnnotatable
 *
 * Get the title (display name) of the annotatable object.
 *
 * Returns: (transfer full): the title
 *
 **/
gchar *
cpg_annotatable_get_title (CpgAnnotatable *annotatable)
{
	g_return_val_if_fail (CPG_IS_ANNOTATABLE (annotatable), NULL);

	return CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->get_title (annotatable);
}

/**
 * cpg_annotatable_set_annotation:
 * @annotatable: A #CpgAnnotatable
 * @annotation: The annotation
 *
 * Set the object annotation.
 *
 **/
void
cpg_annotatable_set_annotation (CpgAnnotatable *annotatable,
                                gchar const    *annotation)
{
	g_return_if_fail (CPG_IS_ANNOTATABLE (annotatable));

	CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->set_annotation (annotatable,
	                                                             annotation);
}

/**
 * cpg_annotatable_get_annotation:
 * @annotatable: A #CpgAnnotatable
 *
 * Get the annotation.
 *
 * Returns: (transfer full): Get the object annotation
 *
 **/
gchar *
cpg_annotatable_get_annotation (CpgAnnotatable *annotatable)
{
	g_return_val_if_fail (CPG_IS_ANNOTATABLE (annotatable), NULL);

	return CPG_ANNOTATABLE_GET_INTERFACE (annotatable)->get_annotation (annotatable);
}
