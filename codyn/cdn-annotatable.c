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
#include "cdn-enum-types.h"

struct _CdnAnnotationInfo
{
	gchar *annotation;
	gchar *text;

	struct
	{
		gdouble x;
		gdouble y;

		gboolean set;
	} location;

	gchar *relative;
	CdnAnnotationAnchor anchor;
};

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

/**
 * cdn_annotatable_parse_annotation:
 * @annotatable: a #CdnAnnotatable.
 *
 * Parse the annotation for additional metadata.
 *
 * Returns: (transfer full): a #CdnAnnotationInfo.
 *
 **/
CdnAnnotationInfo *
cdn_annotatable_parse_annotation (CdnAnnotatable *annotatable)
{
	CdnAnnotationInfo *ret;
	GMatchInfo *info;

	static GRegex *regex = NULL;

	g_return_val_if_fail (CDN_IS_ANNOTATABLE (annotatable), NULL);

	ret = g_slice_new0 (CdnAnnotationInfo);
	ret->annotation = cdn_annotatable_get_annotation (annotatable);

	if (!ret->annotation)
	{
		return ret;
	}

	// Parse it here
	if (!regex)
	{
		regex = g_regex_new ("<at(?:\\.(?P<anchor>"
		                     "north|north-east|east|south-east|south|south-west|west|north-west|center"
		                     "))?\\((?P<x>[-0-9.]+),\\s*(?P<y>[-0-9.]+)\\)(\\s+of\\s+(?P<relative>.*))?>\\s*$",
		                     G_REGEX_MULTILINE | G_REGEX_CASELESS,
		                     0,
		                     NULL);
	}

	if (g_regex_match (regex, ret->annotation, 0, &info))
	{
		gchar *anchor;
		gchar *x;
		gchar *y;
		gchar *relative;
		gint start;
		gint end;
		gchar *pre;
		gchar *post;

		anchor = g_match_info_fetch_named (info, "anchor");

		if (anchor)
		{
			gpointer cls;
			GEnumValue *val;

			cls = g_type_class_ref (CDN_TYPE_ANNOTATION_ANCHOR);
			val = g_enum_get_value_by_nick (cls, anchor);

			ret->anchor = val->value;

			g_type_class_unref (cls);
		}
		else
		{
			ret->anchor = CDN_ANNOTATION_ANCHOR_SOUTH;
		}

		x = g_match_info_fetch_named (info, "x");
		y = g_match_info_fetch_named (info, "y");

		if (x && y && *x && *y)
		{
			ret->location.x = g_ascii_strtod (x, NULL);
			ret->location.y = g_ascii_strtod (y, NULL);
			ret->location.set = TRUE;
		}

		relative = g_match_info_fetch_named (info, "relative");

		if (relative && *relative)
		{
			ret->relative = relative;
			relative = NULL;
		}

		/* Cut out the annotation */
		g_match_info_fetch_pos (info, 0, &start, &end);

		pre = g_strndup (ret->annotation, start);
		post = g_strdup (ret->annotation + end);

		ret->text = g_strconcat (pre, post, NULL);

		g_free (pre);
		g_free (post);

		g_free (anchor);
		g_free (relative);
		g_free (x);
		g_free (y);

		g_match_info_free (info);
	}
	else
	{
		ret->text = g_strdup (ret->annotation);
	}

	return ret;
}

static CdnAnnotationInfo *
cdn_annotation_info_copy (CdnAnnotationInfo *info)
{
	CdnAnnotationInfo *ret;

	ret = g_slice_new (CdnAnnotationInfo);

	ret->annotation = g_strdup (info->annotation);
	ret->text = g_strdup (info->text);

	ret->location.x = info->location.x;
	ret->location.y = info->location.y;

	ret->location.set = info->location.set;

	ret->relative = g_strdup (info->relative);
	ret->anchor = info->anchor;

	return ret;
}

GType
cdn_annotation_info_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnAnnotationInfo",
		                                      (GBoxedCopyFunc)cdn_annotation_info_copy,
		                                      (GBoxedFreeFunc)cdn_annotation_info_free);
	}

	return gtype;
}

/**
 * cdn_annotation_info_free:
 * @info: a #CdnAnnotationInfo.
 *
 * Free the annotation info.
 *
 **/
void
cdn_annotation_info_free (CdnAnnotationInfo *info)
{
	g_free (info->relative);
	g_free (info->text);
	g_free (info->annotation);

	g_slice_free (CdnAnnotationInfo, info);
}

/**
 * cdn_annotation_info_get_annotation:
 * @info: a #CdnAnnotationInfo.
 *
 * Get the original annotation from which the annotation info was extracted.
 *
 * Returns: the original annotation.
 *
 **/
const gchar *
cdn_annotation_info_get_annotation (CdnAnnotationInfo *info)
{
	return info->annotation;
}

/**
 * cdn_annotation_info_get_location:
 * @info: a #CdnAnnotationInfo.
 * @x: (out) (allow-none): the return value of the x location coordinate.
 * @y: (out) (allow-none): the return value of the y location coordinate.
 *
 * Get the location of the annotation.
 *
 * Returns: %TRUE if the location was set in the annotation, %FALSE otherwise.
 *
 **/
gboolean
cdn_annotation_info_get_location (CdnAnnotationInfo *info,
                                  gdouble           *x,
                                  gdouble           *y)
{
	if (!info->location.set)
	{
		return FALSE;
	}

	if (x)
	{
		*x = info->location.x;
	}

	if (y)
	{
		*y = info->location.y;
	}

	return TRUE;
}

/**
 * cdn_annotation_info_get_anchor:
 * @info: a #CdnAnnotationInfo.
 *
 * Get the anchor point from which the location of the annotation is specified.
 *
 * Returns: a #CdnAnnotationAnchor.
 *
 **/
CdnAnnotationAnchor
cdn_annotation_info_get_anchor (CdnAnnotationInfo *info)
{
	return info->anchor;
}

/**
 * cdn_annotation_info_get_relative_to:
 * @info: a #CdnAnnotationInfo.
 *
 * Get a selector string specifying the location from which to interpret the
 * location of the annotation (i.e. it specifies the origin of the annotation
 * location).
 *
 * Returns: a selector string specifying the coordinate origin, or %NULL.
 *
 **/
const gchar *
cdn_annotation_info_get_relative_to (CdnAnnotationInfo *info)
{
	return info->relative;
}

/**
 * cdn_annotation_info_get_text:
 * @info: a #CdnAnnotationInfo.
 *
 * Get the annotation text without metadata.
 *
 * Returns: the annotation text without metadata.
 *
 **/
const gchar *
cdn_annotation_info_get_text (CdnAnnotationInfo *info)
{
	return info->text;
}
