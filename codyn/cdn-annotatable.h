/*
 * cdn-annotatable.h
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

#ifndef __CDN_ANNOTATABLE_H__
#define __CDN_ANNOTATABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_ANNOTATABLE			(cdn_annotatable_get_type ())
#define CDN_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_ANNOTATABLE, CdnAnnotatable))
#define CDN_IS_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_ANNOTATABLE))
#define CDN_ANNOTATABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_ANNOTATABLE, CdnAnnotatableInterface))

typedef struct _CdnAnnotatable		CdnAnnotatable;
typedef struct _CdnAnnotatableInterface	CdnAnnotatableInterface;

typedef struct _CdnAnnotationInfo CdnAnnotationInfo;

typedef enum
{
	CDN_ANNOTATION_ANCHOR_NORTH,
	CDN_ANNOTATION_ANCHOR_NORTH_EAST,
	CDN_ANNOTATION_ANCHOR_EAST,
	CDN_ANNOTATION_ANCHOR_SOUTH_EAST,
	CDN_ANNOTATION_ANCHOR_SOUTH,
	CDN_ANNOTATION_ANCHOR_SOUTH_WEST,
	CDN_ANNOTATION_ANCHOR_WEST,
	CDN_ANNOTATION_ANCHOR_NORTH_WEST,
	CDN_ANNOTATION_ANCHOR_CENTER
} CdnAnnotationAnchor;

/**
 * CdnAnnotatable:
 *
 * Interface for annotating objects.
 *
 * This interface can be implemented when an object supports annotations.
 **/
struct _CdnAnnotatableInterface
{
	GTypeInterface parent;

	gchar *(*get_title) (CdnAnnotatable *annotatable);

	void (*set_annotation) (CdnAnnotatable *annotatable,
	                        gchar const    *annotation);

	gchar *(*get_annotation) (CdnAnnotatable *annotatable);
};

GType  cdn_annotatable_get_type       (void) G_GNUC_CONST;

gchar *cdn_annotatable_get_title      (CdnAnnotatable *annotatable);

gchar *cdn_annotatable_get_annotation (CdnAnnotatable *annotatable);

void   cdn_annotatable_set_annotation (CdnAnnotatable *annotatable,
                                       gchar const    *annotation);

CdnAnnotationInfo *cdn_annotatable_parse_annotation (CdnAnnotatable *annotatable);

GType cdn_annotation_info_get_type ();
void cdn_annotation_info_free (CdnAnnotationInfo *info);

const gchar *cdn_annotation_info_get_annotation (CdnAnnotationInfo *info);

gboolean cdn_annotation_info_get_location   (CdnAnnotationInfo *info,
                                             gdouble           *x,
                                             gdouble           *y);

CdnAnnotationAnchor cdn_annotation_info_get_anchor   (CdnAnnotationInfo *info);
const gchar *cdn_annotation_info_get_relative_to   (CdnAnnotationInfo *info);

const gchar *cdn_annotation_info_get_text       (CdnAnnotationInfo *info);

G_END_DECLS

#endif /* __CDN_ANNOTATABLE_H__ */
