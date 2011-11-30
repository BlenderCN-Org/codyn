/*
 * cdn-attribute.h
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

#ifndef __CDN_ATTRIBUTE_H__
#define __CDN_ATTRIBUTE_H__

#include <glib-object.h>
#include <codyn/cdn-embedded-string.h>

G_BEGIN_DECLS

#define CDN_TYPE_ATTRIBUTE		(cdn_attribute_get_type ())
#define CDN_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_ATTRIBUTE, CdnAttribute))
#define CDN_ATTRIBUTE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_ATTRIBUTE, CdnAttribute const))
#define CDN_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_ATTRIBUTE, CdnAttributeClass))
#define CDN_IS_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_ATTRIBUTE))
#define CDN_IS_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_ATTRIBUTE))
#define CDN_ATTRIBUTE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_ATTRIBUTE, CdnAttributeClass))

typedef struct _CdnAttribute		CdnAttribute;
typedef struct _CdnAttributeClass	CdnAttributeClass;
typedef struct _CdnAttributePrivate	CdnAttributePrivate;

struct _CdnAttribute
{
	/*< private >*/
	GObject parent;

	CdnAttributePrivate *priv;
};

struct _CdnAttributeClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_attribute_get_type (void) G_GNUC_CONST;

CdnAttribute *cdn_attribute_new (gchar const *id);
CdnAttribute *cdn_attribute_newv (gchar const *id,
                                  ...) G_GNUC_NULL_TERMINATED;

void cdn_attribute_set_arguments (CdnAttribute *attribute, GSList *arguments);

gchar const *cdn_attribute_get_id (CdnAttribute *attribute);
GSList *cdn_attribute_get_arguments (CdnAttribute *attribute);

GObject *cdn_attribute_get_argument (CdnAttribute *attribute, gint i);
gint cdn_attribute_num_arguments (CdnAttribute *attribute);

G_END_DECLS

#endif /* __CDN_ATTRIBUTE_H__ */
