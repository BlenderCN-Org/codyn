/*
 * cdn-taggable.h
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

#ifndef __CDN_TAGGABLE_H__
#define __CDN_TAGGABLE_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

#define CDN_TYPE_TAGGABLE			(cdn_taggable_get_type ())
#define CDN_TAGGABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_TAGGABLE, CdnTaggable))
#define CDN_IS_TAGGABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_TAGGABLE))
#define CDN_TAGGABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_TAGGABLE, CdnTaggableInterface))

typedef struct _CdnTaggable			CdnTaggable;
typedef struct _CdnTaggableInterface		CdnTaggableInterface;

struct _CdnTaggableInterface
{
	GTypeInterface parent;

	GHashTable *(*get_tag_table) (CdnTaggable *taggable);
};

typedef void (*CdnTaggableForeachFunc) (CdnTaggable *taggable,
                                        gchar const *key,
                                        gchar const *value,
                                        gpointer     userdata);

GType        cdn_taggable_get_type      (void) G_GNUC_CONST;

gboolean     cdn_taggable_has_tag       (CdnTaggable  *taggable,
                                         gchar const  *tag);

void         cdn_taggable_add_tag       (CdnTaggable  *taggable,
                                         gchar const  *tag,
                                         gchar const  *value);

void         cdn_taggable_remove_tag    (CdnTaggable  *taggable,
                                         gchar const  *tag);

gchar const *cdn_taggable_get_tag       (CdnTaggable  *taggable,
                                         gchar const  *tag);

gboolean     cdn_taggable_try_get_tag   (CdnTaggable  *taggable,
                                         gchar const  *tag,
                                         gchar const **value);

GHashTable  *cdn_taggable_get_tag_table (CdnTaggable  *taggable);

GHashTable  *cdn_taggable_create_table  (void);
void         cdn_taggable_copy_to       (CdnTaggable  *taggable,
                                         GHashTable   *tags);

void         cdn_taggable_foreach       (CdnTaggable            *taggable,
                                         CdnTaggableForeachFunc  func,
                                         gpointer                userdata);

G_END_DECLS

#endif /* __CDN_TAGGABLE_H__ */
