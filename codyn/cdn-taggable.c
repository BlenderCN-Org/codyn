/*
 * cdn-taggable.c
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

#include "cdn-taggable.h"

/**
 * SECTION:cdn-taggable
 * @short_description: Interface for keeping track of the modified state of an object
 *
 * This interface can be implemented when an object needs to keep track of
 * whether or not it has been modified.
 *
 **/

G_DEFINE_INTERFACE (CdnTaggable, cdn_taggable, G_TYPE_OBJECT);

/* Default implementation */
static GHashTable *
cdn_taggable_get_tag_table_default (CdnTaggable *taggable)
{
	return NULL;
}

static void
cdn_taggable_default_init (CdnTaggableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_tag_table = cdn_taggable_get_tag_table_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

gboolean
cdn_taggable_has_tag (CdnTaggable *taggable,
                      gchar const *tag)
{
	g_return_val_if_fail (CDN_TAGGABLE (taggable), FALSE);

	return cdn_taggable_try_get_tag (taggable, tag, NULL);
}

void
cdn_taggable_add_tag (CdnTaggable *taggable,
                      gchar const *tag,
                      gchar const *value)
{
	GHashTable *table;

	g_return_if_fail (CDN_TAGGABLE (taggable));

	table = cdn_taggable_get_tag_table (taggable);

	g_hash_table_insert (table, g_strdup (tag), g_strdup (value));
}

void
cdn_taggable_remove_tag (CdnTaggable *taggable,
                         gchar const *tag)
{
	GHashTable *table;

	g_return_if_fail (CDN_TAGGABLE (taggable));

	table = cdn_taggable_get_tag_table (taggable);

	g_hash_table_remove (table, tag);
}

/**
 * cdn_taggable_create_table:
 *
 * Create a hash table suitable to store tags using #CdnTaggable.
 *
 * Returns: (transfer full): A #GHashTable
 *
 **/
GHashTable *
cdn_taggable_create_table ()
{
	return g_hash_table_new_full (g_str_hash,
	                              g_str_equal,
	                              (GDestroyNotify)g_free,
	                              (GDestroyNotify)g_free);
}

gchar const *
cdn_taggable_get_tag (CdnTaggable  *taggable,
                      gchar const  *tag)

{
	GHashTable *table;

	g_return_val_if_fail (CDN_IS_TAGGABLE (taggable), NULL);

	table = cdn_taggable_get_tag_table (taggable);

	return g_hash_table_lookup (table, tag);
}

gboolean
cdn_taggable_try_get_tag  (CdnTaggable  *taggable,
                           gchar const  *tag,
                           gchar const **value)
{
	GHashTable *table;

	g_return_val_if_fail (CDN_IS_TAGGABLE (taggable), FALSE);

	table = cdn_taggable_get_tag_table (taggable);

	return g_hash_table_lookup_extended (table, tag, NULL, (gpointer *)value);
}

/**
 * cdn_taggable_get_tag_table:
 * @taggable: A #CdnTaggable
 *
 * Get the tag hash table.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cdn_taggable_get_tag_table (CdnTaggable *taggable)
{
	g_return_val_if_fail (CDN_IS_TAGGABLE (taggable), NULL);

	return CDN_TAGGABLE_GET_INTERFACE (taggable)->get_tag_table (taggable);
}

static void
copy_tag (gchar const  *key,
          gchar const  *value,
          GHashTable   *ret)
{
	g_hash_table_insert (ret, g_strdup (key), g_strdup (value));
}

void
cdn_taggable_copy_to (CdnTaggable *taggable,
                      GHashTable  *tags)
{
	g_return_if_fail (CDN_IS_TAGGABLE (taggable));

	g_hash_table_foreach (cdn_taggable_get_tag_table (taggable),
	                      (GHFunc)copy_tag,
	                      tags);
}

typedef struct
{
	CdnTaggable *taggable;
	CdnTaggableForeachFunc func;
	gpointer userdata;
} ForeachInfo;

static void
taggable_foreach (gchar const *key,
                  gchar const *value,
                  ForeachInfo *info)
{
	info->func (info->taggable, key, value, info->userdata);
}

void
cdn_taggable_foreach (CdnTaggable *taggable,
                      CdnTaggableForeachFunc  func,
                      gpointer                userdata)
{
	ForeachInfo info;

	g_return_if_fail (CDN_IS_TAGGABLE (taggable));
	g_return_if_fail (func != NULL);

	info.taggable = taggable;
	info.func = func;
	info.userdata = userdata;

	g_hash_table_foreach (cdn_taggable_get_tag_table (taggable),
	                      (GHFunc)taggable_foreach,
	                      &info);
}
