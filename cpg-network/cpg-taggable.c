/*
 * cpg-taggable.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-taggable.h"

/**
 * SECTION:cpg-taggable
 * @short_description: Interface for keeping track of the modified state of an object
 *
 * This interface can be implemented when an object needs to keep track of
 * whether or not it has been modified.
 *
 **/

G_DEFINE_INTERFACE (CpgTaggable, cpg_taggable, G_TYPE_OBJECT);

/* Default implementation */
static GHashTable *
cpg_taggable_get_tag_table_default (CpgTaggable *taggable)
{
	return NULL;
}

static void
cpg_taggable_default_init (CpgTaggableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_tag_table = cpg_taggable_get_tag_table_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

gboolean
cpg_taggable_has_tag (CpgTaggable *taggable,
                      gchar const *tag)
{
	g_return_val_if_fail (CPG_TAGGABLE (taggable), FALSE);

	return cpg_taggable_try_get_tag (taggable, tag, NULL);
}

void
cpg_taggable_add_tag (CpgTaggable *taggable,
                      gchar const *tag,
                      gchar const *value)
{
	GHashTable *table;

	g_return_if_fail (CPG_TAGGABLE (taggable));

	table = cpg_taggable_get_tag_table (taggable);

	g_hash_table_insert (table, g_strdup (tag), g_strdup (value));
}

void
cpg_taggable_remove_tag (CpgTaggable *taggable,
                         gchar const *tag)
{
	GHashTable *table;

	g_return_if_fail (CPG_TAGGABLE (taggable));

	table = cpg_taggable_get_tag_table (taggable);

	g_hash_table_remove (table, tag);
}

/**
 * cpg_taggable_create_table:
 *
 * Create a hash table suitable to store tags using #CpgTaggable.
 *
 * Returns: (transfer full): A #GHashTable
 *
 **/
GHashTable *
cpg_taggable_create_table ()
{
	return g_hash_table_new_full (g_str_hash,
	                              g_str_equal,
	                              (GDestroyNotify)g_free,
	                              (GDestroyNotify)g_free);
}

gchar const *
cpg_taggable_get_tag (CpgTaggable  *taggable,
                      gchar const  *tag)

{
	GHashTable *table;

	g_return_val_if_fail (CPG_IS_TAGGABLE (taggable), NULL);

	table = cpg_taggable_get_tag_table (taggable);

	return g_hash_table_lookup (table, tag);
}

gboolean
cpg_taggable_try_get_tag  (CpgTaggable  *taggable,
                           gchar const  *tag,
                           gchar const **value)
{
	GHashTable *table;

	g_return_val_if_fail (CPG_IS_TAGGABLE (taggable), FALSE);

	table = cpg_taggable_get_tag_table (taggable);

	return g_hash_table_lookup_extended (table, tag, NULL, (gpointer *)value);
}

/**
 * cpg_taggable_get_tag_table:
 * @taggable: A #CpgTaggable
 *
 * Get the tag hash table.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cpg_taggable_get_tag_table (CpgTaggable *taggable)
{
	g_return_val_if_fail (CPG_IS_TAGGABLE (taggable), NULL);

	return CPG_TAGGABLE_GET_INTERFACE (taggable)->get_tag_table (taggable);
}

static void
copy_tag (gchar const  *key,
          gchar const  *value,
          GHashTable   *ret)
{
	g_hash_table_insert (ret, g_strdup (key), g_strdup (value));
}

void
cpg_taggable_copy_to (CpgTaggable *taggable,
                      GHashTable  *tags)
{
	g_return_if_fail (CPG_IS_TAGGABLE (taggable));

	g_hash_table_foreach (cpg_taggable_get_tag_table (taggable),
	                      (GHFunc)copy_tag,
	                      tags);
}
