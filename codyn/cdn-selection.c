/*
 * cdn-selection.c
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

#include "cdn-selection.h"
#include "cdn-expansion.h"
#include "cdn-taggable.h"

#define CDN_SELECTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_SELECTION, CdnSelectionPrivate))

struct _CdnSelectionPrivate
{
	gpointer  object;

	GSList   *expansions;
	GHashTable *defines;

	gboolean copy_defines_on_write;

	GHashTable *tags;
};

static void cdn_taggable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnSelection,
                         cdn_selection,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_TAGGABLE,
                                                cdn_taggable_iface_init))

static GHashTable *
get_tag_table (CdnTaggable *taggable)
{
	return CDN_SELECTION (taggable)->priv->tags;
}

static void
cdn_taggable_iface_init (gpointer iface)
{
	/* Use default implementation */
	CdnTaggableInterface *taggable = iface;

	taggable->get_tag_table = get_tag_table;
}

static void
cdn_selection_finalize (GObject *object)
{
	CdnSelection *selection;

	selection = CDN_SELECTION (object);

	if (selection->priv->object)
	{
		g_object_unref (selection->priv->object);
	}

	g_slist_foreach (selection->priv->expansions, (GFunc)cdn_expansion_unref, NULL);
	g_slist_free (selection->priv->expansions);

	if (selection->priv->defines)
	{
		g_hash_table_unref (selection->priv->defines);
	}

	g_hash_table_destroy (selection->priv->tags);

	G_OBJECT_CLASS (cdn_selection_parent_class)->finalize (object);
}

static void
cdn_selection_class_init (CdnSelectionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_selection_finalize;

	g_type_class_add_private (object_class, sizeof (CdnSelectionPrivate));
}

static void
cdn_selection_init (CdnSelection *self)
{
	self->priv = CDN_SELECTION_GET_PRIVATE (self);

	self->priv->tags = cdn_taggable_create_table ();
}

static GSList *
copy_expansions (GSList *list)
{
	GSList *ret = NULL;

	while (list)
	{
		ret = g_slist_prepend (ret,
		                       cdn_expansion_copy (list->data));

		list = g_slist_next (list);
	}

	return g_slist_reverse (ret);
}

static void
copy_entry (gchar const  *key,
            CdnExpansion *value,
            GHashTable   *table)
{
	g_hash_table_insert (table,
	                     g_strdup (key),
	                     value ? cdn_expansion_copy (value) : cdn_expansion_new_one (""));
}

static GHashTable *
copy_defines (GHashTable *table)
{
	GHashTable *ret;

	ret = g_hash_table_new_full (g_str_hash,
	                             g_str_equal,
	                             (GDestroyNotify)g_free,
	                             (GDestroyNotify)cdn_expansion_unref);

	if (table)
	{
		g_hash_table_foreach (table, (GHFunc)copy_entry, ret);
	}

	return ret;
}

CdnSelection *
cdn_selection_new (gpointer    object,
                   GSList     *expansions,
                   GHashTable *defines)
{
	CdnSelection *ret;

	ret = g_object_new (CDN_TYPE_SELECTION,
	                    NULL);

	ret->priv->object = object ? g_object_ref (object) : NULL;
	ret->priv->expansions = copy_expansions (expansions);

	if (defines)
	{
		ret->priv->defines = g_hash_table_ref (defines);
		ret->priv->copy_defines_on_write = TRUE;
	}
	else
	{
		ret->priv->defines = copy_defines (NULL);
	}

	return ret;
}

CdnSelection *
cdn_selection_new_defines (gpointer    object,
                           GSList     *expansions,
                           GHashTable *defines,
                           gboolean    copy_defines)
{
	CdnSelection *ret;

	ret = cdn_selection_new (object, expansions, NULL);
	g_hash_table_destroy (ret->priv->defines);

	ret->priv->defines = g_hash_table_ref (defines);
	ret->priv->copy_defines_on_write = copy_defines;

	return ret;
}


/**
 * cdn_selection_copy:
 * @selection: A #CdnSelection
 *
 * Copy a selection.
 *
 * Returns: (transfer full): A #CdnSelection
 *
 **/
CdnSelection *
cdn_selection_copy (CdnSelection *selection)
{
	CdnSelection *ret;

	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	ret = cdn_selection_new (selection->priv->object,
	                         selection->priv->expansions,
	                         selection->priv->defines);

	cdn_taggable_copy_to (CDN_TAGGABLE (selection),
	                      ret->priv->tags);

	return ret;
}

/**
 * cdn_selection_copy_defines:
 * @selection: A #CdnSelection
 * @copy_defines: Whether to copy the defines
 *
 * Copy selection with defines.
 *
 * Returns: (transfer full): A #CdnSelection
 *
 **/
CdnSelection *
cdn_selection_copy_defines (CdnSelection *selection,
                            gboolean      copy_defines)
{
	CdnSelection *ret;

	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	ret = cdn_selection_new_defines (selection->priv->object,
	                                 selection->priv->expansions,
	                                 selection->priv->defines,
	                                 copy_defines);

	if (copy_defines)
	{
		selection->priv->copy_defines_on_write = TRUE;
	}

	return ret;
}

/**
 * cdn_selection_get_object:
 * @selection: A #CdnSelection
 *
 * Get the object being selected.
 *
 * Returns: (transfer none): The object being selected
 *
 **/
gpointer
cdn_selection_get_object (CdnSelection *selection)
{
	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	return selection->priv->object;
}

void
cdn_selection_set_object (CdnSelection *selection,
                          gpointer      object)
{
	g_return_if_fail (CDN_IS_SELECTION (selection));
	g_return_if_fail (object == NULL || G_IS_OBJECT (object));

	if (selection->priv->object)
	{
		g_object_unref (selection->priv->object);
		selection->priv->object = NULL;
	}

	if (object)
	{
		selection->priv->object = g_object_ref (object);
	}
}

/**
 * cdn_selection_get_expansions:
 * @selection: A #CdnSelection
 *
 * Get the list of expansions.
 *
 * Returns: (element-type CdnExpansion) (transfer none): A #GSList of #CdnExpansion
 *
 **/
GSList *
cdn_selection_get_expansions (CdnSelection *selection)
{
	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	return selection->priv->expansions;
}

static void
copy_defines_on_write (CdnSelection *selection)
{
	GHashTable *nd;

	if (!selection->priv->copy_defines_on_write)
	{
		return;
	}

	nd = copy_defines (selection->priv->defines);
	g_hash_table_unref (selection->priv->defines);
	selection->priv->defines = nd;

	selection->priv->copy_defines_on_write = FALSE;
}

void
cdn_selection_add_define (CdnSelection *selection,
                          gchar const  *key,
                          CdnExpansion *value)
{
	g_return_if_fail (CDN_IS_SELECTION (selection));
	g_return_if_fail (value == NULL);

	copy_defines_on_write (selection);

	g_hash_table_insert (selection->priv->defines,
	                     g_strdup (key),
	                     value ? cdn_expansion_copy (value) : cdn_expansion_new_one (""));
}

/**
 * cdn_selection_get_defines:
 * @selection: A #CdnSelection
 *
 * Get the hash table of defines for the selection.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cdn_selection_get_defines (CdnSelection *selection)
{
	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	return selection->priv->defines;
}

CdnExpansion *
cdn_selection_get_define (CdnSelection *selection,
                          gchar const  *key)
{
	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	return g_hash_table_lookup (selection->priv->defines, key);
}
