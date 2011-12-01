/*
 * cpg-selection.c
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

#include "cpg-selection.h"
#include "cpg-expansion.h"
#include "cpg-taggable.h"

#define CPG_SELECTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_SELECTION, CpgSelectionPrivate))

struct _CpgSelectionPrivate
{
	gpointer  object;

	GSList   *expansions;
	GHashTable *defines;

	gboolean copy_defines_on_write;

	GHashTable *tags;
};

static void cpg_taggable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgSelection,
                         cpg_selection,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_TAGGABLE,
                                                cpg_taggable_iface_init))

static GHashTable *
get_tag_table (CpgTaggable *taggable)
{
	return CPG_SELECTION (taggable)->priv->tags;
}

static void
cpg_taggable_iface_init (gpointer iface)
{
	/* Use default implementation */
	CpgTaggableInterface *taggable = iface;

	taggable->get_tag_table = get_tag_table;
}

static void
cpg_selection_finalize (GObject *object)
{
	CpgSelection *selection;

	selection = CPG_SELECTION (object);

	if (selection->priv->object)
	{
		g_object_unref (selection->priv->object);
	}

	g_slist_foreach (selection->priv->expansions, (GFunc)g_object_unref, NULL);
	g_slist_free (selection->priv->expansions);

	if (selection->priv->defines)
	{
		g_hash_table_unref (selection->priv->defines);
	}

	g_hash_table_destroy (selection->priv->tags);

	G_OBJECT_CLASS (cpg_selection_parent_class)->finalize (object);
}

static void
cpg_selection_class_init (CpgSelectionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_selection_finalize;

	g_type_class_add_private (object_class, sizeof (CpgSelectionPrivate));
}

static void
cpg_selection_init (CpgSelection *self)
{
	self->priv = CPG_SELECTION_GET_PRIVATE (self);

	self->priv->tags = cpg_taggable_create_table ();
}

static GSList *
copy_expansions (GSList *list)
{
	GSList *ret = NULL;

	while (list)
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_copy (list->data));

		list = g_slist_next (list);
	}

	return g_slist_reverse (ret);
}

static void
copy_entry (gchar const  *key,
            CpgExpansion *value,
            GHashTable   *table)
{
	g_hash_table_insert (table,
	                     g_strdup (key),
	                     value ? cpg_expansion_copy (value) : cpg_expansion_new_one (""));
}

static GHashTable *
copy_defines (GHashTable *table)
{
	GHashTable *ret;

	ret = g_hash_table_new_full (g_str_hash,
	                             g_str_equal,
	                             (GDestroyNotify)g_free,
	                             (GDestroyNotify)g_object_unref);

	if (table)
	{
		g_hash_table_foreach (table, (GHFunc)copy_entry, ret);
	}

	return ret;
}

CpgSelection *
cpg_selection_new (gpointer    object,
                   GSList     *expansions,
                   GHashTable *defines)
{
	CpgSelection *ret;

	ret = g_object_new (CPG_TYPE_SELECTION,
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

CpgSelection *
cpg_selection_new_defines (gpointer    object,
                           GSList     *expansions,
                           GHashTable *defines,
                           gboolean    copy_defines)
{
	CpgSelection *ret;

	ret = cpg_selection_new (object, expansions, NULL);
	g_hash_table_destroy (ret->priv->defines);

	ret->priv->defines = g_hash_table_ref (defines);
	ret->priv->copy_defines_on_write = copy_defines;

	return ret;
}


/**
 * cpg_selection_copy:
 * @selection: A #CpgSelection
 *
 * Copy a selection.
 *
 * Returns: (transfer full): A #CpgSelection
 *
 **/
CpgSelection *
cpg_selection_copy (CpgSelection *selection)
{
	CpgSelection *ret;

	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	ret = cpg_selection_new (selection->priv->object,
	                         selection->priv->expansions,
	                         selection->priv->defines);

	cpg_taggable_copy_to (CPG_TAGGABLE (selection),
	                      ret->priv->tags);

	return ret;
}

/**
 * cpg_selection_copy_defines:
 * @selection: A #CpgSelection
 * @copy_defines: Whether to copy the defines
 *
 * Copy selection with defines.
 *
 * Returns: (transfer full): A #CpgSelection
 *
 **/
CpgSelection *
cpg_selection_copy_defines (CpgSelection *selection,
                            gboolean      copy_defines)
{
	CpgSelection *ret;

	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	ret = cpg_selection_new_defines (selection->priv->object,
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
 * cpg_selection_get_object:
 * @selection: A #CpgSelection
 *
 * Get the object being selected.
 *
 * Returns: (transfer none): The object being selected
 *
 **/
gpointer
cpg_selection_get_object (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return selection->priv->object;
}

void
cpg_selection_set_object (CpgSelection *selection,
                          gpointer      object)
{
	g_return_if_fail (CPG_IS_SELECTION (selection));
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
 * cpg_selection_get_expansions:
 * @selection: A #CpgSelection
 *
 * Get the list of expansions.
 *
 * Returns: (element-type CpgExpansion) (transfer none): A #GSList of #CpgExpansion
 *
 **/
GSList *
cpg_selection_get_expansions (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return selection->priv->expansions;
}

static void
copy_defines_on_write (CpgSelection *selection)
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
cpg_selection_add_define (CpgSelection *selection,
                          gchar const  *key,
                          CpgExpansion *value)
{
	g_return_if_fail (CPG_IS_SELECTION (selection));
	g_return_if_fail (value == NULL || CPG_IS_EXPANSION (value));

	copy_defines_on_write (selection);

	g_hash_table_insert (selection->priv->defines,
	                     g_strdup (key),
	                     value ? cpg_expansion_copy (value) : cpg_expansion_new_one (""));
}

/**
 * cpg_selection_get_defines:
 * @selection: A #CpgSelection
 *
 * Get the hash table of defines for the selection.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cpg_selection_get_defines (CpgSelection *selection)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return selection->priv->defines;
}

CpgExpansion *
cpg_selection_get_define (CpgSelection *selection,
                          gchar const  *key)
{
	g_return_val_if_fail (CPG_IS_SELECTION (selection), NULL);

	return g_hash_table_lookup (selection->priv->defines, key);
}
