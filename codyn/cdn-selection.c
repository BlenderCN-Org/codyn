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
	CdnExpansionContext *context;
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

	if (selection->priv->context)
	{
		cdn_expansion_context_unref (selection->priv->context);
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

/**
 * cdn_selection_new:
 * @object: The object
 * @context: (transfer none): The expansion context
 *
 * Create a new selection.
 *
 * Returns: A #CdnSelection
 *
 **/
CdnSelection *
cdn_selection_new (gpointer             object,
                   CdnExpansionContext *context)
{
	CdnSelection *ret;

	ret = g_object_new (CDN_TYPE_SELECTION, NULL);

	ret->priv->object = object ? g_object_ref (object) : NULL;
	ret->priv->context = cdn_expansion_context_ref (context);

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
	                         cdn_expansion_context_new_unreffed (selection->priv->context));

	cdn_taggable_copy_to (CDN_TAGGABLE (selection),
	                      ret->priv->tags);

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
 * cdn_selection_get_context:
 * @selection: A #CdnSelection
 *
 * Get the selection context.
 *
 * Returns: (transfer none): a #CdnExpansionContext
 *
 **/
CdnExpansionContext *
cdn_selection_get_context (CdnSelection *selection)
{
	g_return_val_if_fail (CDN_IS_SELECTION (selection), NULL);

	return selection->priv->context;
}
