/*
 * cdn-expansion.c
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

#include "cdn-expansion.h"

#define CDN_EXPANSION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EXPANSION, CdnExpansionPrivate))

typedef struct
{
	gchar *text;
	gint idx;
} Expansion;

static Expansion *
expansion_new (gchar const *text)
{
	Expansion *ret;

	ret = g_slice_new0 (Expansion);
	ret->text = g_strdup (text);

	return ret;
}

static void
expansion_free (Expansion *self)
{
	g_free (self->text);
	g_slice_free (Expansion, self);
}

struct _CdnExpansionPrivate
{
	GPtrArray *expansions;
	gboolean copy_on_write;
};

G_DEFINE_TYPE (CdnExpansion, cdn_expansion, G_TYPE_OBJECT)

static void
cdn_expansion_finalize (GObject *object)
{
	CdnExpansion *expansion;

	expansion = CDN_EXPANSION (object);

	g_ptr_array_unref (expansion->priv->expansions);

	G_OBJECT_CLASS (cdn_expansion_parent_class)->finalize (object);
}

static void
cdn_expansion_class_init (CdnExpansionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_expansion_finalize;

	g_type_class_add_private (object_class, sizeof (CdnExpansionPrivate));
}

static void
cdn_expansion_init (CdnExpansion *self)
{
	self->priv = CDN_EXPANSION_GET_PRIVATE (self);
}

static Expansion *
expansion_copy (Expansion *ex)
{
	Expansion *ret;

	ret = expansion_new (ex->text);
	ret->idx = ex->idx;

	return ret;
}

static Expansion *
get_ex (CdnExpansion *id, gint idx)
{
	if (idx <= -(gint)id->priv->expansions->len || idx >= id->priv->expansions->len)
	{
		return NULL;
	}

	if (idx < 0)
	{
		idx = (gint)id->priv->expansions->len + idx;
	}

	return g_ptr_array_index (id->priv->expansions, idx);
}

static void
copy_on_write (CdnExpansion *expansion)
{
	GPtrArray *ptr = g_ptr_array_new ();
	gint i;

	if (!expansion->priv->copy_on_write)
	{
		return;
	}

	ptr = g_ptr_array_sized_new (cdn_expansion_num (expansion));
	g_ptr_array_set_free_func (ptr, (GDestroyNotify)expansion_free);

	for (i = 0; i < cdn_expansion_num (expansion); ++i)
	{
		g_ptr_array_add (ptr,
		                 expansion_copy (get_ex (expansion, i)));
	}

	g_ptr_array_unref (expansion->priv->expansions);

	expansion->priv->expansions = ptr;
	expansion->priv->copy_on_write = FALSE;
}

CdnExpansion *
cdn_expansion_newv (gchar const *item,
                    ...)
{
	GPtrArray *ptr;
	va_list ap;
	gchar **d;
	CdnExpansion *ret;

	ptr = g_ptr_array_new ();
	g_ptr_array_add (ptr, (gpointer)item);

	va_start (ap, item);

	while ((item = va_arg (ap, gchar const *)))
	{
		g_ptr_array_add (ptr, (gpointer)item);
	}

	g_ptr_array_add (ptr, NULL);
	d = (gchar **)g_ptr_array_free (ptr, FALSE);

	ret = cdn_expansion_new ((gchar const * const *)d);
	g_free (d);

	return ret;
}

CdnExpansion *
cdn_expansion_new_one (gchar const *item)
{
	CdnExpansion *ret;

	ret = g_object_new (CDN_TYPE_EXPANSION, NULL);
	ret->priv->expansions = g_ptr_array_sized_new (1);
	g_ptr_array_set_free_func (ret->priv->expansions, (GDestroyNotify)expansion_free);

	g_ptr_array_add (ret->priv->expansions, expansion_new (item));
	return ret;
}

CdnExpansion *
cdn_expansion_new (gchar const * const *items)
{
	CdnExpansion *ret;

	ret = g_object_new (CDN_TYPE_EXPANSION, NULL);

	ret->priv->expansions =
		g_ptr_array_new_with_free_func ((GDestroyNotify)expansion_free);

	while (items && *items)
	{
		g_ptr_array_add (ret->priv->expansions,
		                 expansion_new (*items));
		++items;
	}

	if (ret->priv->expansions->len == 0)
	{
		g_ptr_array_add (ret->priv->expansions,
		                 expansion_new (""));
	}

	return ret;
}

gint
cdn_expansion_num (CdnExpansion *id)
{
	g_return_val_if_fail (CDN_IS_EXPANSION (id), 0);

	return id->priv->expansions->len;
}

gchar const *
cdn_expansion_get (CdnExpansion *id,
                   gint          idx)
{
	Expansion *ex;

	g_return_val_if_fail (CDN_IS_EXPANSION (id), NULL);

	ex = get_ex (id, idx);

	return ex ? ex->text : NULL;
}

gint
cdn_expansion_get_index (CdnExpansion *id,
                         gint          idx)
{
	Expansion *ex;

	g_return_val_if_fail (CDN_IS_EXPANSION (id), 0);

	ex = get_ex (id, idx);

	return ex ? ex->idx : 0;
}

void
cdn_expansion_set_index (CdnExpansion *id,
                         gint          idx,
                         gint          val)
{
	Expansion *ex;

	g_return_if_fail (CDN_IS_EXPANSION (id));

	copy_on_write (id);

	ex = get_ex (id, idx);

	if (ex)
	{
		ex->idx = val;
	}
}

void
cdn_expansion_set (CdnExpansion *id,
                   gint          idx,
                   gchar const  *val)
{
	Expansion *ex;

	g_return_if_fail (CDN_IS_EXPANSION (id));

	copy_on_write (id);

	ex = get_ex (id, idx);

	if (ex)
	{
		g_free (ex->text);
		ex->text = g_strdup (val);
	}
}

/**
 * cdn_expansion_copy:
 * @id: A #CdnExpansion
 *
 * Copy an expansion.
 *
 * Returns: (transfer full): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_expansion_copy (CdnExpansion *id)
{
	CdnExpansion *ret;

	g_return_val_if_fail (id == NULL || CDN_IS_EXPANSION (id), NULL);

	if (id == NULL)
	{
		return NULL;
	}

	ret = g_object_new (CDN_TYPE_EXPANSION, NULL);
	ret->priv->expansions = g_ptr_array_ref (id->priv->expansions);

	ret->priv->copy_on_write = TRUE;

	return ret;
}

void
cdn_expansion_add (CdnExpansion *id,
                   gchar const  *item)
{
	g_return_if_fail (CDN_IS_EXPANSION (id));
	g_return_if_fail (item != NULL);

	copy_on_write (id);
	g_ptr_array_add (id->priv->expansions,
	                 expansion_new (item));
}

void
cdn_expansion_insert (CdnExpansion *id,
                      gint          idx,
                      gchar const  *item)
{
	gint n;
	gint i;

	g_return_if_fail (CDN_IS_EXPANSION (id));

	copy_on_write (id);

	g_ptr_array_add (id->priv->expansions,
	                 expansion_new (NULL));

	n = cdn_expansion_num (id);

	for (i = n - 1; i > idx; --i)
	{
		Expansion *e1 = get_ex (id, i);
		Expansion *e2 = get_ex (id, i - 1);

		g_free (e1->text);

		e1->text = g_strdup (e2->text);
		e1->idx = e2->idx;
	}

	cdn_expansion_set (id, idx, item);
}

static gboolean
annotate_group (GSList *expansions,
                gint    i)
{
	GHashTable *h;
	gboolean ret = FALSE;
	gint curidx = 0;

	h = g_hash_table_new (g_str_hash,
	                      g_str_equal);

	while (expansions)
	{
		CdnExpansion *ex;
		Expansion *e;

		ex = expansions->data;
		copy_on_write (ex);

		e = get_ex (ex, i);

		if (e)
		{
			gpointer key;
			gpointer val;

			if (g_hash_table_lookup_extended (h, e->text, &key, &val))
			{
				e->idx = GPOINTER_TO_INT (val);
			}
			else
			{
				e->idx = curidx++;
				g_hash_table_insert (h, e->text, GINT_TO_POINTER (e->idx));
			}

			ret = TRUE;
		}

		expansions = g_slist_next (expansions);
	}

	g_hash_table_destroy (h);
	return ret;
}

/**
 * cdn_expansions_annotate_indices:
 * @expansions: (element-type CdnExpansion): expansions
 * @start: the start
 *
 **/
void
cdn_expansions_annotate_indices (GSList *expansions,
                                 gint    start)
{
	gint i = start;

	while (TRUE)
	{
		if (!annotate_group (expansions, i))
		{
			break;
		}

		++i;
	}
}

void
cdn_expansion_append (CdnExpansion *id,
                      CdnExpansion *other,
                      gint          idx)
{
	gint i;

	g_return_if_fail (CDN_IS_EXPANSION (id));
	g_return_if_fail (CDN_IS_EXPANSION (other));

	copy_on_write (id);

	for (i = idx; i < cdn_expansion_num (other); ++i)
	{
		cdn_expansion_add (id,
		                   cdn_expansion_get (other, i));

		cdn_expansion_set_index (id,
		                         cdn_expansion_num (id) - 1,
		                         cdn_expansion_get_index (other, i));
	}
}
