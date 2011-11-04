/*
 * cpg-expansion.c
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

#include "cpg-expansion.h"

#define CPG_EXPANSION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EXPANSION, CpgExpansionPrivate))

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

struct _CpgExpansionPrivate
{
	GPtrArray *expansions;
};

G_DEFINE_TYPE (CpgExpansion, cpg_expansion, G_TYPE_OBJECT)

static void
cpg_expansion_finalize (GObject *object)
{
	CpgExpansion *expansion;

	expansion = CPG_EXPANSION (object);

	g_ptr_array_free (expansion->priv->expansions, TRUE);

	G_OBJECT_CLASS (cpg_expansion_parent_class)->finalize (object);
}

static void
cpg_expansion_class_init (CpgExpansionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_expansion_finalize;

	g_type_class_add_private (object_class, sizeof (CpgExpansionPrivate));
}

static void
cpg_expansion_init (CpgExpansion *self)
{
	self->priv = CPG_EXPANSION_GET_PRIVATE (self);
}

CpgExpansion *
cpg_expansion_new_one (gchar const *item)
{
	gchar const *items[] = {
		item,
		NULL
	};

	return cpg_expansion_new (items);
}

CpgExpansion *
cpg_expansion_new (gchar const * const *items)
{
	CpgExpansion *ret;

	ret = g_object_new (CPG_TYPE_EXPANSION, NULL);

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
cpg_expansion_num (CpgExpansion *id)
{
	g_return_val_if_fail (CPG_IS_EXPANSION (id), 0);

	return id->priv->expansions->len;
}

static Expansion *
get_ex (CpgExpansion *id, gint idx)
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

gchar const *
cpg_expansion_get (CpgExpansion *id,
                   gint          idx)
{
	Expansion *ex;

	g_return_val_if_fail (CPG_IS_EXPANSION (id), NULL);

	ex = get_ex (id, idx);

	return ex ? ex->text : NULL;
}

gint
cpg_expansion_get_index (CpgExpansion *id,
                         gint          idx)
{
	Expansion *ex;

	g_return_val_if_fail (CPG_IS_EXPANSION (id), 0);

	ex = get_ex (id, idx);

	return ex ? ex->idx : 0;
}

void
cpg_expansion_set_index (CpgExpansion *id,
                         gint          idx,
                         gint          val)
{
	Expansion *ex;

	g_return_if_fail (CPG_IS_EXPANSION (id));

	ex = get_ex (id, idx);

	if (ex)
	{
		ex->idx = val;
	}
}

void
cpg_expansion_set (CpgExpansion *id,
                   gint          idx,
                   gchar const  *val)
{
	Expansion *ex;

	g_return_if_fail (CPG_IS_EXPANSION (id));

	ex = get_ex (id, idx);

	if (ex)
	{
		g_free (ex->text);
		ex->text = g_strdup (val);
	}
}

static Expansion *
expansion_copy (Expansion *ex)
{
	Expansion *ret;

	ret = expansion_new (ex->text);
	ret->idx = ex->idx;

	return ret;
}

/**
 * cpg_expansion_copy:
 * @id: A #CpgExpansion
 *
 * Copy an expansion.
 *
 * Returns: (transfer full): A #CpgExpansion
 *
 **/
CpgExpansion *
cpg_expansion_copy (CpgExpansion *id)
{
	CpgExpansion *ret;
	GPtrArray *ptr;
	gint i;

	g_return_val_if_fail (id == NULL || CPG_IS_EXPANSION (id), NULL);

	if (id == NULL)
	{
		return NULL;
	}

	ptr = g_ptr_array_sized_new (cpg_expansion_num (id) + 1);
	g_ptr_array_set_free_func (ptr, (GDestroyNotify)expansion_free);

	for (i = 0; i < cpg_expansion_num (id); ++i)
	{
		g_ptr_array_add (ptr,
		                 expansion_copy (get_ex (id, i)));
	}

	ret = g_object_new (CPG_TYPE_EXPANSION, NULL);
	ret->priv->expansions = ptr;

	return ret;
}

void
cpg_expansion_add (CpgExpansion *id,
                   gchar const  *item)
{
	g_return_if_fail (CPG_IS_EXPANSION (id));
	g_return_if_fail (item != NULL);

	g_ptr_array_add (id->priv->expansions,
	                 expansion_new (item));
}

void
cpg_expansion_insert (CpgExpansion *id,
                      gint          idx,
                      gchar const  *item)
{
	gint n;
	gint i;

	g_return_if_fail (CPG_IS_EXPANSION (id));

	g_ptr_array_add (id->priv->expansions,
	                 expansion_new (NULL));

	n = cpg_expansion_num (id);

	for (i = n - 1; i > idx; --i)
	{
		Expansion *e1 = get_ex (id, i);
		Expansion *e2 = get_ex (id, i - 1);

		g_free (e1->text);

		e1->text = g_strdup (e2->text);
		e1->idx = e2->idx;
	}

	cpg_expansion_set (id, idx, item);
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
		CpgExpansion *ex;
		Expansion *e;

		ex = expansions->data;
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
 * cpg_expansions_annotate_indices:
 * @expansions: (element-type CpgExpansion): expansions
 * @start: the start
 *
 **/
void
cpg_expansions_annotate_indices (GSList *expansions,
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
cpg_expansion_append (CpgExpansion *id,
                      CpgExpansion *other,
                      gint          idx)
{
	gint i;

	g_return_if_fail (CPG_IS_EXPANSION (id));
	g_return_if_fail (CPG_IS_EXPANSION (other));

	for (i = idx; i < cpg_expansion_num (other); ++i)
	{
		cpg_expansion_add (id,
		                   cpg_expansion_get (other, i));

		cpg_expansion_set_index (id,
		                         cpg_expansion_num (id) - 1,
		                         cpg_expansion_get_index (other, i));
	}
}
