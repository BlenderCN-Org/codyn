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
#include <glib/gprintf.h>

G_DEFINE_BOXED_TYPE (CdnExpansion,
                     cdn_expansion,
                     cdn_expansion_ref,
                     cdn_expansion_unref)

typedef struct
{
	gchar *text;
	gint idx;
	gint ref_count;
} Expansion;

static Expansion *
expansion_new (gchar const *text)
{
	Expansion *ret;

	ret = g_slice_new0 (Expansion);
	ret->text = g_strdup (text);
	ret->ref_count = 1;

	return ret;
}

static Expansion *
expansion_ref (Expansion *self)
{
	if (!self)
	{
		return NULL;
	}

	g_atomic_int_inc (&(self->ref_count));
	return self;
}

static void
expansion_unref (Expansion *self)
{
	if (!self)
	{
		return;
	}

	if (g_atomic_int_dec_and_test (&(self->ref_count)))
	{
		g_free (self->text);
		g_slice_free (Expansion, self);
	}
}

struct _CdnExpansion
{
	union
	{
		GPtrArray *expansions;
		Expansion *text;
	};

	gint ref_count;

	guint copy_on_write : 1;
	guint is_one : 1;
};

void
cdn_expansion_unref (CdnExpansion *expansion)
{
	if (!expansion || !g_atomic_int_dec_and_test (&(expansion->ref_count)))
	{
		return;
	}

	if (expansion->is_one)
	{
		expansion_unref (expansion->text);
	}
	else
	{
		g_ptr_array_unref (expansion->expansions);
	}

	g_slice_free (CdnExpansion, expansion);
}

CdnExpansion *
cdn_expansion_ref (CdnExpansion *expansion)
{
	if (!expansion)
	{
		return expansion;
	}

	g_atomic_int_inc (&(expansion->ref_count));
	return expansion;
}

static CdnExpansion *
cdn_expansion_create ()
{
	CdnExpansion *self;

	self = g_slice_new0 (CdnExpansion);
	self->ref_count = 1;

	return self;
}

static Expansion *
expansion_copy (Expansion *ex)
{
	return expansion_ref (ex);
}

static Expansion *
expansion_copy_on_write (Expansion *ex)
{
	Expansion *ret;

	if (ex->ref_count == 1)
	{
		return ex;
	}

	expansion_unref (ex);

	ret = expansion_new (ex->text);
	ret->idx = ex->idx;

	return ret;
}

static Expansion *
get_ex_real (CdnExpansion *id,
             gint          idx,
             gboolean      copy_on_write)
{
	gint num;

	num = cdn_expansion_num (id);

	if (idx <= -(gint)num || idx >= num)
	{
		return NULL;
	}

	if (idx < 0)
	{
		idx = (gint)num + idx;
	}

	if (id->is_one)
	{
		if (copy_on_write)
		{
			id->text = expansion_copy_on_write (id->text);
		}

		return id->text;
	}
	else
	{
		if (copy_on_write)
		{
			id->expansions->pdata[idx] = expansion_copy_on_write (g_ptr_array_index (id->expansions, idx));
		}

		return g_ptr_array_index (id->expansions, idx);
	}
}

static Expansion *
get_ex (CdnExpansion *id,
        gint          idx)
{
	return get_ex_real (id, idx, FALSE);
}

static gboolean
copy_on_write_sized (CdnExpansion *expansion,
                     gboolean      make_multi,
                     gint          num)
{
	GPtrArray *ptr;
	gint i;

	if (!expansion->copy_on_write)
	{
		if ((make_multi || num > 0) && expansion->is_one)
		{
			ptr = g_ptr_array_sized_new (num + 1);
			g_ptr_array_set_size (ptr, num + 1);

			g_ptr_array_set_free_func (ptr, (GDestroyNotify)expansion_unref);

			ptr->pdata[0] = expansion_copy (expansion->text);

			expansion_unref (expansion->text);

			expansion->is_one = FALSE;
			expansion->expansions = ptr;
		}
		else if (num > 0)
		{
			g_ptr_array_set_size (expansion->expansions,
			                      expansion->expansions->len + num);
		}

		return FALSE;
	}

	if (!expansion->is_one || make_multi)
	{
		gint newsize = cdn_expansion_num (expansion) + num;

		ptr = g_ptr_array_sized_new (newsize);
		g_ptr_array_set_free_func (ptr, (GDestroyNotify)expansion_unref);

		for (i = 0; i < cdn_expansion_num (expansion); ++i)
		{
			g_ptr_array_add (ptr,
			                 expansion_copy (get_ex (expansion, i)));
		}

		g_ptr_array_set_size (ptr, newsize);

		if (!expansion->is_one)
		{
			g_ptr_array_unref (expansion->expansions);
		}
		else
		{
			expansion_unref (expansion->text);
		}

		expansion->expansions = ptr;
		expansion->is_one = FALSE;
	}
	else
	{
		if (expansion->text->ref_count > 1)
		{
			expansion_unref (expansion->text);
			expansion->text = expansion_copy (expansion->text);
		}
	}

	expansion->copy_on_write = FALSE;
	return TRUE;
}

static gboolean
copy_on_write (CdnExpansion *expansion,
               gboolean      make_multi)
{
	return copy_on_write_sized (expansion, make_multi, 0);
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

	ret = cdn_expansion_create ();

	ret->text = expansion_new (item);
	ret->is_one = TRUE;

	return ret;
}

CdnExpansion *
cdn_expansion_new (gchar const * const *items)
{
	return cdn_expansion_new_sized (items, 2);
}

CdnExpansion *
cdn_expansion_new_sized (gchar const * const *items,
                         gint                 sized)
{
	CdnExpansion *ret;

	ret = cdn_expansion_create ();

	ret->expansions = g_ptr_array_sized_new (sized);

	g_ptr_array_set_free_func (ret->expansions,
	                           (GDestroyNotify)expansion_unref);

	while (items && *items)
	{
		g_ptr_array_add (ret->expansions,
		                 expansion_new (*items));
		++items;
	}

	if (ret->expansions->len == 0)
	{
		g_ptr_array_add (ret->expansions,
		                 expansion_new (""));
	}

	return ret;

}

gint
cdn_expansion_num (CdnExpansion *id)
{
	return id->is_one ? 1 : id->expansions->len;
}

gchar const *
cdn_expansion_get (CdnExpansion *id,
                   gint          idx)
{
	Expansion *ex;

	ex = get_ex (id, idx);

	return ex ? ex->text : NULL;
}

gint
cdn_expansion_get_index (CdnExpansion *id,
                         gint          idx)
{
	Expansion *ex;

	ex = get_ex (id, idx);

	return ex ? ex->idx : 0;
}

void
cdn_expansion_set_index (CdnExpansion *id,
                         gint          idx,
                         gint          val)
{
	Expansion *ex;

	ex = get_ex (id, idx);

	if (ex)
	{
		if (copy_on_write (id, FALSE))
		{
			ex = get_ex (id, idx);
		}

		ex->idx = val;
	}
}

void
cdn_expansion_set (CdnExpansion *id,
                   gint          idx,
                   gchar const  *val)
{
	Expansion *ex;

	ex = get_ex (id, idx);

	if (ex)
	{
		copy_on_write (id, FALSE);
		ex = get_ex_real (id, idx, TRUE);

		if (id->is_one)
		{
			ex = id->text;
		}
		else
		{
			ex = id->expansions->pdata[idx];
		}

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

	if (id == NULL)
	{
		return NULL;
	}

	ret = cdn_expansion_create ();

	if (id->is_one)
	{
		ret->is_one = TRUE;
		ret->text = expansion_ref (id->text);
	}
	else
	{
		gint i;

		ret->expansions = g_ptr_array_ref (id->expansions);

		for (i = 0; i < cdn_expansion_num (id); ++i)
		{
			expansion_ref (get_ex (id, i));
		}
	}

	ret->copy_on_write = TRUE;

	return ret;
}

void
cdn_expansion_add (CdnExpansion *id,
                   gchar const  *item)
{
	copy_on_write (id, TRUE);

	g_ptr_array_add (id->expansions,
	                 expansion_new (item));
}

void
cdn_expansion_insert (CdnExpansion *id,
                      gint          idx,
                      gchar const  *item)
{
	gint n;
	gint i;

	copy_on_write_sized (id, TRUE, 1);
	n = cdn_expansion_num (id);

	for (i = n - 1; i > idx; --i)
	{
		id->expansions->pdata[i] = id->expansions->pdata[i - 1];
	}

	id->expansions->pdata[idx] = expansion_new (item);
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
		copy_on_write (ex, FALSE);

		e = get_ex_real (ex, i, TRUE);

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
	gint onum;
	gint oldsize;

	onum = cdn_expansion_num (other);

	if (onum <= idx)
	{
		return;
	}

	oldsize = cdn_expansion_num (id);

	copy_on_write_sized (id, TRUE, onum - idx);

	for (i = idx; i < onum; ++i)
	{
		Expansion *ex;

		ex = expansion_copy (get_ex (other, i));
		id->expansions->pdata[oldsize + i - idx] = ex;
	}
}

void
cdn_expansion_prepend (CdnExpansion *id,
                       CdnExpansion *other,
                       gint          idx)
{
	gint i;
	gint onum;
	gint num;
	gint newsize;
	gint oldsize;

	onum = cdn_expansion_num (other);

	if (onum <= idx)
	{
		return;
	}

	num = onum - idx;

	oldsize = cdn_expansion_num (id);
	newsize = oldsize + num;

	copy_on_write_sized (id, TRUE, num);

	// move data
	for (i = 0; i < oldsize - 1; ++i)
	{
		gint end = newsize - i - 1;

		id->expansions->pdata[end] =
			id->expansions->pdata[end - num];
	}

	for (i = idx; i < onum; ++i)
	{
		gint pidx = i - idx + 1;

		id->expansions->pdata[pidx] = expansion_copy (get_ex (other, i));
	}
}

void
cdn_expansion_debug_print (CdnExpansion *expansion,
                           FILE         *file)
{
	gint i;

	g_return_if_fail (expansion != NULL);
	g_return_if_fail (file != NULL);

	g_fprintf (file, "[");

	for (i = 0; i < cdn_expansion_num (expansion); ++i)
	{
		if (i != 0)
		{
			g_fprintf (file, ", ");
		}

		g_fprintf (file,
		           "%s:%d",
		           cdn_expansion_get (expansion, i),
		           cdn_expansion_get_index (expansion, i));
	}

	g_fprintf (file, "]");
}
