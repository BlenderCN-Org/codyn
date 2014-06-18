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

/**
 * CdnExpansion:
 *
 * Expansion class.
 *
 * The #CdnExpansion boxed class, is a simple object which represents a single level
 * of expansion, i.e. something which can be referred to in the codyn modeling language
 * by @N where N is a number. The #CdnExpansion stores an ordered list of strings, where
 * each string can be associated by an index. Normally, a #CdnExpansion is created as
 * a product of generators and selectors, i.e. they represent a group of expansions.
 *
 */

GType
cdn_expansion_get_type (void)
{
	static volatile gsize g_define_type_id__volatile = 0;

	if (g_once_init_enter (&g_define_type_id__volatile))
	{
		GType g_define_type_id;

		g_define_type_id =
			g_boxed_type_register_static (g_intern_static_string ("CdnExpansion"),
			                              (GBoxedCopyFunc)cdn_expansion_ref,
			                              (GBoxedFreeFunc)cdn_expansion_unref);

		g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
	}

	return g_define_type_id__volatile;
}

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

/**
 * cdn_expansion_unref:
 * @expansion: the #CdnExpansion
 *
 * Decrease the reference count of @expansion. If the reference count
 * reaches 0, then the memory associated with @expansion will be freed.
 *
 */
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

/**
 * cdn_expansion_ref:
 * @expansion: the #CdnExpansion
 *
 * Increase the reference count of @expansion.
 *
 * Returns: (transfer none): @expansion
 */
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

/**
 * cdn_expansion_newv:
 * @item: the first item to add
 *
 * Create a new #CdnExpansion from a variadic list of items.
 * The provided must be %NULL terminated. Make sure to use
 * #cdn_expansion_unref to free the newly created expansion
 * when done.
 *
 * Returns: (transfer full): a new #CdnExpansion.
 *
 */
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

/**
 * cdn_expansion_new_one:
 * @item: an item
 *
 * Create a new #CdnExpansion with a single item.
 *
 * Returns: (transfer full): the new #CdnExpansion
 *
 */
CdnExpansion *
cdn_expansion_new_one (gchar const *item)
{
	CdnExpansion *ret;

	ret = cdn_expansion_create ();

	ret->text = expansion_new (item);
	ret->is_one = TRUE;

	return ret;
}

/**
 * cdn_expansion_new:
 * @items: a %NULL terminated list of items
 *
 * Create a new #CdnExpansion from a list of items. The provided
 * @items must be %NULL terminated.
 *
 * Returns: (transfer full): the new #CdnExpansion
 *
 */
CdnExpansion *
cdn_expansion_new (gchar const * const *items)
{
	return cdn_expansion_new_sized (items, 2);
}

/**
 * cdn_expansion_new_sized:
 * @items: a %NULL terminated list of items
 * @sized: the number of items to reserve space for
 *
 * Create a new expansion with an initial capacity of @sized. Note
 * that @items must still be %NULL terminated, @sized only indicates
 * an initial capacity for the number of items in the expansion.
 *
 * If @items is empty, an empty item will be automatically appended
 * to the expansion.
 *
 * Returns: (transfer full): the new #CdnExpansion
 *
 */
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

/**
 * cdn_expansion_num:
 * @expansion: the #CdnExpansion
 *
 * Get the number of items in the expansion.
 *
 * Returns: the number of items in @expansion
 *
 */
gint
cdn_expansion_num (CdnExpansion *expansion)
{
	return expansion->is_one ? 1 : expansion->expansions->len;
}

/**
 * cdn_expansion_get:
 * @expansion: the #CdnExpansion
 * @idx: the index
 *
 * Get a single item from the expansion at index @idx. If @idx is larger
 * or equal than the number of items in the expansion, then this function
 * returns %NULL.
 *
 * Returns: the item at @idx or %NULL.
 *
 */
gchar const *
cdn_expansion_get (CdnExpansion *expansion,
                   gint          idx)
{
	Expansion *ex;

	ex = get_ex (expansion, idx);

	return ex ? ex->text : NULL;
}

/**
 * cdn_expansion_get_index:
 * @expansion: the #CdnExpansion
 * @idx: the index
 *
 * Get a single item index from the expansion at index @idx. If @idx is larger
 * or equal than the number of items in the expansion, then this function
 * returns 0.
 *
 * Returns: the item index at @idx or %NULL.
 *
 */
gint
cdn_expansion_get_index (CdnExpansion *expansion,
                         gint          idx)
{
	Expansion *ex;

	ex = get_ex (expansion, idx);

	return ex ? ex->idx : 0;
}

/**
 * cdn_expansion_set_index:
 * @expansion: the #CdnExpansion
 * @idx: the index
 * @val: the item index
 *
 * Set the index of a particular item. @idx indicates the item in the
 * list of items in the expansion, while @val represents the corresponding
 * item index.
 *
 */
void
cdn_expansion_set_index (CdnExpansion *expansion,
                         gint          idx,
                         gint          val)
{
	Expansion *ex;

	ex = get_ex (expansion, idx);

	if (ex)
	{
		if (copy_on_write (expansion, FALSE))
		{
			ex = get_ex (expansion, idx);
		}

		ex->idx = val;
	}
}

/**
 * cdn_expansion_set:
 * @expansion: the #CdnExpansion
 * @idx: the index
 * @val: the item
 *
 * Set the value of an item at the specified index.
 *
 */
void
cdn_expansion_set (CdnExpansion *expansion,
                   gint          idx,
                   gchar const  *val)
{
	Expansion *ex;

	ex = get_ex (expansion, idx);

	if (ex)
	{
		copy_on_write (expansion, FALSE);
		ex = get_ex_real (expansion, idx, TRUE);

		if (expansion->is_one)
		{
			ex = expansion->text;
		}
		else
		{
			ex = expansion->expansions->pdata[idx];
		}

		g_free (ex->text);
		ex->text = g_strdup (val);
	}
}

/**
 * cdn_expansion_copy:
 * @expansion: A #CdnExpansion
 *
 * Copy an expansion.
 *
 * Returns: (transfer full): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_expansion_copy (CdnExpansion *expansion)
{
	CdnExpansion *ret;

	if (expansion == NULL)
	{
		return NULL;
	}

	ret = cdn_expansion_create ();

	if (expansion->is_one)
	{
		ret->is_one = TRUE;
		ret->text = expansion_ref (expansion->text);
	}
	else
	{
		ret->expansions = g_ptr_array_ref (expansion->expansions);
	}

	ret->copy_on_write = TRUE;

	return ret;
}

/**
 * cdn_expansion_add:
 * @expansion: the #CdnExpansion
 * @item: the item
 *
 * Add a single item (append) to the expansion.
 *
 */
void
cdn_expansion_add (CdnExpansion *expansion,
                   gchar const  *item)
{
	copy_on_write (expansion, TRUE);

	g_ptr_array_add (expansion->expansions,
	                 expansion_new (item));
}

/**
 * cdn_expansion_insert:
 * @expansion: the #CdnExpansion
 * @idx: the index
 * @item: the item
 *
 * Insert a single item at the specified index.
 *
 */
void
cdn_expansion_insert (CdnExpansion *expansion,
                      gint          idx,
                      gchar const  *item)
{
	gint n;
	gint i;

	copy_on_write_sized (expansion, TRUE, 1);
	n = cdn_expansion_num (expansion);

	for (i = n - 1; i > idx; --i)
	{
		expansion->expansions->pdata[i] = expansion->expansions->pdata[i - 1];
	}

	expansion->expansions->pdata[idx] = expansion_new (item);
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
 * Annotate the indices of the expansion items starting at the
 * provided start. Items with the same value will be assigned the
 * same index.
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

/**
 * cdn_expansion_append:
 * @expansion: the #CdnExpansion
 * @other: another #CdnExpansion
 * @idx: the index
 *
 * Append the items in @other to the items in @expansion,
 * starting at @idx.
 *
 */
void
cdn_expansion_append (CdnExpansion *expansion,
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

	oldsize = cdn_expansion_num (expansion);

	copy_on_write_sized (expansion, TRUE, onum - idx);

	for (i = idx; i < onum; ++i)
	{
		Expansion *ex;

		ex = expansion_copy (get_ex (other, i));
		expansion->expansions->pdata[oldsize + i - idx] = ex;
	}
}

/**
 * cdn_expansion_prepend:
 * @expansion: the #CdnExpansion
 * @other: another #CdnExpansion
 * @idx: the index
 *
 * Prepend the items in @other before the items in @expansion,
 * starting at @idx.
 *
 */
void
cdn_expansion_prepend (CdnExpansion *expansion,
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

	oldsize = cdn_expansion_num (expansion);
	newsize = oldsize + num;

	copy_on_write_sized (expansion, TRUE, num);

	// move data
	for (i = 0; i < oldsize - 1; ++i)
	{
		gint end = newsize - i - 1;

		expansion->expansions->pdata[end] =
			expansion->expansions->pdata[end - num];
	}

	for (i = idx; i < onum; ++i)
	{
		gint pidx = i - idx + 1;

		expansion->expansions->pdata[pidx] = expansion_copy (get_ex (other, i));
	}
}

void
_cdn_expansion_debug_print (CdnExpansion *expansion,
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
