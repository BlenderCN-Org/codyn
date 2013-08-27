/*
 * cdn-phaseable.c
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

#include "cdn-phaseable.h"

/**
 * CdnPhaseable:
 *
 * Interface for state dependent actions.
 *
 * This interface can be implemented when an object can be active or inactive
 * depending on node states (see #cdn_node_get_state). Implementations of this
 * interface should call #cdn_phaseable_set_phase_table to specify a hash table
 * which the states are stored.
 *
 **/

G_DEFINE_INTERFACE (CdnPhaseable, cdn_phaseable, G_TYPE_OBJECT);

/* Default implementation */
static GHashTable *
cdn_phaseable_get_phase_table_default (CdnPhaseable *phaseable)
{
	return NULL;
}

static void
cdn_phaseable_set_phase_table_default (CdnPhaseable *phaseable,
                                       GHashTable   *table)
{
}

static void
cdn_phaseable_default_init (CdnPhaseableInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->get_phase_table = cdn_phaseable_get_phase_table_default;
	iface->set_phase_table = cdn_phaseable_set_phase_table_default;

	if (!initialized)
	{
		initialized = TRUE;
	}
}

/**
 * cdn_phaseable_is_active:
 * @phaseable: a #CdnPhaseable.
 * @phase: the phase.
 *
 * Get whether this object is active for the given state.
 *
 * Returns: %TRUE if the object is active in @phase, %FALSE otherwise.
 *
 **/
gboolean
cdn_phaseable_is_active (CdnPhaseable *phaseable,
                         gchar const  *phase)
{
	GHashTable *table;

	g_return_val_if_fail (CDN_PHASEABLE (phaseable), FALSE);

	table = cdn_phaseable_get_phase_table (phaseable);

	if (!table)
	{
		return TRUE;
	}
	else if (phase)
	{
		return g_hash_table_lookup (table, phase) == GINT_TO_POINTER (1);
	}
	else
	{
		return FALSE;
	}
}

static GHashTable *
create_table (CdnPhaseable *phaseable)
{
	GHashTable *table;

	table = g_hash_table_new_full (g_str_hash,
	                               g_str_equal,
	                               (GDestroyNotify)g_free,
	                               NULL);

	cdn_phaseable_set_phase_table (phaseable, table);

	return table;
}

typedef struct
{
	GHashTable *other;
	gboolean ret;
} CompareInfo;

static void
phaseable_foreach_equal (gchar const *key,
                         gpointer     value,
                         CompareInfo *ret)
{
	if (!g_hash_table_lookup (ret->other, key))
	{
		ret->ret = FALSE;
	}
}

/**
 * cdn_phaseable_equal:
 * @phaseable: a #CdnPhaseable.
 * @other: a #CdnPhaseable.
 *
 * Returns whether @phaseable is equal to @other in terms of in which states
 * they are active.
 *
 * Returns: %TRUE if @phaseable and @other are active in the same states, %FALSE otherwise.
 *
 **/
gboolean
cdn_phaseable_equal (CdnPhaseable *phaseable,
                     CdnPhaseable *other)
{
	GHashTable *t1;
	GHashTable *t2;
	CompareInfo info = {NULL, TRUE};

	g_return_val_if_fail (CDN_PHASEABLE (phaseable), FALSE);
	g_return_val_if_fail (CDN_PHASEABLE (other), FALSE);

	t1 = cdn_phaseable_get_phase_table (phaseable);
	t2 = cdn_phaseable_get_phase_table (other);

	if ((t1 != NULL) != (t2 != NULL))
	{
		return FALSE;
	}

	if (t1 == NULL)
	{
		return TRUE;
	}

	info.other = t2;
	g_hash_table_foreach (t1, (GHFunc)phaseable_foreach_equal, &info);

	return info.ret;
}

/**
 * cdn_phaseable_add_phase:
 * @phaseable: a #CdnPhaseable.
 * @phase: the state.
 *
 * Add a state in which @phaseable is active.
 *
 **/
void
cdn_phaseable_add_phase (CdnPhaseable *phaseable,
                         gchar const  *phase)
{
	GHashTable *table;

	g_return_if_fail (CDN_PHASEABLE (phaseable));
	g_return_if_fail (phase != NULL);

	table = cdn_phaseable_get_phase_table (phaseable);

	if (!table)
	{
		table = create_table (phaseable);
	}
	else
	{
		g_hash_table_ref (table);
	}

	g_hash_table_insert (table, g_strdup (phase), GINT_TO_POINTER (1));
	g_hash_table_unref (table);
}

/**
 * cdn_phaseable_remove_phase:
 * @phaseable: a #CdnPhaseable.
 * @phase: a state.
 *
 * Remove a state in which @phaseable was active.
 *
 **/
void
cdn_phaseable_remove_phase (CdnPhaseable *phaseable,
                            gchar const  *phase)
{
	gint num;
	GHashTable *table;

	g_return_if_fail (CDN_IS_PHASEABLE (phaseable));
	g_return_if_fail (phase != NULL);

	table = cdn_phaseable_get_phase_table (phaseable);

	if (!table)
	{
		return;
	}

	num = g_hash_table_size (table);

	if (g_hash_table_remove (table,
	                         phase) && num == 1)
	{
		cdn_phaseable_set_phase_table (phaseable, NULL);
	}
}

/**
 * cdn_phaseable_get_phase_table:
 * @phaseable: A #CdnPhaseable
 *
 * Get the tag hash table.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cdn_phaseable_get_phase_table (CdnPhaseable *phaseable)
{
	g_return_val_if_fail (CDN_IS_PHASEABLE (phaseable), NULL);

	return CDN_PHASEABLE_GET_INTERFACE (phaseable)->get_phase_table (phaseable);
}

/**
 * cdn_phaseable_set_phase_table:
 * @phaseable: a #CdnPhaseable.
 * @table: a #GHashTable.
 *
 * Set the hash table used to store in which states the phaseable is active.
 *
 **/
void
cdn_phaseable_set_phase_table (CdnPhaseable *phaseable,
                               GHashTable   *table)
{
	g_return_if_fail (CDN_IS_PHASEABLE (phaseable));

	return CDN_PHASEABLE_GET_INTERFACE (phaseable)->set_phase_table (phaseable,
	                                                                 table);
}

static void
copy_phase (gchar const  *key,
            gpointer      value,
            GHashTable   *ret)
{
	g_hash_table_insert (ret,
	                     g_strdup (key),
	                     GINT_TO_POINTER (1));
}

/**
 * cdn_phaseable_copy_to:
 * @phaseable: a #CdnPhaseable.
 * @dest: a #CdnPhaseable.
 *
 * Copy the states in which @phaseable is active to @dest.
 *
 **/
void
cdn_phaseable_copy_to (CdnPhaseable *phaseable,
                       CdnPhaseable *dest)
{
	GHashTable *source;
	GHashTable *table;

	g_return_if_fail (CDN_IS_PHASEABLE (phaseable));
	g_return_if_fail (CDN_IS_PHASEABLE (dest));

	source = cdn_phaseable_get_phase_table (phaseable);

	if (!source)
	{
		return;
	}

	table = create_table (dest);

	g_hash_table_foreach (source, (GHFunc)copy_phase, table);
	g_hash_table_unref (table);
}

typedef struct
{
	CdnPhaseable *phaseable;
	CdnPhaseableForeachFunc func;
	gpointer userdata;
} ForeachInfo;

static void
phaseable_foreach (gchar const *key,
                   gpointer     value,
                   ForeachInfo *info)
{
	info->func (info->phaseable, key, info->userdata);
}

/**
 * cdn_phaseable_foreach:
 * @phaseable: A #CdnPhaseable
 * @func: (scope call): A #CdnPhaseableForeachFunc
 * @userdata: The user data for @func
 *
 * Calls @func for each state.
 *
 **/
void
cdn_phaseable_foreach (CdnPhaseable            *phaseable,
                       CdnPhaseableForeachFunc  func,
                       gpointer                 userdata)
{
	ForeachInfo info;
	GHashTable *table;

	g_return_if_fail (CDN_IS_PHASEABLE (phaseable));
	g_return_if_fail (func != NULL);

	info.phaseable = phaseable;
	info.func = func;
	info.userdata = userdata;

	table = cdn_phaseable_get_phase_table (phaseable);

	if (table)
	{
		g_hash_table_foreach (table,
		                      (GHFunc)phaseable_foreach,
		                      &info);
	}
}

static void
phaseable_foreach_add (gchar const *key,
                       gpointer     value,
                       GPtrArray   *ptr)
{
	g_ptr_array_add (ptr, g_strdup (key));
}

/**
 * cdn_phaseable_get_phases:
 * @phaseable: a #CdnPhaseable.
 *
 * Get the list of states in which @phaseable is active. The returned list
 * should be freed with #g_strfreev.
 *
 * Returns: (transfer full): %NULL terminated list of phases. Free the return
 *                           value with #g_strfreev when done.
 *
 **/
gchar **
cdn_phaseable_get_phases (CdnPhaseable *phaseable)
{
	GPtrArray *ret;

	ret = g_ptr_array_new ();

	GHashTable *table;
	table = cdn_phaseable_get_phase_table (phaseable);

	if (table)
	{
		g_hash_table_foreach (table,
		                      (GHFunc)phaseable_foreach_add,
		                      ret);
	}

	g_ptr_array_add (ret, NULL);
	return (gchar **)g_ptr_array_free (ret, FALSE);
}
