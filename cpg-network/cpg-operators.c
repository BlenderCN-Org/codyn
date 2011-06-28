/*
 * cpg-operators.c
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

#include "cpg-operators.h"
#include "cpg-operator-delayed.h"

static GSList *operator_registry = NULL;

static void
ensure_defaults ()
{
	static gboolean initing = FALSE;

	if (!initing)
	{
		initing = TRUE;

		cpg_operators_register (CPG_TYPE_OPERATOR_DELAYED);
	}
}

GSList const *
cpg_operators_list ()
{
	return operator_registry;
}

void
cpg_operators_register (GType gtype)
{
	gpointer klass;

	ensure_defaults ();

	klass = g_type_class_ref (gtype);

	if (g_slist_find ((GSList *)cpg_operators_list (), klass))
	{
		g_type_class_unref (klass);
		return;
	}

	operator_registry = g_slist_append (operator_registry,
	                                    klass);
}

void
cpg_operators_unregister (GType gtype)
{
	gpointer klass = g_type_class_peek (gtype);

	if (klass)
	{
		operator_registry = g_slist_remove (operator_registry,
		                                    klass);
	}
}

CpgOperatorClass *
cpg_operators_find_class (gchar const *name)
{
	GSList const *ops;

	ensure_defaults ();

	ops = cpg_operators_list ();

	while (ops)
	{
		CpgOperatorClass *klass;

		klass = ops->data;

		if (g_strcmp0 (name, cpg_operator_get_name (klass)) == 0)
		{
			return klass;
		}

		ops = g_slist_next (ops);
	}

	return NULL;
}

GType
cpg_operators_find (gchar const *name)
{
	CpgOperatorClass *klass;

	klass = cpg_operators_find_class (name);

	if (klass == NULL)
	{
		return G_TYPE_INVALID;
	}
	else
	{
		return G_TYPE_FROM_CLASS (klass);
	}
}

CpgOperator  *
cpg_operators_instantiate (gchar const     *name,
                           GSList const    *expressions)
{
	GType gtype;
	CpgOperator *ret;

	gtype = cpg_operators_find (name);

	if (gtype == G_TYPE_INVALID)
	{
		return NULL;
	}

	ret = g_object_new (gtype, NULL);
	cpg_operator_initialize (ret, expressions);

	return ret;
}

