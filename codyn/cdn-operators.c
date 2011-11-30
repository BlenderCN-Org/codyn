/*
 * cdn-operators.c
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

#include "cdn-operators.h"
#
static GSList *operator_registry = NULL;

static void
ensure_defaults ()
{
	static gboolean initing = FALSE;

	if (!initing)
	{
		initing = TRUE;

		cdn_operators_register (CDN_TYPE_OPERATOR_DELAYED);
		cdn_operators_register (CDN_TYPE_OPERATOR_DIFF);
		cdn_operators_register (CDN_TYPE_OPERATOR_PDIFF);
		cdn_operators_register (CDN_TYPE_OPERATOR_SIMPLIFY);
		cdn_operators_register (CDN_TYPE_OPERATOR_DF_DT);
		cdn_operators_register (CDN_TYPE_OPERATOR_LINSOLVE);
	}
}

/**
 * cdn_operators_list:
 *
 * Get the list of operators.
 *
 * Returns: (element-type CdnOperatorClass) (transfer none): A #GSList of
            #CdnOperatorClass
 *
 **/
GSList const *
cdn_operators_list ()
{
	return operator_registry;
}

/**
 * cdn_operators_register:
 * @gtype: A #GType
 *
 * Register an operator. The type @gtype should derive from #CdnOperator
 *
 **/
void
cdn_operators_register (GType gtype)
{
	gpointer klass;

	g_return_if_fail (g_type_is_a (gtype, CDN_TYPE_OPERATOR));

	ensure_defaults ();

	klass = g_type_class_ref (gtype);

	if (g_slist_find ((GSList *)cdn_operators_list (), klass))
	{
		g_type_class_unref (klass);
		return;
	}

	operator_registry = g_slist_append (operator_registry,
	                                    klass);
}

/**
 * cdn_operators_unregister:
 * @gtype: A #GType
 *
 * Unregister an operator.
 *
 **/
void
cdn_operators_unregister (GType gtype)
{
	gpointer klass = g_type_class_peek (gtype);

	if (klass)
	{
		operator_registry = g_slist_remove (operator_registry,
		                                    klass);
	}
}

/**
 * cdn_operators_find_class:
 * @name: The name of the operator
 *
 * Find the class of an operator by name.
 *
 * Returns: (transfer none) (allow-none): A #CdnOperatorClass or %NULL if the operator with @name is not found
 *
 **/
CdnOperatorClass *
cdn_operators_find_class (gchar const *name)
{
	GSList const *ops;

	ensure_defaults ();

	ops = cdn_operators_list ();

	while (ops)
	{
		CdnOperatorClass *klass;

		klass = ops->data;

		if (g_strcmp0 (name, cdn_operator_get_class_name (klass)) == 0)
		{
			return klass;
		}

		ops = g_slist_next (ops);
	}

	return NULL;
}

/**
 * cdn_operators_find:
 * @name: The name of the operator
 *
 * Find a custom operator by name.
 *
 * Returns: The #GType of the operator or #G_TYPE_INVALID if not found
 *
 **/
GType
cdn_operators_find (gchar const *name)
{
	CdnOperatorClass *klass;

	klass = cdn_operators_find_class (name);

	if (klass == NULL)
	{
		return G_TYPE_INVALID;
	}
	else
	{
		return G_TYPE_FROM_CLASS (klass);
	}
}

/**
 * cdn_operators_instantiate:
 * @name: The name of the operator
 * @expressions: (element-type CdnExpression): A #GSList of #CdnExpression
 *
 * Instantiate an operator from a name.
 *
 * Returns: (transfer full) (allow-none): A #CdnOperator or %NULL if the operator with @name could not be found
 *
 **/
CdnOperator *
cdn_operators_instantiate (gchar const   *name,
                           GSList const **expressions,
                           gint           num_expressions,
                           GSList const **indices,
                           gint           num_indices,
                           gint           num_arguments,
                           GError       **error)
{
	GType gtype;
	CdnOperator *ret;

	gtype = cdn_operators_find (name);

	if (gtype == G_TYPE_INVALID)
	{
		return NULL;
	}

	ret = g_object_new (gtype, NULL);

	if (!cdn_operator_initialize (ret,
	                              expressions,
	                              num_expressions,
	                              indices,
	                              num_indices,
	                              num_arguments,
	                              error))
	{
		g_object_unref (ret);
		ret = NULL;
	}

	return ret;
}

