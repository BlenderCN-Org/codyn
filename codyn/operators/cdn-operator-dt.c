/*
 * cdn-operator-dt.c
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

#include "cdn-operator-dt.h"
#include "cdn-operator.h"
#include "cdn-variable.h"
#include "cdn-usable.h"
#include "integrators/cdn-integrator.h"
#include "cdn-function.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-network.h"

#include "instructions/cdn-instructions.h"

#include <math.h>

/**
 * SECTION:cdn-operator-dt
 * @short_description: Math operator for dt evaluation of an expression
 *
 * The #CdnOperatorDt is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * dt value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_DT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_DT, CdnOperatorDtPrivate))

struct _CdnOperatorDtPrivate
{
	CdnFunction *function;

	gint order;
};

G_DEFINE_TYPE (CdnOperatorDt,
               cdn_operator_dt,
               CDN_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_ORDER
};

static gchar *
cdn_operator_dt_get_name ()
{
	return g_strdup ("dt");
}

static gboolean
cdn_operator_dt_initialize (CdnOperator        *op,
                            GSList const      **expressions,
                            gint                num_expressions,
                            GSList const      **indices,
                            gint                num_indices,
                            gint                num_arguments,
                            gint               *argdim,
                            CdnCompileContext  *context,
                            GError            **error)
{
	CdnOperatorDt *dt;
	GHashTable *syms;
	CdnVariable *v;
	CdnExpression *expr;
	CdnExpressionTreeIter *iter;
	CdnExpressionTreeIter *derived;

	if (!CDN_OPERATOR_CLASS (cdn_operator_dt_parent_class)->initialize (op,
	                                                                    expressions,
	                                                                    num_expressions,
	                                                                    indices,
	                                                                    num_indices,
	                                                                    num_arguments,
	                                                                    argdim,
	                                                                    context,
	                                                                    error))
	{
		return FALSE;
	}

	if (num_expressions != 1 || (expressions[0]->next && expressions[0]->next->next))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `dt' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	dt = CDN_OPERATOR_DT (op);
	dt->priv->order = 1;

	if (expressions[0]->next)
	{
		dt->priv->order = rint (cdn_expression_evaluate (expressions[0]->next->data));

		if (dt->priv->order < 0)
		{
			dt->priv->order = 0;
		}
	}

	v = cdn_compile_context_lookup_variable_last (context, "t");

	syms = g_hash_table_new_full (g_direct_hash,
	                              g_direct_equal,
	                              (GDestroyNotify)g_object_unref,
	                              (GDestroyNotify)cdn_expression_tree_iter_free);

	if (v)
	{
		CdnExpressionTreeIter *in;
		CdnInstruction *instr;

		instr = cdn_instruction_number_new_from_string ("1");
		in = cdn_expression_tree_iter_new_from_instruction_take (instr);

		g_hash_table_insert (syms, g_object_ref (v), in);
	}

	iter = cdn_expression_tree_iter_new (expressions[0]->data);

	derived = cdn_expression_tree_iter_derive (iter,
	                                           NULL,
	                                           syms,
	                                           dt->priv->order,
	                                           CDN_EXPRESSION_TREE_ITER_DERIVE_TIME |
	                                           CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY,
	                                           error);

	cdn_expression_tree_iter_free (iter);
	g_hash_table_unref (syms);

	if (!derived)
	{
		return FALSE;
	}

	expr = cdn_expression_tree_iter_to_expression (derived);
	cdn_expression_tree_iter_free (derived);

	dt->priv->function = cdn_function_new ("dt", expr);

	return TRUE;
}

static void
cdn_operator_dt_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_operator_dt_parent_class)->finalize (object);
}

static void
cdn_operator_dt_set_property (GObject      *object,
                              guint         prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
	switch (prop_id)
	{
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_operator_dt_get_property (GObject    *object,
                              guint       prop_id,
                              GValue     *value,
                              GParamSpec *pspec)
{
	CdnOperatorDt *self = CDN_OPERATOR_DT (object);

	switch (prop_id)
	{
		case PROP_ORDER:
			g_value_set_int (value, self->priv->order);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cdn_operator_dt_equal (CdnOperator *op,
                       CdnOperator *other,
                       gboolean     asstring)
{
	CdnOperatorDt *dt;
	CdnOperatorDt *odel;

	if (!CDN_IS_OPERATOR_DT (other))
	{
		return FALSE;
	}

	dt = CDN_OPERATOR_DT (op);
	odel = CDN_OPERATOR_DT (other);

	if (dt->priv->order != odel->priv->order)
	{
		return FALSE;
	}

	return CDN_OPERATOR_CLASS (cdn_operator_dt_parent_class)->equal (op,
	                                                                 other,
	                                                                 asstring);
}

static CdnOperator *
cdn_operator_dt_copy (CdnOperator *op)
{
	CdnOperatorDt *dt;
	CdnOperatorDt *ret;

	dt = CDN_OPERATOR_DT (op);

	ret = CDN_OPERATOR_DT (g_object_new (CDN_TYPE_OPERATOR_DT, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_dt_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                               cdn_operator_all_expressions (op),
	                                                               cdn_operator_num_expressions (op),
	                                                               cdn_operator_all_indices (op),
	                                                               cdn_operator_num_indices (op),
	                                                               cdn_operator_get_num_arguments (op),
	                                                               cdn_operator_get_arguments_dimension (op),
	                                                               NULL,
	                                                               NULL);

	if (dt->priv->function)
	{
		ret->priv->function = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (dt->priv->function)));
	}

	ret->priv->order = dt->priv->order;

	return CDN_OPERATOR (ret);
}

static CdnFunction *
cdn_operator_dt_get_function (CdnOperator *op,
                              gint        *idx,
                              gint         numidx)
{
	CdnOperatorDt *dt;

	dt = (CdnOperatorDt *)op;
	return dt->priv->function;
}

static void
cdn_operator_dt_foreach_function (CdnOperator            *op,
                                  CdnForeachFunctionFunc  func,
                                  gpointer                data)
{
	CdnOperatorDt *dt;

	dt = (CdnOperatorDt *)op;

	if (dt->priv->function)
	{
		func (dt->priv->function, data);
	}
}

static void
cdn_operator_dt_class_init (CdnOperatorDtClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_dt_finalize;

	object_class->get_property = cdn_operator_dt_get_property;
	object_class->set_property = cdn_operator_dt_set_property;

	op_class->get_name = cdn_operator_dt_get_name;
	op_class->initialize = cdn_operator_dt_initialize;
	op_class->equal = cdn_operator_dt_equal;
	op_class->copy = cdn_operator_dt_copy;
	op_class->foreach_function = cdn_operator_dt_foreach_function;
	op_class->get_function = cdn_operator_dt_get_function;

	g_type_class_add_private (object_class, sizeof(CdnOperatorDtPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_ORDER,
	                                 g_param_spec_int ("order",
	                                                   "Order",
	                                                   "Order",
	                                                    0,
	                                                    G_MAXINT,
	                                                    1,
	                                                    G_PARAM_READABLE));
}

static void
cdn_operator_dt_init (CdnOperatorDt *self)
{
	self->priv = CDN_OPERATOR_DT_GET_PRIVATE (self);
}

CdnOperatorDt *
cdn_operator_dt_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_DT, NULL);
}

/**
 * cdn_operator_dt_get_order:
 * @dt: A #CdnOperatorDt
 *
 * Get the order.
 *
 * Returns: The order
 *
 **/
gint
cdn_operator_dt_get_order (CdnOperatorDt *dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DT (dt), 0.0);

	return dt->priv->order;
}
