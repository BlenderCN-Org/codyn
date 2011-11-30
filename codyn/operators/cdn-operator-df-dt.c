/*
 * cdn-operator-df_dt.c
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

#include "cdn-operator-df-dt.h"
#include "cdn-operator.h"
#include "cdn-variable.h"
#include "cdn-usable.h"
#include "integrators/cdn-integrator.h"
#include "cdn-symbolic.h"
#include "cdn-function.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-network.h"

#include "instructions/cdn-instructions.h"

#include <math.h>

/**
 * SECTION:cdn-operator-df_dt
 * @short_description: Math operator for df_dt evaluation of an expression
 *
 * The #CdnOperatorDfDt is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * df_dt value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_DF_DT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_DF_DT, CdnOperatorDfDtPrivate))

struct _CdnOperatorDfDtPrivate
{
	CdnExpression *expression;
	CdnExpression *derived;

	gint order;
};

G_DEFINE_TYPE (CdnOperatorDfDt,
               cdn_operator_df_dt,
               CDN_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED,
	PROP_ORDER
};

static gchar *
cdn_operator_df_dt_get_name ()
{
	return g_strdup ("df_dt");
}

static gboolean
cdn_operator_df_dt_initialize (CdnOperator   *op,
                               GSList const **expressions,
                               gint           num_expressions,
                               GSList const **indices,
                               gint           num_indices,
                               gint           num_arguments,
                               GError       **error)
{
	CdnOperatorDfDt *df_dt;

	if (!CDN_OPERATOR_CLASS (cdn_operator_df_dt_parent_class)->initialize (op,
	                                                                       expressions,
	                                                                       num_expressions,
	                                                                       indices,
	                                                                       num_indices,
	                                                                       num_arguments,
	                                                                       error))
	{
		return FALSE;
	}

	if (num_expressions != 1 || expressions[0]->next)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `df_dt' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	df_dt = CDN_OPERATOR_DF_DT (op);
	df_dt->priv->expression = g_object_ref_sink (expressions[0]->data);

	df_dt->priv->order = 1;

	if (expressions[0]->next)
	{
		df_dt->priv->order = rint (cdn_expression_evaluate (expressions[0]->next->data));

		if (df_dt->priv->order < 0)
		{
			df_dt->priv->order = 0;
		}
	}

	df_dt->priv->derived = cdn_symbolic_derive (df_dt->priv->expression,
	                                            NULL,
	                                            NULL,
	                                            NULL,
	                                            df_dt->priv->order,
	                                            CDN_SYMBOLIC_DERIVE_NONE,
	                                            error);

	cdn_expression_set_has_cache (df_dt->priv->derived, FALSE);

	g_object_ref_sink (df_dt->priv->derived);

	return df_dt->priv->derived != NULL;
}

static void
cdn_operator_df_dt_execute (CdnOperator *op,
                            CdnStack    *stack)
{
	CdnOperatorDfDt *d;

	d = (CdnOperatorDfDt *)op;

	if (d->priv->derived)
	{
		cdn_stack_push (stack,
		                cdn_expression_evaluate (d->priv->derived));
	}
	else
	{
		cdn_stack_push (stack, 0);
	}
}

static void
cdn_operator_df_dt_finalize (GObject *object)
{
	CdnOperatorDfDt *df_dt;

	df_dt = CDN_OPERATOR_DF_DT (object);

	if (df_dt->priv->expression)
	{
		g_object_unref (df_dt->priv->expression);
	}

	if (df_dt->priv->derived)
	{
		g_object_unref (df_dt->priv->derived);
	}

	G_OBJECT_CLASS (cdn_operator_df_dt_parent_class)->finalize (object);
}

static void
cdn_operator_df_dt_set_property (GObject      *object,
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
cdn_operator_df_dt_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CdnOperatorDfDt *self = CDN_OPERATOR_DF_DT (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
			break;
		case PROP_DERIVED:
			g_value_set_object (value, self->priv->derived);
			break;
		case PROP_ORDER:
			g_value_set_int (value, self->priv->order);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cdn_operator_df_dt_equal (CdnOperator *op,
                            CdnOperator *other)
{
	CdnOperatorDfDt *df_dt;
	CdnOperatorDfDt *odel;

	if (!CDN_IS_OPERATOR_DF_DT (other))
	{
		return FALSE;
	}

	df_dt = CDN_OPERATOR_DF_DT (op);
	odel = CDN_OPERATOR_DF_DT (other);

	if (df_dt->priv->order != odel->priv->order)
	{
		return FALSE;
	}

	if (!cdn_expression_equal (df_dt->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cdn_operator_df_dt_reset_cache (CdnOperator *operator)
{
	CdnOperatorDfDt *self;

	((CdnOperatorClass *)cdn_operator_df_dt_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CdnOperatorDfDt *)operator;

	if (self->priv->derived)
	{
		cdn_expression_reset_cache (self->priv->derived);
	}
}

static void
cdn_operator_df_dt_reset (CdnOperator *operator)
{
	CdnOperatorDfDt *self;

	CDN_OPERATOR_CLASS (cdn_operator_df_dt_parent_class)->reset (operator);

	self = CDN_OPERATOR_DF_DT (operator);

	if (self->priv->derived)
	{
		cdn_expression_reset (self->priv->derived);
	}
}

static CdnOperator *
cdn_operator_df_dt_copy (CdnOperator *op)
{
	CdnOperatorDfDt *df_dt;
	CdnOperatorDfDt *ret;

	df_dt = CDN_OPERATOR_DF_DT (op);

	ret = CDN_OPERATOR_DF_DT (g_object_new (CDN_TYPE_OPERATOR_DF_DT, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_df_dt_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                                 cdn_operator_all_expressions (op),
	                                                                 cdn_operator_num_expressions (op),
	                                                                 cdn_operator_all_indices (op),
	                                                                 cdn_operator_num_indices (op),
	                                                                 cdn_operator_get_num_arguments (op),
	                                                                 NULL);

	if (df_dt->priv->expression)
	{
		ret->priv->expression = g_object_ref_sink (df_dt->priv->expression);
	}

	if (df_dt->priv->derived)
	{
		ret->priv->derived = g_object_ref_sink (df_dt->priv->derived);
	}

	ret->priv->order = df_dt->priv->order;

	return CDN_OPERATOR (ret);
}

static void
cdn_operator_df_dt_class_init (CdnOperatorDfDtClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_df_dt_finalize;

	object_class->get_property = cdn_operator_df_dt_get_property;
	object_class->set_property = cdn_operator_df_dt_set_property;

	op_class->get_name = cdn_operator_df_dt_get_name;
	op_class->execute = cdn_operator_df_dt_execute;
	op_class->initialize = cdn_operator_df_dt_initialize;
	op_class->equal = cdn_operator_df_dt_equal;
	op_class->reset_cache = cdn_operator_df_dt_reset_cache;
	op_class->reset = cdn_operator_df_dt_reset;
	op_class->copy = cdn_operator_df_dt_copy;

	g_type_class_add_private (object_class, sizeof(CdnOperatorDfDtPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_DERIVED,
	                                 g_param_spec_object ("derived",
	                                                      "Derived",
	                                                      "Derived",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READABLE));

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
cdn_operator_df_dt_init (CdnOperatorDfDt *self)
{
	self->priv = CDN_OPERATOR_DF_DT_GET_PRIVATE (self);
}

CdnOperatorDfDt *
cdn_operator_df_dt_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_DF_DT, NULL);
}

/**
 * cdn_operator_df_dt_get_expression:
 * @df_dt: A #CdnOperatorDfDt
 *
 * Get the expression to be df_dt.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_df_dt_get_expression (CdnOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DF_DT (df_dt), NULL);

	return df_dt->priv->expression;
}

/**
 * cdn_operator_df_dt_get_derived:
 * @df_dt: A #CdnOperatorDfDt
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_df_dt_get_derived (CdnOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DF_DT (df_dt), NULL);

	return df_dt->priv->derived;
}

/**
 * cdn_operator_df_dt_get_order:
 * @df_dt: A #CdnOperatorDfDt
 *
 * Get the order.
 *
 * Returns: The order
 *
 **/
gint
cdn_operator_df_dt_get_order (CdnOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DF_DT (df_dt), 0.0);

	return df_dt->priv->order;
}
