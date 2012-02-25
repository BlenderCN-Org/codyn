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
#include "cdn-symbolic.h"
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
	CdnExpression *expression;
	CdnExpression *derived;

	gint order;
};

G_DEFINE_TYPE (CdnOperatorDt,
               cdn_operator_dt,
               CDN_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED,
	PROP_ORDER
};

static gchar *
cdn_operator_dt_get_name ()
{
	return g_strdup ("dt");
}

static gboolean
cdn_operator_dt_initialize (CdnOperator   *op,
                            GSList const **expressions,
                            gint           num_expressions,
                            GSList const **indices,
                            gint           num_indices,
                            gint           num_arguments,
                            gint          *argdim,
                            GError       **error)
{
	CdnOperatorDt *dt;

	if (!CDN_OPERATOR_CLASS (cdn_operator_dt_parent_class)->initialize (op,
	                                                                    expressions,
	                                                                    num_expressions,
	                                                                    indices,
	                                                                    num_indices,
	                                                                    num_arguments,
	                                                                    argdim,
	                                                                    error))
	{
		return FALSE;
	}

	if (num_expressions != 1 || expressions[0]->next)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `dt' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	dt = CDN_OPERATOR_DT (op);
	dt->priv->expression = g_object_ref_sink (expressions[0]->data);

	dt->priv->order = 1;

	if (expressions[0]->next)
	{
		dt->priv->order = rint (cdn_expression_evaluate (expressions[0]->next->data));

		if (dt->priv->order < 0)
		{
			dt->priv->order = 0;
		}
	}

	dt->priv->derived = cdn_symbolic_derive (dt->priv->expression,
	                                         NULL,
	                                         NULL,
	                                         NULL,
	                                         dt->priv->order,
	                                         CDN_SYMBOLIC_DERIVE_NONE,
	                                         error);

	cdn_expression_set_has_cache (dt->priv->derived, FALSE);

	g_object_ref_sink (dt->priv->derived);

	return dt->priv->derived != NULL;
}

static void
cdn_operator_dt_execute (CdnOperator *op,
                            CdnStack    *stack)
{
	CdnOperatorDt *d;

	d = (CdnOperatorDt *)op;

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
cdn_operator_dt_finalize (GObject *object)
{
	CdnOperatorDt *dt;

	dt = CDN_OPERATOR_DT (object);

	if (dt->priv->expression)
	{
		g_object_unref (dt->priv->expression);
	}

	if (dt->priv->derived)
	{
		g_object_unref (dt->priv->derived);
	}

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

	if (!cdn_expression_equal (dt->priv->expression,
	                           odel->priv->expression,
	                           asstring))
	{
		return FALSE;
	}

	return TRUE;
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
	                                                               NULL);

	if (dt->priv->expression)
	{
		ret->priv->expression = g_object_ref_sink (dt->priv->expression);
	}

	if (dt->priv->derived)
	{
		ret->priv->derived = g_object_ref_sink (dt->priv->derived);
	}

	ret->priv->order = dt->priv->order;

	return CDN_OPERATOR (ret);
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
	op_class->execute = cdn_operator_dt_execute;
	op_class->initialize = cdn_operator_dt_initialize;
	op_class->equal = cdn_operator_dt_equal;
	op_class->copy = cdn_operator_dt_copy;

	g_type_class_add_private (object_class, sizeof(CdnOperatorDtPrivate));

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
 * cdn_operator_dt_get_expression:
 * @dt: A #CdnOperatorDt
 *
 * Get the expression to be dt.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_dt_get_expression (CdnOperatorDt *dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DT (dt), NULL);

	return dt->priv->expression;
}

/**
 * cdn_operator_dt_get_derived:
 * @dt: A #CdnOperatorDt
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_dt_get_derived (CdnOperatorDt *dt)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DT (dt), NULL);

	return dt->priv->derived;
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
