/*
 * cpg-operator-df_dt.c
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

#include "cpg-operator-df-dt.h"
#include "cpg-operator.h"
#include "cpg-property.h"
#include "cpg-usable.h"
#include "cpg-integrator.h"
#include "cpg-symbolic.h"
#include "cpg-function.h"
#include "cpg-expression-tree-iter.h"
#include "cpg-network.h"

#include "instructions/cpg-instructions.h"

#include <math.h>

/**
 * SECTION:cpg-operator-df_dt
 * @short_description: Math operator for df_dt evaluation of an expression
 *
 * The #CpgOperatorDfDt is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * df_dt value of its argument (which can be an arbitrary expression).
 *
 */

#define CPG_OPERATOR_DF_DT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_DF_DT, CpgOperatorDfDtPrivate))

struct _CpgOperatorDfDtPrivate
{
	CpgExpression *expression;
	CpgExpression *derived;

	gint order;
};

G_DEFINE_TYPE (CpgOperatorDfDt,
               cpg_operator_df_dt,
               CPG_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED,
	PROP_ORDER
};

static gchar *
cpg_operator_df_dt_get_name ()
{
	return g_strdup ("df_dt");
}

static gboolean
cpg_operator_df_dt_initialize (CpgOperator   *op,
                               GSList const **expressions,
                               gint           num_expressions,
                               GSList const **indices,
                               gint           num_indices,
                               gint           num_arguments,
                               GError       **error)
{
	CpgOperatorDfDt *df_dt;

	if (!CPG_OPERATOR_CLASS (cpg_operator_df_dt_parent_class)->initialize (op,
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
		             CPG_NETWORK_LOAD_ERROR,
		             CPG_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `df_dt' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	df_dt = CPG_OPERATOR_DF_DT (op);
	df_dt->priv->expression = g_object_ref_sink (expressions[0]->data);

	df_dt->priv->order = 1;

	if (expressions[0]->next)
	{
		df_dt->priv->order = rint (cpg_expression_evaluate (expressions[0]->next->data));

		if (df_dt->priv->order < 0)
		{
			df_dt->priv->order = 0;
		}
	}

	df_dt->priv->derived = cpg_symbolic_derive (df_dt->priv->expression,
	                                            NULL,
	                                            NULL,
	                                            NULL,
	                                            df_dt->priv->order,
	                                            CPG_SYMBOLIC_DERIVE_NONE,
	                                            error);

	g_object_ref_sink (df_dt->priv->derived);

	return df_dt->priv->derived != NULL;
}

static void
cpg_operator_df_dt_execute (CpgOperator *op,
                           CpgStack    *stack)
{
	CpgOperatorDfDt *d;

	d = (CpgOperatorDfDt *)op;

	if (d->priv->derived)
	{
		cpg_stack_push (stack,
		                cpg_expression_evaluate (d->priv->derived));
	}
	else
	{
		cpg_stack_push (stack, 0);
	}
}

static void
cpg_operator_df_dt_finalize (GObject *object)
{
	CpgOperatorDfDt *df_dt;

	df_dt = CPG_OPERATOR_DF_DT (object);

	if (df_dt->priv->expression)
	{
		g_object_unref (df_dt->priv->expression);
	}

	if (df_dt->priv->derived)
	{
		g_object_unref (df_dt->priv->derived);
	}

	G_OBJECT_CLASS (cpg_operator_df_dt_parent_class)->finalize (object);
}

static void
cpg_operator_df_dt_set_property (GObject      *object,
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
cpg_operator_df_dt_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CpgOperatorDfDt *self = CPG_OPERATOR_DF_DT (object);

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
cpg_operator_df_dt_equal (CpgOperator *op,
                            CpgOperator *other)
{
	CpgOperatorDfDt *df_dt;
	CpgOperatorDfDt *odel;

	if (!CPG_IS_OPERATOR_DF_DT (other))
	{
		return FALSE;
	}

	df_dt = CPG_OPERATOR_DF_DT (op);
	odel = CPG_OPERATOR_DF_DT (other);

	if (df_dt->priv->order != odel->priv->order)
	{
		return FALSE;
	}

	if (!cpg_expression_equal (df_dt->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cpg_operator_df_dt_reset_cache (CpgOperator *operator)
{
	CpgOperatorDfDt *self;

	((CpgOperatorClass *)cpg_operator_df_dt_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CpgOperatorDfDt *)operator;

	if (self->priv->derived)
	{
		cpg_expression_reset_cache (self->priv->derived);
	}
}

static void
cpg_operator_df_dt_reset (CpgOperator *operator)
{
	CpgOperatorDfDt *self;

	CPG_OPERATOR_CLASS (cpg_operator_df_dt_parent_class)->reset (operator);

	self = CPG_OPERATOR_DF_DT (operator);

	if (self->priv->derived)
	{
		cpg_expression_reset (self->priv->derived);
	}
}

static CpgOperator *
cpg_operator_df_dt_copy (CpgOperator *op)
{
	CpgOperatorDfDt *df_dt;
	CpgOperatorDfDt *ret;

	df_dt = CPG_OPERATOR_DF_DT (op);

	ret = CPG_OPERATOR_DF_DT (g_object_new (CPG_TYPE_OPERATOR_DF_DT, NULL));

	CPG_OPERATOR_CLASS (cpg_operator_df_dt_parent_class)->initialize (CPG_OPERATOR (ret),
	                                                                 cpg_operator_all_expressions (op),
	                                                                 cpg_operator_num_expressions (op),
	                                                                 cpg_operator_all_indices (op),
	                                                                 cpg_operator_num_indices (op),
	                                                                 cpg_operator_get_num_arguments (op),
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

	return CPG_OPERATOR (ret);
}

static void
cpg_operator_df_dt_class_init (CpgOperatorDfDtClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_df_dt_finalize;

	object_class->get_property = cpg_operator_df_dt_get_property;
	object_class->set_property = cpg_operator_df_dt_set_property;

	op_class->get_name = cpg_operator_df_dt_get_name;
	op_class->execute = cpg_operator_df_dt_execute;
	op_class->initialize = cpg_operator_df_dt_initialize;
	op_class->equal = cpg_operator_df_dt_equal;
	op_class->reset_cache = cpg_operator_df_dt_reset_cache;
	op_class->reset = cpg_operator_df_dt_reset;
	op_class->copy = cpg_operator_df_dt_copy;

	g_type_class_add_private (object_class, sizeof(CpgOperatorDfDtPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_DERIVED,
	                                 g_param_spec_object ("derived",
	                                                      "Derived",
	                                                      "Derived",
	                                                      CPG_TYPE_EXPRESSION,
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
cpg_operator_df_dt_init (CpgOperatorDfDt *self)
{
	self->priv = CPG_OPERATOR_DF_DT_GET_PRIVATE (self);
}

CpgOperatorDfDt *
cpg_operator_df_dt_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_DF_DT, NULL);
}

/**
 * cpg_operator_df_dt_get_expression:
 * @df_dt: A #CpgOperatorDfDt
 *
 * Get the expression to be df_dt.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_df_dt_get_expression (CpgOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DF_DT (df_dt), NULL);

	return df_dt->priv->expression;
}

/**
 * cpg_operator_df_dt_get_derived:
 * @df_dt: A #CpgOperatorDfDt
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_df_dt_get_derived (CpgOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DF_DT (df_dt), NULL);

	return df_dt->priv->derived;
}

/**
 * cpg_operator_df_dt_get_order:
 * @df_dt: A #CpgOperatorDfDt
 *
 * Get the order.
 *
 * Returns: The order
 *
 **/
gint
cpg_operator_df_dt_get_order (CpgOperatorDfDt *df_dt)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DF_DT (df_dt), 0.0);

	return df_dt->priv->order;
}
