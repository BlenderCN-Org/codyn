/*
 * cpg-operator-diff.c
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

#include "cpg-operator-diff.h"
#include "cpg-operator.h"
#include "cpg-property.h"
#include "cpg-usable.h"
#include "cpg-integrator.h"
#include "cpg-symbolic.h"

#include "instructions/cpg-instruction-property.h"

#include <math.h>

/**
 * SECTION:cpg-operator-diff
 * @short_description: Math operator for diff evaluation of an expression
 *
 * The #CpgOperatorDiff is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * diff value of its argument (which can be an arbitrary expression).
 *
 */

#define CPG_OPERATOR_DIFF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_DIFF, CpgOperatorDiffPrivate))

struct _CpgOperatorDiffPrivate
{
	CpgExpression *expression;
	CpgExpression *derived;

	gint order;
};

G_DEFINE_TYPE (CpgOperatorDiff,
               cpg_operator_diff,
               CPG_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED,
	PROP_ORDER
};

static gchar *
cpg_operator_diff_get_name ()
{
	return g_strdup ("diff");
}

static CpgProperty *
derived_property (CpgExpression *expr)
{
	GSList const *instr;

	instr = cpg_expression_get_instructions (expr);

	if (instr->next || !CPG_IS_INSTRUCTION_PROPERTY (instr->data))
	{
		return NULL;
	}

	return cpg_instruction_property_get_property (instr->data);
}

static gboolean
cpg_operator_diff_initialize (CpgOperator   *op,
                              GSList const  *expressions,
                              gint           num_arguments,
                              GError       **error)
{
	CpgOperatorDiff *diff;
	CpgProperty *prop = NULL;

	if (!CPG_OPERATOR_CLASS (cpg_operator_diff_parent_class)->initialize (op,
	                                                                      expressions,
	                                                                      num_arguments,
	                                                                      error))
	{
		return FALSE;
	}

	diff = CPG_OPERATOR_DIFF (op);
	diff->priv->expression = g_object_ref (expressions->data);

	diff->priv->order = 1;

	if (expressions->next)
	{
		prop = derived_property (expressions->next->data);

		if (expressions->next->next)
		{
			diff->priv->order = rint (cpg_expression_evaluate (expressions->next->next->data));
		}
	}

	diff->priv->derived = cpg_symbolic_derive (diff->priv->expression,
	                                           NULL,
	                                           NULL,
	                                           prop,
	                                           diff->priv->order,
	                                           CPG_SYMBOLIC_DERIVE_NONE,
	                                           error);

	return diff->priv->derived != NULL;
}

static void
cpg_operator_diff_execute (CpgOperator *op,
                           CpgStack    *stack)
{
	CpgOperatorDiff *d;

	d = (CpgOperatorDiff *)op;

	if (d->priv->derived)
	{
		cpg_stack_push (stack, cpg_expression_evaluate (d->priv->derived));
	}
	else
	{
		cpg_stack_push (stack, 0);
	}
}

static gint
cpg_operator_diff_validate_num_arguments (gint numsym, gint num)
{
	return numsym >= 1 && numsym <= 3;
}

static void
cpg_operator_diff_finalize (GObject *object)
{
	CpgOperatorDiff *diff;

	diff = CPG_OPERATOR_DIFF (object);

	g_object_unref (diff->priv->expression);

	if (diff->priv->derived)
	{
		g_object_unref (diff->priv->derived);
	}

	G_OBJECT_CLASS (cpg_operator_diff_parent_class)->finalize (object);
}

static void
cpg_operator_diff_set_property (GObject      *object,
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
cpg_operator_diff_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CpgOperatorDiff *self = CPG_OPERATOR_DIFF (object);

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
cpg_operator_diff_equal (CpgOperator *op,
                            CpgOperator *other)
{
	CpgOperatorDiff *diff;
	CpgOperatorDiff *odel;

	if (!CPG_IS_OPERATOR_DIFF (other))
	{
		return FALSE;
	}

	diff = CPG_OPERATOR_DIFF (op);
	odel = CPG_OPERATOR_DIFF (other);

	if (diff->priv->order != odel->priv->order)
	{
		return FALSE;
	}

	if (!cpg_expression_equal (diff->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cpg_operator_diff_reset_cache (CpgOperator *operator)
{
	CpgOperatorDiff *self;

	CPG_OPERATOR_CLASS (cpg_operator_diff_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CpgOperatorDiff *)operator;

	if (self->priv->derived)
	{
		cpg_expression_reset_cache (self->priv->derived);
	}
}

static void
cpg_operator_diff_reset (CpgOperator *operator)
{
	CpgOperatorDiff *self;

	CPG_OPERATOR_CLASS (cpg_operator_diff_parent_class)->reset (operator);

	self = CPG_OPERATOR_DIFF (operator);

	if (self->priv->derived)
	{
		cpg_expression_reset (self->priv->derived);
	}
}

static void
cpg_operator_diff_class_init (CpgOperatorDiffClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_diff_finalize;

	object_class->get_property = cpg_operator_diff_get_property;
	object_class->set_property = cpg_operator_diff_set_property;

	op_class->get_name = cpg_operator_diff_get_name;
	op_class->execute = cpg_operator_diff_execute;
	op_class->initialize = cpg_operator_diff_initialize;
	op_class->validate_num_arguments = cpg_operator_diff_validate_num_arguments;
	op_class->equal = cpg_operator_diff_equal;
	op_class->reset_cache = cpg_operator_diff_reset_cache;
	op_class->reset = cpg_operator_diff_reset;

	g_type_class_add_private (object_class, sizeof(CpgOperatorDiffPrivate));

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
cpg_operator_diff_init (CpgOperatorDiff *self)
{
	self->priv = CPG_OPERATOR_DIFF_GET_PRIVATE (self);
}

CpgOperatorDiff *
cpg_operator_diff_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_DIFF, NULL);
}

/**
 * cpg_operator_diff_get_expression:
 * @diff: A #CpgOperatorDiff
 *
 * Get the expression to be diff.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_diff_get_expression (CpgOperatorDiff *diff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DIFF (diff), NULL);

	return diff->priv->expression;
}

/**
 * cpg_operator_diff_get_derived:
 * @diff: A #CpgOperatorDiff
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_diff_get_derived (CpgOperatorDiff *diff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DIFF (diff), NULL);

	return diff->priv->derived;
}

/**
 * cpg_operator_diff_get_order:
 * @diff: A #CpgOperatorDiff
 *
 * Get the order.
 *
 * Returns: The order
 *
 **/
gint
cpg_operator_diff_get_order (CpgOperatorDiff *diff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_DIFF (diff), 0.0);

	return diff->priv->order;
}
