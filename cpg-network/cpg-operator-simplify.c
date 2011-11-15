/*
 * cpg-operator-simplify.c
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

#include "cpg-operator-simplify.h"
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
 * SECTION:cpg-operator-simplify
 * @short_description: Math operator for simplify evaluation of an expression
 *
 * The #CpgOperatorSimplify is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * simplify value of its argument (which can be an arbitrary expression).
 *
 */

#define CPG_OPERATOR_SIMPLIFY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_SIMPLIFY, CpgOperatorSimplifyPrivate))

struct _CpgOperatorSimplifyPrivate
{
	CpgExpression *expression;
	CpgExpression *derived;
	CpgFunction *function;
};

G_DEFINE_TYPE (CpgOperatorSimplify,
               cpg_operator_simplify,
               CPG_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED
};

static gchar *
cpg_operator_simplify_get_name ()
{
	return g_strdup ("simplify");
}

static CpgFunction *
derived_function (CpgExpression *expr)
{
	GSList const *instr;

	instr = cpg_expression_get_instructions (expr);

	if (instr->next)
	{
		return NULL;
	}

	if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr->data))
	{
		return cpg_instruction_custom_function_ref_get_function (instr->data);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instr->data))
	{
		CpgOperator *op;

		op = cpg_instruction_custom_operator_ref_get_operator (instr->data);
		return cpg_operator_get_primary_function (op);
	}

	return NULL;
}

static void
replace_args (CpgFunction   *func,
              CpgFunction   *nf,
              CpgExpression *expr)
{
	GSList const *instr;

	instr = cpg_expression_get_instructions (expr);

	while (instr)
	{
		CpgInstruction *i = instr->data;
		CpgProperty *prop;

		instr = g_slist_next (instr);

		if (!CPG_IS_INSTRUCTION_PROPERTY (i))
		{
			continue;
		}

		prop = cpg_instruction_property_get_property (CPG_INSTRUCTION_PROPERTY (i));

		if (cpg_property_get_object (prop) == CPG_OBJECT (func))
		{
			prop = cpg_object_get_property (CPG_OBJECT (nf),
			                                cpg_property_get_name (prop));

			if (prop)
			{
				cpg_instruction_property_set_property (CPG_INSTRUCTION_PROPERTY (i),
				                                       prop);
			}
		}
	}
}

static gboolean
cpg_operator_simplify_initialize (CpgOperator   *op,
                                  GSList const **expressions,
                                  gint           num_expressions,
                                  GSList const **indices,
                                  gint           num_indices,
                                  gint           num_arguments,
                                  GError       **error)
{
	CpgOperatorSimplify *simplify;
	CpgFunction *func;
	CpgFunction *nf = NULL;
	gchar *s;

	if (!CPG_OPERATOR_CLASS (cpg_operator_simplify_parent_class)->initialize (op,
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
		             "The operator `simplify' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	simplify = CPG_OPERATOR_SIMPLIFY (op);
	simplify->priv->expression = g_object_ref_sink (expressions[0]->data);

	func = derived_function (expressions[0]->data);

	if (!func)
	{
		g_set_error (error,
		             CPG_OPERATOR_ERROR,
		             CPG_OPERATOR_ERROR_INVALID,
		             "Expected function reference but got `%s'",
		             cpg_expression_get_as_string (expressions[0]->data));

		return FALSE;
	}

	// Here we are going to generate a new function with represents
	s = g_strconcat (cpg_object_get_id (CPG_OBJECT (func)),
	                 "_simplified",
	                 NULL);

	nf = CPG_FUNCTION (cpg_object_copy (CPG_OBJECT (func)));
	cpg_object_set_id (CPG_OBJECT (nf), s);
	g_free (s);

	cpg_function_set_expression (nf, cpg_expression_new0 ());

	simplify->priv->derived = cpg_symbolic_simplify (cpg_function_get_expression (func));

	// Replace args
	replace_args (func, nf, simplify->priv->derived);

	cpg_function_set_expression (nf, simplify->priv->derived);
	simplify->priv->function = nf;

	return simplify->priv->derived != NULL;
}

static void
cpg_operator_simplify_execute (CpgOperator *op,
                               CpgStack    *stack)
{
	CpgOperatorSimplify *d;

	d = (CpgOperatorSimplify *)op;

	if (d->priv->function)
	{
		cpg_function_execute (d->priv->function,
		                      cpg_operator_get_num_arguments (op),
		                      stack);
	}
	else if (d->priv->derived)
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
cpg_operator_simplify_finalize (GObject *object)
{
	CpgOperatorSimplify *simplify;

	simplify = CPG_OPERATOR_SIMPLIFY (object);

	if (simplify->priv->expression)
	{
		g_object_unref (simplify->priv->expression);
	}

	if (simplify->priv->derived)
	{
		g_object_unref (simplify->priv->derived);
	}

	if (simplify->priv->function)
	{
		g_object_unref (simplify->priv->function);
	}

	G_OBJECT_CLASS (cpg_operator_simplify_parent_class)->finalize (object);
}

static void
cpg_operator_simplify_set_property (GObject      *object,
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
cpg_operator_simplify_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CpgOperatorSimplify *self = CPG_OPERATOR_SIMPLIFY (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
			break;
		case PROP_DERIVED:
			g_value_set_object (value, self->priv->derived);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cpg_operator_simplify_equal (CpgOperator *op,
                            CpgOperator *other)
{
	CpgOperatorSimplify *simplify;
	CpgOperatorSimplify *odel;

	if (!CPG_IS_OPERATOR_SIMPLIFY (other))
	{
		return FALSE;
	}

	simplify = CPG_OPERATOR_SIMPLIFY (op);
	odel = CPG_OPERATOR_SIMPLIFY (other);

	if (!cpg_expression_equal (simplify->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cpg_operator_simplify_reset_cache (CpgOperator *operator)
{
	CpgOperatorSimplify *self;

	CPG_OPERATOR_CLASS (cpg_operator_simplify_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CpgOperatorSimplify *)operator;

	if (self->priv->derived)
	{
		cpg_expression_reset_cache (self->priv->derived);
	}
}

static void
cpg_operator_simplify_reset (CpgOperator *operator)
{
	CpgOperatorSimplify *self;

	CPG_OPERATOR_CLASS (cpg_operator_simplify_parent_class)->reset (operator);

	self = CPG_OPERATOR_SIMPLIFY (operator);

	if (self->priv->derived)
	{
		cpg_expression_reset (self->priv->derived);
	}
}

static CpgFunction *
cpg_operator_simplify_get_function (CpgOperator *op,
                                    gint        *idx,
                                    gint         num)
{
	return CPG_OPERATOR_SIMPLIFY (op)->priv->function;
}

static CpgOperator *
cpg_operator_simplify_copy (CpgOperator *op)
{
	CpgOperatorSimplify *simplify;
	CpgOperatorSimplify *ret;

	simplify = CPG_OPERATOR_SIMPLIFY (op);

	ret = CPG_OPERATOR_SIMPLIFY (g_object_new (CPG_TYPE_OPERATOR_SIMPLIFY, NULL));

	CPG_OPERATOR_CLASS (cpg_operator_simplify_parent_class)->initialize (CPG_OPERATOR (ret),
	                                                                 cpg_operator_all_expressions (op),
	                                                                 cpg_operator_num_expressions (op),
	                                                                 cpg_operator_all_indices (op),
	                                                                 cpg_operator_num_indices (op),
	                                                                 cpg_operator_get_num_arguments (op),
	                                                                 NULL);

	if (simplify->priv->expression)
	{
		ret->priv->expression = g_object_ref_sink (simplify->priv->expression);
	}

	if (simplify->priv->derived)
	{
		ret->priv->derived = g_object_ref_sink (simplify->priv->derived);
	}

	if (simplify->priv->function)
	{
		ret->priv->function = g_object_ref (CPG_OBJECT (simplify->priv->function));
	}

	return CPG_OPERATOR (ret);
}

static void
cpg_operator_simplify_class_init (CpgOperatorSimplifyClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_simplify_finalize;

	object_class->get_property = cpg_operator_simplify_get_property;
	object_class->set_property = cpg_operator_simplify_set_property;

	op_class->get_name = cpg_operator_simplify_get_name;
	op_class->execute = cpg_operator_simplify_execute;
	op_class->initialize = cpg_operator_simplify_initialize;
	op_class->equal = cpg_operator_simplify_equal;
	op_class->reset_cache = cpg_operator_simplify_reset_cache;
	op_class->reset = cpg_operator_simplify_reset;
	op_class->get_function = cpg_operator_simplify_get_function;
	op_class->copy = cpg_operator_simplify_copy;

	g_type_class_add_private (object_class, sizeof(CpgOperatorSimplifyPrivate));

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
}

static void
cpg_operator_simplify_init (CpgOperatorSimplify *self)
{
	self->priv = CPG_OPERATOR_SIMPLIFY_GET_PRIVATE (self);
}

CpgOperatorSimplify *
cpg_operator_simplify_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_SIMPLIFY, NULL);
}

/**
 * cpg_operator_simplify_get_expression:
 * @simplify: A #CpgOperatorSimplify
 *
 * Get the expression to be simplify.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_simplify_get_expression (CpgOperatorSimplify *simplify)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_SIMPLIFY (simplify), NULL);

	return simplify->priv->expression;
}

/**
 * cpg_operator_simplify_get_derived:
 * @simplify: A #CpgOperatorSimplify
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_simplify_get_derived (CpgOperatorSimplify *simplify)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_SIMPLIFY (simplify), NULL);

	return simplify->priv->derived;
}
