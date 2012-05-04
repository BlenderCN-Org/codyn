/*
 * cdn-operator-simplify.c
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

#include "cdn-operator-simplify.h"
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
 * SECTION:cdn-operator-simplify
 * @short_description: Math operator for simplify evaluation of an expression
 *
 * The #CdnOperatorSimplify is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * simplify value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_SIMPLIFY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_SIMPLIFY, CdnOperatorSimplifyPrivate))

struct _CdnOperatorSimplifyPrivate
{
	CdnFunction *function;
};

G_DEFINE_TYPE (CdnOperatorSimplify,
               cdn_operator_simplify,
               CDN_TYPE_OPERATOR)

static gchar *
cdn_operator_simplify_get_name ()
{
	return g_strdup ("simplify");
}

static CdnFunction *
derived_function (CdnExpression      *expr,
                  CdnStackArgs const *argdim)
{
	GSList const *instr;
	CdnFunction *ret = NULL;

	instr = cdn_expression_get_instructions (expr);

	if (instr->next)
	{
		return NULL;
	}

	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr->data))
	{
		ret = cdn_instruction_custom_function_ref_get_function (instr->data);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instr->data))
	{
		CdnOperator *op;

		op = cdn_instruction_custom_operator_ref_get_operator (instr->data);
		ret = cdn_operator_get_primary_function (op);
	}

	if (ret)
	{
		ret = cdn_function_for_dimension (ret,
		                                  argdim);
	}

	return ret;
}

static void
replace_args (CdnFunction   *func,
              CdnFunction   *nf,
              CdnExpression *expr)
{
	GSList const *instr;

	instr = cdn_expression_get_instructions (expr);

	while (instr)
	{
		CdnInstruction *i = instr->data;
		CdnVariable *prop;

		instr = g_slist_next (instr);

		if (!CDN_IS_INSTRUCTION_VARIABLE (i))
		{
			continue;
		}

		prop = cdn_instruction_variable_get_variable (CDN_INSTRUCTION_VARIABLE (i));

		if (cdn_variable_get_object (prop) == CDN_OBJECT (func))
		{
			prop = cdn_object_get_variable (CDN_OBJECT (nf),
			                                cdn_variable_get_name (prop));

			if (prop)
			{
				cdn_instruction_variable_set_variable (CDN_INSTRUCTION_VARIABLE (i),
				                                       prop);
			}
		}
	}
}

static gboolean
cdn_operator_simplify_initialize (CdnOperator         *op,
                                  GSList const       **expressions,
                                  gint                 num_expressions,
                                  GSList const       **indices,
                                  gint                 num_indices,
                                  CdnStackArgs const  *argdim,
                                  CdnCompileContext   *context,
                                  GError             **error)
{
	CdnOperatorSimplify *simplify;
	CdnFunction *func;
	CdnFunction *nf = NULL;
	gchar *s;
	CdnExpressionTreeIter *simplified;
	CdnExpressionTreeIter *iter;
	CdnExpression *expr;

	if (!CDN_OPERATOR_CLASS (cdn_operator_simplify_parent_class)->initialize (op,
	                                                                          expressions,
	                                                                          num_expressions,
	                                                                          indices,
	                                                                          num_indices,
	                                                                          argdim,
	                                                                          context,
	                                                                          error))
	{
		return FALSE;
	}

	if (num_expressions != 1 || expressions[0]->next)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `simplify' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	simplify = CDN_OPERATOR_SIMPLIFY (op);

	func = derived_function (expressions[0]->data,
	                         argdim);

	if (!func)
	{
		g_set_error (error,
		             CDN_OPERATOR_ERROR,
		             CDN_OPERATOR_ERROR_INVALID,
		             "Expected function reference but got `%s'",
		             cdn_expression_get_as_string (expressions[0]->data));

		return FALSE;
	}

	if (!cdn_object_is_compiled (CDN_OBJECT (func)))
	{
		CdnCompileError *err;

		err = cdn_compile_error_new ();

		if (!cdn_object_compile (CDN_OBJECT (func), NULL, err))
		{
			if (error)
			{
				*error = g_error_copy (cdn_compile_error_get_error (err));
			}

			g_object_unref (err);

			return FALSE;
		}

		g_object_unref (err);
	}

	// Here we are going to generate a new function with represents
	s = g_strconcat (cdn_object_get_id (CDN_OBJECT (func)),
	                 "_simplified",
	                 NULL);

	nf = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (func)));
	cdn_object_set_id (CDN_OBJECT (nf), s);
	g_free (s);

	cdn_function_set_expression (nf, cdn_expression_new0 ());

	iter = cdn_expression_tree_iter_new (cdn_function_get_expression (func));

	simplified = cdn_expression_tree_iter_simplify (iter);

	expr = cdn_expression_tree_iter_to_expression (simplified);
	cdn_expression_tree_iter_free (simplified);

	// Replace args
	replace_args (func, nf, expr);

	cdn_function_set_expression (nf, expr);
	simplify->priv->function = nf;

	return TRUE;
}

static void
cdn_operator_simplify_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_operator_simplify_parent_class)->finalize (object);
}

static CdnFunction *
cdn_operator_simplify_get_function (CdnOperator *op,
                                    gint        *idx,
                                    gint         num)
{
	return CDN_OPERATOR_SIMPLIFY (op)->priv->function;
}

static void
cdn_operator_simplify_foreach_function (CdnOperator            *op,
                                        CdnForeachFunctionFunc  func,
                                        gpointer                data)
{
	func (CDN_OPERATOR_SIMPLIFY (op)->priv->function, data);
}

static CdnOperator *
cdn_operator_simplify_copy (CdnOperator *op)
{
	CdnOperatorSimplify *simplify;
	CdnOperatorSimplify *ret;

	simplify = CDN_OPERATOR_SIMPLIFY (op);

	ret = CDN_OPERATOR_SIMPLIFY (g_object_new (CDN_TYPE_OPERATOR_SIMPLIFY, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_simplify_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                                     cdn_operator_all_expressions (op),
	                                                                     cdn_operator_num_expressions (op),
	                                                                     cdn_operator_all_indices (op),
	                                                                     cdn_operator_num_indices (op),
	                                                                     cdn_operator_get_arguments_dimension (op),
	                                                                     NULL,
	                                                                     NULL);

	if (simplify->priv->function)
	{
		ret->priv->function = g_object_ref (CDN_OBJECT (simplify->priv->function));
	}

	return CDN_OPERATOR (ret);
}

static void
cdn_operator_simplify_class_init (CdnOperatorSimplifyClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_simplify_finalize;

	op_class->get_name = cdn_operator_simplify_get_name;
	op_class->initialize = cdn_operator_simplify_initialize;
	op_class->get_function = cdn_operator_simplify_get_function;
	op_class->foreach_function = cdn_operator_simplify_foreach_function;
	op_class->copy = cdn_operator_simplify_copy;

	g_type_class_add_private (object_class, sizeof(CdnOperatorSimplifyPrivate));
}

static void
cdn_operator_simplify_init (CdnOperatorSimplify *self)
{
	self->priv = CDN_OPERATOR_SIMPLIFY_GET_PRIVATE (self);
}

CdnOperatorSimplify *
cdn_operator_simplify_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_SIMPLIFY, NULL);
}
