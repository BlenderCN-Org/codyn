/*
 * cdn-operator-diff.c
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

#include "cdn-operator-pdiff.h"
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
 * SECTION:cdn-operator-diff
 * @short_description: Math operator for diff evaluation of an expression
 *
 * The #CdnOperatorPDiff is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * diff value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_PDIFF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_PDIFF, CdnOperatorPDiffPrivate))

struct _CdnOperatorPDiffPrivate
{
	CdnFunction *function;
};

G_DEFINE_TYPE (CdnOperatorPDiff,
               cdn_operator_pdiff,
               CDN_TYPE_OPERATOR)

static gchar *
cdn_operator_pdiff_get_name ()
{
	return g_strdup ("pdiff");
}

static gboolean
cdn_operator_pdiff_responds_to (gchar const *name)
{
	return g_strcmp0 (name, "\xe2\x88\x82") == 0;
}

static CdnFunction *
derived_function (CdnExpression *expr)
{
	GSList const *instr;

	instr = cdn_expression_get_instructions (expr);

	if (instr->next)
	{
		return NULL;
	}

	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr->data))
	{
		return cdn_instruction_custom_function_ref_get_function (instr->data);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instr->data))
	{
		CdnOperator *op;

		op = cdn_instruction_custom_operator_ref_get_operator (instr->data);
		return cdn_operator_get_primary_function (op);
	}

	return NULL;
}

static CdnFunctionArgument *
derived_arg (CdnFunction   *func,
             CdnExpression *expr)
{
	GSList const *instr;
	gchar const *pname;

	instr = cdn_expression_get_instructions (expr);

	if (instr->next || !CDN_IS_INSTRUCTION_VARIABLE (instr->data))
	{
		return NULL;
	}

	pname = cdn_variable_get_name (cdn_instruction_variable_get_variable (instr->data));
	return cdn_function_get_argument (func, pname);
}

static gboolean
validate_arguments (GSList const  *expressions,
                    GSList const  *towardsexpr,
                    CdnFunction  **func,
                    GSList       **towards,
                    gint          *order,
                    GError       **error)
{
	CdnExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data);

	if (!*func)
	{
		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use df_dt[] for deriving expressions",
		             cdn_expression_get_as_string (expressions->data));

		return FALSE;
	}

	*towards = NULL;

	while (towardsexpr)
	{
		CdnFunctionArgument *arg;

		arg = derived_arg (*func, towardsexpr->data);

		if (!arg)
		{
			g_set_error (error,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
			             "Expected partial function variable reference but got `%s'.",
			             cdn_expression_get_as_string (towardsexpr->data));

			return FALSE;
		}

		*towards = g_slist_prepend (*towards, arg);
		towardsexpr = g_slist_next (towardsexpr);
	}

	*towards = g_slist_reverse (*towards);

	if (expressions->next)
	{
		*order = cdn_expression_evaluate (expressions->next->data);
	}

	return TRUE;
}

static gboolean
cdn_operator_pdiff_initialize (CdnOperator        *op,
                               GSList const      **expressions,
                               gint                num_expressions,
                               GSList const      **indices,
                               gint                num_indices,
                               gint                num_arguments,
                               gint               *argdim,
                               CdnCompileContext  *context,
                               GError            **error)
{
	CdnOperatorPDiff *diff;
	CdnFunction *func;
	CdnFunction *nf = NULL;
	GSList *towards;
	gint order;

	if (!CDN_OPERATOR_CLASS (cdn_operator_pdiff_parent_class)->initialize (op,
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

	if (num_expressions != 2 ||
	    (expressions[0]->next && expressions[0]->next->next) ||
	    expressions[1]->next)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `pdiff' expects arguments [Func{, order};variable] {optional} <list>");

		return FALSE;
	}

	diff = CDN_OPERATOR_PDIFF (op);

	order = 1;

	if (!validate_arguments (expressions[0],
	                         num_expressions > 1 ? expressions[1] : NULL,
	                         &func,
	                         &towards,
	                         &order,
	                         error))
	{
		return FALSE;
	}

	nf = cdn_function_get_derivative (func,
	                                  towards,
	                                  order,
	                                  CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL |
	                                  CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY,
	                                  error);

	diff->priv->function = nf;

	g_slist_free (towards);
	return diff->priv->function != NULL;
}

static void
cdn_operator_pdiff_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_operator_pdiff_parent_class)->finalize (object);
}

static CdnFunction *
cdn_operator_pdiff_get_function (CdnOperator *op,
                                 gint        *idx,
                                 gint         numidx)
{
	CdnOperatorPDiff *pdiff;

	pdiff = (CdnOperatorPDiff *)op;
	return pdiff->priv->function;
}

static void
cdn_operator_pdiff_foreach_function (CdnOperator *op,
                                     CdnForeachFunctionFunc func,
                                     gpointer userdata)
{
	CdnOperatorPDiff *pdiff;

	pdiff = (CdnOperatorPDiff *)op;

	if (pdiff->priv->function)
	{
		func (pdiff->priv->function, userdata);
	}
}

static CdnOperator *
cdn_operator_pdiff_copy (CdnOperator *op)
{
	CdnOperatorPDiff *diff;
	CdnOperatorPDiff *ret;

	diff = CDN_OPERATOR_PDIFF (op);

	ret = CDN_OPERATOR_PDIFF (g_object_new (CDN_TYPE_OPERATOR_PDIFF, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_pdiff_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                                 cdn_operator_all_expressions (op),
	                                                                 cdn_operator_num_expressions (op),
	                                                                 cdn_operator_all_indices (op),
	                                                                 cdn_operator_num_indices (op),
	                                                                 cdn_operator_get_num_arguments (op),
	                                                                 cdn_operator_get_arguments_dimension (op),
	                                                                 NULL,
	                                                                 NULL);

	if (diff->priv->function)
	{
		ret->priv->function = g_object_ref (CDN_OBJECT (diff->priv->function));
	}

	return CDN_OPERATOR (ret);
}

static void
cdn_operator_pdiff_class_init (CdnOperatorPDiffClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_pdiff_finalize;

	op_class->get_name = cdn_operator_pdiff_get_name;
	op_class->initialize = cdn_operator_pdiff_initialize;
	op_class->get_function = cdn_operator_pdiff_get_function;
	op_class->foreach_function = cdn_operator_pdiff_foreach_function;
	op_class->copy = cdn_operator_pdiff_copy;
	op_class->responds_to = cdn_operator_pdiff_responds_to;

	g_type_class_add_private (object_class, sizeof(CdnOperatorPDiffPrivate));
}

static void
cdn_operator_pdiff_init (CdnOperatorPDiff *self)
{
	self->priv = CDN_OPERATOR_PDIFF_GET_PRIVATE (self);
}

CdnOperatorPDiff *
cdn_operator_pdiff_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_PDIFF, NULL);
}
