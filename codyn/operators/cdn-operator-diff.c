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

#include "cdn-operator-diff.h"
#include "cdn-operator.h"
#include "cdn-variable.h"
#include "cdn-usable.h"
#include "integrators/cdn-integrator.h"
#include "cdn-function.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-network.h"

#include "instructions/cdn-instructions.h"

#include <math.h>

#define CDN_OPERATOR_DIFF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_DIFF, CdnOperatorDiffPrivate))

struct _CdnOperatorDiffPrivate
{
	CdnFunction *function;

	gint order;
};

G_DEFINE_TYPE (CdnOperatorDiff,
               cdn_operator_diff,
               CDN_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_ORDER
};

static gchar *
cdn_operator_diff_get_name ()
{
	return g_strdup ("diff");
}

static CdnFunction *
derived_function (CdnExpression       *expr,
                  CdnStackArgs const  *argdim,
                  GError             **error)
{
	GSList const *instr;
	CdnFunction *func = NULL;

	instr = cdn_expression_get_instructions (expr);

	if (instr->next)
	{
		return NULL;
	}

	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr->data))
	{
		func = cdn_instruction_custom_function_ref_get_function (instr->data);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instr->data))
	{
		CdnOperator *op;

		op = cdn_instruction_custom_operator_ref_get_operator (instr->data);
		func = cdn_operator_get_primary_function (op);
	}

	if (func)
	{
		gint nargs;
		gint nopt;
		gint nimpl;

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
				return NULL;
			}

			g_object_unref (err);
		}

		// Verify if we could actually call the resulting function with
		// the provided number of arguments
		nargs = cdn_function_get_n_arguments (func);
		nimpl = cdn_function_get_n_implicit (func);
		nopt = cdn_function_get_n_optional (func);

		if (argdim->num < (nargs - nimpl - nopt))
		{
			g_set_error (error,
			             CDN_COMPILE_ERROR_TYPE,
			             CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			             "Expected at least %d function arguments but only got %d for derivative of function `%s'",
			             nargs - nimpl - nopt,
			             argdim->num,
			             cdn_object_get_id (CDN_OBJECT (func)));

			return NULL;
		}

		func = cdn_function_for_dimension (func, argdim);
	}
	else
	{
		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use the dt[] operator for deriving expressions",
		             cdn_expression_get_as_string (expr));
	}

	return func;
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
                    GSList const  *towards,
                    CdnFunction  **func,
                    CdnStackArgs const *argdim,
                    GSList       **syms,
                    gint          *order,
                    GError       **error)
{
	CdnExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data, argdim, error);

	if (!*func)
	{
		return FALSE;
	}

	*order = 1;

	if (expressions->next)
	{
		*order = cdn_expression_evaluate (expressions->next->data);
	}

	*syms = NULL;

	while (towards)
	{
		CdnFunctionArgument *arg;

		arg = derived_arg (*func, towards->data);

		if (!arg)
		{
			g_set_error (error,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
			             "Expected function variable but got `%s' for diff of `%s'",
			             cdn_expression_get_as_string (towards->data),
			             cdn_expression_get_as_string (expr));

			g_slist_free (*syms);
			return FALSE;
		}
		else
		{
			*syms = g_slist_prepend (*syms, arg);
		}

		towards = g_slist_next (towards);
	}

	if (!*syms)
	{
		// By default add all explicit properties of the function as syms
		GList const *args;

		args = cdn_function_get_arguments (*func);

		while (args)
		{
			if (cdn_function_argument_get_explicit (args->data))
			{
				*syms = g_slist_prepend (*syms, args->data);
			}

			args = g_list_next (args);
		}
	}

	*syms = g_slist_reverse (*syms);
	return TRUE;
}

static gboolean
cdn_operator_diff_initialize (CdnOperator        *op,
                              GSList const      **expressions,
                              gint                num_expressions,
                              GSList const      **indices,
                              gint                num_indices,
                              CdnStackArgs const *argdim,
                              CdnCompileContext  *context,
                              GError            **error)
{
	CdnOperatorDiff *diff;
	CdnFunction *func;
	CdnFunction *nf = NULL;
	GSList *towards;

	if (!CDN_OPERATOR_CLASS (cdn_operator_diff_parent_class)->initialize (op,
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

	if (num_expressions <= 0 ||
	    num_expressions > 2 ||
	    (expressions[0]->next && expressions[0]->next->next))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `diff' expects arguments [Func{,order}{;<towards>}] {optional} <list>");

		return FALSE;
	}

	diff = CDN_OPERATOR_DIFF (op);
	diff->priv->order = 1;

	if (!validate_arguments (expressions[0],
	                         num_expressions > 1 ? expressions[1] : NULL,
	                         &func,
	                         argdim,
	                         &towards,
	                         &diff->priv->order,
	                         error))
	{
		return FALSE;
	}

	nf = cdn_function_get_derivative (func,
	                                  towards,
	                                  diff->priv->order,
	                                  CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY,
	                                  error);

	g_slist_free (towards);

	diff->priv->function = nf;
	return diff->priv->function != NULL;
}

static void
cdn_operator_diff_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_operator_diff_parent_class)->finalize (object);
}

static CdnFunction *
cdn_operator_diff_get_function (CdnOperator *op,
                                gint        *idx,
                                gint         numidx)
{
	CdnOperatorDiff *diff;

	diff = (CdnOperatorDiff *)op;

	return diff->priv->function;
}

static void
cdn_operator_diff_foreach_function (CdnOperator            *op,
                                    CdnForeachFunctionFunc  func,
                                    gpointer                data)
{
	CdnOperatorDiff *diff;

	diff = (CdnOperatorDiff *)op;

	if (diff->priv->function)
	{
		func (diff->priv->function, data);
	}
}

static CdnOperator *
cdn_operator_diff_copy (CdnOperator *op)
{
	CdnOperatorDiff *diff;
	CdnOperatorDiff *ret;

	diff = CDN_OPERATOR_DIFF (op);

	ret = CDN_OPERATOR_DIFF (g_object_new (CDN_TYPE_OPERATOR_DIFF, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_diff_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                                 cdn_operator_all_expressions (op),
	                                                                 cdn_operator_num_expressions (op),
	                                                                 cdn_operator_all_indices (op),
	                                                                 cdn_operator_num_indices (op),
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
cdn_operator_diff_class_init (CdnOperatorDiffClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_diff_finalize;

	op_class->get_name = cdn_operator_diff_get_name;
	op_class->initialize = cdn_operator_diff_initialize;
	op_class->get_function = cdn_operator_diff_get_function;
	op_class->foreach_function = cdn_operator_diff_foreach_function;
	op_class->copy = cdn_operator_diff_copy;

	g_type_class_add_private (object_class, sizeof(CdnOperatorDiffPrivate));
}

static void
cdn_operator_diff_init (CdnOperatorDiff *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_operator_diff_get_instance_private (self);
	self->priv = CDN_OPERATOR_DIFF_GET_PRIVATE (self);
}

/**
 * cdn_operator_diff_new:
 *
 * Create a new diff operator
 *
 * Returns: (transfer full): a new diff operator
 */
CdnOperatorDiff *
cdn_operator_diff_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_DIFF, NULL);
}
