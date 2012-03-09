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
#include "cdn-math.h"
#include "cdn-debug.h"

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
derived_function (CdnExpression *expr,
                  gint           numargs,
                  gint          *argdim)
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
	else
	{
		return NULL;
	}

	return cdn_function_for_dimension (ret, numargs, argdim);
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
                    gint           numargs,
                    gint          *argdim,
                    GSList       **towards,
                    gint          *order,
                    GError       **error)
{
	CdnExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data, numargs, argdim);

	if (!*func)
	{
		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use df_dt[] for deriving expressions",
		             cdn_expression_get_as_string (expressions->data));

		return FALSE;
	}

	if (!cdn_object_is_compiled (CDN_OBJECT (*func)))
	{
		CdnCompileError *err;

		err = cdn_compile_error_new ();

		if (!cdn_object_compile (CDN_OBJECT (*func), NULL, err))
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

static GSList *
create_symbols (CdnFunction *function)
{
	GList const *args;
	GSList *ret = NULL;

	args = cdn_function_get_arguments (function);

	while (args)
	{
		CdnFunctionArgument *arg;

		arg = args->data;

		ret = g_slist_prepend (ret,
		                       g_object_ref (_cdn_function_argument_get_variable (arg)));

		args = g_list_next (args);
	}

	return ret;
}

static void
replace_args (CdnExpressionTreeIter *iter,
              CdnFunction           *origf,
              CdnFunction           *newf)
{
	GHashTable *mapping;
	GList const *args;

	mapping = g_hash_table_new_full (g_direct_hash,
	                                 g_direct_equal,
	                                 (GDestroyNotify)g_object_unref,
	                                 (GDestroyNotify)cdn_expression_tree_iter_free);

	args = cdn_function_get_arguments (origf);

	while (args)
	{
		CdnVariable *v;
		CdnExpressionTreeIter *it;
		gchar const *nm;
		CdnVariable *v2;

		v = _cdn_function_argument_get_variable (args->data);
		nm = cdn_function_argument_get_name (args->data);

		v2 = _cdn_function_argument_get_variable (cdn_function_get_argument (newf,
		                                                                     nm));

		it = cdn_expression_tree_iter_new_from_instruction_take (cdn_instruction_variable_new (v2));
		g_hash_table_insert (mapping, g_object_ref (v), it);

		args = g_list_next (args);
	}

	cdn_expression_tree_iter_substitute_hash (iter, mapping);
	g_hash_table_destroy (mapping);
}

static CdnFunction *
derive_jacobian (CdnOperatorPDiff  *pdiff,
                 CdnFunction       *func,
                 GSList            *tows,
                 gint               order,
                 GError           **error)
{
	CdnFunctionArgument *arg;
	gint numr;
	gint numc;
	gint fnumr;
	gint fnumc;
	gint num;
	gint i;
	CdnExpressionTreeIter *iter;
	CdnVariable *argv;
	GHashTable *towards;
	CdnVariable *dummy;
	GSList *syms;
	gboolean retval = TRUE;
	GSList *instructions = NULL;
	CdnFunction *nf;
	CdnFunctionArgument *nfarg;
	CdnVariable *nfargv;
	gint iargdim[4];
	gint *popdims;

	// For each dimension of the argument to derive towards, compute the
	// partial towards that dimension (this results in a jacobian matrix)
	arg = tows->data;

	cdn_function_get_dimension (func, &fnumr, &fnumc);

	cdn_function_argument_get_dimension (arg, &numr, &numc);
	argv = _cdn_function_argument_get_variable (arg);

	dummy = cdn_variable_new (cdn_variable_get_name (argv),
	                          cdn_expression_new0 (),
	                          cdn_variable_get_flags (argv));

	num = numr * numc;
	iter = cdn_expression_tree_iter_new (cdn_function_get_expression (func));

	popdims = g_new0 (gint, num * 2);

	syms = create_symbols (func);

	towards = g_hash_table_new (g_direct_hash,
	                            g_direct_equal);

	g_hash_table_insert (towards, dummy, GINT_TO_POINTER (1));

	nf = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (func)));
	nfarg = cdn_function_get_argument (nf, cdn_function_argument_get_name (arg));
	nfargv = _cdn_function_argument_get_variable (nfarg);

	replace_args (iter, func, nf);

	iargdim[0] = numr;
	iargdim[1] = numc;
	iargdim[2] = 1;
	iargdim[3] = 1;

	for (i = 0; i < num; ++i)
	{
		// To make this work, we are going to replace references to
		// 'arg' with a vector containing [arg, 0, 0] for example, and
		// derive towards the now single dimension arg, ok?
		CdnExpressionTreeIter *newvec;
		GSList *instrs = NULL;
		CdnExpressionTreeIter *cp;
		CdnExpressionTreeIter *iiter;
		CdnExpressionTreeIter *derived;
		gint j;

		popdims[i * 2] = fnumr;
		popdims[i * 2 + 1] = fnumc;

		instrs = g_slist_prepend (instrs,
		                          cdn_instruction_matrix_new (num,
		                                                      NULL,
		                                                      numr,
		                                                      numc));

		for (j = num - 1; j >= 0; --j)
		{
			if (j == i)
			{
				instrs = g_slist_prepend (instrs,
				                          cdn_instruction_variable_new (dummy));
			}
			else
			{
				instrs = g_slist_prepend (instrs,
				                          cdn_instruction_number_new_from_string ("0"));
			}
		}

		newvec = cdn_expression_tree_iter_new_from_instructions (instrs);
		cp = cdn_expression_tree_iter_copy (iter);

		g_slist_foreach (instrs, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (instrs);

		// Substitute the argv with the new vector in the cp
		cdn_expression_tree_iter_substitute (cp, nfargv, newvec);

		// Now do a partial derivative towards the dummy
		derived = cdn_expression_tree_iter_derive (cp,
		                                           syms,
		                                           towards,
		                                           order,
		                                           CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL |
		                                           CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY,
		                                           error);

		cdn_expression_tree_iter_free (cp);

		if (!derived)
		{
			retval = FALSE;
			break;
		}

		// Then substitute the dummy with an indexed version of the
		// new functions variable
		instrs = g_slist_prepend (NULL,
		                           cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_INDEX,
		                                                         NULL,
		                                                         2,
		                                                         iargdim));

		instrs = g_slist_prepend (instrs,
		                          cdn_instruction_variable_new (nfargv));

		instrs = g_slist_prepend (instrs,
		                          cdn_instruction_number_new (i));

		iiter = cdn_expression_tree_iter_new_from_instructions (instrs);

		g_slist_foreach (instrs, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (instrs);

		cdn_expression_tree_iter_substitute (derived, dummy, iiter);
		cdn_expression_tree_iter_free (iiter);

		instructions = g_slist_concat (g_slist_reverse (cdn_expression_tree_iter_to_instructions (derived)),
		                               instructions);

		if (fnumr != 1)
		{
			gint targdim[2];

			targdim[0] = fnumr;
			targdim[1] = fnumc;

			instructions = g_slist_prepend (instructions,
			                                cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TRANSPOSE,
			                                                              NULL,
			                                                              1,
			                                                              targdim));
		}

		cdn_expression_tree_iter_free (derived);
	}

	if (retval)
	{
		gint tdims[2];

		tdims[0] = numr * numc;
		tdims[1] = fnumr * fnumc;

		instructions = g_slist_prepend (instructions,
		                                cdn_instruction_matrix_new (num,
		                                                            popdims,
		                                                            numr * numc,
		                                                            fnumr * fnumc));

		instructions = g_slist_prepend (instructions,
		                                cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TRANSPOSE,
		                                                              NULL,
		                                                              1,
		                                                              tdims));

		instructions = g_slist_reverse (instructions);

		if (cdn_debug_is_enabled (CDN_DEBUG_DIFF))
		{
			CdnExpressionTreeIter *tot;

			tot = cdn_expression_tree_iter_new_from_instructions (instructions);

			cdn_debug_message (DEBUG_DIFF,
			                   "Jacobian: %s",
			                   cdn_expression_tree_iter_to_string (tot));

			cdn_expression_tree_iter_free (tot);
		}

		cdn_expression_set_instructions_take (cdn_function_get_expression (nf),
		                                      instructions);
	}
	else
	{
		g_free (popdims);
		g_object_unref (nf);
		nf = NULL;
	}

	g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instructions);

	g_hash_table_destroy (towards);

	g_object_unref (dummy);
	cdn_expression_tree_iter_free (iter);

	g_slist_foreach (syms, (GFunc)g_object_unref, NULL);
	g_slist_free (syms);

	return nf;
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
	CdnFunctionArgument *arg;
	gint numr;
	gint numc;
	gint fnumr;
	gint fnumc;

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
	                         num_arguments,
	                         argdim,
	                         &towards,
	                         &order,
	                         error))
	{
		return FALSE;
	}

	// Do for each dimension
	arg = towards->data;

	cdn_function_argument_get_dimension (arg, &numr, &numc);
	cdn_function_get_dimension (func, &fnumr, &fnumc);

	if (numr != 1 && numc != 1)
	{
		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Cannot take the partial derivative towards a matrix `%s' of %d-by-%d",
		             cdn_function_argument_get_name (arg),
		             numr,
		             numc);
	}
	else if (fnumr != 1 && fnumc != 1 && !(numr == 1 && numc == 1))
	{
		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Cannot take the partial derivative of a matrix function `%s' (%d-by-%d)",
		             cdn_object_get_id (CDN_OBJECT (func)),
		             numr,
		             numc);
	}
	else if (numr != 1 || numc != 1)
	{
		// Take jacobian
		nf = derive_jacobian (diff,
		                      func,
		                      towards,
		                      order,
		                      error);
	}
	else
	{
		nf = cdn_function_get_derivative (func,
		                                  towards,
		                                  order,
		                                  CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL |
		                                  CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY,
		                                  error);
	}

	g_object_unref (func);
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
