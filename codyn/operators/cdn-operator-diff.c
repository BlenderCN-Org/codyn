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
#include "cdn-symbolic.h"
#include "cdn-function.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-network.h"

#include "instructions/cdn-instructions.h"

#include <math.h>

/**
 * SECTION:cdn-operator-diff
 * @short_description: Math operator for diff evaluation of an expression
 *
 * The #CdnOperatorDiff is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * diff value of its argument (which can be an arbitrary expression).
 *
 */

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
derived_function (CdnExpression *expr,
                  gint           numargs,
                  gint          *argdim)
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
		func = cdn_function_for_dimension (func, numargs, argdim);
	}

	return func;
}

static GHashTable *
get_property_map (GList const *args,
                  GList const *nargs,
                  gint         num,
                  GHashTable  *ret)
{
	if (ret == NULL)
	{
		// Map properties of func to properties in nf
		ret = g_hash_table_new_full (g_direct_hash,
		                             g_direct_equal,
		                             NULL,
		                             (GDestroyNotify)cdn_expression_tree_iter_free);
	}

	while (args && nargs && num != 0)
	{
		CdnFunctionArgument *arg;
		CdnFunctionArgument *argnf;
		CdnInstruction *instr;

		CdnVariable *p;
		CdnVariable *pnf;

		arg = args->data;
		argnf = nargs->data;

		args = g_list_next (args);
		nargs = g_list_next (nargs);

		p = _cdn_function_argument_get_variable (arg);
		pnf = _cdn_function_argument_get_variable (argnf);

		instr = cdn_instruction_variable_new (pnf);

		g_hash_table_insert (ret,
		                     p,
		                     cdn_expression_tree_iter_new_from_instruction (instr));

		cdn_mini_object_unref (instr);

		--num;
	}

	return ret;
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
                    gint           numargs,
                    gint          *argdim,
                    GList        **syms,
                    gint          *order,
                    GError       **error)
{
	CdnExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data, numargs, argdim);

	if (!*func)
	{
		g_set_error (error,
		             CDN_SYMBOLIC_ERROR,
		             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use df_dt[] for deriving expressions",
		             cdn_expression_get_as_string (expressions->data));

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
			             CDN_SYMBOLIC_ERROR,
			             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Expected function variable but got `%s' for diff of `%s'",
			             cdn_expression_get_as_string (towards->data),
			             cdn_expression_get_as_string (expr));

			g_list_free (*syms);
			return FALSE;
		}
		else
		{
			*syms = g_list_prepend (*syms, arg);
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
				*syms = g_list_prepend (*syms, args->data);
			}

			args = g_list_next (args);
		}
	}

	*syms = g_list_reverse (*syms);

	return TRUE;
}

static GList *
resolve_symargs (CdnFunction *f,
                 GList const *symargs)
{
	GList *ret = NULL;

	while (symargs)
	{
		ret = g_list_prepend (ret,
		                      cdn_function_get_argument (f,
		                                                 cdn_function_argument_get_name (symargs->data)));

		symargs = g_list_next (symargs);
	}

	return g_list_reverse (ret);
}

static gboolean
cdn_operator_diff_initialize (CdnOperator   *op,
                              GSList const **expressions,
                              gint           num_expressions,
                              GSList const **indices,
                              gint           num_indices,
                              gint           num_arguments,
                              gint          *argdim,
                              GError       **error)
{
	CdnOperatorDiff *diff;
	CdnFunction *func;
	GList *symargs;
	GHashTable *property_map;
	CdnFunction *nf = NULL;
	gchar *s;
	GList *item;
	gint i;
	GList *newsymargs;
	CdnExpression *derived = NULL;

	if (!CDN_OPERATOR_CLASS (cdn_operator_diff_parent_class)->initialize (op,
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
	                         num_arguments,
	                         argdim,
	                         &symargs,
	                         &diff->priv->order,
	                         error))
	{
		return FALSE;
	}

	// Here we are going to generate a new function with represents
	// the symbolic derivation
	s = g_strconcat ("d",
	                 cdn_object_get_id (CDN_OBJECT (func)),
	                 "dt",
	                 NULL);

	nf = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (func)));
	cdn_object_set_id (CDN_OBJECT (nf), s);
	g_free (s);

	// Resolve the symbols we are going to derive towards as function
	// arguments in the new function
	newsymargs = resolve_symargs (nf, symargs);
	g_list_free (symargs);

	// Start with an empty expression
	cdn_function_set_expression (nf, cdn_expression_new0 ());

	// Map original function properties to the new function arguments
	property_map = get_property_map (cdn_function_get_arguments (func),
	                                 cdn_function_get_arguments (nf),
	                                 -1,
	                                 NULL);

	// Add additional arguments to this function for the time
	// derivative of all the syms (which don't have derivatives present
	// in the current arguments)
	for (i = 0; i < diff->priv->order; ++i)
	{
		for (item = newsymargs; item; item = g_list_next (item))
		{
			CdnFunctionArgument *sarg = item->data;
			CdnFunctionArgument *darg;
			gchar *d;
			gchar *dsname;
			CdnFunctionArgument *oarg;
			CdnVariable *sprop;

			d = g_strnfill (i + 1, '\'');

			dsname = g_strconcat (cdn_function_argument_get_name (sarg),
			                      d,
			                      NULL);

			g_free (d);

			// Test if it already exists
			oarg = cdn_function_get_argument (nf, dsname);
			sprop = _cdn_function_argument_get_variable (sarg);

			if (oarg)
			{
				CdnVariable *oprop;

				oprop = _cdn_function_argument_get_variable (oarg);

				if (cdn_variable_get_derivative (sprop) == oprop)
				{
					// Skip this because we already have
					// a derivative for this
					g_free (dsname);
					continue;
				}
				else
				{
					// The property already exists, which
					// is strange, error out...
					g_list_free (newsymargs);
					g_set_error (error,
					             CDN_SYMBOLIC_ERROR,
					             CDN_SYMBOLIC_ERROR_INVALID,
					             "There is already an variable `%s' but it is not a derivative of `%s'",
					             cdn_variable_get_name (oprop),
					             cdn_variable_get_name (sprop));

					goto cleanup;
				}
			}

			darg = cdn_function_argument_new (dsname,
			                                  TRUE);

			g_free (dsname);

			cdn_function_add_argument (nf, darg);
		}
	}

	g_list_free (newsymargs);

	derived = cdn_symbolic_derive (expressions[0]->data,
	                               NULL,
	                               property_map,
	                               NULL,
	                               diff->priv->order,
	                               CDN_SYMBOLIC_DERIVE_SIMPLIFY,
	                               error);

	cdn_function_set_expression (nf, derived);

cleanup:
	diff->priv->function = nf;

	if (property_map)
	{
		g_hash_table_destroy (property_map);
	}

	return derived != NULL;
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
	return CDN_OPERATOR_DIFF (op)->priv->function;
}

static void
cdn_operator_diff_foreach_function (CdnOperator            *op,
                                    CdnForeachFunctionFunc  func,
                                    gpointer                data)
{
	func (CDN_OPERATOR_DIFF (op)->priv->function, data);
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
	                                                                 cdn_operator_get_num_arguments (op),
	                                                                 cdn_operator_get_arguments_dimension (op),
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
	self->priv = CDN_OPERATOR_DIFF_GET_PRIVATE (self);
}

CdnOperatorDiff *
cdn_operator_diff_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_DIFF, NULL);
}
