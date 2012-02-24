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
		GSList *instr;

		CdnVariable *p;
		CdnVariable *pnf;

		arg = args->data;
		argnf = nargs->data;

		args = g_list_next (args);
		nargs = g_list_next (nargs);

		p = _cdn_function_argument_get_variable (arg);
		pnf = _cdn_function_argument_get_variable (argnf);

		instr = g_slist_prepend (NULL,
		                         cdn_instruction_variable_new (pnf));

		g_hash_table_insert (ret,
		                     p,
		                     cdn_expression_tree_iter_new_from_instructions (instr));

		g_slist_foreach (instr, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (instr);

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

static CdnVariable *
derived_property (CdnFunction   *func,
                  CdnExpression *expr)
{
	CdnFunctionArgument *arg;

	arg = derived_arg (func, expr);

	if (!arg)
	{
		return NULL;
	}

	return _cdn_function_argument_get_variable (arg);
}

static gboolean
validate_arguments (GSList const  *expressions,
                    GSList const  *towardsexpr,
                    CdnFunction  **func,
                    CdnVariable  **towards,
                    GList        **syms,
                    gint          *order,
                    GError       **error)
{
	CdnExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data);

	if (!*func)
	{
		g_set_error (error,
		             CDN_SYMBOLIC_ERROR,
		             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use df_dt[] for deriving expressions",
		             cdn_expression_get_as_string (expressions->data));

		return FALSE;
	}

	*syms = NULL;
	*order = 1;

	expressions = expressions->next;
	*towards = derived_property (*func, expressions->data);

	if (!*towards)
	{
		g_set_error (error,
		             CDN_SYMBOLIC_ERROR,
		             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
		             "Expected partial function variable reference but got `%s'.",
		             cdn_expression_get_as_string (expressions->data));

		return FALSE;
	}

	if (expressions->next)
	{
		*order = cdn_expression_evaluate (expressions->next->data);
	}

	while (towardsexpr)
	{
		CdnFunctionArgument *arg;

		arg = derived_arg (*func, towardsexpr->data);

		if (!arg)
		{
			g_set_error (error,
			             CDN_SYMBOLIC_ERROR,
			             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Expected function variable but got `%s' for pdiff of `%s'",
			             cdn_expression_get_as_string (towardsexpr->data),
			             cdn_expression_get_as_string (expr));

			g_list_free (*syms);
			return FALSE;
		}
		else
		{
			*syms = g_list_prepend (*syms, arg);
		}

		towardsexpr = g_slist_next (towardsexpr);
	}

	if (!*syms)
	{
		// By default add all properties of the function as syms
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

static GSList *
resolve_symprops (CdnFunction *f,
                  GList const *symargs)
{
	GSList *ret = NULL;

	while (symargs)
	{
		CdnFunctionArgument *arg;

		arg = cdn_function_get_argument (f,
		                                 cdn_function_argument_get_name (symargs->data));

		ret = g_slist_prepend (ret, _cdn_function_argument_get_variable (arg));

		symargs = g_list_next (symargs);
	}

	return g_slist_reverse (ret);
}

static gboolean
cdn_operator_pdiff_initialize (CdnOperator   *op,
                               GSList const **expressions,
                               gint           num_expressions,
                               GSList const **indices,
                               gint           num_indices,
                               gint           num_arguments,
                               gint          *argdim,
                               GError       **error)
{
	CdnOperatorPDiff *diff;
	CdnFunction *func;
	GList *symargs;
	GHashTable *property_map;
	GHashTable *diff_map;
	CdnFunction *nf = NULL;
	gchar *s;
	CdnVariable *towards;
	GSList *syms;
	gint order;
	CdnExpression *derived;

	if (!CDN_OPERATOR_CLASS (cdn_operator_pdiff_parent_class)->initialize (op,
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
	    (expressions[0]->next && expressions[0]->next->next) ||
	    (num_expressions > 1 && expressions[1]->next))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `pdiff' expects arguments [Func, towards{,order}{;<vars>}] {optional} <list>");

		return FALSE;
	}

	diff = CDN_OPERATOR_PDIFF (op);

	if (!validate_arguments (expressions[0],
	                         num_expressions > 1 ? expressions[1] : NULL,
	                         &func,
	                         &towards,
	                         &symargs,
	                         &order,
	                         error))
	{
		return FALSE;
	}

	// Here we are going to generate a new function with represents
	// the symbolic derivation
	s = g_strconcat ("pd",
	                 cdn_object_get_id (CDN_OBJECT (func)),
	                 "dt",
	                 NULL);

	nf = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (func)));
	cdn_object_set_id (CDN_OBJECT (nf), s);
	g_free (s);

	cdn_function_set_expression (nf, cdn_expression_new0 ());

	// Map original function properties to the new function arguments
	property_map = get_property_map (cdn_function_get_arguments (func),
	                                 cdn_function_get_arguments (nf),
	                                 -1,
	                                 NULL);

	// We use the diff map in partial derivation to indicate towards
	// which variable we differentiate
	towards = _cdn_function_argument_get_variable (cdn_function_get_argument (nf, cdn_variable_get_name (towards)));

	diff_map = g_hash_table_new (g_direct_hash, g_direct_equal);
	g_hash_table_insert (diff_map, towards, NULL);

	// newsymargs contains the CdnFunctionArgument of the new function
	syms = resolve_symprops (nf, symargs);

	derived = cdn_symbolic_derive (expressions[0]->data,
	                               syms,
	                               property_map,
	                               diff_map,
	                               order,
	                               CDN_SYMBOLIC_DERIVE_PARTIAL |
	                               CDN_SYMBOLIC_DERIVE_SIMPLIFY,
	                               error);

	g_slist_free (syms);

	cdn_function_set_expression (nf, derived);
	diff->priv->function = nf;

	if (property_map)
	{
		g_hash_table_destroy (property_map);
	}

	if (diff_map)
	{
		g_hash_table_destroy (diff_map);
	}

	g_list_free (symargs);

	return derived != NULL;
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
	return CDN_OPERATOR_PDIFF (op)->priv->function;
}

static void
cdn_operator_pdiff_foreach_function (CdnOperator *op,
                                     CdnForeachFunctionFunc func,
                                     gpointer userdata)
{
	func (CDN_OPERATOR_PDIFF (op)->priv->function, userdata);
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
