/*
 * cdn-operator-linsolve.c
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

#include "cdn-operator-linsolve.h"
#include "cdn-operator.h"
#include "cdn-variable.h"
#include "cdn-usable.h"
#include "integrators/cdn-integrator.h"
#include "cdn-function.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-network.h"
#include "cdn-debug.h"

#include "instructions/cdn-instructions.h"

#include <math.h>

/**
 * SECTION:cdn-operator-linsolve
 * @short_description: Math operator for linsolve evaluation of an expression
 *
 * The #CdnOperatorLinsolve is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * linsolve value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_LINSOLVE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_LINSOLVE, CdnOperatorLinsolvePrivate))

struct _CdnOperatorLinsolvePrivate
{
	CdnFunction **functions;
	gint num_functions;
};

G_DEFINE_TYPE (CdnOperatorLinsolve,
               cdn_operator_linsolve,
               CDN_TYPE_OPERATOR)

static gchar *
cdn_operator_linsolve_get_name ()
{
	return g_strdup ("linsolve");
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
                    GSList const  *towards,
                    GSList       **funcs,
                    GSList       **unknowns,
                    GError       **error)
{
	GSList *ptr;

	*funcs = NULL;

	while (expressions)
	{
		CdnFunction *func;

		func = derived_function (expressions->data);

		if (!func)
		{
			g_set_error (error,
			             CDN_NETWORK_LOAD_ERROR,
			             CDN_NETWORK_LOAD_ERROR_OPERATOR,
			             "Expected function reference but got `%s'",
			             cdn_expression_get_as_string (expressions->data));

			g_slist_free (*funcs);

			return FALSE;
		}

		*funcs = g_slist_prepend (*funcs, func);
		expressions = g_slist_next (expressions);
	}

	*funcs = g_slist_reverse (*funcs);
	*unknowns = NULL;

	ptr = *funcs;

	while (towards)
	{
		CdnFunctionArgument *arg;

		arg = derived_arg (ptr->data, towards->data);

		if (!arg)
		{
			g_set_error (error,
			             CDN_NETWORK_LOAD_ERROR,
			             CDN_NETWORK_LOAD_ERROR_OPERATOR,
			             "Expected function variable but got `%s' for linsolve of `%s'",
			             cdn_expression_get_as_string (towards->data),
			             cdn_expression_get_as_string (cdn_function_get_expression (ptr->data)));

			g_slist_free (*unknowns);
			g_slist_free (*funcs);

			return FALSE;
		}
		else
		{
			*unknowns = g_slist_prepend (*unknowns, arg);
		}

		ptr = g_slist_next (ptr);
		towards = g_slist_next (towards);
	}

	*unknowns = g_slist_reverse (*unknowns);
	return TRUE;
}

static gboolean
compare_function_args (CdnFunction *f1,
                       CdnFunction *f2)
{
	GList const *a1;
	GList const *a2;

	a1 = cdn_function_get_arguments (f1);
	a2 = cdn_function_get_arguments (f2);

	while (a1 && a2)
	{
		CdnFunctionArgument *arg1 = a1->data;
		CdnFunctionArgument *arg2 = a2->data;

		if (g_strcmp0 (cdn_function_argument_get_name (arg1),
		               cdn_function_argument_get_name (arg2)) != 0)
		{
			return FALSE;
		}

		if (cdn_function_argument_get_explicit (arg1) !=
		    cdn_function_argument_get_explicit (arg2))
		{
			return FALSE;
		}

		a1 = g_list_next (a1);
		a2 = g_list_next (a2);
	}

	return !(a1 || a2);
}

static gboolean
validate_system (GSList  *funcs,
                 GError **error)
{
	CdnFunction *f1 = funcs->data;
	funcs = g_slist_next (funcs);

	// Check if all the funcs have the same arguments
	while (funcs)
	{
		CdnFunction *func = funcs->data;

		if (!compare_function_args (f1, func))
		{
			g_set_error (error,
			             CDN_NETWORK_LOAD_ERROR,
			             CDN_NETWORK_LOAD_ERROR_OPERATOR,
			             "System of functions provided to linsolve must have the same arguments (%s and %s differ)",
			             cdn_object_get_id (CDN_OBJECT (f1)),
			             cdn_object_get_id (CDN_OBJECT (func)));

			return FALSE;
		}

		funcs = g_slist_next (funcs);
	}

	return TRUE;
}

static CdnVariable *
property_from_iter (CdnExpressionTreeIter *iter)
{
	CdnInstructionVariable *p;

	p = CDN_INSTRUCTION_VARIABLE (cdn_expression_tree_iter_get_instruction (iter));

	return cdn_instruction_variable_get_variable (p);
}

static CdnExpressionTreeIter *
apply_substitutions (CdnExpressionTreeIter *eq,
                     GSList                *iters,
                     GSList                *unknowns,
                     GSList                *until)
{
	while (iters != until)
	{
		CdnVariable *un;
		CdnExpressionTreeIter *it;

		it = iters->data;
		un = unknowns->data;

		// Substitute in eq, the unknown 'un' for 'it'
		eq = cdn_expression_tree_iter_substitute (eq,
		                                          un,
		                                          it);

		iters = g_slist_next (iters);
		unknowns = g_slist_next (unknowns);
	}

	return eq;
}

static CdnExpressionTreeIter *
iter_for_property (CdnVariable *prop)
{
	CdnInstruction *instr;
	CdnExpressionTreeIter *ret;

	instr = cdn_instruction_variable_new (prop);
	ret = cdn_expression_tree_iter_new_from_instruction (instr);

	cdn_mini_object_unref (instr);
	return ret;
}

static GSList *
prepare_functions (GSList  *functions,
                   GSList  *unknowns,
                   GSList **unknownprops,
                   GSList **fprops)
{
	gboolean first = TRUE;
	GSList *ret = NULL;
	GHashTable *propmap;

	propmap = g_hash_table_new_full (g_direct_hash,
	                                 g_direct_equal,
	                                 NULL,
	                                 (GDestroyNotify)cdn_expression_tree_iter_free);

	*unknownprops = NULL;
	*fprops = NULL;

	while (functions)
	{
		CdnVariable *prop;
		GList const *args;
		GSList *ptr = *fprops;
		CdnExpressionTreeIter *fiter;

		args = cdn_function_get_arguments (functions->data);

		while (args)
		{
			if (first)
			{
				prop = cdn_variable_new (cdn_function_argument_get_name (args->data),
				                         cdn_expression_new0 (),
				                         CDN_VARIABLE_FLAG_NONE);

				g_object_ref_sink (prop);

				*fprops = g_slist_prepend (*fprops, prop);
			}
			else
			{
				prop = ptr->data;
				ptr = g_slist_next (ptr);
			}

			// Map from the original argument property to the dummy
			// property
			g_hash_table_insert (propmap,
			                     _cdn_function_argument_get_variable (args->data),
			                     iter_for_property (prop));

			args = g_list_next (args);
		}

		if (first)
		{
			first = FALSE;
			*fprops = g_slist_reverse (*fprops);
		}

		*unknownprops = g_slist_prepend (*unknownprops,
		                                 property_from_iter (g_hash_table_lookup (propmap,
		                                                                          _cdn_function_argument_get_variable (unknowns->data))));

		fiter = cdn_expression_tree_iter_new (cdn_function_get_expression (functions->data));

		cdn_expression_tree_iter_canonicalize (fiter);
		fiter = cdn_expression_tree_iter_simplify (fiter);

		cdn_expression_tree_iter_substitute_hash (fiter, propmap);

		ret = g_slist_prepend (ret, fiter);

		functions = g_slist_next (functions);
		unknowns = g_slist_next (unknowns);
	}

	g_hash_table_destroy (propmap);
	*unknownprops = g_slist_reverse (*unknownprops);

	return g_slist_reverse (ret);
}

static void
generate_functions (CdnOperatorLinsolve *self,
                    GSList              *functions,
                    GSList              *iters,
                    GSList              *unknownprops,
                    GSList              *fprops)
{
	GPtrArray *ptr;
	GSList *unkptr;

	ptr = g_ptr_array_new ();

	unkptr = unknownprops;

	while (iters)
	{
		CdnFunction *f;
		gchar *s;
		CdnExpression *expr;
		GSList *unk;
		GList const *args;
		GHashTable *propmap;
		GSList *fpropit;
		gchar const *es;
		GSList *newinstr;

		s = g_strconcat ("ls_",
		                 cdn_variable_get_name (unkptr->data),
		                 NULL);

		f = CDN_FUNCTION (cdn_object_copy (functions->data));
		cdn_object_set_id (CDN_OBJECT (f), s);
		g_free (s);

		args = cdn_function_get_arguments (f);

		propmap = g_hash_table_new_full (g_direct_hash,
		                                 g_direct_equal,
		                                 NULL,
		                                 (GDestroyNotify)cdn_expression_tree_iter_free);

		fpropit = fprops;

		while (args)
		{
			CdnExpressionTreeIter *iter;
			CdnVariable *pprop;

			// Create property tree for this property
			pprop = _cdn_function_argument_get_variable (args->data);
			iter = iter_for_property (pprop);

			g_hash_table_insert (propmap,
			                     fpropit->data,
			                     iter);

			args = g_list_next (args);
			fpropit = g_slist_next (fpropit);
		}

		cdn_expression_tree_iter_substitute_hash (iters->data,
		                                          propmap);
		g_hash_table_destroy (propmap);

		// Remove function arguments that represented the unknowns
		for (unk = unknownprops; unk; unk = g_slist_next (unk))
		{
			CdnFunctionArgument *arg;

			arg = cdn_function_get_argument (f,
			                                 cdn_variable_get_name (unk->data));

			if (arg)
			{
				cdn_function_remove_argument (f, arg, NULL);
			}
		}

		// Finally, set the expression instructions here
		expr = cdn_function_get_expression (f);

		es = cdn_expression_tree_iter_to_string (iters->data);

		cdn_expression_set_from_string (expr, es);

		newinstr = cdn_expression_tree_iter_to_instructions (iters->data);

		cdn_expression_set_instructions_take (expr, newinstr);

		g_slist_foreach (newinstr, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (newinstr);

		g_ptr_array_add (ptr, f);

		iters = g_slist_next (iters);
		functions = g_slist_next (functions);
		unkptr = g_slist_next (unkptr);
	}

	self->priv->num_functions = ptr->len;
	self->priv->functions = (CdnFunction **)g_ptr_array_free (ptr, FALSE);
}

static gboolean
solve_system (CdnOperatorLinsolve *self,
              GSList              *funcs,
              GSList              *unknowns,
              GError             **error)
{
	GSList *iters;
	GSList *iterit;
	GSList *unkpit;
	GSList *unknownprops;
	GSList *fprops;

	if (!validate_system (funcs, error))
	{
		return FALSE;
	}

	// Generate substitution properties
	iters = prepare_functions (funcs,
	                           unknowns,
	                           &unknownprops,
	                           &fprops);

	if (!iters)
	{
		return FALSE;
	}

	// Iteratively solve the equations for the unknowns
	iterit = iters;
	unkpit = unknownprops;

	cdn_debug_message (DEBUG_LINSOLVE, "Solve forward");

	// Forward substitution
	while (iterit)
	{
		CdnExpressionTreeIter *eq;

		eq = iterit->data;

		// Apply substitutions
		eq = iterit->data = apply_substitutions (eq,
		                                         iters,
		                                         unknownprops,
		                                         iterit);

		// Solve 'eq' for 'un'
		eq = iterit->data = cdn_expression_tree_iter_solve_for (eq,
		                                                        unkpit->data,
		                                                        error);

		if (!eq)
		{
			g_slist_foreach (iters, (GFunc)cdn_expression_tree_iter_free, NULL);
			g_slist_free (iters);

			g_slist_free (unknownprops);

			g_slist_foreach (fprops, (GFunc)g_object_unref, NULL);
			g_slist_free (fprops);

			return FALSE;
		}

		unkpit = g_slist_next (unkpit);
		iterit = g_slist_next (iterit);
	}

	// Now we have one pass forward substitution, we are now going to
	// substitute backwards to solve the complete system
	iterit = iters = g_slist_reverse (iters);
	unknownprops = g_slist_reverse (unknownprops);

	cdn_debug_message (DEBUG_LINSOLVE, "Substitute backward");

	while (iterit)
	{
		CdnExpressionTreeIter *eq = iterit->data;

		// Apply substitutions
		eq = iterit->data = apply_substitutions (eq,
		                                         iters,
		                                         unknownprops,
		                                         iterit);

		eq = iterit->data = cdn_expression_tree_iter_simplify (cdn_expression_tree_iter_canonicalize (eq));

		cdn_debug_message (DEBUG_LINSOLVE,
		                   "Solved: {%s}",
		                   cdn_expression_tree_iter_to_string (eq));

		iterit = g_slist_next (iterit);
	}

	iters = g_slist_reverse (iters);
	unknownprops = g_slist_reverse (unknownprops);

	// Now we have the completed equations in 'iters' for the unknowns
	// as in 'subst'. Generate a set of functions representing those
	// equations
	generate_functions (self, funcs, iters, unknownprops, fprops);

	g_slist_free (unknownprops);
	g_slist_free (fprops);

	g_slist_foreach (iters, (GFunc)cdn_expression_tree_iter_free, NULL);
	g_slist_free (iters);

	return TRUE;
}

static gboolean
cdn_operator_linsolve_initialize (CdnOperator        *op,
                                  GSList const      **expressions,
                                  gint                num_expressions,
                                  GSList const      **indices,
                                  gint                num_indices,
                                  gint                num_arguments,
                                  gint               *argdim,
                                  CdnCompileContext  *context,
                                  GError            **error)
{
	CdnOperatorLinsolve *linsolve;
	GSList *funcs;
	GSList *unknowns;

	if (!CDN_OPERATOR_CLASS (cdn_operator_linsolve_parent_class)->initialize (op,
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
	    g_slist_length ((GSList *)expressions[0]) != g_slist_length ((GSList *)expressions[1]) ||
	    num_indices != 1)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `linsolve' expects arguments [<NxFunc>;<Nxunknowns>][idx] <list>");

		return FALSE;
	}

	linsolve = CDN_OPERATOR_LINSOLVE (op);

	if (!validate_arguments (expressions[0],
	                         expressions[1],
	                         &funcs,
	                         &unknowns,
	                         error))
	{
		return FALSE;
	}

	return solve_system (linsolve, funcs, unknowns, error);
}

static void
cdn_operator_linsolve_finalize (GObject *object)
{
	CdnOperatorLinsolve *self;
	CdnFunction **funcs;

	self = CDN_OPERATOR_LINSOLVE (object);
	funcs = self->priv->functions;

	G_OBJECT_CLASS (cdn_operator_linsolve_parent_class)->finalize (object);

	g_free (funcs);
}

static gboolean
cdn_operator_linsolve_equal (CdnOperator *op,
                             CdnOperator *other,
                             gboolean     asstring)
{
	CdnOperatorLinsolve *linsolve;
	CdnOperatorLinsolve *odel;
	gint i;

	if (!CDN_IS_OPERATOR_LINSOLVE (other))
	{
		return FALSE;
	}

	linsolve = CDN_OPERATOR_LINSOLVE (op);
	odel = CDN_OPERATOR_LINSOLVE (other);

	if (linsolve->priv->num_functions != odel->priv->num_functions)
	{
		return FALSE;
	}

	for (i = 0; i < linsolve->priv->num_functions; ++i)
	{
		if (!cdn_object_equal (CDN_OBJECT (linsolve->priv->functions[i]),
		                       CDN_OBJECT (odel->priv->functions[i])))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static CdnFunction *
cdn_operator_linsolve_get_function (CdnOperator *op,
                                    gint        *idx,
                                    gint         numidx)
{
	CdnOperatorLinsolve *self;

	self = (CdnOperatorLinsolve *)op;

	if (numidx == 1 && idx[0] >= 0 && idx[0] < self->priv->num_functions)
	{
		return self->priv->functions[idx[0]];
	}

	return NULL;
}

static CdnOperator *
cdn_operator_linsolve_copy (CdnOperator *op)
{
	CdnOperatorLinsolve *linsolve;
	CdnOperatorLinsolve *ret;
	gint i;

	linsolve = CDN_OPERATOR_LINSOLVE (op);

	ret = CDN_OPERATOR_LINSOLVE (g_object_new (CDN_TYPE_OPERATOR_LINSOLVE, NULL));

	CDN_OPERATOR_CLASS (cdn_operator_linsolve_parent_class)->initialize (CDN_OPERATOR (ret),
	                                                                     cdn_operator_all_expressions (op),
	                                                                     cdn_operator_num_expressions (op),
	                                                                     cdn_operator_all_indices (op),
	                                                                     cdn_operator_num_indices (op),
	                                                                     cdn_operator_get_num_arguments (op),
	                                                                     cdn_operator_get_arguments_dimension (op),
	                                                                     NULL,
	                                                                     NULL);

	ret->priv->num_functions = linsolve->priv->num_functions;
	ret->priv->functions = g_new0 (CdnFunction *, linsolve->priv->num_functions);

	for (i = 0; i < linsolve->priv->num_functions; ++i)
	{
		ret->priv->functions[i] = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (linsolve->priv->functions[i])));
	}

	return CDN_OPERATOR (ret);
}

static void
cdn_operator_linsolve_foreach_function (CdnOperator            *op,
                                        CdnForeachFunctionFunc  func,
                                        gpointer                userdata)
{
	CdnOperatorLinsolve *self;
	gint i;

	self = (CdnOperatorLinsolve *)op;

	for (i = 0; i < self->priv->num_functions; ++i)
	{
		func (self->priv->functions[i], userdata);
	}
}

static void
cdn_operator_linsolve_class_init (CdnOperatorLinsolveClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_linsolve_finalize;

	op_class->get_name = cdn_operator_linsolve_get_name;
	op_class->initialize = cdn_operator_linsolve_initialize;
	op_class->equal = cdn_operator_linsolve_equal;
	op_class->get_function = cdn_operator_linsolve_get_function;
	op_class->foreach_function = cdn_operator_linsolve_foreach_function;
	op_class->copy = cdn_operator_linsolve_copy;

	g_type_class_add_private (object_class, sizeof(CdnOperatorLinsolvePrivate));
}

static void
cdn_operator_linsolve_init (CdnOperatorLinsolve *self)
{
	self->priv = CDN_OPERATOR_LINSOLVE_GET_PRIVATE (self);
}

CdnOperatorLinsolve *
cdn_operator_linsolve_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_LINSOLVE, NULL);
}
