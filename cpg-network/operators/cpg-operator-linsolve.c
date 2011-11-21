/*
 * cpg-operator-linsolve.c
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

#include "cpg-operator-linsolve.h"
#include "cpg-operator.h"
#include "cpg-property.h"
#include "cpg-usable.h"
#include "cpg-integrator.h"
#include "cpg-symbolic.h"
#include "cpg-function.h"
#include "cpg-expression-tree-iter.h"
#include "cpg-network.h"
#include "cpg-debug.h"

#include "instructions/cpg-instructions.h"

#include <math.h>

/**
 * SECTION:cpg-operator-linsolve
 * @short_description: Math operator for linsolve evaluation of an expression
 *
 * The #CpgOperatorLinsolve is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * linsolve value of its argument (which can be an arbitrary expression).
 *
 */

#define CPG_OPERATOR_LINSOLVE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_LINSOLVE, CpgOperatorLinsolvePrivate))

struct _CpgOperatorLinsolvePrivate
{
	CpgFunction **functions;
	gint num_functions;
};

G_DEFINE_TYPE (CpgOperatorLinsolve,
               cpg_operator_linsolve,
               CPG_TYPE_OPERATOR)

static gchar *
cpg_operator_linsolve_get_name ()
{
	return g_strdup ("linsolve");
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

static CpgFunctionArgument *
derived_arg (CpgFunction   *func,
             CpgExpression *expr)
{
	GSList const *instr;
	gchar const *pname;

	instr = cpg_expression_get_instructions (expr);

	if (instr->next || !CPG_IS_INSTRUCTION_PROPERTY (instr->data))
	{
		return NULL;
	}

	pname = cpg_property_get_name (cpg_instruction_property_get_property (instr->data));
	return cpg_function_get_argument (func, pname);
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
		CpgFunction *func;

		func = derived_function (expressions->data);

		if (!func)
		{
			g_set_error (error,
			             CPG_SYMBOLIC_ERROR,
			             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Expected function reference but got `%s'",
			             cpg_expression_get_as_string (expressions->data));

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
		CpgFunctionArgument *arg;

		arg = derived_arg (ptr->data, towards->data);

		if (!arg)
		{
			g_set_error (error,
			             CPG_SYMBOLIC_ERROR,
			             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Expected function variable but got `%s' for linsolve of `%s'",
			             cpg_expression_get_as_string (towards->data),
			             cpg_expression_get_as_string (cpg_function_get_expression (ptr->data)));

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
compare_function_args (CpgFunction *f1,
                       CpgFunction *f2)
{
	GList const *a1;
	GList const *a2;

	a1 = cpg_function_get_arguments (f1);
	a2 = cpg_function_get_arguments (f2);

	while (a1 && a2)
	{
		CpgFunctionArgument *arg1 = a1->data;
		CpgFunctionArgument *arg2 = a2->data;

		if (g_strcmp0 (cpg_function_argument_get_name (arg1),
		               cpg_function_argument_get_name (arg2)) != 0)
		{
			return FALSE;
		}

		if (cpg_function_argument_get_explicit (arg1) !=
		    cpg_function_argument_get_explicit (arg2))
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
	CpgFunction *f1 = funcs->data;
	funcs = g_slist_next (funcs);

	// Check if all the funcs have the same arguments
	while (funcs)
	{
		CpgFunction *func = funcs->data;

		if (!compare_function_args (f1, func))
		{
			g_set_error (error,
			             CPG_NETWORK_LOAD_ERROR,
			             CPG_NETWORK_LOAD_ERROR_OPERATOR,
			             "System of functions provided to linsolve must have the same arguments (%s and %s differ)",
			             cpg_object_get_id (CPG_OBJECT (f1)),
			             cpg_object_get_id (CPG_OBJECT (func)));

			return FALSE;
		}

		funcs = g_slist_next (funcs);
	}

	return TRUE;
}

static CpgProperty *
property_from_iter (CpgExpressionTreeIter *iter)
{
	CpgInstructionProperty *p;

	p = CPG_INSTRUCTION_PROPERTY (cpg_expression_tree_iter_get_instruction (iter));

	return cpg_instruction_property_get_property (p);
}

static CpgExpressionTreeIter *
apply_substitutions (CpgExpressionTreeIter *eq,
                     GSList                *iters,
                     GSList                *unknowns,
                     GSList                *until)
{
	while (iters != until)
	{
		CpgProperty *un;
		CpgExpressionTreeIter *it;

		it = iters->data;
		un = unknowns->data;

		// Substitute in eq, the unknown 'un' for 'it'
		eq = cpg_expression_tree_iter_substitute (eq,
		                                          un,
		                                          it);

		iters = g_slist_next (iters);
		unknowns = g_slist_next (unknowns);
	}

	return eq;
}

static CpgExpressionTreeIter *
iter_for_property (CpgProperty *prop)
{
	CpgInstruction *instr;
	CpgExpressionTreeIter *ret;

	instr = cpg_instruction_property_new (prop);
	ret = cpg_expression_tree_iter_new_from_instruction (instr);

	cpg_mini_object_free (CPG_MINI_OBJECT (instr));
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
	                                 (GDestroyNotify)cpg_expression_tree_iter_free);

	*unknownprops = NULL;
	*fprops = NULL;

	while (functions)
	{
		CpgProperty *prop;
		GList const *args;
		GSList *ptr = *fprops;
		CpgExpressionTreeIter *fiter;

		args = cpg_function_get_arguments (functions->data);

		while (args)
		{
			if (first)
			{
				prop = cpg_property_new (cpg_function_argument_get_name (args->data),
				                         cpg_expression_new0 (),
				                         CPG_PROPERTY_FLAG_NONE);

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
			                     _cpg_function_argument_get_property (args->data),
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
		                                                                          _cpg_function_argument_get_property (unknowns->data))));

		fiter = cpg_expression_tree_iter_new (cpg_function_get_expression (functions->data));

		cpg_expression_tree_iter_canonicalize (fiter);
		fiter = cpg_expression_tree_iter_simplify (fiter);

		cpg_expression_tree_iter_substitute_hash (fiter, propmap);

		ret = g_slist_prepend (ret, fiter);

		functions = g_slist_next (functions);
		unknowns = g_slist_next (unknowns);
	}

	g_hash_table_destroy (propmap);
	*unknownprops = g_slist_reverse (*unknownprops);

	return g_slist_reverse (ret);
}

static void
generate_functions (CpgOperatorLinsolve *self,
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
		CpgFunction *f;
		gchar *s;
		CpgExpression *expr;
		GSList *unk;
		GList const *args;
		GHashTable *propmap;
		GSList *fpropit;
		gchar const *es;

		s = g_strconcat ("ls_",
		                 cpg_property_get_name (unkptr->data),
		                 NULL);

		f = CPG_FUNCTION (cpg_object_copy (functions->data));
		cpg_object_set_id (CPG_OBJECT (f), s);
		g_free (s);

		args = cpg_function_get_arguments (f);

		propmap = g_hash_table_new_full (g_direct_hash,
		                                 g_direct_equal,
		                                 NULL,
		                                 (GDestroyNotify)cpg_expression_tree_iter_free);

		fpropit = fprops;

		while (args)
		{
			CpgExpressionTreeIter *iter;
			CpgProperty *pprop;

			// Create property tree for this property
			pprop = _cpg_function_argument_get_property (args->data);
			iter = iter_for_property (pprop);

			g_hash_table_insert (propmap,
			                     fpropit->data,
			                     iter);

			args = g_list_next (args);
			fpropit = g_slist_next (fpropit);
		}

		cpg_expression_tree_iter_substitute_hash (iters->data,
		                                          propmap);
		g_hash_table_destroy (propmap);

		// Remove function arguments that represented the unknowns
		for (unk = unknownprops; unk; unk = g_slist_next (unk))
		{
			CpgFunctionArgument *arg;

			arg = cpg_function_get_argument (f,
			                                 cpg_property_get_name (unk->data));

			if (arg)
			{
				cpg_function_remove_argument (f, arg, NULL);
			}
		}

		// Finally, set the expression instructions here
		expr = cpg_function_get_expression (f);

		es = cpg_expression_tree_iter_to_string (iters->data);

		cpg_expression_set_from_string (expr, es);

		_cpg_expression_set_instructions_take (expr,
		                                       cpg_expression_tree_iter_to_instructions (iters->data));

		g_ptr_array_add (ptr, f);

		iters = g_slist_next (iters);
		functions = g_slist_next (functions);
		unkptr = g_slist_next (unkptr);
	}

	self->priv->num_functions = ptr->len;
	self->priv->functions = (CpgFunction **)g_ptr_array_free (ptr, FALSE);
}

static gboolean
solve_system (CpgOperatorLinsolve *self,
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

	cpg_debug_message (DEBUG_LINSOLVE, "Solve forward");

	// Forward substitution
	while (iterit)
	{
		CpgExpressionTreeIter *eq;

		eq = iterit->data;

		// Apply substitutions
		eq = iterit->data = apply_substitutions (eq,
		                                         iters,
		                                         unknownprops,
		                                         iterit);

		// Solve 'eq' for 'un'
		eq = iterit->data = cpg_expression_tree_iter_solve_for (eq,
		                                                        unkpit->data,
		                                                        error);

		if (!eq)
		{
			g_slist_foreach (iters, (GFunc)cpg_expression_tree_iter_free, NULL);
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

	cpg_debug_message (DEBUG_LINSOLVE, "Substitute backward");

	while (iterit)
	{
		CpgExpressionTreeIter *eq = iterit->data;

		// Apply substitutions
		eq = iterit->data = apply_substitutions (eq,
		                                         iters,
		                                         unknownprops,
		                                         iterit);

		eq = iterit->data = cpg_expression_tree_iter_simplify (cpg_expression_tree_iter_canonicalize (eq));

		cpg_debug_message (DEBUG_LINSOLVE,
		                   "Solved: {%s}",
		                   cpg_expression_tree_iter_to_string (eq));

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

	g_slist_foreach (iters, (GFunc)cpg_expression_tree_iter_free, NULL);
	g_slist_free (iters);

	return TRUE;
}

static gboolean
cpg_operator_linsolve_initialize (CpgOperator   *op,
                                  GSList const **expressions,
                                  gint           num_expressions,
                                  GSList const **indices,
                                  gint           num_indices,
                                  gint           num_arguments,
                                  GError       **error)
{
	CpgOperatorLinsolve *linsolve;
	GSList *funcs;
	GSList *unknowns;

	if (!CPG_OPERATOR_CLASS (cpg_operator_linsolve_parent_class)->initialize (op,
	                                                                          expressions,
	                                                                          num_expressions,
	                                                                          indices,
	                                                                          num_indices,
	                                                                          num_arguments,
	                                                                          error))
	{
		return FALSE;
	}

	if (num_expressions != 2 ||
	    g_slist_length ((GSList *)expressions[0]) != g_slist_length ((GSList *)expressions[1]) ||
	    num_indices != 1)
	{
		g_set_error (error,
		             CPG_NETWORK_LOAD_ERROR,
		             CPG_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `linsolve' expects arguments [<NxFunc>;<Nxunknowns>][idx] <list>");

		return FALSE;
	}

	linsolve = CPG_OPERATOR_LINSOLVE (op);

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
cpg_operator_linsolve_finalize (GObject *object)
{
	CpgOperatorLinsolve *self;
	CpgFunction **funcs;

	self = CPG_OPERATOR_LINSOLVE (object);
	funcs = self->priv->functions;

	G_OBJECT_CLASS (cpg_operator_linsolve_parent_class)->finalize (object);

	g_free (funcs);
}

static gboolean
cpg_operator_linsolve_equal (CpgOperator *op,
                             CpgOperator *other)
{
	CpgOperatorLinsolve *linsolve;
	CpgOperatorLinsolve *odel;
	gint i;

	if (!CPG_IS_OPERATOR_LINSOLVE (other))
	{
		return FALSE;
	}

	linsolve = CPG_OPERATOR_LINSOLVE (op);
	odel = CPG_OPERATOR_LINSOLVE (other);

	if (linsolve->priv->num_functions != odel->priv->num_functions)
	{
		return FALSE;
	}

	for (i = 0; i < linsolve->priv->num_functions; ++i)
	{
		if (!cpg_object_equal (CPG_OBJECT (linsolve->priv->functions[i]),
		                       CPG_OBJECT (odel->priv->functions[i])))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static CpgFunction *
cpg_operator_linsolve_get_function (CpgOperator *op,
                                    gint        *idx,
                                    gint         numidx)
{
	CpgOperatorLinsolve *self;

	self = (CpgOperatorLinsolve *)op;

	if (numidx == 1 && idx[0] >= 0 && idx[0] < self->priv->num_functions)
	{
		return self->priv->functions[idx[0]];
	}

	return NULL;
}

static CpgOperator *
cpg_operator_linsolve_copy (CpgOperator *op)
{
	CpgOperatorLinsolve *linsolve;
	CpgOperatorLinsolve *ret;
	gint i;

	linsolve = CPG_OPERATOR_LINSOLVE (op);

	ret = CPG_OPERATOR_LINSOLVE (g_object_new (CPG_TYPE_OPERATOR_LINSOLVE, NULL));

	CPG_OPERATOR_CLASS (cpg_operator_linsolve_parent_class)->initialize (CPG_OPERATOR (ret),
	                                                                     cpg_operator_all_expressions (op),
	                                                                     cpg_operator_num_expressions (op),
	                                                                     cpg_operator_all_indices (op),
	                                                                     cpg_operator_num_indices (op),
	                                                                     cpg_operator_get_num_arguments (op),
	                                                                     NULL);

	ret->priv->num_functions = linsolve->priv->num_functions;
	ret->priv->functions = g_new0 (CpgFunction *, linsolve->priv->num_functions);

	for (i = 0; i < linsolve->priv->num_functions; ++i)
	{
		ret->priv->functions[i] = CPG_FUNCTION (cpg_object_copy (CPG_OBJECT (linsolve->priv->functions[i])));
	}

	return CPG_OPERATOR (ret);
}

static void
cpg_operator_linsolve_foreach_function (CpgOperator            *op,
                                        CpgForeachFunctionFunc  func,
                                        gpointer                userdata)
{
	CpgOperatorLinsolve *self;
	gint i;

	self = (CpgOperatorLinsolve *)op;

	for (i = 0; i < self->priv->num_functions; ++i)
	{
		func (self->priv->functions[i], userdata);
	}
}

static void
cpg_operator_linsolve_class_init (CpgOperatorLinsolveClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_linsolve_finalize;

	op_class->get_name = cpg_operator_linsolve_get_name;
	op_class->initialize = cpg_operator_linsolve_initialize;
	op_class->equal = cpg_operator_linsolve_equal;
	op_class->get_function = cpg_operator_linsolve_get_function;
	op_class->foreach_function = cpg_operator_linsolve_foreach_function;
	op_class->copy = cpg_operator_linsolve_copy;

	g_type_class_add_private (object_class, sizeof(CpgOperatorLinsolvePrivate));
}

static void
cpg_operator_linsolve_init (CpgOperatorLinsolve *self)
{
	self->priv = CPG_OPERATOR_LINSOLVE_GET_PRIVATE (self);
}

CpgOperatorLinsolve *
cpg_operator_linsolve_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_LINSOLVE, NULL);
}
