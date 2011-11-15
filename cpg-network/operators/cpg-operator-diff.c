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
#include "cpg-function.h"
#include "cpg-expression-tree-iter.h"
#include "cpg-network.h"

#include "instructions/cpg-instructions.h"

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
	CpgFunction *function;

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
		                             (GDestroyNotify)cpg_expression_tree_iter_free);
	}

	while (args && nargs && num != 0)
	{
		CpgFunctionArgument *arg;
		CpgFunctionArgument *argnf;
		GSList *instr;

		CpgProperty *p;
		CpgProperty *pnf;

		arg = args->data;
		argnf = nargs->data;

		args = g_list_next (args);
		nargs = g_list_next (nargs);

		p = _cpg_function_argument_get_property (arg);
		pnf = _cpg_function_argument_get_property (argnf);

		instr = g_slist_prepend (NULL,
		                         cpg_instruction_property_new (pnf));

		g_hash_table_insert (ret,
		                     p,
		                     cpg_expression_tree_iter_new_from_instructions (instr));

		g_slist_foreach (instr, (GFunc)cpg_mini_object_free, NULL);
		g_slist_free (instr);

		--num;
	}

	return ret;
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
                    CpgFunction  **func,
                    GList        **syms,
                    gint          *order,
                    GError       **error)
{
	CpgExpression *expr;

	expr = expressions->data;

	*func = derived_function (expressions->data);

	if (!*func)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_ERROR,
		             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
		             "Expected function reference but got `%s'. Use df_dt[] for deriving expressions",
		             cpg_expression_get_as_string (expressions->data));

		return FALSE;
	}

	*order = 1;

	if (expressions->next)
	{
		*order = cpg_expression_evaluate (expressions->next->data);
	}

	*syms = NULL;

	while (towards)
	{
		CpgFunctionArgument *arg;

		arg = derived_arg (*func, towards->data);

		if (!arg)
		{
			g_set_error (error,
			             CPG_SYMBOLIC_ERROR,
			             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Expected function variable but got `%s' for diff of `%s'",
			             cpg_expression_get_as_string (towards->data),
			             cpg_expression_get_as_string (expr));

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

		args = cpg_function_get_arguments (*func);

		while (args)
		{
			if (cpg_function_argument_get_explicit (args->data))
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
resolve_symargs (CpgFunction *f,
                 GList const *symargs)
{
	GList *ret = NULL;

	while (symargs)
	{
		ret = g_list_prepend (ret,
		                      cpg_function_get_argument (f,
		                                                 cpg_function_argument_get_name (symargs->data)));

		symargs = g_list_next (symargs);
	}

	return g_list_reverse (ret);
}

static gboolean
cpg_operator_diff_initialize (CpgOperator   *op,
                              GSList const **expressions,
                              gint           num_expressions,
                              GSList const **indices,
                              gint           num_indices,
                              gint           num_arguments,
                              GError       **error)
{
	CpgOperatorDiff *diff;
	CpgFunction *func;
	GList *symargs;
	GHashTable *property_map;
	CpgFunction *nf = NULL;
	gchar *s;
	GList *item;
	gint i;
	GList *newsymargs;

	if (!CPG_OPERATOR_CLASS (cpg_operator_diff_parent_class)->initialize (op,
	                                                                      expressions,
	                                                                      num_expressions,
	                                                                      indices,
	                                                                      num_indices,
	                                                                      num_arguments,
	                                                                      error))
	{
		return FALSE;
	}

	if (num_expressions <= 0 ||
	    num_expressions > 2 ||
	    (expressions[0]->next && expressions[0]->next->next))
	{
		g_set_error (error,
		             CPG_NETWORK_LOAD_ERROR,
		             CPG_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `diff' expects arguments [Func{,order}{;<towards>}] {optional} <list>");

		return FALSE;
	}

	diff = CPG_OPERATOR_DIFF (op);
	diff->priv->expression = g_object_ref_sink (expressions[0]->data);

	diff->priv->order = 1;

	if (!validate_arguments (expressions[0],
	                         num_expressions > 1 ? expressions[1] : NULL,
	                         &func,
	                         &symargs,
	                         &diff->priv->order,
	                         error))
	{
		return FALSE;
	}

	// Here we are going to generate a new function with represents
	// the symbolic derivation
	s = g_strconcat ("d",
	                 cpg_object_get_id (CPG_OBJECT (func)),
	                 "dt",
	                 NULL);

	nf = CPG_FUNCTION (cpg_object_copy (CPG_OBJECT (func)));
	cpg_object_set_id (CPG_OBJECT (nf), s);
	g_free (s);

	// Resolve the symbols we are going to derive towards as function
	// arguments in the new function
	newsymargs = resolve_symargs (nf, symargs);
	g_list_free (symargs);

	// Start with an empty expression
	cpg_function_set_expression (nf, cpg_expression_new0 ());

	// Map original function properties to the new function arguments
	property_map = get_property_map (cpg_function_get_arguments (func),
	                                 cpg_function_get_arguments (nf),
	                                 -1,
	                                 NULL);

	// Add additional arguments to this function for the time
	// derivative of all the syms (which don't have derivatives present
	// in the current arguments)
	for (i = 0; i < diff->priv->order; ++i)
	{
		for (item = newsymargs; item; item = g_list_next (item))
		{
			CpgFunctionArgument *sarg = item->data;
			CpgFunctionArgument *darg;
			gchar *d;
			gchar *dsname;
			CpgFunctionArgument *oarg;
			CpgProperty *sprop;

			d = g_strnfill (i + 1, '\'');

			dsname = g_strconcat (cpg_function_argument_get_name (sarg),
			                      d,
			                      NULL);

			g_free (d);

			// Test if it already exists
			oarg = cpg_function_get_argument (nf, dsname);
			sprop = _cpg_function_argument_get_property (sarg);

			if (oarg)
			{
				CpgProperty *oprop;

				oprop = _cpg_function_argument_get_property (oarg);

				if (cpg_property_get_derivative (sprop) == oprop)
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
					             CPG_SYMBOLIC_ERROR,
					             CPG_SYMBOLIC_ERROR_INVALID,
					             "There is already an variable `%s' but it is not a derivative of `%s'",
					             cpg_property_get_name (oprop),
					             cpg_property_get_name (sprop));

					goto cleanup;
				}
			}

			darg = cpg_function_argument_new (dsname,
			                                  cpg_expression_new0 (),
			                                  TRUE);

			g_free (dsname);

			cpg_function_add_argument (nf, darg);
		}
	}

	g_list_free (newsymargs);

	diff->priv->derived = cpg_symbolic_derive (diff->priv->expression,
	                                           NULL,
	                                           property_map,
	                                           NULL,
	                                           diff->priv->order,
	                                           CPG_SYMBOLIC_DERIVE_NONE,
	                                           error);

	g_object_ref_sink (diff->priv->derived);

	cpg_function_set_expression (nf, diff->priv->derived);

cleanup:
	diff->priv->function = nf;

	if (property_map)
	{
		g_hash_table_destroy (property_map);
	}

	return diff->priv->derived != NULL;
}

static void
cpg_operator_diff_execute (CpgOperator *op,
                           CpgStack    *stack)
{
	CpgOperatorDiff *d;

	d = (CpgOperatorDiff *)op;

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
cpg_operator_diff_finalize (GObject *object)
{
	CpgOperatorDiff *diff;

	diff = CPG_OPERATOR_DIFF (object);

	if (diff->priv->expression)
	{
		g_object_unref (diff->priv->expression);
	}

	if (diff->priv->derived)
	{
		g_object_unref (diff->priv->derived);
	}

	if (diff->priv->function)
	{
		g_object_unref (diff->priv->function);
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

static CpgFunction *
cpg_operator_diff_get_function (CpgOperator *op,
                                gint        *idx,
                                gint         numidx)
{
	return CPG_OPERATOR_DIFF (op)->priv->function;
}

static CpgOperator *
cpg_operator_diff_copy (CpgOperator *op)
{
	CpgOperatorDiff *diff;
	CpgOperatorDiff *ret;

	diff = CPG_OPERATOR_DIFF (op);

	ret = CPG_OPERATOR_DIFF (g_object_new (CPG_TYPE_OPERATOR_DIFF, NULL));

	CPG_OPERATOR_CLASS (cpg_operator_diff_parent_class)->initialize (CPG_OPERATOR (ret),
	                                                                 cpg_operator_all_expressions (op),
	                                                                 cpg_operator_num_expressions (op),
	                                                                 cpg_operator_all_indices (op),
	                                                                 cpg_operator_num_indices (op),
	                                                                 cpg_operator_get_num_arguments (op),
	                                                                 NULL);

	if (diff->priv->expression)
	{
		ret->priv->expression = g_object_ref_sink (diff->priv->expression);
	}

	if (diff->priv->derived)
	{
		ret->priv->derived = g_object_ref_sink (diff->priv->derived);
	}

	if (diff->priv->function)
	{
		ret->priv->function = g_object_ref (CPG_OBJECT (diff->priv->function));
	}

	ret->priv->order = diff->priv->order;

	return CPG_OPERATOR (ret);
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
	op_class->equal = cpg_operator_diff_equal;
	op_class->reset_cache = cpg_operator_diff_reset_cache;
	op_class->reset = cpg_operator_diff_reset;
	op_class->get_function = cpg_operator_diff_get_function;
	op_class->copy = cpg_operator_diff_copy;

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
