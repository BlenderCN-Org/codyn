/*
 * cpg-operator-pdiff.c
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

#include "cpg-operator-pdiff.h"
#include "cpg-operator.h"
#include "cpg-property.h"
#include "cpg-usable.h"
#include "cpg-integrator.h"
#include "cpg-symbolic.h"
#include "cpg-function.h"
#include "cpg-expression-tree-iter.h"

#include "instructions/cpg-instruction-property.h"
#include "instructions/cpg-instruction-custom-function-ref.h"

#include <math.h>

/**
 * SECTION:cpg-operator-pdiff
 * @short_description: Math operator for partial derivation of a function
 *
 * The #CpgOperatorPDiff is a special operator that can be used in
 * mathematical expressions ('pdiff'). It computes the partial derivative of
 * a defined function
 *
 */

#define CPG_OPERATOR_PDIFF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_PDIFF, CpgOperatorPDiffPrivate))

struct _CpgOperatorPDiffPrivate
{
	CpgExpression *expression;
	CpgExpression *derived;

	gint order;

	CpgFunction *function;
};

G_DEFINE_TYPE (CpgOperatorPDiff,
               cpg_operator_pdiff,
               CPG_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_DERIVED,
	PROP_ORDER
};

static gchar *
cpg_operator_pdiff_get_name ()
{
	return g_strdup ("pdiff");
}

static CpgProperty *
derived_property (CpgExpression *expr,
                  CpgFunction   *func)
{
	GSList const *instr;
	CpgProperty *prop;

	instr = cpg_expression_get_instructions (expr);

	if (instr->next || !CPG_IS_INSTRUCTION_PROPERTY (instr->data))
	{
		return NULL;
	}

	prop = cpg_instruction_property_get_property (instr->data);

	return cpg_object_get_property (CPG_OBJECT (func),
	                                cpg_property_get_name (prop));
}

static CpgFunction *
derived_function (CpgExpression *expr)
{
	GSList const *instr;

	instr = cpg_expression_get_instructions (expr);

	if (instr->next || !CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr->data))
	{
		return NULL;
	}

	return cpg_instruction_custom_function_ref_get_function (instr->data);
}

static GSList *
get_function_symbols (CpgFunction *func)
{
	GSList *ret = NULL;
	GList const *args;

	args = cpg_function_get_arguments (func);

	while (args)
	{
		CpgFunctionArgument *arg;

		arg = args->data;

		if (cpg_function_argument_get_explicit (args->data))
		{
			ret = g_slist_prepend (ret,
			                       _cpg_function_argument_get_property (arg));
		}

		args = g_list_next (args);
	}

	return g_slist_reverse (ret);
}

static void
generate_function (CpgOperatorPDiff *pdiff,
                   CpgFunction      *func)
{
	pdiff->priv->function = CPG_FUNCTION (cpg_object_copy (CPG_OBJECT (func)));

	cpg_function_set_expression (pdiff->priv->function,
	                             cpg_expression_copy (pdiff->priv->derived));
}

static GHashTable *
get_property_map (CpgFunction   *func,
                  GSList const  *expressions,
                  gint           num,
                  GError       **error,
                  gboolean      *ret)
{
	gint nfunc;
	GHashTable *rett;
	GList const *args;

	if (num == 0)
	{
		*ret = TRUE;
		return NULL;
	}

	nfunc = cpg_function_get_n_arguments (func) - cpg_function_get_n_implicit (func);

	if (num != nfunc)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_DERIVE_ERROR,
		             CPG_SYMBOLIC_DERIVE_ERROR_INVALID,
		             "The number of mapped expressions (%d) should be equal to the number of function variables (%d)",
		             num,
		             nfunc);

		*ret = FALSE;
		return NULL;
	}

	rett = g_hash_table_new (g_direct_hash,
	                         g_direct_equal);

	args = cpg_function_get_arguments (func);

	while (num > 0)
	{
		g_hash_table_insert (rett,
		                     _cpg_function_argument_get_property (args->data),
		                     cpg_expression_tree_iter_new (expressions->data));

		args = g_list_next (args);
		expressions = g_slist_next (expressions);

		--num;
	}

	*ret = TRUE;
	return rett;
}

static void
destroy_iter (gpointer key, gpointer value)
{
	cpg_expression_tree_iter_free (value);
}

static gboolean
cpg_operator_pdiff_initialize (CpgOperator   *op,
                               GSList const  *expressions,
                               gint           num_arguments,
                               GError       **error)
{
	CpgOperatorPDiff *pdiff;
	CpgProperty *prop;
	CpgFunction *func;
	GSList *syms;
	gint numsym;
	GHashTable *property_map;
	gboolean noerr;

	if (!CPG_OPERATOR_CLASS (cpg_operator_pdiff_parent_class)->initialize (op,
	                                                                       expressions,
	                                                                       num_arguments,
	                                                                       error))
	{
		return FALSE;
	}

	pdiff = CPG_OPERATOR_PDIFF (op);

	// The first expression must be a function ref and the second must be
	// a property ref
	func = derived_function (expressions->data);
	numsym = g_slist_length ((GSList *)expressions);

	pdiff->priv->order = 1;

	if (!func)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_DERIVE_ERROR,
		             CPG_SYMBOLIC_DERIVE_ERROR_INVALID,
		             "The function `%s' could not be found",
		             cpg_expression_get_as_string (expressions->data));

		return FALSE;
	}

	if (numsym == 2)
	{
		prop = derived_property (expressions->next->data, func);
	}
	else
	{
		// Last can be number or property
		CpgExpression *last;
		CpgExpression *beforelast;

		last = g_slist_last ((GSList *)expressions)->data;
		beforelast = g_slist_nth_data ((GSList *)expressions, numsym - 2);

		prop = derived_property (expressions->next->data, func);

		if (!prop)
		{
			prop = derived_property (beforelast, func);
			pdiff->priv->order = (gint)cpg_expression_evaluate (last);

			--numsym;
		}
	}

	if (!prop)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_DERIVE_ERROR,
		             CPG_SYMBOLIC_DERIVE_ERROR_INVALID,
		             "The property `%s' is not a variable of the function `%s'",
		             cpg_expression_get_as_string (expressions->next->data),
		             cpg_expression_get_as_string (expressions->data));

		return FALSE;
	}

	// The remaining numsym expressions (after the first function) are mapped
	// onto the explicit arguments of the function for derivation
	property_map = get_property_map (func,
	                                 expressions->next,
	                                 numsym - 2,
	                                 error,
	                                 &noerr);

	if (!noerr)
	{
		return FALSE;
	}

	syms = get_function_symbols (func);

	pdiff->priv->derived = cpg_symbolic_derive (cpg_function_get_expression (func),
	                                            syms,
	                                            property_map,
	                                            prop,
	                                            pdiff->priv->order,
	                                            CPG_SYMBOLIC_DERIVE_PARTIAL,
	                                            error);

	g_slist_free (syms);

	if (property_map)
	{
		g_hash_table_foreach (property_map,
		                      (GHFunc)destroy_iter,
		                      NULL);

		g_hash_table_destroy (property_map);
	}

	if (pdiff->priv->derived)
	{
		generate_function (pdiff, func);
	}

	return pdiff->priv->derived != NULL;
}

static void
cpg_operator_pdiff_execute (CpgOperator *op,
                            CpgStack    *stack)
{
	CpgOperatorPDiff *d;

	d = (CpgOperatorPDiff *)op;

	if (d->priv->function)
	{
		cpg_function_execute (d->priv->function,
		                      cpg_operator_get_num_arguments (op),
		                      stack);
	}
	else
	{
		gint i;

		// Keep the stack sane
		for (i = 0; i < cpg_operator_get_num_arguments (op); ++i)
		{
			cpg_stack_pop (stack);
		}

		cpg_stack_push (stack, 0);
	}
}

static gint
cpg_operator_pdiff_validate_num_arguments (gint numsym,
                                           gint num)
{
	return numsym >= 2;
}

static void
cpg_operator_pdiff_finalize (GObject *object)
{
	CpgOperatorPDiff *pdiff;

	pdiff = CPG_OPERATOR_PDIFF (object);

	if (pdiff->priv->expression)
	{
		g_object_unref (pdiff->priv->expression);
	}

	if (pdiff->priv->derived)
	{
		g_object_unref (pdiff->priv->derived);
	}

	if (pdiff->priv->function)
	{
		g_object_unref (pdiff->priv->function);
	}

	G_OBJECT_CLASS (cpg_operator_pdiff_parent_class)->finalize (object);
}

static void
cpg_operator_pdiff_set_property (GObject      *object,
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
cpg_operator_pdiff_get_property (GObject    *object,
                                 guint       prop_id,
                                 GValue     *value,
                                 GParamSpec *pspec)
{
	CpgOperatorPDiff *self = CPG_OPERATOR_PDIFF (object);

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
cpg_operator_pdiff_equal (CpgOperator *op,
                            CpgOperator *other)
{
	CpgOperatorPDiff *pdiff;
	CpgOperatorPDiff *odel;

	if (!CPG_IS_OPERATOR_PDIFF (other))
	{
		return FALSE;
	}

	pdiff = CPG_OPERATOR_PDIFF (op);
	odel = CPG_OPERATOR_PDIFF (other);

	if (pdiff->priv->order != odel->priv->order)
	{
		return FALSE;
	}

	if (!cpg_expression_equal (pdiff->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cpg_operator_pdiff_reset_cache (CpgOperator *operator)
{
	CpgOperatorPDiff *self;

	CPG_OPERATOR_CLASS (cpg_operator_pdiff_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CpgOperatorPDiff *)operator;

	if (self->priv->derived)
	{
		cpg_expression_reset_cache (self->priv->derived);
	}
}

static void
cpg_operator_pdiff_reset (CpgOperator *operator)
{
	CpgOperatorPDiff *self;

	CPG_OPERATOR_CLASS (cpg_operator_pdiff_parent_class)->reset (operator);

	self = CPG_OPERATOR_PDIFF (operator);

	if (self->priv->derived)
	{
		cpg_expression_reset (self->priv->derived);
	}
}

static void
cpg_operator_pdiff_class_init (CpgOperatorPDiffClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_pdiff_finalize;

	object_class->get_property = cpg_operator_pdiff_get_property;
	object_class->set_property = cpg_operator_pdiff_set_property;

	op_class->get_name = cpg_operator_pdiff_get_name;
	op_class->execute = cpg_operator_pdiff_execute;
	op_class->initialize = cpg_operator_pdiff_initialize;
	op_class->validate_num_arguments = cpg_operator_pdiff_validate_num_arguments;
	op_class->equal = cpg_operator_pdiff_equal;
	op_class->reset_cache = cpg_operator_pdiff_reset_cache;
	op_class->reset = cpg_operator_pdiff_reset;

	g_type_class_add_private (object_class, sizeof(CpgOperatorPDiffPrivate));

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
cpg_operator_pdiff_init (CpgOperatorPDiff *self)
{
	self->priv = CPG_OPERATOR_PDIFF_GET_PRIVATE (self);
}

CpgOperatorPDiff *
cpg_operator_pdiff_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_PDIFF, NULL);
}

/**
 * cpg_operator_pdiff_get_expression:
 * @pdiff: A #CpgOperatorPDiff
 *
 * Get the expression to be pdiff.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_pdiff_get_expression (CpgOperatorPDiff *pdiff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_PDIFF (pdiff), NULL);

	return pdiff->priv->expression;
}

/**
 * cpg_operator_pdiff_get_derived:
 * @pdiff: A #CpgOperatorPDiff
 *
 * Get the derived expression.
 *
 * Returns: (transfer none): A #CpgExpression
 *
 **/
CpgExpression *
cpg_operator_pdiff_get_derived (CpgOperatorPDiff *pdiff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_PDIFF (pdiff), NULL);

	return pdiff->priv->derived;
}

/**
 * cpg_operator_pdiff_get_order:
 * @pdiff: A #CpgOperatorPDiff
 *
 * Get the order.
 *
 * Returns: The order
 *
 **/
gint
cpg_operator_pdiff_get_order (CpgOperatorPDiff *pdiff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_PDIFF (pdiff), 0.0);

	return pdiff->priv->order;
}

CpgFunction *
cpg_operator_pdiff_get_function (CpgOperatorPDiff *pdiff)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_PDIFF (pdiff), NULL);

	return pdiff->priv->function;
}
