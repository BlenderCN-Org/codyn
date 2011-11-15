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
	GSList *functions;
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
		CpgExpression *expr;
		CpgFunction *func;

		expr = expressions->data;

		func = derived_function (expressions->data);

		if (!func)
		{
			g_set_error (error,
			             CPG_SYMBOLIC_DERIVE_ERROR,
			             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
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
			             CPG_SYMBOLIC_DERIVE_ERROR,
			             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
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

	// TODO: solve the system of equations

	return TRUE;
}

static void
cpg_operator_linsolve_execute (CpgOperator *op,
                               CpgStack    *stack)
{
	CpgOperatorLinsolve *d;

	d = (CpgOperatorLinsolve *)op;

	// TODO

	cpg_stack_push (stack, 0);
}

static void
cpg_operator_linsolve_finalize (GObject *object)
{
	CpgOperatorLinsolve *linsolve;

	linsolve = CPG_OPERATOR_LINSOLVE (object);

	G_OBJECT_CLASS (cpg_operator_linsolve_parent_class)->finalize (object);
}

static gboolean
cpg_operator_linsolve_equal (CpgOperator *op,
                             CpgOperator *other)
{
	CpgOperatorLinsolve *linsolve;
	CpgOperatorLinsolve *odel;

	if (!CPG_IS_OPERATOR_LINSOLVE (other))
	{
		return FALSE;
	}

	linsolve = CPG_OPERATOR_LINSOLVE (op);
	odel = CPG_OPERATOR_LINSOLVE (other);

	// TODO

	return TRUE;
}

static void
cpg_operator_linsolve_reset_cache (CpgOperator *operator)
{
	CpgOperatorLinsolve *self;

	CPG_OPERATOR_CLASS (cpg_operator_linsolve_parent_class)->reset_cache (operator);

	/* Omit type check to be faster */
	self = (CpgOperatorLinsolve *)operator;

	// TODO
}

static void
cpg_operator_linsolve_reset (CpgOperator *operator)
{
	CpgOperatorLinsolve *self;

	CPG_OPERATOR_CLASS (cpg_operator_linsolve_parent_class)->reset (operator);

	self = CPG_OPERATOR_LINSOLVE (operator);

	// TODO
}

static CpgFunction *
cpg_operator_linsolve_get_function (CpgOperator *op,
                                    gint        *idx,
                                    gint         numidx)
{
	// TODO
	return NULL;
}

static CpgOperator *
cpg_operator_linsolve_copy (CpgOperator *op)
{
	CpgOperatorLinsolve *linsolve;
	CpgOperatorLinsolve *ret;

	linsolve = CPG_OPERATOR_LINSOLVE (op);

	ret = CPG_OPERATOR_LINSOLVE (g_object_new (CPG_TYPE_OPERATOR_LINSOLVE, NULL));

	CPG_OPERATOR_CLASS (cpg_operator_linsolve_parent_class)->initialize (CPG_OPERATOR (ret),
	                                                                     cpg_operator_all_expressions (op),
	                                                                     cpg_operator_num_expressions (op),
	                                                                     cpg_operator_all_indices (op),
	                                                                     cpg_operator_num_indices (op),
	                                                                     cpg_operator_get_num_arguments (op),
	                                                                     NULL);

	// TODO

	return CPG_OPERATOR (ret);
}

static void
cpg_operator_linsolve_class_init (CpgOperatorLinsolveClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_linsolve_finalize;

	op_class->get_name = cpg_operator_linsolve_get_name;
	op_class->execute = cpg_operator_linsolve_execute;
	op_class->initialize = cpg_operator_linsolve_initialize;
	op_class->equal = cpg_operator_linsolve_equal;
	op_class->reset_cache = cpg_operator_linsolve_reset_cache;
	op_class->reset = cpg_operator_linsolve_reset;
	op_class->get_function = cpg_operator_linsolve_get_function;
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
