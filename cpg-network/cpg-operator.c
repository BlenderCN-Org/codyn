/*
 * cpg-operator.c
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

#include "cpg-operator.h"
#include "cpg-expression.h"
#include "cpg-integrator.h"

#define CPG_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR, CpgOperatorPrivate))

struct _CpgOperatorPrivate
{
	GSList *expressions;
	gint num_arguments;
};

G_DEFINE_ABSTRACT_TYPE (CpgOperator,
                        cpg_operator,
                        G_TYPE_OBJECT);

/* Default implementation */
static gchar *
cpg_operator_get_name_default ()
{
	g_assert_not_reached ();
}

static void
cpg_operator_execute_default (CpgOperator     *op,
                              CpgStack        *stack)
{
	cpg_stack_push (stack, 0);
}

static void
cpg_operator_reset_cache_default (CpgOperator *op)
{
	g_slist_foreach (op->priv->expressions,
	                 (GFunc)cpg_expression_reset_cache,
	                 NULL);
}

static void
cpg_operator_reset_variadic_default (CpgOperator *op)
{
	g_slist_foreach (op->priv->expressions,
	                 (GFunc)cpg_expression_reset_variadic,
	                 NULL);
}

static void
cpg_operator_reset_default (CpgOperator *op)
{
	g_slist_foreach (op->priv->expressions,
	                 (GFunc)cpg_expression_reset,
	                 NULL);
}

static gboolean
cpg_operator_validate_num_arguments_default (gint numsym, gint num)
{
	return TRUE;
}

static void
cpg_operator_step_default (CpgOperator     *op,
                           CpgIntegrator   *integrator,
                           gdouble          t,
                           gdouble          timestep)
{
}

static void
cpg_operator_step_prepare_default (CpgOperator     *op,
                                  CpgIntegrator   *integrator,
                                  gdouble          t,
                                  gdouble          timestep)
{
}

static void
cpg_operator_step_evaluate_default (CpgOperator     *op,
                                    CpgIntegrator   *integrator,
                                    gdouble          t,
                                    gdouble          timestep)
{
}


static gboolean
cpg_operator_initialize_default (CpgOperator   *op,
                                 GSList const  *expressions,
                                 gint           num_arguments,
                                 GError       **error)
{
	op->priv->num_arguments = num_arguments;

	while (expressions)
	{
		op->priv->expressions =
			g_slist_prepend (op->priv->expressions,
			                 g_object_ref (expressions->data));

		expressions = g_slist_next (expressions);
	}

	op->priv->expressions = g_slist_reverse (op->priv->expressions);
	return TRUE;
}

static void
cpg_operator_finalize (GObject *object)
{
	CpgOperator *operator;

	operator = CPG_OPERATOR (object);

	g_slist_foreach (operator->priv->expressions, (GFunc)g_object_unref, NULL);
	g_slist_free (operator->priv->expressions);

	G_OBJECT_CLASS (cpg_operator_parent_class)->finalize (object);
}

static gboolean
cpg_operator_equal_default (CpgOperator *op,
                            CpgOperator *other)
{
	return FALSE;
}

static CpgFunction *
cpg_operator_get_function_default (CpgOperator *op)
{
	return NULL;
}

static CpgOperator *
cpg_operator_copy_default (CpgOperator *src)
{
	CpgOperator *ret;

	ret = g_object_new (G_OBJECT_TYPE (src), NULL);

	cpg_operator_initialize (ret,
	                         src->priv->expressions,
	                         src->priv->num_arguments,
	                         NULL);

	return ret;
}

static void
cpg_operator_class_init (CpgOperatorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_operator_finalize;

	klass->execute = cpg_operator_execute_default;
	klass->validate_num_arguments = cpg_operator_validate_num_arguments_default;
	klass->reset_cache = cpg_operator_reset_cache_default;
	klass->reset = cpg_operator_reset_default;
	klass->reset_variadic = cpg_operator_reset_variadic_default;
	klass->step = cpg_operator_step_default;
	klass->step_prepare = cpg_operator_step_prepare_default;
	klass->step_evaluate = cpg_operator_step_evaluate_default;
	klass->get_name = cpg_operator_get_name_default;
	klass->initialize = cpg_operator_initialize_default;
	klass->equal = cpg_operator_equal_default;
	klass->get_function = cpg_operator_get_function_default;
	klass->copy = cpg_operator_copy_default;

	g_type_class_add_private (object_class, sizeof (CpgOperatorPrivate));
}

static void
cpg_operator_init (CpgOperator *self)
{
	self->priv = CPG_OPERATOR_GET_PRIVATE (self);
}

/**
 * cpg_operator_execute:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 * @stack: A #CpgStack
 *
 * Execute the operator. This function should always be overridden by
 * operator implementations and should always push exactly one number
 * on the stack.
 *
 **/
void
cpg_operator_execute (CpgOperator     *op,
                      CpgStack        *stack)
{
	/* Omit type check to increase speed */
	CPG_OPERATOR_GET_CLASS (op)->execute (op, stack);
}

/**
 * cpg_operator_get_class_name:
 * @op: A #CpgOperatorClass
 *
 * Get the operator name. This is the identifier that is used in expressions,
 * and thus can only contain valid identifier characters.
 *
 * Returns: a newly allocated string with the operator name, use #g_free to
 * free the value when it's no longer needed.
 *
 **/
gchar const *
cpg_operator_get_class_name (CpgOperatorClass *klass)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_CLASS (klass), NULL);

	if (!klass->name)
	{
		klass->name = klass->get_name ();
	}

	return klass->name;
}

/**
 * cpg_operator_get_name:
 * @op: A #CpgOperator
 *
 * Get the operator name. This is the identifier that is used in expressions,
 * and thus can only contain valid identifier characters.
 *
 * Returns: a newly allocated string with the operator name, use #g_free to
 * free the value when it's no longer needed.
 *
 **/
gchar const *
cpg_operator_get_name (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return cpg_operator_get_class_name (CPG_OPERATOR_GET_CLASS (op));
}

/**
 * cpg_operator_validate_num_arguments:
 * @op: A #CpgOperator
 *
 * Get the number of arguments that the operators expects.
 *
 * Returns: the number of arguments or -1 if the operator accepts a variable
 *          number of arguments.
 *
 **/
gboolean
cpg_operator_validate_num_arguments (CpgOperatorClass *klass,
                                     gint         numsym,
                                     gint         num)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_CLASS (klass), FALSE);

	return klass->validate_num_arguments (numsym, num);
}

/**
 * cpg_operator_reset_cache:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Reset the cache of the operator instance.
 *
 **/
void
cpg_operator_reset_cache (CpgOperator *op)
{
	/* Omit type check to increase speed */
	CPG_OPERATOR_GET_CLASS (op)->reset_cache (op);
}

/**
 * cpg_operator_reset_variadic:
 * @op: A #CpgOperator
 * @data: A #CpgOperatorData
 *
 * Reset the variadic cache of the operator instance.
 *
 **/
void
cpg_operator_reset_variadic (CpgOperator *op)
{
	/* Omit type check to increase speed */
	CPG_OPERATOR_GET_CLASS (op)->reset_variadic (op);
}

/**
 * cpg_operator_get_expressions:
 * @op: A #CpgOperator
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CpgExpression) (transfer none): a list of #CpgExpression
 **/
GSList const *
cpg_operator_get_expressions (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return op->priv->expressions;
}

void
cpg_operator_step (CpgOperator     *op,
                   CpgIntegrator   *integrator,
                   gdouble          t,
                   gdouble          timestep)
{
	/* Omit type check to increase speed */
	return CPG_OPERATOR_GET_CLASS (op)->step (op, integrator, t, timestep);
}

void
cpg_operator_step_prepare (CpgOperator     *op,
                          CpgIntegrator   *integrator,
                          gdouble          t,
                          gdouble          timestep)
{
	/* Omit type check to increase speed */
	return CPG_OPERATOR_GET_CLASS (op)->step_prepare (op, integrator, t, timestep);
}

void
cpg_operator_step_evaluate (CpgOperator     *op,
                            CpgIntegrator   *integrator,
                            gdouble          t,
                            gdouble          timestep)
{
	/* Omit type check to increase speed */
	return CPG_OPERATOR_GET_CLASS (op)->step_evaluate (op, integrator, t, timestep);
}

gboolean
cpg_operator_initialize (CpgOperator   *op,
                         GSList const  *expressions,
                         gint           num_arguments,
                         GError       **error)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), FALSE);

	return CPG_OPERATOR_GET_CLASS (op)->initialize (op,
	                                                expressions,
	                                                num_arguments,
	                                                error);
}

/**
 * cpg_operator_copy:
 * @op: A #CpgOperator
 *
 * Copy an operator.
 *
 * Returns: (transfer full): A #CpgOperator
 *
 **/
CpgOperator *
cpg_operator_copy (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_CLASS (op)->copy (op);
}

void
cpg_operator_reset (CpgOperator *op)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	CPG_OPERATOR_GET_CLASS (op)->reset (op);
}

gboolean
cpg_operator_equal (CpgOperator *op,
                    CpgOperator *other)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), FALSE);

	if (other == NULL)
	{
		return FALSE;
	}

	return CPG_OPERATOR_GET_CLASS (op)->equal (op, other);
}

gint
cpg_operator_get_num_arguments (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), 0);

	return op->priv->num_arguments;
}

CpgFunction *
cpg_operator_get_function (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_CLASS (op)->get_function (op);
}
