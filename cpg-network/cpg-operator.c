/*
 * cpg-operator.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-operator.h"
#include "cpg-expression.h"
#include "cpg-integrator.h"

#define CPG_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR, CpgOperatorPrivate))
#define CPG_OPERATOR_CLASS_GET_PRIVATE(klass)(G_TYPE_CLASS_GET_PRIVATE((klass), CPG_TYPE_OPERATOR, CpgOperatorClassPrivate))

struct _CpgOperatorPrivate
{
	GSList *expressions;
};

struct _CpgOperatorClassPrivate
{
	gchar *name;
};

G_DEFINE_ABSTRACT_TYPE_WITH_CODE (CpgOperator,
                                  cpg_operator,
                                  G_TYPE_OBJECT,
                                  g_type_add_class_private (g_define_type_id, sizeof (CpgOperatorClassPrivate)));

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
cpg_operator_validate_num_arguments_default (gint num)
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


static void
cpg_operator_initialize_default (CpgOperator  *op,
                                 GSList const *expressions)
{
	while (expressions)
	{
		op->priv->expressions =
			g_slist_prepend (op->priv->expressions,
			                 g_object_ref (expressions->data));

		expressions = g_slist_next (expressions);
	}

	op->priv->expressions = g_slist_reverse (op->priv->expressions);
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

	klass->priv =  CPG_OPERATOR_CLASS_GET_PRIVATE (klass);

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

	if (!klass->priv->name)
	{
		klass->priv->name = klass->get_name ();
	}

	return klass->priv->name;
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
                                     gint         num)
{
	g_return_val_if_fail (CPG_IS_OPERATOR_CLASS (klass), FALSE);

	return klass->validate_num_arguments (num);
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

void
cpg_operator_initialize (CpgOperator  *op,
                         GSList const *expressions)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	return CPG_OPERATOR_GET_CLASS (op)->initialize (op, expressions);
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
	CpgOperator *ret;

	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	ret = g_object_new (G_OBJECT_TYPE (op), NULL);
	cpg_operator_initialize (ret, op->priv->expressions);

	return ret;
}

void
cpg_operator_reset (CpgOperator *op)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	CPG_OPERATOR_GET_CLASS (op)->reset (op);
}
