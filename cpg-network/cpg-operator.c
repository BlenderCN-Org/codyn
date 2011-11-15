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
#include <math.h>

#define CPG_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR, CpgOperatorPrivate))

struct _CpgOperatorPrivate
{
	GSList **expressions;
	gint num_expressions;

	GSList **indices;
	gint num_indices;

	gint num_arguments;
};

G_DEFINE_ABSTRACT_TYPE (CpgOperator,
                        cpg_operator,
                        G_TYPE_OBJECT);

GQuark
cpg_operator_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_operator_error");
	}

	return quark;
}

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
foreach_expression_impl (GSList      **multiexpr,
                         gint          num,
                         GFunc         callback,
                         gpointer      userdata)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		g_slist_foreach (multiexpr[i], callback, userdata);
	}
}

typedef struct
{
	GFunc func;
	gpointer data;
} UData;

static void
foreach_function_expression_impl (CpgFunction *func,
                                  UData       *data)
{
	cpg_object_foreach_expression (CPG_OBJECT (func),
	                               (CpgForeachExpressionFunc)data->func,
	                               data->data);
}

static void
foreach_function_expression (CpgOperator *op,
                             GFunc        func,
                             gpointer     userdata)
{
	UData data = {func, userdata};

	cpg_operator_foreach_function (op,
	                               (CpgForeachFunctionFunc)foreach_function_expression_impl,
	                               &data);
}

static void
foreach_expression (CpgOperator *op,
                    GFunc        callback,
                    gpointer     userdata)
{
	foreach_expression_impl (op->priv->expressions,
	                         op->priv->num_expressions,
	                         callback,
	                         userdata);

	foreach_expression_impl (op->priv->indices,
	                         op->priv->num_indices,
	                         callback,
	                         userdata);

	foreach_function_expression (op,
	                             callback,
	                             userdata);
}

static void
cpg_operator_reset_cache_default (CpgOperator *op)
{
	foreach_expression (op,
	                    (GFunc)cpg_expression_reset_cache,
	                    NULL);
}

static void
cpg_operator_reset_variadic_default (CpgOperator *op)
{
	foreach_expression (op,
	                    (GFunc)cpg_expression_reset_variadic,
	                    NULL);
}

static void
cpg_operator_reset_default (CpgOperator *op)
{
	foreach_expression (op,
	                    (GFunc)cpg_expression_reset,
	                    NULL);
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

static GSList **
copy_2dim_slist (GSList const **lst,
                 gint           num)
{
	GSList **rret;
	gint i;

	rret = g_new0 (GSList *, num);

	for (i = 0; i < num; ++i)
	{
		GSList const *item = lst[i];
		GSList *ret = NULL;

		while (item)
		{
			if (g_object_is_floating (item->data))
			{
				g_object_ref_sink (item->data);
			}
			else
			{
				g_object_ref (item->data);
			}

			ret = g_slist_prepend (ret, item->data);
			item = g_slist_next (item);
		}

		rret[i] = g_slist_reverse (ret);
	}

	return rret;
}

static void
free_2dim_slist (GSList **lst,
                 gint     num)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		g_slist_foreach (lst[i], (GFunc)g_object_unref, NULL);
	}

	g_free (lst);
}

static gboolean
cpg_operator_initialize_default (CpgOperator   *op,
                                 GSList const **expressions,
                                 gint           num_expressions,
                                 GSList const **indices,
                                 gint           num_indices,
                                 gint           num_arguments,
                                 GError       **error)
{
	op->priv->num_arguments = num_arguments;

	op->priv->expressions = copy_2dim_slist (expressions,
	                                         num_expressions);
	op->priv->num_expressions = num_expressions;

	op->priv->indices = copy_2dim_slist (indices,
	                                     num_indices);
	op->priv->num_indices = num_indices;

	return TRUE;
}

static void
cpg_operator_finalize (GObject *object)
{
	CpgOperator *operator;

	operator = CPG_OPERATOR (object);

	free_2dim_slist (operator->priv->expressions,
	                 operator->priv->num_expressions);

	free_2dim_slist (operator->priv->indices,
	                 operator->priv->num_indices);

	G_OBJECT_CLASS (cpg_operator_parent_class)->finalize (object);
}

static gboolean
cpg_operator_equal_default (CpgOperator *op,
                            CpgOperator *other)
{
	return FALSE;
}

static CpgFunction *
cpg_operator_get_function_default (CpgOperator *op,
                                   gint        *idx,
                                   gint         numidx)
{
	return NULL;
}

static CpgOperator *
cpg_operator_copy_default (CpgOperator *src)
{
	CpgOperator *ret;

	ret = g_object_new (G_OBJECT_TYPE (src), NULL);

	cpg_operator_initialize (ret,
	                         (GSList const **)src->priv->expressions,
	                         src->priv->num_expressions,
	                         (GSList const **)src->priv->indices,
	                         src->priv->num_indices,
	                         src->priv->num_arguments,
	                         NULL);

	return ret;
}

static void
cpg_operator_foreach_function_default (CpgOperator            *op,
                                       CpgForeachFunctionFunc  func,
                                       gpointer                userdata)
{
}

static void
cpg_operator_class_init (CpgOperatorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_operator_finalize;

	klass->execute = cpg_operator_execute_default;
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
	klass->foreach_function = cpg_operator_foreach_function_default;

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

GSList const **
cpg_operator_all_expressions (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return (GSList const **)op->priv->expressions;
}

GSList const **
cpg_operator_all_indices (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return (GSList const **)op->priv->indices;
}

/**
 * cpg_operator_get_expressions:
 * @op: A #CpgOperator
 * @idx: the index
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CpgExpression) (transfer none): a list of #CpgExpression
 **/
GSList const *
cpg_operator_get_expressions (CpgOperator *op,
                              gint         idx)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);
	g_return_val_if_fail (idx >= 0 && idx < op->priv->num_expressions, NULL);

	return op->priv->expressions[idx];
}

gint
cpg_operator_num_expressions (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), 0);

	return op->priv->num_expressions;
}

/**
 * cpg_operator_get_expressions:
 * @op: A #CpgOperator
 * @idx: the index
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CpgExpression) (transfer none): a list of #CpgExpression
 **/
GSList const *
cpg_operator_get_indices (CpgOperator *op,
                          gint         idx)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);
	g_return_val_if_fail (idx >= 0 && idx < op->priv->num_indices, NULL);

	return op->priv->indices[idx];
}

gint
cpg_operator_num_indices (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), 0);

	return op->priv->num_indices;
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
                         GSList const **expressions,
                         gint           num_expressions,
                         GSList const **indices,
                         gint           num_indices,
                         gint           num_arguments,
                         GError       **error)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), FALSE);

	return CPG_OPERATOR_GET_CLASS (op)->initialize (op,
	                                                expressions,
	                                                num_expressions,
	                                                indices,
	                                                num_indices,
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
cpg_operator_get_function (CpgOperator *op,
                           gint        *idx,
                           gint         numidx)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	return CPG_OPERATOR_GET_CLASS (op)->get_function (op, idx, numidx);
}

void
_cpg_operator_set_num_arguments (CpgOperator *op,
                                 gint         num)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	op->priv->num_arguments = num;
}

void
cpg_operator_foreach_function (CpgOperator            *op,
                               CpgForeachFunctionFunc  func,
                               gpointer                userdata)
{
	g_return_if_fail (CPG_IS_OPERATOR (op));

	if (func == NULL)
	{
		return;
	}

	CPG_OPERATOR_GET_CLASS (op)->foreach_function (op, func, userdata);
}

CpgFunction *
cpg_operator_get_primary_function (CpgOperator *op)
{
	g_return_val_if_fail (CPG_IS_OPERATOR (op), NULL);

	if (op->priv->num_indices == 0)
	{
		gint idx = 0;

		return cpg_operator_get_function (op, &idx, 1);
	}
	else
	{
		GArray *ret = g_array_new (FALSE, TRUE, sizeof (gint));
		gint num = 0;
		gint i;

		// Try here to evaluate the indices
		for (i = 0; i < op->priv->num_indices; ++i)
		{
			GSList const *idx = op->priv->indices[i];

			while (idx)
			{
				if (!cpg_expression_get_instructions (idx->data))
				{
					g_array_free (ret, TRUE);
					ret = NULL;
					break;
				}
				else
				{
					gint val = rint (cpg_expression_evaluate (idx->data));

					g_array_append_val (ret, val);
					++num;
				}

				idx = g_slist_next (idx);
			}

			if (!ret)
			{
				break;
			}
		}

		if (ret)
		{
			CpgFunction *func;

			gint *ptr = (gint *)g_array_free (ret, FALSE);
			func = cpg_operator_get_function (op, ptr, num);
			g_free (ptr);

			return func;
		}
	}

	return NULL;
}
