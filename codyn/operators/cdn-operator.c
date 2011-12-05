/*
 * cdn-operator.c
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

#include "cdn-operator.h"
#include "cdn-expression.h"
#include "integrators/cdn-integrator.h"
#include <math.h>

#define CDN_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR, CdnOperatorPrivate))

struct _CdnOperatorPrivate
{
	GSList **expressions;
	gint num_expressions;

	GSList **indices;
	gint num_indices;

	gint num_arguments;
};

G_DEFINE_ABSTRACT_TYPE (CdnOperator,
                        cdn_operator,
                        G_TYPE_OBJECT);

GQuark
cdn_operator_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_operator_error");
	}

	return quark;
}

/* Default implementation */
static gchar *
cdn_operator_get_name_default ()
{
	g_assert_not_reached ();
}

static void
cdn_operator_execute_default (CdnOperator     *op,
                              CdnStack        *stack)
{
	CdnFunction *f;

	f = cdn_operator_get_primary_function (op);

	if (!f)
	{
		gint i;

		g_warning ("Operator `%s' cannot be properly executed",
		           cdn_operator_get_name (op));

		// That's not good, but make sure the stack is ok
		for (i = 0; i < op->priv->num_arguments; ++i)
		{
			cdn_stack_pop (stack);
		}

		cdn_stack_push (stack, 0);
	}
	else
	{
		cdn_function_execute (f, op->priv->num_arguments, stack);
	}
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

static void
disable_cache (CdnExpression *expr)
{
	cdn_expression_set_has_cache (expr, FALSE);
}

static gboolean
cdn_operator_initialize_default (CdnOperator      *op,
                                 GSList const    **expressions,
                                 gint              num_expressions,
                                 GSList const    **indices,
                                 gint              num_indices,
                                 gint              num_arguments,
                                 GError          **error)
{
	op->priv->num_arguments = num_arguments;

	op->priv->expressions = copy_2dim_slist (expressions,
	                                         num_expressions);
	op->priv->num_expressions = num_expressions;

	op->priv->indices = copy_2dim_slist (indices,
	                                     num_indices);
	op->priv->num_indices = num_indices;

	// Set no-cache on indices
	foreach_expression_impl (op->priv->indices,
	                         op->priv->num_indices,
	                         (GFunc)disable_cache,
	                         NULL);

	return TRUE;
}

static void
cdn_operator_finalize (GObject *object)
{
	CdnOperator *operator;

	operator = CDN_OPERATOR (object);

	free_2dim_slist (operator->priv->expressions,
	                 operator->priv->num_expressions);

	free_2dim_slist (operator->priv->indices,
	                 operator->priv->num_indices);

	cdn_operator_foreach_function (operator, (CdnForeachFunctionFunc)g_object_unref, NULL);

	G_OBJECT_CLASS (cdn_operator_parent_class)->finalize (object);
}

static gboolean
compare_expressions (GSList **e1,
                     GSList **e2,
                     gint     num)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		GSList *ee1 = e1[i];
		GSList *ee2 = e2[i];

		while (ee1 && ee2)
		{
			if (!cdn_expression_equal (ee1->data, ee2->data))
			{
				return FALSE;
			}

			ee1 = g_slist_next (ee1);
			ee2 = g_slist_next (ee2);
		}

		if (ee1 || ee2)
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
cdn_operator_equal_default (CdnOperator *op,
                            CdnOperator *other)
{
	if (G_OBJECT_TYPE (op) != G_OBJECT_TYPE (other))
	{
		return FALSE;
	}

	if (op->priv->num_arguments != other->priv->num_arguments)
	{
		return FALSE;
	}

	if (op->priv->num_expressions != other->priv->num_expressions)
	{
		return FALSE;
	}

	if (op->priv->num_indices != other->priv->num_indices)
	{
		return FALSE;
	}

	return compare_expressions (op->priv->expressions,
	                            other->priv->expressions,
	                            op->priv->num_expressions) &&
	       compare_expressions (op->priv->indices,
	                            other->priv->indices,
	                            op->priv->num_indices);
}

static CdnFunction *
cdn_operator_get_function_default (CdnOperator *op,
                                   gint        *idx,
                                   gint         numidx)
{
	return NULL;
}

static CdnOperator *
cdn_operator_copy_default (CdnOperator *src)
{
	CdnOperator *ret;

	ret = g_object_new (G_OBJECT_TYPE (src), NULL);

	cdn_operator_initialize (ret,
	                         (GSList const **)src->priv->expressions,
	                         src->priv->num_expressions,
	                         (GSList const **)src->priv->indices,
	                         src->priv->num_indices,
	                         src->priv->num_arguments,
	                         NULL);

	return ret;
}

static void
cdn_operator_foreach_function_default (CdnOperator            *op,
                                       CdnForeachFunctionFunc  func,
                                       gpointer                userdata)
{
}

static void
cdn_operator_class_init (CdnOperatorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_operator_finalize;

	klass->execute = cdn_operator_execute_default;
	klass->get_name = cdn_operator_get_name_default;
	klass->initialize = cdn_operator_initialize_default;
	klass->equal = cdn_operator_equal_default;
	klass->get_function = cdn_operator_get_function_default;
	klass->copy = cdn_operator_copy_default;
	klass->foreach_function = cdn_operator_foreach_function_default;

	g_type_class_add_private (object_class, sizeof (CdnOperatorPrivate));
}

static void
cdn_operator_init (CdnOperator *self)
{
	self->priv = CDN_OPERATOR_GET_PRIVATE (self);
}

/**
 * cdn_operator_execute:
 * @op: A #CdnOperator
 * @data: A #CdnOperatorData
 * @stack: A #CdnStack
 *
 * Execute the operator. This function should always be overridden by
 * operator implementations and should always push exactly one number
 * on the stack.
 *
 **/
void
cdn_operator_execute (CdnOperator     *op,
                      CdnStack        *stack)
{
	/* Omit type check to increase speed */
	CDN_OPERATOR_GET_CLASS (op)->execute (op, stack);
}

/**
 * cdn_operator_get_class_name:
 * @op: A #CdnOperatorClass
 *
 * Get the operator name. This is the identifier that is used in expressions,
 * and thus can only contain valid identifier characters.
 *
 * Returns: a newly allocated string with the operator name, use #g_free to
 * free the value when it's no longer needed.
 *
 **/
gchar const *
cdn_operator_get_class_name (CdnOperatorClass *klass)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_CLASS (klass), NULL);

	if (!klass->name)
	{
		klass->name = klass->get_name ();
	}

	return klass->name;
}

/**
 * cdn_operator_get_name:
 * @op: A #CdnOperator
 *
 * Get the operator name. This is the identifier that is used in expressions,
 * and thus can only contain valid identifier characters.
 *
 * Returns: a newly allocated string with the operator name, use #g_free to
 * free the value when it's no longer needed.
 *
 **/
gchar const *
cdn_operator_get_name (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);

	return cdn_operator_get_class_name (CDN_OPERATOR_GET_CLASS (op));
}

GSList const **
cdn_operator_all_expressions (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);

	return (GSList const **)op->priv->expressions;
}

GSList const **
cdn_operator_all_indices (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);

	return (GSList const **)op->priv->indices;
}

/**
 * cdn_operator_get_expressions:
 * @op: A #CdnOperator
 * @idx: the index
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CdnExpression) (transfer none): a list of #CdnExpression
 **/
GSList const *
cdn_operator_get_expressions (CdnOperator *op,
                              gint         idx)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);
	g_return_val_if_fail (idx >= 0 && idx < op->priv->num_expressions, NULL);

	return op->priv->expressions[idx];
}

gint
cdn_operator_num_expressions (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), 0);

	return op->priv->num_expressions;
}

/**
 * cdn_operator_get_expressions:
 * @op: A #CdnOperator
 * @idx: the index
 *
 * Get the expressions that the operator uses.
 *
 * Return value: (element-type CdnExpression) (transfer none): a list of #CdnExpression
 **/
GSList const *
cdn_operator_get_indices (CdnOperator *op,
                          gint         idx)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);
	g_return_val_if_fail (idx >= 0 && idx < op->priv->num_indices, NULL);

	return op->priv->indices[idx];
}

gint
cdn_operator_num_indices (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), 0);

	return op->priv->num_indices;
}

gboolean
cdn_operator_initialize (CdnOperator   *op,
                         GSList const **expressions,
                         gint           num_expressions,
                         GSList const **indices,
                         gint           num_indices,
                         gint           num_arguments,
                         GError       **error)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), FALSE);

	return CDN_OPERATOR_GET_CLASS (op)->initialize (op,
	                                                expressions,
	                                                num_expressions,
	                                                indices,
	                                                num_indices,
	                                                num_arguments,
	                                                error);
}

/**
 * cdn_operator_copy:
 * @op: A #CdnOperator
 *
 * Copy an operator.
 *
 * Returns: (transfer full): A #CdnOperator
 *
 **/
CdnOperator *
cdn_operator_copy (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), NULL);

	return CDN_OPERATOR_GET_CLASS (op)->copy (op);
}

gboolean
cdn_operator_equal (CdnOperator *op,
                    CdnOperator *other)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), FALSE);

	if (other == NULL)
	{
		return FALSE;
	}

	return CDN_OPERATOR_GET_CLASS (op)->equal (op, other);
}

gint
cdn_operator_get_num_arguments (CdnOperator *op)
{
	g_return_val_if_fail (CDN_IS_OPERATOR (op), 0);

	return op->priv->num_arguments;
}

CdnFunction *
cdn_operator_get_function (CdnOperator *op,
                           gint        *idx,
                           gint         numidx)
{
	return CDN_OPERATOR_GET_CLASS (op)->get_function (op, idx, numidx);
}

void
_cdn_operator_set_num_arguments (CdnOperator *op,
                                 gint         num)
{
	g_return_if_fail (CDN_IS_OPERATOR (op));

	op->priv->num_arguments = num;
}

void
cdn_operator_foreach_function (CdnOperator            *op,
                               CdnForeachFunctionFunc  func,
                               gpointer                userdata)
{
	if (func == NULL)
	{
		return;
	}

	CDN_OPERATOR_GET_CLASS (op)->foreach_function (op, func, userdata);
}

CdnFunction *
cdn_operator_get_primary_function (CdnOperator *op)
{
	if (op->priv->num_indices == 0)
	{
		gint idx = 0;

		return cdn_operator_get_function (op, &idx, 1);
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
				if (!cdn_expression_get_instructions (idx->data))
				{
					g_array_free (ret, TRUE);
					ret = NULL;
					break;
				}
				else
				{
					gint val = rint (cdn_expression_evaluate (idx->data));

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
			CdnFunction *func;

			gint *ptr = (gint *)g_array_free (ret, FALSE);
			func = cdn_operator_get_function (op, ptr, num);
			g_free (ptr);

			return func;
		}
	}

	return NULL;
}
