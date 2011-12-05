/*
 * cdn-operator-delayed.c
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

#include "cdn-operator-delayed.h"
#include "cdn-operator.h"
#include "cdn-variable.h"
#include "cdn-usable.h"
#include "integrators/cdn-integrator.h"
#include "cdn-network.h"

#include <math.h>
#include <codyn/instructions/cdn-instructions.h>

/**
 * SECTION:cdn-operator-delayed
 * @short_description: Math operator for delayed evaluation of an expression
 *
 * The #CdnOperatorDelayed is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * delayed value of its argument (which can be an arbitrary expression).
 *
 */

#define CDN_OPERATOR_DELAYED_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OPERATOR_DELAYED, CdnOperatorDelayedPrivate))

typedef struct _HistoryItem HistoryItem;

struct _HistoryItem
{
	HistoryItem *next;
	HistoryItem *prev;

	gdouble t;
	gdouble v;
};

typedef struct
{
	HistoryItem *first;
	HistoryItem *last;
	guint size;
} HistoryList;

struct _CdnOperatorDelayedPrivate
{
	HistoryList history;
	HistoryList history_pool;

	CdnExpression *expression;
	CdnExpression *initial_value;
	CdnExpression *delay_expression;

	gdouble delay;
	gdouble value;
};

G_DEFINE_TYPE (CdnOperatorDelayed,
               cdn_operator_delayed,
               CDN_TYPE_OPERATOR)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_INITIAL_VALUE,
	PROP_DELAY
};

/*static void
history_remove_slice (HistoryList *history,
                      HistoryItem *start,
                      HistoryItem *end,
                      guint        num)
{
	if (num == 0)
	{
		return;
	}

	if (start->prev)
	{
		start->prev->next = end->next;
	}

	if (end->next)
	{
		end->next->prev = start->prev;
	}

	if (history->first == start)
	{
		history->first = end->next;
	}

	if (history->last == end)
	{
		history->last = start->prev;
	}

	history->size -= num;

	start->prev = end->next = NULL;
}

static void
history_append_slice (HistoryList *history,
                      HistoryItem *first,
                      HistoryItem *last,
                      guint         num)
{
	if (num == 0)
	{
		return;
	}

	first->prev = history->last;

	if (history->last)
	{
		history->last->next = first;
	}

	history->last = last;

	if (history->first == NULL)
	{
		history->first = first;
	}

	history->size += num;
}

static void
history_concat (HistoryList *l1,
                HistoryList *l2)
{
	history_append_slice (l1, l2->first, l2->last, l2->size);

	l2->first = NULL;
	l2->last = NULL;
	l2->size = 0;
}*/

static gchar *
cdn_operator_delayed_get_name ()
{
	return g_strdup ("delayed");
}

static gboolean
cdn_operator_delayed_initialize (CdnOperator   *op,
                                 GSList const **expressions,
                                 gint           num_expressions,
                                 GSList const **indices,
                                 gint           num_indices,
                                 gint           num_arguments,
                                 GError       **error)
{
	CdnOperatorDelayed *delayed;

	if (!CDN_OPERATOR_CLASS (cdn_operator_delayed_parent_class)->initialize (op,
	                                                                         expressions,
	                                                                         num_expressions,
	                                                                         indices,
	                                                                         num_indices,
	                                                                         num_arguments,
	                                                                         error))
	{
		return FALSE;
	}

	if (num_expressions != 1 || expressions[0]->next)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_OPERATOR,
		             "The operator `delayed' expects one argument, but got %d (%d)",
		             num_expressions, num_expressions ? g_slist_length ((GSList *)expressions[0]) : 0);

		return FALSE;
	}

	delayed = CDN_OPERATOR_DELAYED (op);
	delayed->priv->expression = g_object_ref_sink (expressions[0]->data);

	if (expressions[0]->next)
	{
		delayed->priv->delay_expression = g_object_ref_sink (expressions[0]->next->data);

		if (expressions[0]->next->next)
		{
			delayed->priv->initial_value = g_object_ref_sink (expressions[0]->next->next->data);
		}
	}

	delayed->priv->delay = 0;
	return TRUE;
}

static void
cdn_operator_delayed_execute (CdnOperator     *op,
                              CdnStack        *stack)
{
	CdnOperatorDelayed *d;

	d = (CdnOperatorDelayed *)op;

	// TODO: calculate delay...

	cdn_stack_push (stack, d->priv->value);
}

/*static HistoryItem *
pool_to_history (CdnOperatorDelayed  *operator,
                 gint                 num)
{
	HistoryItem *ret;

	if (num == 0)
	{
		return NULL;
	}

	ret = operator->priv->history_pool.first;

	// Remove from the pool
	if (num >= operator->priv->history_pool.size)
	{
		gint i;
		gint n;

		HistoryItem *slice_start = NULL;
		HistoryItem *slice_end = NULL;

		n = num - operator->priv->history_pool.size;

		history_concat (&operator->priv->history,
		                &operator->priv->history_pool);

		for (i = 0; i < n; ++i)
		{
			HistoryItem *h;

			h = g_slice_new0 (HistoryItem);

			h->t = 0;
			h->v = 0;

			if (slice_start == NULL)
			{
				slice_start = slice_end = h;
			}
			else
			{
				h->prev = slice_end;
				slice_end->next = h;

				slice_end = h;
			}
		}

		if (!ret)
		{
			ret = slice_start;
		}

		history_append_slice (&operator->priv->history,
		                      slice_start,
		                      slice_end,
		                      n);
	}
	else
	{
		// Remove first num from pool
		HistoryItem *ptr = ret;
		gint i;

		for (i = 0; i < num - 1; ++i)
		{
			ptr = ptr->next;
		}

		history_remove_slice (&operator->priv->history_pool,
		                      ret,
		                      ptr,
		                      num);

		history_append_slice (&operator->priv->history,
		                      ret,
		                      ptr,
		                      num);
	}

	if (!ret)
	{
		ret = operator->priv->history.first;
	}

	return ret;
}

static void
init_history (CdnOperatorDelayed *operator,
              CdnIntegrator      *integrator,
              gdouble             t,
              gdouble             timestep)
{
	HistoryItem *item;
	gint i;

	if (operator->priv->delay_expression)
	{
		cdn_expression_reset_cache (operator->priv->delay_expression);
		operator->priv->delay = cdn_expression_evaluate (operator->priv->delay_expression);
	}

	// History is empty
	item = pool_to_history (operator, (guint)(ceil (operator->priv->delay / fabs (timestep))));

	// Initialize values
	for (i = 0; i < operator->priv->history.size; ++i)
	{
		gint wri;

		wri = operator->priv->history.size - i;

		item->t = t - timestep * wri;

		cdn_integrator_set_time (integrator, item->t);

		if (operator->priv->initial_value)
		{
			cdn_expression_reset_cache (operator->priv->initial_value);
			item->v = cdn_expression_evaluate (operator->priv->initial_value);
		}

		item = item->next;
	}

	cdn_integrator_set_time (integrator, t);
}

static void
move_to_pool (CdnOperatorDelayed *operator,
              gdouble             fromt)
{
	HistoryItem *item;
	guint num = 0;
	HistoryItem *first;

	item = operator->priv->history.first;

	if (!item || item->t >= fromt)
	{
		return;
	}

	while (item->next && item->next->t < fromt)
	{
		item = item->next;
		++num;
	}

	item = item->prev;

	if (num == 0 || !item)
	{
		return;
	}

	first = operator->priv->history.first;

	history_remove_slice (&operator->priv->history,
	                      first,
	                      item,
	                      num);

	history_append_slice (&operator->priv->history_pool,
	                      first,
	                      item,
	                      num);
}

static gdouble
evaluate_at (CdnOperatorDelayed *operator,
             gdouble             t)
{
	gdouble td;
	HistoryItem *h;

	if (operator->priv->delay == 0)
	{
		return cdn_expression_evaluate (operator->priv->expression);
	}

	// Calculate here where to read the delayed value from (at t)
	td = t - operator->priv->delay;

	h = operator->priv->history.first;

	if (!h || h->t > td)
	{
		g_warning ("Needed history from the past, which I don't have...");
		return 0;
	}

	while (h->next && h->next->t < td)
	{
		h = h->next;
	}

	if (!h->next)
	{
		if (fabs (h->t - td) > 0.00000001)
		{
			g_warning ("Needed history in the future, which I don't have...");
		}

		return h->v;
	}
	else
	{
		// Interpolate value between h and h->next
		gdouble factor;

		if (td == h->t)
		{
			factor = 0;
		}
		else
		{
			factor = (td - h->t) / (h->next->t - h->t);
		}

		return h->v + factor * (h->next->v - h->v);
	}
}*/

static void
history_free (HistoryList *history)
{
	HistoryItem *item;

	item = history->first;

	while (item)
	{
		HistoryItem *next;
		next = item->next;

		g_slice_free (HistoryItem, item);
		item = next;
	}

	history->first = history->last = NULL;
	history->size = 0;
}

static void
cdn_operator_delayed_finalize (GObject *object)
{
	CdnOperatorDelayed *delayed;

	delayed = CDN_OPERATOR_DELAYED (object);

	history_free (&delayed->priv->history);
	history_free (&delayed->priv->history_pool);

	g_object_unref (delayed->priv->expression);

	if (delayed->priv->delay_expression)
	{
		g_object_unref (delayed->priv->delay_expression);
	}

	if (delayed->priv->initial_value)
	{
		g_object_unref (delayed->priv->initial_value);
	}

	G_OBJECT_CLASS (cdn_operator_delayed_parent_class)->finalize (object);
}

static void
cdn_operator_delayed_set_property (GObject      *object,
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
cdn_operator_delayed_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CdnOperatorDelayed *self = CDN_OPERATOR_DELAYED (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
			break;
		case PROP_INITIAL_VALUE:
			g_value_set_object (value, self->priv->initial_value);
			break;
		case PROP_DELAY:
			g_value_set_double (value, self->priv->delay);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cdn_operator_delayed_equal (CdnOperator *op,
                            CdnOperator *other)
{
	CdnOperatorDelayed *delayed;
	CdnOperatorDelayed *odel;

	if (!CDN_IS_OPERATOR_DELAYED (other))
	{
		return FALSE;
	}

	delayed = CDN_OPERATOR_DELAYED (op);
	odel = CDN_OPERATOR_DELAYED (other);

	if (delayed->priv->delay != odel->priv->delay)
	{
		return FALSE;
	}

	if ((delayed->priv->initial_value != NULL) !=
	    (odel->priv->initial_value != NULL))
	{
		return FALSE;
	}

	if (!cdn_expression_equal (delayed->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	if (delayed->priv->initial_value &&
	    !cdn_expression_equal (delayed->priv->expression,
	                           odel->priv->expression))
	{
		return FALSE;
	}

	return TRUE;
}

static void
cdn_operator_delayed_class_init (CdnOperatorDelayedClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnOperatorClass *op_class = CDN_OPERATOR_CLASS (klass);

	object_class->finalize = cdn_operator_delayed_finalize;

	object_class->get_property = cdn_operator_delayed_get_property;
	object_class->set_property = cdn_operator_delayed_set_property;

	op_class->get_name = cdn_operator_delayed_get_name;
	op_class->execute = cdn_operator_delayed_execute;
	op_class->initialize = cdn_operator_delayed_initialize;
	op_class->equal = cdn_operator_delayed_equal;

	g_type_class_add_private (object_class, sizeof(CdnOperatorDelayedPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_INITIAL_VALUE,
	                                 g_param_spec_object ("initial-value",
	                                                      "Initial Value",
	                                                      "Initial value",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_DELAY,
	                                 g_param_spec_double ("delay",
	                                                      "Delay",
	                                                      "Delay",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READABLE));
}

static void
cdn_operator_delayed_init (CdnOperatorDelayed *self)
{
	self->priv = CDN_OPERATOR_DELAYED_GET_PRIVATE (self);
}

CdnOperatorDelayed *
cdn_operator_delayed_new ()
{
	return g_object_new (CDN_TYPE_OPERATOR_DELAYED, NULL);
}

/**
 * cdn_operator_delayed_get_expression:
 * @delayed: A #CdnOperatorDelayed
 *
 * Get the expression to be delayed.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_delayed_get_expression (CdnOperatorDelayed *delayed)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DELAYED (delayed), NULL);

	return delayed->priv->expression;
}

/**
 * cdn_operator_delayed_get_initial_value:
 * @delayed: A #CdnOperatorDelayed
 *
 * Get the initial value of the expression to be delayed.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_operator_delayed_get_initial_value (CdnOperatorDelayed *delayed)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DELAYED (delayed), NULL);

	return delayed->priv->initial_value;
}

/**
 * cdn_operator_delayed_get_delay:
 * @delayed: A #CdnOperatorDelayed
 *
 * Get the time delay in seconds.
 *
 * Returns: The time delay in seconds
 *
 **/
gdouble
cdn_operator_delayed_get_delay (CdnOperatorDelayed *delayed)
{
	g_return_val_if_fail (CDN_IS_OPERATOR_DELAYED (delayed), 0.0);

	return delayed->priv->delay;
}
