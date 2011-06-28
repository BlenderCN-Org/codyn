/*
 * cpg-operator-delayed.c
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

#include "cpg-operator-delayed.h"
#include "cpg-operator.h"
#include "cpg-property.h"
#include "cpg-usable.h"
#include "cpg-integrator.h"

#include <math.h>
#include <cpg-network/instructions/cpg-instructions.h>

#define CPG_OPERATOR_DELAYED_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayedPrivate))

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

struct _CpgOperatorDelayedPrivate
{
	HistoryList history;
	HistoryList history_pool;

	CpgExpression *expression;
	CpgExpression *initial_value;

	gdouble delay;
	gdouble value;
};

G_DEFINE_TYPE (CpgOperatorDelayed,
               cpg_operator_delayed,
               CPG_TYPE_OPERATOR)

static void
history_remove_slice (HistoryList *history,
                      HistoryItem *start,
                      HistoryItem *end,
                      guint        num)
{
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
}

static gchar *
cpg_operator_delayed_get_name ()
{
	return g_strdup ("delayed");
}

static void
cpg_operator_delayed_reset (CpgOperator *op)
{
	CpgOperatorDelayed *delayed;

	CPG_OPERATOR_CLASS (cpg_operator_delayed_parent_class)->reset (op);

	delayed = CPG_OPERATOR_DELAYED (op);

	/* TODO */
	/* move all history items to the pool */
}

static void
cpg_operator_delayed_initialize (CpgOperator  *op,
                                 GSList const *expressions)
{
	CpgOperatorDelayed *delayed;

	CPG_OPERATOR_CLASS (cpg_operator_delayed_parent_class)->initialize (op, expressions);

	delayed = CPG_OPERATOR_DELAYED (op);
	delayed->priv->expression = g_object_ref (expressions->data);

	if (expressions->next)
	{
		delayed->priv->delay = cpg_expression_evaluate (expressions->next->data);

		if (expressions->next->next)
		{
			delayed->priv->initial_value = expressions->next->next->data;
		}
	}
}

static void
cpg_operator_delayed_execute (CpgOperator     *op,
                              CpgStack        *stack)
{
	CpgOperatorDelayed *d;

	d = (CpgOperatorDelayed *)op;

	cpg_stack_push (stack, d->priv->value);
}

static gint
cpg_operator_delayed_validate_num_arguments (gint num)
{
	return num > 0 && num < 4;
}

static HistoryItem *
pool_to_history (CpgOperatorDelayed  *operator,
                 gint                 num)
{
	HistoryItem *ret;

	if (num == 0)
	{
		return NULL;
	}

	ret = operator->priv->history_pool.first;

	/* Remove from the pool */
	if (num >= operator->priv->history_pool.size)
	{
		gint i;
		gint n;

		HistoryItem *slice_start = NULL;
		HistoryItem *slice_end = NULL;

		history_concat (&operator->priv->history,
		                &operator->priv->history_pool);

		n = num - operator->priv->history_pool.size;

		for (i = 0; i < n; ++i)
		{
			HistoryItem *h;

			h = g_slice_new (HistoryItem);

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
		/* Remove first num from pool */
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
init_history (CpgOperatorDelayed *operator,
              CpgIntegrator      *integrator,
              gdouble             t,
              gdouble             timestep)
{
	HistoryItem *item;
	gint i;

	/* History is empty */
	item = pool_to_history (operator, (guint)(ceil (operator->priv->delay / fabs (timestep))));

	/* Initialize values */
	for (i = 0; i < operator->priv->history.size; ++i)
	{
		gint wri;

		wri = operator->priv->history.size - i;

		item->t = t - timestep * wri;

		cpg_integrator_set_time (integrator, item->t);

		if (operator->priv->initial_value)
		{
			cpg_expression_reset_cache (operator->priv->initial_value);
			item->v = cpg_expression_evaluate (operator->priv->initial_value);
		}

		item = item->next;
	}

	cpg_integrator_set_time (integrator, t);
}

static void
move_to_pool (CpgOperatorDelayed *operator,
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
evaluate_at (CpgOperatorDelayed *operator,
             gdouble             t)
{
	gdouble td;
	HistoryItem *h;

	if (operator->priv->delay == 0)
	{
		return cpg_expression_evaluate (operator->priv->expression);
	}

	/* Calculate here where to read the delayed value from (at t) */
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
		if (h->t != td)
		{
			g_warning ("Needed history in the future, which I don't have...");
		}

		return h->v;
	}
	else
	{
		/* Interpolate value between h and h->next */
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

}

static void
cpg_operator_delayed_step_evaluate (CpgOperator     *operator,
                                    CpgIntegrator   *integrator,
                                    gdouble          t,
                                    gdouble          timestep)
{
	CpgOperatorDelayed *d;

	d = (CpgOperatorDelayed *)operator;

	d->priv->value = evaluate_at (d, t);
}

static void
cpg_operator_delayed_step_prepare (CpgOperator     *operator,
                                   CpgIntegrator   *integrator,
                                   gdouble          t,
                                   gdouble          timestep)
{
	CpgOperatorDelayed *d;
	HistoryItem *item;

	d = (CpgOperatorDelayed *)operator;

	if (d->priv->delay == 0)
	{
		d->priv->value = cpg_expression_evaluate (d->priv->expression);
		return;
	}

	/* Initialize history here */
	if (d->priv->history.first == NULL)
	{
		/* Initialize the history first */
		init_history (d, integrator, t, timestep);
	}

	/* Move to pool everything from before td */
	move_to_pool (d, t - d->priv->delay);

	item = pool_to_history (d, 1);

	item->t = t;
	item->v = cpg_expression_evaluate (d->priv->expression);
}

static void
cpg_operator_delayed_step (CpgOperator     *operator,
                           CpgIntegrator   *integrator,
                           gdouble          t,
                           gdouble          timestep)
{
	CpgOperatorDelayed *d;

	d = (CpgOperatorDelayed *)operator;

	d->priv->value = evaluate_at (d, t);
}

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
cpg_operator_delayed_finalize (GObject *object)
{
	CpgOperatorDelayed *delayed;

	delayed = CPG_OPERATOR_DELAYED (object);

	history_free (&delayed->priv->history);
	history_free (&delayed->priv->history_pool);

	g_object_unref (delayed->priv->expression);

	G_OBJECT_CLASS (cpg_operator_delayed_parent_class)->finalize (object);
}

static void
cpg_operator_delayed_class_init (CpgOperatorDelayedClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgOperatorClass *op_class = CPG_OPERATOR_CLASS (klass);

	object_class->finalize = cpg_operator_delayed_finalize;

	op_class->get_name = cpg_operator_delayed_get_name;
	op_class->execute = cpg_operator_delayed_execute;
	op_class->initialize = cpg_operator_delayed_initialize;
	op_class->reset = cpg_operator_delayed_reset;
	op_class->step_prepare = cpg_operator_delayed_step_prepare;
	op_class->step_evaluate = cpg_operator_delayed_step_evaluate;
	op_class->step = cpg_operator_delayed_step;
	op_class->validate_num_arguments = cpg_operator_delayed_validate_num_arguments;

	g_type_class_add_private (object_class, sizeof(CpgOperatorDelayedPrivate));
}

static void
cpg_operator_delayed_init (CpgOperatorDelayed *self)
{
	self->priv = CPG_OPERATOR_DELAYED_GET_PRIVATE (self);
}

CpgOperatorDelayed *
cpg_operator_delayed_new ()
{
	return g_object_new (CPG_TYPE_OPERATOR_DELAYED, NULL);
}