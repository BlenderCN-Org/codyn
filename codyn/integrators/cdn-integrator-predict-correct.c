/*
 * cdn-integrator-predict-correct.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <codyn/cdn-network.h>
#include <codyn/cdn-integrators.h>

#include "cdn-integrator-predict-correct.h"

#define CDN_INTEGRATOR_PREDICT_CORRECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, CdnIntegratorPredictCorrectPrivate))

#define MIN_PREDICTION_ORDER 2
#define MAX_PREDICTION_ORDER 5
#define DEFAULT_PREDICTION_ORDER 3

#define MIN_CORRECTION_ORDER 3
#define MAX_CORRECTION_ORDER 5
#define DEFAULT_CORRECTION_ORDER 5

/**
 * SECTION:cdn-integrator-predict-correct
 * @short_description: Prediction Correction integrator
 *
 * The prediction correction integrator is a #CdnIntegrator subclass
 * implementing a prediction correction integration scheme. It supports multiple
 * orders of prediction and correction.
 *
 */

/* necessary depth of history:
 * prediction method order 'n' uses f(t), f(t-1), ... f(t-n+2)
 * correction method order 'n' uses f(t+1), f(t), f(t-1), ..., f(t-n+3)
 * (note that f(t+1) is not stored in the history) */
#define MAX_HISTORY_DEPTH    (MAX ((MAX_PREDICTION_ORDER) - 1, (MAX_CORRECTION_ORDER) - 2))

static const gdouble prediction_coeffs[][MAX_PREDICTION_ORDER - 1] = {
	{1, 0, 0, 0},
	{3 / 2.0, -1 / 2.0, 0, 0},
	{23 / 12.0, -16 / 12.0, 5 / 12.0, 0},
	{55 / 24.0, -59 / 24.0, 37 / 24.0, -9 / 24.0}
};

static const gdouble correction_coeffs[][MAX_CORRECTION_ORDER - 1] = {
	{0.5, 0.5, 0, 0},
	{5 / 12.0, 8 / 12.0, -1 / 12.0, 0},
	{9 / 24.0, 19 / 24.0, -5 / 24.0, 1 / 24.0}
};

static const guint index_cycle[] = {0, 1, 2, 3, 0, 1, 2};

struct _CdnIntegratorPredictCorrectPrivate
{
	gdouble *current_value;
	gdouble *state_history[MAX_HISTORY_DEPTH];
	gint history_cursor;
	guint step_index;

	guint prediction_order;
	guint correction_order;

	gdouble *tmpd;
};

/* Properties */
enum
{
	PROP_0,
	PROP_PREDICTION_ORDER,
	PROP_CORRECTION_ORDER
};

G_DEFINE_TYPE (CdnIntegratorPredictCorrect, cdn_integrator_predict_correct, CDN_TYPE_INTEGRATOR)

static void
history_move_cursor (CdnIntegratorPredictCorrect *pc)
{
	--pc->priv->history_cursor;

	if (pc->priv->history_cursor < 0)
	{
		pc->priv->history_cursor = MAX_HISTORY_DEPTH - 1;
	}
}

static guint
history_index (CdnIntegratorPredictCorrect *pc,
               guint                        time_offset)
{
	return index_cycle[pc->priv->history_cursor + time_offset];
}

static void
history_set (CdnIntegratorPredictCorrect *pc,
             guint                        state_index,
             guint                        time_offset,
             gdouble                      value)
{
	pc->priv->state_history[history_index (pc, time_offset)][state_index] = value;
}

static gdouble
history_get (CdnIntegratorPredictCorrect *pc,
             guint                        state_index,
             guint                        time_offset)
{
	return pc->priv->state_history[history_index (pc, time_offset)][state_index];
}

static void
history_update_states (CdnIntegratorPredictCorrect *pc,
                       GSList const                *states,
                       guint                       *i)
{
	while (states)
	{
		gint n;
		gint j;
		CdnMatrix *up;
		gdouble *upvals;

		up = cdn_variable_get_update (states->data);
		upvals = cdn_matrix_get_memory (up);

		n = cdn_matrix_size (up);

		for (j = 0; j < n; ++j)
		{
			history_set (pc, (*i)++, 0, upvals[j]);
		}

		states = g_slist_next (states);
	}

}

static void
history_update (CdnIntegratorPredictCorrect *pc,
                GSList const                *integrated)
{
	guint i = 0;

	history_update_states (pc, integrated, &i);
}

static void
read_current_values (CdnIntegratorPredictCorrect *pc,
                     GSList const                *integrated)
{
	guint i = 0;

	while (integrated)
	{
		CdnVariable *prop = integrated->data;

		pc->priv->current_value[i] = cdn_variable_get_value (prop);

		integrated = g_slist_next (integrated);
		++i;
	}
}

static void
prediction_step (CdnIntegratorPredictCorrect *pc,
                 GSList const                *integrated,
                 gdouble                      timestep)
{
	/* for the first timesteps we have to use lower orders, e.g. at
	 * timestep 0 we use prediction method order 2, which only needs
	 * f(t) */
	guint order = MIN (pc->priv->prediction_order, pc->priv->step_index + 2);
	guint coeff_row = order - 2;
	guint n_coeffs = order - 1;

	guint state_index = 0;

	while (integrated)
	{
		CdnVariable *prop = integrated->data;
		guint coeff_index;
		CdnMatrix *up;
		gint i;
		gint n;
		CdnMatrix tmp;

		up = cdn_variable_get_update (prop);
		n = cdn_matrix_size (up);

		/* derivative prediction for t+1 */
		for (i = 0; i < n; ++i)
		{
			pc->priv->tmpd[i] = 0;

			for (coeff_index = 0; coeff_index < n_coeffs; ++coeff_index)
			{
				pc->priv->tmpd[i] +=
					prediction_coeffs[coeff_row][coeff_index] *
					history_get (pc, state_index + i, coeff_index);
			}

			pc->priv->tmpd[i] =
				pc->priv->current_value[state_index + i] +
				timestep * pc->priv->tmpd[i];
		}

		tmp.values = pc->priv->tmpd;
		tmp.dimension = up->dimension;

		cdn_variable_set_values (prop, &tmp);

		integrated = g_slist_next (integrated);
		state_index += n;
	}
}

static void
correction_step (CdnIntegratorPredictCorrect *pc,
                 GSList const                *integrated,
                 gdouble                      timestep)
{
	/* for the first timesteps we have to use lower orders, e.g. at
	 * timestep 0 we use correction method order 3, which only needs
	 * f(t) (and f(t+1)) */
	guint order = MIN (pc->priv->correction_order, pc->priv->step_index + 3);
	guint coeff_row = order - 3;
	guint n_coeffs = order - 1;

	guint state_index = 0;

	while (integrated)
	{
		CdnVariable *prop = integrated->data;
		guint coeff_index;
		CdnMatrix *up;
		gdouble const *upvals;
		gint i;
		gint n;
		CdnMatrix tmp;

		up = cdn_variable_get_update (prop);
		upvals = cdn_matrix_get (up);
		n = cdn_matrix_size (up);

		/* derivative prediction for t+1 */
		for (i = 0; i < n; ++i)
		{
			pc->priv->tmpd[i] = correction_coeffs[coeff_row][0] * upvals[i];

			for (coeff_index = 1; coeff_index < n_coeffs; ++coeff_index)
			{
				pc->priv->tmpd[i] += correction_coeffs[coeff_row][coeff_index] * history_get (pc, state_index + i, coeff_index - 1);
			}

			pc->priv->tmpd[i] = pc->priv->current_value[state_index + i] +
			                    timestep * pc->priv->tmpd[i];
		}

		tmp.dimension = up->dimension;
		tmp.values = pc->priv->tmpd;

		cdn_variable_set_values (prop, &tmp);

		integrated = g_slist_next (integrated);
		state_index += n;
	}
}

static void
cdn_integrator_predict_correct_finalize (GObject *object)
{
	CdnIntegratorPredictCorrect *pc = CDN_INTEGRATOR_PREDICT_CORRECT (object);
	guint i = 0;

	for (i = 0; i < MAX_HISTORY_DEPTH; ++i)
	{
		if (pc->priv->state_history[i])
		{
			g_free (pc->priv->state_history[i]);
		}
	}

	g_free (pc->priv->tmpd);
	g_free (pc->priv->current_value);

	G_OBJECT_CLASS (cdn_integrator_predict_correct_parent_class)->finalize (object);
}

static gint
calculate_props_length (GSList const *props,
                        gint         *longest)
{
	gint len = 0;

	while (props)
	{
		CdnExpression *expr;

		CdnDimension dim;
		gint n;

		expr = cdn_variable_get_expression (props->data);
		cdn_expression_get_dimension (expr, &dim);

		n = cdn_dimension_size (&dim);

		len += n;

		if (n > *longest)
		{
			*longest = n;
		}

		props = g_slist_next (props);
	}

	return len;
}

static gint
calculate_length (CdnIntegratorPredictCorrect *pc,
                  gint                        *longest)
{
	CdnIntegratorState *state;

	state = cdn_integrator_get_state (CDN_INTEGRATOR (pc));

	*longest = 0;

	return calculate_props_length (cdn_integrator_state_integrated_variables (state),
	                               longest);
}

static void
cdn_integrator_predict_correct_reset_impl (CdnIntegrator *integrator)
{
	CdnIntegratorPredictCorrect *pc = CDN_INTEGRATOR_PREDICT_CORRECT (integrator);

	if (CDN_INTEGRATOR_CLASS (cdn_integrator_predict_correct_parent_class)->reset)
	{
		CDN_INTEGRATOR_CLASS (cdn_integrator_predict_correct_parent_class)->reset (integrator);
	}

	gint longest;
	gint len = calculate_length (pc, &longest);
	gint i;

	for (i = 0; i < MAX_HISTORY_DEPTH; ++i)
	{
		g_free (pc->priv->state_history[i]);
		pc->priv->state_history[i] = g_new0 (gdouble, len);
	}

	g_free (pc->priv->current_value);
	pc->priv->current_value = g_new0 (gdouble, len);

	g_free (pc->priv->tmpd);
	pc->priv->tmpd = g_new0 (gdouble, longest);

	pc->priv->history_cursor = 0;
	pc->priv->step_index = 0;
}

static gdouble
cdn_integrator_predict_correct_step_impl (CdnIntegrator *integrator,
                                          gdouble        t,
                                          gdouble        timestep)
{
	CdnIntegratorPredictCorrect *pc;
	CdnIntegratorState *state;
	CdnIntegratorClass *cls;

	pc = CDN_INTEGRATOR_PREDICT_CORRECT (integrator);
	state = cdn_integrator_get_state (integrator);

	if (!cdn_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	GSList const *integrated = cdn_integrator_state_integrated_variables (state);

	/* evaluate f(t) */
	cdn_integrator_evaluate (integrator, t, timestep);

	/* backup of current states */
	read_current_values (pc, integrated);

	/* store f(t) */
	history_update (pc, integrated);

	/* calculate prediction for t+1 */
	prediction_step (pc, integrated, timestep);

	/* evaluate f(t+1) */
	cdn_integrator_evaluate (integrator, t + timestep, timestep);

	/* correct the prediction */
	correction_step (pc, integrated, timestep);

	history_move_cursor (pc);

	++pc->priv->step_index;

	cls = CDN_INTEGRATOR_CLASS (cdn_integrator_predict_correct_parent_class);

	/* Chain up to emit 'step' */
	return cls->step (integrator, t, timestep);
}

static gchar const *
cdn_integrator_predict_correct_get_name_impl (CdnIntegrator *integrator)
{
	return "Prediction Correction";
}

static void
cdn_integrator_predict_correct_set_property (GObject      *object,
                                             guint         prop_id,
                                             const GValue *value,
                                             GParamSpec   *pspec)
{
	CdnIntegratorPredictCorrect *self = CDN_INTEGRATOR_PREDICT_CORRECT (object);

	switch (prop_id)
	{
		case PROP_CORRECTION_ORDER:
			self->priv->correction_order = g_value_get_uint (value);
		break;
		case PROP_PREDICTION_ORDER:
			self->priv->prediction_order = g_value_get_uint (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_predict_correct_get_property (GObject    *object,
                                             guint       prop_id,
                                             GValue     *value,
                                             GParamSpec *pspec)
{
	CdnIntegratorPredictCorrect *self = CDN_INTEGRATOR_PREDICT_CORRECT (object);

	switch (prop_id)
	{
		case PROP_CORRECTION_ORDER:
			g_value_set_uint (value, self->priv->correction_order);
		break;
		case PROP_PREDICTION_ORDER:
			g_value_set_uint (value, self->priv->prediction_order);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_predict_correct_class_init (CdnIntegratorPredictCorrectClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_predict_correct_finalize;
	object_class->set_property = cdn_integrator_predict_correct_set_property;
	object_class->get_property = cdn_integrator_predict_correct_get_property;

	integrator_class->step = cdn_integrator_predict_correct_step_impl;
	integrator_class->get_name = cdn_integrator_predict_correct_get_name_impl;
	integrator_class->reset = cdn_integrator_predict_correct_reset_impl;

	integrator_class->integrator_id = "predict-correct";

	g_object_class_install_property (object_class,
	                                 PROP_PREDICTION_ORDER,
	                                 g_param_spec_uint ("prediction-order",
	                                                    "Prediction Order",
	                                                    "Prediction order",
	                                                    MIN_PREDICTION_ORDER,
	                                                    MAX_PREDICTION_ORDER,
	                                                    DEFAULT_PREDICTION_ORDER,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_CORRECTION_ORDER,
	                                 g_param_spec_uint ("correction-order",
	                                                    "Correction Order",
	                                                    "Correction order",
	                                                    MIN_CORRECTION_ORDER,
	                                                    MAX_CORRECTION_ORDER,
	                                                    DEFAULT_CORRECTION_ORDER,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_type_class_add_private (object_class, sizeof(CdnIntegratorPredictCorrectPrivate));
}

static void
cdn_integrator_predict_correct_init (CdnIntegratorPredictCorrect *self)
{
	self->priv = CDN_INTEGRATOR_PREDICT_CORRECT_GET_PRIVATE (self);
	self->priv->prediction_order = 5;
	self->priv->correction_order = 5;
}

/**
 * cdn_integrator_predict_correct_new:
 *
 * Create a new PredictCorrect integrator.
 *
 * Returns: A #CdnIntegratorPredictCorrect
 *
 **/
CdnIntegratorPredictCorrect *
cdn_integrator_predict_correct_new (void)
{
	return g_object_new (CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, NULL);
}

/**
 * cdn_integrator_predict_correct_get_prediction_order:
 * @pc: A #CdnIntegratorPredictCorrect
 *
 * Get the prediction order.
 *
 * Returns: The prediction order
 *
 **/
guint
cdn_integrator_predict_correct_get_prediction_order (CdnIntegratorPredictCorrect *pc)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_PREDICT_CORRECT (pc), 0);

	return pc->priv->prediction_order;
}

/**
 * cdn_integrator_predict_correct_get_correction_order:
 * @pc: A #CdnIntegratorPredictCorrect
 *
 * Get the correction order.
 *
 * Returns: The correction order
 *
 **/
guint
cdn_integrator_predict_correct_get_correction_order (CdnIntegratorPredictCorrect *pc)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_PREDICT_CORRECT (pc), 0);

	return pc->priv->correction_order;
}

/**
 * cdn_integrator_predict_correct_set_prediction_order:
 * @pc: A #CdnIntegratorPredictCorrect
 * @order: The prediction order
 *
 * Set the prediction order.
 *
 **/
void
cdn_integrator_predict_correct_set_prediction_order (CdnIntegratorPredictCorrect *pc,
                                                     guint                        order)
{
	g_return_if_fail (CDN_IS_INTEGRATOR_PREDICT_CORRECT (pc));
	g_return_if_fail (order >= MIN_PREDICTION_ORDER && order <= MAX_PREDICTION_ORDER);

	g_object_set (pc, "prediction-order", order, NULL);
}

/**
 * cdn_integrator_predict_correct_set_correction_order:
 * @pc: A #CdnIntegratorPredictCorrect
 * @order: The correction order
 *
 * Set the correction order.
 *
 **/
void
cdn_integrator_predict_correct_set_correction_order (CdnIntegratorPredictCorrect *pc,
                                                     guint                        order)
{
	g_return_if_fail (CDN_IS_INTEGRATOR_PREDICT_CORRECT (pc));
	g_return_if_fail (order >= MIN_CORRECTION_ORDER && order <= MAX_CORRECTION_ORDER);

	g_object_set (pc, "correction-order", order, NULL);
}
