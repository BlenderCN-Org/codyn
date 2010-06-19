#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-integrators.h>

#include "cpg-integrator-predict-correct.h"

#define CPG_INTEGRATOR_PREDICT_CORRECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, CpgIntegratorPredictCorrectPrivate))

#define MIN_PREDICTION_ORDER 2
#define MAX_PREDICTION_ORDER 5
#define DEFAULT_PREDICTION_ORDER 3

#define MIN_CORRECTION_ORDER 3
#define MAX_CORRECTION_ORDER 5
#define DEFAULT_CORRECTION_ORDER 5

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

struct _CpgIntegratorPredictCorrectPrivate
{
	gdouble *current_value;
	gdouble *state_history[MAX_HISTORY_DEPTH];
	gint history_cursor;
	guint step_index;

	guint prediction_order;
	guint correction_order;
};

/* Properties */
enum
{
	PROP_0,
	PROP_PREDICTION_ORDER,
	PROP_CORRECTION_ORDER
};

G_DEFINE_TYPE (CpgIntegratorPredictCorrect, cpg_integrator_predict_correct, CPG_TYPE_INTEGRATOR)

static void
history_move_cursor (CpgIntegratorPredictCorrect *pc)
{
	--pc->priv->history_cursor;
	if (pc->priv->history_cursor < 0)
	{
		pc->priv->history_cursor = MAX_HISTORY_DEPTH - 1;
	}
}

static guint
history_index (CpgIntegratorPredictCorrect *pc,
               guint                        time_offset)
{
	return index_cycle[pc->priv->history_cursor + time_offset];
}

static void
history_set (CpgIntegratorPredictCorrect *pc,
             guint                        state_index,
             guint                        time_offset,
             gdouble                      value)
{
	pc->priv->state_history[history_index (pc, time_offset)][state_index] = value;
}

static gdouble
history_get (CpgIntegratorPredictCorrect *pc,
             guint                        state_index,
             guint                        time_offset)
{
	return pc->priv->state_history[history_index (pc, time_offset)][state_index];
}

static void
history_update (CpgIntegratorPredictCorrect *pc,
                GSList const                *state)
{
	guint i = 0;

	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;

		history_set (pc, i, 0, cpg_integrator_state_get_update (st));

		state = g_slist_next (state);
		++i;
	}
}

static void
read_current_values (CpgIntegratorPredictCorrect *pc,
                     GSList const                *state)
{
	guint i = 0;

	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;
		CpgProperty *prop = cpg_integrator_state_get_property (st);

		pc->priv->current_value[i] = cpg_property_get_value (prop);

		state = g_slist_next (state);
		++i;
	}
}

static void
prediction_step (CpgIntegratorPredictCorrect *pc,
                 GSList const                *state,
                 gdouble                      timestep)
{
	/* for the first timesteps we have to use lower orders, e.g. at
	 * timestep 0 we use prediction method order 2, which only needs
	 * f(t) */
	guint order = MIN (pc->priv->prediction_order, pc->priv->step_index + 2);
	guint coeff_row = order - 2;
	guint n_coeffs = order - 1;

	guint state_index = 0;

	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;
		CpgProperty *prop = cpg_integrator_state_get_property (st);

		guint coeff_index;
		gdouble dprediction = 0;

		for (coeff_index = 0; coeff_index < n_coeffs; ++coeff_index)
		{
			dprediction += prediction_coeffs[coeff_row][coeff_index] * history_get (pc, state_index, coeff_index);
		}

		cpg_property_set_value (prop, pc->priv->current_value[state_index] + timestep * dprediction);

		state = g_slist_next (state);
		++state_index;
	}
}

static void
correction_step (CpgIntegratorPredictCorrect *pc,
                 GSList const                *state,
                 gdouble                      timestep)
{
	/* for the first timesteps we have to use lower orders, e.g. at
	 * timestep 0 we use correction method order 3, which only needs
	 * f(t) (and f(t+1)) */
	guint order = MIN (pc->priv->correction_order, pc->priv->step_index + 3);
	guint coeff_row = order - 3;
	guint n_coeffs = order - 1;

	guint state_index = 0;

	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;
		CpgProperty *prop = cpg_integrator_state_get_property (st);

		if (cpg_property_get_integrated (prop))
		{
			guint coeff_index;

			/* derivative prediction for t+1 */
			gdouble dcorrected = correction_coeffs[coeff_row][0] * cpg_integrator_state_get_update (st);

			for (coeff_index = 1; coeff_index < n_coeffs; ++coeff_index)
			{
				dcorrected += correction_coeffs[coeff_row][coeff_index] * history_get (pc, state_index, coeff_index - 1);
			}

			cpg_property_set_value (prop, pc->priv->current_value[state_index] + timestep * dcorrected);
		}
		else
		{
			cpg_property_set_value (prop, history_get (pc, state_index, 0));
		}

		state = g_slist_next (state);
		++state_index;
	}
}

static void
cpg_integrator_predict_correct_finalize (GObject *object)
{
	CpgIntegratorPredictCorrect *pc = CPG_INTEGRATOR_PREDICT_CORRECT (object);
	guint i = 0;

	for (i = 0; i < MAX_HISTORY_DEPTH; ++i)
	{
		if (pc->priv->state_history[i])
		{
			g_free (pc->priv->state_history[i]);
		}
	}

	if (pc->priv->current_value)
	{
		g_free (pc->priv->current_value);
	}

	G_OBJECT_CLASS (cpg_integrator_predict_correct_parent_class)->finalize (object);
}

static void
cpg_integrator_predict_correct_reset_impl (CpgIntegrator *integrator,
                                           GSList const  *state)
{
	CpgIntegratorPredictCorrect *pc = CPG_INTEGRATOR_PREDICT_CORRECT (integrator);
	guint len = g_slist_length ((GSList *)state);
	guint i;

	for (i = 0; i < MAX_HISTORY_DEPTH; ++i)
	{
		if (pc->priv->state_history[i])
		{
			g_free (pc->priv->state_history[i]);
		}

		pc->priv->state_history[i] = g_new0 (gdouble, len);
	}

	if (pc->priv->current_value)
	{
		g_free (pc->priv->current_value);
	}
	pc->priv->current_value = g_new0 (gdouble, len);

	pc->priv->history_cursor = 0;
	pc->priv->step_index = 0;

	CPG_INTEGRATOR_CLASS (cpg_integrator_predict_correct_parent_class)->reset (integrator, state);
}

static gdouble
cpg_integrator_predict_correct_step_impl (CpgIntegrator *integrator,
                                          gdouble        t,
                                          gdouble        timestep)
{
	CpgIntegratorPredictCorrect *pc = CPG_INTEGRATOR_PREDICT_CORRECT (integrator);

	GSList const *state = cpg_integrator_get_state (integrator);

	/* evaluate f(t) */
	cpg_integrator_evaluate (integrator, t, timestep);

	/* backup of current states */
	read_current_values (pc, state);

	/* store f(t) */
	history_update (pc, state);

	/* calculate prediction for t+1 */
	prediction_step (pc, state, timestep);

	/* evaluate f(t+1) */
	cpg_integrator_evaluate (integrator, t + timestep, timestep);

	/* correct the prediction */
	correction_step (pc, state, timestep);

	history_move_cursor (pc);

	++pc->priv->step_index;

	/* Chain up to emit 'step' */
	CPG_INTEGRATOR_CLASS (cpg_integrator_predict_correct_parent_class)->step (integrator,
	                                                                          t,
	                                                                          timestep);

	return timestep;
}

static gchar const *
cpg_integrator_predict_correct_get_name_impl (CpgIntegrator *integrator)
{
	return "Prediction-correction";
}

static void
cpg_integrator_predict_correct_set_property (GObject      *object,
                                             guint         prop_id,
                                             const GValue *value,
                                             GParamSpec   *pspec)
{
	CpgIntegratorPredictCorrect *self = CPG_INTEGRATOR_PREDICT_CORRECT (object);

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
cpg_integrator_predict_correct_get_property (GObject    *object,
                                             guint       prop_id,
                                             GValue     *value,
                                             GParamSpec *pspec)
{
	CpgIntegratorPredictCorrect *self = CPG_INTEGRATOR_PREDICT_CORRECT (object);

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
cpg_integrator_predict_correct_class_init (CpgIntegratorPredictCorrectClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_predict_correct_finalize;
	object_class->set_property = cpg_integrator_predict_correct_set_property;
	object_class->get_property = cpg_integrator_predict_correct_get_property;

	integrator_class->step = cpg_integrator_predict_correct_step_impl;
	integrator_class->get_name = cpg_integrator_predict_correct_get_name_impl;
	integrator_class->reset = cpg_integrator_predict_correct_reset_impl;

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

	g_type_class_add_private (object_class, sizeof(CpgIntegratorPredictCorrectPrivate));
}

static void
cpg_integrator_predict_correct_init (CpgIntegratorPredictCorrect *self)
{
	self->priv = CPG_INTEGRATOR_PREDICT_CORRECT_GET_PRIVATE (self);
	self->priv->prediction_order = 5;
	self->priv->correction_order = 5;
}

/**
 * cpg_integrator_predict_correct_new:
 *
 * Create a new PredictCorrect integrator.
 *
 * Returns: A #CpgIntegratorPredictCorrect
 *
 **/
CpgIntegratorPredictCorrect *
cpg_integrator_predict_correct_new (void)
{
	return g_object_new (CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, NULL);
}

/**
 * cpg_integrator_predict_correct_get_prediction_order:
 * @pc: A #CpgIntegratorPredictCorrect
 *
 * Get the prediction order.
 *
 * Returns: The prediction order
 *
 **/
guint
cpg_integrator_predict_correct_get_prediction_order (CpgIntegratorPredictCorrect *pc)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_PREDICT_CORRECT (pc), 0);

	return pc->priv->prediction_order;
}

/**
 * cpg_integrator_predict_correct_get_correction_order:
 * @pc: A #CpgIntegratorPredictCorrect
 *
 * Get the correction order.
 *
 * Returns: The correction order
 *
 **/
guint
cpg_integrator_predict_correct_get_correction_order (CpgIntegratorPredictCorrect *pc)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_PREDICT_CORRECT (pc), 0);

	return pc->priv->correction_order;
}

/**
 * cpg_integrator_predict_correct_set_prediction_order:
 * @pc: A #CpgIntegratorPredictCorrect
 * @order: The prediction order
 *
 * Set the prediction order.
 *
 **/
void
cpg_integrator_predict_correct_set_prediction_order (CpgIntegratorPredictCorrect *pc,
                                                     guint                        order)
{
	g_return_if_fail (CPG_IS_INTEGRATOR_PREDICT_CORRECT (pc));
	g_return_if_fail (order >= MIN_PREDICTION_ORDER && order <= MAX_PREDICTION_ORDER);

	g_object_set (pc, "prediction-order", order, NULL);
}

/**
 * cpg_integrator_predict_correct_set_correction_order:
 * @pc: A #CpgIntegratorPredictCorrect
 * @order: The correction order
 *
 * Set the correction order.
 *
 **/
void
cpg_integrator_predict_correct_set_correction_order (CpgIntegratorPredictCorrect *pc,
                                                     guint                        order)
{
	g_return_if_fail (CPG_IS_INTEGRATOR_PREDICT_CORRECT (pc));
	g_return_if_fail (order >= MIN_CORRECTION_ORDER && order <= MAX_CORRECTION_ORDER);

	g_object_set (pc, "correction-order", order, NULL);
}
