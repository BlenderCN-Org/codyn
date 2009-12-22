#include "cpg-integrator-runge-kutta.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKuttaPrivate))

struct _CpgIntegratorRungeKuttaPrivate
{
	GSList *last_state;

	gdouble *coefficients[4];
	guint num_coefficients;
};

G_DEFINE_TYPE (CpgIntegratorRungeKutta, cpg_integrator_runge_kutta, CPG_TYPE_INTEGRATOR)

static void
cpg_integrator_runge_kutta_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_integrator_runge_kutta_parent_class)->finalize (object);
}

static void
initialize_coefficients (CpgIntegratorRungeKutta *rk,
                         GSList                  *state)
{
	guint len = g_slist_length (state);

	if (len == rk->priv->num_coefficients)
	{
		return;
	}

	guint i;
	for (i = 0; i < 4; ++i)
	{
		if (rk->priv->coefficients[i])
		{
			g_free (rk->priv->coefficients[i]);
		}

		rk->priv->coefficients[i] = g_new0 (gdouble, len);
	}

	rk->priv->num_coefficients = len;
}

static void
store_coefficients (CpgIntegratorRungeKutta *rk,
                    GSList                  *state,
                    guint                    order,
                    gdouble                  norm)
{
	guint i = 0;

	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;
		CpgProperty *prop = cpg_integrator_state_get_property (st);

		if (order == 0)
		{
			rk->priv->coefficients[order][i] = cpg_property_get_value (prop);
		}
		else if (cpg_property_get_integrated (prop))
		{
			if (order == 4)
			{
				/* Do the final update right away */
				cpg_property_set_value (prop, rk->priv->coefficients[0][i] + 1.0 / 6 * norm * (
					rk->priv->coefficients[1][i] +
					2 * rk->priv->coefficients[2][i] +
					2 * rk->priv->coefficients[3][i] +
					cpg_integrator_state_get_update (st)
				));
			}
			else
			{
				gdouble ret = cpg_integrator_state_get_update (st);
				rk->priv->coefficients[order][i] = ret;

				/* Prepare for the next iteration */
				cpg_property_set_value (prop, rk->priv->coefficients[0][i] +
				                              norm * ret);
			}
		}
		else if (order == 4)
		{
			cpg_property_set_value (prop, rk->priv->coefficients[0][i]);
		}

		state = g_slist_next (state);
		++i;
	}
}

static gdouble
cpg_integrator_runge_kutta_step_impl (CpgIntegrator *integrator,
                                      GSList        *state,
                                      gdouble        t,
                                      gdouble        step)
{
	CpgIntegratorRungeKutta *rk = CPG_INTEGRATOR_RUNGE_KUTTA (integrator);

	/* Calculate RK4 coefficients */
	if (state != rk->priv->last_state)
	{
		initialize_coefficients (rk, state);
	}

	/* K_1 = f(t_n, y_n) */
	store_coefficients (rk, state, 0, 0);
	cpg_integrator_evaluate (integrator, state, t, step);

	/* K_2 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_1) */
	store_coefficients (rk, state, 1, 0.5 * step);
	cpg_integrator_evaluate (integrator, state, t + 0.5 * step, 0.5 * step);

	/* K_3 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_2) */
	store_coefficients (rk, state, 2, 0.5 * step);
	cpg_integrator_evaluate (integrator, state, t + 0.5 * step, 0.5 * step);

	/* K_4 = f(t_n + h, y_n + h * K_3) */
	store_coefficients (rk, state, 3, 0.5 * step);
	cpg_integrator_evaluate (integrator, state, t + step, step);

	/* This last call will also transfer the new state */
	store_coefficients (rk, state, 4, step);
	return step;
}

static void
cpg_integrator_runge_kutta_class_init (CpgIntegratorRungeKuttaClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_runge_kutta_finalize;

	integrator_class->step = cpg_integrator_runge_kutta_step_impl;

	g_type_class_add_private (object_class, sizeof(CpgIntegratorRungeKuttaPrivate));
}

static void
cpg_integrator_runge_kutta_init (CpgIntegratorRungeKutta *self)
{
	self->priv = CPG_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE (self);
}

CpgIntegratorRungeKutta *
cpg_integrator_runge_kutta_new ()
{
	return g_object_new (CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, NULL);
}
