#include "cpg-integrator-runge-kutta.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKuttaPrivate))

#define MAX_COEFFICIENTS 4

struct _CpgIntegratorRungeKuttaPrivate
{
	gdouble *coefficients[MAX_COEFFICIENTS];
	guint num_coefficients;
};

G_DEFINE_TYPE (CpgIntegratorRungeKutta, cpg_integrator_runge_kutta, CPG_TYPE_INTEGRATOR)

static void
cpg_integrator_runge_kutta_finalize (GObject *object)
{
	CpgIntegratorRungeKutta *rk = CPG_INTEGRATOR_RUNGE_KUTTA (object);
	guint i = 0;

	for (i = 0; i < MAX_COEFFICIENTS; ++i)
	{
		if (rk->priv->coefficients[i])
		{
			g_free (rk->priv->coefficients[i]);
		}
	}

	G_OBJECT_CLASS (cpg_integrator_runge_kutta_parent_class)->finalize (object);
}

static void
initialize_coefficients (CpgIntegratorRungeKutta *rk)
{
	CpgIntegratorState *state = cpg_integrator_get_state (CPG_INTEGRATOR (rk));
	guint len = g_slist_length ((GSList *)cpg_integrator_state_integrated_properties (state));

	if (len == rk->priv->num_coefficients)
	{
		return;
	}

	guint i;
	for (i = 0; i < MAX_COEFFICIENTS; ++i)
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
                    GSList const            *integrated,
                    guint                    order,
                    gdouble                  norm)
{
	guint i = 0;

	while (integrated)
	{
		CpgProperty *prop = integrated->data;

		if (order == 0)
		{
			rk->priv->coefficients[order][i] = cpg_property_get_value (prop);
		}
		else
		{
			if (order == MAX_COEFFICIENTS)
			{
				/* Do the final update right away */
				cpg_property_set_value (prop, rk->priv->coefficients[0][i] + 1.0 / 6 * norm * (
					rk->priv->coefficients[1][i] +
					2 * rk->priv->coefficients[2][i] +
					2 * rk->priv->coefficients[3][i] +
					cpg_property_get_update (prop)
				));
			}
			else
			{
				gdouble ret = cpg_property_get_update (prop);
				rk->priv->coefficients[order][i] = ret;

				/* Prepare for the next iteration */
				cpg_property_set_value (prop,
				                        rk->priv->coefficients[0][i] +
				                        norm * ret);
			}
		}

		integrated = g_slist_next (integrated);
		++i;
	}
}

static void
cpg_integrator_runge_kutta_reset_impl (CpgIntegrator *integrator)
{
	if (CPG_INTEGRATOR_CLASS (cpg_integrator_runge_kutta_parent_class)->reset)
	{
		CPG_INTEGRATOR_CLASS (cpg_integrator_runge_kutta_parent_class)->reset (integrator);
	}

	initialize_coefficients (CPG_INTEGRATOR_RUNGE_KUTTA (integrator));
}

static gdouble
cpg_integrator_runge_kutta_step_impl (CpgIntegrator *integrator,
                                      gdouble        t,
                                      gdouble        timestep)
{
	CpgIntegratorRungeKutta *rk = CPG_INTEGRATOR_RUNGE_KUTTA (integrator);

	CpgIntegratorState *state = cpg_integrator_get_state (integrator);
	GSList const *integrated = cpg_integrator_state_integrated_properties (state);

	/* K_1 = f(t_n, y_n) */
	store_coefficients (rk, integrated, 0, 0);
	cpg_integrator_evaluate (integrator, t, timestep);

	/* K_2 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_1) */
	store_coefficients (rk, integrated, 1, 0.5 * timestep);
	cpg_integrator_evaluate (integrator, t + 0.5 * timestep, 0.5 * timestep);

	/* K_3 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_2) */
	store_coefficients (rk, integrated, 2, 0.5 * timestep);
	cpg_integrator_evaluate (integrator, t + 0.5 * timestep, 0.5 * timestep);

	/* K_4 = f(t_n + h, y_n + h * K_3) */
	store_coefficients (rk, integrated, 3, timestep);
	cpg_integrator_evaluate (integrator, t + timestep, timestep);

	/* This last call will also transfer the new state */
	store_coefficients (rk, integrated, 4, timestep);

	/* Chain up to emit 'step' */
	CPG_INTEGRATOR_CLASS (cpg_integrator_runge_kutta_parent_class)->step (integrator,
	                                                                      t,
	                                                                      timestep);

	return timestep;
}

static gchar const *
cpg_integrator_runge_kutta_get_name_impl (CpgIntegrator *integrator)
{
	return "Runge-Kutta 4th order";
}

static void
cpg_integrator_runge_kutta_class_init (CpgIntegratorRungeKuttaClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_runge_kutta_finalize;

	integrator_class->step = cpg_integrator_runge_kutta_step_impl;
	integrator_class->get_name = cpg_integrator_runge_kutta_get_name_impl;
	integrator_class->reset = cpg_integrator_runge_kutta_reset_impl;

	integrator_class->integrator_id = "runge-kutta";

	g_type_class_add_private (object_class, sizeof(CpgIntegratorRungeKuttaPrivate));
}

static void
cpg_integrator_runge_kutta_init (CpgIntegratorRungeKutta *self)
{
	self->priv = CPG_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE (self);
}

/**
 * cpg_integrator_runge_kutta_new:
 * 
 * Create a new Runge-Kutta, 4th order integrator.
 *
 * Returns: A #CpgIntegratorRungeKutta
 *
 **/
CpgIntegratorRungeKutta *
cpg_integrator_runge_kutta_new ()
{
	return g_object_new (CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, NULL);
}
