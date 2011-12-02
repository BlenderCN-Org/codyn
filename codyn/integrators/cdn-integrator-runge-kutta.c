/*
 * cdn-integrator-runge-kutta.c
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

#include "cdn-integrator-runge-kutta.h"
#include "cdn-network.h"

#define CDN_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, CdnIntegratorRungeKuttaPrivate))

#define MAX_COEFFICIENTS 4

/**
 * SECTION:cdn-integrator-runge-kutta
 * @short_description: Runge-Kutta order 4 integrator
 *
 * The runge kutta integrator is a #CdnIntegrator subclass implementing a
 * runge-kutta order 4 integration scheme. It is the most accurate integrator
 * available, but also the slowest. For a good compromise between speed and
 * accuracy see #CdnIntegratorPredictCorrect.
 *
 */

struct _CdnIntegratorRungeKuttaPrivate
{
	gdouble *coefficients[MAX_COEFFICIENTS];
	guint num_coefficients;
};

G_DEFINE_TYPE (CdnIntegratorRungeKutta, cdn_integrator_runge_kutta, CDN_TYPE_INTEGRATOR)

static void
cdn_integrator_runge_kutta_finalize (GObject *object)
{
	CdnIntegratorRungeKutta *rk = CDN_INTEGRATOR_RUNGE_KUTTA (object);
	guint i = 0;

	for (i = 0; i < MAX_COEFFICIENTS; ++i)
	{
		if (rk->priv->coefficients[i])
		{
			g_free (rk->priv->coefficients[i]);
		}
	}

	G_OBJECT_CLASS (cdn_integrator_runge_kutta_parent_class)->finalize (object);
}

static void
initialize_coefficients (CdnIntegratorRungeKutta *rk)
{
	CdnIntegratorState *state = cdn_integrator_get_state (CDN_INTEGRATOR (rk));
	guint len = g_slist_length ((GSList *)cdn_integrator_state_integrated_properties (state));

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
store_coefficients (CdnIntegratorRungeKutta *rk,
                    GSList const            *integrated,
                    guint                    order,
                    gdouble                  norm)
{
	guint i = 0;

	while (integrated)
	{
		CdnVariable *prop = integrated->data;

		if (order == 0)
		{
			rk->priv->coefficients[order][i] = cdn_variable_get_value (prop);
		}
		else
		{
			if (order == MAX_COEFFICIENTS)
			{
				/* Do the final update right away */
				cdn_variable_set_value (prop, rk->priv->coefficients[0][i] + 1.0 / 6 * norm * (
					rk->priv->coefficients[1][i] +
					2 * rk->priv->coefficients[2][i] +
					2 * rk->priv->coefficients[3][i] +
					cdn_variable_get_update (prop)
				));
			}
			else
			{
				gdouble ret = cdn_variable_get_update (prop);
				rk->priv->coefficients[order][i] = ret;

				/* Prepare for the next iteration */
				cdn_variable_set_value (prop,
				                        rk->priv->coefficients[0][i] +
				                        norm * ret);
			}
		}

		integrated = g_slist_next (integrated);
		++i;
	}
}

static void
cdn_integrator_runge_kutta_reset_impl (CdnIntegrator *integrator)
{
	if (CDN_INTEGRATOR_CLASS (cdn_integrator_runge_kutta_parent_class)->reset)
	{
		CDN_INTEGRATOR_CLASS (cdn_integrator_runge_kutta_parent_class)->reset (integrator);
	}

	initialize_coefficients (CDN_INTEGRATOR_RUNGE_KUTTA (integrator));
}

static gdouble
cdn_integrator_runge_kutta_step_impl (CdnIntegrator *integrator,
                                      gdouble        t,
                                      gdouble        timestep)
{
	CdnIntegratorRungeKutta *rk = CDN_INTEGRATOR_RUNGE_KUTTA (integrator);

	if (!cdn_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	CdnIntegratorState *state = cdn_integrator_get_state (integrator);
	GSList const *integrated = cdn_integrator_state_integrated_properties (state);

	/* K_1 = f(t_n, y_n) */
	store_coefficients (rk, integrated, 0, 0);
	cdn_integrator_evaluate (integrator, t, timestep);

	/* K_2 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_1) */
	store_coefficients (rk, integrated, 1, 0.5 * timestep);
	cdn_integrator_evaluate (integrator, t + 0.5 * timestep, 0.5 * timestep);

	/* K_3 = f(t_n + 0.5 * h, y_n + 0.5 * h * K_2) */
	store_coefficients (rk, integrated, 2, 0.5 * timestep);
	cdn_integrator_evaluate (integrator, t + 0.5 * timestep, 0.5 * timestep);

	/* K_4 = f(t_n + h, y_n + h * K_3) */
	store_coefficients (rk, integrated, 3, timestep);
	cdn_integrator_evaluate (integrator, t + timestep, timestep);

	/* This last call will also transfer the new state */
	store_coefficients (rk, integrated, 4, timestep);

	/* Chain up to emit 'step' */
	return CDN_INTEGRATOR_CLASS (cdn_integrator_runge_kutta_parent_class)->step (integrator,
	                                                                             t,
	                                                                             timestep);
}

static gchar const *
cdn_integrator_runge_kutta_get_name_impl (CdnIntegrator *integrator)
{
	return "Runge-Kutta 4th order";
}

static void
cdn_integrator_runge_kutta_class_init (CdnIntegratorRungeKuttaClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_runge_kutta_finalize;

	integrator_class->step = cdn_integrator_runge_kutta_step_impl;
	integrator_class->get_name = cdn_integrator_runge_kutta_get_name_impl;
	integrator_class->reset = cdn_integrator_runge_kutta_reset_impl;

	integrator_class->integrator_id = "runge-kutta";

	g_type_class_add_private (object_class, sizeof(CdnIntegratorRungeKuttaPrivate));
}

static void
cdn_integrator_runge_kutta_init (CdnIntegratorRungeKutta *self)
{
	self->priv = CDN_INTEGRATOR_RUNGE_KUTTA_GET_PRIVATE (self);
}

/**
 * cdn_integrator_runge_kutta_new:
 * 
 * Create a new Runge-Kutta, 4th order integrator.
 *
 * Returns: A #CdnIntegratorRungeKutta
 *
 **/
CdnIntegratorRungeKutta *
cdn_integrator_runge_kutta_new ()
{
	return g_object_new (CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, NULL);
}