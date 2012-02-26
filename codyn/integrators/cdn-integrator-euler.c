/*
 * cdn-integrator-euler.c
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

#include "cdn-integrator-euler.h"
#include "cdn-network.h"

#define CDN_INTEGRATOR_EULER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_EULER, CdnIntegratorEulerPrivate))

/**
 * SECTION:cdn-integrator-euler
 * @short_description: Euler integrator
 *
 * The euler integrator is a #CdnIntegrator subclass implementing a simple
 * euler integration scheme. See #CdnIntegratorPredictCorrect or
 * #CdnIntegratorRungeKutta for more accurate (but slower) integrators.
 *
 */

/*struct _CdnIntegratorEulerPrivate
{
};*/

G_DEFINE_TYPE (CdnIntegratorEuler, cdn_integrator_euler, CDN_TYPE_INTEGRATOR)

static void
cdn_integrator_euler_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_integrator_euler_parent_class)->finalize (object);
}

static void
integrate_values (gdouble *values,
                  gdouble const *s,
                  gint num,
                  gdouble timestep)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		values[i] = s[i] + values[i] * timestep;
	}
}

static gdouble
cdn_integrator_euler_step_impl (CdnIntegrator *integrator,
                                gdouble        t,
                                gdouble        timestep)
{
	CdnIntegratorClass *cls;
	GSList const *integrated;
	CdnIntegratorState *state;

	if (!cdn_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	cdn_integrator_evaluate (integrator, t, timestep);

	/* Update values are now contained in state, update the values in the
	   states */
	state = cdn_integrator_get_state (integrator);
	integrated = cdn_integrator_state_integrated_variables (state);

	while (integrated)
	{
		CdnVariable *variable;
		gint numr;
		gint numc;
		gdouble *update;

		variable = integrated->data;
		update = cdn_variable_get_update (variable, &numr, &numc);

		integrate_values (update,
		                  cdn_variable_get_values (variable, &numr, &numc),
		                  numr * numc,
		                  timestep);

		integrated = g_slist_next (integrated);
	}

	integrated = cdn_integrator_state_integrated_variables (state);

	while (integrated)
	{
		CdnVariable *variable;
		gdouble *update;
		gint numr;
		gint numc;

		variable = integrated->data;
		update = cdn_variable_get_update (integrated->data, &numr, &numc);
		cdn_variable_set_values (variable, update, numr, numc);

		integrated = g_slist_next (integrated);
	}

	cls = CDN_INTEGRATOR_CLASS (cdn_integrator_euler_parent_class);

	/* Chain up to emit 'step' */
	return cls->step (integrator, t, timestep);
}

static gchar const *
cdn_integrator_euler_get_name_impl (CdnIntegrator *integrator)
{
	return "Euler";
}

static void
cdn_integrator_euler_class_init (CdnIntegratorEulerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_euler_finalize;

	integrator_class->step = cdn_integrator_euler_step_impl;
	integrator_class->get_name = cdn_integrator_euler_get_name_impl;

	integrator_class->integrator_id = "euler";

	/*g_type_class_add_private (object_class, sizeof(CdnIntegratorEulerPrivate));*/
}

static void
cdn_integrator_euler_init (CdnIntegratorEuler *self)
{
	/*self->priv = CDN_INTEGRATOR_EULER_GET_PRIVATE (self);*/
}

/**
 * cdn_integrator_euler_new:
 * 
 * Create a new Euler integrator.
 *
 * Returns: A #CdnIntegratorEuler
 *
 **/
CdnIntegratorEuler *
cdn_integrator_euler_new (void)
{
	return g_object_new (CDN_TYPE_INTEGRATOR_EULER, NULL);
}
