/*
 * cpg-integrator-euler.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-integrator-euler.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_EULER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEulerPrivate))

/**
 * SECTION:cpg-integrator-euler
 * @short_description: Euler integrator
 *
 * The euler integrator is a #CpgIntegrator subclass implementing a simple
 * euler integration scheme. See #CpgIntegratorPredictCorrect or
 * #CpgIntegratorRungeKutta for more accurate (but slower) integrators.
 *
 */

/*struct _CpgIntegratorEulerPrivate
{
};*/

G_DEFINE_TYPE (CpgIntegratorEuler, cpg_integrator_euler, CPG_TYPE_INTEGRATOR)

static void
cpg_integrator_euler_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_integrator_euler_parent_class)->finalize (object);
}

static gdouble
cpg_integrator_euler_step_impl (CpgIntegrator *integrator,
                                gdouble        t,
                                gdouble        timestep)
{
	if (!cpg_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	cpg_integrator_evaluate (integrator, t, timestep);

	/* Update values are now contained in state, update the values in the
	   states */
	CpgIntegratorState *state = cpg_integrator_get_state (integrator);
	GSList const *integrated = cpg_integrator_state_integrated_properties (state);

	while (integrated)
	{
		CpgProperty *property = integrated->data;

		cpg_property_set_value (property,
		                        cpg_property_get_value (property) +
		                        cpg_property_get_update (property) * timestep);

		integrated = g_slist_next (integrated);
	}

	/* Chain up to emit 'step' */
	return CPG_INTEGRATOR_CLASS (cpg_integrator_euler_parent_class)->step (integrator,
	                                                                       t,
	                                                                       timestep);
}

static gchar const *
cpg_integrator_euler_get_name_impl (CpgIntegrator *integrator)
{
	return "Euler";
}

static void
cpg_integrator_euler_class_init (CpgIntegratorEulerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_euler_finalize;

	integrator_class->step = cpg_integrator_euler_step_impl;
	integrator_class->get_name = cpg_integrator_euler_get_name_impl;

	integrator_class->integrator_id = "euler";

	/*g_type_class_add_private (object_class, sizeof(CpgIntegratorEulerPrivate));*/
}

static void
cpg_integrator_euler_init (CpgIntegratorEuler *self)
{
	/*self->priv = CPG_INTEGRATOR_EULER_GET_PRIVATE (self);*/
}

/**
 * cpg_integrator_euler_new:
 * 
 * Create a new Euler integrator.
 *
 * Returns: A #CpgIntegratorEuler
 *
 **/
CpgIntegratorEuler *
cpg_integrator_euler_new (void)
{
	return g_object_new (CPG_TYPE_INTEGRATOR_EULER, NULL);
}
