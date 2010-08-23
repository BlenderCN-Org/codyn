/*
 * cpg-integrator-stub.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-integrator-stub.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_STUB_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_STUB, CpgIntegratorStubPrivate))

/*struct _CpgIntegratorStubPrivate
{
};*/

G_DEFINE_TYPE (CpgIntegratorStub, cpg_integrator_stub, CPG_TYPE_INTEGRATOR)

static void
cpg_integrator_stub_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_integrator_stub_parent_class)->finalize (object);
}

void
cpg_integrator_stub_update (CpgIntegratorStub *stub,
                            GSList            *state,
                            gdouble            t,
                            gdouble            dt,
                            gboolean           integrate)
{
	g_return_if_fail (CPG_IS_INTEGRATOR_STUB (stub));

	/* First restore network state from the 'update' value of the cpg states */
	GSList *item = state;

	while (item)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)item->data;
		CpgProperty *property = cpg_integrator_state_get_property (st);

		cpg_property_set_value (property,
		                        cpg_integrator_state_get_update (st));

		item = g_slist_next (item);
	}

	/* Then calculate the differential equations in the network like any other
	   integrator */
	if (integrate)
	{
		cpg_integrator_evaluate (CPG_INTEGRATOR (stub), state, t, dt);
	}
}

static gdouble
cpg_integrator_stub_step_impl (CpgIntegrator *integrator,
                               GSList        *state,
                               gdouble        t,
                               gdouble        timestep)
{
	cpg_integrator_stub_update (CPG_INTEGRATOR_STUB (integrator),
	                            state,
	                            t,
	                            timestep,
	                            FALSE);

	/* Chain up to emit 'step' */
	CPG_INTEGRATOR_CLASS (cpg_integrator_stub_parent_class)->step (integrator,
	                                                               state,
	                                                               t,
	                                                               timestep);

	return timestep;
}

static gchar const *
cpg_integrator_stub_get_name_impl (CpgIntegrator *integrator)
{
	return "Stub";
}

static void
cpg_integrator_stub_class_init (CpgIntegratorStubClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_stub_finalize;

	integrator_class->step = cpg_integrator_stub_step_impl;
	integrator_class->get_name = cpg_integrator_stub_get_name_impl;

	integrator_class->integrator_id = "stub";

	//g_type_class_add_private (object_class, sizeof(CpgIntegratorStubPrivate));
}

static void
cpg_integrator_stub_init (CpgIntegratorStub *self)
{
	//self->priv = CPG_INTEGRATOR_STUB_GET_PRIVATE (self);
}

/**
 * cpg_integrator_stub_new:
 * 
 * Create a new Stub integrator.
 *
 * Returns: A #CpgIntegratorStub
 *
 **/
CpgIntegratorStub *
cpg_integrator_stub_new (void)
{
	return g_object_new (CPG_TYPE_INTEGRATOR_STUB, NULL);
}

GSList *
cpg_integrator_stub_get_state (CpgIntegratorStub *stub)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STUB (stub), NULL);

	return cpg_network_get_integration_state (cpg_integrator_get_network (CPG_INTEGRATOR (stub)));
}
