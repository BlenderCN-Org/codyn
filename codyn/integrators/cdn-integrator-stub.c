/*
 * cdn-integrator-stub.c
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

#include "cdn-integrator-stub.h"
#include "cdn-network.h"

/**
 * SECTION:cdn-integrator-stub
 * @short_description: Stub integrator
 *
 * Stub integrator. This integrator does not really do anything. It can be used
 * as a stub when an external integration scheme is used (for example from
 * matlab).
 *
 */

#define CDN_INTEGRATOR_STUB_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_STUB, CdnIntegratorStubPrivate))

/*struct _CdnIntegratorStubPrivate
{
};*/

G_DEFINE_TYPE (CdnIntegratorStub, cdn_integrator_stub, CDN_TYPE_INTEGRATOR)

static void
cdn_integrator_stub_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_integrator_stub_parent_class)->finalize (object);
}

static void
stub_update (GSList const *properties)
{
	while (properties)
	{
		CdnVariable *property = properties->data;

		cdn_variable_set_value (property,
		                        cdn_variable_get_update (property));

		properties = g_slist_next (properties);
	}
}

/**
 * cdn_integrator_stub_update:
 * @stub: A #CdnIntegratorStub
 * @t: the time
 * @dt: the time step
 * @integrate: whether or not to integrate the system
 * 
 * Update the stub integrator. This will reset property values from their
 * update values and optionally calculate the differential equations.
 *
 **/
void
cdn_integrator_stub_update (CdnIntegratorStub *stub,
                            gdouble            t,
                            gdouble            dt,
                            gboolean           integrate)
{
	g_return_if_fail (CDN_IS_INTEGRATOR_STUB (stub));

	/* First restore network state from the 'update' value of the cdn states */
	CdnIntegratorState *state = cdn_integrator_get_state (CDN_INTEGRATOR (stub));

	stub_update (cdn_integrator_state_integrated_properties (state));
	stub_update (cdn_integrator_state_direct_properties (state));

	/* Then calculate the differential equations in the network like any other
	   integrator */
	if (integrate)
	{
		cdn_integrator_evaluate (CDN_INTEGRATOR (stub), t, dt);
	}
}

static gdouble
cdn_integrator_stub_step_impl (CdnIntegrator *integrator,
                               gdouble        t,
                               gdouble        timestep)
{
	cdn_integrator_stub_update (CDN_INTEGRATOR_STUB (integrator),
	                            t,
	                            timestep,
	                            FALSE);

	/* Chain up to emit 'step' */
	CDN_INTEGRATOR_CLASS (cdn_integrator_stub_parent_class)->step (integrator,
	                                                               t,
	                                                               timestep);

	return timestep;
}

static gchar const *
cdn_integrator_stub_get_name_impl (CdnIntegrator *integrator)
{
	return "Stub";
}

static void
cdn_integrator_stub_class_init (CdnIntegratorStubClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_stub_finalize;

	integrator_class->step = cdn_integrator_stub_step_impl;
	integrator_class->get_name = cdn_integrator_stub_get_name_impl;

	integrator_class->integrator_id = "stub";

	//g_type_class_add_private (object_class, sizeof(CdnIntegratorStubPrivate));
}

static void
cdn_integrator_stub_init (CdnIntegratorStub *self)
{
	//self->priv = CDN_INTEGRATOR_STUB_GET_PRIVATE (self);
}

/**
 * cdn_integrator_stub_new:
 * 
 * Create a new Stub integrator.
 *
 * Returns: A #CdnIntegratorStub
 *
 **/
CdnIntegratorStub *
cdn_integrator_stub_new (void)
{
	return g_object_new (CDN_TYPE_INTEGRATOR_STUB, NULL);
}
