/*
 * cdn-integrator-leap-frog.c
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

#include "cdn-integrator-leap-frog.h"
#include "cdn-network.h"

#define CDN_INTEGRATOR_LEAP_FROG_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_LEAP_FROG, CdnIntegratorLeapFrogPrivate))

/**
 * SECTION:cdn-integrator-leap-frog
 * @short_description: LeapFrog integrator
 *
 * The leap frog integrator is a #CdnIntegrator subclass implementing a leap
 * frog integration scheme (useful for second order systems). You can use this
 * integrator to efficiently integrate dynamical equations of motion. Note
 * that this integration scheme is exactly the same as Euler integration when
 * dealing with first order systems.
 *
 */

struct _CdnIntegratorLeapFrogPrivate
{
	GSList *first_order;
	GSList *second_order;

	GSList *first_order_properties;
	GSList *second_order_properties;
};

G_DEFINE_TYPE (CdnIntegratorLeapFrog, cdn_integrator_leap_frog, CDN_TYPE_INTEGRATOR)

static void
clear_lists (CdnIntegratorLeapFrog *self)
{
	g_slist_free (self->priv->first_order);
	self->priv->first_order = NULL;

	g_slist_free (self->priv->second_order);
	self->priv->second_order = NULL;

	g_slist_free (self->priv->first_order_properties);
	self->priv->first_order_properties = NULL;

	g_slist_free (self->priv->second_order_properties);
	self->priv->second_order_properties = NULL;
}

static void
cdn_integrator_leap_frog_finalize (GObject *object)
{
	CdnIntegratorLeapFrog *self;

	self = CDN_INTEGRATOR_LEAP_FROG (object);

	clear_lists (self);

	G_OBJECT_CLASS (cdn_integrator_leap_frog_parent_class)->finalize (object);
}

static gchar const *
cdn_integrator_leap_frog_get_name_impl (CdnIntegrator *integrator)
{
	return "Leap Frog";
}

static gdouble
cdn_integrator_leap_frog_step_impl (CdnIntegrator *integrator,
                                    gdouble        t,
                                    gdouble        timestep)
{
	GSList const *integrated;
	CdnIntegratorLeapFrog *self;
	CdnIntegratorState *state;
	GSList *item;

	if (!cdn_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	self = (CdnIntegratorLeapFrog *)integrator;
	state = cdn_integrator_get_state (integrator);

	// Reset accumulated state
	integrated = cdn_integrator_state_integrated_properties (state);

	while (integrated)
	{
		cdn_variable_set_update (integrated->data, 0);
		integrated = g_slist_next (integrated);
	}

	if (self->priv->second_order)
	{
		cdn_integrator_step_prepare (integrator, t - timestep / 2, timestep);
		cdn_integrator_simulation_step_direct (integrator);

		// Calculate v
		cdn_integrator_simulation_step_integrate (integrator,
		                                          self->priv->second_order);

		// Integrate v
		for (item = self->priv->second_order_properties; item; item = g_slist_next (item))
		{
			cdn_variable_set_value (item->data,
			                        cdn_variable_get_value (item->data) +
			                        cdn_variable_get_update (item->data) * timestep);
		}
	}

	cdn_integrator_step_prepare (integrator, t, timestep);
	cdn_integrator_simulation_step_direct (integrator);

	// Calculate x
	cdn_integrator_simulation_step_integrate (integrator,
	                                          self->priv->first_order);

	// Integrate x
	for (item = self->priv->first_order_properties; item; item = g_slist_next (item))
	{
		cdn_variable_set_value (item->data,
		                        cdn_variable_get_value (item->data) +
		                        cdn_variable_get_update (item->data) * timestep);
	}

	/* Chain up to emit 'step' */
	return CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class)->step (integrator,
	                                                                           t,
	                                                                           timestep);
}

static void
find_integrated (CdnIntegratorLeapFrog *self)
{
	CdnIntegratorState *state;
	GSList const *integrated;

	state = cdn_integrator_get_state (CDN_INTEGRATOR (self));
	integrated = cdn_integrator_state_integrated_edge_actions (state);

	clear_lists (self);

	while (integrated)
	{
		CdnEdgeAction *action = integrated->data;
		CdnVariable *target = cdn_edge_action_get_target_variable (action);

		if (cdn_variable_get_integral (target))
		{
			self->priv->second_order =
				g_slist_prepend (self->priv->second_order,
				                 integrated->data);
		}
		else
		{
			self->priv->first_order =
				g_slist_prepend (self->priv->first_order,
				                 integrated->data);
		}

		integrated = g_slist_next (integrated);
	}

	self->priv->second_order = g_slist_reverse (self->priv->second_order);
	self->priv->first_order = g_slist_reverse (self->priv->first_order);

	integrated = cdn_integrator_state_integrated_properties (state);

	while (integrated)
	{
		if (cdn_variable_get_integral (integrated->data))
		{
			self->priv->second_order_properties =
				g_slist_prepend (self->priv->second_order_properties,
				                 integrated->data);
		}
		else
		{
			self->priv->first_order_properties =
				g_slist_prepend (self->priv->first_order_properties,
				                 integrated->data);
		}

		integrated = g_slist_next (integrated);
	}

	self->priv->second_order_properties = g_slist_reverse (self->priv->second_order_properties);
	self->priv->first_order_properties = g_slist_reverse (self->priv->first_order_properties);
}

static void
cdn_integrator_leap_frog_reset_impl (CdnIntegrator *integrator)
{
	if (CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class)->reset)
	{
		CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class)->reset (integrator);
	}

	find_integrated (CDN_INTEGRATOR_LEAP_FROG (integrator));
}

static void
cdn_integrator_leap_frog_class_init (CdnIntegratorLeapFrogClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_leap_frog_finalize;

	integrator_class->step = cdn_integrator_leap_frog_step_impl;
	integrator_class->get_name = cdn_integrator_leap_frog_get_name_impl;
	integrator_class->reset = cdn_integrator_leap_frog_reset_impl;

	integrator_class->integrator_id = "leap-frog";

	g_type_class_add_private (object_class, sizeof(CdnIntegratorLeapFrogPrivate));
}

static void
cdn_integrator_leap_frog_init (CdnIntegratorLeapFrog *self)
{
	self->priv = CDN_INTEGRATOR_LEAP_FROG_GET_PRIVATE (self);
}

/**
 * cdn_integrator_leap_frog_new:
 * 
 * Create a new LeapFrog integrator.
 *
 * Returns: A #CdnIntegratorLeapFrog
 *
 **/
CdnIntegratorLeapFrog *
cdn_integrator_leap_frog_new (void)
{
	return g_object_new (CDN_TYPE_INTEGRATOR_LEAP_FROG, NULL);
}
