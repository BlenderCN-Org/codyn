/*
 * cpg-integrator-leap-frog.c
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

#include "cpg-integrator-leap-frog.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_LEAP_FROG_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_LEAP_FROG, CpgIntegratorLeapFrogPrivate))

/**
 * SECTION:cpg-integrator-leap-frog
 * @short_description: LeapFrog integrator
 *
 * The leap frog integrator is a #CpgIntegrator subclass implementing a leap
 * frog integration scheme (useful for second order systems). You can use this
 * integrator to efficiently integrate dynamical equations of motion. Note
 * that this integration scheme is exactly the same as Euler integration when
 * dealing with first order systems.
 *
 */

struct _CpgIntegratorLeapFrogPrivate
{
	GSList *first_order;
	GSList *second_order;

	GSList *first_order_properties;
	GSList *second_order_properties;
};

G_DEFINE_TYPE (CpgIntegratorLeapFrog, cpg_integrator_leap_frog, CPG_TYPE_INTEGRATOR)

static void
clear_lists (CpgIntegratorLeapFrog *self)
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
cpg_integrator_leap_frog_finalize (GObject *object)
{
	CpgIntegratorLeapFrog *self;

	self = CPG_INTEGRATOR_LEAP_FROG (object);

	clear_lists (self);

	G_OBJECT_CLASS (cpg_integrator_leap_frog_parent_class)->finalize (object);
}

static gchar const *
cpg_integrator_leap_frog_get_name_impl (CpgIntegrator *integrator)
{
	return "Leap Frog";
}

static gdouble
cpg_integrator_leap_frog_step_impl (CpgIntegrator *integrator,
                                    gdouble        t,
                                    gdouble        timestep)
{
	GSList const *integrated;
	CpgIntegratorLeapFrog *self;
	CpgIntegratorState *state;
	GSList *item;

	if (!cpg_integrator_step_prepare (integrator, t, timestep))
	{
		return 0;
	}

	self = (CpgIntegratorLeapFrog *)integrator;
	state = cpg_integrator_get_state (integrator);

	// Reset accumulated state
	integrated = cpg_integrator_state_integrated_properties (state);

	while (integrated)
	{
		cpg_property_set_update (integrated->data, 0);
		integrated = g_slist_next (integrated);
	}

	if (self->priv->second_order)
	{
		cpg_integrator_step_prepare (integrator, t - timestep / 2, timestep);
		cpg_integrator_simulation_step_direct (integrator);

		// Calculate v
		cpg_integrator_simulation_step_integrate (integrator,
		                                          self->priv->second_order);

		// Integrate v
		for (item = self->priv->second_order_properties; item; item = g_slist_next (item))
		{
			cpg_property_set_value (item->data,
			                        cpg_property_get_value (item->data) +
			                        cpg_property_get_update (item->data) * timestep);
		}
	}

	cpg_integrator_step_prepare (integrator, t, timestep);
	cpg_integrator_simulation_step_direct (integrator);

	// Calculate x
	cpg_integrator_simulation_step_integrate (integrator,
	                                          self->priv->first_order);

	// Integrate x
	for (item = self->priv->first_order_properties; item; item = g_slist_next (item))
	{
		cpg_property_set_value (item->data,
		                        cpg_property_get_value (item->data) +
		                        cpg_property_get_update (item->data) * timestep);
	}

	/* Chain up to emit 'step' */
	CPG_INTEGRATOR_CLASS (cpg_integrator_leap_frog_parent_class)->step (integrator,
	                                                                t,
	                                                                timestep);

	return timestep;
}

static void
find_integrated (CpgIntegratorLeapFrog *self)
{
	CpgIntegratorState *state;
	GSList const *integrated;

	state = cpg_integrator_get_state (CPG_INTEGRATOR (self));
	integrated = cpg_integrator_state_integrated_link_actions (state);

	clear_lists (self);

	while (integrated)
	{
		CpgLinkAction *action = integrated->data;
		CpgProperty *target = cpg_link_action_get_target_property (action);

		if (cpg_property_get_integral (target))
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

	integrated = cpg_integrator_state_integrated_properties (state);

	while (integrated)
	{
		if (cpg_property_get_integral (integrated->data))
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
cpg_integrator_leap_frog_reset_impl (CpgIntegrator *integrator)
{
	if (CPG_INTEGRATOR_CLASS (cpg_integrator_leap_frog_parent_class)->reset)
	{
		CPG_INTEGRATOR_CLASS (cpg_integrator_leap_frog_parent_class)->reset (integrator);
	}

	find_integrated (CPG_INTEGRATOR_LEAP_FROG (integrator));
}

static void
cpg_integrator_leap_frog_class_init (CpgIntegratorLeapFrogClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_leap_frog_finalize;

	integrator_class->step = cpg_integrator_leap_frog_step_impl;
	integrator_class->get_name = cpg_integrator_leap_frog_get_name_impl;
	integrator_class->reset = cpg_integrator_leap_frog_reset_impl;

	integrator_class->integrator_id = "leap-frog";

	g_type_class_add_private (object_class, sizeof(CpgIntegratorLeapFrogPrivate));
}

static void
cpg_integrator_leap_frog_init (CpgIntegratorLeapFrog *self)
{
	self->priv = CPG_INTEGRATOR_LEAP_FROG_GET_PRIVATE (self);
}

/**
 * cpg_integrator_leap_frog_new:
 * 
 * Create a new LeapFrog integrator.
 *
 * Returns: A #CpgIntegratorLeapFrog
 *
 **/
CpgIntegratorLeapFrog *
cpg_integrator_leap_frog_new (void)
{
	return g_object_new (CPG_TYPE_INTEGRATOR_LEAP_FROG, NULL);
}
