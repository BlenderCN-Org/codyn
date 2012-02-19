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

	CdnIntegratorState *state;
	guint state_phase_changed_id;
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

static void
cdn_integrator_leap_frog_dispose (GObject *object)
{
	CdnIntegratorLeapFrog *self;

	self = CDN_INTEGRATOR_LEAP_FROG (object);

	if (self->priv->state)
	{
		g_signal_handler_disconnect (self->priv->state,
		                             self->priv->state_phase_changed_id);

		g_object_unref (self->priv->state);
		self->priv->state = NULL;
	}

	G_OBJECT_CLASS (cdn_integrator_leap_frog_parent_class)->dispose (object);
}

static gchar const *
cdn_integrator_leap_frog_get_name_impl (CdnIntegrator *integrator)
{
	return "Leap Frog";
}

static void
integrate_values (CdnVariable *variable,
                  gdouble      timestep)
{
	gint numr;
	gint numc;
	gdouble *update;
	gdouble const *s;
	gint i;
	gint num;

	update = cdn_variable_get_update (variable, &numr, &numc);
	s = cdn_variable_get_values (variable, &numr, &numc);
	num = numr * numc;

	for (i = 0; i < num; ++i)
	{
		update[i] = s[i] + update[i] * timestep;
	}

	cdn_variable_set_values (variable, update, numr, numc);
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
	CdnIntegratorClass *cls;

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
		cdn_variable_clear_update (integrated->data);
		integrated = g_slist_next (integrated);
	}

	if (self->priv->second_order)
	{
		cdn_integrator_step_prepare (integrator, t - timestep / 2, timestep);

		// Calculate v
		cdn_integrator_simulation_step_integrate (integrator,
		                                          self->priv->second_order);

		// Integrate v
		for (item = self->priv->second_order_properties; item; item = g_slist_next (item))
		{
			integrate_values (item->data, timestep);
		}
	}

	cdn_integrator_step_prepare (integrator, t, timestep);

	// Calculate x
	cdn_integrator_simulation_step_integrate (integrator,
	                                          self->priv->first_order);

	// Integrate x
	for (item = self->priv->first_order_properties; item; item = g_slist_next (item))
	{
		integrate_values (item->data, timestep);
	}

	/* Chain up to emit 'step' */
	cls = CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class);

	return cls->step (integrator, t, timestep);
}

static void
find_integrated (CdnIntegratorLeapFrog *self)
{
	CdnIntegratorState *state;
	GSList const *integrated;

	state = cdn_integrator_get_state (CDN_INTEGRATOR (self));
	integrated = cdn_integrator_state_phase_integrated_edge_actions (state);

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

	self->priv->second_order_properties =
		g_slist_reverse (self->priv->second_order_properties);

	self->priv->first_order_properties =
		g_slist_reverse (self->priv->first_order_properties);
}

static void
cdn_integrator_leap_frog_reset_impl (CdnIntegrator *integrator)
{
	CdnIntegratorLeapFrog *self;

	if (CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class)->reset)
	{
		CDN_INTEGRATOR_CLASS (cdn_integrator_leap_frog_parent_class)->reset (integrator);
	}

	self = CDN_INTEGRATOR_LEAP_FROG (integrator);

	if (self->priv->state)
	{
		g_signal_handler_disconnect (self->priv->state,
		                             self->priv->state_phase_changed_id);

		g_object_unref (self->priv->state);
		self->priv->state = NULL;
	}

	self->priv->state = cdn_integrator_get_state (integrator);

	if (self->priv->state)
	{
		g_object_ref (self->priv->state);

		self->priv->state_phase_changed_id =
			g_signal_connect_swapped (self->priv->state,
			                          "notify::phase",
			                          G_CALLBACK (find_integrated),
			                          integrator);
	}

	find_integrated (self);
}

static void
cdn_integrator_leap_frog_class_init (CdnIntegratorLeapFrogClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnIntegratorClass *integrator_class = CDN_INTEGRATOR_CLASS (klass);

	object_class->finalize = cdn_integrator_leap_frog_finalize;
	object_class->dispose = cdn_integrator_leap_frog_dispose;

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
