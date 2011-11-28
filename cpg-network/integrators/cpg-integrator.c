/*
 * cpg-integrator.c
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

#include "cpg-integrator.h"
#include "cpg-link.h"
#include "cpg-compile-error.h"
#include "cpg-marshal.h"
#include "cpg-input.h"
#include "operators/cpg-operator.h"
#include "instructions/cpg-instruction-rand.h"

#include <math.h>

/**
 * SECTION:cpg-integrator
 * @short_description: Simulation integrator
 *
 * #CpgIntegrator is a base class for implementing different integration
 * methods.
 *
 */

#define CPG_INTEGRATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR, CpgIntegratorPrivate))

typedef gdouble (*CpgIntegratorStepFunc)(CpgIntegrator *, gdouble, gdouble);

/* Properties */
enum
{
	PROP_0,

	PROP_OBJECT,
	PROP_TIME,
	PROP_STATE
};

/* Signals */
enum
{
	STEP,
	BEGIN,
	END,
	NUM_SIGNALS
};

struct _CpgIntegratorPrivate
{
	CpgObject *object;

	CpgProperty *property_time;
	CpgProperty *property_timestep;

	CpgIntegratorState *state;
};

static guint integrator_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgIntegrator, cpg_integrator, CPG_TYPE_OBJECT)

static void
cpg_integrator_finalize (GObject *object)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);

	if (self->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (self->priv->object),
		                              (gpointer *)&(self->priv->object));
	}

	G_OBJECT_CLASS (cpg_integrator_parent_class)->finalize (object);
}

static void
on_integrator_state_updated (CpgIntegrator *integrator)
{
	cpg_integrator_reset (integrator);
}

static void
set_state (CpgIntegrator      *integrator,
           CpgIntegratorState *state)
{
	if (integrator->priv->state == state)
	{
		return;
	}

	if (integrator->priv->state)
	{
		g_signal_handlers_disconnect_by_func (integrator->priv->state,
		                                      on_integrator_state_updated,
		                                      integrator);

		g_object_unref (integrator->priv->state);
		integrator->priv->state = NULL;
	}

	if (state)
	{
		integrator->priv->state = g_object_ref (state);

		g_signal_connect_swapped (state,
		                          "updated",
		                          G_CALLBACK (on_integrator_state_updated),
		                          integrator);
	}

	g_object_notify (G_OBJECT (integrator), "state");

	cpg_integrator_reset (integrator);
}

static void
cpg_integrator_dispose (GObject *object)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);

	set_state (self, NULL);

	G_OBJECT_CLASS (cpg_integrator_parent_class)->dispose (object);
}

static void
cpg_integrator_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			if (self->priv->object)
			{
				g_object_remove_weak_pointer (G_OBJECT (self->priv->object),
				                              (gpointer *)&(self->priv->object));
			}

			self->priv->object = CPG_OBJECT (g_value_get_object (value));

			g_object_add_weak_pointer (G_OBJECT (self->priv->object),
			                           (gpointer *)&(self->priv->object));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_integrator_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_TIME:
			g_value_set_double (value, cpg_property_get_value (self->priv->property_time));
		break;
		case PROP_STATE:
			g_value_set_object (value, self->priv->state);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_integrator_run_impl (CpgIntegrator *integrator,
                         gdouble        from,
                         gdouble        timestep,
                         gdouble        to)
{
	CpgIntegratorStepFunc step_func = CPG_INTEGRATOR_GET_CLASS (integrator)->step;

	if (!step_func)
	{
		return;
	}

	while (from < to)
	{
		if (to - from < timestep)
		{
			timestep = to - from;
		}

		gdouble realstep = step_func (integrator, from, timestep);

		if (realstep <= 0)
		{
			break;
		}

		from += realstep;
	}
}

static void
next_random (CpgIntegrator *integrator)
{
	GSList const *rnd;
	GSList const *rndexpr;

	/* After the step, we are going to calculate the next random values */
	rnd = cpg_integrator_state_rand_instructions (integrator->priv->state);

	while (rnd)
	{
		cpg_instruction_rand_next (CPG_INSTRUCTION_RAND (rnd->data));
		rnd = g_slist_next (rnd);
	}

	rndexpr = cpg_integrator_state_rand_expressions (integrator->priv->state);

	while (rndexpr)
	{
		cpg_expression_reset_cache (rndexpr->data);
		rndexpr = g_slist_next (rndexpr);
	}
}

static void
reset_function_cache (CpgIntegrator *integrator)
{
	GSList const *func;

	// call reset cache on all the functions
	func = cpg_integrator_state_functions (integrator->priv->state);

	while (func)
	{
		cpg_expression_reset_cache (cpg_function_get_expression (func->data));
		func = g_slist_next (func);
	}
}

static void
prepare_next_step (CpgIntegrator *integrator,
                   gdouble        t,
                   gdouble        timestep)
{
	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	next_random (integrator);

	reset_function_cache (integrator);
}

static gdouble
cpg_integrator_step_impl (CpgIntegrator *integrator,
                          gdouble        t,
                          gdouble        timestep)
{
	prepare_next_step (integrator, t + timestep, timestep);

	g_signal_emit (integrator, integrator_signals[STEP], 0, timestep, t + timestep);
	return timestep;
}

static GObject *
cpg_integrator_constructor (GType                  type,
                            guint                  n_construct_properties,
                            GObjectConstructParam *construct_properties)
{
	GObject *ret = G_OBJECT_CLASS (cpg_integrator_parent_class)->constructor (type,
	                                                                          n_construct_properties,
	                                                                          construct_properties);

	CpgIntegrator *integrator = CPG_INTEGRATOR (ret);
	CpgIntegratorClass *klass = CPG_INTEGRATOR_GET_CLASS (integrator);

	if (!klass->step || klass->step == cpg_integrator_step_impl)
	{
		g_critical ("Subclasses of CpgIntegrator MUST implement the `step' function");

		g_object_unref (ret);
		ret = NULL;
	}
	else if (!klass->get_name)
	{
		g_critical ("Subclasses of CpgIntegrator MUST implement the `get_name` function");

		g_object_unref (ret);
		ret = NULL;
	}
	else
	{
		if (klass->integrator_id)
		{
			cpg_object_set_id (CPG_OBJECT (ret), klass->integrator_id);
		}
	}

	return ret;
}

static void
cpg_integrator_reset_impl (CpgIntegrator *integrator)
{
}

static gboolean
ensure_compiled (CpgIntegrator *integrator)
{
	CpgObject *object = cpg_integrator_state_get_object (integrator->priv->state);

	if (!cpg_object_is_compiled (object))
	{
		CpgCompileError *error = cpg_compile_error_new ();
		gboolean ret = cpg_object_compile (object, NULL, error);
		g_object_unref (error);

		return ret;
	}

	return TRUE;
}

static void
cpg_integrator_class_init (CpgIntegratorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->constructor = cpg_integrator_constructor;
	object_class->finalize = cpg_integrator_finalize;
	object_class->dispose = cpg_integrator_dispose;

	object_class->set_property = cpg_integrator_set_property;
	object_class->get_property = cpg_integrator_get_property;

	klass->run = cpg_integrator_run_impl;
	klass->step = cpg_integrator_step_impl;
	klass->reset = cpg_integrator_reset_impl;

	/**
	 * CpgIntegrator:object:
	 *
	 * The object to integrate
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE));

	/**
	 * CpgIntegrator:time:
	 *
	 * The current simulated time
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_TIME,
	                                 g_param_spec_double ("time",
	                                                      "Time",
	                                                      "Time",
	                                                      0.0,
	                                                      G_MAXDOUBLE,
	                                                      0.0,
	                                                      G_PARAM_READABLE));

	/**
	 * CpgIntegrator::step:
	 * @object: the integrator
	 * @timestep: the timestep
	 * @time: the elapsed time
	 *
	 * Emitted when an integrator step has been performed
	 *
	 **/
	integrator_signals[STEP] =
			g_signal_new ("step",
			              G_OBJECT_CLASS_TYPE (object_class),
			              G_SIGNAL_RUN_LAST,
			              0,
			              NULL, NULL,
			              cpg_marshal_VOID__DOUBLE_DOUBLE,
			              G_TYPE_NONE,
			              2,
			              G_TYPE_DOUBLE,
			              G_TYPE_DOUBLE);

	/**
	 * CpgIntegrator::begin:
	 * @object: the integrator
	 * @from: from where to start the simulation
	 * @timestep: the desired timestep
	 * @to: to where to simulate
	 *
	 * Emitted before running an integration of several steps
	 *
	 **/
	integrator_signals[BEGIN] =
			g_signal_new ("begin",
			              G_OBJECT_CLASS_TYPE (object_class),
			              G_SIGNAL_RUN_LAST,
			              0,
			              NULL, NULL,
			              cpg_marshal_VOID__DOUBLE_DOUBLE_DOUBLE,
			              G_TYPE_NONE,
			              3,
			              G_TYPE_DOUBLE,
			              G_TYPE_DOUBLE,
			              G_TYPE_DOUBLE);

	/**
	 * CpgIntegrator::end:
	 *
	 * Emitted after running an integration of several steps has finished
	 *
	 **/
	integrator_signals[END] =
			g_signal_new ("end",
			              G_OBJECT_CLASS_TYPE (object_class),
			              G_SIGNAL_RUN_LAST,
			              0,
			              NULL, NULL,
			              g_cclosure_marshal_VOID__VOID,
			              G_TYPE_NONE,
			              0);

	g_type_class_add_private (object_class, sizeof(CpgIntegratorPrivate));


	g_object_class_install_property (object_class,
	                                 PROP_STATE,
	                                 g_param_spec_object ("state",
	                                                      "State",
	                                                      "State",
	                                                      CPG_TYPE_INTEGRATOR_STATE,
	                                                      G_PARAM_READABLE));
}

static void
cpg_integrator_init (CpgIntegrator *self)
{
	self->priv = CPG_INTEGRATOR_GET_PRIVATE (self);

	self->priv->property_time = cpg_property_new ("t",
	                                              cpg_expression_new0(),
	                                              CPG_PROPERTY_FLAG_INOUT);
	cpg_object_add_property (CPG_OBJECT (self), self->priv->property_time, NULL);

	self->priv->property_timestep = cpg_property_new ("dt",
	                                                  cpg_expression_new0(),
	                                                  CPG_PROPERTY_FLAG_INOUT);
	cpg_object_add_property (CPG_OBJECT (self), self->priv->property_timestep, NULL);
}

void
cpg_integrator_simulation_step_direct (CpgIntegrator *integrator)
{
	/* First calculate all the direct properties */
	GSList const *direct;

	direct = cpg_integrator_state_direct_link_actions (integrator->priv->state);

	while (direct)
	{
		CpgLinkAction *action = direct->data;

		if (cpg_link_action_get_enabled (action))
		{
			CpgProperty *target = cpg_link_action_get_target_property (action);

			if (target)
			{
				cpg_property_set_update (target, 0);
			}
		}

		direct = g_slist_next (direct);
	}

	direct = cpg_integrator_state_direct_link_actions (integrator->priv->state);

	while (direct)
	{
		CpgLinkAction *action = direct->data;

		if (cpg_link_action_get_enabled (action))
		{
			CpgProperty *target = cpg_link_action_get_target_property (action);

			if (target != NULL)
			{
				CpgExpression *expr = cpg_link_action_get_equation (action);

				cpg_property_set_update (target,
				                         cpg_property_get_update (target) +
				                         cpg_expression_evaluate (expr));

				cpg_property_set_value (target,
				                        cpg_property_get_update (target));
			}
		}

		direct = g_slist_next (direct);
	}
}

void
cpg_integrator_simulation_step_integrate (CpgIntegrator *integrator,
                                          GSList const  *actions)
{
	if (!actions)
	{
		actions = cpg_integrator_state_integrated_link_actions (integrator->priv->state);
	}

	while (actions)
	{
		CpgLinkAction *action = actions->data;

		if (cpg_link_action_get_enabled (action))
		{
			CpgProperty *target = cpg_link_action_get_target_property (action);

			if (target != NULL)
			{
				CpgExpression *expr = cpg_link_action_get_equation (action);

				cpg_property_set_update (target,
				                         cpg_property_get_update (target) +
				                         cpg_expression_evaluate (expr));
			}
		}

		actions = g_slist_next (actions);
	}
}

static void
simulation_step (CpgIntegrator *integrator)
{
	GSList const *integrated;

	/* First calculate all the direct properties */
	cpg_integrator_simulation_step_direct (integrator);

	integrated = cpg_integrator_state_integrated_properties (integrator->priv->state);

	while (integrated)
	{
		cpg_property_set_update (integrated->data, 0);
		integrated = g_slist_next (integrated);
	}

	cpg_integrator_simulation_step_integrate (integrator, NULL);
}

/**
 * cpg_integrator_run:
 * @integrator: A #CpgIntegrator
 * @from: The time at which to start integrating
 * @timestep: The timestep to use for integration
 * @to: The time until which to run the integration
 *
 * Integrate the object for a certain period of time.
 *
 **/
void
cpg_integrator_run (CpgIntegrator *integrator,
                    gdouble        from,
                    gdouble        timestep,
                    gdouble        to)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	cpg_object_reset (integrator->priv->object);

	if (!ensure_compiled (integrator))
	{
		return;
	}

	// Generate set of next random values
	prepare_next_step (integrator, from, timestep);

	if (CPG_INTEGRATOR_GET_CLASS (integrator)->run)
	{
		g_signal_emit (integrator,
		               integrator_signals[BEGIN],
		               0,
		               from,
		               timestep,
		               to);

		CPG_INTEGRATOR_GET_CLASS (integrator)->run (integrator,
		                                            from,
		                                            timestep,
		                                            to);

		g_signal_emit (integrator, integrator_signals[END], 0);
	}
}

/**
 * cpg_integrator_step:
 * @integrator: A #CpgIntegrator
 * @t: The time at which to perform the integration step
 * @timestep: The timestep with which to perform the integration step
 *
 * Perform a single integration step. Use #cpg_integrator_run if you want to
 * run the integration for a period of time.
 *
 * Returns: The real time step with which the integration was performed
 *
 **/
gdouble
cpg_integrator_step (CpgIntegrator *integrator,
                     gdouble        t,
                     gdouble        timestep)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), 0);

	return CPG_INTEGRATOR_GET_CLASS (integrator)->step (integrator, t, timestep);
}

/**
 * cpg_integrator_get_object:
 * @integrator: A #CpgIntegrator
 *
 * Get the object associated with the integrator.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_integrator_get_object (CpgIntegrator *integrator)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), NULL);

	return integrator->priv->object;
}

/**
 * cpg_integrator_evaluate:
 * @integrator: A #CpgIntegrator
 * @t: The time at which to evaluate the object
 * @timestep: The timestep with which the current step is evaluating
 *
 * Evaluate the system of equations comprising the object. This is a utility
 * function for integrator implementations. Call this function to calculate
 * all the states. After this function completes, the update values for the
 * states can be found (@see #cpg_integrator_state_get_update)
 *
 **/
void
cpg_integrator_evaluate (CpgIntegrator *integrator,
                         gdouble        t,
                         gdouble        timestep)
{
	/* Omit type check to increase speed */
	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	/* Do one simulation step which will set all the update values */
	simulation_step (integrator);
}

gboolean
cpg_integrator_step_prepare (CpgIntegrator *integrator,
                             gdouble        t,
                             gdouble        timestep)
{
	/* Omit type check makes it faster */
	if (!ensure_compiled (integrator))
	{
		return FALSE;
	}

	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	return TRUE;
}

/**
 * cpg_integrator_get_time:
 * @integrator: A #CpgIntegrator
 *
 * Get the current time at which the object is being integrated.
 *
 * Returns: the current integration time
 *
 **/
gdouble
cpg_integrator_get_time	(CpgIntegrator *integrator)
{
	/* Omit type check to increase speed */
	return cpg_property_get_value (integrator->priv->property_time);
}

void
cpg_integrator_set_time (CpgIntegrator *integrator,
                         gdouble        t)
{
	/* Omit type check to increase speed */
	cpg_property_set_value (integrator->priv->property_time, t);
}


/**
 * cpg_integrator_reset:
 * @integrator: A #CpgIntegrator
 *
 * Reset the integrator. This is usually called from #cpg_object_reset on the
 * main network.
 *
 **/
void
cpg_integrator_reset (CpgIntegrator *integrator)
{
	/* Omit type check to increase speed */
	return CPG_INTEGRATOR_GET_CLASS (integrator)->reset (integrator);
}

/**
 * cpg_integrator_get_name:
 * @integrator: A #CpgIntegrator
 *
 * The integrator name.
 *
 * Returns: the integrator name
 *
 **/
gchar const *
cpg_integrator_get_name (CpgIntegrator *integrator)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), NULL);

	return CPG_INTEGRATOR_GET_CLASS (integrator)->get_name (integrator);
}

/**
 * cpg_integrator_get_state:
 * @integrator: A #CpgIntegrator
 *
 * Get the integrator state.
 *
 * Returns: (transfer none): A #CpgIntegratorState
 *
 **/
CpgIntegratorState *
cpg_integrator_get_state (CpgIntegrator *integrator)
{
	/* Omit check for speed up */
	return integrator->priv->state;
}

/**
 * cpg_integrator_set_state:
 * @integrator: A #CpgIntegrator
 * @state: A #CpgIntegratorState
 * 
 * Set the integrator state. You should normally not need to use this function.
 *
 **/
void
cpg_integrator_set_state (CpgIntegrator      *integrator,
                          CpgIntegratorState *state)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));
	g_return_if_fail (CPG_IS_INTEGRATOR_STATE (state));

	set_state (integrator, state);
}
