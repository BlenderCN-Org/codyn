/*
 * cdn-integrator.c
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

#include "cdn-integrator.h"
#include "cdn-edge.h"
#include "cdn-compile-error.h"
#include "cdn-marshal.h"
#include "cdn-io.h"
#include "cdn-event.h"
#include "operators/cdn-operator.h"
#include "instructions/cdn-instruction-rand.h"
#include "cdn-debug.h"
#include "cdn-phaseable.h"

#include <math.h>
#include <string.h>

#define CDN_INTEGRATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR, CdnIntegratorPrivate))

typedef gdouble (*CdnIntegratorStepFunc)(CdnIntegrator *, gdouble, gdouble);

/* Properties */
enum
{
	PROP_0,

	PROP_OBJECT,
	PROP_TIME,
	PROP_STATE,
	PROP_REAL_TIME,
	PROP_MINIMUM_TIMESTEP,
	PROP_DEFAULT_TIMESTEP
};

/* Signals */
enum
{
	STEP,
	BEGIN,
	END,
	NUM_SIGNALS
};

struct _CdnIntegratorPrivate
{
	CdnObject *object;

	CdnVariable *property_time;
	CdnVariable *property_timestep;

	CdnIntegratorState *state;
	GSList *saved_state;


	GTimer *step_timer;
	gdouble real_time;
	gdouble rt_correction;
	gdouble minimum_timestep;
	gdouble default_timestep;

	guint terminate : 1;
	guint inner_event_loop : 1;
	guint events_handled : 1;
};

static guint integrator_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CdnIntegrator, cdn_integrator, CDN_TYPE_OBJECT)

typedef struct
{
	CdnVariable *property;
	CdnMatrix values;
} SavedState;

static SavedState *
saved_state_new (CdnVariable *property)
{
	SavedState *ret;

	ret = g_slice_new0 (SavedState);

	ret->property = g_object_ref_sink (property);

	return ret;
}

static void
saved_state_free (SavedState *self)
{
	cdn_matrix_destroy (&self->values);

	g_object_unref (self->property);
	g_slice_free (SavedState, self);
}

static void
cdn_integrator_finalize (GObject *object)
{
	CdnIntegrator *self = CDN_INTEGRATOR (object);

	if (self->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (self->priv->object),
		                              (gpointer *)&(self->priv->object));
	}

	g_slist_foreach (self->priv->saved_state, (GFunc)saved_state_free, NULL);
	g_slist_free (self->priv->saved_state);

	g_timer_destroy (self->priv->step_timer);

	G_OBJECT_CLASS (cdn_integrator_parent_class)->finalize (object);
}

static void
on_integrator_state_updated (CdnIntegrator *integrator)
{
	cdn_integrator_reset (integrator);
}

static void
set_state (CdnIntegrator      *integrator,
           CdnIntegratorState *state)
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

	if (state)
	{
		cdn_integrator_reset (integrator);
	}
}

static void
cdn_integrator_dispose (GObject *object)
{
	CdnIntegrator *self = CDN_INTEGRATOR (object);

	set_state (self, NULL);

	G_OBJECT_CLASS (cdn_integrator_parent_class)->dispose (object);
}

static void
cdn_integrator_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CdnIntegrator *self = CDN_INTEGRATOR (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			if (self->priv->object)
			{
				g_object_remove_weak_pointer (G_OBJECT (self->priv->object),
				                              (gpointer *)&(self->priv->object));
			}

			self->priv->object = CDN_OBJECT (g_value_get_object (value));

			g_object_add_weak_pointer (G_OBJECT (self->priv->object),
			                           (gpointer *)&(self->priv->object));
		break;
		case PROP_REAL_TIME:
			self->priv->real_time = g_value_get_double (value);
			break;
		case PROP_MINIMUM_TIMESTEP:
			self->priv->minimum_timestep = g_value_get_double (value);
			break;
		case PROP_DEFAULT_TIMESTEP:
			self->priv->default_timestep = g_value_get_double (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CdnIntegrator *self = CDN_INTEGRATOR (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_TIME:
			g_value_set_double (value, cdn_variable_get_value (self->priv->property_time));
		break;
		case PROP_STATE:
			g_value_set_object (value, self->priv->state);
		break;
		case PROP_REAL_TIME:
			g_value_set_double (value, self->priv->real_time);
			break;
		case PROP_MINIMUM_TIMESTEP:
			g_value_set_double (value, self->priv->minimum_timestep);
			break;
		case PROP_DEFAULT_TIMESTEP:
			g_value_set_double (value, self->priv->default_timestep);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_run_impl (CdnIntegrator *integrator,
                         gdouble        from,
                         gdouble        timestep,
                         gdouble        to)
{
	CdnIntegratorStepFunc step_func = CDN_INTEGRATOR_GET_CLASS (integrator)->step;

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

		if (realstep <= 0 || integrator->priv->terminate)
		{
			break;
		}

		from += realstep;
	}
}

static void
next_random (CdnIntegrator *integrator)
{
	GSList const *rnd;
	GSList const *rndexpr;

	/* After the step, we are going to calculate the next random values */
	rnd = cdn_integrator_state_rand_instructions (integrator->priv->state);

	while (rnd)
	{
		cdn_instruction_rand_next (CDN_INSTRUCTION_RAND (rnd->data));
		rnd = g_slist_next (rnd);
	}

	rndexpr = cdn_integrator_state_rand_expressions (integrator->priv->state);

	while (rndexpr)
	{
		// We need to explicitly reset the cache of the expressions
		// that have random numbers because the expression has no
		// means of detecting that we changed the random number...
		_cdn_expression_reset_rand_cache (rndexpr->data);
		rndexpr = g_slist_next (rndexpr);
	}
}

static void
reset_function_cache (CdnIntegrator *integrator)
{
	GSList const *func;

	// call reset cache on all the functions
	func = cdn_integrator_state_functions (integrator->priv->state);

	while (func)
	{
		CdnExpression *expr;

		expr = cdn_function_get_expression (func->data);

		if (expr)
		{
			cdn_expression_reset_cache (expr);
		}

		func = g_slist_next (func);
	}
}

static void
update_events (CdnIntegrator *integrator)
{
	GSList const *events;

	// Here we are going to check our events
	events = cdn_integrator_state_events (integrator->priv->state);

	while (events)
	{
		cdn_event_update (events->data);
		events = g_slist_next (events);
	}
}

static void
prepare_next_step (CdnIntegrator *integrator,
                   gdouble        t,
                   gdouble        timestep,
                   gboolean       genrand)
{
	GSList *item;
	GSList const *io;
	GSList const *ops;

	if (!integrator->priv->events_handled)
	{
		update_events (integrator);
	}

	cdn_variable_set_value (integrator->priv->property_time, t);
	cdn_variable_set_value (integrator->priv->property_timestep, timestep);

	if (genrand)
	{
		next_random (integrator);
	}

	reset_function_cache (integrator);

	// Update io
	io = cdn_integrator_state_io (integrator->priv->state);

	while (io)
	{
		cdn_io_update (CDN_IO (io->data), integrator);
		io = g_slist_next (io);
	}

	for (item = integrator->priv->saved_state; item; item = g_slist_next (item))
	{
		SavedState *s = item->data;
		CdnMatrix const *vals;

		vals = cdn_variable_get_values (s->property);
		cdn_matrix_copy (&s->values, vals);
	}

	// Update operators
	ops = cdn_integrator_state_operators (integrator->priv->state);

	while (ops)
	{
		cdn_operator_step (ops->data, t, timestep);
		ops = g_slist_next (ops);
	}
}

static void
restore_saved_state (CdnIntegrator *integrator)
{
	GSList *item;

	for (item = integrator->priv->saved_state; item; item = g_slist_next (item))
	{
		SavedState *s = item->data;

		cdn_variable_set_values (s->property, &s->values);
	}
}

static void
execute_events (CdnIntegrator *integrator,
                GSList        *events)
{
	gchar const *state;

	update_events (integrator);

	while (events)
	{
		CdnEvent *ev = events->data;
		CdnNode *parent;
		gchar const *curstate;

		parent = cdn_object_get_parent (CDN_OBJECT (ev));
		curstate = cdn_node_get_state (parent);

		// Make sure event is still active
		if (cdn_phaseable_is_active (CDN_PHASEABLE (ev), curstate))
		{
			state = cdn_event_get_goto_state (ev);

			cdn_event_execute (ev);

			if (state)
			{
				cdn_integrator_state_set_state (integrator->priv->state,
				                                parent,
				                                state,
				                                NULL,
				                                NULL);
			}
			else if (cdn_event_get_terminal (ev))
			{
				integrator->priv->terminate = TRUE;
			}
		}

		events = g_slist_next (events);
	}
}

static GSList *
append_happened (GSList    *events,
                 CdnEvent  *event,
                 gdouble   *smallest,
                 CdnEvent **smallest_event)
{
	gdouble dist;
	gdouble prec = 1e-9;
	GSList *first = events;

	dist = cdn_event_last_distance (event);

	if (*smallest_event != event)
	{
		*smallest = dist;
		*smallest_event = event;

		return g_slist_prepend (events, event);
	}

	if (dist > *smallest + prec)
	{
		return events;
	}

	if (dist >= *smallest)
	{
		return g_slist_prepend (events, event);
	}

	*smallest = dist;

	while (events)
	{
		gdouble olddist = cdn_event_last_distance (events->data);

		if (olddist > *smallest + prec)
		{
			GSList *next;

			next = events->next;
			first = g_slist_delete_link (first, events);
			events = next;
		}
		else
		{
			events = g_slist_next (events);
		}
	}

	first = g_slist_prepend (first, event);
	return first;
}

static void
handle_events (CdnIntegrator *integrator,
               gdouble        t,
               gdouble       *timestep)
{
	GSList const *events;
	GSList *happened = NULL;
	CdnEvent *smallest_event = NULL;
	gdouble smallest = 0;

	if (!integrator->priv->inner_event_loop)
	{
		integrator->priv->events_handled = FALSE;
	}

	// Here we are going to check our events
	events = cdn_integrator_state_phase_events (integrator->priv->state);

	while (events)
	{
		CdnEvent *ev = events->data;

		if (cdn_event_happened (ev, NULL))
		{
			happened = append_happened (happened, ev, &smallest, &smallest_event);
		}

		events = g_slist_next (events);
	}

	if (happened)
	{
		// Backup the simulation a bit if needed
		if (smallest < (1 - 1e-9) && *timestep > integrator->priv->minimum_timestep)
		{
			gdouble nts;
			gboolean isinner;

			restore_saved_state (integrator);

			*timestep = smallest * *timestep;

			if (*timestep < integrator->priv->minimum_timestep)
			{
				*timestep = integrator->priv->minimum_timestep;
			}

			isinner = integrator->priv->inner_event_loop;
			integrator->priv->inner_event_loop = TRUE;

			nts = cdn_integrator_step (integrator, t, *timestep);

			integrator->priv->inner_event_loop = isinner;

			*timestep = nts;
		}
		else
		{
			execute_events (integrator, happened);
			integrator->priv->events_handled = TRUE;
		}

		g_slist_free (happened);
	}
}

static void
evaluate_discrete (CdnIntegrator *integrator)
{
	GSList const *variables;
	GSList const *actions;
	GSList const *variable;

	variables = cdn_integrator_state_discrete_variables (integrator->priv->state);

	for (variable = variables; variable; variable = g_slist_next (variable))
	{
		cdn_variable_clear_update (variable->data);
	}

	actions = cdn_integrator_state_phase_discrete_edge_actions (integrator->priv->state);

	if (actions)
	{
		cdn_integrator_simulation_step_integrate (integrator, actions);
	}
}

static void
update_discrete (CdnIntegrator *integrator)
{
	GSList const *variables;
	GSList const *variable;

	variables = cdn_integrator_state_discrete_variables (integrator->priv->state);

	for (variable = variables; variable; variable = g_slist_next (variable))
	{
		CdnMatrix *update;
		CdnMatrix const *m;
		gint i;
		gdouble *updatemem;
		gdouble const *vals;
		gint num;

		update = cdn_variable_get_update (variable->data);
		m = cdn_variable_get_values (variable->data);

		updatemem = cdn_matrix_get_memory (update);
		vals = cdn_matrix_get (m);

		num = cdn_matrix_size (m);

		for (i = 0; i < num; ++i)
		{
			updatemem[i] = vals[i] + updatemem[i];
		}

		cdn_variable_set_values (variable->data, update);
	}
}

static gdouble
cdn_integrator_step_impl (CdnIntegrator *integrator,
                          gdouble        t,
                          gdouble        timestep)
{
	gdouble elapsed;

	update_discrete (integrator);

	handle_events (integrator, t, &timestep);

	if (integrator->priv->inner_event_loop)
	{
		return timestep;
	}

	prepare_next_step (integrator, t + timestep, timestep, TRUE);

	g_signal_emit (integrator,
	               integrator_signals[STEP],
	               0,
	               t + timestep,
	               timestep);

	cdn_debug_message (DEBUG_INTEGRATOR, "Step");

	elapsed = g_timer_elapsed (integrator->priv->step_timer, NULL);

	if (integrator->priv->real_time > 0 &&
	    timestep > elapsed * integrator->priv->real_time)
	{
		gdouble s = (timestep - elapsed) / integrator->priv->real_time +
		            integrator->priv->rt_correction;
		gulong ms = (gulong)(G_USEC_PER_SEC * s);
		gdouble slept;

		if (s > 0)
		{
			g_usleep (ms);
		}

		slept = g_timer_elapsed (integrator->priv->step_timer, NULL);
		integrator->priv->rt_correction = s - slept;
	}

	g_timer_reset (integrator->priv->step_timer);

	return timestep;
}

static GObject *
cdn_integrator_constructor (GType                  type,
                            guint                  n_construct_properties,
                            GObjectConstructParam *construct_properties)
{
	GObject *ret = G_OBJECT_CLASS (cdn_integrator_parent_class)->constructor (type,
	                                                                          n_construct_properties,
	                                                                          construct_properties);

	CdnIntegrator *integrator = CDN_INTEGRATOR (ret);
	CdnIntegratorClass *klass = CDN_INTEGRATOR_GET_CLASS (integrator);

	if (!klass->step || klass->step == cdn_integrator_step_impl)
	{
		g_critical ("Subclasses of CdnIntegrator MUST implement the `step' function");

		g_object_unref (ret);
		ret = NULL;
	}
	else if (!klass->get_name)
	{
		g_critical ("Subclasses of CdnIntegrator MUST implement the `get_name` function");

		g_object_unref (ret);
		ret = NULL;
	}
	else
	{
		if (klass->integrator_id)
		{
			cdn_object_set_id (CDN_OBJECT (ret), klass->integrator_id);
		}
	}

	return ret;
}

static void
cdn_integrator_reset_impl (CdnIntegrator *integrator)
{
	GSList const *items;

	g_slist_foreach (integrator->priv->saved_state, (GFunc)saved_state_free, NULL);
	g_slist_free (integrator->priv->saved_state);
	integrator->priv->saved_state = NULL;

	integrator->priv->terminate = FALSE;

	if (!integrator->priv->state)
	{
		return;
	}

	/* Only store state if needed (i.e. when there are events) */
	if (cdn_integrator_state_events (integrator->priv->state))
	{
		items = cdn_integrator_state_integrated_variables (integrator->priv->state);

		while (items)
		{
			integrator->priv->saved_state =
				g_slist_prepend (integrator->priv->saved_state,
				                 saved_state_new (items->data));

			items = g_slist_next (items);
		}
	}

	items = cdn_integrator_state_operators (integrator->priv->state);

	while (items)
	{
		cdn_operator_initialize_integrate (items->data, integrator);
		items = g_slist_next (items);
	}
}

static gboolean
ensure_compiled (CdnIntegrator  *integrator,
                 GError        **err)
{
	CdnObject *object = cdn_integrator_state_get_object (integrator->priv->state);

	if (!cdn_object_is_compiled (object))
	{
		CdnCompileError *error = cdn_compile_error_new ();
		gboolean ret = cdn_object_compile (object, NULL, error);

		if (!ret && err)
		{
			*err = g_error_copy (cdn_compile_error_get_error (error));
		}

		g_object_unref (error);

		return ret;
	}

	return TRUE;
}

static void
cdn_integrator_class_init (CdnIntegratorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->constructor = cdn_integrator_constructor;
	object_class->finalize = cdn_integrator_finalize;
	object_class->dispose = cdn_integrator_dispose;

	object_class->set_property = cdn_integrator_set_property;
	object_class->get_property = cdn_integrator_get_property;

	klass->run = cdn_integrator_run_impl;
	klass->step = cdn_integrator_step_impl;
	klass->reset = cdn_integrator_reset_impl;

	/**
	 * CdnIntegrator:object:
	 *
	 * The object to integrate
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE));

	/**
	 * CdnIntegrator:time:
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
	 * CdnIntegrator::step:
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
			              cdn_marshal_VOID__DOUBLE_DOUBLE,
			              G_TYPE_NONE,
			              2,
			              G_TYPE_DOUBLE,
			              G_TYPE_DOUBLE);

	/**
	 * CdnIntegrator::begin:
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
			              g_cclosure_marshal_VOID__DOUBLE,
			              G_TYPE_NONE,
			              1,
			              G_TYPE_DOUBLE);

	/**
	 * CdnIntegrator::end:
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

	g_type_class_add_private (object_class, sizeof(CdnIntegratorPrivate));


	g_object_class_install_property (object_class,
	                                 PROP_STATE,
	                                 g_param_spec_object ("state",
	                                                      "State",
	                                                      "State",
	                                                      CDN_TYPE_INTEGRATOR_STATE,
	                                                      G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_REAL_TIME,
	                                 g_param_spec_double ("real-time",
	                                                      "Real Time",
	                                                      "Real time",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_MINIMUM_TIMESTEP,
	                                 g_param_spec_double ("minimum-timestep",
	                                                      "Minimum Timestep",
	                                                      "Minimum timestep",
	                                                      G_MINDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      1e-9,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_DEFAULT_TIMESTEP,
	                                 g_param_spec_double ("default-timestep",
	                                                      "Default Timestep",
	                                                      "Default timestep",
	                                                      G_MINDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      0.001,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS |
	                                                      G_PARAM_CONSTRUCT));
}

static void
cdn_integrator_init (CdnIntegrator *self)
{
	self->priv = CDN_INTEGRATOR_GET_PRIVATE (self);

	self->priv->property_time = cdn_variable_new ("t",
	                                              cdn_expression_new0(),
	                                              CDN_VARIABLE_FLAG_INOUT);

	cdn_object_add_variable (CDN_OBJECT (self), self->priv->property_time, NULL);

	self->priv->property_timestep = cdn_variable_new ("dt",
	                                                  cdn_expression_new0(),
	                                                  CDN_VARIABLE_FLAG_INOUT);

	cdn_object_add_variable (CDN_OBJECT (self), self->priv->property_timestep, NULL);

	self->priv->step_timer = g_timer_new ();
}

static void
sum_values (gdouble       *values,
            gdouble const *s,
            gint const    *indices,
            gint           num)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		gint idx;

		idx = indices ? indices[i] : i;

		values[idx] += s[i];
	}
}

/**
 * cdn_integrator_simulation_step_integrate:
 * @integrator: A #CdnIntegrator
 * @actions: (element-type CdnEdgeAction): A #GSList of #CdnEdgeAction
 *
 * Execute one integration step.
 *
 **/
void
cdn_integrator_simulation_step_integrate (CdnIntegrator *integrator,
                                          GSList const  *actions)
{
	if (!actions)
	{
		actions = cdn_integrator_state_phase_integrated_edge_actions (integrator->priv->state);
	}

	while (actions)
	{
		CdnEdgeAction *action;
		CdnVariable *target;

		action = actions->data;
		target = cdn_edge_action_get_target_variable (action);

		if (target != NULL)
		{
			CdnExpression *expr;
			CdnMatrix *update;
			gint const *indices;
			gint num_indices;
			CdnMatrix const *values;

			expr = cdn_edge_action_get_equation (action);

			update = cdn_variable_get_update (target);

			indices = cdn_edge_action_get_indices (action,
			                                       &num_indices);

			values = cdn_expression_evaluate_values (expr);

			sum_values (cdn_matrix_get_memory (update),
			            cdn_matrix_get (values),
			            indices,
			            indices ? num_indices : cdn_matrix_size (values));
		}

		actions = g_slist_next (actions);
	}
}

static void
simulation_step (CdnIntegrator *integrator)
{
	GSList const *integrated;

	integrated = cdn_integrator_state_integrated_variables (integrator->priv->state);

	while (integrated)
	{
		cdn_variable_clear_update (integrated->data);
		integrated = g_slist_next (integrated);
	}

	cdn_integrator_simulation_step_integrate (integrator, NULL);
}

typedef void (*IoStartFunc) (CdnIo *io,
                             GCancellable *cancellable,
                             GAsyncReadyCallback callback,
                             gpointer userdata);

typedef gboolean (*IoFinishFunc) (CdnIo *io, GAsyncResult *result, GError **error);

typedef struct
{
	GMainLoop *loop;
	guint cnt;
	GError *error;
	IoFinishFunc ffunc;
} IoInfo;

static void
io_inifini_ready (CdnIo        *io,
                  GAsyncResult *result,
                  IoInfo       *info)
{
	GError *error = NULL;

	if (!info->ffunc (io, result, &error))
	{
		if (info->error == NULL)
		{
			g_propagate_error (&info->error, error);
		}
		else
		{
			g_error_free (error);
		}
	}

	if (--info->cnt == 0)
	{
		g_main_loop_quit (info->loop);
	}
}

static gboolean
inifini_io (CdnIntegrator  *integrator,
            IoStartFunc     startfunc,
            IoFinishFunc    callback,
            GError        **error)
{
	GSList const *io;
	IoInfo info = {0,};
	gboolean ret;

	io = cdn_integrator_state_io (integrator->priv->state);

	if (!io)
	{
		return TRUE;
	}

	info.loop = g_main_loop_new (NULL, FALSE);
	info.ffunc = callback;

	while (io)
	{
		++info.cnt;

		startfunc (io->data,
		           NULL,
		           (GAsyncReadyCallback)io_inifini_ready,
		           &info);

		io = g_slist_next (io);
	}

	g_main_loop_run (info.loop);
	g_main_loop_unref (info.loop);

	ret = info.error == NULL;

	if (info.error)
	{
		g_propagate_error (error, info.error);
	}

	return ret;
}

/**
 * cdn_integrator_begin:
 * @start: the start
 * @error: a #GError or %NULL
 *
 * Initialize the integrator for integration starting at @start. This
 * emits the ::begin signal.
 *
 * Returns: %TRUE if the integrator was initialized properly, %FALSE otherwise.
 *
 */
gboolean
cdn_integrator_begin (CdnIntegrator  *integrator,
                      gdouble         start,
                      GError        **error)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), FALSE);

	if (!ensure_compiled (integrator, error))
	{
		return FALSE;
	}

	// Initialize io
	if (!inifini_io (integrator,
	                 cdn_io_initialize_async,
	                 cdn_io_initialize_finish,
	                 error))
	{
		return FALSE;
	}

	integrator->priv->terminate = FALSE;

	// Generate set of next random values
	prepare_next_step (integrator, start, 0, FALSE);

	g_timer_reset (integrator->priv->step_timer);

	g_signal_emit (integrator,
	               integrator_signals[BEGIN],
	               0,
	               start);

	cdn_debug_message (DEBUG_INTEGRATOR, "Begin");

	return TRUE;
}

/**
 * cdn_integrator_end:
 * @error: a #GError or %NULL
 *
 * Finalize the integrator for ending integration. This emits the ::end signal.
 *
 * Returns: %TRUE if the integrator was finalized properly, %FALSE otherwise.
 *
 */
gboolean
cdn_integrator_end (CdnIntegrator  *integrator,
                    GError        **error)
{
	gboolean ret;

	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), FALSE);

	// Finalize io
	ret = inifini_io (integrator,
	                  cdn_io_finalize_async,
	                  cdn_io_finalize_finish,
	                  error);

	g_signal_emit (integrator, integrator_signals[END], 0);

	cdn_debug_message (DEBUG_INTEGRATOR, "End");
	return ret;
}

/**
 * cdn_integrator_run:
 * @integrator: A #CdnIntegrator
 * @from: The time at which to start integrating
 * @timestep: The timestep to use for integration
 * @to: The time until which to run the integration
 * @error: a #GError
 *
 * Integrate the object for a certain period of time.
 *
 **/
gboolean
cdn_integrator_run (CdnIntegrator  *integrator,
                    gdouble         from,
                    gdouble         timestep,
                    gdouble         to,
                    GError        **error)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), FALSE);

	if (!cdn_integrator_begin (integrator, from, error))
	{
		return FALSE;
	}

	if (timestep == 0)
	{
		timestep = integrator->priv->default_timestep;
	}

	CDN_INTEGRATOR_GET_CLASS (integrator)->run (integrator,
	                                            from,
	                                            timestep,
	                                            to);

	if (!cdn_integrator_end (integrator, error))
	{
		return FALSE;
	}

	return TRUE;
}

/**
 * cdn_integrator_step:
 * @integrator: A #CdnIntegrator
 * @t: The time at which to perform the integration step
 * @timestep: The timestep with which to perform the integration step
 *
 * Perform a single integration step. Use #cdn_integrator_run if you want to
 * run the integration for a period of time.
 *
 * Returns: The real time step with which the integration was performed
 *
 **/
gdouble
cdn_integrator_step (CdnIntegrator *integrator,
                     gdouble        t,
                     gdouble        timestep)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), 0);

	if (timestep == 0)
	{
		timestep = integrator->priv->default_timestep;
	}

	return CDN_INTEGRATOR_GET_CLASS (integrator)->step (integrator, t, timestep);
}

/**
 * cdn_integrator_get_object:
 * @integrator: A #CdnIntegrator
 *
 * Get the object associated with the integrator.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_integrator_get_object (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), NULL);

	return integrator->priv->object;
}

/**
 * cdn_integrator_evaluate:
 * @integrator: A #CdnIntegrator
 * @t: The time at which to evaluate the object
 * @timestep: The timestep with which the current step is evaluating
 *
 * Evaluate the system of equations comprising the object. This is a utility
 * function for integrator implementations. Call this function to calculate
 * all the states. After this function completes, the update values for the
 * states can be found (@see #cdn_integrator_state_get_update)
 *
 **/
void
cdn_integrator_evaluate (CdnIntegrator *integrator,
                         gdouble        t,
                         gdouble        timestep)
{
	/* Omit type check to increase speed */
	cdn_variable_set_value (integrator->priv->property_time, t);
	cdn_variable_set_value (integrator->priv->property_timestep, timestep);

	/* Do one simulation step which will set all the update values */
	simulation_step (integrator);
}

/**
 * cdn_integrator_step_prepare:
 * @integrator: the #CdnIntegrator
 * @t: the time
 * @timestep: the time step
 *
 * Prepare the integrator for the next time step. This can be called by integrator
 * implementations. You usually do not need to call this manually.
 *
 * Returns: %TRUE if the step was prepared, %FALSE otherwise.
 *
 */
gboolean
cdn_integrator_step_prepare (CdnIntegrator *integrator,
                             gdouble        t,
                             gdouble        timestep)
{
	/* Omit type check makes it faster */
	if (!ensure_compiled (integrator, NULL))
	{
		return FALSE;
	}

	cdn_variable_set_value (integrator->priv->property_time, t);
	cdn_variable_set_value (integrator->priv->property_timestep, timestep);

	evaluate_discrete (integrator);

	return TRUE;
}

/**
 * cdn_integrator_get_time:
 * @integrator: A #CdnIntegrator
 *
 * Get the current time at which the object is being integrated.
 *
 * Returns: the current integration time
 *
 **/
gdouble
cdn_integrator_get_time	(CdnIntegrator *integrator)
{
	/* Omit type check to increase speed */
	return cdn_variable_get_value (integrator->priv->property_time);
}

/**
 * cdn_integrator_set_time:
 * @integrator: A #CdnIntegrator
 * @t: the time
 *
 * Set the current time at which the object is being integrated.
 *
 **/
void
cdn_integrator_set_time (CdnIntegrator *integrator,
                         gdouble        t)
{
	/* Omit type check to increase speed */
	cdn_variable_set_value (integrator->priv->property_time, t);
}


/**
 * cdn_integrator_reset:
 * @integrator: A #CdnIntegrator
 *
 * Reset the integrator. This is usually called from #cdn_object_reset on the
 * main network.
 *
 **/
void
cdn_integrator_reset (CdnIntegrator *integrator)
{
	/* Omit type check to increase speed */
	return CDN_INTEGRATOR_GET_CLASS (integrator)->reset (integrator);
}

/**
 * cdn_integrator_get_name:
 * @integrator: A #CdnIntegrator
 *
 * The integrator name.
 *
 * Returns: the integrator name
 *
 **/
gchar const *
cdn_integrator_get_name (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), NULL);

	return CDN_INTEGRATOR_GET_CLASS (integrator)->get_name (integrator);
}

/**
 * cdn_integrator_get_class_id:
 * @integrator: A #CdnIntegrator
 *
 * The integrator class id.
 *
 * Returns: the integrator class id
 *
 **/
gchar const *
cdn_integrator_get_class_id (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), NULL);

	return CDN_INTEGRATOR_GET_CLASS (integrator)->integrator_id;
}

/**
 * cdn_integrator_get_state:
 * @integrator: A #CdnIntegrator
 *
 * Get the integrator state.
 *
 * Returns: (transfer none): A #CdnIntegratorState
 *
 **/
CdnIntegratorState *
cdn_integrator_get_state (CdnIntegrator *integrator)
{
	/* Omit check for speed up */
	return integrator->priv->state;
}

/**
 * cdn_integrator_set_state:
 * @integrator: A #CdnIntegrator
 * @state: A #CdnIntegratorState
 *
 * Set the integrator state. You should normally not need to use this function.
 *
 **/
void
cdn_integrator_set_state (CdnIntegrator      *integrator,
                          CdnIntegratorState *state)
{
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));
	g_return_if_fail (CDN_IS_INTEGRATOR_STATE (state));

	set_state (integrator, state);
}

/**
 * cdn_integrator_set_real_time:
 * @integrator: a #CdnIntegrator.
 * @real_time: real time value.
 *
 * Set how many times real time the integrator is trying to integrate the
 * network. Set @real_time to 0 to integrate the network as fast as possible.
 * Otherwise, the integrator will integrate at @real_time real time, using
 * sleep to pause the integration if needed. Note that this can only make your
 * network integrate slower, not faster.
 *
 **/
void
cdn_integrator_set_real_time (CdnIntegrator *integrator,
                              gdouble        real_time)
{
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));
	g_return_if_fail (real_time >= 0);

	integrator->priv->real_time = real_time;
}

/**
 * cdn_integrator_get_real_time:
 * @integrator: a #CdnIntegrator.
 *
 * Get how many times real time the integrator is trying to integrate the
 * network. The special value 0 means that the integrator will integrate as
 * fast as possible.
 *
 * Returns: the real-time.
 *
 **/
gdouble
cdn_integrator_get_real_time (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), 0);

	return integrator->priv->real_time;
}

/**
 * cdn_integrator_get_terminate:
 * @integrator: the #CdnIntegrator
 *
 * Get whether the integration has been terminated by an event.
 *
 * Returns: %TRUE if the integration has been terminated, %FALSE otherwise
 *
 */
gboolean
cdn_integrator_get_terminate (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), TRUE);

	return integrator->priv->terminate;
}

/**
 * cdn_integrator_get_default_timestep:
 * @integrator: the #CdnIntegrator
 *
 * Get the default integration time step
 *
 * Returns: the default integration time step
 *
 */
gdouble
cdn_integrator_get_default_timestep (CdnIntegrator *integrator)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR (integrator), 0);

	return integrator->priv->default_timestep;
}

/**
 * cdn_integrator_set_default_timestep:
 * @integrator: the #CdnIntegrator
 * @timestep: the default time step
 *
 * Set the default integration time step
 *
 */
void
cdn_integrator_set_default_timestep (CdnIntegrator *integrator,
                                     gdouble        timestep)
{
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));

	integrator->priv->default_timestep = timestep;
	g_object_notify (G_OBJECT (integrator), "default-timestep");
}
