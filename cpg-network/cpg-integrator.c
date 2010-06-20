#include "cpg-integrator.h"
#include "cpg-ref-counted-private.h"

/**
 * SECTION:integrator
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
	PROP_TIME
};

/* Signals */
enum
{
	STEP,
	BEGIN,
	END,
	NUM_SIGNALS
};

struct _CpgIntegratorState
{
	CpgRefCounted parent;

	CpgProperty *property;
	gdouble update;
};

struct _CpgIntegratorPrivate
{
	CpgObject *object;

	CpgProperty *property_time;
	CpgProperty *property_timestep;

	GSList *state;
};

static guint integrator_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgIntegrator, cpg_integrator, CPG_TYPE_OBJECT)

GType
cpg_integrator_state_get_type (void)
{
	static GType type_id = 0;

	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgIntegratorState",
		                                        cpg_ref_counted_ref,
		                                        cpg_ref_counted_unref);
	}

	return type_id;
}

static void
cpg_integrator_state_free (CpgIntegratorState *state)
{
	g_object_unref (state->property);
	g_slice_free (CpgIntegratorState, state);
}

/**
 * cpg_integrator_state_new:
 * @property: A #CpgProperty
 *
 * Create a new integrator state.
 *
 * Returns: A #CpgIntegratorState
 *
 **/
CpgIntegratorState *
cpg_integrator_state_new (CpgProperty *property)
{
	CpgIntegratorState *state = g_slice_new0 (CpgIntegratorState);

	cpg_ref_counted_init (&(state->parent), (GDestroyNotify)cpg_integrator_state_free);

	state->property = g_object_ref (property);
	return state;
}

static void
cpg_integrator_finalize (GObject *object)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);

	if (self->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (self->priv->object),
		                              (gpointer *)&(self->priv->object));
	}

	g_slist_free (self->priv->state);
	self->priv->state = NULL;

	G_OBJECT_CLASS (cpg_integrator_parent_class)->finalize (object);
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

	to += timestep / 2;

	while (from < to)
	{
		gdouble realstep = step_func (integrator, from, timestep);

		if (realstep <= 0)
		{
			break;
		}

		from += realstep;
	}
}

static void
reset_cache (CpgIntegrator *integrator)
{
	cpg_object_reset_cache (integrator->priv->object);
}

static gdouble
cpg_integrator_step_impl (CpgIntegrator *integrator,
                          gdouble        t,
                          gdouble        timestep)
{
	cpg_property_set_value (integrator->priv->property_time, t + timestep);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	reset_cache (integrator);

	g_signal_emit (integrator, integrator_signals[STEP], 0);
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
cpg_integrator_reset_impl (CpgIntegrator *integrator,
                           GSList const  *state)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	cpg_expression_reset (cpg_property_get_expression (integrator->priv->property_time));
	cpg_expression_reset (cpg_property_get_expression (integrator->priv->property_timestep));

	cpg_expression_compile (cpg_property_get_expression (integrator->priv->property_time),
	                        NULL,
	                        NULL);

	cpg_expression_compile (cpg_property_get_expression (integrator->priv->property_timestep),
	                        NULL,
	                        NULL);

	g_slist_free (integrator->priv->state);
	integrator->priv->state = g_slist_copy ((GSList *)state);
}

static void
cpg_integrator_class_init (CpgIntegratorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->constructor = cpg_integrator_constructor;
	object_class->finalize = cpg_integrator_finalize;

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
			              g_cclosure_marshal_VOID__VOID,
			              G_TYPE_NONE,
			              0);

	/**
	 * CpgIntegrator::begin:
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
			              g_cclosure_marshal_VOID__VOID,
			              G_TYPE_NONE,
			              0);

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
}

static void
cpg_integrator_init (CpgIntegrator *self)
{
	self->priv = CPG_INTEGRATOR_GET_PRIVATE (self);

	self->priv->property_time = cpg_object_add_property (CPG_OBJECT (self), "t", "0", FALSE);
	self->priv->property_timestep = cpg_object_add_property (CPG_OBJECT (self), "dt", "0", FALSE);

	cpg_expression_compile (cpg_property_get_expression (self->priv->property_time),
	                        NULL,
	                        NULL);

	cpg_expression_compile (cpg_property_get_expression (self->priv->property_timestep),
	                        NULL,
	                        NULL);
}

static void
simulation_step (CpgIntegrator *integrator)
{
	cpg_object_evaluate (integrator->priv->object);

	GSList *state = integrator->priv->state;

	/* Collect updates */
	while (state)
	{
		CpgIntegratorState *st = state->data;

		st->update = _cpg_property_get_update (st->property);
		state = g_slist_next (state);
	}
}

/**
 * cpg_integrator_run:
 * @integrator: A #CpgIntegrator
 * @state: A #GSList of #CpgIntegratorState
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
	g_return_if_fail (from < to);
	g_return_if_fail (timestep > 0);

	if (CPG_INTEGRATOR_GET_CLASS (integrator)->run)
	{
		g_signal_emit (integrator, integrator_signals[BEGIN], 0);

		CPG_INTEGRATOR_GET_CLASS (integrator)->run (integrator, from, timestep, to);

		g_signal_emit (integrator, integrator_signals[END], 0);
	}
}

/**
 * cpg_integrator_step:
 * @integrator: A #CpgIntegrator
 * @state: A #GSList of #CpgIntegratorState
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
	g_return_val_if_fail (timestep > 0, 0);

	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	return CPG_INTEGRATOR_GET_CLASS (integrator)->step (integrator, t, timestep);
}

/**
 * cpg_integrator_get_object:
 * @integrator: A #CpgIntegrator
 *
 * Get the object associated with the integrator.
 *
 * Returns: A #CpgObject
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
 * @state: A #GSList of #CpgIntegratorState
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
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, timestep);

	reset_cache (integrator);

	/* Do one simulation step which will set all the update values */
	simulation_step (integrator);
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
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), 0);

	return cpg_property_get_value (integrator->priv->property_time);
}

/**
 * cpg_integrator_reset:
 * @integrator: A #CpgIntegrator
 * @state: A list of #CpgIntegratorState
 *
 * Reset the integrator. This is usually called from #cpg_object_reset on the
 * main network.
 *
 **/
void
cpg_integrator_reset (CpgIntegrator *integrator,
                      GSList const  *state)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	return CPG_INTEGRATOR_GET_CLASS (integrator)->reset (integrator, state);
}

/**
 * cpg_integrator_state_get_update:
 * @state: A #CpgIntegratorState
 *
 * Get the update value for the state.
 *
 * Returns: the update value
 *
 **/
gdouble
cpg_integrator_state_get_update (CpgIntegratorState *state)
{
	return state->update;
}

/**
 * cpg_integrator_state_set_update:
 * @state: A # CpgIntegratorState
 * @value: State update value
 *
 * Set the state update value.
 *
 **/
void
cpg_integrator_state_set_update	(CpgIntegratorState *state,
                                 gdouble             value)
{
	state->update = value;
}

/**
 * cpg_integrator_state_get_property:
 * @state: A #CpgIntegratorState
 *
 * Get the #CpgProperty for the state.
 *
 * Returns: A #CpgProperty
 *
 **/
CpgProperty	*
cpg_integrator_state_get_property (CpgIntegratorState *state)
{
	return state->property;
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

GSList const *
cpg_integrator_get_state (CpgIntegrator *integrator)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), NULL);

	return integrator->priv->state;
}

