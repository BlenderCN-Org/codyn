#include "cpg-integrator.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR, CpgIntegratorPrivate))

typedef gdouble (*CpgIntegratorStepFunc)(CpgIntegrator *, GSList *, gdouble, gdouble);

/* Properties */
enum
{
	PROP_0,

	PROP_NETWORK,
	PROP_TIME
};

struct _CpgIntegratorState
{
	CpgRefCounted parent;

	CpgProperty *property;

	gdouble value;
	gdouble update;
};

struct _CpgIntegratorPrivate
{
	CpgNetwork *network;

	CpgProperty *property_time;
	CpgProperty *property_timestep;
};

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
	cpg_ref_counted_unref (state->property);
	g_slice_free (CpgIntegratorState, state);
}

CpgIntegratorState *
cpg_integrator_state_new (CpgProperty *property)
{
	CpgIntegratorState *state = g_slice_new0 (CpgIntegratorState);

	cpg_ref_counted_init (&(state->parent), (GDestroyNotify)cpg_integrator_state_free);

	state->property = cpg_ref_counted_ref (property);
	state->value = cpg_property_get_value (state->property);

	return state;
}

static void
cpg_integrator_finalize (GObject *object)
{
	if (self->priv->network)
	{
		g_object_remove_weak_pointer (G_OBJECT (self->priv->network),
		                              &(self->priv->network));
	}

	G_OBJECT_CLASS (cpg_integrator_parent_class)->finalize (object);
}

static void
cpg_integrator_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);
	
	switch (prop_id)
	{
		case PROP_NETWORK:
			if (self->priv->network)
			{
				g_object_remove_weak_pointer (G_OBJECT (self->priv->network),
				                              &(self->priv->network));
			}

			self->priv->network = CPG_NETWORK (g_value_get_object (value));
			g_object_add_weak_pointer (G_OBJECT (self->priv->network),
			                           &(self->priv->network));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_integrator_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgIntegrator *self = CPG_INTEGRATOR (object);
	
	switch (prop_id)
	{
		case PROP_NETWORK:
			g_value_set_object (value, self->priv->network);
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
                         GSList        *state,
                         gdouble        from,
                         gdouble        step,
                         gdouble        to)
{
	CpgIntegratorStepFunc step_func = CPG_INTEGRATOR_GET_CLASS (integrator)->step;

	if (!step_func)
	{
		return;
	}

	to += step / 2;

	while (from < to)
	{
		from += step_func (integrator, state, from, step);
	}
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

	if (!CPG_INTEGRATOR_GET_CLASS (integrator)->step)
	{
		g_critical ("Subclasses of CpgIntegrator MUST implement the `step' function");
		g_object_unref (ret);

		ret = NULL;
	}

	return ret;
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

	g_object_class_install_property (object_class,
	                                 PROP_NETWORK,
	                                 g_param_spec_object ("network",
	                                                      "Network",
	                                                      "Network",
	                                                      G_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_TIME,
	                                 g_param_spec_double ("time",
	                                                      "Time",
	                                                      "Time",
	                                                      G_MINDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      0.0,
	                                                      G_PARAM_READABLE));

	g_type_class_add_private (object_class, sizeof(CpgIntegratorPrivate));
}

static void
cpg_integrator_init (CpgIntegrator *self)
{
	self->priv = CPG_INTEGRATOR_GET_PRIVATE (self);

	self->priv->property_time = cpg_object_add_property (CPG_OBJECT (self), "t", "0", FALSE);
	self->priv->property_timestep = cpg_object_add_property (CPG_OBJECT (self), "dt", "0", FALSE);

	cpg_expression_compile (cpg_property_get_value_expression (self->priv->property_time),
	                        NULL,
	                        NULL);

	cpg_expression_compile (cpg_property_get_value_expression (self->priv->property_timestep),
	                        NULL,
	                        NULL);
}

static void
evaluate_objects (CpgIntegrator  *integrator,
                  GType           type)
{
	GSList *item;
	
	for (item = network->priv->states; item; item = g_slist_next (item))
	{
		CpgObject *obj = CPG_OBJECT (item->data);
		
		if (g_type_is_a (G_TYPE_FROM_INSTANCE (obj), type))
		{
			cpg_object_evaluate (obj, network->priv->timestep);
		}
	}
}

static void
simulation_evaluate_relays (CpgIntegrator *integrator)
{
	cpg_debug_evaluate ("Evaluate relays");
	evaluate_objects (integrator, CPG_TYPE_RELAY);
}

static void
simulation_evaluate_states (CpgIntegrator *integrator)
{
	cpg_debug_evaluate ("Evaluate states");
	evaluate_objects (integrator, CPG_TYPE_STATE);
}

static void
simulation_step (CpgIntegrator *integrator,
                 GSList         *state)
{
	GSList *all = cpg_network_get_states (integrator->priv->network);
	GList *states = NULL;
	GList *laststate = NULL;

	/* Evaluate all the relays, then the states */
	while (all)
	{
		CpgObject *obj = CPG_OBJECT (all->data);
		
		if (CPG_IS_RELAY (obj))
		{
			cpg_object_evaluate (obj);
		}
		else
		{
			states = g_list_prepend (states, obj);

			if (!states->next)
			{
				laststate = states;
			}
		}

		all = g_slist_next (all);
	}

	/* Evaluate states */
	while (laststate)
	{
		cpg_object_evaluate (CPG_OBJECT (laststate->data));
		laststate = g_list_previous (laststate);
	}

	/* Collect updates */
	while (state)
	{
		CpgIntegratorState *st = (CpgIntegratorState *)state->data;

		st->update = _cpg_property_get_update (st->property);
		state = g_slist_next (state);
	}
}

static GSList *
create_states (GSList *states)
{
	GSList *ret = NULL;

	while (states)
	{
		ret = g_slist_prepend (ret, cpg_integrator_state_new ((CpgProperty *)states->data));
		states = g_slist_next (states);
	}

	return g_slist_reverse (ret);
}

static void
reset_cache (CpgIntegrator *integrator)
{
	g_slist_foreach (cpg_network_get_states (integrator->priv->network),
	                 (GFunc)cpg_object_reset_cache,
	                 NULL);

	g_slist_foreach (cpg_network_get_links (integrator->priv->network),
	                 (GFunc)cpg_object_reset_cache,
	                 NULL);
	
	cpg_object_reset (cpg_network_get_globals (integrator->priv->network));

	g_slist_foreach (cpg_network_get_functions (integrator->priv->network),
	                 (GFunc)cpg_object_reset_cache,
	                 NULL);
}

static void
apply_states (CpgIntegrator *integrator
              GSList        *states)
{
	while (states)
	{
		CpgIntegratorState *state = (CpgIntegratorState *)states->data;

		cpg_property_set_value (state->property, state->value);
		states = g_slist_next (states);
	}
}

CpgIntegrator *
cpg_integrator_new (CpgNetwork *network)
{
	return g_object_new (CPG_TYPE_INTEGRATOR, "network", network, NULL);
}

void
cpg_integrator_run (CpgIntegrator *integrator,
                    GSList        *state,
                    gdouble        from,
                    gdouble        step,
                    gdouble        to)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));
	g_return_if_fail (from < to);
	g_return_if_fail (step > 0);

	if (!state)
	{
		return;
	}

	if (CPG_INTEGRATOR_GET_CLASS (integrator)->run)
	{
		GSList *intstates = create_states (state);

		CPG_INTEGRATOR_GET_CLASS (integrator)->run (intstates, from, step, to);

		g_slist_foreach (intstates, (GFunc)cpg_ref_counted_unref, NULL);
		g_slist_free (intstates);
	}
}

gdouble
cpg_integrator_step (CpgIntegrator *integrator,
                     GSList        *state,
                     gdouble        t,
                     gdouble        step)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));
	g_return_if_fail (step > 0);

	if (!state)
	{
		return;
	}

	GSList *intstates = create_states (state);

	gdouble ret = CPG_INTEGRATOR_GET_CLASS (integrator)->step (integrator, state, t, step);

	g_slist_foreach (intstates, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (intstates);

	return ret;
}

CpgNetwork	*
cpg_integrator_get_network (CpgIntegrator *integrator)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), NULL);

	return integrator->priv->network;
}

void
cpg_integrator_evaluate (CpgIntegrator *integrator,
                         GSList        *state,
                         gdouble        t,
                         gdouble        step)
{
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	if (!state)
	{
		return;
	}

	/* Compile network if needed */
	if (!cpg_network_is_compiled (integrator->priv->network))
	{
		CpgCompileError *error = cpg_compile_context_new ();

		if (!cpg_network_compile (integrator->priv->network, error))
		{
			cpg_ref_counted_unref (error);
			return;
		}

		cpg_ref_counted_unref (error);
	}

	cpg_property_set_value (integrator->priv->property_time, t);
	cpg_property_set_value (integrator->priv->property_timestep, step);

	apply_states (integrator, state);
	reset_cache (integrator);

	/* Do one simulation step which will set all the update values */
	simulation_step (integrator, state);
}

gdouble
cpg_integrator_get_time	(CpgIntegrator *integrator)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR (integrator), 0);

	return cpg_property_get_value (integrator->priv->property_time);
}
