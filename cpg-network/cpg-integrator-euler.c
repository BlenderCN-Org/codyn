#include "cpg-integrator-euler.h"
#include "cpg-network.h"

#define CPG_INTEGRATOR_EULER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEulerPrivate))

struct _CpgIntegratorEulerPrivate
{
};

G_DEFINE_TYPE (CpgIntegratorEuler, cpg_integrator_euler, CPG_TYPE_INTEGRATOR)

static void
cpg_integrator_euler_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_integrator_euler_parent_class)->finalize (object);
}

static gdouble
cpg_integrator_euler_step_impl (CpgIntegrator *integrator,
                                GSList        *state,
                                gdouble        t,
                                gdouble        step)
{
	cpg_integrator_evaluate (integrator, state, t, step);

	/* Update values are now contained in state, update the values in the
	   states */
	GSList *item;

	for (item = state; item; item = g_slist_next (item))
	{
		CpgIntegratorState *st = (CpgIntegratorState *)item->data;
		CpgProperty *property = cpg_integrator_state_get_property (st);

		if (cpg_property_get_integrated (property))
		{
			cpg_property_set_value (property,
			                        cpg_property_get_value (property) +
			                        cpg_integrator_state_get_update (st) * step);
		}
		else
		{
			cpg_property_set_value (property,
			                        cpg_integrator_state_get_update (st));
		}
	}

	return step;
}

static void
cpg_integrator_euler_class_init (CpgIntegratorEulerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgIntegratorClass *integrator_class = CPG_INTEGRATOR_CLASS (klass);

	object_class->finalize = cpg_integrator_euler_finalize;

	integrator_class->step = cpg_integrator_euler_step_impl;

	g_type_class_add_private (object_class, sizeof(CpgIntegratorEulerPrivate));
}

static void
cpg_integrator_euler_init (CpgIntegratorEuler *self)
{
	self->priv = CPG_INTEGRATOR_EULER_GET_PRIVATE (self);
}

CpgIntegratorEuler *
cpg_integrator_euler_new (void)
{
	return g_object_new (CPG_TYPE_INTEGRATOR_EULER, NULL);
}
