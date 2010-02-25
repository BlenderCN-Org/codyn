#include "cpg-integrators.h"

/**
 * SECTION:integrators
 * @short_description: Integrator registry
 *
 * #CpgIntegrators provides a registry for integrators. All integrators must
 * be registered before loading a #CpgNetwork.
 *
 */

static GSList *integrators = NULL;

static void
ensure_defaults ()
{
	static gboolean initing = FALSE;

	if (integrators == NULL && !initing)
	{
		initing = TRUE;

		cpg_integrators_register (CPG_TYPE_INTEGRATOR_EULER);
		cpg_integrators_register (CPG_TYPE_INTEGRATOR_RUNGE_KUTTA);
		cpg_integrators_register (CPG_TYPE_INTEGRATOR_STUB);

		initing = FALSE;
	}
}

/**
 * cpg_integrators_list:
 * 
 * Get the list of integrators.
 *
 * Returns: A GSList of #GType
 *
 **/
GSList const *
cpg_integrators_list (void)
{
	ensure_defaults ();

	return integrators;
}

/**
 * cpg_integrators_register:
 * @gtype: A #GType
 * 
 * Register a new integrator.
 *
 **/
void
cpg_integrators_register (GType gtype)
{
	ensure_defaults ();

	if (g_slist_find (integrators, GINT_TO_POINTER (gtype)))
	{
		return;
	}

	integrators = g_slist_append (integrators, GINT_TO_POINTER (gtype));
}

/**
 * cpg_integrators_unregister:
 * @gtype: A #GType
 * 
 * Unregister an integrator.
 *
 **/
void
cpg_integrators_unregister (GType gtype)
{
	integrators = g_slist_remove (integrators, GINT_TO_POINTER (gtype));
}

/**
 * cpg_integrators_find:
 * @id: The integrator id
 * 
 * Find an integrator by id.
 *
 * Returns: A #GType of the integrator or #G_TYPE_INVALID when the integrator
 * could not be found.
 *
 **/
GType
cpg_integrators_find (gchar const *id)
{
	GSList const *ints = cpg_integrators_list ();
	GType ret = G_TYPE_INVALID;
	
	while (ints)
	{
		GType gtype = GPOINTER_TO_INT (ints->data);

		CpgIntegratorClass *klass = CPG_INTEGRATOR_CLASS (g_type_class_ref (gtype));

		if (g_strcmp0 (id, klass->integrator_id) == 0)
		{
			ret = gtype;
		}

		g_type_class_unref (klass);
		ints = g_slist_next (ints);

		if (ret != G_TYPE_INVALID)
		{
			break;
		}
	}

	return ret;
}
