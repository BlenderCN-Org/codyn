/*
 * cpg-integrators.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-integrators.h"

/**
 * SECTION:cpg-integrators
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
		cpg_integrators_register (CPG_TYPE_INTEGRATOR_PREDICT_CORRECT);
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
 * Returns: (element-type GTypeClass) (transfer none): A GSList of #GType
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

	gpointer klass = g_type_class_ref (gtype);

	if (g_slist_find (integrators, klass))
	{
		g_type_class_unref (klass);
		return;
	}

	integrators = g_slist_append (integrators, klass);
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
	gpointer klass = g_type_class_peek (gtype);

	if (klass)
	{
		integrators = g_slist_remove (integrators, klass);
	}
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
		CpgIntegratorClass *klass = ints->data;

		if (g_strcmp0 (id, klass->integrator_id) == 0)
		{
			ret = G_TYPE_FROM_CLASS (klass);
		}

		ints = g_slist_next (ints);

		if (ret != G_TYPE_INVALID)
		{
			break;
		}
	}

	return ret;
}

/**
 * cpg_integrators_create:
 *
 * Create a list of instances of all registered integrators.
 *
 * Returns: (element-type CpgIntegrator) (transfer full): A #GSList
 *
 **/
GSList *
cpg_integrators_create (void)
{
	GSList const *ints = cpg_integrators_list ();
	GSList *ret = NULL;

	while (ints)
	{
		ret = g_slist_prepend (ret,
		                       g_object_new (G_TYPE_FROM_CLASS (ints->data), NULL));
		ints = g_slist_next (ints);
	}

	return g_slist_reverse (ret);
}
