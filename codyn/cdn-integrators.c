/*
 * cdn-integrators.c
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

#include "cdn-integrators.h"

static GSList *integrators = NULL;

G_DEFINE_TYPE (CdnIntegrators, cdn_integrators, G_TYPE_OBJECT)

static void
cdn_integrators_class_init (CdnIntegratorsClass *klass)
{
}

static void
cdn_integrators_init (CdnIntegrators *self)
{
}

static void
ensure_defaults ()
{
	static gboolean initing = FALSE;

	if (integrators == NULL && !initing)
	{
		initing = TRUE;

		cdn_integrators_register (CDN_TYPE_INTEGRATOR_EULER);
		cdn_integrators_register (CDN_TYPE_INTEGRATOR_LEAP_FROG);
		cdn_integrators_register (CDN_TYPE_INTEGRATOR_PREDICT_CORRECT);
		cdn_integrators_register (CDN_TYPE_INTEGRATOR_RUNGE_KUTTA);

		initing = FALSE;
	}
}

/**
 * cdn_integrators_list:
 * 
 * Get the list of integrators.
 *
 * Returns: (element-type GTypeClass) (transfer none): A GSList of #GType
 *
 **/
GSList const *
cdn_integrators_list (void)
{
	ensure_defaults ();

	return integrators;
}

/**
 * cdn_integrators_register:
 * @gtype: A #GType
 * 
 * Register a new integrator.
 *
 **/
void
cdn_integrators_register (GType gtype)
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
 * cdn_integrators_unregister:
 * @gtype: A #GType
 * 
 * Unregister an integrator.
 *
 **/
void
cdn_integrators_unregister (GType gtype)
{
	gpointer klass = g_type_class_peek (gtype);

	if (klass)
	{
		integrators = g_slist_remove (integrators, klass);
	}
}

/**
 * cdn_integrators_find:
 * @id: The integrator id
 * 
 * Find an integrator by id.
 *
 * Returns: A #GType of the integrator or #G_TYPE_INVALID when the integrator
 * could not be found.
 *
 **/
GType
cdn_integrators_find (gchar const *id)
{
	GSList const *ints = cdn_integrators_list ();
	GType ret = G_TYPE_INVALID;
	
	while (ints)
	{
		CdnIntegratorClass *klass = ints->data;

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
 * cdn_integrators_create:
 *
 * Create a list of instances of all registered integrators.
 *
 * Returns: (element-type CdnIntegrator) (transfer full): A #GSList
 *
 **/
GSList *
cdn_integrators_create (void)
{
	GSList const *ints = cdn_integrators_list ();
	GSList *ret = NULL;

	while (ints)
	{
		ret = g_slist_prepend (ret,
		                       g_object_new (G_TYPE_FROM_CLASS (ints->data), NULL));
		ints = g_slist_next (ints);
	}

	return g_slist_reverse (ret);
}
