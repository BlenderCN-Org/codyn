/*
 * cdn-input.c
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

#include "cdn-input.h"

G_DEFINE_INTERFACE (CdnInput, cdn_input, CDN_TYPE_OBJECT)

/* Default implementation */
static void
cdn_input_update_default (CdnInput      *self,
                          CdnIntegrator *integrator)
{
}

static void
cdn_input_default_init (CdnInputInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->update = cdn_input_update_default;

	if (G_UNLIKELY (!initialized))
	{
		initialized = TRUE;
	}
}

void
cdn_input_update (CdnInput      *input,
                  CdnIntegrator *integrator)
{
	g_return_if_fail (CDN_IS_INPUT (input));
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));

	return CDN_INPUT_GET_INTERFACE (input)->update (input, integrator);
}
