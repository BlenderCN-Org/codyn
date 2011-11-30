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

#define CDN_INPUT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT, CdnInputPrivate))

struct _CdnInputPrivate
{
};

G_DEFINE_ABSTRACT_TYPE (CdnInput, cdn_input, CDN_TYPE_OBJECT)

static void
cdn_input_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_input_parent_class)->finalize (object);
}

static void
cdn_input_class_init (CdnInputClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_input_finalize;

	/*g_type_class_add_private (object_class, sizeof(CdnInputPrivate));*/
}

static void
cdn_input_init (CdnInput *self)
{
	/*self->priv = CDN_INPUT_GET_PRIVATE (self);*/
}

void
cdn_input_update (CdnInput      *input,
                  CdnIntegrator *integrator)
{
	g_return_if_fail (CDN_IS_INPUT (input));
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));

	if (CDN_INPUT_GET_CLASS (input)->update)
	{
		CDN_INPUT_GET_CLASS (input)->update (input, integrator);
	}
}
