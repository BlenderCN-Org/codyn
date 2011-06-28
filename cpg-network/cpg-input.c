/*
 * cpg-input.c
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

#include "cpg-input.h"

#define CPG_INPUT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INPUT, CpgInputPrivate))

struct _CpgInputPrivate
{
};

G_DEFINE_ABSTRACT_TYPE (CpgInput, cpg_input, CPG_TYPE_OBJECT)

static void
cpg_input_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_input_parent_class)->finalize (object);
}

static void
cpg_input_class_init (CpgInputClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_input_finalize;

	/*g_type_class_add_private (object_class, sizeof(CpgInputPrivate));*/
}

static void
cpg_input_init (CpgInput *self)
{
	/*self->priv = CPG_INPUT_GET_PRIVATE (self);*/
}

void
cpg_input_update (CpgInput      *input,
                  CpgIntegrator *integrator)
{
	g_return_if_fail (CPG_IS_INPUT (input));
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	if (CPG_INPUT_GET_CLASS (input)->update)
	{
		CPG_INPUT_GET_CLASS (input)->update (input, integrator);
	}
}
