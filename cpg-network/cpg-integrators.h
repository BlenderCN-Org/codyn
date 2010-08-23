/*
 * cpg-integrators.h
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

#ifndef __CPG_INTEGRATORS_H__
#define __CPG_INTEGRATORS_H__

#include <cpg-network/cpg-integrator-euler.h>
#include <cpg-network/cpg-integrator-predict-correct.h>
#include <cpg-network/cpg-integrator-runge-kutta.h>
#include <cpg-network/cpg-integrator-stub.h>

G_BEGIN_DECLS

GSList const *cpg_integrators_list (void);

void cpg_integrators_register (GType gtype);
void cpg_integrators_unregister (GType gtype);

GSList *cpg_integrators_create (void);

GType cpg_integrators_find (gchar const *id);

G_END_DECLS

#endif /* __CPG_INTEGRATORS_H__ */

