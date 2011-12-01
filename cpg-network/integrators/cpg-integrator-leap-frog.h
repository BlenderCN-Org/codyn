/*
 * cpg-integrator-leap-frog.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_INTEGRATOR_LEAP_FROG_H__
#define __CPG_INTEGRATOR_LEAP_FROG_H__

#include <cpg-network/integrators/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_LEAP_FROG				(cpg_integrator_leap_frog_get_type ())
#define CPG_INTEGRATOR_LEAP_FROG(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_LEAP_FROG, CpgIntegratorLeapFrog))
#define CPG_INTEGRATOR_LEAP_FROG_CONST(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_LEAP_FROG, CpgIntegratorLeapFrog const))
#define CPG_INTEGRATOR_LEAP_FROG_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_LEAP_FROG, CpgIntegratorLeapFrogClass))
#define CPG_IS_INTEGRATOR_LEAP_FROG(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_LEAP_FROG))
#define CPG_IS_INTEGRATOR_LEAP_FROG_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_LEAP_FROG))
#define CPG_INTEGRATOR_LEAP_FROG_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_LEAP_FROG, CpgIntegratorLeapFrogClass))

typedef struct _CpgIntegratorLeapFrog		CpgIntegratorLeapFrog;
typedef struct _CpgIntegratorLeapFrogClass	CpgIntegratorLeapFrogClass;
typedef struct _CpgIntegratorLeapFrogPrivate	CpgIntegratorLeapFrogPrivate;

struct _CpgIntegratorLeapFrog
{
	/*< private >*/
	CpgIntegrator parent;

	CpgIntegratorLeapFrogPrivate *priv;
};

struct _CpgIntegratorLeapFrogClass
{
	/*< private >*/
	CpgIntegratorClass parent_class;
};

GType cpg_integrator_leap_frog_get_type (void) G_GNUC_CONST;
CpgIntegratorLeapFrog *cpg_integrator_leap_frog_new (void);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_LEAP_FROG_H__ */
