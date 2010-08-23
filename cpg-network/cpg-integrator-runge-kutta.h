/*
 * cpg-integrator-runge-kutta.h
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

#ifndef __CPG_INTEGRATOR_RUNGE_KUTTA_H__
#define __CPG_INTEGRATOR_RUNGE_KUTTA_H__

#include <cpg-network/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_RUNGE_KUTTA				(cpg_integrator_runge_kutta_get_type ())
#define CPG_INTEGRATOR_RUNGE_KUTTA(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKutta))
#define CPG_INTEGRATOR_RUNGE_KUTTA_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKutta const))
#define CPG_INTEGRATOR_RUNGE_KUTTA_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKuttaClass))
#define CPG_IS_INTEGRATOR_RUNGE_KUTTA(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA))
#define CPG_IS_INTEGRATOR_RUNGE_KUTTA_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA))
#define CPG_INTEGRATOR_RUNGE_KUTTA_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_RUNGE_KUTTA, CpgIntegratorRungeKuttaClass))

typedef struct _CpgIntegratorRungeKutta			CpgIntegratorRungeKutta;
typedef struct _CpgIntegratorRungeKuttaClass	CpgIntegratorRungeKuttaClass;
typedef struct _CpgIntegratorRungeKuttaPrivate	CpgIntegratorRungeKuttaPrivate;

struct _CpgIntegratorRungeKutta {
	CpgIntegrator parent;
	
	CpgIntegratorRungeKuttaPrivate *priv;
};

struct _CpgIntegratorRungeKuttaClass {
	CpgIntegratorClass parent_class;
};

GType cpg_integrator_runge_kutta_get_type (void) G_GNUC_CONST;

CpgIntegratorRungeKutta *cpg_integrator_runge_kutta_new (void);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_RUNGE_KUTTA_H__ */
