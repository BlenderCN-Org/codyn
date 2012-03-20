/*
 * cdn-integrator-runge-kutta.h
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

#ifndef __CDN_INTEGRATOR_RUNGE_KUTTA_H__
#define __CDN_INTEGRATOR_RUNGE_KUTTA_H__

#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR_RUNGE_KUTTA				(cdn_integrator_runge_kutta_get_type ())
#define CDN_INTEGRATOR_RUNGE_KUTTA(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, CdnIntegratorRungeKutta))
#define CDN_INTEGRATOR_RUNGE_KUTTA_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, CdnIntegratorRungeKutta const))
#define CDN_INTEGRATOR_RUNGE_KUTTA_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, CdnIntegratorRungeKuttaClass))
#define CDN_IS_INTEGRATOR_RUNGE_KUTTA(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA))
#define CDN_IS_INTEGRATOR_RUNGE_KUTTA_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA))
#define CDN_INTEGRATOR_RUNGE_KUTTA_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR_RUNGE_KUTTA, CdnIntegratorRungeKuttaClass))

typedef struct _CdnIntegratorRungeKutta			CdnIntegratorRungeKutta;
typedef struct _CdnIntegratorRungeKuttaClass	CdnIntegratorRungeKuttaClass;
typedef struct _CdnIntegratorRungeKuttaPrivate	CdnIntegratorRungeKuttaPrivate;

struct _CdnIntegratorRungeKutta
{
	/*< private >*/
	CdnIntegrator parent;

	CdnIntegratorRungeKuttaPrivate *priv;
};

struct _CdnIntegratorRungeKuttaClass
{
	/*< private >*/
	CdnIntegratorClass parent_class;
};

GType cdn_integrator_runge_kutta_get_type (void) G_GNUC_CONST;

CdnIntegratorRungeKutta *cdn_integrator_runge_kutta_new (void);

G_END_DECLS

#endif /* __CDN_INTEGRATOR_RUNGE_KUTTA_H__ */
