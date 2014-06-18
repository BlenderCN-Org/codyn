/*
 * cdn-integrator-euler.h
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

#ifndef __CDN_INTEGRATOR_EULER_H__
#define __CDN_INTEGRATOR_EULER_H__

#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR_EULER				(cdn_integrator_euler_get_type ())
#define CDN_INTEGRATOR_EULER(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_EULER, CdnIntegratorEuler))
#define CDN_INTEGRATOR_EULER_CONST(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_EULER, CdnIntegratorEuler const))
#define CDN_INTEGRATOR_EULER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR_EULER, CdnIntegratorEulerClass))
#define CDN_IS_INTEGRATOR_EULER(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR_EULER))
#define CDN_IS_INTEGRATOR_EULER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR_EULER))
#define CDN_INTEGRATOR_EULER_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR_EULER, CdnIntegratorEulerClass))

typedef struct _CdnIntegratorEuler			CdnIntegratorEuler;
typedef struct _CdnIntegratorEulerClass		CdnIntegratorEulerClass;
typedef struct _CdnIntegratorEulerPrivate	CdnIntegratorEulerPrivate;

/**
 * CdnIntegratorEuler:
 *
 * Euler integrator.
 *
 * The euler integrator is a #CdnIntegrator subclass implementing a simple
 * euler integration scheme. See #CdnIntegratorPredictCorrect or
 * #CdnIntegratorRungeKutta for more accurate (but slower) integrators.
 */
struct _CdnIntegratorEuler
{
	/*< private >*/
	CdnIntegrator parent;

	CdnIntegratorEulerPrivate *priv;
};

struct _CdnIntegratorEulerClass
{
	/*< private >*/
	CdnIntegratorClass parent_class;
};

GType cdn_integrator_euler_get_type (void) G_GNUC_CONST;
CdnIntegratorEuler *cdn_integrator_euler_new (void);

G_END_DECLS

#endif /* __CDN_INTEGRATOR_EULER_H__ */
