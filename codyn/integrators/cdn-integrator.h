/*
 * cdn-integrator.h
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

#ifndef __CDN_INTEGRATOR_H__
#define __CDN_INTEGRATOR_H__

#include <codyn/cdn-object.h>
#include <codyn/cdn-utils.h>
#include <codyn/integrators/cdn-integrator-state.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR            (cdn_integrator_get_type ())
#define CDN_INTEGRATOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR, CdnIntegrator))
#define CDN_INTEGRATOR_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR, CdnIntegrator const))
#define CDN_INTEGRATOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR, CdnIntegratorClass))
#define CDN_IS_INTEGRATOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR))
#define CDN_IS_INTEGRATOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR))
#define CDN_INTEGRATOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR, CdnIntegratorClass))

typedef struct _CdnIntegrator        CdnIntegrator;
typedef struct _CdnIntegratorClass   CdnIntegratorClass;
typedef struct _CdnIntegratorPrivate CdnIntegratorPrivate;

/**
 * CdnIntegrator:
 *
 * Simulation integrator.
 *
 * #CdnIntegrator is a base class for implementing different integration
 * methods.
 */
struct _CdnIntegrator
{
	/*< private >*/
	CdnObject parent;

	CdnIntegratorPrivate *priv;
};

/**
 * CdnIntegratorClass:
 * @run: run virtual function
 * @step: step virtual function
 * @get_name: get_name virtual function
 * @reset: reset virtual function
 * @integrator_id: the integrator id
 *
 * The CdnIntegrator class
 *
 */
struct _CdnIntegratorClass
{
	/*< private >*/
	CdnObjectClass parent_class;

	/*< public >*/
	void         (*run)          (CdnIntegrator *integrator,
	                              gdouble        from,
	                              gdouble        timestep,
	                              gdouble        to);

	gdouble      (*step)         (CdnIntegrator *integrator,
	                              gdouble        t,
	                              gdouble        timestep);

	const gchar *(*get_name)     (CdnIntegrator *integrator);

	void         (*reset)        (CdnIntegrator *integrator);

	/* private field */
	const gchar *integrator_id;
};

GType                cdn_integrator_get_type        (void) G_GNUC_CONST;

CdnIntegratorState  *cdn_integrator_get_state       (CdnIntegrator *integrator);
void                 cdn_integrator_set_state       (CdnIntegrator *integrator,
                                                     CdnIntegratorState *state);

gboolean             cdn_integrator_begin           (CdnIntegrator  *integrator,
                                                     gdouble         start,
                                                     GError        **error);

gboolean             cdn_integrator_end             (CdnIntegrator  *integrator,
                                                     GError        **error);

gboolean             cdn_integrator_run             (CdnIntegrator  *integrator,
                                                     gdouble         from,
                                                     gdouble         timestep,
                                                     gdouble         to,
                                                     GError        **error);

gdouble              cdn_integrator_step            (CdnIntegrator *integrator,
                                                     gdouble        t,
                                                     gdouble        timestep);

gboolean             cdn_integrator_step_prepare    (CdnIntegrator *integrator,
                                                     gdouble        t,
                                                     gdouble        timestep);

void                 cdn_integrator_evaluate        (CdnIntegrator *integrator,
                                                     gdouble        t,
                                                     gdouble        timestep);

void                 cdn_integrator_simulation_step_integrate (CdnIntegrator *integrator,
                                                               GSList const  *actions);

void                 cdn_integrator_reset           (CdnIntegrator *integrator);

const gchar         *cdn_integrator_get_class_id    (CdnIntegrator *integrator);
const gchar         *cdn_integrator_get_name        (CdnIntegrator *integrator);

gdouble              cdn_integrator_get_time        (CdnIntegrator *integrator);
void                 cdn_integrator_set_time        (CdnIntegrator *integrator,
                                                     gdouble        t);

CdnObject           *cdn_integrator_get_object      (CdnIntegrator *integrator);

void                 cdn_integrator_set_real_time   (CdnIntegrator *integrator,
                                                     gdouble        real_time);

gdouble              cdn_integrator_get_real_time   (CdnIntegrator *integrator);

gboolean             cdn_integrator_get_terminate   (CdnIntegrator *integrator);

gdouble              cdn_integrator_get_default_timestep (CdnIntegrator *integrator);
void                 cdn_integrator_set_default_timestep (CdnIntegrator *integrator,
                                                          gdouble        timestep);

G_END_DECLS

#endif /* __CDN_INTEGRATOR_H__ */
