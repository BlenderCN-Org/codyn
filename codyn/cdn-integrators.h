/*
 * cdn-integrators.h
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

#ifndef __CDN_INTEGRATORS_H__
#define __CDN_INTEGRATORS_H__

#include <codyn/integrators/cdn-integrator-euler.h>
#include <codyn/integrators/cdn-integrator-leap-frog.h>
#include <codyn/integrators/cdn-integrator-predict-correct.h>
#include <codyn/integrators/cdn-integrator-runge-kutta.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATORS		(cdn_integrators_get_type ())
#define CDN_INTEGRATORS(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATORS, CdnIntegrators))
#define CDN_INTEGRATORS_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATORS, CdnIntegrators const))
#define CDN_INTEGRATORS_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATORS, CdnIntegratorsClass))
#define CDN_IS_INTEGRATORS(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATORS))
#define CDN_IS_INTEGRATORS_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATORS))
#define CDN_INTEGRATORS_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATORS, CdnIntegratorsClass))

typedef struct _CdnIntegrators		CdnIntegrators;
typedef struct _CdnIntegratorsClass	CdnIntegratorsClass;

/**
 * CdnIntegrators:
 *
 * Integrator registry.
 *
 * #CdnIntegrators provides a registry for integrators. All integrators must
 * be registered before loading a #CdnNetwork.
 **/
struct _CdnIntegrators
{
	/*< private >*/
	GObject parent;
};

struct _CdnIntegratorsClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_integrators_get_type (void);
GSList const *cdn_integrators_list (void);

void cdn_integrators_register (GType gtype);
void cdn_integrators_unregister (GType gtype);

GSList *cdn_integrators_create (void);

GType cdn_integrators_find (const gchar *id);

G_END_DECLS

#endif /* __CDN_INTEGRATORS_H__ */

