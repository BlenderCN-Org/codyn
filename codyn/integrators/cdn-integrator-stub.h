/*
 * cdn-integrator-stub.h
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

#ifndef __CDN_INTEGRATOR_STUB_H__
#define __CDN_INTEGRATOR_STUB_H__

#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR_STUB            (cdn_integrator_stub_get_type ())
#define CDN_INTEGRATOR_STUB(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_STUB, CdnIntegratorStub))
#define CDN_INTEGRATOR_STUB_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_STUB, CdnIntegratorStub const))
#define CDN_INTEGRATOR_STUB_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR_STUB, CdnIntegratorStubClass))
#define CDN_IS_INTEGRATOR_STUB(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR_STUB))
#define CDN_IS_INTEGRATOR_STUB_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR_STUB))
#define CDN_INTEGRATOR_STUB_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR_STUB, CdnIntegratorStubClass))

typedef struct _CdnIntegratorStub        CdnIntegratorStub;
typedef struct _CdnIntegratorStubClass   CdnIntegratorStubClass;
typedef struct _CdnIntegratorStubPrivate CdnIntegratorStubPrivate;

struct _CdnIntegratorStub
{
	/*< private >*/
	CdnIntegrator parent;

	CdnIntegratorStubPrivate *priv;
};

struct _CdnIntegratorStubClass
{
	/*< private >*/
	CdnIntegratorClass parent_class;
};

GType              cdn_integrator_stub_get_type (void) G_GNUC_CONST;
CdnIntegratorStub *cdn_integrator_stub_new      (void);

void               cdn_integrator_stub_update   (CdnIntegratorStub *stub,
                                                 gdouble            t,
                                                 gdouble            dt,
                                                 gboolean           integrate);


G_END_DECLS

#endif /* __CDN_INTEGRATOR_STUB_H__ */
