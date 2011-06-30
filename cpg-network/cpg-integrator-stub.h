/*
 * cpg-integrator-stub.h
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

#ifndef __CPG_INTEGRATOR_STUB_H__
#define __CPG_INTEGRATOR_STUB_H__

#include <cpg-network/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_STUB            (cpg_integrator_stub_get_type ())
#define CPG_INTEGRATOR_STUB(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STUB, CpgIntegratorStub))
#define CPG_INTEGRATOR_STUB_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STUB, CpgIntegratorStub const))
#define CPG_INTEGRATOR_STUB_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_STUB, CpgIntegratorStubClass))
#define CPG_IS_INTEGRATOR_STUB(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_STUB))
#define CPG_IS_INTEGRATOR_STUB_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_STUB))
#define CPG_INTEGRATOR_STUB_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_STUB, CpgIntegratorStubClass))

typedef struct _CpgIntegratorStub        CpgIntegratorStub;
typedef struct _CpgIntegratorStubClass   CpgIntegratorStubClass;
typedef struct _CpgIntegratorStubPrivate CpgIntegratorStubPrivate;

struct _CpgIntegratorStub
{
	/*< private >*/
	CpgIntegrator parent;

	CpgIntegratorStubPrivate *priv;
};

struct _CpgIntegratorStubClass
{
	/*< private >*/
	CpgIntegratorClass parent_class;
};

GType              cpg_integrator_stub_get_type (void) G_GNUC_CONST;
CpgIntegratorStub *cpg_integrator_stub_new      (void);

void               cpg_integrator_stub_update   (CpgIntegratorStub *stub,
                                                 gdouble            t,
                                                 gdouble            dt,
                                                 gboolean           integrate);


G_END_DECLS

#endif /* __CPG_INTEGRATOR_STUB_H__ */
