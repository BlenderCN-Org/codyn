#ifndef __CPG_INTEGRATORS_H__
#define __CPG_INTEGRATORS_H__

#include <cpg-network/cpg-integrator-euler.h>
#include <cpg-network/cpg-integrator-runge-kutta.h>
#include <cpg-network/cpg-integrator-stub.h>

G_BEGIN_DECLS

GSList const *cpg_integrators_list (void);

void cpg_integrators_register (GType gtype);
void cpg_integrators_unregister (GType gtype);

CpgIntegrator *cpg_integrators_create (GType gtype);

GType cpg_integrators_find (gchar const *id);

G_END_DECLS

#endif /* __CPG_INTEGRATORS_H__ */

