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

struct _CpgIntegratorRungeKutta
{
	/*< private >*/
	CpgIntegrator parent;

	CpgIntegratorRungeKuttaPrivate *priv;
};

struct _CpgIntegratorRungeKuttaClass
{
	/*< private >*/
	CpgIntegratorClass parent_class;
};

GType cpg_integrator_runge_kutta_get_type (void) G_GNUC_CONST;

CpgIntegratorRungeKutta *cpg_integrator_runge_kutta_new (void);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_RUNGE_KUTTA_H__ */
