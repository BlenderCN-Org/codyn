#ifndef __CPG_INTEGRATOR_EULER_H__
#define __CPG_INTEGRATOR_EULER_H__

#include <cpg-network/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_EULER				(cpg_integrator_euler_get_type ())
#define CPG_INTEGRATOR_EULER(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEuler))
#define CPG_INTEGRATOR_EULER_CONST(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEuler const))
#define CPG_INTEGRATOR_EULER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEulerClass))
#define CPG_IS_INTEGRATOR_EULER(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_EULER))
#define CPG_IS_INTEGRATOR_EULER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_EULER))
#define CPG_INTEGRATOR_EULER_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_EULER, CpgIntegratorEulerClass))

typedef struct _CpgIntegratorEuler			CpgIntegratorEuler;
typedef struct _CpgIntegratorEulerClass		CpgIntegratorEulerClass;
typedef struct _CpgIntegratorEulerPrivate	CpgIntegratorEulerPrivate;

struct _CpgIntegratorEuler {
	CpgIntegrator parent;
	
	CpgIntegratorEulerPrivate *priv;
};

struct _CpgIntegratorEulerClass {
	CpgIntegratorClass parent_class;
};

GType cpg_integrator_euler_get_type (void) G_GNUC_CONST;
CpgIntegratorEuler *cpg_integrator_euler_new (void);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_EULER_H__ */
