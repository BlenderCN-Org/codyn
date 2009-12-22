#ifndef __CPG_INTEGRATOR_H__
#define __CPG_INTEGRATOR_H__

#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR				(cpg_integrator_get_type ())
#define CPG_INTEGRATOR(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR, CpgIntegrator))
#define CPG_INTEGRATOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR, CpgIntegrator const))
#define CPG_INTEGRATOR_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR, CpgIntegratorClass))
#define CPG_IS_INTEGRATOR(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR))
#define CPG_IS_INTEGRATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR))
#define CPG_INTEGRATOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR, CpgIntegratorClass))

typedef struct _CpgIntegrator			CpgIntegrator;
typedef struct _CpgIntegratorClass		CpgIntegratorClass;
typedef struct _CpgIntegratorPrivate	CpgIntegratorPrivate;

typedef struct _CpgIntegratorState		CpgIntegratorState;

struct _CpgIntegrator {
	CpgObject parent;
	
	CpgIntegratorPrivate *priv;
};

struct _CpgIntegratorClass {
	CpgObjectClass parent_class;

	/* virtual functions */
	void	(*run)		(CpgIntegrator *integrator,
	                     GSList        *state,
	                     gdouble        from,
	                     gdouble        timestep,
	                     gdouble        to);

	gdouble	(*step)		(CpgIntegrator *integrator,
	                     GSList        *state,
	                     gdouble        t,
	                     gdouble        timestep);

	gchar const *(*get_name)	(CpgIntegrator *integrator);

	/* private field */
	gchar const *integrator_id;
};

GType				 cpg_integrator_get_type		(void) G_GNUC_CONST;
GType				 cpg_integrator_state_get_type	(void) G_GNUC_CONST;

CpgIntegratorState	*cpg_integrator_state_new		(CpgProperty *property);

void				 cpg_integrator_run				(CpgIntegrator 		*integrator,
													 GSList				*state,
													 gdouble			 from,
													 gdouble			 timestep,
													 gdouble			 to);

gdouble				 cpg_integrator_step			(CpgIntegrator		*integrator,
													 GSList				*state,
													 gdouble             t,
													 gdouble			 timestep);

void 				 cpg_integrator_evaluate		(CpgIntegrator		*integrator,
													 GSList				*state,
													 gdouble             t,
													 gdouble             timestep);

gchar const			*cpg_integrator_get_id			(CpgIntegrator 		*integrator);
gchar const			*cpg_integrator_get_name		(CpgIntegrator 		*integrator);

gdouble				 cpg_integrator_get_time		(CpgIntegrator		*integrator);
void				 cpg_integrator_reset			(CpgIntegrator		*integrator);

gdouble				 cpg_integrator_state_get_update	(CpgIntegratorState *state);
CpgProperty			*cpg_integrator_state_get_property	(CpgIntegratorState *state);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_H__ */
