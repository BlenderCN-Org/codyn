#ifndef __CPG_INTEGRATOR_H__
#define __CPG_INTEGRATOR_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-utils.h>
#include <cpg-network/cpg-integrator-state.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR            (cpg_integrator_get_type ())
#define CPG_INTEGRATOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR, CpgIntegrator))
#define CPG_INTEGRATOR_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR, CpgIntegrator const))
#define CPG_INTEGRATOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR, CpgIntegratorClass))
#define CPG_IS_INTEGRATOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR))
#define CPG_IS_INTEGRATOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR))
#define CPG_INTEGRATOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR, CpgIntegratorClass))

typedef struct _CpgIntegrator        CpgIntegrator;
typedef struct _CpgIntegratorClass   CpgIntegratorClass;
typedef struct _CpgIntegratorPrivate CpgIntegratorPrivate;

struct _CpgIntegrator
{
	/*< private >*/
	CpgObject parent;

	CpgIntegratorPrivate *priv;
};

/**
 * CpgIntegratorClass:
 * @run: run virtual function
 * @step: step virtual function
 * @get_name: get_name virtual function
 * @reset: reset virtual function
 * @integrator_id: the integrator id
 *
 * The CpgIntegrator class
 *
 */
struct _CpgIntegratorClass
{
	/*< private >*/
	CpgObjectClass parent_class;

	/*< public >*/
	void         (*run)      (CpgIntegrator *integrator,
	                          gdouble        from,
	                          gdouble        timestep,
	                          gdouble        to);

	gdouble      (*step)     (CpgIntegrator *integrator,
	                          gdouble        t,
	                          gdouble        timestep);

	gchar const *(*get_name) (CpgIntegrator *integrator);

	void         (*reset)     (CpgIntegrator *integrator);

	/* private field */
	gchar const *integrator_id;
};

GType                cpg_integrator_get_type        (void) G_GNUC_CONST;

CpgIntegratorState  *cpg_integrator_get_state       (CpgIntegrator *integrator);
void                 cpg_integrator_set_state       (CpgIntegrator *integrator,
                                                     CpgIntegratorState *state);

void                 cpg_integrator_run             (CpgIntegrator *integrator,
                                                     gdouble        from,
                                                     gdouble        timestep,
                                                     gdouble        to);

gdouble              cpg_integrator_step            (CpgIntegrator *integrator,
                                                     gdouble        t,
                                                     gdouble        timestep);

void                 cpg_integrator_evaluate        (CpgIntegrator *integrator,
                                                     gdouble        t,
                                                     gdouble        timestep);

void                 cpg_integrator_reset           (CpgIntegrator *integrator);

gchar const         *cpg_integrator_get_name        (CpgIntegrator *integrator);

gdouble              cpg_integrator_get_time        (CpgIntegrator *integrator);


CpgObject           *cpg_integrator_get_object        (CpgIntegrator *integrator);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_H__ */
