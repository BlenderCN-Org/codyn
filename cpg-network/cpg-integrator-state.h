#ifndef __CPG_INTEGRATOR_STATE_H__
#define __CPG_INTEGRATOR_STATE_H__

#include <glib-object.h>
#include <cpg-network/cpg-group.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_STATE            (cpg_integrator_state_get_type ())
#define CPG_INTEGRATOR_STATE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorState))
#define CPG_INTEGRATOR_STATE_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorState const))
#define CPG_INTEGRATOR_STATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorStateClass))
#define CPG_IS_INTEGRATOR_STATE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_STATE))
#define CPG_IS_INTEGRATOR_STATE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_STATE))
#define CPG_INTEGRATOR_STATE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorStateClass))

typedef struct _CpgIntegratorState        CpgIntegratorState;
typedef struct _CpgIntegratorStateClass   CpgIntegratorStateClass;
typedef struct _CpgIntegratorStatePrivate CpgIntegratorStatePrivate;

struct _CpgIntegratorState
{
	/*< private >*/
	GObject parent;

	CpgIntegratorStatePrivate *priv;
};

struct _CpgIntegratorStateClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType               cpg_integrator_state_get_type                (void) G_GNUC_CONST;

CpgIntegratorState *cpg_integrator_state_new                     (CpgObject *object);
CpgObject          *cpg_integrator_state_get_object              (CpgIntegratorState *state);

const GSList       *cpg_integrator_state_integrated_properties   (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_direct_properties       (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_all_properties          (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_integrated_link_actions (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_direct_link_actions     (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_inputs                  (CpgIntegratorState *state);

const GSList       *cpg_integrator_state_expressions             (CpgIntegratorState *state);

void                cpg_integrator_state_update                  (CpgIntegratorState *state);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_STATE_H__ */
