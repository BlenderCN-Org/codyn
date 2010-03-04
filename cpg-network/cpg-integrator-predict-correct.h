#ifndef __CPG_INTEGRATOR_PREDICT_CORRECT_H__
#define __CPG_INTEGRATOR_PREDICT_CORRECT_H__

#include <cpg-network/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_PREDICT_CORRECT            (cpg_integrator_predict_correct_get_type ())
#define CPG_INTEGRATOR_PREDICT_CORRECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, CpgIntegratorPredictCorrect))
#define CPG_INTEGRATOR_PREDICT_CORRECT_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, CpgIntegratorPredictCorrect const))
#define CPG_INTEGRATOR_PREDICT_CORRECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, CpgIntegratorPredictCorrectClass))
#define CPG_IS_INTEGRATOR_PREDICT_CORRECT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT))
#define CPG_IS_INTEGRATOR_PREDICT_CORRECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT))
#define CPG_INTEGRATOR_PREDICT_CORRECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_PREDICT_CORRECT, CpgIntegratorPredictCorrectClass))

typedef struct _CpgIntegratorPredictCorrect	        CpgIntegratorPredictCorrect;
typedef struct _CpgIntegratorPredictCorrectClass    CpgIntegratorPredictCorrectClass;
typedef struct _CpgIntegratorPredictCorrectPrivate  CpgIntegratorPredictCorrectPrivate;

struct _CpgIntegratorPredictCorrect {
	CpgIntegrator parent;
	
	CpgIntegratorPredictCorrectPrivate *priv;
};

struct _CpgIntegratorPredictCorrectClass {
	CpgIntegratorClass parent_class;
};

GType cpg_integrator_predict_correct_get_type (void) G_GNUC_CONST;
CpgIntegratorPredictCorrect *cpg_integrator_predict_correct_new (void);

guint cpg_integrator_predict_correct_get_prediction_order (CpgIntegratorPredictCorrect *pc);
guint cpg_integrator_predict_correct_get_correction_order (CpgIntegratorPredictCorrect *pc);
void cpg_integrator_predict_correct_set_prediction_order (CpgIntegratorPredictCorrect *pc, guint order);
void cpg_integrator_predict_correct_set_correction_order (CpgIntegratorPredictCorrect *pc, guint order);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_PREDICT_CORRECT_H__ */
