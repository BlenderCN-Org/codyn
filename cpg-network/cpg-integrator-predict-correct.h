/*
 * cpg-integrator-predict-correct.h
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

struct _CpgIntegratorPredictCorrect
{
	/*< private >*/
	CpgIntegrator parent;
	
	CpgIntegratorPredictCorrectPrivate *priv;
};

struct _CpgIntegratorPredictCorrectClass
{
	/*< private >*/
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
