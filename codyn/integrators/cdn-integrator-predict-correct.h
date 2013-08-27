/*
 * cdn-integrator-predict-correct.h
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

#ifndef __CDN_INTEGRATOR_PREDICT_CORRECT_H__
#define __CDN_INTEGRATOR_PREDICT_CORRECT_H__

#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR_PREDICT_CORRECT            (cdn_integrator_predict_correct_get_type ())
#define CDN_INTEGRATOR_PREDICT_CORRECT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, CdnIntegratorPredictCorrect))
#define CDN_INTEGRATOR_PREDICT_CORRECT_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, CdnIntegratorPredictCorrect const))
#define CDN_INTEGRATOR_PREDICT_CORRECT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, CdnIntegratorPredictCorrectClass))
#define CDN_IS_INTEGRATOR_PREDICT_CORRECT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT))
#define CDN_IS_INTEGRATOR_PREDICT_CORRECT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT))
#define CDN_INTEGRATOR_PREDICT_CORRECT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR_PREDICT_CORRECT, CdnIntegratorPredictCorrectClass))

typedef struct _CdnIntegratorPredictCorrect	        CdnIntegratorPredictCorrect;
typedef struct _CdnIntegratorPredictCorrectClass    CdnIntegratorPredictCorrectClass;
typedef struct _CdnIntegratorPredictCorrectPrivate  CdnIntegratorPredictCorrectPrivate;

/**
 * CdnIntegratorPredictCorrect:
 *
 * Prediction Correction integrator.
 *
 * The prediction correction integrator is a #CdnIntegrator subclass
 * implementing a prediction correction integration scheme. It supports multiple
 * orders of prediction and correction.
 */
struct _CdnIntegratorPredictCorrect
{
	/*< private >*/
	CdnIntegrator parent;
	
	CdnIntegratorPredictCorrectPrivate *priv;
};

struct _CdnIntegratorPredictCorrectClass
{
	/*< private >*/
	CdnIntegratorClass parent_class;
};

GType cdn_integrator_predict_correct_get_type (void) G_GNUC_CONST;
CdnIntegratorPredictCorrect *cdn_integrator_predict_correct_new (void);

guint cdn_integrator_predict_correct_get_prediction_order (CdnIntegratorPredictCorrect *pc);
guint cdn_integrator_predict_correct_get_correction_order (CdnIntegratorPredictCorrect *pc);
void cdn_integrator_predict_correct_set_prediction_order (CdnIntegratorPredictCorrect *pc, guint order);
void cdn_integrator_predict_correct_set_correction_order (CdnIntegratorPredictCorrect *pc, guint order);

G_END_DECLS

#endif /* __CDN_INTEGRATOR_PREDICT_CORRECT_H__ */
