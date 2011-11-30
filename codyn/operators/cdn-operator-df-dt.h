/*
 * cdn-operator-df-dt.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_OPERATOR_DF_DT_H__
#define __CDN_OPERATOR_DF_DT_H__

#include <codyn/operators/cdn-operator.h>
#include <codyn/cdn-expression.h>

G_BEGIN_DECLS

#define CDN_TYPE_OPERATOR_DF_DT		(cdn_operator_df_dt_get_type ())
#define CDN_OPERATOR_DF_DT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_DF_DT, CdnOperatorDfDt))
#define CDN_OPERATOR_DF_DT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_DF_DT, CdnOperatorDfDt const))
#define CDN_OPERATOR_DF_DT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_OPERATOR_DF_DT, CdnOperatorDfDtClass))
#define CDN_IS_OPERATOR_DF_DT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_OPERATOR_DF_DT))
#define CDN_IS_OPERATOR_DF_DT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_OPERATOR_DF_DT))
#define CDN_OPERATOR_DF_DT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_OPERATOR_DF_DT, CdnOperatorDfDtClass))

typedef struct _CdnOperatorDfDt		CdnOperatorDfDt;
typedef struct _CdnOperatorDfDtClass	CdnOperatorDfDtClass;
typedef struct _CdnOperatorDfDtPrivate	CdnOperatorDfDtPrivate;

struct _CdnOperatorDfDt
{
	/*< private >*/
	CdnOperator parent;

	CdnOperatorDfDtPrivate *priv;
};

struct _CdnOperatorDfDtClass
{
	/*< private >*/
	CdnOperatorClass parent_class;
};

GType               cdn_operator_df_dt_get_type          (void) G_GNUC_CONST;
CdnOperatorDfDt    *cdn_operator_df_dt_new               (void);

CdnExpression      *cdn_operator_df_dt_get_expression    (CdnOperatorDfDt *df_dt);
CdnExpression      *cdn_operator_df_dt_get_derived       (CdnOperatorDfDt *df_dt);
gint                cdn_operator_df_dt_get_order         (CdnOperatorDfDt *df_dt);

G_END_DECLS

#endif /* __CDN_OPERATOR_DF_DT_H__ */
