/*
 * cdn-operator-pdiff.h
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

#ifndef __CDN_OPERATOR_PDIFF_H__
#define __CDN_OPERATOR_PDIFF_H__

#include <codyn/operators/cdn-operator.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-function.h>

G_BEGIN_DECLS

#define CDN_TYPE_OPERATOR_PDIFF		(cdn_operator_pdiff_get_type ())
#define CDN_OPERATOR_PDIFF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_PDIFF, CdnOperatorPDiff))
#define CDN_OPERATOR_PDIFF_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_PDIFF, CdnOperatorPDiff const))
#define CDN_OPERATOR_PDIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_OPERATOR_PDIFF, CdnOperatorPDiffClass))
#define CDN_IS_OPERATOR_PDIFF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_OPERATOR_PDIFF))
#define CDN_IS_OPERATOR_PDIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_OPERATOR_PDIFF))
#define CDN_OPERATOR_PDIFF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_OPERATOR_PDIFF, CdnOperatorPDiffClass))

typedef struct _CdnOperatorPDiff		CdnOperatorPDiff;
typedef struct _CdnOperatorPDiffClass	CdnOperatorPDiffClass;
typedef struct _CdnOperatorPDiffPrivate	CdnOperatorPDiffPrivate;

struct _CdnOperatorPDiff
{
	/*< private >*/
	CdnOperator parent;

	CdnOperatorPDiffPrivate *priv;
};

struct _CdnOperatorPDiffClass
{
	/*< private >*/
	CdnOperatorClass parent_class;
};

GType               cdn_operator_pdiff_get_type          (void) G_GNUC_CONST;
CdnOperatorPDiff   *cdn_operator_pdiff_new               (void);

G_END_DECLS

#endif /* __CDN_OPERATOR_PDIFF_H__ */
