/*
 * cpg-operator-pdiff.h
 * This file is part of cpg-network
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

#ifndef __CPG_OPERATOR_PDIFF_H__
#define __CPG_OPERATOR_PDIFF_H__

#include <cpg-network/cpg-operator.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_PDIFF		(cpg_operator_pdiff_get_type ())
#define CPG_OPERATOR_PDIFF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_PDIFF, CpgOperatorPDiff))
#define CPG_OPERATOR_PDIFF_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_PDIFF, CpgOperatorPDiff const))
#define CPG_OPERATOR_PDIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_PDIFF, CpgOperatorPDiffClass))
#define CPG_IS_OPERATOR_PDIFF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_PDIFF))
#define CPG_IS_OPERATOR_PDIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_PDIFF))
#define CPG_OPERATOR_PDIFF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_PDIFF, CpgOperatorPDiffClass))

typedef struct _CpgOperatorPDiff		CpgOperatorPDiff;
typedef struct _CpgOperatorPDiffClass	CpgOperatorPDiffClass;
typedef struct _CpgOperatorPDiffPrivate	CpgOperatorPDiffPrivate;

struct _CpgOperatorPDiff
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorPDiffPrivate *priv;
};

struct _CpgOperatorPDiffClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType               cpg_operator_pdiff_get_type          (void) G_GNUC_CONST;
CpgOperatorPDiff   *cpg_operator_pdiff_new               (void);

CpgExpression      *cpg_operator_pdiff_get_expression    (CpgOperatorPDiff *pdiff);
CpgExpression      *cpg_operator_pdiff_get_derived       (CpgOperatorPDiff *pdiff);
gint                cpg_operator_pdiff_get_order         (CpgOperatorPDiff *pdiff);
CpgFunction        *cpg_operator_pdiff_get_function      (CpgOperatorPDiff *pdiff);

G_END_DECLS

#endif /* __CPG_OPERATOR_PDIFF_H__ */
