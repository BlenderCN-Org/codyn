/*
 * cpg-operator-diff.h
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

#ifndef __CPG_OPERATOR_DIFF_H__
#define __CPG_OPERATOR_DIFF_H__

#include <cpg-network/cpg-operator.h>
#include <cpg-network/cpg-expression.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_DIFF		(cpg_operator_diff_get_type ())
#define CPG_OPERATOR_DIFF(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DIFF, CpgOperatorDiff))
#define CPG_OPERATOR_DIFF_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DIFF, CpgOperatorDiff const))
#define CPG_OPERATOR_DIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_DIFF, CpgOperatorDiffClass))
#define CPG_IS_OPERATOR_DIFF(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_DIFF))
#define CPG_IS_OPERATOR_DIFF_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_DIFF))
#define CPG_OPERATOR_DIFF_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_DIFF, CpgOperatorDiffClass))

typedef struct _CpgOperatorDiff		CpgOperatorDiff;
typedef struct _CpgOperatorDiffClass	CpgOperatorDiffClass;
typedef struct _CpgOperatorDiffPrivate	CpgOperatorDiffPrivate;

struct _CpgOperatorDiff
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorDiffPrivate *priv;
};

struct _CpgOperatorDiffClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType               cpg_operator_diff_get_type          (void) G_GNUC_CONST;
CpgOperatorDiff    *cpg_operator_diff_new               (void);

CpgExpression      *cpg_operator_diff_get_expression    (CpgOperatorDiff *diff);
CpgExpression      *cpg_operator_diff_get_derived       (CpgOperatorDiff *diff);
gint                cpg_operator_diff_get_order         (CpgOperatorDiff *diff);

G_END_DECLS

#endif /* __CPG_OPERATOR_DIFF_H__ */
