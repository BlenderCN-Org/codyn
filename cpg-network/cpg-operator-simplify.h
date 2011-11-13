/*
 * cpg-operator-simplify.h
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

#ifndef __CPG_OPERATOR_SIMPLIFY_H__
#define __CPG_OPERATOR_SIMPLIFY_H__

#include <cpg-network/cpg-operator.h>
#include <cpg-network/cpg-expression.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_SIMPLIFY		(cpg_operator_simplify_get_type ())
#define CPG_OPERATOR_SIMPLIFY(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_SIMPLIFY, CpgOperatorSimplify))
#define CPG_OPERATOR_SIMPLIFY_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_SIMPLIFY, CpgOperatorSimplify const))
#define CPG_OPERATOR_SIMPLIFY_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_SIMPLIFY, CpgOperatorSimplifyClass))
#define CPG_IS_OPERATOR_SIMPLIFY(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_SIMPLIFY))
#define CPG_IS_OPERATOR_SIMPLIFY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_SIMPLIFY))
#define CPG_OPERATOR_SIMPLIFY_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_SIMPLIFY, CpgOperatorSimplifyClass))

typedef struct _CpgOperatorSimplify		CpgOperatorSimplify;
typedef struct _CpgOperatorSimplifyClass	CpgOperatorSimplifyClass;
typedef struct _CpgOperatorSimplifyPrivate	CpgOperatorSimplifyPrivate;

struct _CpgOperatorSimplify
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorSimplifyPrivate *priv;
};

struct _CpgOperatorSimplifyClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType                cpg_operator_simplify_get_type          (void) G_GNUC_CONST;
CpgOperatorSimplify *cpg_operator_simplify_new               (void);

CpgExpression       *cpg_operator_simplify_get_expression    (CpgOperatorSimplify *simplify);
CpgExpression       *cpg_operator_simplify_get_derived       (CpgOperatorSimplify *simplify);

G_END_DECLS

#endif /* __CPG_OPERATOR_SIMPLIFY_H__ */
