/*
 * cpg-operator-delayed.h
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

#ifndef __CPG_OPERATOR_DELAYED_H__
#define __CPG_OPERATOR_DELAYED_H__

#include <cpg-network/cpg-operator.h>
#include <cpg-network/cpg-expression.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_DELAYED		(cpg_operator_delayed_get_type ())
#define CPG_OPERATOR_DELAYED(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayed))
#define CPG_OPERATOR_DELAYED_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayed const))
#define CPG_OPERATOR_DELAYED_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayedClass))
#define CPG_IS_OPERATOR_DELAYED(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_DELAYED))
#define CPG_IS_OPERATOR_DELAYED_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_DELAYED))
#define CPG_OPERATOR_DELAYED_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_DELAYED, CpgOperatorDelayedClass))

typedef struct _CpgOperatorDelayed		CpgOperatorDelayed;
typedef struct _CpgOperatorDelayedClass		CpgOperatorDelayedClass;
typedef struct _CpgOperatorDelayedPrivate	CpgOperatorDelayedPrivate;

struct _CpgOperatorDelayed
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorDelayedPrivate *priv;
};

struct _CpgOperatorDelayedClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType               cpg_operator_delayed_get_type          (void) G_GNUC_CONST;
CpgOperatorDelayed *cpg_operator_delayed_new               (void);

CpgExpression      *cpg_operator_delayed_get_expression    (CpgOperatorDelayed *delayed);
CpgExpression      *cpg_operator_delayed_get_initial_value (CpgOperatorDelayed *delayed);
gdouble             cpg_operator_delayed_get_delay         (CpgOperatorDelayed *delayed);

G_END_DECLS

#endif /* __CPG_OPERATOR_DELAYED_H__ */
