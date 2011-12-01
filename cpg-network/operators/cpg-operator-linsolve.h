/*
 * cpg-operator-linsolve.h
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

#ifndef __CPG_OPERATOR_LINSOLVE_H__
#define __CPG_OPERATOR_LINSOLVE_H__

#include <cpg-network/operators/cpg-operator.h>
#include <cpg-network/cpg-expression.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR_LINSOLVE		(cpg_operator_linsolve_get_type ())
#define CPG_OPERATOR_LINSOLVE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_LINSOLVE, CpgOperatorLinsolve))
#define CPG_OPERATOR_LINSOLVE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR_LINSOLVE, CpgOperatorLinsolve const))
#define CPG_OPERATOR_LINSOLVE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR_LINSOLVE, CpgOperatorLinsolveClass))
#define CPG_IS_OPERATOR_LINSOLVE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR_LINSOLVE))
#define CPG_IS_OPERATOR_LINSOLVE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR_LINSOLVE))
#define CPG_OPERATOR_LINSOLVE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR_LINSOLVE, CpgOperatorLinsolveClass))

typedef struct _CpgOperatorLinsolve		CpgOperatorLinsolve;
typedef struct _CpgOperatorLinsolveClass	CpgOperatorLinsolveClass;
typedef struct _CpgOperatorLinsolvePrivate	CpgOperatorLinsolvePrivate;

struct _CpgOperatorLinsolve
{
	/*< private >*/
	CpgOperator parent;

	CpgOperatorLinsolvePrivate *priv;
};

struct _CpgOperatorLinsolveClass
{
	/*< private >*/
	CpgOperatorClass parent_class;
};

GType                cpg_operator_linsolve_get_type          (void) G_GNUC_CONST;
CpgOperatorLinsolve *cpg_operator_linsolve_new               (void);

G_END_DECLS

#endif /* __CPG_OPERATOR_LINSOLVE_H__ */
