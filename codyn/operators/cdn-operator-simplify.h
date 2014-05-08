/*
 * cdn-operator-simplify.h
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

#ifndef __CDN_OPERATOR_SIMPLIFY_H__
#define __CDN_OPERATOR_SIMPLIFY_H__

#include <codyn/operators/cdn-operator.h>
#include <codyn/cdn-expression.h>

G_BEGIN_DECLS

#define CDN_TYPE_OPERATOR_SIMPLIFY		(cdn_operator_simplify_get_type ())
#define CDN_OPERATOR_SIMPLIFY(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_SIMPLIFY, CdnOperatorSimplify))
#define CDN_OPERATOR_SIMPLIFY_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_OPERATOR_SIMPLIFY, CdnOperatorSimplify const))
#define CDN_OPERATOR_SIMPLIFY_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_OPERATOR_SIMPLIFY, CdnOperatorSimplifyClass))
#define CDN_IS_OPERATOR_SIMPLIFY(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_OPERATOR_SIMPLIFY))
#define CDN_IS_OPERATOR_SIMPLIFY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_OPERATOR_SIMPLIFY))
#define CDN_OPERATOR_SIMPLIFY_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_OPERATOR_SIMPLIFY, CdnOperatorSimplifyClass))

typedef struct _CdnOperatorSimplify		CdnOperatorSimplify;
typedef struct _CdnOperatorSimplifyClass	CdnOperatorSimplifyClass;
typedef struct _CdnOperatorSimplifyPrivate	CdnOperatorSimplifyPrivate;

/**
 * CdnOperatorSimplify:
 *
 * Math operator for simplify evaluation of an expression.
 *
 * The #CdnOperatorSimplify is a special operator that can be used in
 * mathematical expressions ('delay'). When evaluated, it will return the
 * simplify value of its argument (which can be an arbitrary expression).
 */
struct _CdnOperatorSimplify
{
	/*< private >*/
	CdnOperator parent;

	CdnOperatorSimplifyPrivate *priv;
};

struct _CdnOperatorSimplifyClass
{
	/*< private >*/
	CdnOperatorClass parent_class;
};

GType                cdn_operator_simplify_get_type          (void) G_GNUC_CONST;
CdnOperatorSimplify *cdn_operator_simplify_new               (void);

G_END_DECLS

#endif /* __CDN_OPERATOR_SIMPLIFY_H__ */
