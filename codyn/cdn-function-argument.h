/*
 * cdn-function-argument.h
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

#ifndef __CDN_FUNCTION_ARGUMENT_H__
#define __CDN_FUNCTION_ARGUMENT_H__

#include <glib-object.h>
#include <codyn/cdn-variable.h>

G_BEGIN_DECLS

#define CDN_TYPE_FUNCTION_ARGUMENT		(cdn_function_argument_get_type ())
#define CDN_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_ARGUMENT, CdnFunctionArgument))
#define CDN_FUNCTION_ARGUMENT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION_ARGUMENT, CdnFunctionArgument const))
#define CDN_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_FUNCTION_ARGUMENT, CdnFunctionArgumentClass))
#define CDN_IS_FUNCTION_ARGUMENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_FUNCTION_ARGUMENT))
#define CDN_IS_FUNCTION_ARGUMENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_FUNCTION_ARGUMENT))
#define CDN_FUNCTION_ARGUMENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_FUNCTION_ARGUMENT, CdnFunctionArgumentClass))

typedef struct _CdnFunctionArgument		CdnFunctionArgument;
typedef struct _CdnFunctionArgumentClass	CdnFunctionArgumentClass;
typedef struct _CdnFunctionArgumentPrivate	CdnFunctionArgumentPrivate;

struct _CdnFunctionArgument
{
	/*< private >*/
	GInitiallyUnowned parent;

	CdnFunctionArgumentPrivate *priv;
};

struct _CdnFunctionArgumentClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;

	/*< public >*/

	/* signals */
	gboolean (*invalidate_name) (CdnFunctionArgument *argument,
	                             const gchar         *name);
};

GType                cdn_function_argument_get_type           (void) G_GNUC_CONST;

CdnFunctionArgument *cdn_function_argument_new                (const gchar         *name,
                                                               gboolean             isexplicit,
                                                               CdnExpression       *default_value);

CdnFunctionArgument *cdn_function_argument_copy               (CdnFunctionArgument *argument);

const gchar         *cdn_function_argument_get_name           (CdnFunctionArgument *argument);
gboolean             cdn_function_argument_set_name           (CdnFunctionArgument *argument,
                                                               const gchar         *name);

gboolean             cdn_function_argument_get_explicit       (CdnFunctionArgument *argument);
void                 cdn_function_argument_set_explicit       (CdnFunctionArgument *argument,
                                                               gboolean             isexplicit);

gboolean             cdn_function_argument_get_optional       (CdnFunctionArgument *argument);

CdnExpression       *cdn_function_argument_get_default_value  (CdnFunctionArgument *argument);

void                 cdn_function_argument_set_default_value  (CdnFunctionArgument *argument,
                                                               CdnExpression       *value);

void                 _cdn_function_argument_set_variable      (CdnFunctionArgument *argument,
                                                               CdnVariable         *property);

CdnVariable         *_cdn_function_argument_get_variable      (CdnFunctionArgument *argument);

void                  cdn_function_argument_get_dimension     (CdnFunctionArgument *argument,
                                                               gint                *numr,
                                                               gint                *numc);

void                  cdn_function_argument_set_dimension     (CdnFunctionArgument *argument,
                                                               gint                 numr,
                                                               gint                 numc);
G_END_DECLS

#endif /* __CDN_FUNCTION_ARGUMENT_H__ */
