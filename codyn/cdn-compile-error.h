/*
 * cdn-compile-error.h
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

#ifndef __CDN_COMPILE_ERROR_H__
#define __CDN_COMPILE_ERROR_H__

#include <glib-object.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-edge.h>

G_BEGIN_DECLS

#define CDN_TYPE_COMPILE_ERROR			(cdn_compile_error_get_type ())
#define CDN_COMPILE_ERROR(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_COMPILE_ERROR, CdnCompileError))
#define CDN_COMPILE_ERROR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_COMPILE_ERROR, CdnCompileError const))
#define CDN_COMPILE_ERROR_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_COMPILE_ERROR, CdnCompileErrorClass))
#define CDN_IS_COMPILE_ERROR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_COMPILE_ERROR))
#define CDN_IS_COMPILE_ERROR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_COMPILE_ERROR))
#define CDN_COMPILE_ERROR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_COMPILE_ERROR, CdnCompileErrorClass))

typedef struct _CdnCompileError		CdnCompileError;
typedef struct _CdnCompileErrorClass	CdnCompileErrorClass;
typedef struct _CdnCompileErrorPrivate	CdnCompileErrorPrivate;

struct _CdnCompileError
{
	GObject parent;

	CdnCompileErrorPrivate *priv;
};

struct _CdnCompileErrorClass
{
	GObjectClass parent_class;
};

#define CDN_COMPILE_ERROR_TYPE (cdn_compile_error_type_quark ())

/**
 * CdnCompileErrorCode:
 * @CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND: variable not found
 * @CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND: function not found
 * @CDN_COMPILE_ERROR_OPERATOR_NOT_FOUND: operator not found
 * @CDN_COMPILE_ERROR_INVALID_TOKEN: invalid token
 * @CDN_COMPILE_ERROR_MAXARG: maximum number of arguments exceeded
 * @CDN_COMPILE_ERROR_INVALID_STACK: invalid stack produced
 * @CDN_COMPILE_ERROR_VARIABLE_RECURSE: variable recurses on itself
 * @CDN_COMPILE_ERROR_INVALID_ARGUMENTS: invalid arguments to function
 * @CDN_COMPILE_ERROR_NUM_ERRORS: num errors
 *
 * Enum used to indicate the type of compile error
 *
 **/
typedef enum
{
	CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
	CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND,
	CDN_COMPILE_ERROR_OPERATOR_NOT_FOUND,
	CDN_COMPILE_ERROR_INVALID_TOKEN,
	CDN_COMPILE_ERROR_MAXARG,
	CDN_COMPILE_ERROR_INVALID_STACK,
	CDN_COMPILE_ERROR_VARIABLE_RECURSE,
	CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
	CDN_COMPILE_ERROR_NUM_ERRORS
} CdnCompileErrorCode;

GType            cdn_compile_error_get_type         (void) G_GNUC_CONST;
CdnCompileError *cdn_compile_error_new              (void);

GQuark           cdn_compile_error_type_quark       (void);

void             cdn_compile_error_set              (CdnCompileError *error,
                                                     GError          *gerror,
                                                     CdnObject       *object,
                                                     CdnVariable     *property,
                                                     CdnEdgeAction   *action,
                                                     CdnExpression   *expression);

GError           *cdn_compile_error_get_error       (CdnCompileError *error);
CdnObject        *cdn_compile_error_get_object      (CdnCompileError *error);
CdnVariable      *cdn_compile_error_get_variable    (CdnCompileError *error);
CdnEdgeAction    *cdn_compile_error_get_edge_action (CdnCompileError *error);
CdnExpression    *cdn_compile_error_get_expression  (CdnCompileError *error);

const gchar      *cdn_compile_error_string          (CdnCompileError *error);
const gchar      *cdn_compile_error_code_string     (gint             code);
gint              cdn_compile_error_get_code        (CdnCompileError *error);
const gchar      *cdn_compile_error_get_message     (CdnCompileError *error);

gchar *           cdn_compile_error_get_formatted_string (CdnCompileError *error);

G_END_DECLS

#endif /* __CDN_COMPILE_ERROR_H__ */


