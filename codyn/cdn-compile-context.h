/*
 * cdn-compile-context.h
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

#ifndef __CDN_COMPILE_CONTEXT_H__
#define __CDN_COMPILE_CONTEXT_H__

#include <glib-object.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_COMPILE_CONTEXT		(cdn_compile_context_get_type ())
#define CDN_COMPILE_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_COMPILE_CONTEXT, CdnCompileContext))
#define CDN_COMPILE_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_COMPILE_CONTEXT, CdnCompileContext const))
#define CDN_COMPILE_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_COMPILE_CONTEXT, CdnCompileContextClass))
#define CDN_IS_COMPILE_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_COMPILE_CONTEXT))
#define CDN_IS_COMPILE_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_COMPILE_CONTEXT))
#define CDN_COMPILE_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_COMPILE_CONTEXT, CdnCompileContextClass))

typedef struct _CdnCompileContext		CdnCompileContext;
typedef struct _CdnCompileContextClass		CdnCompileContextClass;
typedef struct _CdnCompileContextPrivate	CdnCompileContextPrivate;

/**
 * CdnCompileContext:
 *
 * The expression compile context
 *
 * The compile context provides information for compiling expressions such
 * as the available user defined functions and the objects that can be used
 * to lookup properties used in the expression.
 */
struct _CdnCompileContext
{
	/*< private >*/
	GObject parent;

	CdnCompileContextPrivate *priv;
};

struct _CdnCompileContextClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_compile_context_get_type (void) G_GNUC_CONST;
CdnCompileContext *cdn_compile_context_new (void);

void cdn_compile_context_save (CdnCompileContext *context);
void cdn_compile_context_restore (CdnCompileContext *context);

void cdn_compile_context_prepend_object (CdnCompileContext *context,
                                         CdnObjectForward *object);

void cdn_compile_context_append_object (CdnCompileContext *context,
                                        CdnObjectForward *object);

void cdn_compile_context_prepend_function (CdnCompileContext *context,
                                           CdnFunctionForward *function);

void cdn_compile_context_append_function (CdnCompileContext *context,
                                          CdnFunctionForward *function);

void cdn_compile_context_set_function_ref_priority (CdnCompileContext *context,
                                                    gboolean           prio);

gboolean cdn_compile_context_get_function_ref_priority (CdnCompileContext *context);

void cdn_compile_context_set_function_arg_priority (CdnCompileContext *context,
                                                    gboolean           prio);

gboolean cdn_compile_context_get_function_arg_priority (CdnCompileContext *context);

CdnVariableForward *
cdn_compile_context_lookup_variable (CdnCompileContext *context,
                                     const gchar       *name);

CdnVariableForward *
cdn_compile_context_lookup_variable_last (CdnCompileContext *context,
                                          const gchar       *name);

CdnFunctionForward *
cdn_compile_context_lookup_function (CdnCompileContext *context,
                                     const gchar       *name);

const GSList *cdn_compile_context_get_objects (CdnCompileContext *context);
const GSList *cdn_compile_context_get_functions (CdnCompileContext *context);

void cdn_compile_context_set_only_local_variables (CdnCompileContext *context,
                                                   gboolean           only_local);

gboolean cdn_compile_context_get_only_local_variables (CdnCompileContext *context);


G_END_DECLS

#endif /* __CDN_COMPILE_CONTEXT_H__ */

