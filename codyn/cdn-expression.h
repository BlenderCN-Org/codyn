/*
 * cdn-expression.h
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

#ifndef __CDN_EXPRESSION_H__
#define __CDN_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-forward-decl.h>
#include <codyn/cdn-stack.h>
#include <codyn/cdn-matrix.h>

G_BEGIN_DECLS

#define CDN_TYPE_EXPRESSION             (cdn_expression_get_type ())
#define CDN_EXPRESSION(obj)             (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EXPRESSION, CdnExpression))
#define CDN_EXPRESSION_CONST(obj)       (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EXPRESSION, CdnExpression const))
#define CDN_EXPRESSION_CLASS(klass)     (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EXPRESSION, CdnExpressionClass))
#define CDN_IS_EXPRESSION(obj)          (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EXPRESSION))
#define CDN_IS_EXPRESSION_CLASS(klass)  (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EXPRESSION))
#define CDN_EXPRESSION_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EXPRESSION, CdnExpressionClass))

typedef struct _CdnExpression		CdnExpression;
typedef struct _CdnExpressionClass	CdnExpressionClass;
typedef struct _CdnExpressionPrivate	CdnExpressionPrivate;

typedef void (*CdnExpressionCacheNotify) (CdnExpression *expression,
                                          gpointer       userdata);

typedef void (*CdnExpressionEvaluateNotify) (CdnExpression *expression,
                                             gpointer       userdata);

/**
 * CdnExpression:
 *
 * Mathematical expression evaluation.
 *
 * A #CdnExpression contains a mathematical expression. The expression in
 * string format can be compiled and evaluated. At the compilation phase,
 * a list of #CdnObject is provided as a context in which variables are mapped
 * to #CdnVariable in this context.
 */
struct _CdnExpression
{
	/*< private >*/
	GInitiallyUnowned parent;

	CdnExpressionPrivate *priv;
};

struct _CdnExpressionClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;
};

GType          cdn_expression_get_type         (void) G_GNUC_CONST;

CdnExpression *cdn_expression_new              (const gchar        *expression);
CdnExpression *cdn_expression_new0             ();

CdnExpression *cdn_expression_new_number       (gdouble             number);

CdnExpression *cdn_expression_copy             (CdnExpression      *expression);

gboolean       cdn_expression_depends_on       (CdnExpression      *expression,
                                                CdnExpression      *depends_on);

const GSList  *cdn_expression_get_dependencies   (CdnExpression      *expression);
const GSList  *cdn_expression_get_depends_on_me (CdnExpression      *expression);
GSList        *cdn_expression_get_variable_dependencies (CdnExpression *expression);

const gchar   *cdn_expression_get_as_string    (CdnExpression      *expression);

gboolean       cdn_expression_compile          (CdnExpression      *expression,
                                                CdnCompileContext  *context,
                                                CdnCompileErrorForward    *error);

gdouble        cdn_expression_evaluate         (CdnExpression      *expression);

CdnMatrix const *cdn_expression_evaluate_values  (CdnExpression      *expression);

void           cdn_expression_set_value        (CdnExpression      *expression,
                                                gdouble             value);

void           cdn_expression_set_values       (CdnExpression      *expression,
                                                CdnMatrix const    *values);

gboolean       cdn_expression_is_cached        (CdnExpression      *expression);

void           cdn_expression_reset            (CdnExpression      *expression);

gboolean       cdn_expression_equal            (CdnExpression      *expression,
                                                CdnExpression      *other,
                                                gboolean            asstring);

void           cdn_expression_set_from_string  (CdnExpression      *expression,
                                                const gchar        *value);

void           cdn_expression_reset_cache      (CdnExpression      *expression);
void           cdn_expression_force_reset_cache (CdnExpression *expression);
void          _cdn_expression_reset_rand_cache (CdnExpression *expression);

gboolean       cdn_expression_get_has_cache    (CdnExpression      *expression);
void           cdn_expression_set_has_cache    (CdnExpression      *expression,
                                                gboolean            cache);

const GSList  *cdn_expression_get_instructions (CdnExpression const *expression);
void           cdn_expression_set_instructions (CdnExpression      *expression,
                                                const GSList       *instructions);

void           cdn_expression_set_instructions_take (CdnExpression      *expression,
                                                     GSList             *instructions);

const GSList  *cdn_expression_get_rand_instructions (CdnExpression      *expression);

gboolean       cdn_expression_get_once         (CdnExpression      *expression);
void           cdn_expression_set_once         (CdnExpression      *expression,
                                                gboolean            instant);

gint           cdn_expression_get_error_at     (CdnExpression      *expression);
gint           cdn_expression_get_error_start  (CdnExpression      *expression);

gboolean       cdn_expression_get_dimension    (CdnExpression      *expression,
                                                CdnDimension       *dimension);

CdnStackArg   *cdn_expression_get_stack_arg    (CdnExpression      *expression);
void           cdn_expression_recalculate_sparsity (CdnExpression *expression);

void           cdn_expression_set_cache_notify (CdnExpression            *expression,
                                                CdnExpressionCacheNotify  notify,
                                                gpointer                  userdata,
                                                GDestroyNotify            destroy_notify);

void           cdn_expression_set_evaluate_notify (CdnExpression            *expression,
                                                   CdnExpressionEvaluateNotify  notify,
                                                   gpointer                  userdata,
                                                   GDestroyNotify            destroy_notify);

guint          cdn_expression_get_stack_size   (CdnExpression       *expression);

void           cdn_expression_set_pinned_sparsity (CdnExpression *expression,
                                                   gboolean       pinned);

gboolean       cdn_expression_get_pinned_sparsity (CdnExpression *expression);

void          _cdn_expression_transfer_dependencies (CdnExpression *expression,
                                                     CdnExpression *transfer_to);

CdnExpression *cdn_expression_sum                (GSList const *expressions);

G_END_DECLS

#endif /* __CDN_EXPRESSION_H__ */
