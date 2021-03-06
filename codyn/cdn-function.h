/*
 * cdn-function.h
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

#ifndef __CDN_FUNCTION_H__
#define __CDN_FUNCTION_H__

#include <codyn/cdn-object.h>
#include <codyn/cdn-stack.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-function-argument.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-expression-tree-iter.h>

#include <stdarg.h>

G_BEGIN_DECLS

#define CDN_TYPE_FUNCTION            (cdn_function_get_type ())
#define CDN_FUNCTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION, CdnFunction))
#define CDN_FUNCTION_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_FUNCTION, CdnFunction const))
#define CDN_FUNCTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_FUNCTION, CdnFunctionClass))
#define CDN_IS_FUNCTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_FUNCTION))
#define CDN_IS_FUNCTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_FUNCTION))
#define CDN_FUNCTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_FUNCTION, CdnFunctionClass))

typedef struct _CdnFunction         CdnFunction;
typedef struct _CdnFunctionClass    CdnFunctionClass;
typedef struct _CdnFunctionPrivate  CdnFunctionPrivate;

/**
 * CdnFunctionError:
 * @CDN_FUNCTION_ERROR_UNKNOWN: unknown
 * @CDN_FUNCTION_ERROR_ARGUMENT_NOT_FOUND: argument not found
 * @CDN_FUNCTION_NUM_ERRORS: num errors
 *
 * Function error codes.
 */
typedef enum
{
	CDN_FUNCTION_ERROR_UNKNOWN,
	CDN_FUNCTION_ERROR_ARGUMENT_NOT_FOUND,
	CDN_FUNCTION_NUM_ERRORS
} CdnFunctionError;

/**
 * CdnFunction:
 *
 * User defined function.
 *
 * It is possible to define custom user functions in the network which can
 * then be used from any expression. This class provides the basic
 * user function functionality. User defined functions can reference global
 * constants as well as use other user defined functions in their expressions.
 *
 * The #CdnFunction class can be subclassed to provide more specific types
 * of functions. One such example is the #CdnFunctionPolynomial class which
 * can be used to define and evaluate piecewise polynomials.
 *
 * <refsect2 id="CdnFunction-COPY">
 * <title>CdnFunction Copy Semantics</title>
 * When a function is copied with #cdn_object_copy, the function expression
 * and all the arguments are copied as well.
 * </refsect2>
 */
struct _CdnFunction
{
	/*< private >*/
	CdnObject parent;

	CdnFunctionPrivate *priv;
};

/**
 * CdnFunctionClass:
 * @evaluate: evaluate virtual function
 * @execute: execute virtual function
 *
 * The CdnFunction class
 *
 */
struct _CdnFunctionClass
{
	/*< private >*/
	CdnObjectClass parent_class;

	/*< public >*/
	void    (*evaluate)           (CdnFunction         *function,
	                               CdnStack            *stack);

	void    (*execute)            (CdnFunction         *function,
	                               CdnStack            *stack);

	CdnStackManipulation const *
	        (*get_stack_manipulation) (CdnFunction      *function);

	CdnFunction *(*for_dimension) (CdnFunction          *function,
	                               CdnStackArgs const   *argdim);

	CdnFunction *(*get_derivative) (CdnFunction                       *function,
	                                GSList                            *towards,
	                                gint                               order,
	                                CdnExpressionTreeIterDeriveFlags   flags,
	                                GError                           **error);

	/* signals */
	void   (*argument_added)      (CdnFunction         *function,
	                               CdnFunctionArgument *argument);

	void   (*argument_removed)    (CdnFunction         *function,
	                               CdnFunctionArgument *argument);

	void   (*arguments_reordered) (CdnFunction      *function);
};

GQuark               cdn_function_error_quark                 (void);

GType                cdn_function_get_type                    (void) G_GNUC_CONST;

CdnFunction         *cdn_function_new                         (const gchar   *name,
                                                               CdnExpression *expression);

void                 cdn_function_add_argument                (CdnFunction          *function,
                                                               CdnFunctionArgument  *argument);

gboolean             cdn_function_remove_argument             (CdnFunction          *function,
                                                               CdnFunctionArgument  *argument,
                                                               GError              **error);

gboolean             cdn_function_clear_arguments             (CdnFunction          *function,
                                                               GError              **error);

CdnFunctionArgument *cdn_function_get_argument                (CdnFunction          *function,
                                                               gchar const          *name);

const GList         *cdn_function_get_arguments               (CdnFunction          *function);
guint                cdn_function_get_n_arguments             (CdnFunction          *function);
guint                cdn_function_get_n_implicit              (CdnFunction          *function);
guint                cdn_function_get_n_optional              (CdnFunction          *function);

void                 cdn_function_execute                     (CdnFunction          *function,
                                                               CdnStack             *stack);

void                 cdn_function_set_expression              (CdnFunction          *function,
                                                               CdnExpression        *expression);

CdnExpression       *cdn_function_get_expression              (CdnFunction          *function);
GSList const        *cdn_function_get_dependencies            (CdnFunction          *function);

CdnStackManipulation const *
                     cdn_function_get_stack_manipulation      (CdnFunction          *function);

CdnFunction         *cdn_function_for_dimension               (CdnFunction          *function,
                                                               CdnStackArgs const   *argdim);

CdnFunction         *cdn_function_get_derivative              (CdnFunction                       *function,
                                                               GSList                            *towards,
                                                               gint                               order,
                                                               CdnExpressionTreeIterDeriveFlags   flags,
                                                               GError                           **error);

gboolean             cdn_function_is_pure                     (CdnFunction          *function);

void                _cdn_function_set_arguments_dimension     (CdnFunction          *function,
                                                               CdnStackArgs const   *argdim);

void                cdn_function_set_rand_as_argument         (gboolean              rand_as_argument);
GSList const       *_cdn_function_get_rand_arguments          (CdnFunction          *function);
void                _cdn_function_expression_changed          (CdnFunction          *function);

G_END_DECLS

#endif /* __CDN_FUNCTION_H__ */
