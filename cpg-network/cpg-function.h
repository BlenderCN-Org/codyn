/*
 * cpg-function.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_FUNCTION_H__
#define __CPG_FUNCTION_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-stack.h>
#include <cpg-network/cpg-utils.h>
#include <cpg-network/cpg-function-argument.h>
#include <cpg-network/cpg-expression.h>

#include <stdarg.h>

G_BEGIN_DECLS

#define CPG_TYPE_FUNCTION            (cpg_function_get_type ())
#define CPG_FUNCTION(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION, CpgFunction))
#define CPG_FUNCTION_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_FUNCTION, CpgFunction const))
#define CPG_FUNCTION_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_FUNCTION, CpgFunctionClass))
#define CPG_IS_FUNCTION(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_FUNCTION))
#define CPG_IS_FUNCTION_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_FUNCTION))
#define CPG_FUNCTION_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_FUNCTION, CpgFunctionClass))

typedef struct _CpgFunction         CpgFunction;
typedef struct _CpgFunctionClass    CpgFunctionClass;
typedef struct _CpgFunctionPrivate  CpgFunctionPrivate;

/**
 * CpgFunctionError:
 * @CPG_FUNCTION_ERROR_UNKNOWN: unknown
 * @CPG_FUNCTION_ERROR_ARGUMENT_NOT_FOUND: property not found
 * @CPG_FUNCTION_NUM_ERRORS: num errors
 *
 *
 **/
typedef enum
{
	CPG_FUNCTION_ERROR_UNKNOWN,
	CPG_FUNCTION_ERROR_ARGUMENT_NOT_FOUND,
	CPG_FUNCTION_NUM_ERRORS
} CpgFunctionError;

struct _CpgFunction
{
	/*< private >*/
	CpgObject parent;

	CpgFunctionPrivate *priv;
};

/**
 * CpgFunctionClass:
 * @evaluate: evaluate virtual function
 * @execute: execute virtual function
 *
 * The CpgFunction class
 *
 */
struct _CpgFunctionClass
{
	/*< private >*/
	CpgObjectClass parent_class;

	/*< public >*/
	gdouble (*evaluate)           (CpgFunction         *function);
	void    (*execute)            (CpgFunction         *function,
	                               CpgStack            *stack);

	/* signals */
	void   (*argument_added)      (CpgFunction         *function,
	                               CpgFunctionArgument *argument);

	void   (*argument_removed)    (CpgFunction         *function,
	                               CpgFunctionArgument *argument);

	void   (*arguments_reordered) (CpgFunction      *function);
};

GQuark               cpg_function_error_quark                 (void);

GType                cpg_function_get_type                    (void) G_GNUC_CONST;

CpgFunction         *cpg_function_new                         (const gchar          *name,
                                                               const gchar          *expression);

void                 cpg_function_add_argument                (CpgFunction          *function,
                                                               CpgFunctionArgument  *argument);

gboolean             cpg_function_remove_argument             (CpgFunction          *function,
                                                               CpgFunctionArgument  *argument,
                                                               GError              **error);

gboolean             cpg_function_clear_arguments             (CpgFunction          *function,
                                                               GError              **error);

const GList         *cpg_function_get_arguments               (CpgFunction          *function);
guint                cpg_function_get_n_optional              (CpgFunction          *function);
guint                cpg_function_get_n_arguments             (CpgFunction          *function);
void                 cpg_function_execute                     (CpgFunction          *function,
                                                               CpgStack             *stack);
void                 cpg_function_set_expression              (CpgFunction          *function,
                                                               CpgExpression        *expression);

CpgExpression       *cpg_function_get_expression              (CpgFunction          *function);

G_END_DECLS

#endif /* __CPG_FUNCTION_H__ */
