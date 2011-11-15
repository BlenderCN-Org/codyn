/*
 * cpg-operator.h
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

#ifndef __CPG_OPERATOR_H__
#define __CPG_OPERATOR_H__

#include <glib-object.h>
#include <cpg-network/cpg-stack.h>
#include <cpg-network/cpg-function.h>

G_BEGIN_DECLS

#define CPG_TYPE_OPERATOR		(cpg_operator_get_type ())
#define CPG_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR, CpgOperator))
#define CPG_OPERATOR_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_OPERATOR, CpgOperator const))
#define CPG_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_OPERATOR, CpgOperatorClass))
#define CPG_IS_OPERATOR(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_OPERATOR))
#define CPG_IS_OPERATOR_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_OPERATOR))
#define CPG_OPERATOR_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_OPERATOR, CpgOperatorClass))

typedef struct _CpgOperator		CpgOperator;
typedef struct _CpgOperatorClass	CpgOperatorClass;
typedef struct _CpgOperatorPrivate	CpgOperatorPrivate;

#define CPG_OPERATOR_ERROR (cpg_operator_error_quark ())

typedef enum
{
	CPG_OPERATOR_ERROR_UNSUPPORTED,
	CPG_OPERATOR_ERROR_INVALID
} CpgOperatorError;

GQuark            cpg_operator_error_quark      (void);

struct _CpgIntegrator;

typedef CPG_FORWARD_DECL (CpgFunction) CpgFunctionForward;

typedef void (*CpgForeachFunctionFunc)(CpgFunctionForward *func,
                                       gpointer            userdata);

struct _CpgOperator
{
	GObject parent;

	CpgOperatorPrivate *priv;
};

struct _CpgOperatorClass
{
	/*< private >*/
	GObjectClass parent_class;

	gchar *name;

	/*< public >*/
	void             (*execute)     (CpgOperator     *op,
	                                 CpgStack        *stack);

	gboolean         (*initialize) (CpgOperator   *op,
	                                GSList const **expressions,
	                                gint           num_expressions,
	                                GSList const **indices,
	                                gint           num_indices,
	                                gint           num_arguments,
	                                GError       **error);

	gchar           *(*get_name) ();

	void             (*reset_cache) (CpgOperator     *op);

	void             (*reset_variadic) (CpgOperator     *op);

	void             (*reset)           (CpgOperator *op);

	void             (*step)            (CpgOperator *op,
	                                     struct _CpgIntegrator *integrator,
	                                     gdouble      t,
	                                     gdouble      timestep);

	void             (*step_prepare)    (CpgOperator *op,
	                                     struct _CpgIntegrator *integrator,
	                                     gdouble      t,
	                                     gdouble      timestep);

	void             (*step_evaluate)   (CpgOperator *op,
	                                     struct _CpgIntegrator *integrator,
	                                     gdouble      t,
	                                     gdouble      timestep);

	gboolean         (*equal)           (CpgOperator *op,
	                                     CpgOperator *other);

	CpgFunctionForward *(*get_function)  (CpgOperator *op,
	                                      gint        *idx,
	                                      gint         numidx);

	void             (*foreach_function) (CpgOperator            *op,
	                                      CpgForeachFunctionFunc  func,
	                                      gpointer                userdata);

	CpgOperator     *(*copy)             (CpgOperator *src);
};

GType                cpg_operator_get_type                    (void) G_GNUC_CONST;

gboolean             cpg_operator_initialize                  (CpgOperator     *op,
                                                               GSList const   **expressions,
                                                               gint             num_expressions,
                                                               GSList const   **indices,
                                                               gint             num_indices,
                                                               gint             num_arguments,
                                                               GError         **error);

void                 cpg_operator_execute                     (CpgOperator     *op,
                                                               CpgStack        *stack);

void                 cpg_operator_reset_cache                 (CpgOperator     *op);
void                 cpg_operator_reset_variadic              (CpgOperator     *op);

gchar const         *cpg_operator_get_name                    (CpgOperator      *op);
gchar const         *cpg_operator_get_class_name              (CpgOperatorClass *op);

GSList const       **cpg_operator_all_expressions             (CpgOperator     *op);

GSList const        *cpg_operator_get_expressions             (CpgOperator     *op,
                                                               gint             idx);

gint                 cpg_operator_num_expressions             (CpgOperator     *op);

GSList const       **cpg_operator_all_indices                 (CpgOperator     *op);

GSList const        *cpg_operator_get_indices                 (CpgOperator     *op,
                                                               gint             idx);

gint                 cpg_operator_num_indices                 (CpgOperator     *op);

gboolean             cpg_operator_equal                       (CpgOperator     *op,
                                                               CpgOperator     *other);

void                 cpg_operator_step                        (CpgOperator     *op,
                                                               struct _CpgIntegrator *integrator,
                                                               gdouble          t,
                                                               gdouble          timestep);

void                 cpg_operator_step_prepare                (CpgOperator     *op,
                                                               struct _CpgIntegrator *integrator,
                                                               gdouble          t,
                                                               gdouble          timestep);

void                 cpg_operator_step_evaluate               (CpgOperator     *op,
                                                               struct _CpgIntegrator *integrator,
                                                               gdouble          t,
                                                               gdouble          timestep);

void                 cpg_operator_reset                       (CpgOperator     *op);

CpgOperator         *cpg_operator_copy                        (CpgOperator     *op);

gint                 cpg_operator_get_num_arguments           (CpgOperator     *op);
void                _cpg_operator_set_num_arguments           (CpgOperator     *op,
                                                               gint             num);

CpgFunction         *cpg_operator_get_primary_function        (CpgOperator     *op);

CpgFunction         *cpg_operator_get_function                (CpgOperator     *op,
                                                               gint            *idx,
                                                               gint             numidx);

void                 cpg_operator_foreach_function           (CpgOperator            *op,
                                                              CpgForeachFunctionFunc  func,
                                                              gpointer                userdata);

G_END_DECLS

#endif /* __CPG_OPERATOR_H__ */
