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
typedef struct _CpgOperatorClassPrivate CpgOperatorClassPrivate;

struct _CpgIntegrator;

struct _CpgOperator
{
	GObject parent;

	CpgOperatorPrivate *priv;
};

struct _CpgOperatorClass
{
	/*< private >*/
	GObjectClass parent_class;

	CpgOperatorClassPrivate *priv;

	/*< public >*/
	void             (*execute)     (CpgOperator     *op,
	                                 CpgStack        *stack);

	void             (*initialize) (CpgOperator *op,
	                                GSList const *expressions);

	gchar           *(*get_name) ();

	gboolean         (*validate_num_arguments) (gint         num);

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
};

GType                cpg_operator_get_type                    (void) G_GNUC_CONST;

void                 cpg_operator_initialize                  (CpgOperator     *op,
                                                               GSList const    *expressions);

void                 cpg_operator_execute                     (CpgOperator     *op,
                                                               CpgStack        *stack);

void                 cpg_operator_reset_cache                 (CpgOperator     *op);
void                 cpg_operator_reset_variadic              (CpgOperator     *op);

gchar const         *cpg_operator_get_name                    (CpgOperator      *op);
gchar const         *cpg_operator_get_class_name              (CpgOperatorClass *op);
gboolean             cpg_operator_validate_num_arguments      (CpgOperatorClass *op,
                                                               gint             num);

GSList const        *cpg_operator_get_expressions             (CpgOperator     *op);

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

G_END_DECLS

#endif /* __CPG_OPERATOR_H__ */
