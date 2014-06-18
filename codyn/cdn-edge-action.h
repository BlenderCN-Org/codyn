/*
 * cdn-edge-action.h
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

#ifndef __CDN_EDGE_ACTION_H__
#define __CDN_EDGE_ACTION_H__

#include <glib-object.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-modifiable.h>
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_EDGE_ACTION		(cdn_edge_action_get_type ())
#define CDN_EDGE_ACTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EDGE_ACTION, CdnEdgeAction))
#define CDN_EDGE_ACTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EDGE_ACTION, CdnEdgeAction const))
#define CDN_EDGE_ACTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EDGE_ACTION, CdnEdgeActionClass))
#define CDN_IS_EDGE_ACTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EDGE_ACTION))
#define CDN_IS_EDGE_ACTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EDGE_ACTION))
#define CDN_EDGE_ACTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EDGE_ACTION, CdnEdgeActionClass))

typedef struct _CdnEdgeAction		CdnEdgeAction;
typedef struct _CdnEdgeActionClass	CdnEdgeActionClass;
typedef struct _CdnEdgeActionPrivate	CdnEdgeActionPrivate;

/**
 * CdnEdgeAction:
 *
 * Edge action equation.
 *
 * A #CdnEdgeAction is an action inside an edge which sets a target
 * #CdnVariable to the value of a particular #CdnExpression equation.
 */
struct _CdnEdgeAction
{
	/*< private >*/
	GInitiallyUnowned parent;

	CdnEdgeActionPrivate *priv;
};

struct _CdnEdgeActionClass
{
	/*< private >*/
	GInitiallyUnownedClass parent_class;
};

GType          cdn_edge_action_get_type      (void) G_GNUC_CONST;

CdnEdgeAction *cdn_edge_action_new           (const gchar   *target,
                                              CdnExpression *equation);

void           _cdn_edge_action_set_integrated (CdnEdgeAction *action,
                                               gboolean       integrated);
gboolean       _cdn_edge_action_get_integrated (CdnEdgeAction *action,
                                                gboolean      *integrated);

void           cdn_edge_action_set_adds      (CdnEdgeAction *action,
                                              gboolean       adds);
gboolean       cdn_edge_action_get_adds      (CdnEdgeAction *action);

CdnEdgeAction *cdn_edge_action_copy          (CdnEdgeAction *action);

const gchar   *cdn_edge_action_get_target    (CdnEdgeAction *action);
void           cdn_edge_action_set_target    (CdnEdgeAction *action,
                                              const gchar   *target);

CdnExpression  *cdn_edge_action_get_equation (CdnEdgeAction *action);
void            cdn_edge_action_set_equation (CdnEdgeAction *action,
                                              CdnExpression *equation);

gboolean        cdn_edge_action_equal        (CdnEdgeAction *action,
                                              CdnEdgeAction *other);

CdnVariable    *cdn_edge_action_get_target_variable  (CdnEdgeAction *action);
void            _cdn_edge_action_set_target_variable (CdnEdgeAction *action,
                                                      CdnVariable   *property);

CdnEdgeForward *cdn_edge_action_get_edge (CdnEdgeAction *action);

void            _cdn_edge_action_set_edge             (CdnEdgeAction              *action,
                                                       CdnEdgeForward *link);

void            cdn_edge_action_set_index       (CdnEdgeAction *action,
                                                 CdnExpression *expression);

CdnExpression   *cdn_edge_action_get_index      (CdnEdgeAction *action);


gint const      *cdn_edge_action_get_indices     (CdnEdgeAction *action,
                                                  gint          *num_indices);

gboolean         cdn_edge_action_compile         (CdnEdgeAction   *action,
                                                  CdnCompileContext *context,
                                                  CdnCompileErrorForward *error);

G_END_DECLS

#endif /* __CDN_EDGE_ACTION_H__ */
