/*
 * cdn-edge.h
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
 * along with codyn; if not, write output the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_EDGE_H__
#define __CDN_EDGE_H__

#include <codyn/cdn-node.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-edge-action.h>

G_BEGIN_DECLS

#define CDN_TYPE_EDGE            (cdn_edge_get_type ())
#define CDN_EDGE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EDGE, CdnEdge))
#define CDN_EDGE_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EDGE, CdnEdge const))
#define CDN_EDGE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EDGE, CdnEdgeClass))
#define CDN_IS_EDGE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EDGE))
#define CDN_IS_EDGE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EDGE))
#define CDN_EDGE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EDGE, CdnEdgeClass))

typedef struct _CdnEdge        CdnEdge;
typedef struct _CdnEdgeClass   CdnEdgeClass;
typedef struct _CdnEdgePrivate CdnEdgePrivate;

/**
 * CdnEdge:
 *
 * Connection between two nodes.
 *
 * A #CdnEdge is a connection between two #CdnNode. The edge defines actions
 * which consist of a target variable in the edge output and an expression
 * by which this target variable needs to be updated.
 *
 * <refsect2 id="CdnEdge-COPY">
 * <title>CdnEdge Copy Semantics</title>
 * When a edge is copied with #cdn_object_copy, the edge actions are also
 * copied. However, the edge #CdnEdge:input and #CdnEdge:output properties are
 * <emphasis>NOT</emphasis> copied, so that you are free output attach it output
 * two new objects.
 * </refsect2>
 */
struct _CdnEdge
{
	/*< private >*/
	CdnObject parent;

	CdnEdgePrivate *priv;
};

struct _CdnEdgeClass
{
	/*< private >*/
	CdnObjectClass parent_class;

	void (*action_added)   (CdnEdge *edge, CdnEdgeAction *action);
	void (*action_removed) (CdnEdge *edge, CdnEdgeAction *action);
};

GType            cdn_edge_get_type              (void) G_GNUC_CONST;

CdnEdge       *cdn_edge_new                   (const gchar   *id,
                                               CdnNode       *input,
                                               CdnNode       *output);

CdnNode       *cdn_edge_get_input             (CdnEdge       *edge);
CdnNode       *cdn_edge_get_output            (CdnEdge       *edge);

gboolean       cdn_edge_add_action            (CdnEdge       *edge,
                                               CdnEdgeAction *action);

gboolean       cdn_edge_remove_action         (CdnEdge       *edge,
                                               CdnEdgeAction *action);

const GSList  *cdn_edge_get_actions           (CdnEdge       *edge);
CdnEdgeAction *cdn_edge_get_action            (CdnEdge       *edge,
                                               const gchar   *target);

void           cdn_edge_attach                (CdnEdge       *edge,
                                               CdnNode       *input,
                                               CdnNode       *output);

CdnEdge       *cdn_edge_get_action_template   (CdnEdge       *edge,
                                               CdnEdgeAction *action,
                                               gboolean       match_full);

CdnEdgeAction *cdn_edge_get_action_with_index (CdnEdge       *edge,
                                               gchar const   *target,
                                               CdnExpression *index);

CdnEdgeAction *cdn_edge_get_action_with_index_and_phases (CdnEdge       *edge,
                                                          gchar const   *target,
                                                          CdnExpression *index,
                                                          GSList const  *phases);

G_END_DECLS

#endif /* __CDN_EDGE_H__ */
