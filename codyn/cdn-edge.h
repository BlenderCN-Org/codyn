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

	void (*action_added)   (CdnEdge *link, CdnEdgeAction *action);
	void (*action_removed) (CdnEdge *link, CdnEdgeAction *action);
};

GType            cdn_edge_get_type              (void) G_GNUC_CONST;

CdnEdge       *cdn_edge_new                   (const gchar   *id,
                                               CdnNode       *input,
                                               CdnNode       *output);

CdnNode       *cdn_edge_get_input             (CdnEdge       *link);
CdnNode       *cdn_edge_get_output            (CdnEdge       *link);

gboolean       cdn_edge_add_action            (CdnEdge       *link,
                                               CdnEdgeAction *action);

gboolean       cdn_edge_remove_action         (CdnEdge       *link,
                                               CdnEdgeAction *action);

const GSList  *cdn_edge_get_actions           (CdnEdge       *link);
CdnEdgeAction *cdn_edge_get_action            (CdnEdge       *link,
                                               const gchar   *target);

void           cdn_edge_attach                (CdnEdge       *link,
                                               CdnNode       *input,
                                               CdnNode       *output);

CdnEdge       *cdn_edge_get_action_template   (CdnEdge       *link,
                                               CdnEdgeAction *action,
                                               gboolean       match_full);

CdnEdgeAction *cdn_edge_get_action_with_index (CdnEdge       *link,
                                               gchar const   *target,
                                               CdnExpression *index);

G_END_DECLS

#endif /* __CDN_EDGE_H__ */
