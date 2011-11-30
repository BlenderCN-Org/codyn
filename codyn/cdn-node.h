/*
 * cdn-node.h
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

#ifndef __CDN_NODE_H__
#define __CDN_NODE_H__

#include <codyn/cdn-object.h>
#include <codyn/cdn-variable-interface.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_NODE			(cdn_node_get_type ())
#define CDN_NODE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NODE, CdnNode))
#define CDN_NODE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NODE, CdnNode const))
#define CDN_NODE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NODE, CdnNodeClass))
#define CDN_IS_NODE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NODE))
#define CDN_IS_NODE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NODE))
#define CDN_NODE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NODE, CdnNodeClass))

typedef enum
{
	CDN_NODE_ERROR_CHILD_ALREADY_EXISTS,
	CDN_NODE_ERROR_CHILD_DOES_NOT_EXIST,
	CDN_NODE_ERROR_INTERFACE_IS_PROXY,
	CDN_NODE_ERROR_CHILD_IN_USE,
	CDN_NODE_ERROR_NUM
} CdnNodeError;

typedef struct _CdnNode	CdnNode;
typedef struct _CdnNodeClass	CdnNodeClass;
typedef struct _CdnNodePrivate	CdnNodePrivate;

struct _CdnNode
{
	/*< private >*/
	CdnObject parent;

	CdnNodePrivate *priv;
};

/**
 * CdnNodeClass:
 * @add: add virtual function
 * @remove: remove virtual function
 * @child_added: child added default signal handler
 * @child_removed: child removed default signal handler
 * The CdnNode class
 *
 */
struct _CdnNodeClass
{
	/*< private >*/
	CdnObjectClass parent_class;

	/*< public >*/
	gboolean      (*add)           (CdnNode   *group,
	                                CdnObject  *object,
	                                GError    **error);

	gboolean      (*remove)        (CdnNode   *group,
	                                CdnObject  *object,
	                                GError    **error);

	gboolean      (*verify_remove_child) (CdnNode   *group,
	                                      CdnObject  *object,
	                                      GError    **error);

	GSList const *(*get_children)  (CdnNode   *group);

	/* signals */
	void          (*child_added)   (CdnNode   *group,
	                                CdnObject  *object);
	void          (*child_removed) (CdnNode   *group,
	                                CdnObject  *object);
};

#define CDN_NODE_ERROR (cdn_node_error_quark ())

GQuark        cdn_node_error_quark   (void);

GType         cdn_node_get_type      (void) G_GNUC_CONST;

CdnNode     *cdn_node_new           (const gchar *id,
                                       CdnObject   *proxy);

const GSList *cdn_node_get_children  (CdnNode    *group);

gboolean      cdn_node_add           (CdnNode    *group,
                                       CdnObject   *object,
                                       GError     **error);
gboolean      cdn_node_remove        (CdnNode    *group,
                                       CdnObject   *object,
                                       GError     **error);

gboolean      cdn_node_set_proxy     (CdnNode    *group,
                                       CdnObject   *proxy);
CdnObject    *cdn_node_get_proxy     (CdnNode    *group);

gboolean      cdn_node_variable_is_proxy (CdnNode    *group,
                                           const gchar *name);

void          cdn_node_foreach       (CdnNode    *group,
                                       GFunc        func,
                                       gpointer     data);

CdnObject    *cdn_node_get_child     (CdnNode    *group,
                                       const gchar *name);

CdnObject    *cdn_node_find_object   (CdnNode    *group,
                                       const gchar *selector);

GSList       *cdn_node_find_objects  (CdnNode    *group,
                                       const gchar *selector);

CdnVariable  *cdn_node_find_variable (CdnNode    *group,
                                       const gchar *selector);

GSList       *cdn_node_remove_variables (CdnNode    *group,
                                         const gchar *selector);

gboolean      cdn_node_verify_remove_child (CdnNode   *group,
                                             CdnObject  *child,
                                             GError    **error);

CdnVariableInterface *
              cdn_node_get_variable_interface (CdnNode *group);

GSList       *cdn_node_get_auto_templates_for_child (CdnNode  *group,
                                                      CdnObject *child);

const GSList     *cdn_node_get_edges      (CdnNode *group);
const GSList     *cdn_node_get_actors     (CdnNode  *group);

CdnEdgeForward *cdn_node_get_self_edge    (CdnNode *group);
gboolean cdn_node_has_self_edge    (CdnNode *group);

/* used for referencing links */
void             _cdn_node_link           (CdnNode       *group,
                                            CdnEdgeForward *link);

void             _cdn_node_unlink         (CdnNode       *group,
                                            CdnEdgeForward *link);


G_END_DECLS

#endif /* __CDN_NODE_H__ */
