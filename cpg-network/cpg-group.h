/*
 * cpg-group.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_GROUP_H__
#define __CPG_GROUP_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-property-interface.h>

G_BEGIN_DECLS

#define CPG_TYPE_GROUP			(cpg_group_get_type ())
#define CPG_GROUP(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_GROUP, CpgGroup))
#define CPG_GROUP_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_GROUP, CpgGroup const))
#define CPG_GROUP_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_GROUP, CpgGroupClass))
#define CPG_IS_GROUP(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_GROUP))
#define CPG_IS_GROUP_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_GROUP))
#define CPG_GROUP_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_GROUP, CpgGroupClass))

typedef enum
{
	CPG_GROUP_ERROR_CHILD_ALREADY_EXISTS,
	CPG_GROUP_ERROR_CHILD_DOES_NOT_EXIST,
	CPG_GROUP_ERROR_INTERFACE_IS_PROXY,
	CPG_GROUP_ERROR_CHILD_IN_USE,
	CPG_GROUP_ERROR_NUM
} CpgGroupError;

typedef struct _CpgGroup	CpgGroup;
typedef struct _CpgGroupClass	CpgGroupClass;
typedef struct _CpgGroupPrivate	CpgGroupPrivate;

struct _CpgGroup
{
	/*< private >*/
	CpgObject parent;

	CpgGroupPrivate *priv;
};

/**
 * CpgGroupClass:
 * @add: add virtual function
 * @remove: remove virtual function
 * @child_added: child added default signal handler
 * @child_removed: child removed default signal handler
 * The CpgGroup class
 *
 */
struct _CpgGroupClass
{
	/*< private >*/
	CpgObjectClass parent_class;

	/*< public >*/
	gboolean      (*add)           (CpgGroup   *group,
	                                CpgObject  *object,
	                                GError    **error);

	gboolean      (*remove)        (CpgGroup   *group,
	                                CpgObject  *object,
	                                GError    **error);

	gboolean      (*verify_remove_child) (CpgGroup   *group,
	                                      CpgObject  *object,
	                                      GError    **error);

	GSList const *(*get_children)  (CpgGroup   *group);

	/* signals */
	void          (*child_added)   (CpgGroup   *group,
	                                CpgObject  *object);
	void          (*child_removed) (CpgGroup   *group,
	                                CpgObject  *object);
};

#define CPG_GROUP_ERROR (cpg_group_error_quark ())

GQuark        cpg_group_error_quark   (void);

GType         cpg_group_get_type      (void) G_GNUC_CONST;

CpgGroup     *cpg_group_new           (const gchar *id,
                                       CpgObject   *proxy);

const GSList *cpg_group_get_children  (CpgGroup    *group);

gboolean      cpg_group_add           (CpgGroup    *group,
                                       CpgObject   *object,
                                       GError     **error);
gboolean      cpg_group_remove        (CpgGroup    *group,
                                       CpgObject   *object,
                                       GError     **error);

gboolean      cpg_group_set_proxy     (CpgGroup    *group,
                                       CpgObject   *proxy);
CpgObject    *cpg_group_get_proxy     (CpgGroup    *group);

gboolean      cpg_group_property_is_proxy (CpgGroup    *group,
                                           const gchar *name);

void          cpg_group_foreach       (CpgGroup    *group,
                                       GFunc        func,
                                       gpointer     data);

CpgObject    *cpg_group_get_child     (CpgGroup    *group,
                                       const gchar *name);

CpgObject    *cpg_group_find_object   (CpgGroup    *group,
                                       const gchar *selector);

GSList       *cpg_group_find_objects  (CpgGroup    *group,
                                       const gchar *selector);

CpgProperty  *cpg_group_find_property (CpgGroup    *group,
                                       const gchar *selector);

GSList       *cpg_group_find_properties (CpgGroup    *group,
                                         const gchar *selector);

gboolean      cpg_group_verify_remove_child (CpgGroup   *group,
                                             CpgObject  *child,
                                             GError    **error);

CpgPropertyInterface *
              cpg_group_get_property_interface (CpgGroup *group);

GSList       *cpg_group_get_auto_templates_for_child (CpgGroup  *group,
                                                      CpgObject *child);

G_END_DECLS

#endif /* __CPG_GROUP_H__ */
