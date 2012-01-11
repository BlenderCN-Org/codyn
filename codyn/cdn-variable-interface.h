/*
 * cdn-variable-interface.h
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

#ifndef __CDN_VARIABLE_INTERFACE_H__
#define __CDN_VARIABLE_INTERFACE_H__

#include <glib-object.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_VARIABLE_INTERFACE		(cdn_variable_interface_get_type ())
#define CDN_VARIABLE_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_VARIABLE_INTERFACE, CdnVariableInterface))
#define CDN_VARIABLE_INTERFACE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_VARIABLE_INTERFACE, CdnVariableInterface const))
#define CDN_VARIABLE_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_VARIABLE_INTERFACE, CdnVariableInterfaceClass))
#define CDN_IS_VARIABLE_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_VARIABLE_INTERFACE))
#define CDN_IS_VARIABLE_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_VARIABLE_INTERFACE))
#define CDN_VARIABLE_INTERFACE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_VARIABLE_INTERFACE, CdnVariableInterfaceClass))

#define CDN_VARIABLE_INTERFACE_ERROR (cdn_object_error_quark ())

/**
 * CdnVariableInterfaceError:
 *
 * Enum used to indicate an error when interacting with the variable interface
 *
 **/
typedef enum
{
	CDN_VARIABLE_INTERFACE_ERROR_EXISTS,
	CDN_VARIABLE_INTERFACE_ERROR_NOT_FOUND,
	CDN_VARIABLE_INTERFACE_ERROR_NUM_ERRORS
} CdnVariableInterfaceError;

typedef struct _CdnVariableInterface		CdnVariableInterface;
typedef struct _CdnVariableInterfaceClass	CdnVariableInterfaceClass;
typedef struct _CdnVariableInterfacePrivate	CdnVariableInterfacePrivate;

struct _CdnVariableInterface
{
	/*< private >*/
	GObject parent;

	CdnVariableInterfacePrivate *priv;
};

struct _CdnVariableInterfaceClass
{
	/*< private >*/
	GObjectClass parent_class;

	void (*added) (CdnVariableInterface *iface,
	               gchar const *name,
	               gchar const *child_name,
	               gchar const *property_name);

	void (*removed) (CdnVariableInterface *iface,
	                 gchar const *name,
	                 gchar const *child_name,
	                 gchar const *property_name);

	gboolean (*verify_remove) (CdnVariableInterface *iface,
	                           gchar const *name,
	                           gchar const *child_name,
	                           gchar const *property_name,
	                           GError **error);

	gboolean (*verify_add) (CdnVariableInterface *iface,
	                        gchar const *name,
	                        gchar const *child_name,
	                        gchar const *property_name,
	                        GError **error);
};

GQuark                 cdn_variable_interface_error_quark (void);

GType                  cdn_variable_interface_get_type    (void) G_GNUC_CONST;
CdnVariableInterface  *cdn_variable_interface_new         (CdnNodeForward *node);

CdnNodeForward       *cdn_variable_interface_get_node  (CdnVariableInterface  *iface);

gchar                **cdn_variable_interface_get_names   (CdnVariableInterface  *iface);

gboolean               cdn_variable_interface_implements  (CdnVariableInterface *iface,
                                                           gchar const          *name);

CdnVariable           *cdn_variable_interface_lookup      (CdnVariableInterface  *iface,
                                                           gchar const           *name);

gchar const           *cdn_variable_interface_lookup_child_name (CdnVariableInterface  *iface,
                                                                 gchar const           *name);

gchar const           *cdn_variable_interface_lookup_variable_name (CdnVariableInterface  *iface,
                                                                    gchar const           *name);

gboolean               cdn_variable_interface_add         (CdnVariableInterface  *iface,
                                                           gchar const           *name,
                                                           gchar const           *child_name,
                                                           gchar const           *variable_name,
                                                           GError               **error);

gboolean               cdn_variable_interface_remove      (CdnVariableInterface  *iface,
                                                           gchar const           *name,
                                                           GError               **error);

G_END_DECLS

#endif /* __CDN_VARIABLE_INTERFACE_H__ */
