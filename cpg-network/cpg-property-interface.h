/*
 * cpg-property-interface.h
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

#ifndef __CPG_PROPERTY_INTERFACE_H__
#define __CPG_PROPERTY_INTERFACE_H__

#include <glib-object.h>
#include <cpg-network/cpg-property.h>
#include <cpg-network/cpg-forward-decl.h>

G_BEGIN_DECLS

#define CPG_TYPE_PROPERTY_INTERFACE		(cpg_property_interface_get_type ())
#define CPG_PROPERTY_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterface))
#define CPG_PROPERTY_INTERFACE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterface const))
#define CPG_PROPERTY_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterfaceClass))
#define CPG_IS_PROPERTY_INTERFACE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_PROPERTY_INTERFACE))
#define CPG_IS_PROPERTY_INTERFACE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_PROPERTY_INTERFACE))
#define CPG_PROPERTY_INTERFACE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterfaceClass))

#define CPG_PROPERTY_INTERFACE_ERROR (cpg_object_error_quark ())

/**
 * CpgPropertyInterfaceError:
 *
 * Enum used to indicate an error when interacting with the property interface
 *
 **/
typedef enum
{
	CPG_PROPERTY_INTERFACE_ERROR_EXISTS,
	CPG_PROPERTY_INTERFACE_ERROR_NOT_FOUND,
	CPG_PROPERTY_INTERFACE_ERROR_NUM_ERRORS
} CpgPropertyInterfaceError;

typedef struct _CpgPropertyInterface		CpgPropertyInterface;
typedef struct _CpgPropertyInterfaceClass	CpgPropertyInterfaceClass;
typedef struct _CpgPropertyInterfacePrivate	CpgPropertyInterfacePrivate;

struct _CpgPropertyInterface
{
	/*< private >*/
	GObject parent;

	CpgPropertyInterfacePrivate *priv;
};

struct _CpgPropertyInterfaceClass
{
	/*< private >*/
	GObjectClass parent_class;

	void (*added) (CpgPropertyInterface *iface,
	               gchar const *name,
	               gchar const *child_name,
	               gchar const *property_name);

	void (*removed) (CpgPropertyInterface *iface,
	                 gchar const *name,
	                 gchar const *child_name,
	                 gchar const *property_name);

	gboolean (*verify_remove) (CpgPropertyInterface *iface,
	                           gchar const *name,
	                           gchar const *child_name,
	                           gchar const *property_name,
	                           GError **error);

	gboolean (*verify_add) (CpgPropertyInterface *iface,
	                        gchar const *name,
	                        gchar const *child_name,
	                        gchar const *property_name,
	                        GError **error);
};

GQuark                 cpg_property_interface_error_quark (void);

GType                  cpg_property_interface_get_type    (void) G_GNUC_CONST;
CpgPropertyInterface  *cpg_property_interface_new         (CpgGroupForward *group);

CpgGroupForward       *cpg_property_interface_get_group  (CpgPropertyInterface  *iface);

gchar                **cpg_property_interface_get_names   (CpgPropertyInterface  *iface);

gboolean               cpg_property_interface_implements  (CpgPropertyInterface *iface,
                                                           gchar const          *name);

CpgProperty           *cpg_property_interface_lookup      (CpgPropertyInterface  *iface,
                                                           gchar const           *name);

gchar const           *cpg_property_interface_lookup_child_name (CpgPropertyInterface  *iface,
                                                                 gchar const           *name);

gchar const           *cpg_property_interface_lookup_property_name (CpgPropertyInterface  *iface,
                                                                    gchar const           *name);

gboolean               cpg_property_interface_add         (CpgPropertyInterface  *iface,
                                                           gchar const           *name,
                                                           gchar const           *child_name,
                                                           gchar const           *property_name,
                                                           GError               **error);

gboolean               cpg_property_interface_remove      (CpgPropertyInterface  *iface,
                                                           gchar const           *name,
                                                           GError               **error);

G_END_DECLS

#endif /* __CPG_PROPERTY_INTERFACE_H__ */
