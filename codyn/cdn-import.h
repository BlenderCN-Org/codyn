/*
 * cdn-import.h
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

#ifndef __CDN_IMPORT_H__
#define __CDN_IMPORT_H__

#include <codyn/cdn-network.h>

G_BEGIN_DECLS

#define CDN_TYPE_IMPORT			(cdn_import_get_type ())
#define CDN_IMPORT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IMPORT, CdnImport))
#define CDN_IMPORT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IMPORT, CdnImport const))
#define CDN_IMPORT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IMPORT, CdnImportClass))
#define CDN_IS_IMPORT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IMPORT))
#define CDN_IS_IMPORT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IMPORT))
#define CDN_IMPORT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IMPORT, CdnImportClass))

#define CDN_IMPORT_ERROR (cdn_import_error_quark ())

/**
 * CdnImportError:
 * @CDN_IMPORT_ERROR_REMOVE: cannot remove imported object
 *
 * Import error types.
 *
 */
typedef enum
{
	CDN_IMPORT_ERROR_REMOVE,
} CdnImportError;

typedef struct _CdnImport		CdnImport;
typedef struct _CdnImportClass		CdnImportClass;
typedef struct _CdnImportPrivate	CdnImportPrivate;

/**
 * CdnImport:
 *
 * Network import object.
 *
 * The #CdnImport object can be used to import templates and objects from
 * an external network file.
 **/
struct _CdnImport
{
	/*< private >*/
	CdnNode parent;

	CdnImportPrivate *priv;
};

/**
 * CdnImportClass:
 *
 * The #CdnImport class
 *
 */
struct _CdnImportClass
{
	/*< private >*/
	CdnNodeClass parent_class;
};

GType cdn_import_get_type (void) G_GNUC_CONST;

GQuark cdn_import_error_quark (void);

CdnImport   *cdn_import_new                 (CdnNetwork   *network,
                                             CdnNode     *parent,
                                             const gchar  *id,
                                             GFile        *file,
                                             GError      **error);

CdnImport   *cdn_import_new_from_path       (CdnNetwork   *network,
                                             CdnNode     *parent,
                                             const gchar  *id,
                                             const gchar  *path,
                                             GError      **error);

gboolean     cdn_import_load                (CdnImport    *self,
                                             CdnNetwork   *network,
                                             CdnNode     *parent,
                                             GError      **error);

GFile       *cdn_import_get_file            (CdnImport    *self);
gchar       *cdn_import_get_path            (CdnImport    *self);

gboolean     cdn_import_imports_object      (CdnImport    *self,
                                             CdnObject    *object);

G_CONST_RETURN gchar * G_CONST_RETURN *
             cdn_import_get_search_path     ();

void         cdn_import_set_search_path     (gchar **path);

void         cdn_import_append_search_path  (const gchar  *path);

void         cdn_import_prepend_search_path (const gchar  *path);


G_END_DECLS

#endif /* __CDN_IMPORT_H__ */
