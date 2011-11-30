/*
 * cdn-import-alias.h
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

#ifndef __CDN_IMPORT_ALIAS_H__
#define __CDN_IMPORT_ALIAS_H__

#include <codyn/cdn-import.h>

G_BEGIN_DECLS

#define CDN_TYPE_IMPORT_ALIAS            (cdn_import_alias_get_type ())
#define CDN_IMPORT_ALIAS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IMPORT_ALIAS, CdnImportAlias))
#define CDN_IMPORT_ALIAS_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IMPORT_ALIAS, CdnImportAlias const))
#define CDN_IMPORT_ALIAS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IMPORT_ALIAS, CdnImportAliasClass))
#define CDN_IS_IMPORT_ALIAS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IMPORT_ALIAS))
#define CDN_IS_IMPORT_ALIAS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IMPORT_ALIAS))
#define CDN_IMPORT_ALIAS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IMPORT_ALIAS, CdnImportAliasClass))

typedef struct _CdnImportAlias        CdnImportAlias;
typedef struct _CdnImportAliasClass   CdnImportAliasClass;
typedef struct _CdnImportAliasPrivate CdnImportAliasPrivate;

struct _CdnImportAlias
{
	/*< private >*/
	CdnImport parent;

	CdnImportAliasPrivate *priv;
};

/**
 * CdnImportAliasClass:
 *
 * The #CdnImportAlias class
 *
 */
struct _CdnImportAliasClass
{
	/*< private >*/
	CdnImportClass parent_class;
};

GType cdn_import_alias_get_type (void) G_GNUC_CONST;

CdnImportAlias *cdn_import_alias_new (CdnImport *source);

G_END_DECLS

#endif /* __CDN_IMPORT_ALIAS_H__ */
