/*
 * cdn-import-alias.c
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

#include "cdn-import-alias.h"

#define CDN_IMPORT_ALIAS_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IMPORT_ALIAS, CdnImportAliasPrivate))

struct _CdnImportAliasPrivate
{
	CdnImport *source;
};

G_DEFINE_TYPE (CdnImportAlias, cdn_import_alias, CDN_TYPE_IMPORT)

enum
{
	PROP_0,
	PROP_SOURCE
};

static void
cdn_import_alias_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_import_alias_parent_class)->finalize (object);
}

static void
cdn_import_alias_dispose (GObject *object)
{
	CdnImportAlias *alias;

	alias = CDN_IMPORT_ALIAS (object);

	if (alias->priv->source)
	{
		g_object_unref (alias->priv->source);
		alias->priv->source = NULL;
	}

	G_OBJECT_CLASS (cdn_import_alias_parent_class)->dispose (object);
}

static void
cdn_import_alias_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CdnImportAlias *self = CDN_IMPORT_ALIAS (object);

	switch (prop_id)
	{
		case PROP_SOURCE:
			self->priv->source = g_value_dup_object (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_import_alias_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CdnImportAlias *self = CDN_IMPORT_ALIAS (object);

	switch (prop_id)
	{
		case PROP_SOURCE:
			g_value_set_object (value, self->priv->source);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static GSList const *
cdn_import_alias_get_children (CdnNode *group)
{
	CdnImportAlias *alias = CDN_IMPORT_ALIAS (group);
	return cdn_node_get_children (CDN_NODE (alias->priv->source));
}

static CdnVariable *
cdn_import_alias_cdn_get_property (CdnObject   *object,
                                   gchar const *name)
{
	CdnImportAlias *alias = CDN_IMPORT_ALIAS (object);

	return cdn_object_get_variable (CDN_OBJECT (alias->priv->source),
	                                name);
}

static GSList *
cdn_import_alias_cdn_get_variables (CdnObject *object)
{
	CdnImportAlias *alias = CDN_IMPORT_ALIAS (object);
	return cdn_object_get_variables (CDN_OBJECT (alias->priv->source));
}

static void
cdn_import_alias_cdn_copy (CdnObject *object,
                           CdnObject *source)
{
	CdnImportAlias *alias;

	alias = CDN_IMPORT_ALIAS (object);

	if (CDN_IS_IMPORT_ALIAS (source))
	{
		alias->priv->source = g_object_ref (CDN_IMPORT_ALIAS (source)->priv->source);
	}
	else
	{
		alias->priv->source = g_object_ref (source);
	}

	CDN_OBJECT_CLASS (cdn_import_alias_parent_class)->copy (object, source);
}

static void
cdn_import_alias_class_init (CdnImportAliasClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnNodeClass *group_class = CDN_NODE_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_import_alias_finalize;
	object_class->dispose = cdn_import_alias_dispose;

	object_class->get_property = cdn_import_alias_get_property;
	object_class->set_property = cdn_import_alias_set_property;

	group_class->get_children = cdn_import_alias_get_children;

	cdn_class->get_variable = cdn_import_alias_cdn_get_property;
	cdn_class->get_variables = cdn_import_alias_cdn_get_variables;
	cdn_class->copy = cdn_import_alias_cdn_copy;

	g_type_class_add_private (object_class, sizeof(CdnImportAliasPrivate));

	/**
	 * CdnImportAlias:source:
	 *
	 * The source #CdnImport to alias.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_SOURCE,
	                                 g_param_spec_object ("source",
	                                                      "Source",
	                                                      "Source",
	                                                      CDN_TYPE_IMPORT,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_import_alias_init (CdnImportAlias *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_import_alias_get_instance_private (self);
	self->priv = CDN_IMPORT_ALIAS_GET_PRIVATE (self);
}

/**
 * cdn_import_alias_new:
 * @source: A #CdnImport
 *
 * Create a new alias for an existing import.
 *
 * Returns: A #CdnImportAlias
 *
 **/
CdnImportAlias *
cdn_import_alias_new (CdnImport *source)
{
	GFile *file = cdn_import_get_file (source);
	CdnImportAlias *ret;

	ret = g_object_new (CDN_TYPE_IMPORT_ALIAS,
	                    "id", cdn_object_get_id (CDN_OBJECT (source)),
	                    "file", file,
	                    "source", source,
	                    NULL);

	g_object_unref (file);
	return ret;
}
