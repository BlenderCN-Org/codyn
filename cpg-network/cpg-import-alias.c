/*
 * cpg-import-alias.c
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

#include "cpg-import-alias.h"

/**
 * SECTION:cpg-import-alias
 * @short_description: Network import alias object
 *
 * This object provides an alias for an imported network. This can be useful
 * when you have a shared library of templates A. Furthermore, imagine a network
 * B which imports A and a network C which imports both B and A. The second
 * import of A into C will then be an alias to the already imported A in B.
 *
 */

#define CPG_IMPORT_ALIAS_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_IMPORT_ALIAS, CpgImportAliasPrivate))

struct _CpgImportAliasPrivate
{
	CpgImport *source;
};

G_DEFINE_TYPE (CpgImportAlias, cpg_import_alias, CPG_TYPE_IMPORT)

enum
{
	PROP_0,
	PROP_SOURCE
};

static void
cpg_import_alias_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_import_alias_parent_class)->finalize (object);
}

static void
cpg_import_alias_dispose (GObject *object)
{
	CpgImportAlias *alias;

	alias = CPG_IMPORT_ALIAS (object);

	if (alias->priv->source)
	{
		g_object_unref (alias->priv->source);
		alias->priv->source = NULL;
	}

	G_OBJECT_CLASS (cpg_import_alias_parent_class)->dispose (object);
}

static void
cpg_import_alias_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CpgImportAlias *self = CPG_IMPORT_ALIAS (object);

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
cpg_import_alias_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CpgImportAlias *self = CPG_IMPORT_ALIAS (object);

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
cpg_import_alias_get_children (CpgGroup *group)
{
	CpgImportAlias *alias = CPG_IMPORT_ALIAS (group);
	return cpg_group_get_children (CPG_GROUP (alias->priv->source));
}

static CpgProperty *
cpg_import_alias_cpg_get_property (CpgObject   *object,
                                   gchar const *name)
{
	CpgImportAlias *alias = CPG_IMPORT_ALIAS (object);

	return cpg_object_get_property (CPG_OBJECT (alias->priv->source),
	                                name);
}

static GSList *
cpg_import_alias_cpg_get_properties (CpgObject *object)
{
	CpgImportAlias *alias = CPG_IMPORT_ALIAS (object);
	return cpg_object_get_properties (CPG_OBJECT (alias->priv->source));
}

static void
cpg_import_alias_cpg_copy (CpgObject *object,
                           CpgObject *source)
{
	CpgImportAlias *alias;

	alias = CPG_IMPORT_ALIAS (object);

	if (CPG_IS_IMPORT_ALIAS (source))
	{
		alias->priv->source = g_object_ref (CPG_IMPORT_ALIAS (source)->priv->source);
	}
	else
	{
		alias->priv->source = g_object_ref (source);
	}

	CPG_OBJECT_CLASS (cpg_import_alias_parent_class)->copy (object, source);
}

static void
cpg_import_alias_class_init (CpgImportAliasClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgGroupClass *group_class = CPG_GROUP_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	object_class->finalize = cpg_import_alias_finalize;
	object_class->dispose = cpg_import_alias_dispose;

	object_class->get_property = cpg_import_alias_get_property;
	object_class->set_property = cpg_import_alias_set_property;

	group_class->get_children = cpg_import_alias_get_children;

	cpg_class->get_property = cpg_import_alias_cpg_get_property;
	cpg_class->get_properties = cpg_import_alias_cpg_get_properties;
	cpg_class->copy = cpg_import_alias_cpg_copy;

	g_type_class_add_private (object_class, sizeof(CpgImportAliasPrivate));

	/**
	 * CpgImportAlias:source:
	 *
	 * The source #CpgImport to alias.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_SOURCE,
	                                 g_param_spec_object ("source",
	                                                      "Source",
	                                                      "Source",
	                                                      CPG_TYPE_IMPORT,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_import_alias_init (CpgImportAlias *self)
{
	self->priv = CPG_IMPORT_ALIAS_GET_PRIVATE (self);
}

/**
 * cpg_import_alias_new:
 * @source: A #CpgImport
 * 
 * Create a new alias for an existing import.
 *
 * Returns: A #CpgImportAlias
 *
 **/
CpgImportAlias *
cpg_import_alias_new (CpgImport *source)
{
	GFile *file = cpg_import_get_file (source);
	CpgImportAlias *ret;

	ret = g_object_new (CPG_TYPE_IMPORT_ALIAS,
	                    "id", cpg_object_get_id (CPG_OBJECT (source)),
	                    "file", file,
	                    "source", source,
	                    NULL);

	g_object_unref (file);
	return ret;
}
