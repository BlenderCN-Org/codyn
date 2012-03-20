/*
 * codyn-parser-utils.c
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

#include "cdn-network-parser-utils.h"
#include <string.h>

gboolean
cdn_network_parser_utils_get_templates (CdnNetwork           *network,
                                        CdnNode             *parent,
                                        gboolean              for_template,
                                        GSList               *selectors,
                                        CdnEmbeddedContext   *context,
                                        gchar               **missing,
                                        GSList              **templates)
{
	CdnNode *template_group;
	gboolean ret;

	if (templates)
	{
		*templates = NULL;
	}

	if (missing)
	{
		*missing = NULL;
	}

	if (!selectors)
	{
		return TRUE;
	}

	template_group = cdn_network_get_template_node (network);
	ret = TRUE;

	while (selectors)
	{
		GSList *template = NULL;

		/* Find the reference relative to the current parent if we
		   are parsing templates, since they can reference other
		   templates */
		if (for_template && parent)
		{
			template = cdn_selector_select (selectors->data,
			                                G_OBJECT (parent),
			                                CDN_SELECTOR_TYPE_OBJECT,
			                                context);
		}

		if (!template)
		{
			/* Find template in the root template group */
			template = cdn_selector_select (selectors->data,
			                                G_OBJECT (template_group),
			                                CDN_SELECTOR_TYPE_OBJECT,
			                                context);
		}

		if (!template)
		{
			if (missing)
			{
				*missing = cdn_selector_as_string (selectors->data);
			}

			ret = FALSE;
		}
		else if (templates)
		{
			*templates = g_slist_concat (*templates, template);
		}
		else
		{
			g_slist_free (template);
		}

		if (!ret)
		{
			break;
		}

		selectors = g_slist_next (selectors);
	}

	if (templates)
	{
		if (!ret)
		{
			g_slist_foreach (*templates, (GFunc)g_object_unref, NULL);
			g_slist_free (*templates);

			*templates = NULL;
		}
	}

	return ret;
}

GType
cdn_network_parser_utils_type_from_templates (GType   orig,
                                              GSList *templates)
{
	/* This only needs to be checked for states since in the XML groups
	   can be defined with a <state> to make it nicer to write the XML */
	if (orig != CDN_TYPE_OBJECT)
	{
		return orig;
	}

	/* Check here if any of the templates applied are actually groups,
	   if so, the resulting type for the new object should also be a
	   group */
	while (templates)
	{
		if (G_TYPE_FROM_INSTANCE (templates->data) == CDN_TYPE_NODE)
		{
			return CDN_TYPE_NODE;
		}

		templates = g_slist_next (templates);
	}

	return orig;
}

GFile *
cdn_network_parser_utils_resolve_import (GFile       *root,
                                         gchar const *filename)
{
	GFile *file = NULL;

	if (g_path_is_absolute (filename))
	{
		return g_file_new_for_path (filename);
	}

	if (root)
	{
		GFile *parent;

		parent = g_file_get_parent (root);

		/* Relative to file being imported */
		file = g_file_get_child (parent, filename);
		g_object_unref (parent);

		if (!g_file_query_exists (file, NULL))
		{
			g_object_unref (file);
			file = NULL;
		}
	}

	if (!file)
	{
		/* Current working directory maybe? */
		file = g_file_new_for_path (filename);

		if (!g_file_query_exists (file, NULL))
		{
			g_object_unref (file);
			file = NULL;
		}
	}

	if (!file)
	{
		/* Search directories */
		const gchar * const *dirs = cdn_import_get_search_path ();

		while (dirs && *dirs)
		{
			gchar *path = g_build_filename (*dirs, filename, NULL);
			file = g_file_new_for_path (path);

			g_free (path);

			if (g_file_query_exists (file, NULL))
			{
				break;
			}

			g_object_unref (file);
			file = NULL;

			++dirs;
		}
	}

	return file;
}

CdnImport *
cdn_network_parser_utils_find_template_import (CdnObject *child,
                                               GFile     *file)
{
	if (CDN_IS_IMPORT (child))
	{
		GFile *f = cdn_import_get_file (CDN_IMPORT (child));
		gboolean equal = g_file_equal (file, f);

		g_object_unref (f);

		if (equal)
		{
			return CDN_IMPORT (child);
		}
	}

	if (CDN_IS_NODE (child))
	{
		GSList const *children = cdn_node_get_children (CDN_NODE (child));

		while (children)
		{
			CdnImport *ret = cdn_network_parser_utils_find_template_import (children->data, file);

			if (ret)
			{
				return ret;
			}

			children = g_slist_next (children);
		}
	}

	return NULL;
}
