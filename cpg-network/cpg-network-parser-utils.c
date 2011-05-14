#include "cpg-network-parser-utils.h"

gboolean
cpg_network_parser_utils_get_templates (CpgNetwork           *network,
                                        CpgGroup             *parent,
                                        gboolean              for_template,
                                        gchar const * const  *names,
                                        gchar               **missing,
                                        GSList              **templates)
{
	CpgGroup *template_group;
	gboolean ret;
	gchar const * const *ptr;

	if (templates)
	{
		*templates = NULL;
	}

	if (missing)
	{
		*missing = NULL;
	}

	if (!names)
	{
		return TRUE;
	}

	template_group = cpg_network_get_template_group (network);
	ret = TRUE;

	/* Multiple templates are allowed, iterate over all the template ids
	   and resolve them from the template group */
	for (ptr = names; *ptr; ++ptr)
	{
		CpgObject *template = NULL;

		if (!**ptr)
		{
			continue;
		}

		/* Find the reference relative to the current parent if we
		   are parsing templates, since they can reference other
		   templates */
		if (for_template && parent)
		{
			template = cpg_group_find_object (parent, *ptr);
		}

		if (!template)
		{
			/* Find template in the root template group */
			template = cpg_group_find_object (template_group,
			                                  *ptr);
		}

		if (!template)
		{
			if (missing)
			{
				*missing = g_strdup (*ptr);
			}

			ret = FALSE;
		}
		else if (templates)
		{
			*templates = g_slist_prepend (*templates, template);
		}

		if (!ret)
		{
			break;
		}
	}

	if (templates)
	{
		if (ret)
		{
			*templates = g_slist_reverse (*templates);
		}
		else
		{
			g_slist_free (*templates);
		}
	}

	return ret;
}

GType
cpg_network_parser_utils_type_from_templates (GType   orig,
                                              GSList *templates)
{
	/* This only needs to be checked for states since in the XML groups
	   can be defined with a <state> to make it nicer to write the XML */
	if (orig != CPG_TYPE_OBJECT)
	{
		return orig;
	}

	/* Check here if any of the templates applied are actually groups,
	   if so, the resulting type for the new object should also be a
	   group */
	while (templates)
	{
		if (G_TYPE_FROM_INSTANCE (templates->data) == CPG_TYPE_GROUP)
		{
			return CPG_TYPE_GROUP;
		}

		templates = g_slist_next (templates);
	}

	return orig;
}

GFile *
cpg_network_parser_utils_resolve_import (GFile       *root,
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
		const gchar * const *dirs = cpg_import_get_search_path ();

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

CpgImport *
cpg_network_parser_utils_find_template_import (CpgObject *child,
                                               GFile     *file)
{
	if (CPG_IS_IMPORT (child))
	{
		GFile *f = cpg_import_get_file (CPG_IMPORT (child));
		gboolean equal = g_file_equal (file, f);

		g_object_unref (f);

		if (equal)
		{
			return CPG_IMPORT (child);
		}
	}

	if (CPG_IS_GROUP (child))
	{
		GSList const *children = cpg_group_get_children (CPG_GROUP (child));

		while (children)
		{
			CpgImport *ret = cpg_network_parser_utils_find_template_import (children->data, file);

			if (ret)
			{
				return ret;
			}

			children = g_slist_next (children);
		}
	}

	return NULL;
}
