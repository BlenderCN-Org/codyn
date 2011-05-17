#include "cpg-network-parser-utils.h"

gboolean
cpg_network_parser_utils_get_templates (CpgNetwork           *network,
                                        CpgGroup             *parent,
                                        gboolean              for_template,
                                        GSList               *selectors,
                                        gchar               **missing,
                                        GSList              **templates)
{
	CpgGroup *template_group;
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

	template_group = cpg_network_get_template_group (network);
	ret = TRUE;

	while (selectors)
	{
		GSList *template = NULL;

		/* Find the reference relative to the current parent if we
		   are parsing templates, since they can reference other
		   templates */
		if (for_template && parent)
		{
			template = cpg_selector_select (selectors->data,
			                                CPG_OBJECT (parent));
		}

		if (!template)
		{
			/* Find template in the root template group */
			template = cpg_selector_select (selectors->data,
			                                CPG_OBJECT (template_group));
		}

		if (!template)
		{
			if (missing)
			{
				*missing = g_strdup (cpg_selector_as_string (selectors->data));
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
			g_slist_foreach (*templates, (GFunc)cpg_selection_free, NULL);
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

static GSList *
parse_expansion_range (GSList *ret,
                       gchar  *id)
{
	static GRegex *rangereg = NULL;
	GMatchInfo *info;

	if (rangereg == NULL)
	{
		rangereg = g_regex_new ("([0-9]+)-([0-9]+)",
		                        G_REGEX_ANCHORED,
		                        G_REGEX_MATCH_ANCHORED,
		                        NULL);
	}

	if (g_regex_match (rangereg, id, 0, &info))
	{
		gchar *start = g_match_info_fetch (info, 1);
		gchar *end = g_match_info_fetch (info, 2);

		gint cstart = (gint)g_ascii_strtoll (start, NULL, 10);
		gint cend = (gint)g_ascii_strtoll (end, NULL, 10);

		while (TRUE)
		{
			ret = g_slist_prepend (ret,
			                       g_strdup_printf ("%d", cstart));

			if (cstart == cend)
			{
				break;
			}

			if (cstart > cend)
			{
				--cstart;
			}
			else
			{
				++cstart;
			}
		}

		g_free (start);
		g_free (end);

		g_match_info_free (info);
		g_free (id);
	}
	else
	{
		ret = g_slist_prepend (ret, id);
	}

	return ret;
}

static GSList *
parse_expansion (gchar const **id)
{
	gchar const *ptr = *id;
	GSList *ret = NULL;
	gint depth = 0;

	while (**id && (**id != '}' || depth != 0))
	{
		if (**id == ',')
		{
			if (ptr != *id)
			{
				ret = parse_expansion_range (ret,
				                             g_strndup (ptr, *id - ptr));
			}

			ptr = *id + 1;
		}
		else if (**id == '{')
		{
			++depth;
		}
		else if (**id == '}')
		{
			--depth;
		}

		++*id;
	}

	if (ptr != *id)
	{
		ret = parse_expansion_range (ret,
		                             g_strndup (ptr, *id - ptr));
	}

	if (**id == '}')
	{
		++*id;
	}

	return g_slist_reverse (ret);
}

static CpgExpansion *
expand_id (CpgExpansion *id,
           gchar const  *s)
{
	gchar *c;
	CpgExpansion *ret;

	c = g_strconcat (cpg_expansion_get (id, 0), s, NULL);

	ret = cpg_expansion_copy (id);
	cpg_expansion_set (id, 0, c);
	g_free (c);

	cpg_expansion_add (id, s);

	return ret;
}

static GSList *
prepend_expansion (GSList *items,
                   GSList *expansions)
{
	GSList *ret = NULL;
	GSList *ptr;

	for (ptr = items; ptr; ptr = g_slist_next (ptr))
	{
		CpgExpansion *id = ptr->data;
		GSList *ex;

		for (ex = expansions; ex; ex = g_slist_next (ex))
		{
			ret = g_slist_prepend (ret,
			                       expand_id (id,
			                                  ex->data));
		}

		cpg_expansion_free (id);
	}

	g_slist_free (items);
	return ret;
}

static void
add_to_expansions (GSList      *ids,
                   gchar const *id,
                   gint         len)
{
	gchar *added;

	added = g_strndup (id, len);

	while (ids)
	{
		CpgExpansion *p;
		gchar *tmp;

		p = ids->data;

		tmp = g_strconcat (cpg_expansion_get (p, 0),
		                   added,
		                   NULL);

		cpg_expansion_set (p, 0, tmp);
		g_free (tmp);

		ids = g_slist_next (ids);
	}

	g_free (added);
}

GSList *
cpg_network_parser_utils_expand_id (gchar const *id)
{
	GSList *ret = NULL;
	gchar const *ptr = id;

	ret = g_slist_prepend (NULL,
	                       cpg_expansion_new (NULL));

	while (*id)
	{
		if (*id == '{')
		{
			GSList *ex;

			if (ptr != id)
			{
				add_to_expansions (ret, ptr, id - ptr);
			}

			++id;

			ex = parse_expansion (&id);
			ret = prepend_expansion (g_slist_reverse (ret), ex);

			g_slist_foreach (ex, (GFunc)g_free, NULL);
			g_slist_free (ex);

			ptr = id;
		}

		++id;
	}

	if (ptr != id)
	{
		add_to_expansions (ret, ptr, id - ptr);
	}

	return g_slist_reverse (ret);
}
