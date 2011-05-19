#include "cpg-network-parser-utils.h"
#include <string.h>

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
parse_expansion_range (gchar const *s,
                       gint         len)
{
	static GRegex *rangereg = NULL;
	GMatchInfo *info;
	gchar *id;
	GSList *ret = NULL;

	if (rangereg == NULL)
	{
		rangereg = g_regex_new ("([0-9]+)-([0-9]+)",
		                        G_REGEX_ANCHORED,
		                        G_REGEX_MATCH_ANCHORED,
		                        NULL);
	}

	id = g_strndup (s, len);

	if (g_regex_match (rangereg, id, 0, &info))
	{
		gchar *start = g_match_info_fetch (info, 1);
		gchar *end = g_match_info_fetch (info, 2);

		gint cstart = (gint)g_ascii_strtoll (start, NULL, 10);
		gint cend = (gint)g_ascii_strtoll (end, NULL, 10);

		while (TRUE)
		{
			gchar *it;

			it = g_strdup_printf ("%d", cstart);

			ret = g_slist_prepend (ret,
			                       cpg_expansion_new_one (it));

			g_free (it);

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
	}
	else
	{
		ret = g_slist_prepend (ret,
		                       cpg_expansion_new_one (id));
	}

	g_free (id);

	return g_slist_reverse (ret);
}

static void
expansions_add (GSList      *expansions,
                gchar const *s,
                gint         len,
                gboolean     prepend)
{
	gchar *ss;

	if (len < 0)
	{
		len = strlen (s);
	}

	if (len == 0)
	{
		return;
	}

	ss = g_strndup (s, len);

	while (expansions)
	{
		gchar *c;
		gchar const *cur = cpg_expansion_get (expansions->data, 0);

		if (prepend)
		{
			c = g_strconcat (ss, cur, NULL);
		}
		else
		{
			c = g_strconcat (cur, ss, NULL);
		}

		cpg_expansion_set (expansions->data, 0, c);
		g_free (c);

		expansions = g_slist_next (expansions);
	}

	g_free (ss);
}

static void
expansions_prepend (GSList      *expansions,
                    gchar const *s,
                    gint         len)
{
	expansions_add (expansions, s, len, TRUE);
}

static void
expansions_append (GSList      *expansions,
                   gchar const *s,
                   gint         len)
{
	expansions_add (expansions, s, len, FALSE);
}

static CpgExpansion *
expansion_concat (CpgExpansion *s1,
                  CpgExpansion *s2)
{
	CpgExpansion *copy;
	gchar *n0;
	gint i;

	copy = cpg_expansion_copy (s1);
	n0 = g_strconcat (cpg_expansion_get (s1, 0),
	                  cpg_expansion_get (s2, 0),
	                  NULL);

	cpg_expansion_set (copy, 0, n0);
	g_free (n0);

	for (i = 1; i < cpg_expansion_num (s2); ++i)
	{
		cpg_expansion_add (copy, cpg_expansion_get (s2, i));
	}

	return copy;
}

static GSList *
expansions_concat (GSList *s1,
                   GSList *s2)
{
	GSList *ret = NULL;

	if (s1 == NULL)
	{
		s1 = g_slist_prepend (s1, cpg_expansion_new (NULL));
	}

	while (s1)
	{
		GSList *item = s2;

		while (item)
		{
			ret = g_slist_prepend (ret,
			                       expansion_concat (s1->data,
			                                         item->data));

			if (s1->next)
			{
				item = g_slist_next (item);
			}
			else
			{
				cpg_expansion_free (item->data);
				item = g_slist_delete_link (item, item);
			}
		}

		cpg_expansion_free (s1->data);
		s1 = g_slist_delete_link (s1, s1);
	}

	return g_slist_reverse (ret);
}

static GSList *expand_id_recurse (gchar const **id, gchar const *endings);

static void
expansion_shift (CpgExpansion *expansion)
{
	gint i;

	cpg_expansion_add (expansion, "");

	for (i = cpg_expansion_num (expansion) - 2; i >= 0; --i)
	{
		cpg_expansion_set (expansion,
		                   i + 1,
		                   cpg_expansion_get (expansion, i));
	}
}

static GSList *
parse_expansion (gchar const **id)
{
	GSList *ret = NULL;

	while (**id)
	{
		GSList *items;
		GSList *it;

		items = expand_id_recurse (id, ",}");

		for (it = items; it; it = g_slist_next (it))
		{
			expansion_shift (it->data);
		}

		ret = g_slist_concat (ret, items);

		if (**id)
		{
			if (*((*id)++) == '}')
			{
				break;
			}
		}
	}

	return ret;
}

GSList *
expand_id_recurse (gchar const **id,
                   gchar const *endings)
{
	GSList *ret = NULL;
	gchar const *ptr = *id;

	while (**id && strchr (endings, **id) == NULL)
	{
		if (**id == '{')
		{
			GSList *ex;
			gint len = *id - ptr;

			++*id;

			/* Recursively parse the expansions */
			ex = parse_expansion (id);

			/* Prepend what we got till now */
			expansions_prepend (ex, ptr, len);

			/* Concatenate the expansions */
			ret = expansions_concat (ret, ex);
			ptr = *id;
		}
		else if (**id)
		{
			++*id;
		}
	}

	if (ptr != *id)
	{
		if (ret != NULL)
		{
			expansions_append (ret, ptr, *id - ptr);
		}
		else
		{
			ret = parse_expansion_range (ptr, *id - ptr);
		}
	}

	return ret;
}

GSList *
cpg_network_parser_utils_expand_id (gchar const *id)
{
	return expand_id_recurse (&id, "\0");
}
