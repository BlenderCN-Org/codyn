/*
 * cpg-render.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-layoutable.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>
#include <gio/gunixinputstream.h>

static gchar *output_file = NULL;
static gchar *select_root = NULL;
static gboolean output_dot = TRUE;
static gboolean output_tikz = FALSE;
static gboolean use_labels = FALSE;
static gboolean no_preview = FALSE;

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"dot", 'd', 0, G_OPTION_ARG_NONE, &output_dot, "Output a DOT file (default)", NULL},
	{"tikz", 't', 0, G_OPTION_ARG_NONE, &output_tikz, "Output a TiKz file", NULL},
	{"root", 'r', 0, G_OPTION_ARG_STRING, &select_root, "Select root group to output", NULL},
	{"labels", 'l', 0, G_OPTION_ARG_NONE, &use_labels, "Use labels in nodes", NULL},
	{"no-preview", 0, 0, G_OPTION_ARG_NONE, &no_preview, "Do not create preview file for TiKz output", NULL},
	{NULL}
};

static CpgNetwork *
parse_network (gchar const *filename, GError **error)
{
	CpgNetwork *network;
	gboolean fromstdin;

	fromstdin = (filename == NULL || g_strcmp0 (filename, "-") == 0);

	if (!fromstdin)
	{
		GFile *file;

		file = g_file_new_for_commandline_arg (filename);

		if (!g_file_query_exists (file, NULL))
		{
			g_printerr ("Could not open file: %s\n", filename);
			g_object_unref (file);

			return NULL;
		}

		network = cpg_network_new_from_file (file, error);
		g_object_unref (file);
	}
	else
	{
		GInputStream *stream;

		stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);

		network = cpg_network_new_from_stream (stream,
		                                       error);

		g_object_unref (stream);
	}

	return network;
}

static GDataOutputStream *
create_output_stream (GFile        *input_file,
                      gchar const  *suffix,
                      gchar       **name,
                      GError      **error)
{
	GOutputStream *ret;
	GDataOutputStream *data;

	if ((output_file == NULL && input_file == NULL) ||
	     g_strcmp0 (output_file, "-") == 0)
	{
		ret = g_unix_output_stream_new (STDOUT_FILENO, TRUE);

		if (name)
		{
			*name = NULL;
		}
	}
	else
	{
		GFile *file;
		gchar *b;
		gchar *p;
		gchar *prefix;

		if (output_file)
		{
			prefix = g_strdup (output_file);
		}
		else
		{
			b = g_file_get_basename (input_file);
			p = strrchr (b, '.');

			if (p != NULL)
			{
				prefix = g_strndup (b, p - b);
			}
			else
			{
				prefix = g_strdup (b);
			}

			g_free (b);
		}

		b = g_strconcat (prefix, ".", suffix, NULL);

		file = g_file_new_for_path (b);

		if (name)
		{
			*name = g_strdup (prefix);
		}

		g_free (b);
		g_free (prefix);

		ret = G_OUTPUT_STREAM (g_file_replace (file, NULL, FALSE, G_FILE_CREATE_NONE, NULL, error));

		g_object_unref (file);
	}

	if (!ret)
	{
		return NULL;
	}

	data = g_data_output_stream_new (ret);
	g_object_unref (ret);

	return data;
}

#define write_stream(s) if (!g_data_output_stream_put_string (stream, s, NULL, error)) { return FALSE; }
#define write_stream_printf(format, args...)						\
do {										\
	gchar *t = g_strdup_printf (format, args);				\
	write_stream (t);							\
	g_free (t);								\
} while (0);

#define write_stream_nl(s) write_stream (s); write_stream ("\n")

static gboolean
get_location (CpgObject *o, gint *x, gint *y)
{
	CpgLayoutable *l;

	if (!CPG_IS_LAYOUTABLE (o))
	{
		return FALSE;
	}

	l = CPG_LAYOUTABLE (o);

	if (!cpg_layoutable_supports_location (l))
	{
		return FALSE;
	}

	cpg_layoutable_get_location (l, x, y);
	return TRUE;
}

static gboolean
output_to_dot (CpgNetwork  *network,
               CpgGroup    *root,
               GError     **error)
{
	GDataOutputStream *stream;
	GSList const *children;
	GSList *links = NULL;
	GFile *file;

	file = cpg_network_get_file (network);
	stream = create_output_stream (file, "dot", NULL, error);

	if (file)
	{
		g_object_unref (file);
	}

	if (!stream)
	{
		return FALSE;
	}

	write_stream_nl ("strict digraph cpg {");

	children = cpg_group_get_children (root);

	while (children)
	{
		CpgObject *child;
		gint x;
		gint y;

		child = children->data;
		children = g_slist_next (children);

		if (CPG_IS_LINK (child))
		{
			links = g_slist_prepend (links, child);
			continue;
		}

		write_stream_printf ("\t%s [label=\"%s\"",
		                     cpg_object_get_id (child),
		                     use_labels ? cpg_object_get_id (child) : "");

		if (get_location (child, &x, &y))
		{
			write_stream_printf (",pos=\"%d,%d!\",pin=true", x, -y);
		}

		write_stream_nl ("];");
	}

	links = g_slist_reverse (links);

	if (links)
	{
		GSList *item;

		write_stream_nl ("");

		for (item = links; item; item = g_slist_next (item))
		{
			CpgLink *link;
			CpgGroup *from;
			CpgGroup *to;

			link = item->data;

			from = cpg_link_get_from (link);
			to = cpg_link_get_to (link);

			if (from && to)
			{
				write_stream_printf ("\t%s -> %s;\n",
				                     cpg_object_get_id (CPG_OBJECT (from)),
				                     cpg_object_get_id (CPG_OBJECT (to)));
			}
		}

		g_slist_free (links);
	}

	write_stream_nl ("}");

	g_object_unref (stream);

	return TRUE;
}

#define write_style(name,val) write_stream_printf ("\t\\tikzstyle{%s custom}+=[]\n\t\\tikzstyle{%s}=[%s,%s custom]\n\n", name, name, val, name)

static gboolean
tikz_styles (CpgGroup           *root,
             GDataOutputStream  *stream,
             GError            **error)
{
	GSList const *children;
	GHashTable *table;

	write_style ("state", "circle, draw, minimum size=6mm");
	write_style ("coupling", "max distance=6mm, -stealth'");
	write_style ("sinewave", "scale=0.5,semithick");

	write_stream_nl ("\t\\tikzstyle{bends custom}+=[]");
	write_stream_nl ("\t\\tikzset{bends/.style={bend left=##1,bends custom=##1}}");
	write_stream_nl ("");

	children = cpg_group_get_children (root);

	table = g_hash_table_new_full (g_str_hash,
	                               g_str_equal,
	                               (GDestroyNotify)g_free,
	                               NULL);

	while (children)
	{
		CpgObject *obj = children->data;
		GSList const *temps = cpg_object_get_applied_templates (obj);

		write_stream_printf ("\t\\tikzstyle{%s %s}+=[]\n",
		                     CPG_IS_LINK (obj) ? "link" : "state",
		                     cpg_object_get_id (obj));

		while (temps)
		{
			CpgObject *t = temps->data;
			gchar *id;

			id = cpg_object_get_full_id (t);

			if (!g_hash_table_lookup (table, id))
			{
				write_stream_printf ("\t\\tikzstyle{template %s}+=[]\n", id);
				g_hash_table_insert (table, id, GINT_TO_POINTER (1));
			}
			else
			{
				g_free (id);
			}

			temps = g_slist_next (temps);
		}

		children = g_slist_next (children);
	}

	g_hash_table_destroy (table);

	write_stream_nl ("");

	return TRUE;
}

typedef struct
{
	CpgLink *link;
	gchar const *bendname;
	gint offset;
} LinkInfo;

static LinkInfo *
link_info_new (CpgLink     *link,
               gchar const *bendname,
               gint         offset)
{
	LinkInfo *ret;

	ret = g_slice_new0 (LinkInfo);

	ret->link = link;
	ret->bendname = bendname;
	ret->offset = offset;

	return ret;
}

static void
link_info_free (LinkInfo *self)
{
	g_slice_free (LinkInfo, self);
}

static GSList *
calculate_link_info (GSList *links)
{
	GSList *ret = NULL;
	GHashTable *offset;
	GHashTable *fromto;

	offset = g_hash_table_new_full (g_str_hash,
	                                g_str_equal,
	                                (GDestroyNotify)g_free,
	                                NULL);

	fromto = g_hash_table_new_full (g_str_hash,
	                                g_str_equal,
	                                (GDestroyNotify)g_free,
	                                NULL);

	while (links)
	{
		CpgLink *link;
		CpgGroup *from;
		CpgGroup *to;
		LinkInfo *other;
		LinkInfo *info;

		gchar *ptr;
		gchar *optr;
		gint ofs;

		link = links->data;
		links = g_slist_next (links);

		from = cpg_link_get_from (link);
		to = cpg_link_get_to (link);

		if (!from || !to)
		{
			continue;
		}

		ptr = g_strconcat (cpg_object_get_id (CPG_OBJECT (from)),
		                   "##",
		                   cpg_object_get_id (CPG_OBJECT (to)),
		                   NULL);

		optr = g_strconcat (cpg_object_get_id (CPG_OBJECT (to)),
		                    "##",
		                    cpg_object_get_id (CPG_OBJECT (from)),
		                    NULL);

		other = g_hash_table_lookup (fromto,
		                             optr);

		if (other)
		{
			ofs = 1;
			other->offset = 1;

			g_hash_table_remove (fromto, optr);
		}
		else
		{
			ofs = GPOINTER_TO_INT (g_hash_table_lookup (offset, ptr));
		}

		info = link_info_new (link, "bend left", ofs);
		ret = g_slist_prepend (ret, info);

		if (ofs == 0)
		{
			g_hash_table_insert (fromto, g_strdup (ptr), info);
		}

		g_free (optr);

		g_hash_table_insert (offset, ptr, GINT_TO_POINTER (ofs + 1));
	}

	g_hash_table_destroy (fromto);
	g_hash_table_destroy (offset);

	return g_slist_reverse (ret);
}

static gchar *
object_styles (CpgObject *obj)
{
	GString *ret;
	GSList const *temps;

	ret = g_string_new ("");

	temps = cpg_object_get_applied_templates (obj);

	while (temps)
	{
		CpgObject *t;
		gchar *id;

		t = temps->data;

		if (ret->len != 0)
		{
			g_string_append_c (ret, ',');
		}

		g_string_append (ret, "template ");

		id = cpg_object_get_full_id (t);
		g_string_append (ret, id);
		g_free (id);

		temps = g_slist_next (temps);
	}

	if (ret->len != 0)
	{
		g_string_append_c (ret, ',');
	}

	if (CPG_IS_LINK (obj))
	{
		g_string_append (ret, "link ");
	}
	else
	{
		g_string_append (ret, "state ");
	}

	g_string_append (ret, cpg_object_get_id (obj));

	return g_string_free (ret, FALSE);
}

static gboolean
output_to_tikz (CpgNetwork  *network,
                CpgGroup    *root,
                GError     **error)
{
	GDataOutputStream *stream;
	GFile *file;
	GSList const *children;
	GSList *links = NULL;
	GSList *infos;
	gchar *name;

	file = cpg_network_get_file (network);

	if (!no_preview)
	{
		stream = create_output_stream (file, "tex", &name, error);

		if (!stream)
		{
			if (file)
			{
				g_object_unref (file);
			}

			return FALSE;
		}

		write_stream_nl ("\\documentclass{article}"
"\\usepackage{tikz}\n"
"\\usepackage[active,tightpage]{preview}\n"
"\n"
"\\usetikzlibrary{calc,positioning,arrows,shapes}\n"
"\n"
"\\PreviewEnvironment{tikzpicture}\n"
"\\setlength\\PreviewBorder{5pt}\n");

		if (name)
		{
			write_stream_printf ("\\include{%s.inc}\n\n", name);
		}

		write_stream_nl ("\\begin{document}\n"
"	\\begin{tikzpicture}\n"
"		\\rendercpg[20]\n"
"	\\end{tikzpicture}\n"
"\\end{document}");

		if (name)
		{
			g_object_unref (stream);
			stream = create_output_stream (file, "inc.tex", NULL, error);
		}

		g_free (name);
	}
	else
	{
		stream = create_output_stream (file, "inc.tex", NULL, error);
	}

	if (file)
	{
		g_object_unref (file);
	}

	if (!stream)
	{
		return FALSE;
	}

	write_stream_nl ("\\newcommand{\\cpgconnect}[4] {");
	write_stream_nl ("\t\\path (#1) edge [coupling,bends=#3,#4] (#2);");
	write_stream_nl ("}\n");

	write_stream_nl ("\\newcommand{\\cpgbendandconnect}[5] {");
	write_stream_nl ("\t\\pgfmathsetmacro{\\ThisBend}{#3 * #4}");
	write_stream_nl ("\t\\cpgconnect{#1}{#2}{\\ThisBend}{#5}");
	write_stream_nl ("}\n");

	write_stream_nl ("\\newcommand{\\cpgconnectself}[4] {");
	write_stream_nl ("\t\\path (#1) edge [loop above,in=60,out=120,min distance=#3cm,coupling,#4] (#2);");
	write_stream_nl ("}\n");

	write_stream_nl ("\\newcommand{\\cpgbendandconnectself}[4] {");
	write_stream_nl ("\t\\pgfmathsetmacro{\\ThisLength}{(#3 + 1) * 0.5}");
	write_stream_nl ("\t\\cpgconnectself{#1}{#2}{\\ThisLength}{#4}");
	write_stream_nl ("}\n");

	write_stream_nl ("\\newcommand{\\rendercpg}[1][30]{");
	write_stream_nl ("\t\\def\\Bending{#1}");

	if (!tikz_styles (root, stream, error))
	{
		return FALSE;
	}

	children = cpg_group_get_children (root);

	while (children)
	{
		CpgObject *child;
		gint x;
		gint y;
		gchar *styles;

		child = children->data;
		children = g_slist_next (children);

		if (CPG_IS_LINK (child))
		{
			links = g_slist_prepend (links, child);
			continue;
		}

		styles = object_styles (child);

		write_stream_printf ("\t\\node[state,%s] (%s)",
		                     styles,
		                     cpg_object_get_id (child));

		g_free (styles);

		if (get_location (child, &x, &y))
		{
			write_stream_printf (" at (%d, %d)", x, -y);
		}

		write_stream_printf (" {%s}",
		                     use_labels ? cpg_object_get_id (child) : "");

		write_stream_nl (";");

		write_stream_printf ("\t\\draw [sinewave,x=1.57ex,y=1ex] ($(%s.center) - (2,0)$) sin +(1,1) cos +(1,-1) sin +(1,-1) cos +(1,1);\n\n", cpg_object_get_id (child));

	}

	links = g_slist_reverse (links);
	infos = calculate_link_info (links);
	g_slist_free (links);

	for (links = infos; links; links = g_slist_next (links))
	{
		LinkInfo *info;
		gchar *styles;

		info = links->data;

		styles = object_styles (CPG_OBJECT (info->link));

		if (cpg_link_get_from (info->link) ==
		    cpg_link_get_to (info->link))
		{
			write_stream_printf ("\t\\cpgbendandconnectself{%s}{%s}{%d}{%s}\n",
			                     cpg_object_get_id (CPG_OBJECT (cpg_link_get_from (info->link))),
			                     cpg_object_get_id (CPG_OBJECT (cpg_link_get_to (info->link))),
			                     info->offset,
			                     styles);
		}
		else
		{
			write_stream_printf ("\t\\cpgbendandconnect{%s}{%s}{%d}{\\Bending}{%s}\n",
			                     cpg_object_get_id (CPG_OBJECT (cpg_link_get_from (info->link))),
			                     cpg_object_get_id (CPG_OBJECT (cpg_link_get_to (info->link))),
			                     info->offset,
			                     styles);
		}

		g_free (styles);
	}

	write_stream_nl ("}");

	g_slist_foreach (infos, (GFunc)link_info_free, NULL);
	g_slist_free (infos);

	g_object_unref (stream);

	return TRUE;
}

static gboolean
generate (gchar const  *filename,
          GError      **error)
{
	CpgNetwork *network;
	CpgGroup *root;
	gboolean ret;

	network = parse_network (filename, error);

	if (!network)
	{
		return FALSE;
	}

	if (select_root)
	{
		CpgObject *obj;

		obj = cpg_group_find_object (CPG_GROUP (network),
		                             select_root);

		if (!obj)
		{
			g_printerr ("Could not find root `%s'", select_root);
			g_object_unref (network);
			return FALSE;
		}
		else if (!CPG_IS_GROUP (obj))
		{
			g_printerr ("The selected root `%s' is not a group", select_root);
			g_object_unref (network);
			return FALSE;
		}

		root = CPG_GROUP (obj);
	}
	else
	{
		root = CPG_GROUP (network);
	}

	if (output_tikz)
	{
		ret = output_to_tikz (network, root, error);
	}
	else
	{
		ret = output_to_dot (network, root, error);
	}

	g_object_unref (network);

	return ret;
}

int
main (int argc, char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gboolean ret;
	gint i;

	g_type_init ();

	ctx = g_option_context_new ("[FILES...] - render cpg network");
	g_option_context_add_main_entries (ctx, entries, NULL);

	ret = g_option_context_parse (ctx, &argc, &argv, &error);

	if (!ret)
	{
		g_printerr ("Failed to parse options:%s\n", error->message);
		g_error_free (error);

		return 1;
	}

	for (i = 1; i < argc; ++i)
	{
		if (!generate (argv[i], &error))
		{
			g_printerr ("Failed to parse network: %s\n", error->message);
			g_error_free (error);

			return 1;
		}
	}

	if (argc == 1)
	{
		if (!generate (NULL, &error))
		{
			g_printerr ("Failed to parse network: %s\n", error->message);
			g_error_free (error);

			return 1;
		}
	}

	return 0;
}
