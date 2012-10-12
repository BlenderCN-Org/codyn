/*
 * cdn-render.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <codyn/codyn.h>
#include <codyn/cdn-layoutable.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <codyn/cdn-cfile-stream.h>
#include <math.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_GIO_UNIX
#include <gio/gunixinputstream.h>
#endif

static gchar *output_file = NULL;
static gchar *select_root = NULL;
static gboolean output_tikz = TRUE;
static gboolean output_dot = FALSE;
static gboolean use_labels = FALSE;

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file basename (default is based on input file)", "FILE"},
	{"tikz", 't', 0, G_OPTION_ARG_NONE, &output_tikz, "Output a TikZ file (default)", NULL},
	{"dot", 'd', 0, G_OPTION_ARG_NONE, &output_dot, "Output a DOT file", NULL},
	{"root", 'r', 0, G_OPTION_ARG_STRING, &select_root, "Select root group to output", NULL},
	{"labels", 'l', 0, G_OPTION_ARG_NONE, &use_labels, "Use labels in nodes (no effect for TikZ)", NULL},
	{NULL}
};

static CdnNetwork *
parse_network (gchar const *filename, GError **error)
{
	CdnNetwork *network;
	gboolean fromstdin;

#ifdef ENABLE_GIO_UNIX
	fromstdin = (filename == NULL || g_strcmp0 (filename, "-") == 0);
#else
	fromstdin = FALSE;
#endif

	if (!fromstdin)
	{
		GFile *file;

		if (!filename)
		{
			g_printerr ("Please provide an input file\n");
			return NULL;
		}

		file = g_file_new_for_commandline_arg (filename);

		if (!g_file_query_exists (file, NULL))
		{
			g_printerr ("Could not open file: %s\n", filename);
			g_object_unref (file);

			return NULL;
		}

		network = cdn_network_new_from_file (file, error);
		g_object_unref (file);
	}
#ifdef ENABLE_GIO_UNIX
	else
	{
		GInputStream *stream;

		stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);

		network = cdn_network_new_from_stream (stream,
		                                       error);

		g_object_unref (stream);
	}
#endif

	return network;
}

static GDataOutputStream *
create_output_stream (GFile        *input_file,
                      gchar const  *suffix,
                      gboolean      overwrite,
                      gchar       **name,
                      GError      **error)
{
	GOutputStream *ret;
	GDataOutputStream *data;

	if ((output_file == NULL && input_file == NULL) ||
	     g_strcmp0 (output_file, "-") == 0)
	{
		ret = cdn_cfile_stream_new (stdout);

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

		if (g_file_query_exists (file, NULL) && !overwrite)
		{
			ret = NULL;
		}
		else
		{
			ret = G_OUTPUT_STREAM (g_file_replace (file, NULL, FALSE, G_FILE_CREATE_NONE, NULL, error));
		}

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
get_location (CdnObject *o, gint *x, gint *y)
{
	CdnLayoutable *l;

	if (!CDN_IS_LAYOUTABLE (o))
	{
		return FALSE;
	}

	l = CDN_LAYOUTABLE (o);

	if (!cdn_layoutable_supports_location (l))
	{
		return FALSE;
	}

	cdn_layoutable_get_location (l, x, y);
	return TRUE;
}

static gboolean
output_to_dot (CdnNetwork  *network,
               CdnNode    *root,
               GError     **error)
{
	GDataOutputStream *stream;
	GSList const *children;
	GSList *links = NULL;
	GFile *file;

	file = cdn_network_get_file (network);
	stream = create_output_stream (file, "dot", TRUE, NULL, error);

	if (file)
	{
		g_object_unref (file);
	}

	if (!stream)
	{
		return FALSE;
	}

	write_stream_nl ("strict digraph cdn {");

	children = cdn_node_get_children (root);

	while (children)
	{
		CdnObject *child;
		gint x;
		gint y;

		child = children->data;
		children = g_slist_next (children);

		if (CDN_IS_EDGE (child))
		{
			links = g_slist_prepend (links, child);
			continue;
		}

		write_stream_printf ("\t%s [label=\"%s\"",
		                     cdn_object_get_id (child),
		                     use_labels ? cdn_object_get_id (child) : "");

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
			CdnEdge *link;
			CdnNode *from;
			CdnNode *to;

			link = item->data;

			from = cdn_edge_get_input (link);
			to = cdn_edge_get_output (link);

			if (from && to)
			{
				write_stream_printf ("\t%s -> %s;\n",
				                     cdn_object_get_id (CDN_OBJECT (from)),
				                     cdn_object_get_id (CDN_OBJECT (to)));
			}
		}

		g_slist_free (links);
	}

	write_stream_nl ("}");

	g_object_unref (stream);

	return TRUE;
}

typedef struct
{
	CdnEdge *link;
	gint offset;
} LinkInfo;

static LinkInfo *
link_info_new (CdnEdge     *link,
               gint         offset)
{
	LinkInfo *ret;

	ret = g_slice_new0 (LinkInfo);

	ret->link = link;
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
		CdnEdge *link;
		CdnNode *from;
		CdnNode *to;
		LinkInfo *other;
		LinkInfo *info;

		gchar *ptr;
		gchar *optr;
		gint ofs;

		link = links->data;
		links = g_slist_next (links);

		from = cdn_edge_get_input (link);
		to = cdn_edge_get_output (link);

		if (!from || !to)
		{
			continue;
		}

		ptr = g_strconcat (cdn_object_get_id (CDN_OBJECT (from)),
		                   "##",
		                   cdn_object_get_id (CDN_OBJECT (to)),
		                   NULL);

		optr = g_strconcat (cdn_object_get_id (CDN_OBJECT (to)),
		                    "##",
		                    cdn_object_get_id (CDN_OBJECT (from)),
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

		info = link_info_new (link, ofs);
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
object_styles (CdnObject *obj)
{
	GString *ret;
	GSList const *temps;

	ret = g_string_new ("");

	temps = cdn_object_get_applied_templates (obj);

	while (temps)
	{
		CdnObject *t;
		gchar *id;

		t = temps->data;

		if (ret->len != 0)
		{
			g_string_append_c (ret, ',');
		}

		g_string_append (ret, "template ");

		id = cdn_object_get_full_id (t);
		g_string_append (ret, id);
		g_free (id);

		g_string_append (ret, "/.try");

		temps = g_slist_next (temps);
	}

	if (ret->len != 0)
	{
		g_string_append_c (ret, ',');
	}

	if (CDN_IS_EDGE (obj))
	{
		g_string_append (ret, "edge ");
	}
	else
	{
		g_string_append (ret, "node ");
	}

	g_string_append (ret, cdn_object_get_id (obj));
	g_string_append (ret, "/.try");

	return g_string_free (ret, FALSE);
}

static gchar const *
calculate_label_position (CdnNode *node)
{
	/* enum {EAST, NORTH, WEST, SOUTH, N_POS}; */
	enum {EAST, NORTH_EAST, NORTH, NORTH_WEST, WEST, SOUTH_WEST, SOUTH, SOUTH_EAST, N_POS};
	
	gchar const *names[N_POS] = {"east", "north east", "north", "north west", "west", "south west", "south", "south east"};
	gdouble weights[N_POS] = {0,};
	GSList const *edges = cdn_node_get_edges (node);

	GSList const *item;

	CdnEdge *self = NULL;

	if (cdn_node_has_self_edge (node))
	{
		self = cdn_node_get_self_edge (node);
	}

	for (item = edges; item; item = g_slist_next (item))
	{
		CdnNode *input = cdn_edge_get_input (CDN_EDGE (item->data));
		CdnNode *output = cdn_edge_get_output (CDN_EDGE (item->data));

		if (input == output)
		{
			if (item->data != self)
			{
				weights[SOUTH] += 2 * log (M_PI); /* counts as two edges */
			}
			continue;
		}

		gint x1;
		gint x2;
		gint y1;
		gint y2;

		get_location (CDN_OBJECT (node), &x1, &y1);
		get_location (CDN_OBJECT (node == input ? output : input), &x2, &y2);

		double angle = atan2 (y1 - y2, x2 - x1); /* y1 - y2 as Y axis points downwards in Codyn */

		gint i;
		
		for (i = 0; i < N_POS; ++i)
		{
			double pos = 2 * M_PI * i / N_POS;
			double dist = pos - angle;
			
			/* get a value in (-pi, pi) */
			dist = fmod (dist + M_PI, 2 * M_PI) - M_PI;

			weights[i] += log (fabs (dist));
		}
	}

	gint i;
	gint imax = 0;

	for (i = 1; i < N_POS; ++i)
	{
		if (weights[i] > weights[imax])
		{
			imax = i;
		}
	}

	return names[imax];
}


static gboolean
output_to_tikz (CdnNetwork  *network,
                CdnNode    *root,
                GError     **error)
{
	GDataOutputStream *stream;
	GFile *file;
	GSList const *children;
	GSList *links = NULL;
	GSList *infos;
	gchar *name;

	file = cdn_network_get_file (network);

	stream = create_output_stream (file, "tex", FALSE, &name, error);
	if (stream)
	{
		write_stream_nl ("\\documentclass{article}\n"
		                 "\\usepackage{codyn}\n"
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
"		\\rendercodyn\n"
"	\\end{tikzpicture}\n"
"\\end{document}");

		if (name)
		{
			g_object_unref (stream);
		}

		g_free (name);
	}

	stream = create_output_stream (file, "inc.tex", TRUE, NULL, error);

	if (file)
	{
		g_object_unref (file);
	}

	if (!stream)
	{
		return FALSE;
	}

	write_stream_nl ("\\newcommand{\\rendercodyn}[1][]{");
	write_stream_nl ("\t\\begin{scope}[#1]");

	children = cdn_node_get_children (root);

	while (children)
	{
		CdnObject *child;
		gint x;
		gint y;
		gchar *styles;

		child = children->data;
		children = g_slist_next (children);

		if (CDN_IS_EDGE (child))
		{
			links = g_slist_prepend (links, child);
			continue;
		}

		styles = object_styles (child);

		write_stream_printf ("\t\t\\cdnnode[cdn node,%s] (%s)",
		                     styles,
		                     cdn_object_get_id (child));

		g_free (styles);

		if (get_location (child, &x, &y))
		{
			write_stream_printf (" at (%d, %d)", x, -y);
		}

		write_stream_nl ("");
	}

	write_stream_nl ("");

	links = g_slist_reverse (links);
	infos = calculate_link_info (links);
	g_slist_free (links);

	for (links = infos; links; links = g_slist_next (links))
	{
		LinkInfo *info;
		gchar *styles;

		info = links->data;

		styles = object_styles (CDN_OBJECT (info->link));

		write_stream_printf ("\t\t\\cdnconnect[%scdn offset=%d,cdn edge,%s] (%s) from (%s) to (%s)\n",
		                     cdn_edge_get_input (info->link) == cdn_edge_get_output (info->link) ? "cdn self," : "",
		                     info->offset,
		                     styles,
		                     cdn_object_get_id (CDN_OBJECT (info->link)),
		                     cdn_object_get_id (CDN_OBJECT (cdn_edge_get_input (info->link))),
		                     cdn_object_get_id (CDN_OBJECT (cdn_edge_get_output (info->link))));

		g_free (styles);
	}

	write_stream_nl ("");
	
	children = cdn_node_get_children (root);

	while (children)
	{
		CdnObject *child;
		gchar *styles;

		child = children->data;
		children = g_slist_next (children);

		if (!CDN_IS_NODE (child))
		{
			continue;
		}
		styles = object_styles (child);

		write_stream_printf ("\t\t\\cdnlabel[label position=%s,cdn node,%s] (%s)\n",
		                     calculate_label_position (CDN_NODE (child)),
		                     styles,
		                     cdn_object_get_id (child));

		g_free (styles);
	}

	write_stream_nl ("\t\\end{scope}");
	write_stream_nl ("}");
	write_stream_nl ("");

	g_slist_foreach (infos, (GFunc)link_info_free, NULL);
	g_slist_free (infos);

	g_object_unref (stream);

	return TRUE;
}

static gboolean
generate (gchar const  *filename,
          GError      **error)
{
	CdnNetwork *network;
	CdnNode *root;
	gboolean ret;

	network = parse_network (filename, error);

	if (!network)
	{
		return FALSE;
	}

	if (select_root)
	{
		CdnObject *obj;

		obj = cdn_node_find_object (CDN_NODE (network),
		                             select_root);

		if (!obj)
		{
			g_printerr ("Could not find root `%s'", select_root);
			g_object_unref (network);
			return FALSE;
		}
		else if (!CDN_IS_NODE (obj))
		{
			g_printerr ("The selected root `%s' is not a group", select_root);
			g_object_unref (network);
			return FALSE;
		}

		root = CDN_NODE (obj);
	}
	else
	{
		root = CDN_NODE (network);
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

	ctx = g_option_context_new ("[FILES...] - render cdn network");
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
