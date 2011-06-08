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

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"dot", 'd', 0, G_OPTION_ARG_NONE, &output_dot, "Output a DOT file (default)", NULL},
	{"tikz", 't', 0, G_OPTION_ARG_NONE, &output_tikz, "Output a TiKz file", NULL},
	{"root", 'r', 0, G_OPTION_ARG_STRING, &select_root, "Select root group to output", NULL},
	{"labels", 'l', 0, G_OPTION_ARG_NONE, &use_labels, "Use labels in nodes", NULL},
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
                      GError      **error)
{
	GOutputStream *ret;
	GDataOutputStream *data;

	if ((output_file == NULL && input_file == NULL) ||
	     g_strcmp0 (output_file, "-") == 0)
	{
		ret = g_unix_output_stream_new (STDOUT_FILENO, TRUE);
	}
	else
	{
		GFile *file;

		if (output_file)
		{
			file = g_file_new_for_commandline_arg (output_file);
		}
		else
		{
			gchar *b;
			gchar *p;
			gchar *prefix;

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
			b = g_strconcat (prefix, ".", suffix, NULL);

			file = g_file_new_for_path (b);

			g_free (b);
		}

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
	stream = create_output_stream (file, "dot", error);

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
			x *= 1;
			y *= -1;

			write_stream_printf (",pos=\"%d,%d!\",pin=true", x, y);
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
			CpgObject *from;
			CpgObject *to;

			link = item->data;

			from = cpg_link_get_from (link);
			to = cpg_link_get_to (link);

			if (from && to)
			{
				write_stream_printf ("\t%s -> %s;\n",
				                     cpg_object_get_id (from),
				                     cpg_object_get_id (to));
			}
		}

		g_slist_free (links);
	}

	write_stream_nl ("}");

	g_object_unref (stream);

	return TRUE;
}

static gboolean
output_to_tikz (CpgNetwork  *network,
                CpgGroup    *root,
                GError     **error)
{
	GDataOutputStream *stream;
	GFile *file;

	file = cpg_network_get_file (network);
	stream = create_output_stream (file, "tex", error);

	if (file)
	{
		g_object_unref (file);
	}

	if (!stream)
	{
		return FALSE;
	}

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

	if (output_dot)
	{
		ret = output_to_dot (network, root, error);
	}
	else
	{
		ret = output_to_tikz (network, root, error);
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
