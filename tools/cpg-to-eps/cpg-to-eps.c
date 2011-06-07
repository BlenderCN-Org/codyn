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
static gboolean preserve_format = FALSE;
static gboolean no_compile = FALSE;

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"dot", 'd', 0, G_OPTION_ARG_NONE, &output_dot, "Output a DOT file (default)", NULL},
	{"tikz", 't', 0, G_OPTION_ARG_NONE, &output_tikz, "Output a TiKz file", NULL},
	{"preserve", 'p', 0, G_OPTION_ARG_NONE, &preserve_format, "Preserve the generated formatted file", NULL},
	{"no-compile", 'n', 0, G_OPTION_ARG_NONE, &no_compile, "Do not compile to EPS (implies --preserve)", NULL},
	{"root", 'r', 0, G_OPTION_ARG_STRING, &select_root, "Select root group to output", NULL},
	{NULL}
};

static CpgNetwork *
parse_network (gchar const *args[], gint argc, GError **error)
{
	CpgNetwork *network;
	gboolean fromstdin;

	fromstdin = (argc == 0 || g_strcmp0 (args[0], "-") == 0);

	if (!fromstdin)
	{
		GFile *file;

		file = g_file_new_for_commandline_arg (args[0]);

		if (!g_file_query_exists (file, NULL))
		{
			g_printerr ("Could not open file: %s\n", args[0]);
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
create_output_stream (GError **error)
{
	GOutputStream *ret;
	GDataOutputStream *data;

	if (output_file == NULL || (output_file[0] == '-' && !output_file[1]))
	{
		ret = g_unix_output_stream_new (STDOUT_FILENO, TRUE);
	}
	else
	{
		GFile *file;

		file = g_file_new_for_commandline_arg (output_file);
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
output_to_dot (CpgNetwork  *network,
               CpgGroup    *root,
               GError     **error)
{
	GDataOutputStream *stream;
	GSList const *children;
	GSList *links = NULL;

	stream = create_output_stream (error);

	if (!stream)
	{
		return FALSE;
	}

	write_stream_nl ("digraph cpg {");

	children = cpg_group_get_children (root);

	while (children)
	{
		CpgObject *child;

		child = children->data;
		children = g_slist_next (children);

		if (CPG_IS_LINK (child))
		{
			links = g_slist_prepend (links, child);
			continue;
		}

		write_stream_printf ("\t%s [label=\"%s\"",
		                     cpg_object_get_id (child),
		                     cpg_object_get_id (child));

		if (CPG_IS_LAYOUTABLE (child))
		{
			CpgLayoutable *l;

			l = CPG_LAYOUTABLE (child);

			if (cpg_layoutable_supports_location (l))
			{
				gint x;
				gint y;

				cpg_layoutable_get_location (l, &x, &y);

				x *= 1;
				y *= -1;

				write_stream_printf (",pos=\"%d,%d!\",pin=true", x, y);
			}
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

	stream = create_output_stream (error);

	if (!stream)
	{
		return FALSE;
	}

	g_object_unref (stream);

	return TRUE;
}

int
main (int argc, char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gboolean ret;
	CpgNetwork *network;
	CpgGroup *root;

	g_type_init ();

	ctx = g_option_context_new ("<NETWORK> - parse cpg network");
	g_option_context_add_main_entries (ctx, entries, NULL);

	ret = g_option_context_parse (ctx, &argc, &argv, &error);

	if (!ret)
	{
		g_printerr ("Failed to parse options:%s\n", error->message);
		g_error_free (error);

		return 1;
	}

	network = parse_network ((gchar const **)(argv + 1), argc - 1, &error);

	if (!network)
	{
		if (error)
		{
			g_printerr ("Failed to parse network: %s\n", error->message);
			g_error_free (error);
		}

		return 1;
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
			return 1;
		}
		else if (!CPG_IS_GROUP (obj))
		{
			g_printerr ("The selected root `%s' is not a group", select_root);
			g_object_unref (network);
			return 1;
		}

		root = CPG_GROUP (obj);
	}
	else
	{
		root = CPG_GROUP (network);
	}

	if (output_dot)
	{
		ret = output_to_dot (network, root, &error);
	}
	else
	{
		ret = output_to_tikz (network, root, &error);
	}

	g_object_unref (network);

	if (!ret)
	{
		if (error)
		{
			g_printerr ("Failed: %s", error->message);
			g_error_free (error);
		}

		return 1;
	}

	return 0;
}
