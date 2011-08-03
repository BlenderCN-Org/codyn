#include <cpg-network/cpg-parser-context.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>
#include <gio/gunixinputstream.h>
#include <cpg-network/cpg-network-serializer.h>
#include <termcap.h>
#include <sys/time.h>
#include "cpg-readline-stream.h"

static gchar *output_file;
static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GSList *defines = NULL;
static gint64 seed = 0;

static gboolean
add_define (gchar const  *option_name,
            gchar const  *value,
            gpointer      data,
            GError      **error)
{
	defines = g_slist_prepend (defines, g_strdup (value));

	return TRUE;
}

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors, "Do not use colors in the output", NULL},
	{"define", 'D', 0, G_OPTION_ARG_CALLBACK, (GOptionArgFunc)add_define, "Define variable", "NAME=VALUE"},
	{"seed", 's', 0, G_OPTION_ARG_INT64, &seed, "Random numbers seed (defaults to current time)", "SEED"},
	{NULL}
};

static void
add_defines (CpgParserContext *context)
{
	GSList *defs;
	GSList *item;

	defs = g_slist_reverse (defines);

	for (item = defs; item; item = g_slist_next (item))
	{
		gchar *s = item->data;
		gchar **parts = g_strsplit (s, "=", 2);

		if (parts && parts[0] && parts[1])
		{
			GSList *defines;

			defines = g_slist_prepend (NULL,
			                           cpg_embedded_string_new_from_string (parts[1]));

			cpg_parser_context_define (context,
			                           cpg_embedded_string_new_from_string (parts[0]),
			                           defines,
			                           FALSE,
			                           FALSE);
		}

		g_strfreev (parts);
		g_free (s);
	}

	g_slist_free (defs);
}

static void
remove_double_dash (gchar const **args, gint *argc)
{
	gint i = 0;
	gboolean shiftit = FALSE;

	while (i < *argc)
	{
		if (!shiftit && g_strcmp0 (args[i], "--") == 0)
		{
			shiftit = TRUE;
		}
		else if (shiftit)
		{
			args[i - 1] = args[i];
		}

		++i;
	}

	if (shiftit)
	{
		args[--*argc] = NULL;
	}
}

static int
parse_network (gchar const *args[], gint argc)
{
	CpgParserContext *context;
	GFile *file;
	CpgNetwork *network;
	gboolean ret;
	GError *error = NULL;
	CpgExpansion *expansion;
	CpgEmbeddedContext *embedded;
	gboolean fromstdin;

	remove_double_dash (args, &argc);

	fromstdin = (argc > 0 && g_strcmp0 (args[0], "-") == 0);

	file = NULL;

	if (!fromstdin)
	{
		file = g_file_new_for_commandline_arg (args[0]);

		if (!g_file_query_exists (file, NULL))
		{
			g_printerr ("Could not open file: %s\n", args[0]);
			g_object_unref (file);

			return 1;
		}
	}

	network = cpg_network_new ();
	context = cpg_parser_context_new (network);

	/* We replace the filename here with the collapsed arguments */
	args[0] = cpg_embedded_string_collapse (args + 1);

	expansion = cpg_expansion_new (args);

	embedded = cpg_parser_context_get_embedded (context);
	cpg_embedded_context_add_expansion (embedded, expansion);
	g_object_unref (expansion);

	add_defines (context);

	if (!fromstdin)
	{
		cpg_parser_context_push_input (context, file, NULL);
		g_object_unref (file);
	}
	else
	{
		GInputStream *stream;

		if (isatty (STDIN_FILENO))
		{
			stream = cpg_readline_stream_new ("* ");
		}
		else
		{
			stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);
		}

		cpg_parser_context_push_input (context, NULL, stream);
		g_object_unref (stream);
	}

	if (cpg_parser_context_parse (context, &error))
	{
		CpgNetworkSerializer *serializer;

		serializer = cpg_network_serializer_new (network, NULL);

		if (output_file && strcmp (output_file, "-") != 0)
		{
			GFile *outfile;

			outfile = g_file_new_for_commandline_arg (output_file);

			ret = cpg_network_serializer_serialize_file (serializer,
			                                             outfile,
			                                             &error);
			g_object_unref (outfile);
		}
		else
		{
			GOutputStream *stream;

			stream = g_unix_output_stream_new (STDOUT_FILENO, TRUE);

			ret = cpg_network_serializer_serialize (serializer,
			                                        stream,
			                                        &error);
			g_object_unref (stream);
		}

		if (!ret)
		{
			g_printerr ("Failed to write network: %s\n", error->message);
			g_error_free (error);
		}

		g_object_unref (serializer);
	}
	else
	{
		gchar const *line;
		gint lineno;
		gint cstart;
		gint cend;
		gchar *prefix;
		gchar *lstr;
		gchar *dash;

		g_printerr ("Failed to parse: %s\n\n", error->message);

		line = cpg_parser_context_get_line (context, &lineno);
		cpg_parser_context_get_column (context, &cstart, &cend);

		lstr = g_strdup_printf ("%d.%d", lineno, cstart);

		g_printerr ("(%s%s%s) %s%s%s\n", color_bold, lstr, color_off, color_blue, line, color_off);
		prefix = g_strnfill (strlen (lstr) + 2 + cstart, ' ');
		dash = g_strnfill (MAX (0, cend - cstart - 1), '-');

		g_printerr ("%s%s^%s%s%s%s%s\n", prefix, color_red, color_yellow, dash, color_red, *dash ? "^" : "", color_off);

		ret = FALSE;
	}

	g_object_unref (network);
	g_object_unref (context);

	return ret ? 0 : 1;
}

static void
determine_color_support ()
{
	gchar const *term;
	gchar term_buffer[2048];

	term = g_getenv ("TERM");

	if (!term)
	{
		term = "xterm";
	}

	if (tgetent (term_buffer, term) == 1)
	{
		no_colors = tgetnum ("colors") == 0;
	}
	else
	{
		no_colors = TRUE;
	}
}

static void
disable_colors ()
{
	color_blue = "";
	color_red = "";
	color_green = "";
	color_yellow = "";
	color_off = "";
	color_bold = "";
}

int
main (int argc, char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gboolean ret;
	struct timeval tv;

	g_type_init ();

	gettimeofday (&tv, NULL);
	seed = tv.tv_sec;

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - parse cpg network");

	g_option_context_set_summary (ctx,
	                              "Use a dash '-' for the network name to read from standard input.\n"
	                              "Parameters provided after the network name will will be assigned to @1, etc.\n"
	                              "Use a double dash '--' to prevent option parsing in the parameter list.");

	g_option_context_add_main_entries (ctx, entries, NULL);

	ret = g_option_context_parse (ctx, &argc, &argv, &error);

	if (no_colors)
	{
		disable_colors ();
	}

	if (!ret)
	{
		g_printerr ("%sFailed to parse options:%s %s\n", color_red, color_off, error->message);
		g_error_free (error);

		return 1;
	}

	if (argc < 2)
	{
		g_printerr ("%sPlease provide a network to parse%s\n", color_red, color_off);

		return 1;
	}

	srand (seed);

	return parse_network ((gchar const **)(argv + 1), argc - 1);
}
