#include <cpg-network/cpg-parser-context.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>
#include <cpg-network/cpg-network-serializer.h>
#include <termcap.h>

static gchar *output_file;
static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors, "Do not use colors in the output", NULL},
	{NULL}
};

static int
parse_network (gchar const *filename)
{
	CpgParserContext *context;
	GFile *file;
	CpgNetwork *network;
	gboolean ret;
	GError *error = NULL;

	file = g_file_new_for_commandline_arg (filename);
	network = cpg_network_new ();

	context = cpg_parser_context_new (network);
	cpg_parser_context_push_input (context, file, NULL);

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

	g_type_init ();

	determine_color_support ();

	ctx = g_option_context_new ("<NETWORK> - parse cpg network");
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

	if (argc != 2)
	{
		g_printerr ("%sPlease provide exactly one network to parse%s\n", color_red, color_off);

		return 1;
	}

	return parse_network (argv[1]);
}
