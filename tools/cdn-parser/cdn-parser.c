/*
 * cdn-parser.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <codyn/cdn-parser-context.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>
#include <gio/gunixinputstream.h>
#include <codyn/cdn-network-serializer.h>
#include <termcap.h>
#include <sys/time.h>
#include "cdn-readline-stream.h"

static gchar *output_file;
static gboolean no_colors = FALSE;
static gboolean list_files = FALSE;

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
	{"list-files", 'l', 0, G_OPTION_ARG_NONE, &list_files, "Print list of included files instead of XML", NULL},
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors, "Do not use colors in the output", NULL},
	{"define", 'D', 0, G_OPTION_ARG_CALLBACK, (GOptionArgFunc)add_define, "Define variable", "NAME=VALUE"},
	{"seed", 's', 0, G_OPTION_ARG_INT64, &seed, "Random numbers seed (defaults to current time)", "SEED"},
	{NULL}
};

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

static void
on_context_file_used (CdnParserContext *context,
                      GFile            *file,
                      gchar const      *filename,
                      GOutputStream    *stream)
{
	GError *error;
	gchar *path;
	gchar *str;
	gboolean ret;

	path = g_file_get_path (file);
	str = g_strdup_printf ("%s\n", path);
	ret = g_output_stream_write_all (stream, str, strlen (str), NULL, NULL, &error);

	if (!ret)
	{
		g_printerr ("Failed to write to output file: %s\n", error->message);
		g_error_free (error);
	}

	g_free (str);
	g_free (path);
}

static int
parse_network (gchar const *args[], gint argc)
{
	CdnParserContext *context;
	GFile *file;
	CdnNetwork *network;
	gboolean ret = TRUE;
	GError *error = NULL;
	gboolean fromstdin;
	GOutputStream *output_stream;

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

	if (output_file && strcmp (output_file, "-") != 0)
	{
		GFile *outfile;

		outfile = g_file_new_for_commandline_arg (output_file);

		output_stream = G_OUTPUT_STREAM (g_file_replace (outfile,
		                                                 NULL,
		                                                 FALSE,
		                                                 G_FILE_CREATE_NONE,
		                                                 NULL,
		                                                 NULL));

		g_object_unref (outfile);

		if (!output_stream)
		{
			g_printerr ("Could not open file: %s\n", output_file);
			return 1;
		}
	}
	else
	{
		output_stream = g_unix_output_stream_new (STDOUT_FILENO, TRUE);
	}

	network = cdn_network_new ();
	context = cdn_parser_context_new (network);

	if (list_files)
	{
		g_signal_connect (context,
		                  "file-used",
		                  G_CALLBACK (on_context_file_used),
		                  output_stream);
	}

	if (!fromstdin)
	{
		cdn_parser_context_push_input (context, file, NULL, NULL);
		g_object_unref (file);
	}
	else
	{
		GInputStream *stream;

		if (isatty (STDIN_FILENO))
		{
			stream = cdn_readline_stream_new ("* ");
		}
		else
		{
			stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);
		}

		cdn_parser_context_push_input (context, NULL, stream, NULL);
		g_object_unref (stream);
	}

	if (cdn_parser_context_parse (context, TRUE, &error))
	{
		if (!list_files)
		{
			CdnNetworkSerializer *serializer;

			serializer = cdn_network_serializer_new (network, NULL);

			ret = cdn_network_serializer_serialize (serializer,
			                                        output_stream,
			                                        &error);

			if (!ret)
			{
				g_printerr ("Failed to write network: %s\n", error->message);
				g_error_free (error);
			}

			g_object_unref (serializer);
		}
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

		cdn_parser_context_get_error_location (context,
		                                       &lineno,
		                                       NULL,
		                                       &cstart,
		                                       &cend);

		line = cdn_parser_context_get_line_at (context, lineno);

		lstr = g_strdup_printf ("%d.%d", lineno, cstart);

		g_printerr ("(%s%s%s) %s%s%s\n", color_bold, lstr, color_off, color_blue, line, color_off);
		prefix = g_strnfill (strlen (lstr) + 2 + cstart, ' ');
		dash = g_strnfill (MAX (0, cend - cstart - 1), '-');

		g_printerr ("%s%s^%s%s%s%s%s\n", prefix, color_red, color_yellow, dash, color_red, *dash ? "^" : "", color_off);

		ret = FALSE;
	}

	g_object_unref (output_stream);
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
	seed = tv.tv_sec * 1000 + tv.tv_usec / 1000;

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - parse cdn network");

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
