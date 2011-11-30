/*
 * cdn-compile.c
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
#include <termcap.h>
#include <sys/time.h>

static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GOptionEntry entries[] = {
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors, "Do not use colors in the output", NULL},
	{NULL}
};

static int
compile_network (gchar const *filename)
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnCompileError *err;

	if (g_strcmp0 (filename, "-") == 0)
	{
		GInputStream *stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);
		network = cdn_network_new_from_stream (stream, &error);
		g_object_unref (stream);
	}
	else
	{
		GFile *file = g_file_new_for_commandline_arg (filename);
		network = cdn_network_new_from_file (file, &error);
		g_object_unref (file);
	}

	if (!network)
	{
		g_printerr ("%sFailed to load network `%s%s%s%s': %s%s%s\n",
		            color_red,
		            color_off,
		            color_bold,
		            filename,
		            color_off,
		            color_red,
		            error->message,
		            color_off);

		g_error_free (error);

		return 1;
	}

	err = cdn_compile_error_new ();

	if (!cdn_object_compile (CDN_OBJECT (network), NULL, err))
	{
		gchar *msg;

		msg = cdn_compile_error_get_formatted_string (err);

		g_printerr ("%sFailed to compile network:%s%s %s%s\n\n%s%s%s\n",
		            color_red,
		            color_off,
		            color_bold,
		            filename,
		            color_off,
		            color_red,
		            msg,
		            color_off);

		g_free (msg);

		g_object_unref (network);
		g_object_unref (err);

		return 1;
	}

	g_object_unref (err);
	g_object_unref (network);

	return 0;
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
	gchar const *file = NULL;

	g_type_init ();

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - compile cdn network");

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

	if (argc == 1)
	{
		file = "-";
	}
	else if (argc == 2)
	{
		file = argv[1];
	}
	else
	{
		g_printerr ("%sPlease provide a network to parse%s\n", color_red, color_off);
		return 1;
	}

	return compile_network (file);
}
