/*
 * cdn-archive.c
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
#include <sys/time.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_TERMCAP
#include <termcap.h>
#endif

#include <libtar.h>
#include <bzlib.h>
#include <fcntl.h>
#include <locale.h>

static gchar *output_file;
static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to input.tar.bz2)", "FILE"},
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors, "Do not use colors in the output", NULL},
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

typedef struct
{
	gchar *filename;
	GFile *file;
} InputFile;

static InputFile *
input_file_new (GFile *file, gchar const *filename)
{
	InputFile *ret;

	ret = g_slice_new0 (InputFile);

	ret->file = g_file_dup (file);
	ret->filename = g_strdup (filename);

	return ret;
}

static void
input_file_free (InputFile *self)
{
	g_object_unref (self->file);
	g_free (self->filename);

	g_slice_free (InputFile, self);
}

typedef struct
{
	GSList *files;
} InputInfo;

static gint
find_file (InputFile *f1, GFile *file)
{
	if (g_file_equal (f1->file, file))
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

static void
on_context_file_used (CdnParserContext *context,
                      GFile            *file,
                      gchar const      *filename,
                      InputInfo        *info)
{
	if (g_slist_find_custom (info->files, file, (GCompareFunc)find_file))
	{
		return;
	}

	info->files = g_slist_prepend (info->files,
	                               input_file_new (file, filename));
}

static void
clear_files (InputInfo *info)
{
	g_slist_foreach (info->files, (GFunc)input_file_free, NULL);
	g_slist_free (info->files);
}

static FILE *archive_file;
static BZFILE *archive_bzfile;

static ssize_t
do_bzwrite (int fd, void const *data, size_t len)
{
	gint err;
	BZ2_bzWrite (&err, archive_bzfile, (void *)data, len);

	if (err != BZ_OK)
	{
		return -1;
	}

	return (ssize_t)len;
}

static int
do_bzclose (int fd)
{
	gint err;

	BZ2_bzWriteClose (&err, archive_bzfile, 0, NULL, NULL);

	if (err == BZ_OK)
	{
		return 0;
	}

	return -1;
}

static int
parse_network (gchar const *args[], gint argc)
{
	CdnParserContext *context;
	GFile *file;
	CdnNetwork *network;
	gboolean ret;
	GError *error = NULL;

	remove_double_dash (args, &argc);

	file = g_file_new_for_commandline_arg (args[0]);

	if (!g_file_query_exists (file, NULL))
	{
		g_printerr ("Could not open file: %s\n", args[0]);
		g_object_unref (file);

		return 1;
	}

	network = cdn_network_new ();
	context = cdn_parser_context_new (network);

	InputInfo info = {NULL};

	info.files = g_slist_prepend (info.files, input_file_new (file, args[0]));

	g_signal_connect (context,
	                  "file-used",
	                  G_CALLBACK (on_context_file_used),
	                  &info);

	cdn_parser_context_push_input (context, file, NULL, FALSE);

	if (cdn_parser_context_parse (context, TRUE, &error))
	{
		info.files = g_slist_reverse (info.files);

		tartype_t tartype = {
			NULL,
			(closefunc_t)do_bzclose,
			NULL,
			(writefunc_t)do_bzwrite
		};

		TAR *tar = NULL;

		if (output_file)
		{
			if (strcmp (output_file, "-") != 0)
			{
				archive_file = fopen (output_file, "w");
			}
			else
			{
				archive_file = stdout;
			}
		}
		else
		{
			gchar *path;
			gchar *cmb;

			path = g_file_get_path (file);
			cmb = g_strconcat (path, ".tar.bz2", NULL);
			g_free (path);

			archive_file = fopen (cmb, "w");
			g_free (cmb);
		}

		tar_fdopen (&tar,
		            fileno (archive_file),
		            NULL,
		            &tartype,
		            O_WRONLY,
		            0644,
		            0);

		if (!tar)
		{
			ret = FALSE;
			goto cleanup;
		}

		gint bzerr = 0;

		archive_bzfile = BZ2_bzWriteOpen (&bzerr,
		                                  archive_file,
		                                  9,
		                                  0,
		                                  0);

		if (!archive_bzfile)
		{
			ret = FALSE;
			goto cleanup;
		}

		GSList *item;

		for (item = info.files; item; item = g_slist_next (item))
		{
			InputFile *f = item->data;
			gchar *path;

			path = g_file_get_path (f->file);

			tar_append_file (tar, path, f->filename);
			g_free (path);
		}

		tar_close (tar);
		ret = TRUE;
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
		GFile *file;

		g_printerr ("Failed to parse: %s\n\n", error->message);

		cdn_parser_context_get_error_location (context,
		                                       &lineno,
		                                       NULL,
		                                       &cstart,
		                                       &cend,
		                                       &file);

		if (file)
		{
			gchar *fbase;

			fbase = g_file_get_basename (file);
			g_printerr ("%s:\n", fbase);

			g_free (fbase);
			g_object_unref (file);
		}

		line = cdn_parser_context_get_line_at (context, lineno);

		lstr = g_strdup_printf ("%d.%d", lineno, cstart);

		g_printerr ("(%s%s%s) %s%s%s\n", color_bold, lstr, color_off, color_blue, line, color_off);
		prefix = g_strnfill (strlen (lstr) + 2 + cstart, ' ');
		dash = g_strnfill (MAX (0, cend - cstart - 1), '-');

		g_printerr ("%s%s^%s%s%s%s%s\n", prefix, color_red, color_yellow, dash, color_red, *dash ? "^" : "", color_off);

		ret = FALSE;
	}

cleanup:
	g_object_unref (file);

	clear_files (&info);

	g_object_unref (network);
	g_object_unref (context);

	return ret ? 0 : 1;
}

static void
determine_color_support ()
{
	gchar const *term;

	term = g_getenv ("TERM");

	if (!term)
	{
		term = "xterm";
	}

#ifdef ENABLE_TERMCAP
{
	gchar term_buffer[2048];

	if (tgetent (term_buffer, term) == 1)
	{
		no_colors = tgetnum ("colors") == 0;
	}
	else
	{
		no_colors = TRUE;
	}
}
#else
	no_colors = TRUE;
#endif

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

#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

	setlocale (LC_ALL, "");

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - create network archive");

	g_option_context_set_summary (ctx,
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

	return parse_network ((gchar const **)(argv + 1), argc - 1);
}
