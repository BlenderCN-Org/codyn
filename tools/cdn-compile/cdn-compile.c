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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_GIO_UNIX
#include <gio/gunixinputstream.h>
#endif

#ifdef ENABLE_TERMCAP
#include <termcap.h>
#endif

#include <sys/time.h>
#include <locale.h>

static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GPtrArray *display;
static gboolean simplify = FALSE;

static gboolean
parse_display (gchar const  *option_name,
               gchar const  *value,
               gpointer      data,
               GError      **error)
{
	g_ptr_array_add (display, g_strdup (value));
	return TRUE;
}

static GOptionEntry entries[] = {
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors,
	 "Do not use colors in the output", NULL},
	{"display", 'd', 0, G_OPTION_ARG_CALLBACK, parse_display,
	 "Display variable values (e.g. /state_.*/.\"{x,y}\")", "SEL"},
	{"simplify", 'x', 0, G_OPTION_ARG_NONE, &simplify,
	 "Enable global simplifications", NULL},
	{NULL}
};

static void
display_variable (CdnVariable *v, gchar const *indent)
{
	gchar *name;
	gint r;
	gint c;
	gint i;
	CdnMatrix const *ret;
	gdouble const *values;
	CdnExpression const *expr;
	CdnExpressionTreeIter *iter;
	CdnDimension dim;

	name = cdn_variable_get_full_name_for_display (v);

	expr = cdn_variable_get_expression (v);
	iter = cdn_expression_tree_iter_new (expr);
	cdn_variable_get_dimension (v, &dim);

	g_printf ("%s%s [%d-by-%d]: %s\n",
	          indent,
	          name,
	          dim.rows,
	          dim.columns,
	          cdn_expression_tree_iter_to_string_dbg (iter));

	g_free (name);
	cdn_expression_tree_iter_free (iter);

	ret = cdn_variable_get_values (v);
	values = cdn_matrix_get (ret);

	if (cdn_dimension_is_one (&ret->dimension))
	{
		g_printf ("%s%.5f\n", indent, values[0]);
		return;
	}

	i = 0;

	g_printf ("%s[", indent);

	for (r = 0; r < ret->dimension.rows; ++r)
	{
		if (r != 0)
		{
			g_printf ("%s ", indent);
		}

		for (c = 0; c < ret->dimension.columns; ++c)
		{
			gchar *sv;

			if (c != 0)
			{
				g_printf (", ");
			}

			sv = g_strdup_printf ("%.5f", cdn_matrix_get_at (ret, r, c));

			if (sv[0] != '-')
			{
				g_printf (" ");
			}

			g_printf ("%s", sv);
			++i;
		}

		if (r != ret->dimension.rows - 1)
		{
			g_printf ("\n");
		}
	}

	g_printf ("%s]\n", indent);
}

static void
display_function (CdnFunction *f)
{
	gchar *id;
	GList const *args;
	CdnExpression const *expr;
	CdnExpressionTreeIter *iter;

	expr = cdn_function_get_expression (f);
	iter = cdn_expression_tree_iter_new (expr);

	id = cdn_object_get_full_id_for_display (CDN_OBJECT (f));

	g_printf ("Function %s(", id);

	args = cdn_function_get_arguments (f);

	while (args)
	{
		CdnFunctionArgument *arg = args->data;
		CdnDimension dim;

		cdn_function_argument_get_dimension (arg, &dim);

		g_printf ("%s [%d-by-%d]",
		          cdn_function_argument_get_name (arg),
		          dim.rows,
		          dim.columns);

		args = g_list_next (args);

		if (args)
		{
			g_printf (", ");
		}
	}

	g_printf ("): %s\n",
	          cdn_expression_tree_iter_to_string_dbg (iter));

	g_free (id);
	cdn_expression_tree_iter_free (iter);
}

static void
display_object (CdnObject *o)
{
	gchar *id;
	GSList *vars;

	id = cdn_object_get_full_id_for_display (o);

	g_printf ("Object %s\n", id);

	vars = cdn_object_get_variables (o);

	while (vars)
	{
		CdnVariable *v = vars->data;
		vars = g_slist_delete_link (vars, vars);

		display_variable (v, "  ");
		g_printf ("\n");
	}

	g_free (id);
}

static gint
display_value (CdnNetwork  *network,
               gchar const *expr)
{
	CdnSelector *sel;
	GError *err = NULL;
	GSList *selection;

	sel = cdn_selector_parse (CDN_OBJECT (network), expr, &err);

	if (!sel)
	{
		g_printerr ("Failed to parse selector `%s': %s\n",
		            expr,
		            err->message);

		g_error_free (err);
		return 1;
	}

	selection = cdn_selector_select (sel,
	                                 G_OBJECT (network),
	                                 CDN_SELECTOR_TYPE_ANY,
	                                 NULL);

	while (selection)
	{
		CdnSelection *s = selection->data;
		gpointer v;
		gboolean ignore = FALSE;

		v = cdn_selection_get_object (s);

		if (CDN_IS_VARIABLE (v))
		{
			display_variable (v, "");
		}
		else if (CDN_IS_FUNCTION (v))
		{
			display_function (v);
		}
		else if (CDN_IS_OBJECT (v))
		{
			display_object (v);
		}
		else
		{
			ignore = TRUE;
		}

		if (!ignore && selection->next)
		{
			g_printf ("\n");
		}

		cdn_selection_unref (s);
		selection = g_slist_delete_link (selection, selection);
	}

	g_object_unref (sel);
	return 0;
}

static gint
display_values (CdnNetwork *network)
{
	gint i;

	for (i = 0; i < display->len; ++i)
	{
		gint ret;

		ret = display_value (network, display->pdata[i]);

		if (ret != 0)
		{
			return ret;
		}

		if (i != display->len - 1)
		{
			g_printf ("\n");
		}
	}

	return 0;
}

static void
print_parse_error (CdnNetwork *network)
{
	gchar const *line;
	gint lineno;
	gint cstart;
	gint cend;
	gchar *prefix;
	gchar *lstr;
	gchar *dash;
	GFile *file;
	CdnParserContext *context;

	context = cdn_network_get_parser_context (network);

	if (!context)
	{
		return;
	}

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

		lstr = g_strdup_printf ("%s:%d.%d", fbase, lineno, cstart);

		g_free (fbase);
		g_object_unref (file);
	}
	else
	{
		lstr = g_strdup_printf ("%d.%d", lineno, cstart);
	}

	line = cdn_parser_context_get_line_at (context, lineno);

	g_printerr ("(%s%s%s) %s%s%s\n", color_bold, lstr, color_off, color_blue, line, color_off);
	prefix = g_strnfill (strlen (lstr) + 2 + cstart, ' ');
	dash = g_strnfill (MAX (0, cend - cstart - 1), '-');

	g_printerr ("%s%s^%s%s%s%s%s\n", prefix, color_red, color_yellow, dash, color_red, *dash ? "^" : "", color_off);

	g_free (lstr);
	g_free (prefix);
	g_free (dash);
}

static int
compile_network (gchar const *filename)
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnCompileError *err;
	gboolean loaded;

	network = cdn_network_new ();

#ifdef ENABLE_GIO_UNIX
	if (g_strcmp0 (filename, "-") == 0)
	{
		GInputStream *stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);

		loaded = cdn_network_load_from_stream (network, stream, &error);

		g_object_unref (stream);
	}
	else
#endif
	{
		GFile *file = g_file_new_for_commandline_arg (filename);
		loaded = cdn_network_load_from_file (network, file, &error);
		g_object_unref (file);
	}

	if (!loaded)
	{
		g_printerr ("%sFailed to parse network `%s%s%s%s': %s%s%s\n\n",
		            color_red,
		            color_off,
		            color_bold,
		            filename,
		            color_off,
		            color_red,
		            error->message,
		            color_off);

		print_parse_error (network);

		g_error_free (error);
		g_object_unref (network);

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

	if (simplify)
	{
		cdn_network_simplify (network);
	}

	display_values (network);

	g_object_unref (err);
	g_object_unref (network);

	return 0;
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
	gchar const *file = NULL;

#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

	setlocale (LC_ALL, "");

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - compile cdn network");

	display = g_ptr_array_new_with_free_func ((GDestroyNotify)g_free);

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
