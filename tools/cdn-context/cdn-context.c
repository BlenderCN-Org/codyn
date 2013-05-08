/*
 * cdn-context.c
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

#include <codyn/cdn-parser-context.h>
#include <codyn/cdn-statement.h>
#include <codyn/cdn-io.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <codyn/cdn-cfile-stream.h>
#include <json-glib/json-glib.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_GIO_UNIX
#include <gio/gunixinputstream.h>
#endif

#ifdef ENABLE_TERMCAP
#include <termcap.h>
#endif

static gchar *output_file;
static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static gint context_line = -1;
static gint context_column = -1;

static GOptionEntry entries[] = {
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file,
	 "Output file (defaults to standard output)", "FILE"},
	{"no-color", 'n', 0, G_OPTION_ARG_NONE, &no_colors,
	 "Do not use colors in the output", NULL},
	{"line", 'l', 0, G_OPTION_ARG_INT, &context_line,
	 "Report contexts on a particular line (starting at 1)", "LINE"},
	{"column", 'c', 0, G_OPTION_ARG_INT, &context_column,
	 "Only report contexts on a particular column (starting at 1, requires --line/-l)", "COL"},
	{NULL}
};

typedef struct
{
	GSList *in;
	GSList *out;
} Selection;

typedef struct
{
	gint line_start;
	gint column_start;

	gint line_end;
	gint column_end;

	GFile *file;

	GSList *selections;
} Context;

typedef struct
{
	CdnParserContext *parser;
	GFile *file;

	GSList *context_stack;
	GSList *contexts;

	GHashTable *selectors;
} Info;


typedef struct
{
	guint id;
	gint line_start;
	gint line_end;
	gint cstart;
	gint cend;

	Context *context;
} SelectorItemAnnotation;


static Context *
context_new (CdnParserContext *context)
{
	Context *ret;
	GSList const *selections;
	Selection *s;

	ret = g_slice_new0 (Context);

	cdn_parser_context_get_line (context, &ret->line_start);
	cdn_parser_context_get_column (context, &ret->column_start, NULL);

	s = g_slice_new0 (Selection);

	selections = cdn_parser_context_previous_selections (context);

	while (selections)
	{
		s->in = g_slist_prepend (s->in,
		                         cdn_selection_copy (selections->data));

		selections = g_slist_next (selections);
	}

	s->in = g_slist_reverse (s->in);

	selections = cdn_parser_context_current_selections (context);

	while (selections)
	{
		s->out = g_slist_prepend (s->out,
		                          cdn_selection_copy (selections->data));

		selections = g_slist_next (selections);
	}

	s->out = g_slist_reverse (s->out);
	ret->selections = g_slist_prepend (NULL, s);

	ret->file = cdn_parser_context_get_file (context);

	return ret;
}

static void
selection_free (Selection *s)
{
	g_slist_foreach (s->in, (GFunc)g_object_unref, NULL);
	g_slist_free (s->in);

	g_slist_foreach (s->out, (GFunc)g_object_unref, NULL);
	g_slist_free (s->out);

	g_slice_free (Selection, s);
}

static void
context_free (Context *context)
{
	if (context->file)
	{
		g_object_unref (context->file);
	}

	g_slist_foreach (context->selections, (GFunc)selection_free, NULL);
	g_slist_free (context->selections);

	g_slice_free (Context, context);
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

static gboolean
same_file (Info *info)
{
	GFile *ctxfile;

	ctxfile = cdn_parser_context_get_file (info->parser);

	if (!ctxfile)
	{
		return TRUE;
	}

	if (!info->file)
	{
		return FALSE;
	}

	return g_file_equal (ctxfile, info->file);
}

static void
on_context_pushed (CdnParserContext *context,
                   Info             *info)
{
	Context *ctx;

	if (!same_file (info))
	{
		return;
	}

	ctx = context_new (context);

	info->contexts = g_slist_prepend (info->contexts, ctx);
	info->context_stack = g_slist_prepend (info->context_stack, ctx);
}

static gboolean
check_region (gint line_start, gint line_end, gint cstart, gint cend)
{
	if (context_line == -1)
	{
		return TRUE;
	}

	if (line_start > context_line ||
	    line_end < context_line)
	{
		return FALSE;
	}

	if (context_column == -1)
	{
		return TRUE;
	}

	if (context_line == line_start && context_column <= cstart)
	{
		return FALSE;
	}

	if (context_line == line_end && context_column > cend)
	{
		return FALSE;
	}

	return TRUE;
}

static void
on_context_popped (CdnParserContext *context,
                   Info             *info)
{
	Context *ctx;

	if (!same_file (info) || !info->context_stack)
	{
		return;
	}

	ctx = info->context_stack->data;

	cdn_parser_context_get_line (context, &ctx->line_end);
	cdn_parser_context_get_column (context, NULL, &ctx->column_end);

	info->context_stack =
		g_slist_delete_link (info->context_stack,
		                     info->context_stack);

	if (!check_region (ctx->line_start,
	                   ctx->line_end,
	                   ctx->column_start,
	                   ctx->column_end))
	{
		info->contexts = g_slist_remove (info->contexts, ctx);
		context_free (ctx);
	}
}

static void
copy_selector_selections (GSList const  *selections,
                          GSList       **ret)
{
	*ret = NULL;

	while (selections)
	{
		*ret = g_slist_prepend (*ret,
		                        cdn_selection_copy (selections->data));

		selections = g_slist_next (selections);
	}

	*ret = g_slist_reverse (*ret);
}

static void
on_selector_select (CdnSelector *selector,
                    guint        id,
                    Info        *info)
{
	GSList *items;

	items = g_hash_table_lookup (info->selectors, selector);

	while (items)
	{
		SelectorItemAnnotation *annot;

		annot = items->data;

		if (annot->id == id)
		{
			Selection *s;

			if (!annot->context)
			{
				GFile *f;

				f = cdn_parser_context_get_file (info->parser);

				annot->context = g_slice_new0 (Context);
				annot->context->line_start = annot->line_start;
				annot->context->column_start = annot->cstart;
				annot->context->line_end = annot->line_end;
				annot->context->column_end = annot->cend;
				annot->context->file = f;

				info->contexts =
					g_slist_prepend (info->contexts,
					                 annot->context);
			}

			s = g_slice_new0 (Selection);

			copy_selector_selections (cdn_selector_get_in_context (selector, id),
			                          &s->in);

			copy_selector_selections (cdn_selector_get_out_context (selector, id),
			                          &s->out);

			annot->context->selections =
				g_slist_append (annot->context->selections,
				                s);

			break;
		}

		items = g_slist_next (items);
	}
}

static void
on_selector_item_pushed (CdnParserContext *context,
                         CdnSelector      *selector,
                         Info             *info)
{
	GSList *annot;
	SelectorItemAnnotation *an;
	gint line_start;
	gint line_end;
	gint cstart;
	gint cend;

	cdn_statement_get_line (CDN_STATEMENT (selector), &line_start, &line_end);
	cdn_statement_get_column (CDN_STATEMENT (selector), &cstart, &cend);

	if (!check_region (line_start, line_end, cstart, cend))
	{
		return;
	}

	if (!info->selectors)
	{
		info->selectors = g_hash_table_new (g_direct_hash,
		                                    g_direct_equal);
	}

	annot = g_hash_table_lookup (info->selectors, selector);

	an = g_slice_new0 (SelectorItemAnnotation);
	an->id = cdn_selector_get_last_id (selector);

	an->line_start = line_start;
	an->line_end = line_end;
	an->cstart = cstart;
	an->cend = cend;

	if (annot == NULL)
	{
		g_signal_connect (selector,
		                  "select",
		                  G_CALLBACK (on_selector_select),
		                  info);
	}

	annot = g_slist_prepend (annot, an);

	g_hash_table_insert (info->selectors, selector, annot);
}

static void
info_free (Info *info)
{
	g_slist_foreach (info->contexts, (GFunc)context_free, NULL);
	g_slist_free (info->contexts);

	g_slist_free (info->context_stack);

	if (info->file)
	{
		g_object_unref (info->file);
	}
}

static void
write_string_field (JsonBuilder *builder,
                    gchar const *name,
                    gchar const *value)
{
	json_builder_set_member_name (builder, name);
	json_builder_add_string_value (builder, value);
}

static void
write_int_field (JsonBuilder *builder,
                 gchar const *name,
                 gint64       value)
{
	json_builder_set_member_name (builder, name);
	json_builder_add_int_value (builder, value);
}

static void
write_cdn_expansion (CdnExpansion *expansion,
                     JsonBuilder  *builder)
{
	gint i;

	json_builder_begin_array (builder);

	for (i = 0; i < cdn_expansion_num (expansion); ++i)
	{
		json_builder_begin_object (builder);

		write_string_field (builder,
		                    "value",
		                    cdn_expansion_get (expansion, i));

		write_int_field (builder,
		                 "index",
		                 cdn_expansion_get_index (expansion, i));

		json_builder_end_object (builder);
	}

	json_builder_end_array (builder);
}

static void
foreach_define (gchar const  *name,
                CdnExpansion *value,
                JsonBuilder  *builder)
{
	json_builder_begin_object (builder);

	write_string_field (builder, "key", name);

	json_builder_set_member_name (builder, "value");
	write_cdn_expansion (value, builder);

	json_builder_end_object (builder);
}

static void
write_cdn_selection (CdnSelection *selection,
                     JsonBuilder  *builder)
{
	gpointer obj;
	gchar const *name = "";
	gchar const *typename = "";
	GSList *expansions;
	CdnExpansionContext *ctx;

	obj = cdn_selection_get_object (selection);

	json_builder_begin_object (builder);

	if (CDN_IS_OBJECT (obj))
	{
		name = cdn_object_get_id (obj);

		if (CDN_IS_NETWORK (obj))
		{
			typename = "network";
		}
		else if (CDN_IS_IO (obj))
		{
			typename = "io";
		}
		else if (CDN_IS_IMPORT (obj))
		{
			typename = "import";
		}
		else if (CDN_IS_FUNCTION (obj))
		{
			typename = "function";
		}
		else if (CDN_IS_NODE (obj))
		{
			typename = "node";
		}
		else if (CDN_IS_EDGE (obj))
		{
			typename = "edge";
		}
	}
	else if (CDN_IS_VARIABLE (obj))
	{
		name = cdn_variable_get_name (obj);
		typename = "variable";
	}
	else if (CDN_IS_EDGE_ACTION (obj))
	{
		name = cdn_edge_action_get_target (obj);
		typename = "action";
	}

	write_string_field (builder, "name", name);
	write_string_field (builder, "typename", typename);

	json_builder_set_member_name (builder, "expansions");
	json_builder_begin_array (builder);

	ctx = cdn_selection_get_context (selection);
	expansions = cdn_expansion_context_get_expansions (ctx);

	while (expansions)
	{
		write_cdn_expansion (expansions->data, builder);
		expansions = g_slist_delete_link (expansions,
		                                  expansions);
	}

	json_builder_end_array (builder);

	json_builder_set_member_name (builder, "defines");
	json_builder_begin_array (builder);

	cdn_expansion_context_foreach_define (ctx,
	                                      (GHFunc)foreach_define,
	                                      builder);

	json_builder_end_array (builder);

	json_builder_end_object (builder);
}

static void
write_selection (Selection   *selection,
                 JsonBuilder *builder)
{
	GSList *item;

	json_builder_begin_object (builder);

	json_builder_set_member_name (builder, "in");
	json_builder_begin_array (builder);

	for (item = selection->in; item; item = g_slist_next (item))
	{
		write_cdn_selection (item->data, builder);
	}

	json_builder_end_array (builder);

	json_builder_set_member_name (builder, "out");
	json_builder_begin_array (builder);

	for (item = selection->out; item; item = g_slist_next (item))
	{
		write_cdn_selection (item->data, builder);
	}

	json_builder_end_array (builder);
	json_builder_end_object (builder);
}

static void
write_context (Context     *context,
               JsonBuilder *builder)
{
	gchar *filename;
	GSList *item;

	json_builder_begin_object (builder);

	filename = context->file ? g_file_get_path (context->file) : g_strdup ("");

	write_string_field (builder, "filename", filename);
	write_int_field (builder, "line_start", context->line_start);
	write_int_field (builder, "line_end", context->line_end);
	write_int_field (builder, "column_start", context->column_start);
	write_int_field (builder, "column_end", context->column_end);

	json_builder_set_member_name (builder, "selections");
	json_builder_begin_array (builder);

	for (item = context->selections; item; item = g_slist_next (item))
	{
		write_selection (item->data, builder);
	}

	json_builder_end_array (builder);
	json_builder_end_object (builder);

	g_free (filename);
}

static void
write_contexts (Info          *info,
                GOutputStream *stream)
{
	/* Write out json formatted list of contexts */
	GSList *item;
	JsonBuilder *builder;
	JsonGenerator *generator;

	builder = json_builder_new ();

	json_builder_begin_object (builder);
	json_builder_set_member_name (builder, "status");
	json_builder_add_string_value (builder, "ok");
	json_builder_set_member_name (builder, "data");
	json_builder_begin_array (builder);

	for (item = info->contexts; item; item = g_slist_next (item))
	{
		write_context (item->data, builder);
	}

	json_builder_end_array (builder);
	json_builder_end_object (builder);

	generator = json_generator_new ();
	json_generator_set_root (generator,
	                         json_builder_get_root (builder));

	json_generator_to_stream (generator, stream, NULL, NULL);

	g_object_unref (generator);
	g_object_unref (builder);
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
	Info info = {0,};
	GOutputStream *stream;

	remove_double_dash (args, &argc);

#ifdef ENABLE_GIO_UNIX
	fromstdin = (argc > 0 && g_strcmp0 (args[0], "-") == 0);
#else
	fromstdin = FALSE;
#endif

	file = NULL;

	if (!fromstdin)
	{
		if (argc == 0)
		{
			g_printerr ("Please provide an input file\n");
			return 1;
		}

		file = g_file_new_for_commandline_arg (args[0]);

		if (!g_file_query_exists (file, NULL))
		{
			g_printerr ("Could not open file: %s\n", args[0]);
			g_object_unref (file);

			return 1;
		}
	}

	network = cdn_network_new ();
	context = cdn_parser_context_new (network);

#ifdef ENABLE_GIO_UNIX
	if (!fromstdin)
#endif
	{
		cdn_parser_context_push_input (context, file, NULL, FALSE);
		info.file = file;
	}
#ifdef ENABLE_GIO_UNIX
	else
	{
		GInputStream *stream;

		stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);

		cdn_parser_context_push_input (context, NULL, stream, FALSE);
		g_object_unref (stream);
	}
#endif

	if (output_file && strcmp (output_file, "-") != 0)
	{
		GFile *outfile;

		outfile = g_file_new_for_commandline_arg (output_file);

		stream = G_OUTPUT_STREAM (g_file_replace (outfile,
		                                          NULL,
		                                          FALSE,
		                                          G_FILE_CREATE_NONE,
		                                          NULL,
		                                          NULL));

		g_object_unref (outfile);
	}
	else
	{
		stream = cdn_cfile_stream_new (stdout);
	}

	if (!stream)
	{
		ret = FALSE;
	}
	else
	{
		if (context_line != -1)
		{
			g_signal_connect (context,
			                  "context-pushed",
			                  G_CALLBACK (on_context_pushed),
			                  &info);

			g_signal_connect (context,
			                  "context-popped",
			                  G_CALLBACK (on_context_popped),
			                  &info);

			g_signal_connect (context,
			                  "selector-item-pushed",
			                  G_CALLBACK (on_selector_item_pushed),
			                  &info);
		}

		info.parser = context;

		if (cdn_parser_context_parse (context, TRUE, &error))
		{
			if (context_line != -1 && context_column != -1)
			{
				/* Only keep the last */
				g_slist_foreach (info.contexts->next, (GFunc)context_free, NULL);

				g_slist_free (info.contexts->next);
				info.contexts->next = NULL;
			}
			else
			{
				info.contexts = g_slist_reverse (info.contexts);
			}

			write_contexts (&info, stream);
		}
		else
		{
			gchar *line;
			GFile *file;
			gchar *filename;
			gint lstart;
			gint lend;
			gint cstart;
			gint cend;
			JsonBuilder *builder;
			JsonGenerator *generator;

			builder = json_builder_new ();
			json_builder_begin_object (builder);
			json_builder_set_member_name (builder, "status");
			json_builder_add_string_value (builder, "error");
			json_builder_set_member_name (builder, "data");

			json_builder_begin_object (builder);
			json_builder_set_member_name (builder, "message");
			json_builder_add_string_value (builder, error->message);

			cdn_parser_context_get_error_location (context,
			                                       &lstart,
			                                       &lend,
			                                       &cstart,
			                                       &cend,
			                                       NULL);

			file = cdn_parser_context_get_file (context);
			filename = file ? g_file_get_path (file) : g_strdup ("");

			if (file)
			{
				g_object_unref (file);
			}

			json_builder_set_member_name (builder, "filename");
			json_builder_add_string_value (builder, filename);

			g_free (filename);

			line = cdn_parser_context_get_error_lines (context);

			json_builder_set_member_name (builder, "line");
			json_builder_add_string_value (builder, line);

			g_free (line);

			json_builder_set_member_name (builder, "line_start");
			json_builder_add_int_value (builder, lstart);

			json_builder_set_member_name (builder, "line_end");
			json_builder_add_int_value (builder, lend);

			json_builder_set_member_name (builder, "column_start");
			json_builder_add_int_value (builder, cstart);

			json_builder_set_member_name (builder, "column_end");
			json_builder_add_int_value (builder, cend);

			json_builder_end_object (builder);
			json_builder_end_object (builder);

			generator = json_generator_new ();
			json_generator_set_root (generator,
			                         json_builder_get_root (builder));

			json_generator_to_stream (generator, stream, NULL, NULL);

			g_object_unref (generator);
			g_object_unref (builder);

			ret = FALSE;
		}
	}

	info_free (&info);

	g_object_unref (network);
	g_object_unref (context);

	if (stream)
	{
		g_output_stream_write_all (stream, "\n", 1, NULL, NULL, NULL);
		g_object_unref (stream);
	}

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

	close (STDERR_FILENO);

	determine_color_support ();

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - dump contexts in cdn format");

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

	if (context_line != -1 && context_column == -1)
	{
		context_column = 0;
	}

	if (argc < 2)
	{
		g_printerr ("%sPlease provide a network to parse%s\n", color_red, color_off);

		return 1;
	}

	return parse_network ((gchar const **)(argv + 1), argc - 1);
}
