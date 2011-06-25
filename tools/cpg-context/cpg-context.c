#include <cpg-network/cpg-parser-context.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>
#include <gio/gunixinputstream.h>
#include <cpg-network/cpg-input-file.h>
#include <termcap.h>

static gchar *output_file;
static gboolean no_colors = FALSE;

static gchar const *color_red = "\e[31m";
static gchar const *color_green = "\e[32m";
static gchar const *color_yellow = "\e[33m";
static gchar const *color_blue = "\e[34m";
static gchar const *color_bold = "\e[1m";
static gchar const *color_off = "\e[0m";

static GSList *defines = NULL;

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
	{NULL}
};

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
	GSList *context_stack;
	GSList *contexts;
} Info;

static Context *
context_new (CpgParserContext *context)
{
	Context *ret;
	GSList const *selections;

	ret = g_slice_new0(Context);

	cpg_parser_context_get_line (context, &ret->line_start);
	cpg_parser_context_get_column (context, &ret->column_start, NULL);

	selections = cpg_parser_context_current_selections (context);

	while (selections)
	{
		ret->selections = g_slist_prepend (ret->selections,
		                                   cpg_selection_copy_defines (selections->data, FALSE));

		selections = g_slist_next (selections);
	}

	ret->selections = g_slist_reverse (ret->selections);

	ret->file = cpg_parser_context_get_file (context);

	return ret;
}

static void
context_free (Context *context)
{
	if (context->file)
	{
		g_object_unref (context->file);
	}

	g_slist_foreach (context->selections, (GFunc)g_object_unref, NULL);
	g_slist_free (context->selections);

	g_slice_free (Context, context);
}

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

static void
on_context_pushed (CpgParserContext *context,
                   Info             *info)
{
	Context *ctx;

	ctx = context_new (context);
	info->contexts = g_slist_prepend (info->contexts, ctx);
	info->context_stack = g_slist_prepend (info->context_stack, ctx);
}

static void
on_context_popped (CpgParserContext *context,
                   Info             *info)
{
	Context *ctx;

	ctx = info->context_stack->data;

	cpg_parser_context_get_line (context, &ctx->line_end);
	cpg_parser_context_get_column (context, NULL, &ctx->column_end);

	info->context_stack =
		g_slist_delete_link (info->context_stack,
		                     info->context_stack);
}

static void
info_free (Info *info)
{
	g_slist_foreach (info->contexts, (GFunc)context_free, NULL);
	g_slist_free (info->contexts);

	g_slist_free (info->context_stack);
}

#define write_stream_format(stream, format, ...) do {				\
	gchar *s;								\
										\
	s = g_strdup_printf (format, __VA_ARGS__);				\
	g_data_output_stream_put_string (stream, s, NULL, NULL);		\
	g_free (s);								\
} while (0)

#define write_stream(stream, s) write_stream_format (stream, "%s", s)
#define write_stream_nl(stream, s) write_stream_format (stream, "%s\n", s)

typedef struct
{
	gboolean first;
	GDataOutputStream *out;
} ForeachInfo;

static void
foreach_define (gchar const *name,
                gchar const *value,
                ForeachInfo *info)
{
	gchar *name_esc;
	gchar *value_esc;

	name_esc = g_strescape (name, NULL);
	value_esc = g_strescape (value, NULL);

	if (!info->first)
	{
		write_stream_nl (info->out, ",");
	}
	else
	{
		info->first = FALSE;
	}

	write_stream_format (info->out,
	                     "            {\"key\": \"%s\", \"value\": \"%s\"}",
	                     name_esc,
	                     value_esc);

	g_free (name_esc);
	g_free (value_esc);
}

static void
write_selection (CpgSelection      *selection,
                 GDataOutputStream *out)
{
	gpointer obj;
	gchar const *name = "";
	gchar const *typename = "";
	GSList const *expansions;
	gchar *name_esc;
	ForeachInfo info = {0,};

	obj = cpg_selection_get_object (selection);

	write_stream_nl (out, "\n        {");

	if (CPG_IS_OBJECT (obj))
	{
		name = cpg_object_get_id (obj);

		if (CPG_IS_NETWORK (obj))
		{
			typename = "network";
		}
		else if (CPG_IS_IMPORT (obj))
		{
			typename = "import";
		}
		else if (CPG_IS_FUNCTION (obj))
		{
			typename = "function";
		}
		else if (CPG_IS_GROUP (obj))
		{
			typename = "group";
		}
		else if (CPG_IS_LINK (obj))
		{
			typename = "link";
		}
		else if (CPG_IS_INPUT_FILE (obj))
		{
			typename = "input-file";
		}
		else
		{
			typename = "state";
		}
	}
	else if (CPG_IS_PROPERTY (obj))
	{
		name = cpg_property_get_name (obj);
		typename = "property";
	}
	else if (CPG_IS_LINK_ACTION (obj))
	{
		name = cpg_link_action_get_target (obj);
		typename = "action";
	}

	name_esc = g_strescape (name, NULL);

	write_stream_format (out, "          \"name\": \"%s\",\n", name_esc);
	g_free (name_esc);

	name_esc = g_strescape (typename, NULL);
	write_stream_format (out, "          \"typename\": \"%s\",\n", name_esc);

	write_stream_nl (out, "          \"expansions\": [");

	g_free (name_esc);

	expansions = cpg_selection_get_expansions (selection);

	while (expansions)
	{
		CpgExpansion *exp;
		gint i;

		exp = expansions->data;

		write_stream (out, "            [");

		for (i = 0; i < cpg_expansion_num (exp); ++i)
		{
			gchar *value_esc;

			if (i != 0)
			{
				write_stream (out, ", ");
			}

			value_esc = g_strescape (cpg_expansion_get (exp, i), NULL);

			write_stream_format (out,
			                     "{\"value\": \"%s\", \"index\": %d}",
			                     value_esc,
			                     cpg_expansion_get_index (exp, i));

			g_free (value_esc);
		}

		write_stream (out, "]");

		if (expansions->next)
		{
			write_stream_nl (out, ",");
		}
		else
		{
			write_stream_nl (out, "");
		}

		expansions = g_slist_next (expansions);
	}

	write_stream_nl (out, "          ],");

	write_stream_nl (out, "          \"defines\": [");

	info.out = out;
	info.first = TRUE;

	g_hash_table_foreach (cpg_selection_get_defines (selection),
	                      (GHFunc)foreach_define,
	                      &info);

	write_stream_nl (out, "\n          ]");

	write_stream (out, "        }");
}

static void
write_context (Context           *context,
               GDataOutputStream *out)
{
	gchar *filename;
	GSList *item;
	gchar *filename_esc;

	write_stream_nl (out, "\n    {");

	filename = context->file ? g_file_get_path (context->file) : g_strdup ("");
	filename_esc = g_strescape (filename, NULL);
	g_free (filename);

	write_stream_format (out, "      \"filename\": \"%s\",\n", filename_esc);
	write_stream_format (out, "      \"line_start\": %d,\n", context->line_start);
	write_stream_format (out, "      \"line_end\": %d,\n", context->line_end);
	write_stream_format (out, "      \"column_start\": %d,\n", context->column_start);
	write_stream_format (out, "      \"column_end\": %d,\n", context->column_end);
	write_stream (out, "      \"selections\": [");

	for (item = context->selections; item; item = g_slist_next (item))
	{
		if (item != context->selections)
		{
			write_stream (out, ",");
		}

		write_selection (item->data, out);
	}

	write_stream_nl (out, "\n      ]");
	g_free (filename_esc);

	write_stream (out, "    }");
}

static void
write_contexts (Info          *info,
                GOutputStream *stream)
{
	/* Write out json formatted list of contexts */
	GDataOutputStream *out;
	GSList *item;

	out = g_data_output_stream_new (stream);

	write_stream_nl (out, "{");
	write_stream_nl (out, "  \"status\": \"ok\",");
	write_stream (out, "  \"data\": [");

	for (item = info->contexts; item; item = g_slist_next (item))
	{
		Context *ctx;

		if (item != info->contexts)
		{
			write_stream (out, ",");
		}

		ctx = item->data;
		write_context (ctx, out);
	}

	write_stream_nl (out, "\n  ]");

	write_stream_nl (out, "}");

	g_object_unref (out);
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
	Info info = {0,};
	GOutputStream *stream;

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

		stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);

		cpg_parser_context_push_input (context, NULL, stream);
		g_object_unref (stream);
	}

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
		stream = g_unix_output_stream_new (STDOUT_FILENO, TRUE);
	}

	if (!stream)
	{
		ret = FALSE;
	}
	else
	{
		g_signal_connect (context,
		                  "context-pushed",
		                  G_CALLBACK (on_context_pushed),
		                  &info);

		g_signal_connect (context,
		                  "context-popped",
		                  G_CALLBACK (on_context_popped),
		                  &info);

		if (cpg_parser_context_parse (context, &error))
		{
			info.contexts = g_slist_reverse (info.contexts);
			write_contexts (&info, stream);
		}
		else
		{
			gchar const *line;
			GFile *file;
			gchar *filename;
			gint lineno;
			gint cstart;
			gint cend;
			gchar *esc;
			GDataOutputStream *out;

			out = g_data_output_stream_new (stream);

			write_stream_nl (out, "{");
			write_stream_nl (out, "  \"status\": \"error\",");
			write_stream_nl (out, "  \"data\": {");

			esc = g_strescape (error->message, NULL);
			write_stream_format (out, "    \"message\": \"%s\",\n", esc);
			g_free (esc);

			line = cpg_parser_context_get_line (context, &lineno);
			cpg_parser_context_get_column (context, &cstart, &cend);

			file = cpg_parser_context_get_file (context);
			filename = file ? g_file_get_path (file) : g_strdup ("");
			esc = g_strescape (filename, NULL);
			g_free (filename);

			if (file)
			{
				g_object_unref (file);
			}

			write_stream_format (out, "    \"filename\": \"%s\",\n", esc);
			g_free (esc);

			esc = g_strescape (line, NULL);
			write_stream_format (out, "    \"line\": \"%s\",\n", esc);
			g_free (esc);

			write_stream_format (out, "    \"lineno\": %d,\n", lineno);
			write_stream_format (out, "    \"column_start\": %d,\n", cstart);
			write_stream_format (out, "    \"column_end\": %d\n", cend);

			write_stream_nl (out, "  }");
			write_stream_nl (out, "}");

			g_object_unref (out);

			ret = FALSE;
		}
	}

	info_free (&info);

	g_object_unref (network);
	g_object_unref (context);

	if (stream)
	{
		g_object_unref (stream);
	}

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

	ctx = g_option_context_new ("NETWORK [--] [PARAMETER...] - dump contexts in cpg format");

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

	return parse_network ((gchar const **)(argv + 1), argc - 1);
}
