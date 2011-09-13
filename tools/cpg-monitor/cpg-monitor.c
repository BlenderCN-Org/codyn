/*
 * cpg-monitor.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-selector.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixinputstream.h>
#include <gio/gunixoutputstream.h>
#include <sys/time.h>

static GPtrArray *monitored = 0;
static gboolean include_header = FALSE;
static gchar *delimiter = NULL;
static gdouble from = 0;
static gdouble step = 0.001;
static gdouble to = 1;
static gchar *output_file = NULL;
static gint64 seed = 0;

#define CPG_MONITOR_ERROR (cpg_monitor_error_quark())

static GQuark
cpg_monitor_error_quark ()
{
	return g_quark_from_static_string ("cpg-monitor-error");
}

typedef enum
{
	CPG_MONITOR_ERROR_RANGE
} CpgMonitorError;

static gboolean
parse_monitored (gchar const  *option_name,
                 gchar const  *value,
                 gpointer      data,
                 GError      **error)
{
	g_ptr_array_add (monitored, g_strdup (value));
	return TRUE;
}

static gboolean
parse_time (gchar const  *option_name,
            gchar const  *value,
            gpointer      data,
            GError      **error)
{
	gchar **parts = g_strsplit (value, ":", 0);
	gint len = g_strv_length (parts);

	if (len < 2)
	{
		if (error)
		{
			g_set_error (error,
			             CPG_MONITOR_ERROR,
			             CPG_MONITOR_ERROR_RANGE,
			             "Could not parse range: %s",
			             value);
		}

		g_strfreev (parts);
		return FALSE;
	}

	if (len == 2)
	{
		from = g_ascii_strtod (parts[0], NULL);
		to = g_ascii_strtod (parts[1], NULL);
	}
	else
	{
		from = g_ascii_strtod (parts[0], NULL);
		step = g_ascii_strtod (parts[1], NULL);
		to = g_ascii_strtod (parts[2], NULL);
	}

	g_strfreev (parts);

	if ((to - (from + step)) >= to - from)
	{
		if (error)
		{
			g_set_error (error,
			             CPG_MONITOR_ERROR,
			             CPG_MONITOR_ERROR_RANGE,
			             "Invalid range: %s",
			             value);
		}

		g_strfreev (parts);
		return FALSE;
	}

	return TRUE;
}

static GOptionEntry entries[] = {
	{"monitor", 'm', 0, G_OPTION_ARG_CALLBACK, parse_monitored, "Selector for properties to monitor (e.g. /state_.*/.\"{x,y}\")", "SEL"},
	{"include-header", 'i', 0, G_OPTION_ARG_NONE, &include_header, "Include header in output", NULL},
	{"delimiter", 'd', 0, G_OPTION_ARG_STRING, &delimiter, "Column delimiter (defaults to tab)", "DELIM"},
	{"time", 't', 0, G_OPTION_ARG_CALLBACK, parse_time, "Time range (from:to or from:step:to, defaults to 0:0.01:1)", "RANGE"},
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file (defaults to standard output)", "FILE"},
	{"seed", 's', 0, G_OPTION_ARG_INT64, &seed, "Random numbers seed (defaults to current time)", "SEED"},
	{NULL}
};

static void
write_monitors (CpgNetwork    *network,
                GSList        *monitors,
                GSList        *names,
                GOutputStream *stream)
{
	gint i;
	gint num;
	gint row;
	GSList *item_monitor;
	GSList *item_name;
	gdouble time = from;

	if (include_header)
	{
		gchar *header_start = g_strdup_printf ("#%1$st%1$s", delimiter);

		g_output_stream_write_all (stream,
		                           header_start,
		                           strlen (header_start),
		                           NULL,
		                           NULL,
		                           NULL);

		item_monitor = monitors;
		item_name = names;

		while (item_monitor)
		{
			gchar *name = item_name->data;

			g_output_stream_write_all (stream,
			                           delimiter,
			                           strlen (delimiter),
			                           NULL,
			                           NULL,
			                           NULL);

			g_output_stream_write_all (stream,
			                           name,
			                           strlen (name),
			                           NULL,
			                           NULL,
			                           NULL);

			item_monitor = g_slist_next (item_monitor);
			item_name = g_slist_next (item_name);
		}

		g_output_stream_write_all (stream,
		                           "\n",
		                           1,
		                           NULL,
		                           NULL,
		                           NULL);

		g_free (header_start);
	}

	num = g_slist_length (monitors);
	gdouble const **data = g_new (gdouble const *, num);
	guint size;

	for (i = 0; i < num; ++i)
	{
		data[i] = cpg_monitor_get_data (monitors->data, &size);
		monitors = monitors->next;
	}

	for (row = 0; row < size; ++row)
	{
		gchar value[G_ASCII_DTOSTR_BUF_SIZE];

		g_ascii_dtostr (value,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                time);

		g_output_stream_write_all (stream,
		                           value,
		                           strlen (value),
		                           NULL,
		                           NULL,
		                           NULL);

		time += step;

		for (i = 0; i < num; ++i)
		{
			g_output_stream_write_all (stream,
			                           delimiter,
			                           strlen (delimiter),
			                           NULL,
			                           NULL,
			                           NULL);

			g_ascii_dtostr (value,
			                G_ASCII_DTOSTR_BUF_SIZE,
			                data[i][row]);

			g_output_stream_write_all (stream,
			                           value,
			                           strlen (value),
			                           NULL,
			                           NULL,
			                           NULL);
		}

		g_output_stream_write_all (stream, "\n", 1, NULL, NULL, NULL);
	}

	g_free (data);
}

static GSList *
find_matching_properties (CpgNetwork  *network,
                          gchar const *expression)
{
	GError *err = NULL;
	CpgSelector *sel;

	sel = cpg_selector_parse (CPG_OBJECT (network), expression, &err);

	if (err)
	{
		g_printerr ("Failed to parse selector \"%s\": %s\n", expression, err->message);
		g_error_free (err);

		return NULL;
	}

	CpgEmbeddedContext *context = cpg_embedded_context_new ();
	GSList *selection = cpg_selector_select (sel,
	                                         G_OBJECT (network),
	                                         CPG_SELECTOR_TYPE_PROPERTY,
	                                         context);
	GSList *element = selection;
	GSList *properties = NULL;

	while (element)
	{
		CpgProperty *property = cpg_selection_get_object (element->data);
		properties = g_slist_prepend (properties, property);

		g_object_unref (element->data);
		element = g_slist_next (element);
	}

	g_slist_free (selection);
	g_object_unref (context);
	g_object_unref (sel);

	return properties;
}

static gint
monitor_network (gchar const *filename)
{
	CpgNetwork *network;
	GError *error = NULL;
	gint i;
	CpgCompileError *err;
	gint ret;

	if (g_strcmp0 (filename, "-") == 0)
	{
		GInputStream *stream = g_unix_input_stream_new (STDIN_FILENO, TRUE);
		network = cpg_network_new_from_stream (stream, &error);
		g_object_unref (stream);
	}
	else
	{
		GFile *file = g_file_new_for_commandline_arg (filename);
		network = cpg_network_new_from_file (file, &error);
		g_object_unref (file);
	}

	if (!network)
	{
		g_printerr ("Failed to load network `%s': %s\n", filename, error->message);
		g_error_free (error);

		return 1;
	}

	err = cpg_compile_error_new ();

	if (!cpg_object_compile (CPG_OBJECT (network), NULL, err))
	{
		gchar *msg;

		msg = cpg_compile_error_get_formatted_string (err);

		g_printerr ("Failed to compile network `%s'\n\n%s\n",
		            filename,
		            msg);

		g_free (msg);

		g_object_unref (network);
		g_object_unref (err);

		return 1;
	}

	g_object_unref (err);

	GSList *monitors = NULL;
	GSList *names = NULL;

	for (i = monitored->len - 1; i >= 0; --i)
	{
		GSList *properties = find_matching_properties (network, monitored->pdata[i]);
		GSList *prop = properties;

		while (prop)
		{
			CpgMonitor *monitor;

			monitor = cpg_monitor_new (network,
			                           prop->data);

			monitors = g_slist_prepend (monitors, monitor);
			names = g_slist_prepend (names, cpg_property_get_full_name (prop->data));

			prop = g_slist_next (prop);
		}

		g_slist_free (properties);
	}

	if (!monitors)
	{
		g_printerr ("There were no properties found to monitor\n");
		g_object_unref (network);

		return 1;
	}

	cpg_network_run (network, from, step, to);

	GOutputStream *out;

	if (output_file != NULL && g_strcmp0 (output_file, "-") != 0)
	{
		GFile *output;

		output = g_file_new_for_path (output_file);

		out = G_OUTPUT_STREAM (g_file_create (output,
		                                      G_FILE_CREATE_REPLACE_DESTINATION,
		                                      NULL,
		                                      &error));

		if (!out && error->code == G_IO_ERROR_EXISTS)
		{
			g_error_free (error);
			error = NULL;

			out = G_OUTPUT_STREAM (g_file_replace (output,
			                                       NULL,
			                                       FALSE,
			                                       G_FILE_CREATE_NONE,
			                                       NULL,
			                                       &error));
		}

		if (!out)
		{
			g_printerr ("Could not create output file `%s': %s\n",
			            output_file,
			            error->message);

			g_error_free (error);
		}

		g_object_unref (output);
	}
	else
	{
		out = g_unix_output_stream_new (STDOUT_FILENO,
		                                TRUE);
	}

	if (out)
	{
		write_monitors (network, monitors, names, out);

		g_output_stream_flush (out, NULL, NULL);
		g_output_stream_close (out, NULL, NULL);

		ret = 0;
	}
	else
	{
		ret = 1;
	}

	g_slist_foreach (monitors, (GFunc)g_object_unref, NULL);
	g_slist_free (monitors);
	g_slist_foreach (names, (GFunc)g_free, NULL);
	g_slist_free (names);

	g_object_unref (network);

	return ret;
}

static void
cleanup ()
{
	g_ptr_array_free (monitored, TRUE);
	g_free (delimiter);
	g_free (output_file);
}

int
main (int argc,
      char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gchar const *file;
	gint ret = 1;
	struct timeval tv;

	g_type_init ();

	gettimeofday (&tv, NULL);
	seed = tv.tv_sec * 1000 + tv.tv_usec / 1000;

	monitored = g_ptr_array_new ();
	delimiter = g_strdup ("\t");

	ctx = g_option_context_new ("-m <SELECTOR> [-m ...] [NETWORK] - monitor cpg network");
	g_option_context_set_summary (ctx, "Omit the network name or use a dash '-' to read from standard input.");
	g_option_context_add_main_entries (ctx, entries, NULL);

	if (!g_option_context_parse (ctx, &argc, &argv, &error))
	{
		g_printerr ("Failed to parse options: %s\n", error->message);
		g_error_free (error);
		cleanup ();

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
		g_printerr ("Too many arguments. Please provide at most one network to monitor\n");
		cleanup ();

		return 1;
	}

	if (monitored->len == 0)
	{
		g_printerr ("Please specify state variables to monitor\n");
		cleanup ();

		return 1;
	}

	srand (seed);

	ret = monitor_network (file);

	cleanup ();

	return ret;
}
