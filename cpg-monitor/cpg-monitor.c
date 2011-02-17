#include <cpg-network/cpg-network.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <gio/gunixoutputstream.h>

static GPtrArray *monitored = 0;
static gboolean include_header = FALSE;
static gchar *delimiter = NULL;
static gdouble from = 0;
static gdouble step = 0.001;
static gdouble to = 1;
static gchar *output_file = NULL;

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
parse_monitor_options (gchar const  *option_name,
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
	{"monitor", 'm', 0, G_OPTION_ARG_CALLBACK, parse_monitor_options, "Monitor variable (state.name)", "VAR"},
	{"include-header", 'i', 0, G_OPTION_ARG_NONE, &include_header, "Include header in output", NULL},
	{"delimiter", 'd', 0, G_OPTION_ARG_STRING, &delimiter, "Column delimiter (defaults to tab)", "DELIM"},
	{"time", 't', 0, G_OPTION_ARG_CALLBACK, parse_time, "Time range", "RANGE"},
	{"output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file", "FILE"},
	{NULL}
};

static void
write_monitors (CpgNetwork    *network,
                GSList        *monitors,
                GOutputStream *stream)
{
	gint i;
	gint num;
	gint row;
	GSList *item;
	gdouble time = from;

	if (include_header)
	{
		g_output_stream_write_all (stream,
		                           "# ",
		                           2,
		                           NULL,
		                           NULL,
		                           NULL);

		for (item = monitors; item; item = g_slist_next (item))
		{
			CpgMonitor *monitor = item->data;

			if (item != monitors)
			{
				g_output_stream_write_all (stream,
				                           delimiter,
				                           strlen (delimiter),
				                           NULL,
				                           NULL,
				                           NULL);
			}

			CpgProperty *prop = cpg_monitor_get_property (monitor);

			gchar *name = g_strconcat (cpg_object_get_id (cpg_property_get_object (prop)),
			                           ".",
			                           cpg_property_get_name (prop),
			                           NULL);

			g_output_stream_write_all (stream,
			                           name,
			                           strlen (name),
			                           NULL,
			                           NULL,
			                           NULL);
		}

		g_output_stream_write_all (stream,
		                           "\n",
		                           1,
		                           NULL,
		                           NULL,
		                           NULL);
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

static void
monitor_network (gchar const *filename)
{
	CpgNetwork *network;
	GError *error = NULL;
	gint i;
	GFile *file;
	CpgCompileError *err;

	file = g_file_new_for_commandline_arg (filename);

	network = cpg_network_new_from_file (file, &error);
	g_object_unref (file);

	if (!network)
	{
		g_print ("Failed to load network `%s': %s\n", filename, error->message);
		g_error_free (error);

		return;
	}

	err = cpg_compile_error_new ();

	if (!cpg_object_compile (CPG_OBJECT (network), NULL, err))
	{
		g_print ("Failed to compile network `%s': %s",
		         filename,
		         cpg_compile_error_string (err));

		g_object_unref (network);
		g_object_unref (err);

		return;
	}

	g_object_unref (err);

	GSList *monitors = NULL;

	for (i = 0; i < monitored->len; ++i)
	{
		CpgMonitor *monitor;
		CpgProperty *prop;

		prop = cpg_group_find_property (CPG_GROUP (network),
		                                monitored->pdata[i]);

		if (!prop)
		{
			g_print ("Could not find property `%s' for network `%s'\n",
			         (gchar const *)monitored->pdata[i],
			         filename);

			continue;
		}

		monitor = cpg_monitor_new (network,
		                           prop);

		monitors = g_slist_prepend (monitors, monitor);
	}

	monitors = g_slist_reverse (monitors);
	cpg_network_run (network, from, step, to);

	GOutputStream *out;

	if (output_file != NULL && g_strcmp0 (output_file, "-") != 0)
	{
		GFile *output;
		gchar *outname = g_strconcat (filename, ".txt", NULL);

		output = g_file_new_for_path (outname);

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
			g_print ("Could not create output file `%s': %s\n",
			         outname,
			         error->message);

			g_error_free (error);
		}

		g_free (outname);
		g_object_unref (output);
	}
	else
	{
		out = g_unix_output_stream_new (STDOUT_FILENO,
		                                TRUE);
	}

	if (out)
	{
		write_monitors (network, monitors, out);

		g_output_stream_flush (out, NULL, NULL);
		g_output_stream_close (out, NULL, NULL);
	}

	g_slist_foreach (monitors, (GFunc)g_object_unref, NULL);
	g_object_unref (network);
}

static void
cleanup ()
{
	g_ptr_array_free (monitored, TRUE);
	g_free (delimiter);
	g_free (output_file);
}

int
main (int argc, char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gint i;

	g_type_init ();

	monitored = g_ptr_array_new ();
	delimiter = g_strdup ("\t");

	ctx = g_option_context_new ("- monitor cpg network");
	g_option_context_add_main_entries (ctx, entries, NULL);

	if (!g_option_context_parse (ctx, &argc, &argv, &error))
	{
		g_print ("Failed to parse options: %s\n", error->message);
		g_error_free (error);
		cleanup ();

		return 1;
	}

	if (argc != 2)
	{
		g_print ("Please provide exactly one network to monitor\n");
		cleanup ();

		return 1;
	}

	if (monitored->len == 0)
	{
		g_print ("Please provide at least one state variable to monitor\n");
		cleanup ();

		return 1;
	}

	monitor_network (argv[1]);

	cleanup ();

	return 0;
}
