#include <cpg-network/cpg-network.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>

static GPtrArray *monitors = 0;
static gboolean include_header = FALSE;
static gchar *delimiter = NULL;
static gdouble from = 0;
static gdouble step = 0.001;
static gdouble to = 1;

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
	g_ptr_array_add (monitors, g_strdup (value));
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
	{"monitor", 'm', 0, G_OPTION_ARG_CALLBACK, parse_monitor_options, "Monitor", "VAR"},
	{"include-header", 'i', 0, G_OPTION_ARG_NONE, &include_header, "Include header in output", NULL},
	{"delimiter", 'd', 0, G_OPTION_ARG_STRING, &delimiter, "Column delimiter (defaults to tab)", "DELIM"},
	{"time", 't', 0, G_OPTION_ARG_CALLBACK, parse_time, "Time range", "RANGE"},
	{NULL}
};

static CpgProperty *
find_property (CpgNetwork  *network,
               gchar const *id)
{
	gchar **parts = g_strsplit (id, ".", 0);
	CpgObject *obj;
	gchar const *prop;
	CpgProperty *ret;

	if (g_strv_length (parts) == 1)
	{
		obj = cpg_network_get_globals (network);
		prop = parts[0];
	}
	else
	{
		obj = cpg_network_get_object (network, parts[0]);
		prop = parts[1];
	}

	ret = cpg_object_get_property (obj, prop);
	g_strfreev (parts);

	return ret;
}

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

			gchar *name = g_strconcat (cpg_object_get_id (cpg_monitor_get_object (monitor)),
			                           ".",
			                           cpg_property_get_name (cpg_monitor_get_property (monitor)),
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

	network = cpg_network_new_from_file (filename, &error);

	if (!network)
	{
		g_print ("Failed to load network `%s': %s\n", filename, error->message);
		g_error_free (error);

		return;
	}

	GSList *mons = NULL;

	for (i = 0; i < monitors->len; ++i)
	{
		CpgMonitor *monitor;
		CpgProperty *prop;

		prop = find_property (network, monitors->pdata[i]);

		if (!prop)
		{
			g_print ("Could not find property `%s' for network `%s'\n",
			         (gchar const *)monitors->pdata[i],
			         filename);

			continue;
		}

		monitor = cpg_monitor_new (network,
		                           cpg_property_get_object (prop),
		                           cpg_property_get_name (prop));
		mons = g_slist_prepend (mons, monitor);
	}

	mons = g_slist_reverse (mons);
	cpg_network_run (network, from, step, to);

	GSList *item;

	GFile *output;
	gchar *outname = g_strconcat (filename, ".txt", NULL);

	output = g_file_new_for_path (outname);

	GFileOutputStream *out;
	out = g_file_create (output, G_FILE_CREATE_REPLACE_DESTINATION, NULL, &error);

	if (!out && error->code == G_IO_ERROR_EXISTS)
	{
		g_error_free (error);
		error = NULL;

		out = g_file_replace (output,
		                      NULL,
		                      FALSE,
		                      G_FILE_CREATE_NONE,
		                      NULL,
		                      &error);
	}

	if (!out)
	{
		g_print ("Could not create output file `%s': %s\n", outname, error->message);
		g_error_free (error);
	}
	else
	{
		write_monitors (network, mons, G_OUTPUT_STREAM (out));

		g_output_stream_flush (G_OUTPUT_STREAM (out), NULL, NULL);
		g_output_stream_close (G_OUTPUT_STREAM (out), NULL, NULL);
	}

	g_free (outname);
	g_slist_foreach (mons, (GFunc)cpg_ref_counted_unref, NULL);

	g_object_unref (output);
	g_object_unref (network);
}

static void
cleanup ()
{
	g_ptr_array_free (monitors, TRUE);
	g_free (delimiter);
}

int
main (int argc, char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gint i;

	g_type_init ();

	monitors = g_ptr_array_new ();
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

	if (argc <= 1)
	{
		g_print ("Please provide at least one network to monitor\n");
		cleanup ();

		return 1;
	}

	if (monitors->len == 0)
	{
		g_print ("Please provide at least one state variable to monitor\n");
		cleanup ();

		return 1;
	}

	for (i = 1; i < argc; ++i)
	{
		monitor_network (argv[i]);
	}

	cleanup ();

	return 0;
}
