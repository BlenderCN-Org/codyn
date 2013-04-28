/*
 * cdn-monitor.c
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

#include <codyn/codyn.h>
#include <codyn/cdn-selector.h>
#include <gio/gio.h>
#include <glib/gprintf.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <math.h>
#include <locale.h>
#include <codyn/cdn-cfile-stream.h>

#include "monitor.h"
#include "implementation.h"

static GPtrArray *monitored = NULL;
static gboolean include_header = FALSE;
static gchar *delimiter = NULL;
static gdouble from = 0;
static gdouble step = 0.001;
static gdouble to = 1;
static guint seed = 0;
static gboolean seed_set = FALSE;
static gboolean simplify = FALSE;
static gboolean timestamp = FALSE;

#define CDN_MONITOR_ERROR (cdn_monitor_error_quark())

static GQuark
cdn_monitor_error_quark ()
{
	return g_quark_from_static_string ("cdn-monitor-error");
}

typedef enum
{
	CDN_MONITOR_ERROR_RANGE
} CdnMonitorError;

static gboolean
parse_monitored (gchar const  *option_name,
                 gchar const  *value,
                 gpointer      data,
                 GError      **error)
{
	CdnMonitored *mon;

	if (monitored->len == 0)
	{
		mon = cdn_monitored_new ();
		g_ptr_array_add (monitored, mon);
	}
	else
	{
		mon = monitored->pdata[monitored->len - 1];

		if (!mon)
		{
			mon = cdn_monitored_new ();
			monitored->pdata[monitored->len - 1] = mon;
		}
	}

	g_ptr_array_add (mon->monitored, g_strdup (value));
	return TRUE;
}

static gboolean
parse_output_file (gchar const  *option_name,
                   gchar const  *value,
                   gpointer      data,
                   GError      **error)
{
	CdnMonitored *mon;

	if (monitored->len == 0)
	{
		mon = cdn_monitored_new ();
		g_ptr_array_add (monitored, mon);
	}
	else
	{
		mon = monitored->pdata[monitored->len - 1];
	}

	g_free (mon->output_file);
	mon->output_file = g_strdup (value);

	g_ptr_array_add (monitored, NULL);
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
			             CDN_MONITOR_ERROR,
			             CDN_MONITOR_ERROR_RANGE,
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
			             CDN_MONITOR_ERROR,
			             CDN_MONITOR_ERROR_RANGE,
			             "Invalid range: %s",
			             value);
		}

		g_strfreev (parts);
		return FALSE;
	}

	return TRUE;
}

static void
parse_seed (gchar const  *option_name,
            gchar const  *value,
            gpointer      data,
            GError      **error)
{
	seed_set = TRUE;
	seed = (guint)g_ascii_strtoull (value, NULL, 10);
}

static GOptionEntry entries[] = {
	{"monitor", 'm', 0, G_OPTION_ARG_CALLBACK, parse_monitored,
	 "Selector for variables to monitor (e.g. /state_.*/.\"{x,y}\")", "SEL"},
	{"include-header", 'i', 0, G_OPTION_ARG_NONE, &include_header,
	 "Include header in output", NULL},
	{"delimiter", 'd', 0, G_OPTION_ARG_STRING, &delimiter,
	 "Column delimiter (defaults to tab)", "DELIM"},
	{"time", 't', 0, G_OPTION_ARG_CALLBACK, parse_time,
	 "Time range (from:to or from:step:to, defaults to 0:0.01:1)", "RANGE"},
	{"output", 'o', 0, G_OPTION_ARG_CALLBACK, parse_output_file,
	 "Output file (defaults to standard output)", "FILE"},
	{"seed", 's', 0, G_OPTION_ARG_CALLBACK, parse_seed,
	 "Random numbers seed (defaults to current time)", "SEED"},
	{"simplify", 'x', 0, G_OPTION_ARG_NONE, &simplify,
	 "Enable global simplifications", NULL},
	{"timestamp", 'p', 0, G_OPTION_ARG_NONE, &timestamp,
	 "Write a timestamp (Unix time) at the beginning of each line", NULL},
	{NULL}
};

static void
write_headers (CdnMonitored *monmon)
{
	GOutputStream *stream = monmon->stream;
	GSList *names = monmon->names;

	if (!include_header)
	{
		return;
	}

	g_output_stream_write_all (stream,
	                           "time",
	                           4,
	                           NULL,
	                           NULL,
	                           NULL);

	while (names)
	{
		gchar const *name = names->data;

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

		names = g_slist_next (names);
	}

	g_output_stream_write_all (stream,
	                           "\n",
	                           1,
	                           NULL,
	                           NULL,
	                           NULL);
}

static CdnSelector *
make_selector (CdnNetwork  *network,
               gchar const *expression,
               gint        *ncol,
               gint        *nrow)
{
	GError *err = NULL;
	CdnSelector *sel;
	static GRegex *r = NULL;
	GMatchInfo *info;

	*ncol = -1;
	*nrow = -1;

	if (!r)
	{
		r = g_regex_new ("(.*?)\\[\\s*([0-9]+)(\\s*,\\s*([0-9]+))?\\s*\\]$",
		                 0,
		                 0,
		                 NULL);
	}

	if (g_regex_match (r, expression, 0, &info))
	{
		gchar *ex;

		ex = g_match_info_fetch (info, 1);
		sel = cdn_selector_parse (CDN_OBJECT (network), ex, &err);
		g_free (ex);

		if (sel)
		{
			gchar *row;

			row = g_match_info_fetch (info, 2);

			if (row && *row)
			{
				gchar *col;

				*nrow = g_ascii_strtoll (row, NULL, 10);

				col = g_match_info_fetch (info, 4);

				if (col && *col)
				{
					*ncol = g_ascii_strtoll (col, NULL, 10);
				}

				g_free (col);
			}

			g_free (row);
		}

		g_match_info_free (info);
	}
	else
	{
		sel = cdn_selector_parse (CDN_OBJECT (network), expression, &err);
	}

	if (err)
	{
		g_printerr ("Failed to parse selector \"%s\": %s\n", expression, err->message);
		g_error_free (err);

		return NULL;
	}

	return sel;
}

static void
resolve_output_stream (CdnMonitored *monmon)
{
	GError *error = NULL;

	if (monmon->stream)
	{
		return;
	}

	if (monmon->output_file != NULL && g_strcmp0 (monmon->output_file, "-") != 0)
	{
		GFile *output;

		output = g_file_new_for_path (monmon->output_file);

		monmon->stream = G_OUTPUT_STREAM (g_file_create (output,
		                                                 G_FILE_CREATE_REPLACE_DESTINATION,
		                                                 NULL,
		                                                 &error));

		if (!monmon->stream && error->code == G_IO_ERROR_EXISTS)
		{
			g_error_free (error);
			error = NULL;

			monmon->stream = G_OUTPUT_STREAM (g_file_replace (output,
			                                                  NULL,
			                                                  FALSE,
			                                                  G_FILE_CREATE_NONE,
			                                                  NULL,
			                                                  &error));
		}

		if (!monmon->stream)
		{
			g_printerr ("Could not create output file `%s': %s\n",
			            monmon->output_file,
			            error->message);

			g_error_free (error);
		}

		g_object_unref (output);
	}
	else
	{
		monmon->stream = cdn_cfile_stream_new (stdout);
	}
}

static double
get_current_time ()
{
	struct timeval tv;
	struct timezone tz;

	gettimeofday (&tv, &tz);

	return tv.tv_sec + 1.e-6 * tv.tv_usec;
}

static void
record_monitors (CdnMonitored *monitored)
{
	// Record all monitors
	gboolean first = TRUE;
	GSList *monitors;

	if (!monitored)
	{
		return;
	}

	if (timestamp)
	{
		gchar *stamp;

		stamp = g_strdup_printf ("%f", get_current_time ());

		g_output_stream_write_all (monitored->stream,
		                           stamp,
		                           strlen (stamp),
		                           NULL,
		                           NULL,
		                           NULL);
		g_free (stamp);
		
		first = FALSE;
	}

	monitors = monitored->monitors;

	while (monitors)
	{
		CdnMonitorVariable *mon = monitors->data;
		CdnDimension dim;
		gdouble const *values;
		gint num;

		values = mon->get_values (mon);

		dim = mon->dimension;
		num = cdn_dimension_size (&dim);

		if (!first)
		{
			g_output_stream_write_all (monitored->stream,
			                           delimiter,
			                           strlen (delimiter),
			                           NULL,
			                           NULL,
			                           NULL);
		}
		else
		{
			first = FALSE;
		}

		if (mon->row >= 0)
		{
			gchar value[G_ASCII_DTOSTR_BUF_SIZE];
			gint idx;

			if (mon->col >= 0)
			{
				idx = mon->row * dim.columns + mon->col;
			}
			else
			{
				idx = mon->row;
			}

			if (idx >= num)
			{
				strcpy (value, "NAN");
			}
			else
			{
				g_ascii_dtostr (value,
				                G_ASCII_DTOSTR_BUF_SIZE,
				                values[idx]);
			}

			g_output_stream_write_all (monitored->stream,
			                           value,
			                           strlen (value),
			                           NULL,
			                           NULL,
			                           NULL);
		}
		else
		{
			gint i;

			for (i = 0; i < num; ++i)
			{
				gchar value[G_ASCII_DTOSTR_BUF_SIZE];

				g_ascii_dtostr (value,
				                G_ASCII_DTOSTR_BUF_SIZE,
				                values[i]);

				if (i != 0)
				{
					g_output_stream_write_all (monitored->stream,
					                           delimiter,
					                           strlen (delimiter),
					                           NULL,
					                           NULL,
					                           NULL);
				}

				g_output_stream_write_all (monitored->stream,
				                           value,
				                           strlen (value),
				                           NULL,
				                           NULL,
				                           NULL);
			}
		}

		monitors = g_slist_next (monitors);
	}

	g_output_stream_write_all (monitored->stream,
	                           "\n",
	                           1,
	                           NULL,
	                           NULL,
	                           NULL);
}

static void
write_values (CdnMonitorImplementation *implementation)
{
	gint i;

	for (i = 0; i < monitored->len; ++i)
	{
		record_monitors (monitored->pdata[i]);
	}
}

static gboolean
resolve_monitors (CdnMonitorImplementation *implementation,
                  CdnMonitored             *monmon)
{
	gint i;
	GPtrArray *monitored;

	if (!monmon)
	{
		return TRUE;
	}

	monitored = monmon->monitored;

	for (i = monitored->len - 1; i >= 0; --i)
	{
		GSList *variables;
		GSList *prop;
		CdnSelector *sel;
		gint nrow;
		gint ncol;

		sel = make_selector (implementation->network,
		                     monitored->pdata[i],
		                     &nrow,
		                     &ncol);

		if (!sel)
		{
			return FALSE;
		}

		variables = implementation->resolve (implementation, sel);
		prop = variables;

		while (prop)
		{
			CdnMonitorVariable *mon = prop->data;
			CdnDimension ndim;
			gchar *name;

			mon->row = nrow;
			mon->col = ncol;

			monmon->monitors = g_slist_prepend (monmon->monitors,
			                                    mon);

			name = mon->get_name (mon);
			ndim = mon->dimension;

			if (mon->row >= 0 || cdn_dimension_is_one (&ndim))
			{
				monmon->names = g_slist_prepend (monmon->names,
				                                 name);
			}
			else
			{
				gint r;

				for (r = ndim.rows - 1; r >= 0; --r)
				{
					gint c;

					for (c = ndim.columns - 1; c >= 0; --c)
					{
						gchar *s;

						s = g_strdup_printf ("%s[%d,%d]",
						                     name,
						                     r,
						                     c);

						monmon->names =
							g_slist_prepend (monmon->names,
							                 s);
					}
				}

				g_free (name);
			}

			prop = g_slist_next (prop);
		}

		g_slist_free (variables);
	}

	// Prepend 't' monitor
	monmon->monitors = g_slist_prepend (monmon->monitors,
	                                    implementation->get_time (implementation));

	resolve_output_stream (monmon);

	if (monmon->stream)
	{
		write_headers (monmon);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static gboolean
resolve_all_monitors (CdnMonitorImplementation *implementation)
{
	gint i;

	for (i = monitored->len - 1; i >= 0; --i)
	{
		if (!resolve_monitors (implementation, monitored->pdata[i]))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gint
run_simple_monitor (CdnMonitorImplementation *implementation)
{
	gdouble t;

	if (!resolve_all_monitors (implementation))
	{
		return 1;
	}

	t = from;

	implementation->begin (implementation, t, step);

	write_values (implementation);

	while (t < to)
	{
		gdouble realstep;

		if (to - t < step)
		{
			step = to - t;
		}

		realstep = implementation->step (implementation,
		                                 t,
		                                 step);

		t += realstep;

		write_values (implementation);

		if (realstep <= 0 || implementation->terminated (implementation))
		{
			break;
		}
	}

	if (implementation->end)
	{
		implementation->end (implementation);
	}

	return 0;
}

static gint
monitor_network (gchar const *filename)
{
	CdnMonitorImplementation *implementation;
	gint ret;

	if (g_str_has_suffix (filename, ".so"))
	{
		implementation = cdn_monitor_implementation_rawc_new (filename);
	}
	else
	{
		implementation = cdn_monitor_implementation_codyn_new (filename);
	}

	if (seed_set)
	{
		implementation->set_seed (implementation, seed);
	}

	if (!implementation)
	{
		return 1;
	}

	if (simplify && implementation->simplify)
	{
		implementation->simplify (implementation);
	}

	ret = run_simple_monitor (implementation);
	cdn_monitor_implementation_free (implementation);
	return ret;
}

static void
cleanup ()
{
	g_ptr_array_free (monitored, TRUE);
	g_free (delimiter);
}

int
main (int argc,
      char *argv[])
{
	GOptionContext *ctx;
	GError *error = NULL;
	gchar const *file;
	gint ret = 1;

#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

	setlocale (LC_ALL, "");

	monitored = g_ptr_array_new_with_free_func ((GDestroyNotify)cdn_monitored_free);

	delimiter = g_strdup ("\t");

	ctx = g_option_context_new ("-m <SELECTOR> [-m ...] [-v RANGE...] [NETWORK] - monitor Codyn network");
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

	ret = monitor_network (file);

	cleanup ();

	return ret;
}
