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
#include "defines.h"

static GPtrArray *monitored = NULL;
static gboolean include_header = FALSE;
static gdouble from = 0;
static gdouble step = 0;
static gdouble to = 1;
static guint seed = 0;
static gboolean seed_set = FALSE;
static gboolean simplify = FALSE;
static gboolean timestamp = FALSE;
static gboolean rawc = FALSE;
static gchar *precision = NULL;
static gboolean display = FALSE;

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

	if (len == 1)
	{
		to = g_ascii_strtod (parts[0], NULL);
	}
	else if (len == 2)
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

	if (step != 0 && ((to - (from + step)) >= to - from))
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
	{"time", 't', 0, G_OPTION_ARG_CALLBACK, parse_time,
	 "Time range (to, from:to or from:step:to, defaults to 1)", "RANGE"},
	{"output", 'o', 0, G_OPTION_ARG_CALLBACK, parse_output_file,
	 "Output file (defaults to standard output)", "FILE"},
	{"seed", 's', 0, G_OPTION_ARG_CALLBACK, parse_seed,
	 "Random numbers seed (defaults to current time)", "SEED"},
	{"simplify", 'x', 0, G_OPTION_ARG_NONE, &simplify,
	 "Enable global simplifications", NULL},
	{"timestamp", 'p', 0, G_OPTION_ARG_NONE, &timestamp,
	 "Write a timestamp (Unix time) at the beginning of each line", NULL},
	{"rawc", 'r', 0, G_OPTION_ARG_NONE, &rawc,
	 "Use rawc (if possible) to run the network", NULL},
	{"precision", 'c', 0, G_OPTION_ARG_STRING, &precision,
	 "Precision at which to print values (printf style)", "FORMAT"},
	{"display", 'd', 0, G_OPTION_ARG_NONE, &display,
	 "Display variable contents after simulation", NULL},
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
	GSList *monitors;

	if (!monitored)
	{
		return;
	}

	if (timestamp)
	{
		gchar *stamp;

		stamp = g_strdup_printf (precision, get_current_time ());

		g_output_stream_write_all (monitored->stream,
		                           stamp,
		                           strlen (stamp),
		                           NULL,
		                           NULL,
		                           NULL);
		g_free (stamp);
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

		if (mon->row >= 0)
		{
			gchar *rv;
			gint idx;

			if (mon->col >= 0)
			{
				idx = mon->col * dim.rows + mon->row;
			}
			else
			{
				idx = mon->row;
			}

			if (idx >= num)
			{
				rv = g_strdup ("NAN");
			}
			else
			{
				rv = g_strdup_printf (precision, values[idx]);
			}

			g_output_stream_write_all (monitored->stream,
			                           rv,
			                           strlen (rv),
			                           NULL,
			                           NULL,
			                           NULL);

			g_free (rv);
		}
		else
		{
			gint i;

			for (i = 0; i < num; ++i)
			{
				gchar *rv;

				rv = g_strdup_printf (precision, values[i]);

				g_output_stream_write_all (monitored->stream,
				                           rv,
				                           strlen (rv),
				                           NULL,
				                           NULL,
				                           NULL);

				g_free (rv);
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
display_monitors (CdnMonitored *monitored)
{
	GSList *monitors;

	monitors = monitored->monitors;

	while (monitors)
	{
		CdnMonitorVariable *mon = monitors->data;
		CdnDimension dim;
		gdouble const *values;
		CdnMatrix *m;
		gchar *name;
		gchar *val;

		values = mon->get_values (mon);

		dim = mon->dimension;

		m = cdn_matrix_new (values, &dim);

		name = mon->get_name (mon);

		g_output_stream_write_all (monitored->stream,
		                           name,
		                           strlen (name),
		                           NULL,
		                           NULL,
		                           NULL);

		g_output_stream_write_all (monitored->stream,
		                           ": ",
		                           2,
		                           NULL,
		                           NULL,
		                           NULL);

		val = cdn_matrix_to_string (m);

		g_output_stream_write_all (monitored->stream,
		                           val,
		                           strlen (val),
		                           NULL,
		                           NULL,
		                           NULL);

		g_free (name);
		g_free (val);

		g_output_stream_write_all (monitored->stream,
		                           "\n\n",
		                           2,
		                           NULL,
		                           NULL,
		                           NULL);

		cdn_matrix_free (m);

		monitors = g_slist_next (monitors);
	}
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

static void
display_values (CdnMonitorImplementation *implementation)
{
	gint i;

	for (i = 0; i < monitored->len; ++i)
	{
		display_monitors (monitored->pdata[i]);
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

	if (step == 0)
	{
		if (implementation->default_timestep)
		{
			step = implementation->default_timestep (implementation);
		}
		else
		{
			step = 0.001;
		}

		if (from > to)
		{
			step = -step;
		}
	}

	implementation->begin (implementation, t, step);

	if (!display)
	{
		write_values (implementation);
	}

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

		if (!display)
		{
			write_values (implementation);
		}

		if (realstep <= 0 || implementation->terminated (implementation))
		{
			break;
		}
	}

	if (implementation->end)
	{
		implementation->end (implementation);
	}

	if (display)
	{
		display_values (implementation);
	}

	return 0;
}

static gboolean
query_mtime (GFile    *f,
             GTimeVal *mod)
{
	GFileInfo *info;

	info = g_file_query_info (f,
	                          G_FILE_ATTRIBUTE_TIME_MODIFIED,
	                          0,
	                          NULL,
	                          NULL);

	if (!info)
	{
		return FALSE;
	}

	g_file_info_get_modification_time (info, mod);
	g_object_unref (info);
	return TRUE;
}

static gboolean
generate_rawc (gchar const *filename)
{
	gchar *wd;
	gint exit_status = 0;
	gboolean ret = TRUE;

	gchar const *argv[] = {
		"cdn-rawc",
		"--compile",
		"--shared",
		filename,
		NULL,
	};

	wd = g_path_get_dirname (filename);

	g_printerr ("Generating rawc version of the network...\n");

	if (!g_spawn_sync (wd,
	                   (gchar **)argv,
	                   NULL,
	                   G_SPAWN_SEARCH_PATH |
	                   G_SPAWN_STDOUT_TO_DEV_NULL,
	                   (GSpawnChildSetupFunc)NULL,
	                   NULL,
	                   NULL,
	                   NULL,
	                   &exit_status,
	                   NULL) || exit_status != 0)
	{
		ret = FALSE;
	}

	return ret;
}

static gchar *
try_rawc (gchar const *filename)
{
	GFile *d;
	GFile *f;
	gchar *b;
	gchar *dpos;
	gchar *libname;
	GFile *fi;
	GTimeVal cdnmod;
	GTimeVal rawcmod;
	gchar *ret;

	if (!rawc)
	{
		return g_strdup (filename);
	}

	// Check if the file is already a rawc file
	if (g_str_has_suffix (filename, DYLIB_SUFFIX))
	{
		return g_strdup (filename);
	}

	// Query cdn file
	fi = g_file_new_for_commandline_arg (filename);

	if (!query_mtime (fi, &cdnmod))
	{
		g_object_unref (fi);
		return g_strdup (filename);
	}

	// Try to find the corresponding rawc dynamic library
	d = g_file_get_parent (fi);
	b = g_file_get_basename (fi);

	dpos = strchr (b, '.');

	if (dpos)
	{
		*dpos = '\0';
	}

	libname = g_strconcat ("lib", b, DYLIB_SUFFIX, NULL);

	f = g_file_get_child (d, libname);

	g_free (b);
	g_free (libname);
	g_object_unref (d);

	ret = g_file_get_path (f);

	if (!query_mtime (f, &rawcmod) ||
	    (cdnmod.tv_sec + cdnmod.tv_usec * 1e-6) >
	    (rawcmod.tv_sec + rawcmod.tv_usec * 1e-6))
	{
		// Regenerate rawc file, or try to anyway
		if (!generate_rawc (filename))
		{
			g_free (ret);
			ret = g_strdup (filename);
		}
	}

	g_object_unref (f);
	return ret;
}

static gint
monitor_network (gchar const *filename)
{
	CdnMonitorImplementation *implementation;
	gint ret;
	gchar *f;

	f = try_rawc (filename);

	if (g_str_has_suffix (f, DYLIB_SUFFIX))
	{
		implementation = cdn_monitor_implementation_rawc_new (f);
	}
	else
	{
		implementation = cdn_monitor_implementation_codyn_new (f);
	}

	g_free (f);

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
	g_free (precision);
}

static gchar **
parse_interactive_flags (gchar const  *arg0,
                         gchar const  *filename,
                         gint         *l,
                         GError      **error)
{
	gchar **ret = NULL;
	GFile *f;
	GFileInputStream *s;
	GDataInputStream *d;
	gboolean seencomment = FALSE;

	*l = 0;
	f = g_file_new_for_commandline_arg (filename);

	s = g_file_read (f, NULL, error);

	if (!s)
	{
		g_object_unref (f);
		return NULL;
	}

	d = g_data_input_stream_new (G_INPUT_STREAM (s));

	ret = g_new0 (gchar *, 2);

	ret[0] = g_strdup (arg0);
	*l = 1;

	while (TRUE)
	{
		gchar *line;
		gchar *ptr;
		gsize n;
		gboolean isempty = TRUE;

		line = g_data_input_stream_read_line (d, &n, NULL, error);

		// Skip spaces
		for (ptr = line; ptr && *ptr; ptr = g_utf8_next_char (ptr))
		{
			gunichar c = g_utf8_get_char (ptr);

			if (!g_unichar_isspace (c))
			{
				isempty = FALSE;

				if (c != '#')
				{
					// Not a comment, break out
					g_free (line);
					line = NULL;
				}
				else
				{
					// Skip hash
					ptr = g_utf8_next_char (ptr);
				}

				break;
			}
		}

		if (line == NULL || (seencomment && isempty))
		{
			break;
		}

		if (g_utf8_get_char (ptr) != '!' || seencomment)
		{
			gint nargs;
			gchar **argvp;
			gchar **tmp;
			gint i;

			seencomment = TRUE;

			// Parse command line arguments from the line
			if (!g_shell_parse_argv (ptr, &nargs, &argvp, error))
			{
				g_strfreev (ret);

				ret = NULL;
				*l = 0;

				g_free (line);
				break;
			}

			tmp = ret;
			ret = g_new0 (gchar *, *l + nargs + 1);

			for (i = 0; i < *l; ++i)
			{
				ret[i] = tmp[i];
			}

			g_free (tmp);

			for (i = 0; i < nargs; ++i)
			{
				ret[*l + i] = argvp[i];
			}

			g_free (argvp);
			*l += nargs;
		}
	}

	g_object_unref (s);
	g_object_unref (f);

	return ret;
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
	precision = g_strdup ("% 18.12f");

	ctx = g_option_context_new ("-m <SELECTOR> [-m ...] [-v RANGE...] [NETWORK] - monitor Codyn network");
	g_option_context_set_summary (ctx, "Omit the network name or use a dash '-' to read from standard input.");
	g_option_context_add_main_entries (ctx, entries, NULL);

	if (argc == 3 && (strcmp (argv[1], "-i") == 0 || strcmp (argv[1], "--interactive") == 0))
	{
		// Interactive mode. The second argument is the filename to monitor. Extract
		// arguments from its first comments
		gchar **args;
		gint nargs;

		file = argv[2];

		args = parse_interactive_flags (argv[0], file, &nargs, &error);

		if (error != NULL)
		{
			g_printerr ("Failed to parse interactive flags: %s\n", error->message);
			g_error_free (error);
			cleanup ();

			return 1;
		}

		if (!g_option_context_parse (ctx, &nargs, &args, &error))
		{
			g_printerr ("Failed to parse options: %s\n", error->message);
			g_error_free (error);
			cleanup ();

			return 1;
		}

		g_strfreev (args);
	}
	else
	{
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
	}

	ret = monitor_network (file);

	cleanup ();

	return ret;
}
