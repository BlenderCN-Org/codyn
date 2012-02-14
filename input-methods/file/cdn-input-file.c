#include "cdn-input-file.h"
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-compile-error.h>
#include <codyn/cdn-network.h>

#include <math.h>

#define CDN_INPUT_FILE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT_FILE, CdnInputFilePrivate))

static void cdn_input_iface_init (gpointer iface);

struct _CdnInputFilePrivate
{
	GFile *file;
	gchar *path;

	gdouble **values;

	guint size;
	guint num;
	gint time_column;

	guint ptr;
	gdouble time_correction;

	CdnVariable **columns;
	guint num_columns;

	guint column_names_set : 1;
	guint time_column_set : 1;
	guint repeat : 1;
	guint warned : 1;
};

static gboolean read_file (CdnInputFile  *input,
                           GError       **error);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnInputFile,
                                cdn_input_file,
                                CDN_TYPE_FUNCTION,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (CDN_TYPE_INPUT, cdn_input_iface_init))

enum
{
	PROP_0,
	PROP_PATH,
	PROP_REPEAT,
	PROP_TIME_COLUMN
};

static void
clear (CdnInputFile *input,
       gboolean      finalize)
{
	gint i;

	input->priv->ptr = 0;
	input->priv->size = 0;
	input->priv->time_column = -1;
	input->priv->num = 0;
	input->priv->time_correction = 0;

	guint size = sizeof (gdouble) * input->priv->num_columns;

	for (i = 0; i < input->priv->num; ++i)
	{
		g_slice_free1 (size, input->priv->values[i]);
	}

	if (!input->priv->column_names_set || finalize)
	{
		g_free (input->priv->columns);

		input->priv->columns = NULL;
		input->priv->num_columns = 0;
	}

	g_free (input->priv->values);
	input->priv->values = NULL;

	input->priv->warned = FALSE;
}

static void
cdn_input_file_update (CdnInput      *input,
                       CdnIntegrator *integrator)
{
	CdnInputFile *file;
	gdouble time;
	gdouble timespan;
	gint i;
	gdouble factor;

	file = CDN_INPUT_FILE (input);

	if (!file->priv->values)
	{
		return;
	}

	gint tc = file->priv->time_column;
	gdouble *prev;
	gdouble *next;

	if (!file->priv->repeat &&
	    cdn_integrator_get_time (integrator) > file->priv->values[file->priv->num - 1][tc])
	{
		prev = file->priv->values[file->priv->num - 1];
		next = prev;

		time = prev[tc];
	}
	else
	{
		time = cdn_integrator_get_time (integrator) - file->priv->time_correction;

		timespan = file->priv->values[file->priv->num - 1][tc] -
		           file->priv->values[0][tc];

		if (time < 0)
		{
			/* Inefficient */
			time = fmod (cdn_integrator_get_time (integrator), timespan);
			file->priv->ptr = 0;
		}

		while (time > file->priv->values[file->priv->ptr][tc])
		{
			++file->priv->ptr;

			if (file->priv->ptr >= file->priv->num)
			{
				file->priv->time_correction += timespan;
				time -= timespan;

				file->priv->ptr = 0;
			}
		}

		next = file->priv->values[file->priv->ptr];

		if (file->priv->ptr != 0)
		{
			prev = file->priv->values[file->priv->ptr - 1];
		}
		else
		{
			prev = next;
		}
	}

	if (next[tc] == prev[tc])
	{
		factor = 0;
	}
	else
	{
		factor = (time - prev[tc]) / (next[tc] - prev[tc]);
	}

	for (i = 0; i < file->priv->num_columns; ++i)
	{
		gint column;

		if (i >= tc)
		{
			column = i + 1;
		}
		else
		{
			column = i;
		}

		gdouble value = prev[column] + (factor * (next[column] - prev[column]));

		cdn_variable_set_value (file->priv->columns[i],
		                        value);
	}
}

static gboolean
cdn_input_file_initialize_impl (CdnInput      *input,
                                GCancellable  *cancellable,
                                GError       **error)
{
	return read_file (CDN_INPUT_FILE (input), error);
}

static gboolean
cdn_input_file_finalize_impl (CdnInput      *input,
                              GCancellable  *cancellable,
                              GError       **error)
{
	// Nothing to finalize
	return TRUE;
}

static void
cdn_input_iface_init (gpointer iface)
{
	CdnInputInterface *i = iface;

	i->update = cdn_input_file_update;
	i->initialize = cdn_input_file_initialize_impl;
	i->finalize = cdn_input_file_finalize_impl;
}

static void
cdn_input_file_finalize (GObject *object)
{
	CdnInputFile *input = CDN_INPUT_FILE (object);

	clear (CDN_INPUT_FILE (object), TRUE);

	if (input->priv->file)
	{
		g_object_unref (input->priv->file);
	}

	g_free (input->priv->path);

	G_OBJECT_CLASS (cdn_input_file_parent_class)->finalize (object);
}

static gchar *
normalize_name (gchar const *name)
{
	gchar *ret;
	gchar *ptr;
	gchar const *prefix = "";

	if (g_ascii_isdigit (*name))
	{
		prefix = "_";
	}

	ret = g_strdup_printf ("%s%s", prefix, name);
	ptr = ret;

	while (*ptr)
	{
		if (!g_ascii_isalnum (*ptr))
		{
			*ptr = '_';
		}

		++ptr;
	}

	return ret;
}

static gboolean
name_is_time (gchar const *name)
{
	return g_strcmp0 (name, "time") == 0 || g_strcmp0 (name, "t") == 0;
}

static void
guess_time_column (CdnInputFile *input)
{
	gint i;

	input->priv->time_column = 0;

	for (i = 0; i < input->priv->num_columns; ++i)
	{
		if (name_is_time (cdn_variable_get_name (input->priv->columns[i])))
		{
			input->priv->time_column = i;
			break;
		}
	}
}

static void
set_time_column (CdnInputFile *input,
                 gint          time_column)
{
	if (time_column == -1 && !input->priv->time_column_set)
	{
		return;
	}

	if (time_column == input->priv->time_column && input->priv->time_column_set)
	{
		return;
	}

	if (time_column == -1)
	{
		guess_time_column (input);
		input->priv->time_column_set = FALSE;
	}
	else
	{
		input->priv->time_column = time_column;
		input->priv->time_column_set = TRUE;
	}

	input->priv->ptr = 0;
	g_object_notify (G_OBJECT (input), "time-column");
}

static void
set_time_from_columns (CdnInputFile        *input,
                       gchar const * const *columns)
{
	gint ptr = 0;
	gchar *name;
	gint tc;

	if (input->priv->time_column_set)
	{
		return;
	}

	tc = 0;

	while (columns && *columns)
	{
		if (!**columns)
		{
			++columns;
			continue;
		}

		name = normalize_name (*columns);

		if (name_is_time (name))
		{
			g_free (name);
			tc = ptr;

			break;
		}

		g_free (name);

		++ptr;
		++columns;
	}

	if (input->priv->time_column != tc)
	{
		input->priv->time_column = tc;
		g_object_notify (G_OBJECT (input), "time-column");
	}
}

static void
set_column_names (CdnInputFile        *input,
                  gchar const * const *columns)
{
	gint ptr = 0;
	GSList *oldc = NULL;
	GSList *newc = NULL;
	guint i;

	for (i = 0; i < input->priv->num_columns; ++i)
	{
		oldc = g_slist_prepend (newc, input->priv->columns[i]);
	}

	guint num_columns = 0;

	if (columns)
	{
		set_time_from_columns (input, columns);
	}

	gint tc = input->priv->time_column;

	while (columns && *columns)
	{
		gchar *name;
		CdnVariable *prop;

		if (!**columns)
		{
			++columns;
			continue;
		}

		name = normalize_name (*columns);

		/* Skip time column here */
		if (input->priv->time_column != ptr)
		{
			prop = cdn_object_get_variable (CDN_OBJECT (input),
			                                name);

			if (!prop)
			{
				prop = cdn_variable_new (name,
				                         cdn_expression_new0 (),
				                         CDN_VARIABLE_FLAG_NONE);

				cdn_object_add_variable (CDN_OBJECT (input),
				                         prop,
				                         NULL);
			}
			else
			{
				oldc = g_slist_remove (oldc, prop);
			}

			cdn_variable_set_flags (prop,
			                        cdn_variable_get_flags (prop) |
			                        CDN_VARIABLE_FLAG_IN);

			newc = g_slist_prepend (newc, prop);
			++num_columns;
		}

		g_free (name);

		++columns;
		++ptr;
	}

	/* Remove properties that are still in oldc */
	GSList *item;

	for (item = oldc; item; item = g_slist_next (item))
	{
		cdn_object_remove_variable (CDN_OBJECT (input),
		                            cdn_variable_get_name (item->data),
		                            NULL);
	}

	g_slist_free (oldc);

	/* Make columns from newc */
	newc = g_slist_reverse (newc);

	clear (input, FALSE);

	input->priv->num_columns = num_columns;
	input->priv->columns = g_new (CdnVariable *, num_columns);

	ptr = 0;

	for (item = newc; item; item = g_slist_next (item))
	{
		input->priv->columns[ptr++] = item->data;
	}

	g_slist_free (newc);

	input->priv->time_column = tc;
}

static void
parse_columns (CdnInputFile *input,
               gchar const  *line)
{
	gchar **parts;

	parts = g_strsplit_set (line, "\t ;,", -1);

	set_column_names (input, (gchar const * const *)parts);

	g_strfreev (parts);
}

static void
parse_numeric_columns (CdnInputFile  *input,
                       gchar        **parts)
{
	GPtrArray *columns;
	gchar **s;
	gint ptr = 0;

	columns = g_ptr_array_new ();

	if (!input->priv->time_column_set)
	{
		input->priv->time_column = 0;
	}

	while (parts && *parts)
	{
		if (ptr != input->priv->time_column)
		{
			gchar *name;
			gint column = ptr;

			if (ptr > input->priv->time_column)
			{
				column = ptr - 1;
			}

			name = g_strdup_printf ("y%d", column);
			g_ptr_array_add (columns, name);
		}
		else
		{
			g_ptr_array_add (columns, g_strdup ("t"));
		}

		++parts;
		++ptr;
	}

	g_ptr_array_add (columns, NULL);

	s = (gchar **)g_ptr_array_free (columns, FALSE);
	set_column_names (input, (gchar const * const *)s);

	g_strfreev (s);
}

static void
add_values (CdnInputFile  *input,
            gchar        **parts)
{
	// note the num_columns + 1 because num_columns is without the time column
	gdouble *row = (gdouble *)g_slice_alloc0 (sizeof (gdouble) * (input->priv->num_columns + 1));
	guint ptr = 0;

	while (parts && *parts)
	{
		if (!**parts)
		{
			++parts;
			continue;
		}

		if (ptr > input->priv->num_columns)
		{
			break;
		}

		row[ptr++] = g_strtod (*parts, NULL);
		++parts;
	}

	if (input->priv->size == input->priv->num)
	{
		input->priv->size += 100;
		array_resize (input->priv->values, gdouble *, input->priv->size);
	}

	input->priv->values[input->priv->num++] = row;

	if (input->priv->num > 1 &&
	    row[input->priv->time_column] <= input->priv->values[input->priv->num - 2][input->priv->time_column])
	{
		gchar *uri;

		if (!input->priv->warned)
		{
			uri = g_file_get_uri (input->priv->file);
			g_warning ("Time is not monotonic in: %s, column = %d", uri, input->priv->time_column);
			g_free (uri);

			input->priv->warned = TRUE;
		}
	}
}

static gboolean
read_header (CdnInputFile  *input,
             GError       **error)
{
	GDataInputStream *stream;
	GInputStream *base;

	clear (input, FALSE);

	base = G_INPUT_STREAM (g_file_read (input->priv->file,
	                                    NULL,
	                                    error));

	if (!base)
	{
		return FALSE;
	}

	stream = g_data_input_stream_new (base);
	g_object_unref (base);

	while (TRUE)
	{
		gchar *line;
		gsize length;

		line = g_data_input_stream_read_line (stream,
		                                      &length,
		                                      NULL,
		                                      error);

		if (line == NULL)
		{
			break;
		}

		g_strstrip (line);

		if (*line == '#')
		{
			if (input->priv->columns == NULL)
			{
				parse_columns (input, line + 1);
			}

			g_free (line);
			break;
		}
		else if (*line)
		{
			g_free (line);
			break;
		}

		g_free (line);
	}

	g_object_unref (stream);
	return *error == NULL;
}

static gboolean
read_file (CdnInputFile  *input,
           GError       **error)
{
	GDataInputStream *stream;
	GInputStream *base;

	clear (input, FALSE);

	base = G_INPUT_STREAM (g_file_read (input->priv->file,
	                                    NULL,
	                                    error));

	if (!base)
	{
		return FALSE;
	}

	stream = g_data_input_stream_new (base);
	g_object_unref (base);

	while (TRUE)
	{
		gchar *line;
		gsize length;

		line = g_data_input_stream_read_line (stream,
		                                      &length,
		                                      NULL,
		                                      error);

		if (line == NULL)
		{
			break;
		}

		g_strstrip (line);

		if (*line == '#')
		{
			if (input->priv->columns == NULL)
			{
				parse_columns (input, line + 1);
			}

			g_free (line);
			continue;
		}

		gchar **parts = g_strsplit_set (line, "\t ;,", -1);

		if (input->priv->columns == NULL)
		{
			parse_numeric_columns (input, parts);
		}

		add_values (input, parts);

		g_strfreev (parts);
	}

	g_object_unref (stream);
	return *error == NULL;
}

static CdnObject *
top_parent (CdnObject *o)
{
	CdnNode *p;

	p = cdn_object_get_parent (o);

	if (!p)
	{
		return o;
	}

	return top_parent (CDN_OBJECT (p));
}

static GFile *
resolve_path (CdnInputFile *input,
              gchar const  *path)
{
	CdnObject *p;
	GFile *pf;
	gchar *cwd;
	GFile *f;

	if (!path)
	{
		return NULL;
	}

	if (g_path_is_absolute (path))
	{
		return g_file_new_for_path (path);
	}

	p = top_parent (CDN_OBJECT (input));

	if (CDN_IS_NETWORK (p))
	{
		pf = cdn_network_get_file (CDN_NETWORK (p));

		if (pf)
		{
			GFile *f;

			f = g_file_resolve_relative_path (pf, path);
			g_object_unref (pf);

			return f;
		}
	}

	cwd = g_get_current_dir ();

	pf = g_file_new_for_path (cwd);
	g_free (cwd);

	f = g_file_resolve_relative_path (pf, path);

	g_object_unref (pf);

	return f;
}

static void
set_path (CdnInputFile *input,
          gchar const  *path)
{
	GFile *f = NULL;

	if (path != NULL)
	{
		f = resolve_path (input, path);
	}

	if (input->priv->file == NULL && f == NULL)
	{
		return;
	}

	if (input->priv->file && f && g_file_equal (input->priv->file, f))
	{
		g_object_unref (f);
		return;
	}

	if (input->priv->file)
	{
		g_object_unref (input->priv->file);
		input->priv->file = NULL;

		g_free (input->priv->path);
		input->priv->path = NULL;
	}

	if (f)
	{
		input->priv->file = g_file_dup (f);
		input->priv->path = g_strdup (path);
	}

	cdn_object_taint (CDN_OBJECT (input));
	g_object_notify (G_OBJECT (input), "path");
}

static void
set_repeat (CdnInputFile *input,
            gboolean      repeat)
{
	if (input->priv->repeat == repeat)
	{
		return;
	}

	input->priv->repeat = repeat;
	g_object_notify (G_OBJECT (input), "repeat");
}

static void
cdn_input_file_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CdnInputFile *self = CDN_INPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_PATH:
			set_path (self, g_value_get_string (value));
			break;
		case PROP_REPEAT:
			set_repeat (self, g_value_get_boolean (value));
			break;
		case PROP_TIME_COLUMN:
			set_time_column (self, g_value_get_int (value));
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_input_file_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CdnInputFile *self = CDN_INPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_PATH:
			g_value_set_string (value, self->priv->path);
			break;
		case PROP_REPEAT:
			g_value_set_boolean (value, self->priv->repeat);
			break;
		case PROP_TIME_COLUMN:
			g_value_set_int (value, self->priv->time_column);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cdn_input_file_compile (CdnObject         *object,
                        CdnCompileContext *context,
                        CdnCompileError   *error)
{
	CdnInputFile *input;
	GError *err = NULL;

	input = CDN_INPUT_FILE (object);

	if (!input->priv->column_names_set)
	{
		if (!read_header (input, &err))
		{
			cdn_compile_error_set (error,
			                       err,
			                       object,
			                       NULL,
			                       NULL,
			                       NULL);

			g_error_free (err);
			return FALSE;
		}
	}

	if (CDN_OBJECT_CLASS (cdn_input_file_parent_class)->compile)
	{
		if (!CDN_OBJECT_CLASS (cdn_input_file_parent_class)->compile (object,
		                                                              context,
		                                                              error))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
cdn_input_file_reset (CdnObject *object)
{
	CdnInputFile *input;

	if (CDN_OBJECT_CLASS (cdn_input_file_parent_class)->reset)
	{
		CDN_OBJECT_CLASS (cdn_input_file_parent_class)->reset (object);
	}

	input = CDN_INPUT_FILE (object);
	clear (input, FALSE);
}

static void
cdn_input_file_class_init (CdnInputFileClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdnobject_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_input_file_finalize;

	object_class->get_property = cdn_input_file_get_property;
	object_class->set_property = cdn_input_file_set_property;

	cdnobject_class->compile = cdn_input_file_compile;
	cdnobject_class->reset = cdn_input_file_reset;

	g_type_class_add_private (object_class, sizeof(CdnInputFilePrivate));

	g_object_class_install_property (object_class,
	                                 PROP_PATH,
	                                 g_param_spec_string ("path",
	                                                      "Path",
	                                                      "Path",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_REPEAT,
	                                 g_param_spec_boolean ("repeat",
	                                                       "Repeat",
	                                                       "Repeat",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT |
	                                                       G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_TIME_COLUMN,
	                                 g_param_spec_int ("time-column",
	                                                   "Time Column",
	                                                   "Time column",
	                                                   0,
	                                                   G_MAXINT,
	                                                   0,
	                                                   G_PARAM_READWRITE |
	                                                   G_PARAM_CONSTRUCT |
	                                                   G_PARAM_STATIC_STRINGS));
}

static void
cdn_input_file_class_finalize (CdnInputFileClass *klass)
{
}

static void
cdn_input_file_init (CdnInputFile *self)
{
	self->priv = CDN_INPUT_FILE_GET_PRIVATE (self);
}

void
cdn_input_method_register_types (GTypeModule *type_module)
{
	cdn_input_file_register_type (type_module);
}
