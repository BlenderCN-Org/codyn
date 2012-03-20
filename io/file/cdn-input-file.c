#include "cdn-input-file.h"
#include <codyn/cdn-compile-context.h>
#include <codyn/cdn-compile-error.h>
#include <codyn/cdn-network.h>

#include <math.h>
#include <string.h>

#define CDN_INPUT_FILE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT_FILE, CdnInputFilePrivate))

static void cdn_io_iface_init (gpointer iface);

struct _CdnInputFilePrivate
{
	GFile *file;
	gchar *path;

	gdouble **values;
	gdouble *current_row;
	gdouble current_row_at;

	guint size;
	guint num;
	gint time_column;

	gdouble time_start;
	gdouble estimated_dt;

	guint ptr;
	gdouble time_correction;
	gdouble dt;

	GPtrArray *created_columns;
	GPtrArray *columns;
	guint num_columns;

	CdnIoMode mode;

	guint temporal : 1;
	guint time_column_set : 1;
	guint repeat : 1;
	guint dt_set : 1;
	guint interpolate : 1;
	guint has_header : 1;
};

static gboolean read_file (CdnInputFile  *input,
                           GError       **error);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnInputFile,
                                cdn_input_file,
                                CDN_TYPE_FUNCTION,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (CDN_TYPE_IO, cdn_io_iface_init))

enum
{
	PROP_0,
	PROP_PATH,
	PROP_TEMPORAL,
	PROP_REPEAT,
	PROP_TIME_COLUMN,
	PROP_DT,
	PROP_INTERPOLATE,
	PROP_MODE
};

static void
clear (CdnInputFile *input,
       gboolean      finalize)
{
	gint i;

	input->priv->ptr = 0;
	input->priv->size = 0;
	input->priv->num = 0;
	input->priv->time_correction = 0;

	guint size = sizeof (gdouble) * input->priv->num_columns;

	for (i = 0; i < input->priv->num; ++i)
	{
		g_slice_free1 (size, input->priv->values[i]);
	}

	g_free (input->priv->values);
	input->priv->values = NULL;
}

static gdouble
time_at (CdnInputFile *input,
         guint         idx)
{
	if (input->priv->dt_set)
	{
		return idx * input->priv->dt;
	}
	else
	{
		return input->priv->values[idx][input->priv->time_column];
	}
}

static void
evaluate_at (CdnInputFile *file,
             gdouble       t,
             gdouble      *ret)
{
	gdouble time;
	gdouble timespan;
	gint i;
	gdouble factor;
	gdouble prevt;
	gdouble nextt;
	gdouble *prev;
	gdouble *next;

	if (!file->priv->values || !file->priv->temporal)
	{
		return;
	}

	prevt = time_at (file, file->priv->num - 1);

	if (!file->priv->repeat && t > prevt)
	{
		prev = file->priv->values[file->priv->num - 1];

		next = prev;
		nextt = prevt;
		time = prevt;
	}
	else
	{
		timespan = prevt - time_at (file, 0);

		if (!ret)
		{
			time = t - file->priv->time_correction;

			if (time < 0)
			{
				/* Inefficient */
				time = fmod (t, timespan);
				file->priv->ptr = 0;
			}

			// Sequential access
			while (time > time_at (file, file->priv->ptr))
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
			nextt = time_at (file, file->priv->ptr);

			if (file->priv->ptr != 0)
			{
				prev = file->priv->values[file->priv->ptr - 1];
				prevt = time_at (file, file->priv->ptr - 1);
			}
			else
			{
				prev = next;
				prevt = nextt;
			}
		}
		else
		{
			gint pidx;
			gint nidx;

			if (file->priv->repeat)
			{
				time = fmod (t, timespan);
			}
			else
			{
				time = t;
			}

			// Random access
			if (file->priv->estimated_dt > 0)
			{
				gdouble pp = (time - file->priv->time_start) / file->priv->estimated_dt;

				pidx = (gint)floor (pp);
				nidx = (gint)ceil (pp);
			}
			else
			{
				pidx = file->priv->ptr;

				while (pidx < file->priv->num - 1 && time_at (file, pidx + 1) < time)
				{
					++pidx;
				}

				while (pidx > 0 && time_at (file, pidx) > time)
				{
					--pidx;
				}

				if (pidx == file->priv->num - 1)
				{
					nidx = 0;
				}
				else
				{
					nidx = pidx + 1;
				}
			}

			prev = file->priv->values[pidx];
			next = file->priv->values[nidx];

			prevt = time_at (file, pidx);
			nextt = time_at (file, nidx);
		}
	}

	if (fabs (nextt - prevt) <= DBL_EPSILON)
	{
		factor = 0;
	}
	else
	{
		factor = (time - prevt) / (nextt - prevt);
	}

	for (i = 0; i < file->priv->num_columns; ++i)
	{
		gdouble value;

		if (file->priv->interpolate)
		{
			value = prev[i] + (factor * (next[i] - prev[i]));
		}
		else if (factor < 0.5)
		{
			value = prev[i];
		}
		else
		{
			value = next[i];
		}

		if (ret)
		{
			ret[i] = value;
		}
		else
		{
			file->priv->current_row[i] = value;

			cdn_variable_set_value (g_ptr_array_index (file->priv->columns, i),
			                        value);
		}
	}

	if (!ret)
	{
		file->priv->current_row_at = t;
	}
}

static void
cdn_input_file_update (CdnIo        *input,
                       CdnIntegrator *integrator)
{
	CdnInputFile *file;

	file = CDN_INPUT_FILE (input);

	evaluate_at (file, cdn_integrator_get_time (integrator), NULL);
}

static gboolean
cdn_input_file_initialize_impl (CdnIo        *input,
                                GCancellable  *cancellable,
                                GError       **error)
{
	CdnInputFile *f;

	f = CDN_INPUT_FILE (input);

	if (f->priv->file == NULL)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (f));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "No path was set for input file `%s'",
		             id);

		g_free (id);
		return FALSE;
	}

	return read_file (f, error);
}

static gboolean
cdn_input_file_finalize_impl (CdnIo        *input,
                              GCancellable  *cancellable,
                              GError       **error)
{
	// Nothing to finalize
	return TRUE;
}

static void
cdn_io_iface_init (gpointer iface)
{
	CdnIoInterface *i = iface;

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

static gboolean
name_is_time (gchar const *name)
{
	return g_strcmp0 (name, "time") == 0 || g_strcmp0 (name, "t") == 0;
}

static gboolean
is_separator (gunichar c)
{
	return g_unichar_isspace (c) || c == ';' || c == ',';
}

static gboolean
next_value (CdnInputFile  *input,
            gchar const  **line,
            gdouble       *v,
            GError       **error)
{
	gunichar c;
	gchar *ret;
	gchar *valid;
	gchar const *ptr;
	gdouble val;

	// Skip separators
	while ((c = g_utf8_get_char (*line)))
	{
		if (!is_separator (c))
		{
			break;
		}

		*line = g_utf8_next_char (*line);
	}

	if (!c)
	{
		return FALSE;
	}

	ptr = *line;

	while ((c = g_utf8_get_char (ptr)))
	{
		if (is_separator (c))
		{
			break;
		}

		ptr = g_utf8_next_char (ptr);
	}

	ret = g_strndup (*line, ptr - *line);
	val = g_ascii_strtod (ret, &valid);

	if (v)
	{
		*v = val;
	}

	if (valid - ret != ptr - *line)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (input));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "Invalid number for input file `%s': `%s'",
		             id,
		             ret);

		g_free (id);
		valid = NULL;
	}

	g_free (ret);
	*line = ptr;

	return valid != NULL;
}

static gboolean
add_values (CdnInputFile  *input,
            gchar const   *line,
            GError       **error)
{
	gdouble *row;
	guint ptr = 0;
	gboolean estimatedt;
	gdouble v;
	GError *err = NULL;

	row = (gdouble *)g_slice_alloc0 (sizeof (gdouble) *
	                                 input->priv->num_columns);

	estimatedt = input->priv->temporal && !input->priv->dt_set;

	while (next_value (input, &line, &v, &err))
	{
		if (ptr > input->priv->num_columns)
		{
			break;
		}

		if (estimatedt && ptr == input->priv->time_column)
		{
			if (input->priv->num == 0)
			{
				input->priv->time_start = v;
			}
			else
			{
				gdouble dt = v - input->priv->values[input->priv->num - 1][ptr];

				if (input->priv->num == 1)
				{
					input->priv->estimated_dt = dt;
				}
				else if (fabs (input->priv->estimated_dt - dt) > DBL_EPSILON)
				{
					input->priv->estimated_dt = -1;
					estimatedt = FALSE;
				}
			}
		}

		row[ptr++] = v;
	}

	if (err)
	{
		g_propagate_error (error, err);
		return FALSE;
	}

	if (input->priv->size == input->priv->num)
	{
		input->priv->size += 100;
		array_resize (input->priv->values, gdouble *, input->priv->size);
	}

	input->priv->values[input->priv->num++] = row;
	return TRUE;
}

static CdnVariable *
create_column (CdnInputFile  *input,
               gchar const   *colname,
               GError       **error)
{
	CdnVariable *v;
	gchar *name;

	if (g_strcmp0 (colname, "t") == 0)
	{
		name = g_strdup ("time");
	}
	else
	{
		name = g_strdup (colname);
	}

	v = cdn_variable_new (name,
	                      cdn_expression_new0 (),
	                      CDN_VARIABLE_FLAG_IN);

	g_free (name);

	if (cdn_object_add_variable (CDN_OBJECT (input),
	                             v,
	                             error))
	{
		g_ptr_array_add (input->priv->created_columns,
		                 v);

		if (input->priv->temporal &&
		    !input->priv->time_column_set &&
		    name_is_time (colname))
		{
			input->priv->time_column =
				input->priv->created_columns->len - 1;
		}

		return v;
	}
	else
	{
		return NULL;
	}
}

static gboolean
create_columns (CdnInputFile  *input,
                gchar const   *line,
                GError       **error)
{
	GString *ret;
	gunichar c;

	ret = g_string_sized_new (strlen (line));

	while ((c = g_utf8_get_char (line)))
	{
		if (is_separator (c))
		{
			if (ret->len != 0)
			{
				CdnVariable *v;

				v = create_column (input, ret->str, error);

				if (!v)
				{
					g_string_free (ret, TRUE);
					return FALSE;
				}

				g_string_erase (ret, 0, -1);
			}
		}
		else if (!g_unichar_isalnum (c))
		{
			g_string_append_c (ret, '_');
		}
		else
		{
			g_string_append_unichar (ret, c);
		}

		line = g_utf8_next_char (line);
	}

	if (ret->len != 0)
	{
		if (!create_column (input, ret->str, error))
		{
			g_string_free (ret, TRUE);
			return FALSE;
		}
	}

	g_string_free (ret, TRUE);
	return TRUE;
}

static void
clear_columns (CdnInputFile *input)
{
	gint i;

	if (input->priv->created_columns)
	{
		for (i = 0; i < input->priv->created_columns->len; ++i)
		{
			cdn_object_remove_variable (CDN_OBJECT (input),
			                            g_ptr_array_index (input->priv->created_columns, i),
			                            NULL);
		}

		g_ptr_array_free (input->priv->created_columns, TRUE);
	}

	input->priv->created_columns = g_ptr_array_sized_new (10);

	if (!input->priv->time_column_set)
	{
		input->priv->time_column = 0;
	}

	input->priv->num_columns = 0;

	g_free (input->priv->current_row);
	input->priv->current_row = NULL;
}

static guint
extract_num_columns (gchar const *line)
{
	guint ret = 0;
	gboolean nospace = FALSE;
	gunichar c;

	while ((c = g_utf8_get_char (line)))
	{
		if (is_separator (c))
		{
			if (nospace)
			{
				++ret;
			}

			nospace = FALSE;
		}
		else
		{
			nospace = TRUE;
		}

		line = g_utf8_next_char (line);
	}

	if (nospace && *line)
	{
		++ret;
	}

	return ret;
}

static gboolean
extract_columns (CdnInputFile  *input,
                 GError       **error)
{
	GDataInputStream *stream;
	GInputStream *base;
	gboolean ret = TRUE;

	clear_columns (input);

	if (!input->priv->file)
	{
		return TRUE;
	}

	base = G_INPUT_STREAM (g_file_read (input->priv->file,
	                                    NULL,
	                                    error));

	if (!base)
	{
		return FALSE;
	}

	stream = g_data_input_stream_new (base);
	g_object_unref (base);

	input->priv->has_header = FALSE;

	while (TRUE)
	{
		gchar *line;
		gsize length;
		gunichar c;

		line = g_data_input_stream_read_line (stream,
		                                      &length,
		                                      NULL,
		                                      error);

		if (line == NULL)
		{
			ret = FALSE;
			break;
		}

		g_strstrip (line);

		if (!*line)
		{
			g_free (line);
			continue;
		}

		c = g_utf8_get_char (line);

		if (input->priv->temporal &&
		    !(g_unichar_isdigit (c) || c == '.'))
		{
			ret = create_columns (input, line, error);
			input->priv->num_columns = input->priv->created_columns->len;
			input->priv->has_header = TRUE;
		}
		else
		{
			input->priv->num_columns = extract_num_columns (line);
		}

		g_free (line);
		break;
	}

	input->priv->current_row = g_new0 (gdouble, input->priv->num_columns);

	g_object_unref (stream);
	return ret;
}

static gboolean
prepare_temporal_columns (CdnInputFile  *input,
                          GError       **error)
{
	GSList *vars;

	if (input->priv->columns)
	{
		g_ptr_array_free (input->priv->columns, TRUE);
	}

	input->priv->columns = g_ptr_array_sized_new (10);

	// We only care about columns in temporal mode
	if (!input->priv->temporal)
	{
		return TRUE;
	}

	// Each _in_ variable is a column
	vars = cdn_object_get_variables (CDN_OBJECT (input));

	while (vars)
	{
		CdnVariable *v;

		v = vars->data;

		if ((cdn_variable_get_flags (v) & CDN_VARIABLE_FLAG_IN) &&
		    g_strcmp0 (cdn_variable_get_name (v), "t") != 0)
		{
			g_ptr_array_add (input->priv->columns,
			                 v);
		}

		vars = g_slist_delete_link (vars, vars);
	}

	if (input->priv->columns->len != input->priv->num_columns)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "The number of expected columns (%u) does not match the number of variables (%d)",
		             input->priv->num_columns,
		             input->priv->columns->len);

		return FALSE;
	}

	return TRUE;
}

static gboolean
read_file (CdnInputFile  *input,
           GError       **error)
{
	GDataInputStream *stream;
	GInputStream *base;
	gboolean ret = TRUE;
	GError *err = NULL;
	gboolean first;

	clear (input, FALSE);

	if (!prepare_temporal_columns (input, error))
	{
		return FALSE;
	}

	base = G_INPUT_STREAM (g_file_read (input->priv->file,
	                                    NULL,
	                                    error));

	if (!base)
	{
		return FALSE;
	}

	stream = g_data_input_stream_new (base);
	g_object_unref (base);

	first = TRUE;

	while (TRUE)
	{
		gchar *line;
		gsize length;

		line = g_data_input_stream_read_line (stream,
		                                      &length,
		                                      NULL,
		                                      &err);

		if (line == NULL)
		{
			ret = err == NULL;

			if (err)
			{
				g_propagate_error (error, err);
			}

			break;
		}

		g_strstrip (line);

		if (*line && (!first || !input->priv->has_header))
		{
			if (!add_values (input, line, error))
			{
				g_free (line);
				ret = FALSE;
				break;
			}
		}

		first = FALSE;
		g_free (line);
	}

	g_object_unref (stream);
	return ret;
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
			GFile *par;

			par = g_file_get_parent (pf);
			f = g_file_resolve_relative_path (par, path);

			g_object_unref (par);
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

	extract_columns (input, NULL);

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
set_temporal (CdnInputFile *input,
              gboolean      temporal)
{
	if (input->priv->temporal == temporal)
	{
		return;
	}

	input->priv->temporal = temporal;
	extract_columns (input, NULL);

	g_object_notify (G_OBJECT (input), "temporal");
}

static void
set_current_time_column (CdnInputFile *input)
{
	gint i;

	if (!input->priv->temporal)
	{
		input->priv->time_column = -1;
		return;
	}

	input->priv->time_column = 0;

	for (i = 0; i < input->priv->created_columns->len; ++i)
	{
		CdnVariable *v;

		v = g_ptr_array_index (input->priv->created_columns, i);

		if (name_is_time (cdn_variable_get_name (v)))
		{
			input->priv->time_column = i;
			break;
		}
	}
}

static void
set_time_column (CdnInputFile *input,
                 gint          col)
{
	if (col < 0 && !input->priv->time_column_set)
	{
		return;
	}

	if (col >= 0 && input->priv->time_column_set && input->priv->time_column == col)
	{
		return;
	}

	if (col < 0)
	{
		input->priv->time_column_set = FALSE;
		set_current_time_column (input);
	}
	else
	{
		input->priv->time_column_set = TRUE;
		input->priv->time_column = col;
	}

	g_object_notify (G_OBJECT (input), "time-column");
}

static void
set_dt (CdnInputFile *input,
        gdouble       dt)
{
	if (dt < 0 && !input->priv->dt_set)
	{
		return;
	}

	if (dt >= 0 && input->priv->dt_set && input->priv->dt == dt)
	{
		return;
	}

	if (dt < 0)
	{
		input->priv->dt_set = FALSE;
		input->priv->dt = -1;
	}
	else
	{
		input->priv->dt_set = TRUE;
		input->priv->dt = dt;
	}

	g_object_notify (G_OBJECT (input), "dt");
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
		case PROP_TEMPORAL:
			set_temporal (self, g_value_get_boolean (value));
			break;
		case PROP_DT:
			set_dt (self, g_value_get_double (value));
			break;
		case PROP_INTERPOLATE:
			self->priv->interpolate = g_value_get_boolean (value);
			break;
		case PROP_MODE:
			self->priv->mode = g_value_get_flags (value);
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
		case PROP_TEMPORAL:
			g_value_set_boolean (value, self->priv->temporal);
			break;
		case PROP_DT:
			g_value_set_double (value, self->priv->dt);
			break;
		case PROP_INTERPOLATE:
			g_value_set_boolean (value, self->priv->interpolate);
			break;
		case PROP_MODE:
			g_value_set_flags (value, self->priv->mode);
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

	if (!input->priv->temporal)
	{
		// Need to read the file here if it's not temporal because
		// we need to determine the dimensionality
		if (!read_file (input, &err))
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
cdn_input_file_evaluate (CdnFunction *function,
                         CdnStack    *stack)
{
	CdnInputFile *f;
	gdouble t;

	f = CDN_INPUT_FILE (function);
	t = cdn_stack_pop (stack);

	if (!f->priv->temporal)
	{
		gint i;

		// Push everything on the stack
		for (i = 0; i < f->priv->num; ++i)
		{
			cdn_stack_pushn (stack,
			                 f->priv->values[i],
			                 f->priv->num_columns);
		}
	}
	else if (t == f->priv->current_row_at)
	{
		// Push current row on the stack
		cdn_stack_pushn (stack,
		                 f->priv->current_row,
		                 f->priv->num_columns);
	}
	else
	{
		// Random access
		gdouble *ptr;

		ptr = cdn_stack_output_ptr (stack);

		// Evaluate again
		evaluate_at (f, t, ptr);

		cdn_stack_set_output_ptr (stack,
		                          ptr + f->priv->num_columns);
	}
}

static void
cdn_input_file_get_dimension (CdnFunction *function,
                              gint        *numr,
                              gint        *numc)
{
	CdnInputFile *input;

	input = CDN_INPUT_FILE (function);

	*numc = input->priv->num_columns;

	if (input->priv->temporal)
	{
		*numr = 1;
	}
	else
	{
		*numr = input->priv->num;
	}
}

static void
cdn_input_file_class_init (CdnInputFileClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdnobject_class = CDN_OBJECT_CLASS (klass);
	CdnFunctionClass *cdnfunction_class = CDN_FUNCTION_CLASS (klass);

	object_class->finalize = cdn_input_file_finalize;

	object_class->get_property = cdn_input_file_get_property;
	object_class->set_property = cdn_input_file_set_property;

	cdnobject_class->compile = cdn_input_file_compile;
	cdnobject_class->reset = cdn_input_file_reset;

	cdnfunction_class->get_dimension = cdn_input_file_get_dimension;
	cdnfunction_class->evaluate = cdn_input_file_evaluate;

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
	                                                   G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_TEMPORAL,
	                                 g_param_spec_boolean ("temporal",
	                                                       "Temporal",
	                                                       "Temporal",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT |
	                                                       G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_DT,
	                                 g_param_spec_double ("dt",
	                                                      "Dt",
	                                                      "Dt",
	                                                      -2,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_INTERPOLATE,
	                                 g_param_spec_boolean ("interpolate",
	                                                       "Interpolate",
	                                                       "Interpolate",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT |
	                                                       G_PARAM_STATIC_STRINGS));

	g_object_class_override_property (object_class,
	                                  PROP_MODE,
	                                  "mode");
}

static void
cdn_input_file_class_finalize (CdnInputFileClass *klass)
{
}

static void
cdn_input_file_init (CdnInputFile *self)
{
	self->priv = CDN_INPUT_FILE_GET_PRIVATE (self);

	cdn_function_add_argument (CDN_FUNCTION (self),
	                           cdn_function_argument_new ("t", FALSE, NULL));
}

void
cdn_input_file_register (GTypeModule *type_module)
{
	cdn_input_file_register_type (type_module);
}
