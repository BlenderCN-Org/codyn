#include "cpg-input-file.h"
#include "ctype.h"
#include "cpg-compile-error.h"

#include <math.h>

#define CPG_INPUT_FILE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INPUT_FILE, CpgInputFilePrivate))

struct _CpgInputFilePrivate
{
	GFile *file;

	gdouble **values;

	guint size;
	guint num;
	gint time_column;

	guint ptr;
	gdouble time_correction;

	CpgProperty **columns;
	guint num_columns;

	guint column_names_set : 1;
	guint time_column_set : 1;
	guint repeat : 1;
};

G_DEFINE_TYPE (CpgInputFile, cpg_input_file, CPG_TYPE_INPUT)

enum
{
	PROP_0,
	PROP_FILE,
	PROP_REPEAT,
	PROP_TIME_COLUMN
};

static void
clear (CpgInputFile *input, gboolean finalize)
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
}

static void
cpg_input_file_finalize (GObject *object)
{
	CpgInputFile *input = CPG_INPUT_FILE (object);

	clear (input, TRUE);

	g_object_unref (input->priv->file);

	G_OBJECT_CLASS (cpg_input_file_parent_class)->finalize (object);
}

static void
cpg_input_file_update (CpgInput      *input,
                       CpgIntegrator *integrator)
{
	CpgInputFile *file;
	gdouble time;
	gdouble timespan;
	gint i;
	gdouble factor;

	file = CPG_INPUT_FILE (input);

	if (!file->priv->values)
	{
		return;
	}

	gint tc = file->priv->time_column;
	gdouble *prev;
	gdouble *next;

	if (!file->priv->repeat && cpg_integrator_get_time (integrator) > file->priv->values[file->priv->num - 1][tc])
	{
		prev = file->priv->values[file->priv->num - 1];
		next = prev;

		time = prev[tc];
	}
	else
	{
		time = cpg_integrator_get_time (integrator) - file->priv->time_correction;

		timespan = file->priv->values[file->priv->num - 1][tc] -
		           file->priv->values[0][tc];

		if (time < 0)
		{
			/* Inefficient */
			time = fmod(cpg_integrator_get_time (integrator), timespan);
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

		cpg_property_set_value (file->priv->columns[i],
		                        value);
	}
}

static gchar *
normalize_name (gchar const *name)
{
	gchar *ret;
	gchar *ptr;
	gchar const *prefix = "";

	if (isdigit(*name))
	{
		prefix = "_";
	}

	ret = g_strdup_printf ("%s%s", prefix, name);
	ptr = ret;

	while (*ptr)
	{
		if (!isalnum (*ptr))
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
guess_time_column (CpgInputFile *input)
{
	gint i;

	input->priv->time_column = 0;

	for (i = 0; i < input->priv->num_columns; ++i)
	{
		if (name_is_time (cpg_property_get_name (input->priv->columns[i])))
		{
			input->priv->time_column = i;
			break;
		}
	}
}

static void
set_time_column (CpgInputFile *input,
                 gint          time_column)
{
	if (time_column == -1 && !input->priv->time_column_set)
	{
		return;
	}

	if (time_column == time_column && input->priv->time_column_set)
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
set_time_from_columns (CpgInputFile        *input,
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
set_column_names (CpgInputFile        *input,
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

	set_time_from_columns (input, columns);

	gint tc = input->priv->time_column;

	while (columns && *columns)
	{
		gchar *name;
		CpgProperty *prop;

		if (!**columns)
		{
			++columns;
			continue;
		}

		name = normalize_name (*columns);

		/* Skip time column here */
		if (input->priv->time_column != ptr)
		{
			prop = cpg_object_get_property (CPG_OBJECT (input),
			                                name);

			if (!prop)
			{
				prop = cpg_property_new (name,
				                         "0",
				                         CPG_PROPERTY_FLAG_NONE);

				cpg_object_add_property (CPG_OBJECT (input),
				                         prop);
			}
			else
			{
				oldc = g_slist_remove (oldc, prop);
			}

			cpg_property_set_flags (prop,
			                        cpg_property_get_flags (prop) |
			                        CPG_PROPERTY_FLAG_IN);

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
		cpg_object_remove_property (CPG_OBJECT (input),
		                            item->data,
		                            NULL);
	}

	g_slist_free (oldc);

	/* Make columns from newc */
	newc = g_slist_reverse (newc);

	clear (input, FALSE);

	input->priv->num_columns = num_columns;
	input->priv->columns = g_new (CpgProperty *, num_columns);

	ptr = 0;

	for (item = newc; item; item = g_slist_next (item))
	{
		input->priv->columns[ptr++] = item->data;
	}

	g_slist_free (newc);

	input->priv->time_column = tc;
}

static void
parse_columns (CpgInputFile *input,
               gchar const  *line)
{
	gchar **parts;

	parts = g_strsplit_set (line, "\t ;,", -1);

	set_column_names (input, (gchar const * const *)parts);

	g_strfreev (parts);
}

static void
parse_numeric_columns (CpgInputFile  *input,
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

		++parts;
		++ptr;
	}

	g_ptr_array_add (columns, NULL);

	s = (gchar **)g_ptr_array_free (columns, FALSE);
	set_column_names (input, (gchar const * const *)s);

	g_strfreev (s);
}

static void
add_values (CpgInputFile  *input,
            gchar        **parts)
{
	gdouble *row = (gdouble *)g_slice_alloc0 (sizeof (gdouble) * input->priv->num_columns);
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

		uri = g_file_get_uri (input->priv->file);
		g_warning ("Time is not monotonic in: %s, %d", uri, input->priv->time_column);
		g_free (uri);
	}
}

static gboolean
read_file (CpgInputFile *input, GError **error)
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

static void
set_file (CpgInputFile *input,
          GFile        *file)
{
	if (input->priv->file == file)
	{
		return;
	}

	if (input->priv->file && file && g_file_equal (input->priv->file, file))
	{
		return;
	}

	if (input->priv->file)
	{
		g_object_unref (input->priv->file);
		input->priv->file = NULL;
	}

	if (file)
	{
		input->priv->file = g_file_dup (file);
	}

	cpg_object_taint (CPG_OBJECT (input));
	g_object_notify (G_OBJECT (input), "file");
}

static void
set_repeat (CpgInputFile *input,
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
cpg_input_file_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CpgInputFile *self = CPG_INPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_FILE:
			set_file (self, g_value_get_object (value));
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
cpg_input_file_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CpgInputFile *self = CPG_INPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_FILE:
			g_value_set_object (value, self->priv->file);
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
cpg_input_file_compile (CpgObject         *object,
                        CpgCompileContext *context,
                        CpgCompileError   *error)
{
	CpgInputFile *input;
	GError *err = NULL;

	input = CPG_INPUT_FILE (object);

	if (CPG_OBJECT_CLASS (cpg_input_file_parent_class)->compile)
	{
		if (!CPG_OBJECT_CLASS (cpg_input_file_parent_class)->compile (object,
		                                                              context,
		                                                              error))
		{
			return FALSE;
		}
	}

	if (input->priv->values)
	{
		return TRUE;
	}

	if (!read_file (input, &err))
	{
		cpg_compile_error_set (error,
		                       err,
		                       object,
		                       NULL,
		                       NULL);

		g_error_free (err);

		return FALSE;
	}

	if (CPG_OBJECT_CLASS (cpg_input_file_parent_class)->compile)
	{
		if (!CPG_OBJECT_CLASS (cpg_input_file_parent_class)->compile (object,
		                                                              context,
		                                                              error))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
cpg_input_file_reset (CpgObject *object)
{
	CpgInputFile *input;

	if (CPG_OBJECT_CLASS (cpg_input_file_parent_class)->reset)
	{
		CPG_OBJECT_CLASS (cpg_input_file_parent_class)->reset (object);
	}

	input = CPG_INPUT_FILE (object);
	clear (input, FALSE);
}

static void
cpg_input_file_class_init (CpgInputFileClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgInputClass *input_class = CPG_INPUT_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	input_class->update = cpg_input_file_update;

	object_class->finalize = cpg_input_file_finalize;

	object_class->get_property = cpg_input_file_get_property;
	object_class->set_property = cpg_input_file_set_property;

	cpg_class->compile = cpg_input_file_compile;
	cpg_class->reset = cpg_input_file_reset;

	g_type_class_add_private (object_class, sizeof(CpgInputFilePrivate));

	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_REPEAT,
	                                 g_param_spec_boolean ("repeat",
	                                                       "Repeat",
	                                                       "Repeat",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
	                                 PROP_TIME_COLUMN,
	                                 g_param_spec_int ("time-column",
	                                                   "Time column",
	                                                   "Time Column",
	                                                   -1,
	                                                   G_MAXINT,
	                                                   0,
	                                                   G_PARAM_READWRITE));
}

static void
cpg_input_file_init (CpgInputFile *self)
{
	self->priv = CPG_INPUT_FILE_GET_PRIVATE (self);
}

CpgInputFile *
cpg_input_file_new (gchar const *id,
                    GFile       *file)
{
	return g_object_new (CPG_TYPE_INPUT_FILE,
	                     "id", id,
	                     "file", file,
	                     NULL);
}

CpgInputFile *
cpg_input_file_new_for_path (gchar const *id,
                             gchar const *path)
{
	GFile *file;
	CpgInputFile *ret;

	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (path != NULL, NULL);

	file = g_file_new_for_path (path);
	ret = cpg_input_file_new (id, file);
	g_object_unref (file);

	return ret;
}

/**
 * cpg_input_file_get_file:
 * @input: A #CpgInputFile
 *
 * Get the file.
 *
 * Returns: (transfer full): A #GFile
 *
 **/
GFile *
cpg_input_file_get_file (CpgInputFile *input)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), NULL);

	if (input->priv->file)
	{
		return g_file_dup (input->priv->file);
	}
	else
	{
		return NULL;
	}
}

void
cpg_input_file_set_file (CpgInputFile *input,
                         GFile        *file)
{
	g_return_if_fail (CPG_IS_INPUT_FILE (input));
	g_return_if_fail (file == NULL || G_IS_FILE (file));

	set_file (input, file);
	g_object_notify (G_OBJECT (input), "file");
}

gchar *
cpg_input_file_get_file_path (CpgInputFile *input)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), NULL);

	if (input->priv->file)
	{
		return g_file_get_path (input->priv->file);
	}
	else
	{
		return NULL;
	}
}

void
cpg_input_file_set_file_path (CpgInputFile *input,
                              gchar const  *path)
{
	GFile *file;

	g_return_if_fail (CPG_IS_INPUT_FILE (input));

	if (path)
	{
		file = g_file_new_for_path (path);
	}
	else
	{
		file = NULL;
	}

	cpg_input_file_set_file (input, file);
}

void
cpg_input_file_set_column_names (CpgInputFile       *input,
                                 gchar const * const *names)
{
	g_return_if_fail (CPG_IS_INPUT_FILE (input));

	set_column_names (input, names);
	input->priv->column_names_set = TRUE;

	cpg_object_taint (CPG_OBJECT (input));
}

/**
 * cpg_input_file_get_columns:
 * @input: A #CpgInputFilecolumn
 * @num_columns: (out callee-allocates): Return location for the number of columns
 *
 * Get the columns.
 *
 * Returns: (transfer none) (array length=num_columns): A list of #CpgProperty
 *
 **/
CpgProperty **
cpg_input_file_get_columns (CpgInputFile *input,
                            guint        *num_columns)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), NULL);

	if (num_columns)
	{
		*num_columns = input->priv->num_columns;
	}

	return input->priv->columns;
}

gboolean
cpg_input_file_get_repeat (CpgInputFile *input)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), FALSE);

	return input->priv->repeat;
}

void
cpg_input_file_set_repeat (CpgInputFile *input,
                           gboolean       repeat)
{
	g_return_if_fail (CPG_IS_INPUT_FILE (input));

	set_repeat (input, repeat);
}

gint
cpg_input_file_get_time_column (CpgInputFile *input)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), 0);

	return input->priv->time_column;
}

void
cpg_input_file_set_time_column (CpgInputFile *input,
                                gint          column)
{
	g_return_if_fail (CPG_IS_INPUT_FILE (input));

	set_time_column (input, column);
}

gdouble const * const *
cpg_input_file_get_data (CpgInputFile *input,
                         guint         *num_rows,
                         guint         *num_columns)
{
	g_return_val_if_fail (CPG_IS_INPUT_FILE (input), NULL);

	if (num_rows)
	{
		*num_rows = input->priv->num;
	}

	if (num_columns)
	{
		*num_columns = input->priv->num_columns;
	}

	return (gdouble const * const *)input->priv->values;
}
