/*
 * cdn-input-file.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-input-file.h"
#include "ctype.h"
#include "cdn-compile-error.h"

#include <math.h>

#define CDN_INPUT_FILE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT_FILE, CdnInputFilePrivate))

struct _CdnInputFilePrivate
{
	GFile *file;

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

G_DEFINE_TYPE (CdnInputFile, cdn_input_file, CDN_TYPE_INPUT)

enum
{
	PROP_0,
	PROP_FILE,
	PROP_REPEAT,
	PROP_TIME_COLUMN
};

static void
clear (CdnInputFile *input, gboolean finalize)
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
cdn_input_file_finalize (GObject *object)
{
	CdnInputFile *input = CDN_INPUT_FILE (object);

	clear (input, TRUE);

	g_object_unref (input->priv->file);

	G_OBJECT_CLASS (cdn_input_file_parent_class)->finalize (object);
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

	if (!file->priv->repeat && cdn_integrator_get_time (integrator) > file->priv->values[file->priv->num - 1][tc])
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
			time = fmod(cdn_integrator_get_time (integrator), timespan);
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
read_file (CdnInputFile *input, GError **error)
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
set_file (CdnInputFile *input,
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

	cdn_object_taint (CDN_OBJECT (input));
	g_object_notify (G_OBJECT (input), "file");
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
cdn_input_file_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CdnInputFile *self = CDN_INPUT_FILE (object);

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
cdn_input_file_compile (CdnObject         *object,
                        CdnCompileContext *context,
                        CdnCompileError   *error)
{
	CdnInputFile *input;
	GError *err = NULL;

	input = CDN_INPUT_FILE (object);

	if (CDN_OBJECT_CLASS (cdn_input_file_parent_class)->compile)
	{
		if (!CDN_OBJECT_CLASS (cdn_input_file_parent_class)->compile (object,
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
		cdn_compile_error_set (error,
		                       err,
		                       object,
		                       NULL,
		                       NULL,
		                       NULL);

		g_error_free (err);

		return FALSE;
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
	CdnInputClass *input_class = CDN_INPUT_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	input_class->update = cdn_input_file_update;

	object_class->finalize = cdn_input_file_finalize;

	object_class->get_property = cdn_input_file_get_property;
	object_class->set_property = cdn_input_file_set_property;

	cdn_class->compile = cdn_input_file_compile;
	cdn_class->reset = cdn_input_file_reset;

	g_type_class_add_private (object_class, sizeof(CdnInputFilePrivate));

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
cdn_input_file_init (CdnInputFile *self)
{
	self->priv = CDN_INPUT_FILE_GET_PRIVATE (self);
}

CdnInputFile *
cdn_input_file_new (gchar const *id,
                    GFile       *file)
{
	return g_object_new (CDN_TYPE_INPUT_FILE,
	                     "id", id,
	                     "file", file,
	                     NULL);
}

CdnInputFile *
cdn_input_file_new_for_path (gchar const *id,
                             gchar const *path)
{
	GFile *file;
	CdnInputFile *ret;

	g_return_val_if_fail (id != NULL, NULL);

	if (path)
	{
		file = g_file_new_for_path (path);
		ret = cdn_input_file_new (id, file);
		g_object_unref (file);
	}
	else
	{
		ret = cdn_input_file_new (id, NULL);
	}

	return ret;
}

/**
 * cdn_input_file_get_file:
 * @input: A #CdnInputFile
 *
 * Get the file.
 *
 * Returns: (transfer full): A #GFile
 *
 **/
GFile *
cdn_input_file_get_file (CdnInputFile *input)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), NULL);

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
cdn_input_file_set_file (CdnInputFile *input,
                         GFile        *file)
{
	g_return_if_fail (CDN_IS_INPUT_FILE (input));
	g_return_if_fail (file == NULL || G_IS_FILE (file));

	set_file (input, file);
	g_object_notify (G_OBJECT (input), "file");
}

gchar *
cdn_input_file_get_file_path (CdnInputFile *input)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), NULL);

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
cdn_input_file_set_file_path (CdnInputFile *input,
                              gchar const  *path)
{
	GFile *file;

	g_return_if_fail (CDN_IS_INPUT_FILE (input));

	if (path)
	{
		file = g_file_new_for_path (path);
	}
	else
	{
		file = NULL;
	}

	cdn_input_file_set_file (input, file);
}

void
cdn_input_file_set_columns (CdnInputFile       *input,
                            gchar const * const *names)
{
	g_return_if_fail (CDN_IS_INPUT_FILE (input));

	set_column_names (input, names);
	input->priv->column_names_set = TRUE;

	cdn_object_taint (CDN_OBJECT (input));
}

/**
 * cdn_input_file_get_columns:
 * @input: A #CdnInputFilecolumn
 *
 * Get the columns.
 *
 * Returns: (transfer full): A list of column names
 *
 **/
gchar **
cdn_input_file_get_columns (CdnInputFile *input)
{
	GPtrArray *ret;
	guint i;

	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), NULL);

	ret = g_ptr_array_new ();

	for (i = 0; i < input->priv->num_columns; ++i)
	{
		g_ptr_array_add (ret, g_strdup (cdn_variable_get_name (input->priv->columns[i])));
	}

	g_ptr_array_add (ret, NULL);

	return (gchar **)g_ptr_array_free (ret, FALSE);
}

gboolean
cdn_input_file_get_repeat (CdnInputFile *input)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), FALSE);

	return input->priv->repeat;
}

void
cdn_input_file_set_repeat (CdnInputFile *input,
                           gboolean       repeat)
{
	g_return_if_fail (CDN_IS_INPUT_FILE (input));

	set_repeat (input, repeat);
}

gint
cdn_input_file_get_time_column (CdnInputFile *input,
                                gboolean     *isset)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), 0);

	if (isset)
	{
		*isset = input->priv->time_column_set;
	}

	return input->priv->time_column;
}

void
cdn_input_file_set_time_column (CdnInputFile *input,
                                gint          column)
{
	g_return_if_fail (CDN_IS_INPUT_FILE (input));

	set_time_column (input, column);
}

gdouble const * const *
cdn_input_file_get_data (CdnInputFile *input,
                         guint         *num_rows,
                         guint         *num_columns)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), NULL);

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

gboolean
cdn_input_file_ensure (CdnInputFile  *input,
                       GError       **error)
{
	g_return_val_if_fail (CDN_IS_INPUT_FILE (input), FALSE);

	if (input->priv->file && !input->priv->values)
	{
		return read_file (input, error);
	}
	else if (!input->priv->file && input->priv->values)
	{
		set_column_names (input, NULL);
	}

	return TRUE;
}
