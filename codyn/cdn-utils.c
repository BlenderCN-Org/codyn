/*
 * cdn-utils.c
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

#include "cdn-utils.h"
#include "cdn-network.h"
#include <string.h>

gboolean
cdn_signal_accumulator_false_handled (GSignalInvocationHint *ihint,
                                      GValue                *return_accu,
                                      const GValue          *handler_return,
                                      gpointer               dummy)
{
	gboolean continue_emission;
	gboolean signal_handled;

	signal_handled = g_value_get_boolean (handler_return);

	g_value_set_boolean (return_accu, !signal_handled);
	continue_emission = signal_handled;

	return continue_emission;
}

static gboolean
type_transform (GValue  *dest,
                GType    gtype,
                GError **error)
{
	GValue tmp = {0,};

	if (G_VALUE_TYPE (dest) == gtype)
	{
		return TRUE;
	}

	if (!g_value_type_transformable (G_VALUE_TYPE (dest), gtype))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert %s to %s",
		             g_type_name (G_VALUE_TYPE (dest)),
		             g_type_name (gtype));

		g_value_unset (dest);
		return FALSE;
	}

	g_value_init (&tmp, gtype);

	if (!g_value_transform (dest, &tmp))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert %s to %s",
		             g_type_name (G_VALUE_TYPE (dest)),
		             g_type_name (gtype));

		g_value_unset (dest);
		g_value_unset (&tmp);
		return FALSE;
	}

	g_value_unset (dest);
	g_value_init (dest, gtype);

	g_value_copy (&tmp, dest);
	g_value_unset (&tmp);

	return TRUE;
}

static gboolean
string_to_double (gchar const  *s,
                  GType         gtype,
                  GValue       *value,
                  GError      **error)
{
	gdouble ret;
	gchar *r;

	ret = g_ascii_strtod (s, &r);

	if (!r || *r)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to double",
		             s);

		return FALSE;
	}

	g_value_init (value, G_TYPE_DOUBLE);
	g_value_set_double (value, ret);

	return type_transform (value, gtype, error);
}

static gboolean
string_to_int (gchar const  *s,
               GType         gtype,
               GValue       *value,
               GError      **error)
{
	gint64 ret;
	gchar *r;

	ret = g_ascii_strtoll (s, &r, 10);

	if (!r || *r)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to int",
		             s);

		return FALSE;
	}

	g_value_init (value, G_TYPE_INT64);
	g_value_set_int64 (value, ret);

	return type_transform (value, gtype, error);
}

static gboolean
string_to_uint (gchar const  *s,
                GType         gtype,
                GValue       *value,
                GError      **error)
{
	guint64 ret;
	gchar *r;

	ret = g_ascii_strtoull (s, &r, 10);

	if (!r || *r)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to uint",
		             s);

		return FALSE;
	}

	g_value_init (value, G_TYPE_UINT64);
	g_value_set_uint64 (value, ret);

	return type_transform (value, gtype, error);
}

static gboolean
string_to_boolean (gchar const  *s,
                   GType         gtype,
                   GValue       *value,
                   GError      **error)
{
	g_value_init (value, G_TYPE_BOOLEAN);

	g_value_set_boolean (value, g_ascii_strcasecmp (s, "true") == 0);

	return type_transform (value, gtype, error);
}

static gboolean
string_to_enum (gchar const  *s,
                GType         gtype,
                GValue       *value,
                GError      **error)
{
	GEnumClass *cls;
	GEnumValue *v;
	gchar *r;
	guint64 idx;

	cls = g_type_class_ref (gtype);

	idx = g_ascii_strtoull (s, &r, 10);

	if (r && !*r)
	{
		v = g_enum_get_value (cls, (gint)idx);
	}

	if (!v)
	{
		v = g_enum_get_value_by_nick (cls, s);
	}

	if (!v)
	{
		v = g_enum_get_value_by_name (cls, s);
	}

	if (!v)
	{
		g_type_class_unref (cls);

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to enum value",
		             s);

		return FALSE;
	}

	g_value_init (value, gtype);
	g_value_set_enum (value, v->value);

	g_type_class_unref (cls);

	return TRUE;
}

static gboolean
string_to_flags (gchar const  *s,
                 GType         gtype,
                 GValue       *value,
                 GError      **error)
{
	GFlagsClass *cls;
	GFlagsValue *v;
	gchar *r;
	guint64 idx;

	cls = g_type_class_ref (gtype);

	idx = g_ascii_strtoull (s, &r, 10);

	if (r && !*r)
	{
		g_value_init (value, gtype);
		g_value_set_flags (value, v->value);

		g_type_class_unref (cls);
		return TRUE;
	}

	if (!v)
	{
		v = g_flags_get_value_by_nick (cls, s);
	}

	if (!v)
	{
		v = g_flags_get_value_by_name (cls, s);
	}

	if (!v)
	{
		g_type_class_unref (cls);

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to flags value",
		             s);

		return FALSE;
	}

	g_value_init (value, gtype);
	g_value_set_flags (value, v->value);

	g_type_class_unref (cls);

	return TRUE;
}

static gboolean
string_to_char (gchar const  *s,
                GType         gtype,
                GValue       *value,
                GError      **error)
{
	if (!*s || *(s + 1))
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Failed to convert `%s' to character",
		             s);

		return FALSE;
	}

	g_value_init (value, G_TYPE_CHAR);
	g_value_set_char (value, *s);

	return type_transform (value, gtype, error);
}

static gboolean
string_to_string (gchar const  *s,
                  GType         gtype,
                  GValue       *value,
                  GError      **error)
{
	g_value_init (value, G_TYPE_STRING);
	g_value_set_string (value, s);

	return type_transform (value, gtype, error);
}

typedef gboolean (*FromStringConversionFunc) (gchar const *, GType, GValue *, GError **);

typedef struct
{
	GType type;
	FromStringConversionFunc func;
} FromStringConversionMap;

static const FromStringConversionMap from_string_conv_map[] =
{
	{G_TYPE_BOOLEAN, string_to_boolean},
	{G_TYPE_DOUBLE, string_to_double},
	{G_TYPE_FLOAT, string_to_double},
	{G_TYPE_STRING, string_to_string},
	{G_TYPE_CHAR, string_to_char},
	{G_TYPE_UCHAR, string_to_char},
	{G_TYPE_INT, string_to_int},
	{G_TYPE_INT64, string_to_int},
	{G_TYPE_LONG, string_to_int},
	{G_TYPE_UINT, string_to_uint},
	{G_TYPE_UINT64, string_to_uint},
	{G_TYPE_ULONG, string_to_uint}
};

gboolean
cdn_string_to_value (gchar const  *s,
                     GType         type,
                     GValue       *value,
                     GError      **error)
{
	gint i;

	g_return_val_if_fail (s, FALSE);
	g_return_val_if_fail (value != NULL, FALSE);

	for (i = 0; i < G_N_ELEMENTS (from_string_conv_map); ++i)
	{
		if (from_string_conv_map[i].type == type)
		{
			return from_string_conv_map[i].func (s,
			                                     type,
			                                     value,
			                                     error);
		}
	}

	if (G_TYPE_IS_FLAGS (type))
	{
		return string_to_flags (s, type, value, error);
	}
	else if (G_TYPE_IS_ENUM (type))
	{
		return string_to_enum (s, type, value, error);
	}

	g_set_error (error,
	             CDN_NETWORK_LOAD_ERROR,
	             CDN_NETWORK_LOAD_ERROR_SYNTAX,
	             "String value `%s' cannot be converted to `%s'",
	             s,
	             g_type_name (type));

	return FALSE;
}
