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

gboolean
cdn_string_to_value (gchar const  *s,
                     GType         type,
                     GValue       *value,
                     GError      **error)
{
	GValue sv = {0,};

	g_return_val_if_fail (value != NULL, FALSE);

	g_value_init (value, type);

	if (type == G_TYPE_BOOLEAN)
	{
		gint len = strlen (s);

		// Custom transform from string to boolean
		g_value_set_boolean (value,
		                     g_ascii_strcasecmp (s, "true") == 0 ||
		                     g_ascii_strcasecmp (s, "yes") == 0 ||
		                     (len == 1 && *s == '1'));

		return TRUE;
	}
	else if (type == G_TYPE_DOUBLE)
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

		g_value_set_double (value, ret);
		return TRUE;
	}
	else if (type == G_TYPE_INT || type == G_TYPE_UINT ||
	         type == G_TYPE_INT64 || type == G_TYPE_UINT64)
	{
		gint64 ret;
		gchar *r;

		ret = g_ascii_strtoll (s, &r, 10);

		if (!r || *r)
		{
			g_set_error (error,
			             CDN_NETWORK_LOAD_ERROR,
			             CDN_NETWORK_LOAD_ERROR_SYNTAX,
			             "Failed to convert `%s' to integer",
			             s);

			return FALSE;
		}

		g_value_init (&sv, G_TYPE_INT64);
		g_value_set_int64 (&sv, ret);

		if (!g_value_transform (&sv, value))
		{
			g_value_unset (&sv);
			g_value_unset (value);

			g_set_error (error,
			             CDN_NETWORK_LOAD_ERROR,
			             CDN_NETWORK_LOAD_ERROR_SYNTAX,
			             "Failed to convert `%s' to integer",
			             s);

			return FALSE;
		}

		g_value_unset (&sv);
		return TRUE;
	}

	if (!g_value_type_transformable (G_TYPE_STRING, type))
	{
		g_value_unset (value);

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "String value `%s' cannot be converted to `%s'",
		             s,
		             g_type_name (type));

		return FALSE;
	}

	g_value_init (&sv, G_TYPE_STRING);
	g_value_set_string (&sv, s);

	if (!g_value_transform (&sv, value))
	{
		g_value_unset (&sv);
		g_value_unset (value);

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_SYNTAX,
		             "Could not convert `%s' to `%s'",
		             s,
		             g_type_name (type));

		return FALSE;
	}

	g_value_unset (&sv);
	return TRUE;
}
