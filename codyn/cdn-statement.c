/*
 * cdn-statement.c
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

#include "cdn-statement.h"

G_DEFINE_INTERFACE (CdnStatement, cdn_statement, G_TYPE_OBJECT);

/* Default implementation */

static void
cdn_statement_default_init (CdnStatementInterface *iface)
{
	static gboolean initialized = FALSE;

	if (!initialized)
	{
		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("line-start",
		                                                       "Line Start",
		                                                       "The line start",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("line-end",
		                                                       "Line End",
		                                                       "The line end",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("column-start",
		                                                       "Column Start",
		                                                       "The column start",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		g_object_interface_install_property (iface,
		                                     g_param_spec_int ("column-end",
		                                                       "Column End",
		                                                       "The column end",
		                                                       G_MININT,
		                                                       G_MAXINT,
		                                                       0,
		                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

		initialized = TRUE;
	}
}

void
cdn_statement_get_line (CdnStatement *statement,
                        gint         *start,
                        gint         *end)
{
	g_return_if_fail (CDN_STATEMENT (statement));

	if (start)
	{
		g_object_get (statement, "line-start", start, NULL);
	}

	if (end)
	{
		g_object_get (statement, "line-end", end, NULL);
	}
}

void
cdn_statement_set_line (CdnStatement *statement,
                        gint          start,
                        gint          end)
{
	g_return_if_fail (CDN_STATEMENT (statement));

	g_object_set (statement, "line-start", start, "line-end", end, NULL);
}

void
cdn_statement_get_column (CdnStatement *statement,
                          gint         *start,
                          gint         *end)
{
	g_return_if_fail (CDN_STATEMENT (statement));

	if (start)
	{
		g_object_get (statement, "column-start", start, NULL);
	}

	if (end)
	{
		g_object_get (statement, "column-end", end, NULL);
	}
}

void
cdn_statement_set_column (CdnStatement *statement,
                          gint          start,
                          gint          end)
{
	g_return_if_fail (CDN_STATEMENT (statement));

	g_object_set (statement, "column-start", start, "column-end", end, NULL);
}
