/*
 * cdn-statement.h
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

#ifndef __CDN_STATEMENT_H__
#define __CDN_STATEMENT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_STATEMENT			(cdn_statement_get_type ())
#define CDN_STATEMENT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_STATEMENT, CdnStatement))
#define CDN_IS_STATEMENT(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_STATEMENT))
#define CDN_STATEMENT_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_STATEMENT, CdnStatementInterface))

typedef struct _CdnStatement			CdnStatement;
typedef struct _CdnStatementInterface		CdnStatementInterface;

/**
 * CdnStatement:
 *
 * Interface for keeping track statements.
 **/
struct _CdnStatementInterface
{
	GTypeInterface parent;
};

GType cdn_statement_get_type (void) G_GNUC_CONST;

void cdn_statement_get_line (CdnStatement *statement,
                             gint         *start,
                             gint         *end);

void cdn_statement_set_line (CdnStatement *statement,
                             gint          start,
                             gint          end);

void cdn_statement_get_column (CdnStatement *statement,
                               gint         *start,
                               gint         *end);

void cdn_statement_set_column (CdnStatement *statement,
                               gint          start,
                               gint          end);

G_END_DECLS

#endif /* __CDN_STATEMENT_H__ */
