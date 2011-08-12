/*
 * cpg-statement.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_STATEMENT_H__
#define __CPG_STATEMENT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_STATEMENT			(cpg_statement_get_type ())
#define CPG_STATEMENT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_STATEMENT, CpgStatement))
#define CPG_IS_STATEMENT(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_STATEMENT))
#define CPG_STATEMENT_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_STATEMENT, CpgStatementInterface))

typedef struct _CpgStatement			CpgStatement;
typedef struct _CpgStatementInterface		CpgStatementInterface;

struct _CpgStatementInterface
{
	GTypeInterface parent;
};

GType cpg_statement_get_type (void) G_GNUC_CONST;

void cpg_statement_get_line (CpgStatement *statement,
                             gint         *start,
                             gint         *end);

void cpg_statement_set_line (CpgStatement *statement,
                             gint          start,
                             gint          end);

void cpg_statement_get_column (CpgStatement *statement,
                               gint         *start,
                               gint         *end);

void cpg_statement_set_column (CpgStatement *statement,
                               gint          start,
                               gint          end);

G_END_DECLS

#endif /* __CPG_STATEMENT_H__ */