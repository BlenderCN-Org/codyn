/*
 * cpg-embedded-context.h
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

#ifndef __CPG_EMBEDDED_CONTEXT_H__
#define __CPG_EMBEDDED_CONTEXT_H__

#include <glib-object.h>
#include <cpg-network/cpg-expansion.h>
#include <cpg-network/cpg-selection.h>

G_BEGIN_DECLS

#define CPG_TYPE_EMBEDDED_CONTEXT		(cpg_embedded_context_get_type ())
#define CPG_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContext))
#define CPG_EMBEDDED_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContext const))
#define CPG_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextClass))
#define CPG_IS_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_EMBEDDED_CONTEXT))
#define CPG_IS_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_EMBEDDED_CONTEXT))
#define CPG_EMBEDDED_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextClass))

typedef struct _CpgEmbeddedContext		CpgEmbeddedContext;
typedef struct _CpgEmbeddedContextClass		CpgEmbeddedContextClass;
typedef struct _CpgEmbeddedContextPrivate	CpgEmbeddedContextPrivate;

struct _CpgEmbeddedContext
{
	GObject parent;

	CpgEmbeddedContextPrivate *priv;
};

struct _CpgEmbeddedContextClass
{
	GObjectClass parent_class;
};

GType               cpg_embedded_context_get_type       (void) G_GNUC_CONST;

CpgEmbeddedContext *cpg_embedded_context_new            (void);

CpgEmbeddedContext *cpg_embedded_context_copy_top       (CpgEmbeddedContext *context);

void                cpg_embedded_context_add_define         (CpgEmbeddedContext *context,
                                                             gchar const        *name,
                                                             gchar const        *value);

void                cpg_embedded_context_add_defines        (CpgEmbeddedContext *context,
                                                             GHashTable         *defines);

gint                cpg_embedded_context_increment_define  (CpgEmbeddedContext  *context,
                                                            gchar const         *name,
                                                            gint                 num,
                                                            gboolean             retold);

void                cpg_embedded_context_save           (CpgEmbeddedContext *context);
void                cpg_embedded_context_save_defines   (CpgEmbeddedContext *context,
                                                         gboolean            copy_defines);
void                cpg_embedded_context_restore        (CpgEmbeddedContext *context);

void                cpg_embedded_context_add_selection  (CpgEmbeddedContext *context,
                                                         CpgSelection       *selection);

void                cpg_embedded_context_set_selection  (CpgEmbeddedContext *context,
                                                         CpgSelection       *selection);

void                cpg_embedded_context_add_expansion  (CpgEmbeddedContext *context,
                                                         CpgExpansion       *expansion);

void                cpg_embedded_context_add_expansions (CpgEmbeddedContext *context,
                                                         GSList             *expansions);

void                cpg_embedded_context_set_expansions (CpgEmbeddedContext *context,
                                                         GSList             *expansions);

GSList             *cpg_embedded_context_get_expansions (CpgEmbeddedContext *context);

gchar              *cpg_embedded_context_get_define     (CpgEmbeddedContext *context,
                                                         gchar const        *name);

GHashTable         *cpg_embedded_context_get_defines    (CpgEmbeddedContext *context);

void                cpg_embedded_context_set_defines    (CpgEmbeddedContext *context,
                                                         GHashTable         *defines,
                                                         gboolean            inherit);

CpgExpansion       *cpg_embedded_context_get_expansion  (CpgEmbeddedContext *context,
                                                         gint                depth);

gchar              *cpg_embedded_context_calculate      (CpgEmbeddedContext  *context,
                                                         gchar const         *equation,
                                                         GError             **error);

gulong              cpg_embedded_context_get_marker     (CpgEmbeddedContext *context);

G_END_DECLS

#endif /* __CPG_EMBEDDED_CONTEXT_H__ */
