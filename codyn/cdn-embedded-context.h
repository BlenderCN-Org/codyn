/*
 * cdn-embedded-context.h
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

#ifndef __CDN_EMBEDDED_CONTEXT_H__
#define __CDN_EMBEDDED_CONTEXT_H__

#include <glib-object.h>
#include <codyn/cdn-expansion.h>
#include <codyn/cdn-selection.h>

G_BEGIN_DECLS

#define CDN_TYPE_EMBEDDED_CONTEXT		(cdn_embedded_context_get_type ())
#define CDN_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EMBEDDED_CONTEXT, CdnEmbeddedContext))
#define CDN_EMBEDDED_CONTEXT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EMBEDDED_CONTEXT, CdnEmbeddedContext const))
#define CDN_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EMBEDDED_CONTEXT, CdnEmbeddedContextClass))
#define CDN_IS_EMBEDDED_CONTEXT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EMBEDDED_CONTEXT))
#define CDN_IS_EMBEDDED_CONTEXT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EMBEDDED_CONTEXT))
#define CDN_EMBEDDED_CONTEXT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EMBEDDED_CONTEXT, CdnEmbeddedContextClass))

typedef struct _CdnEmbeddedContext		CdnEmbeddedContext;
typedef struct _CdnEmbeddedContextClass		CdnEmbeddedContextClass;
typedef struct _CdnEmbeddedContextPrivate	CdnEmbeddedContextPrivate;

struct _CdnEmbeddedContext
{
	GObject parent;

	CdnEmbeddedContextPrivate *priv;
};

struct _CdnEmbeddedContextClass
{
	GObjectClass parent_class;
};

GType               cdn_embedded_context_get_type       (void) G_GNUC_CONST;

CdnEmbeddedContext *cdn_embedded_context_new            (void);

CdnEmbeddedContext *cdn_embedded_context_copy_top       (CdnEmbeddedContext *context);

void                cdn_embedded_context_add_define         (CdnEmbeddedContext *context,
                                                             gchar const        *name,
                                                             CdnExpansion       *expansion);

void                cdn_embedded_context_add_defines        (CdnEmbeddedContext *context,
                                                             GHashTable         *defines);

gint                cdn_embedded_context_increment_define  (CdnEmbeddedContext  *context,
                                                            gchar const         *name,
                                                            gint                 num);

void                cdn_embedded_context_save           (CdnEmbeddedContext *context);
void                cdn_embedded_context_save_defines   (CdnEmbeddedContext *context,
                                                         gboolean            copy_defines);
void                cdn_embedded_context_restore        (CdnEmbeddedContext *context);

void                cdn_embedded_context_add_selection  (CdnEmbeddedContext *context,
                                                         CdnSelection       *selection);

void                cdn_embedded_context_set_selection  (CdnEmbeddedContext *context,
                                                         CdnSelection       *selection);

void                cdn_embedded_context_add_expansion  (CdnEmbeddedContext *context,
                                                         CdnExpansion       *expansion);

void                cdn_embedded_context_add_expansions (CdnEmbeddedContext *context,
                                                         GSList             *expansions);

void                cdn_embedded_context_set_expansions (CdnEmbeddedContext *context,
                                                         GSList             *expansions);

GSList             *cdn_embedded_context_get_expansions (CdnEmbeddedContext *context);

CdnExpansion       *cdn_embedded_context_get_define     (CdnEmbeddedContext *context,
                                                         gchar const        *name);

GHashTable         *cdn_embedded_context_get_defines    (CdnEmbeddedContext *context);

void                cdn_embedded_context_set_defines    (CdnEmbeddedContext *context,
                                                         GHashTable         *defines,
                                                         gboolean            inherit);

CdnExpansion       *cdn_embedded_context_get_expansion  (CdnEmbeddedContext *context,
                                                         gint                depth);

gchar              *cdn_embedded_context_calculate      (CdnEmbeddedContext  *context,
                                                         gchar const         *equation,
                                                         GError             **error);

gulong              cdn_embedded_context_get_marker     (CdnEmbeddedContext *context);

G_END_DECLS

#endif /* __CDN_EMBEDDED_CONTEXT_H__ */
