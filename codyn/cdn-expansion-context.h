/*
 * cdn-expansion-context.h
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

#ifndef __CDN_EXPANSION_CONTEXT_H__
#define __CDN_EXPANSION_CONTEXT_H__

#include <glib-object.h>
#include <codyn/cdn-expansion.h>

G_BEGIN_DECLS

typedef struct _CdnExpansionContext CdnExpansionContext;

GType                cdn_expansion_context_get_type         (void) G_GNUC_CONST;

CdnExpansionContext *cdn_expansion_context_new              (CdnExpansionContext  *context);
CdnExpansionContext *cdn_expansion_context_new_unreffed     (CdnExpansionContext  *context);

CdnExpansionContext *cdn_expansion_context_ref              (CdnExpansionContext  *context);
void                 cdn_expansion_context_unref            (CdnExpansionContext  *context);

void                 cdn_expansion_context_shared_defines   (CdnExpansionContext  *context,
                                                             CdnExpansionContext  *from);

void                 cdn_expansion_context_add_define       (CdnExpansionContext  *context,
                                                             gchar const          *name,
                                                             CdnExpansion         *expansion);

void                 cdn_expansion_context_debug_print      (CdnExpansionContext  *context,
                                                             FILE                 *file);

void                 cdn_expansion_context_add_defines      (CdnExpansionContext  *context,
                                                             GHashTable           *defines);

gint                 cdn_expansion_context_increment_define (CdnExpansionContext  *context,
                                                             gchar const          *name,
                                                             gint                  num);

void                 cdn_expansion_context_add_expansion    (CdnExpansionContext  *context,
                                                             CdnExpansion         *expansion);

void                 cdn_expansion_context_add_expansions   (CdnExpansionContext  *context,
                                                             GSList const         *expansions);

GSList              *cdn_expansion_context_get_expansions   (CdnExpansionContext  *context);

CdnExpansion        *cdn_expansion_context_get_define       (CdnExpansionContext  *context,
                                                             gchar const          *name);

CdnExpansion        *cdn_expansion_context_get_expansion    (CdnExpansionContext  *context,
                                                             gint                  depth);

gchar               *cdn_expansion_context_calculate        (CdnExpansionContext  *context,
                                                             gchar const          *equation,
                                                             GError              **error);

gulong               cdn_expansion_context_get_marker       (CdnExpansionContext *context);
G_END_DECLS

#endif /* __CDN_EXPANSION_CONTEXT_H__ */
