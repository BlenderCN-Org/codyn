/*
 * cdn-expansion.h
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

#ifndef __CDN_EXPANSION_H__
#define __CDN_EXPANSION_H__

#include <glib-object.h>
#include <codyn/cdn-mini-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_EXPANSION		(cdn_expansion_get_type ())

typedef struct _CdnExpansion		CdnExpansion;

GType         cdn_expansion_get_type         (void) G_GNUC_CONST;

CdnExpansion *cdn_expansion_new              (gchar const * const    *items);
CdnExpansion *cdn_expansion_new_sized        (gchar const * const    *items,
                                              gint                    sized);
CdnExpansion *cdn_expansion_newv             (gchar const            *item,
                                              ...);

CdnExpansion *cdn_expansion_new_one          (gchar const            *item);

CdnExpansion *cdn_expansion_copy             (CdnExpansion           *id);
CdnExpansion *cdn_expansion_ref             (CdnExpansion           *id);
void cdn_expansion_unref             (CdnExpansion           *id);

gint          cdn_expansion_num              (CdnExpansion           *id);

gchar const  *cdn_expansion_get              (CdnExpansion           *id,
                                              gint                    idx);

gint          cdn_expansion_get_index        (CdnExpansion           *id,
                                              gint                    idx);

void          cdn_expansion_set_index        (CdnExpansion           *id,
                                              gint                    idx,
                                              gint                    val);

void          cdn_expansion_add              (CdnExpansion           *id,
                                              gchar const            *item);

void          cdn_expansion_insert           (CdnExpansion           *id,
                                              gint                    idx,
                                              gchar const            *item);

void          cdn_expansion_set              (CdnExpansion           *id,
                                              gint                    idx,
                                              gchar const            *val);

void          cdn_expansion_append           (CdnExpansion           *id,
                                              CdnExpansion           *other,
                                              gint                    idx);

void          cdn_expansion_prepend          (CdnExpansion           *id,
                                              CdnExpansion           *other,
                                              gint                    idx);

void          cdn_expansions_annotate_indices (GSList                *expansions,
                                               gint                   start);

G_END_DECLS

#endif /* __CDN_EXPANSION_H__ */

