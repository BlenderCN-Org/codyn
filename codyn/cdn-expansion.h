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

G_BEGIN_DECLS

#define CDN_TYPE_EXPANSION		(cdn_expansion_get_type ())
#define CDN_EXPANSION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EXPANSION, CdnExpansion))
#define CDN_EXPANSION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_EXPANSION, CdnExpansion const))
#define CDN_EXPANSION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_EXPANSION, CdnExpansionClass))
#define CDN_IS_EXPANSION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_EXPANSION))
#define CDN_IS_EXPANSION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_EXPANSION))
#define CDN_EXPANSION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_EXPANSION, CdnExpansionClass))

typedef struct _CdnExpansion		CdnExpansion;
typedef struct _CdnExpansionClass	CdnExpansionClass;
typedef struct _CdnExpansionPrivate	CdnExpansionPrivate;

struct _CdnExpansion
{
	GObject parent;

	CdnExpansionPrivate *priv;
};

struct _CdnExpansionClass
{
	GObjectClass parent_class;
};

GType         cdn_expansion_get_type         (void) G_GNUC_CONST;

CdnExpansion *cdn_expansion_new              (gchar const * const    *items);
CdnExpansion *cdn_expansion_newv             (gchar const            *item,
                                              ...);

CdnExpansion *cdn_expansion_new_one          (gchar const            *item);

CdnExpansion *cdn_expansion_copy             (CdnExpansion           *id);

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

gchar        *cdn_expansions_expand          (GSList                 *expansions,
                                              gchar const            *s,
                                              GRegex                 *regex);

void          cdn_expansions_annotate_indices (GSList                *expansions,
                                               gint                   start);

G_END_DECLS

#endif /* __CDN_EXPANSION_H__ */

