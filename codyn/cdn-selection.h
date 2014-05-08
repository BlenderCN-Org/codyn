/*
 * cdn-selection.h
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

#ifndef __CDN_SELECTION_H__
#define __CDN_SELECTION_H__

#include <glib-object.h>
#include <codyn/cdn-expansion-context.h>

G_BEGIN_DECLS

#define CDN_TYPE_SELECTION		(cdn_selection_get_type ())
#define CDN_SELECTION(x)		((CdnSelection *)(x))

typedef struct _CdnSelection		CdnSelection;

GType                cdn_selection_get_type    (void) G_GNUC_CONST;

CdnSelection        *cdn_selection_new         (gpointer             object,
                                                CdnExpansionContext *context);

CdnSelection        *cdn_selection_ref         (CdnSelection *selection);
void                 cdn_selection_unref       (CdnSelection *selection);

void                 cdn_selection_set_object  (CdnSelection *selection,
                                                gpointer      object);

CdnSelection        *cdn_selection_copy        (CdnSelection *selection);

gpointer             cdn_selection_get_object  (CdnSelection *selection);
CdnExpansionContext *cdn_selection_get_context (CdnSelection *selection);
void                 cdn_selection_set_context (CdnSelection *selection,
                                                CdnExpansionContext *context);

gchar const        *_cdn_selection_get_override_name (CdnSelection *selection);

void                _cdn_selection_set_override_name (CdnSelection *selection,
                                                      gchar const  *name);

G_END_DECLS

#endif /* __CDN_SELECTION_H__ */

