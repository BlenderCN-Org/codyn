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
#define CDN_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_SELECTION, CdnSelection))
#define CDN_SELECTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_SELECTION, CdnSelection const))
#define CDN_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_SELECTION, CdnSelectionClass))
#define CDN_IS_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_SELECTION))
#define CDN_IS_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_SELECTION))
#define CDN_SELECTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_SELECTION, CdnSelectionClass))

typedef struct _CdnSelection		CdnSelection;
typedef struct _CdnSelectionClass	CdnSelectionClass;
typedef struct _CdnSelectionPrivate	CdnSelectionPrivate;

struct _CdnSelection
{
	GObject parent;

	CdnSelectionPrivate *priv;
};

struct _CdnSelectionClass
{
	GObjectClass parent_class;
};

GType                cdn_selection_get_type    (void) G_GNUC_CONST;

CdnSelection        *cdn_selection_new         (gpointer             object,
                                                CdnExpansionContext *context);

void                 cdn_selection_set_object  (CdnSelection *selection,
                                                gpointer      object);

CdnSelection        *cdn_selection_copy        (CdnSelection *selection);

gpointer             cdn_selection_get_object  (CdnSelection *selection);
CdnExpansionContext *cdn_selection_get_context (CdnSelection *selection);

G_END_DECLS

#endif /* __CDN_SELECTION_H__ */

