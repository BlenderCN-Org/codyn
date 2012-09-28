/*
 * cdn-layout.h
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

#ifndef __CDN_LAYOUT_H__
#define __CDN_LAYOUT_H__

#include <codyn/cdn-network.h>
#include <codyn/cdn-layoutable.h>

G_BEGIN_DECLS

#define CDN_TYPE_LAYOUT			(cdn_layout_get_type ())
#define CDN_LAYOUT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_LAYOUT, CdnLayout))
#define CDN_LAYOUT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_LAYOUT, CdnLayout const))
#define CDN_LAYOUT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_LAYOUT, CdnLayoutClass))
#define CDN_IS_LAYOUT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_LAYOUT))
#define CDN_IS_LAYOUT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_LAYOUT))
#define CDN_LAYOUT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_LAYOUT, CdnLayoutClass))

typedef struct _CdnLayout		CdnLayout;
typedef struct _CdnLayoutClass		CdnLayoutClass;
typedef struct _CdnLayoutPrivate	CdnLayoutPrivate;

struct _CdnLayout
{
	/*< private >*/
	GObject parent;

	CdnLayoutPrivate *priv;
};

struct _CdnLayoutClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType      cdn_layout_get_type (void) G_GNUC_CONST;

CdnLayout *cdn_layout_new      (CdnNetwork        *network);

void       cdn_layout_set      (CdnLayout         *layout,
                                CdnLayoutable     *layoutable,
                                gint               x,
                                gint               y);

G_END_DECLS

#endif /* __CDN_LAYOUT_H__ */
