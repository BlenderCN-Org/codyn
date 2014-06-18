/*
 * cdn-layoutable.h
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

#ifndef __CDN_LAYOUTABLE_H__
#define __CDN_LAYOUTABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_LAYOUTABLE			(cdn_layoutable_get_type ())
#define CDN_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_LAYOUTABLE, CdnLayoutable))
#define CDN_IS_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_LAYOUTABLE))
#define CDN_LAYOUTABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_LAYOUTABLE, CdnLayoutableInterface))

typedef struct _CdnLayoutable		CdnLayoutable;
typedef struct _CdnLayoutableInterface	CdnLayoutableInterface;

/**
 * CdnLayoutable:
 *
 * Interface for object layouting.
 *
 * This interface can be implemented when an object can be layouted.
 **/
struct _CdnLayoutableInterface
{
	GTypeInterface parent;

	void (*get_location) (CdnLayoutable *layoutable,
	                          gint          *x,
	                          gint          *y);

	void (*set_location) (CdnLayoutable *layoutable,
	                      gint           x,
	                      gint           y);

	gboolean (*supports_location) (CdnLayoutable *layoutable);

	gboolean (*get_has_location) (CdnLayoutable *layoutable);
	void (*set_has_location) (CdnLayoutable *layoutable,
	                          gboolean       has_location);
};

GType    cdn_layoutable_get_type          (void) G_GNUC_CONST;

void     cdn_layoutable_get_location      (CdnLayoutable *layoutable,
                                           gint          *x,
                                           gint          *y);

void     cdn_layoutable_set_location      (CdnLayoutable *layoutable,
                                           gint           x,
                                           gint           y);

gboolean cdn_layoutable_get_has_location  (CdnLayoutable *layoutable);
void     cdn_layoutable_set_has_location  (CdnLayoutable *layoutable,
                                           gboolean       has_location);

gboolean cdn_layoutable_supports_location (CdnLayoutable *layoutable);

G_END_DECLS

#endif /* __CDN_LAYOUTABLE_H__ */
