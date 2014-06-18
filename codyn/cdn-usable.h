/*
 * cdn-usable.h
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

#ifndef __CDN_USABLE_H__
#define __CDN_USABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_USABLE			(cdn_usable_get_type ())
#define CDN_USABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_USABLE, CdnUsable))
#define CDN_IS_USABLE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_USABLE))
#define CDN_USABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_USABLE, CdnUsableInterface))

typedef struct _CdnUsable		CdnUsable;
typedef struct _CdnUsableInterface	CdnUsableInterface;

/**
 * CdnUsable:
 *
 * Interface for counting the uses of an object.
 *
 * This interface can be implemented when an object provides a use count.
 **/
struct _CdnUsableInterface
{
	GTypeInterface parent;

	guint    (*use_count) (CdnUsable *self);
	void     (*use)       (CdnUsable *self);
	gboolean (*unuse)     (CdnUsable *self);
};

GType cdn_usable_get_type (void) G_GNUC_CONST;

guint    cdn_usable_use_count (CdnUsable *self);
void     cdn_usable_use       (CdnUsable *self);
gboolean cdn_usable_unuse     (CdnUsable *self);

G_END_DECLS

#endif /* __CDN_USABLE_H__ */
