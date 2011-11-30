/*
 * cdn-modifiable.h
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

#ifndef __CDN_MODIFIABLE_H__
#define __CDN_MODIFIABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_MODIFIABLE			(cdn_modifiable_get_type ())
#define CDN_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_MODIFIABLE, CdnModifiable))
#define CDN_IS_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_MODIFIABLE))
#define CDN_MODIFIABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_MODIFIABLE, CdnModifiableInterface))

typedef struct _CdnModifiable			CdnModifiable;
typedef struct _CdnModifiableInterface		CdnModifiableInterface;

struct _CdnModifiableInterface
{
	GTypeInterface parent;

	gboolean (*get_modified) (CdnModifiable *modifiable);
	void     (*set_modified) (CdnModifiable *modifiable,
	                          gboolean       modified);
};

GType cdn_modifiable_get_type (void) G_GNUC_CONST;

gboolean cdn_modifiable_get_modified (CdnModifiable *modifiable);
void     cdn_modifiable_set_modified (CdnModifiable *modifiable,
                                      gboolean       modified);

G_END_DECLS

#endif /* __CDN_MODIFIABLE_H__ */
