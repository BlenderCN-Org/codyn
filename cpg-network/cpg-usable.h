/*
 * cpg-usable.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_USABLE_H__
#define __CPG_USABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_USABLE			(cpg_usable_get_type ())
#define CPG_USABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_USABLE, CpgUsable))
#define CPG_IS_USABLE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_USABLE))
#define CPG_USABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_USABLE, CpgUsableInterface))

typedef struct _CpgUsable		CpgUsable;
typedef struct _CpgUsableInterface	CpgUsableInterface;

struct _CpgUsableInterface
{
	GTypeInterface parent;

	guint    (*use_count) (CpgUsable *self);
	void     (*use)       (CpgUsable *self);
	gboolean (*unuse)     (CpgUsable *self);
};

GType cpg_usable_get_type (void) G_GNUC_CONST;

guint    cpg_usable_use_count (CpgUsable *self);
void     cpg_usable_use       (CpgUsable *self);
gboolean cpg_usable_unuse     (CpgUsable *self);

G_END_DECLS

#endif /* __CPG_USABLE_H__ */
