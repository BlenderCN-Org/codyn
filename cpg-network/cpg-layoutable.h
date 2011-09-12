/*
 * cpg-layoutable.h
 * This file is part of cpg-network
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

#ifndef __CPG_LAYOUTABLE_H__
#define __CPG_LAYOUTABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_LAYOUTABLE			(cpg_layoutable_get_type ())
#define CPG_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_LAYOUTABLE, CpgLayoutable))
#define CPG_IS_LAYOUTABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_LAYOUTABLE))
#define CPG_LAYOUTABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_LAYOUTABLE, CpgLayoutableInterface))

typedef struct _CpgLayoutable		CpgLayoutable;
typedef struct _CpgLayoutableInterface	CpgLayoutableInterface;

struct _CpgLayoutableInterface
{
	GTypeInterface parent;

	void (*get_location) (CpgLayoutable *layoutable,
	                          gint          *x,
	                          gint          *y);

	void (*set_location) (CpgLayoutable *layoutable,
	                      gint           x,
	                      gint           y);

	gboolean (*supports_location) (CpgLayoutable *layoutable);

	gboolean (*get_has_location) (CpgLayoutable *layoutable);
	void (*set_has_location) (CpgLayoutable *layoutable,
	                          gboolean       has_location);
};

GType    cpg_layoutable_get_type          (void) G_GNUC_CONST;

void     cpg_layoutable_get_location      (CpgLayoutable *self,
                                           gint          *x,
                                           gint          *y);

void     cpg_layoutable_set_location      (CpgLayoutable *self,
                                           gint           x,
                                           gint           y);

gboolean cpg_layoutable_get_has_location  (CpgLayoutable *self);
void     cpg_layoutable_set_has_location  (CpgLayoutable *self,
                                           gboolean       has_location);

gboolean cpg_layoutable_supports_location (CpgLayoutable *self);

G_END_DECLS

#endif /* __CPG_LAYOUTABLE_H__ */
