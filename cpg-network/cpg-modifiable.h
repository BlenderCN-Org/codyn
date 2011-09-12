/*
 * cpg-modifiable.h
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

#ifndef __CPG_MODIFIABLE_H__
#define __CPG_MODIFIABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_MODIFIABLE			(cpg_modifiable_get_type ())
#define CPG_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MODIFIABLE, CpgModifiable))
#define CPG_IS_MODIFIABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_MODIFIABLE))
#define CPG_MODIFIABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_MODIFIABLE, CpgModifiableInterface))

typedef struct _CpgModifiable			CpgModifiable;
typedef struct _CpgModifiableInterface		CpgModifiableInterface;

struct _CpgModifiableInterface
{
	GTypeInterface parent;

	gboolean (*get_modified) (CpgModifiable *modifiable);
	void     (*set_modified) (CpgModifiable *modifiable,
	                          gboolean       modified);
};

GType cpg_modifiable_get_type (void) G_GNUC_CONST;

gboolean cpg_modifiable_get_modified (CpgModifiable *modifiable);
void     cpg_modifiable_set_modified (CpgModifiable *modifiable,
                                      gboolean       modified);

G_END_DECLS

#endif /* __CPG_MODIFIABLE_H__ */
