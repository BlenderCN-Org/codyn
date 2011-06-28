/*
 * cpg-annotatable.h
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

#ifndef __CPG_ANNOTATABLE_H__
#define __CPG_ANNOTATABLE_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_ANNOTATABLE			(cpg_annotatable_get_type ())
#define CPG_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ANNOTATABLE, CpgAnnotatable))
#define CPG_IS_ANNOTATABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_ANNOTATABLE))
#define CPG_ANNOTATABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_ANNOTATABLE, CpgAnnotatableInterface))

typedef struct _CpgAnnotatable		CpgAnnotatable;
typedef struct _CpgAnnotatableInterface	CpgAnnotatableInterface;

struct _CpgAnnotatableInterface
{
	GTypeInterface parent;

	gchar *(*get_title) (CpgAnnotatable *annotatable);

	void (*set_annotation) (CpgAnnotatable *annotatable,
	                        gchar const    *annotation);

	gchar *(*get_annotation) (CpgAnnotatable *annotatable);
};

GType cpg_annotatable_get_type (void) G_GNUC_CONST;

gchar *cpg_annotatable_get_title (CpgAnnotatable *self);

gchar *cpg_annotatable_get_annotation (CpgAnnotatable *self);

void cpg_annotatable_set_annotation (CpgAnnotatable *self,
                                     gchar const    *annotation);

G_END_DECLS

#endif /* __CPG_ANNOTATABLE_H__ */
