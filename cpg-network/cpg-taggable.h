/*
 * cpg-taggable.h
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

#ifndef __CPG_TAGGABLE_H__
#define __CPG_TAGGABLE_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

#define CPG_TYPE_TAGGABLE			(cpg_taggable_get_type ())
#define CPG_TAGGABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_TAGGABLE, CpgTaggable))
#define CPG_IS_TAGGABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_TAGGABLE))
#define CPG_TAGGABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CPG_TYPE_TAGGABLE, CpgTaggableInterface))

typedef struct _CpgTaggable			CpgTaggable;
typedef struct _CpgTaggableInterface		CpgTaggableInterface;

struct _CpgTaggableInterface
{
	GTypeInterface parent;

	GHashTable *(*get_tagtable) (CpgTaggable *taggable);
};

GType        cpg_taggable_get_type     (void) G_GNUC_CONST;

gboolean     cpg_taggable_has_tag      (CpgTaggable  *taggable,
                                        gchar const  *tag);

void         cpg_taggable_add_tag      (CpgTaggable  *taggable,
                                        gchar const  *tag,
                                        gchar const  *value);

void         cpg_taggable_remove_tag   (CpgTaggable  *taggable,
                                        gchar const  *tag);

gchar const *cpg_taggable_get_tag      (CpgTaggable  *taggable,
                                        gchar const  *tag);

gboolean     cpg_taggable_try_get_tag  (CpgTaggable  *taggable,
                                        gchar const  *tag,
                                        gchar const **value);

GHashTable  *cpg_taggable_create_table ();

G_END_DECLS

#endif /* __CPG_TAGGABLE_H__ */
