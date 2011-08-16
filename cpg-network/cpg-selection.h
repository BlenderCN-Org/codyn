/*
 * cpg-selection.h
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

#ifndef __CPG_SELECTION_H__
#define __CPG_SELECTION_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_SELECTION		(cpg_selection_get_type ())
#define CPG_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTION, CpgSelection))
#define CPG_SELECTION_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_SELECTION, CpgSelection const))
#define CPG_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_SELECTION, CpgSelectionClass))
#define CPG_IS_SELECTION(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_SELECTION))
#define CPG_IS_SELECTION_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_SELECTION))
#define CPG_SELECTION_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_SELECTION, CpgSelectionClass))

typedef struct _CpgSelection		CpgSelection;
typedef struct _CpgSelectionClass	CpgSelectionClass;
typedef struct _CpgSelectionPrivate	CpgSelectionPrivate;

struct _CpgSelection
{
	GObject parent;

	CpgSelectionPrivate *priv;
};

struct _CpgSelectionClass
{
	GObjectClass parent_class;
};

GType         cpg_selection_get_type       (void) G_GNUC_CONST;

CpgSelection *cpg_selection_new            (gpointer      object,
                                            GSList       *expansions,
                                            GHashTable   *defines);

CpgSelection *cpg_selection_new_defines    (gpointer      object,
                                            GSList       *expansions,
                                            GHashTable   *defines,
                                            gboolean      copy_defines);

CpgSelection *cpg_selection_copy           (CpgSelection *selection);
CpgSelection *cpg_selection_copy_defines   (CpgSelection *selection,
                                            gboolean      copy_defines);

gpointer      cpg_selection_get_object     (CpgSelection *selection);
GSList       *cpg_selection_get_expansions (CpgSelection *selection);

gchar const  *cpg_selection_get_define     (CpgSelection *selection,
                                            gchar const  *key);

GHashTable   *cpg_selection_get_defines    (CpgSelection *selection);

void          cpg_selection_add_define     (CpgSelection *selection,
                                            gchar const  *key,
                                            gchar const  *value);

G_END_DECLS

#endif /* __CPG_SELECTION_H__ */

