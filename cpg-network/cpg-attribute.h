/*
 * cpg-attribute.h
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

#ifndef __CPG_ATTRIBUTE_H__
#define __CPG_ATTRIBUTE_H__

#include <glib-object.h>
#include <cpg-network/cpg-embedded-string.h>

G_BEGIN_DECLS

#define CPG_TYPE_ATTRIBUTE		(cpg_attribute_get_type ())
#define CPG_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ATTRIBUTE, CpgAttribute))
#define CPG_ATTRIBUTE_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_ATTRIBUTE, CpgAttribute const))
#define CPG_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_ATTRIBUTE, CpgAttributeClass))
#define CPG_IS_ATTRIBUTE(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_ATTRIBUTE))
#define CPG_IS_ATTRIBUTE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_ATTRIBUTE))
#define CPG_ATTRIBUTE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_ATTRIBUTE, CpgAttributeClass))

typedef struct _CpgAttribute		CpgAttribute;
typedef struct _CpgAttributeClass	CpgAttributeClass;
typedef struct _CpgAttributePrivate	CpgAttributePrivate;

struct _CpgAttribute
{
	/*< private >*/
	GObject parent;

	CpgAttributePrivate *priv;
};

struct _CpgAttributeClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_attribute_get_type (void) G_GNUC_CONST;

CpgAttribute *cpg_attribute_new (gchar const *id);
CpgAttribute *cpg_attribute_newv (gchar const *id,
                                  ...) G_GNUC_NULL_TERMINATED;

void cpg_attribute_set_arguments (CpgAttribute *attr, GSList *arguments);

gchar const *cpg_attribute_get_id (CpgAttribute *attr);
GSList *cpg_attribute_get_arguments (CpgAttribute *attr);

GObject *cpg_attribute_get_argument (CpgAttribute *attr, gint i);
gint cpg_attribute_num_arguments (CpgAttribute *attr);

G_END_DECLS

#endif /* __CPG_ATTRIBUTE_H__ */
