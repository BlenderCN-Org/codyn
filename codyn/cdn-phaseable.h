/*
 * cdn-phaseable.h
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

#ifndef __CDN_PHASEABLE_H__
#define __CDN_PHASEABLE_H__

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

#define CDN_TYPE_PHASEABLE			(cdn_phaseable_get_type ())
#define CDN_PHASEABLE(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_PHASEABLE, CdnPhaseable))
#define CDN_IS_PHASEABLE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_PHASEABLE))
#define CDN_PHASEABLE_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_PHASEABLE, CdnPhaseableInterface))

typedef struct _CdnPhaseable			CdnPhaseable;
typedef struct _CdnPhaseableInterface		CdnPhaseableInterface;

struct _CdnPhaseableInterface
{
	GTypeInterface parent;

	GHashTable *(*get_phase_table) (CdnPhaseable *phaseable);
	void        (*set_phase_table) (CdnPhaseable *phaseable,
	                                GHashTable   *table);
};

typedef void (*CdnPhaseableForeachFunc) (CdnPhaseable *phaseable,
                                         gchar const  *phase,
                                         gpointer     userdata);

GType       cdn_phaseable_get_type           (void) G_GNUC_CONST;

gboolean    cdn_phaseable_is_active          (CdnPhaseable            *phaseable,
                                              gchar const             *phase);

void        cdn_phaseable_add_phase          (CdnPhaseable            *phaseable,
                                              gchar const             *phase);

void        cdn_phaseable_remove_phase       (CdnPhaseable            *phaseable,
                                              gchar const             *phase);

GHashTable *cdn_phaseable_get_phase_table    (CdnPhaseable            *phaseable);
void        cdn_phaseable_set_phase_table    (CdnPhaseable            *phaseable,
                                              GHashTable              *table);

void        cdn_phaseable_copy_to            (CdnPhaseable            *phaseable,
                                              CdnPhaseable            *dest);

void        cdn_phaseable_foreach            (CdnPhaseable            *phaseable,
                                              CdnPhaseableForeachFunc  func,
                                              gpointer                 userdata);

gboolean    cdn_phaseable_equal              (CdnPhaseable            *phaseable,
                                              CdnPhaseable            *other);

gchar     **cdn_phaseable_get_phases         (CdnPhaseable            *phaseable) G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /* __CDN_PHASEABLE_H__ */
