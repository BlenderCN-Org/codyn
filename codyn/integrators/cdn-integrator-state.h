/*
 * cdn-integrator-state.h
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

#ifndef __CDN_INTEGRATOR_STATE_H__
#define __CDN_INTEGRATOR_STATE_H__

#include <glib-object.h>
#include <codyn/cdn-node.h>

G_BEGIN_DECLS

#define CDN_TYPE_INTEGRATOR_STATE            (cdn_integrator_state_get_type ())
#define CDN_INTEGRATOR_STATE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorState))
#define CDN_INTEGRATOR_STATE_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorState const))
#define CDN_INTEGRATOR_STATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorStateClass))
#define CDN_IS_INTEGRATOR_STATE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INTEGRATOR_STATE))
#define CDN_IS_INTEGRATOR_STATE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INTEGRATOR_STATE))
#define CDN_INTEGRATOR_STATE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorStateClass))

typedef struct _CdnIntegratorState        CdnIntegratorState;
typedef struct _CdnIntegratorStateClass   CdnIntegratorStateClass;
typedef struct _CdnIntegratorStatePrivate CdnIntegratorStatePrivate;

struct _CdnIntegratorState
{
	/*< private >*/
	GObject parent;

	CdnIntegratorStatePrivate *priv;
};

struct _CdnIntegratorStateClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType               cdn_integrator_state_get_type                (void) G_GNUC_CONST;

CdnIntegratorState *cdn_integrator_state_new                     (CdnObject *object);
CdnObject          *cdn_integrator_state_get_object              (CdnIntegratorState *state);

const GSList       *cdn_integrator_state_integrated_properties   (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_direct_properties       (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_all_properties          (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_integrated_edge_actions (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_direct_edge_actions     (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_phase_integrated_edge_actions (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_phase_direct_edge_actions     (CdnIntegratorState *state);

const GSList       *cdn_integrator_state_io                      (CdnIntegratorState *state);

const GSList       *cdn_integrator_state_events                  (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_phase_events            (CdnIntegratorState *state);

GSList const       *cdn_integrator_state_rand_instructions       (CdnIntegratorState *state);
GSList const       *cdn_integrator_state_rand_expressions        (CdnIntegratorState *state);
GSList const       *cdn_integrator_state_functions               (CdnIntegratorState *state);

const GSList       *cdn_integrator_state_expressions             (CdnIntegratorState *state);
const GSList       *cdn_integrator_state_operators               (CdnIntegratorState *state);

void                cdn_integrator_state_update                  (CdnIntegratorState *state);

void                cdn_integrator_state_set_phase               (CdnIntegratorState *state,
                                                                  gchar const        *phase);

gchar const        *cdn_integrator_state_get_phase               (CdnIntegratorState *state);

G_END_DECLS

#endif /* __CDN_INTEGRATOR_STATE_H__ */
