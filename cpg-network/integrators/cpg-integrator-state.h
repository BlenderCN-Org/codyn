/*
 * cpg-integrator-state.h
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

#ifndef __CPG_INTEGRATOR_STATE_H__
#define __CPG_INTEGRATOR_STATE_H__

#include <glib-object.h>
#include <cpg-network/cpg-group.h>

G_BEGIN_DECLS

#define CPG_TYPE_INTEGRATOR_STATE            (cpg_integrator_state_get_type ())
#define CPG_INTEGRATOR_STATE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorState))
#define CPG_INTEGRATOR_STATE_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorState const))
#define CPG_INTEGRATOR_STATE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorStateClass))
#define CPG_IS_INTEGRATOR_STATE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INTEGRATOR_STATE))
#define CPG_IS_INTEGRATOR_STATE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INTEGRATOR_STATE))
#define CPG_INTEGRATOR_STATE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorStateClass))

typedef struct _CpgIntegratorState        CpgIntegratorState;
typedef struct _CpgIntegratorStateClass   CpgIntegratorStateClass;
typedef struct _CpgIntegratorStatePrivate CpgIntegratorStatePrivate;

struct _CpgIntegratorState
{
	/*< private >*/
	GObject parent;

	CpgIntegratorStatePrivate *priv;
};

struct _CpgIntegratorStateClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType               cpg_integrator_state_get_type                (void) G_GNUC_CONST;

CpgIntegratorState *cpg_integrator_state_new                     (CpgObject *object);
CpgObject          *cpg_integrator_state_get_object              (CpgIntegratorState *state);

const GSList       *cpg_integrator_state_integrated_properties   (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_direct_properties       (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_all_properties          (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_integrated_link_actions (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_direct_link_actions     (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_inputs                  (CpgIntegratorState *state);

GSList const       *cpg_integrator_state_rand_instructions       (CpgIntegratorState *state);
GSList const       *cpg_integrator_state_rand_expressions        (CpgIntegratorState *state);
GSList const       *cpg_integrator_state_functions               (CpgIntegratorState *state);

const GSList       *cpg_integrator_state_expressions             (CpgIntegratorState *state);
const GSList       *cpg_integrator_state_operators               (CpgIntegratorState *state);

void                cpg_integrator_state_update                  (CpgIntegratorState *state);

G_END_DECLS

#endif /* __CPG_INTEGRATOR_STATE_H__ */
