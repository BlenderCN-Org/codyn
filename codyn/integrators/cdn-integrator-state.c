/*
 * cdn-integrator-state.c
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

#include "cdn-integrator-state.h"
#include "cdn-edge.h"
#include "cdn-input.h"
#include "instructions/cdn-instruction-custom-operator.h"
#include "instructions/cdn-instruction-rand.h"
#include "cdn-phaseable.h"
#include "cdn-event.h"

#define CDN_INTEGRATOR_STATE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorStatePrivate))

static void update_phases (CdnIntegratorState *state);

/**
 * SECTION:cdn-integrator-state
 * @short_description: The integrator state
 *
 * The integrator state stores information on which properties need to be
 * integrated and how, based on a root object. It automatically tracks changes
 * in the root object and recalculates the properties that need to be
 * integrated.
 *
 */

struct _CdnIntegratorStatePrivate
{
	CdnObject *object;

	GSList *integrated_properties;
	GSList *direct_properties;
	GSList *all_properties;
	GSList *rand_instructions;
	GSList *rand_expressions;

	GSList *integrated_edge_actions;
	GSList *direct_edge_actions;
	GSList *operators;
	GSList *functions;

	GSList *inputs;
	GSList *expressions;
	GSList *events;
	GSList *phase_events;

	GSList *phase_direct_edge_actions;
	GSList *phase_integrated_edge_actions;

	GHashTable *direct_variables_hash;

	gchar *phase;
};

G_DEFINE_TYPE (CdnIntegratorState, cdn_integrator_state, G_TYPE_OBJECT)

enum
{
	UPDATED,
	NUM_SIGNALS
};

enum
{
	PROP_0,
	PROP_OBJECT,
	PROP_PHASE
};

static guint signals[NUM_SIGNALS] = {0,};

typedef struct
{
	CdnVariable *variable;
	GSList      *actions;
} DirectInfo;

static void
evaluate_notify (CdnExpression *expression,
                 DirectInfo    *info);

static void
direct_info_destroy (DirectInfo *info)
{
	if (info->variable)
	{
		g_object_unref (info->variable);
		info->variable = NULL;
	}
}

static DirectInfo *
direct_info_new (CdnVariable *variable)
{
	DirectInfo *info;
	CdnExpression *expr;

	info = g_slice_new (DirectInfo);

	info->variable = g_object_ref (variable);
	info->actions = NULL;

	expr = cdn_variable_get_expression (variable);

	cdn_expression_set_evaluate_notify (expr,
	                                    (CdnExpressionEvaluateNotify)evaluate_notify,
	                                    info,
	                                    (GDestroyNotify)direct_info_destroy);

	return info;
}

static void
direct_info_free (DirectInfo *info)
{
	if (info->variable)
	{
		CdnExpression *expr;

		expr = cdn_variable_get_expression (info->variable);

		cdn_expression_set_evaluate_notify (expr,
		                                    NULL,
		                                    NULL,
		                                    NULL);
	}

	g_slist_foreach (info->actions, (GFunc)g_object_unref, NULL);
	g_slist_free (info->actions);
	g_slice_free (DirectInfo, info);
}

static void
cdn_integrator_state_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_integrator_state_parent_class)->finalize (object);
}

static void
clear_list (GSList **lst)
{
	g_slist_free (*lst);
	*lst = NULL;
}

static void
clear_lists (CdnIntegratorState *state)
{
	clear_list (&(state->priv->integrated_properties));
	clear_list (&(state->priv->direct_properties));
	clear_list (&(state->priv->all_properties));

	clear_list (&(state->priv->integrated_edge_actions));
	clear_list (&(state->priv->direct_edge_actions));

	clear_list (&(state->priv->phase_integrated_edge_actions));
	clear_list (&(state->priv->phase_direct_edge_actions));

	clear_list (&(state->priv->inputs));
	clear_list (&(state->priv->expressions));
	clear_list (&(state->priv->operators));

	clear_list (&(state->priv->rand_expressions));
	clear_list (&(state->priv->rand_instructions));
	clear_list (&(state->priv->functions));
	clear_list (&(state->priv->events));
	clear_list (&(state->priv->phase_events));

	// Clear the table
	g_hash_table_ref (state->priv->direct_variables_hash);
	g_hash_table_destroy (state->priv->direct_variables_hash);
}

static void
on_object_compiled (CdnIntegratorState *state)
{
	cdn_integrator_state_update (state);
}

static void
object_weak_notify (CdnIntegratorState *state)
{
	state->priv->object = NULL;
}

static void
set_object (CdnIntegratorState *state,
            CdnObject           *object)
{
	if (state->priv->object)
	{
		g_signal_handlers_disconnect_by_func (state->priv->object,
		                                      on_object_compiled,
		                                      state);

		g_object_weak_unref (G_OBJECT (state->priv->object),
		                     (GWeakNotify)object_weak_notify,
		                     state);

		state->priv->object = NULL;
	}

	if (object)
	{
		state->priv->object = object;

		g_object_weak_ref (G_OBJECT (object),
		                   (GWeakNotify)object_weak_notify,
		                   state);

		if (cdn_object_is_compiled (CDN_OBJECT (object)))
		{
			cdn_integrator_state_update (state);
		}

		g_signal_connect_swapped (state->priv->object,
		                          "compiled",
		                          G_CALLBACK (on_object_compiled),
		                          state);
	}
}

static void
cdn_integrator_state_dispose (GObject *object)
{
	CdnIntegratorState *state = CDN_INTEGRATOR_STATE (object);

	set_object (state, NULL);
	clear_lists (state);

	g_free (state->priv->phase);

	if (state->priv->direct_variables_hash)
	{
		g_hash_table_destroy (state->priv->direct_variables_hash);
		state->priv->direct_variables_hash = NULL;
	}

	G_OBJECT_CLASS (cdn_integrator_state_parent_class)->dispose (object);
}

static gboolean
set_phase (CdnIntegratorState *state,
           gchar const        *phase)
{
	if (phase == state->priv->phase)
	{
		return FALSE;
	}

	if (g_strcmp0 (phase, state->priv->phase) == 0)
	{
		return FALSE;
	}

	g_free (state->priv->phase);
	state->priv->phase = g_strdup (phase);

	update_phases (state);
	return TRUE;
}

static void
cdn_integrator_state_set_property (GObject      *object,
                                   guint         prop_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
	CdnIntegratorState *self = CDN_INTEGRATOR_STATE (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			set_object (self, g_value_get_object (value));
		break;
		case PROP_PHASE:
			set_phase (self, g_value_get_string (value));
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_state_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CdnIntegratorState *self = CDN_INTEGRATOR_STATE (object);

	switch (prop_id)
	{
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_PHASE:
			g_value_set_string (value, self->priv->phase);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_integrator_state_class_init (CdnIntegratorStateClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_integrator_state_finalize;
	object_class->dispose = cdn_integrator_state_dispose;

	object_class->get_property = cdn_integrator_state_get_property;
	object_class->set_property = cdn_integrator_state_set_property;

	g_type_class_add_private (object_class, sizeof (CdnIntegratorStatePrivate));

	/**
	 * CdnIntegratorState::updated:
	 *
	 * Emitted when an integrator step has been performed
	 *
	 **/
	signals[UPDATED] =
		g_signal_new ("updated",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL, NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CdnIntegratorState:object:
	 *
	 * The object which is integrated
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_PHASE,
	                                 g_param_spec_string ("phase",
	                                                      "Phase",
	                                                      "Phase",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cdn_integrator_state_init (CdnIntegratorState *self)
{
	self->priv = CDN_INTEGRATOR_STATE_GET_PRIVATE (self);

	self->priv->direct_variables_hash =
		g_hash_table_new_full (g_direct_hash,
		                       g_direct_equal,
		                       NULL,
		                       (GDestroyNotify)direct_info_free);
}

/**
 * cdn_integrator_state_new:
 * @object: A #CdnObject
 *
 * Create a new integrator state for the given object.
 *
 * Returns: A #CdnIntegratorState
 *
 **/
CdnIntegratorState *
cdn_integrator_state_new (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return g_object_new (CDN_TYPE_INTEGRATOR_STATE,
	                     "object", object,
	                     NULL);
}

static GSList *
prepend_gslist_unique (GSList   *list,
                       gpointer  data)
{
	if (!g_slist_find (list, data))
	{
		return g_slist_prepend (list, data);
	}
	else
	{
		return list;
	}
}

static void
direct_cache_notify (CdnExpression *expression,
                     CdnEdgeAction *action)
{
	CdnVariable *v;

	v = cdn_edge_action_get_target_variable (action);

	if (v)
	{
		CdnExpression *expr;

		expr = cdn_variable_get_expression (v);

		cdn_expression_force_reset_cache (expr);
	}
}

static void
collect_link (CdnIntegratorState *state,
              CdnEdge            *link)
{
	GSList const *actions = cdn_edge_get_actions (link);

	while (actions)
	{
		CdnEdgeAction *action = actions->data;

		CdnVariable *target = cdn_edge_action_get_target_variable (action);

		if (cdn_variable_get_integrated (target))
		{
			state->priv->integrated_edge_actions =
				prepend_gslist_unique (state->priv->integrated_edge_actions,
				                       action);
		}
		else
		{
			DirectInfo *info;
			CdnExpression *expr;

			state->priv->direct_edge_actions =
				prepend_gslist_unique (state->priv->direct_edge_actions,
				                       action);

			info = g_hash_table_lookup (state->priv->direct_variables_hash,
			                            target);

			if (!info)
			{
				info = direct_info_new (target);

				g_hash_table_insert (state->priv->direct_variables_hash,
				                     target,
				                     info);
			}

			info->actions = g_slist_prepend (info->actions,
			                                 g_object_ref (action));

			expr = cdn_edge_action_get_equation (action);

			cdn_expression_set_cache_notify (expr,
			                                 (CdnExpressionCacheNotify)direct_cache_notify,
			                                 action,
			                                 NULL);
		}

		actions = g_slist_next (actions);
	}
}

static void
collect_actors (CdnIntegratorState *state,
                CdnNode           *object)
{
	GSList const *actors;

	actors = cdn_node_get_actors (object);

	while (actors)
	{
		CdnVariable *property = actors->data;

		if (cdn_variable_get_integrated (property))
		{
			state->priv->integrated_properties =
				prepend_gslist_unique (state->priv->integrated_properties,
				                       property);
		}
		else
		{
			state->priv->direct_properties =
				prepend_gslist_unique (state->priv->direct_properties,
				                       property);
		}

		actors = g_slist_next (actors);
	}
}

static void
collect_properties (CdnIntegratorState *state,
                    CdnObject          *object)
{
	GSList const *props;

	for (props = cdn_object_get_variables (object); props; props = g_slist_next (props))
	{
		state->priv->all_properties =
			prepend_gslist_unique (state->priv->all_properties,
			                       props->data);
	}
}

static void
collect (CdnIntegratorState *state,
         CdnObject          *object)
{
	collect_properties (state, object);

	if (CDN_IS_EDGE (object))
	{
		collect_link (state, CDN_EDGE (object));
		return;
	}

	if (CDN_IS_EVENT (object))
	{
		state->priv->events =
			g_slist_prepend (state->priv->events,
			                 object);
	}

	if (CDN_IS_NODE (object))
	{
		collect_actors (state, CDN_NODE (object));

		if (cdn_node_has_self_edge (CDN_NODE (object)))
		{
			collect_link (state, cdn_node_get_self_edge (CDN_NODE (object)));
		}
	}

	if (CDN_IS_INPUT (object))
	{
		state->priv->inputs =
			prepend_gslist_unique (state->priv->inputs,
			                       object);
	}

	if (CDN_IS_FUNCTION (object))
	{
		state->priv->functions =
			g_slist_prepend (state->priv->functions,
			                 object);
	}

	if (CDN_IS_NODE (object))
	{
		GSList const *children = cdn_node_get_children (CDN_NODE (object));

		while (children)
		{
			collect (state, children->data);
			children = g_slist_next (children);
		}
	}
}

static gboolean
action_depends_on (CdnEdgeAction *action,
                   CdnVariable   *prop)
{
	CdnExpression *a;
	CdnExpression *p;

	a = cdn_edge_action_get_equation (action);
	p = cdn_variable_get_expression (prop);

	return cdn_expression_depends_on (a, p);
}

static gint
edge_action_compare (CdnEdgeAction *a,
                     CdnEdgeAction *b)
{
	/* if a depends on b => 1
	   if b depends on a => -1
	   else 0 */
	if (action_depends_on (a, cdn_edge_action_get_target_variable (b)))
	{
		return 1;
	}
	else if (action_depends_on (b, cdn_edge_action_get_target_variable (a)))
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

static void
sort_edge_actions (CdnIntegratorState *state)
{
	state->priv->direct_edge_actions =
		g_slist_sort (state->priv->direct_edge_actions,
		              (GCompareFunc)edge_action_compare);
}

static void
collect_expressions (CdnExpression      *expression,
                     CdnIntegratorState *state)
{
	GSList const *instr;
	gboolean hadrand = FALSE;

	state->priv->expressions =
		g_slist_prepend (state->priv->expressions, expression);

	instr = cdn_expression_get_instructions (expression);

	while (instr)
	{
		if (CDN_IS_INSTRUCTION_RAND (instr->data))
		{
			state->priv->rand_instructions =
				g_slist_prepend (state->priv->rand_instructions,
				                 instr->data);

			if (!hadrand)
			{
				state->priv->rand_expressions =
					g_slist_prepend (state->priv->rand_expressions,
					                 expression);
			}

			hadrand = TRUE;
		}
		else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (instr->data))
		{
			CdnOperator *op;

			op = cdn_instruction_custom_operator_get_operator (instr->data);

			state->priv->operators =
				g_slist_prepend (state->priv->operators, op);
		}

		instr = g_slist_next (instr);
	}
}

static GSList *
get_phase_actions (CdnIntegratorState *state,
                   GSList             *actions)
{
	GSList *ret = NULL;

	while (actions)
	{
		CdnEdge *edge;

		edge = cdn_edge_action_get_edge (actions->data);

		if (cdn_phaseable_is_active (CDN_PHASEABLE (edge), state->priv->phase) &&
		    cdn_phaseable_is_active (CDN_PHASEABLE (actions->data), state->priv->phase))
		{
			ret = g_slist_prepend (ret, actions->data);
		}

		actions = g_slist_next (actions);
	}

	return g_slist_reverse (ret);
}

static GSList *
get_phase_events (CdnIntegratorState *state,
                  GSList             *events)
{
	GSList *ret = NULL;

	while (events)
	{
		if (cdn_phaseable_is_active (CDN_PHASEABLE (events->data),
		                             state->priv->phase))
		{
			ret = g_slist_prepend (ret, events->data);
		}

		events = g_slist_next (events);
	}

	return g_slist_reverse (ret);
}

static void
update_phases (CdnIntegratorState *state)
{
	clear_list (&(state->priv->phase_direct_edge_actions));
	clear_list (&(state->priv->phase_integrated_edge_actions));

	state->priv->phase_direct_edge_actions =
		get_phase_actions (state, state->priv->direct_edge_actions);

	state->priv->phase_integrated_edge_actions =
		get_phase_actions (state, state->priv->integrated_edge_actions);

	clear_list (&(state->priv->phase_events));
	state->priv->phase_events =
		get_phase_events (state, state->priv->events);
}

static void
sum_values (gdouble       *values,
            gdouble const *s,
            gint const    *indices,
            gint           num)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		gint idx;

		idx = indices ? indices[i] : i;

		values[idx] += s[i];
	}
}

static void
evaluate_notify (CdnExpression *expression,
                 DirectInfo    *info)
{
	GSList *item;
	gdouble *update;
	gint enumr;
	gint enumc;

	// Update variable cache from the direct actions
	cdn_variable_clear_update (info->variable);

	update = cdn_variable_get_update (info->variable, &enumr, &enumc);

	for (item = info->actions; item; item = g_slist_next (item))
	{
		CdnEdgeAction *action = item->data;
		CdnExpression *expr = cdn_edge_action_get_equation (action);
		gint const *indices;
		gint num_indices;
		gdouble const *values;
		gint numr;
		gint numc;

		indices = cdn_edge_action_get_indices (action, &num_indices);

		values = cdn_expression_evaluate_values (expr, &numr, &numc);

		sum_values (update,
		            values,
		            indices,
		            indices ? num_indices : numr * numc);
	}

	cdn_variable_set_values (info->variable,
	                         update,
	                         enumr,
	                         enumc);
}

/**
 * cdn_integrator_state_update:
 * @state: A #CdnIntegratorState
 *
 * Update the integrator state. This recursively goes through all the objects
 * contained in the associated #CdnIntegratorState:object and collects the
 * links and properties that need to be integrated.
 *
 **/
void
cdn_integrator_state_update (CdnIntegratorState *state)
{
	g_return_if_fail (CDN_IS_INTEGRATOR_STATE (state));

	clear_lists (state);

	g_free (state->priv->phase);
	state->priv->phase = NULL;

	collect (state, CDN_OBJECT (state->priv->object));

	state->priv->all_properties =
		g_slist_reverse (state->priv->all_properties);

	state->priv->integrated_properties =
		g_slist_reverse (state->priv->integrated_properties);

	state->priv->direct_properties =
		g_slist_reverse (state->priv->direct_properties);

	state->priv->inputs =
		g_slist_reverse (state->priv->inputs);

	state->priv->events =
		g_slist_reverse (state->priv->events);

	/* order the direct link actions based on their dependencies */
	sort_edge_actions (state);

	update_phases (state);

	cdn_object_foreach_expression (CDN_OBJECT (state->priv->object),
	                               (CdnForeachExpressionFunc)collect_expressions,
	                               state);

	state->priv->expressions =
		g_slist_reverse (state->priv->expressions);

	state->priv->rand_expressions =
		g_slist_reverse (state->priv->rand_expressions);

	state->priv->rand_instructions =
		g_slist_reverse (state->priv->rand_instructions);

	state->priv->operators =
		g_slist_reverse (state->priv->operators);

	g_signal_emit (state, signals[UPDATED], 0);
}

/**
 * cdn_integrator_state_integrated_properties:
 * @state: A #CdnIntegratorState
 *
 * Get the integrated properties which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_integrated_properties (CdnIntegratorState *state)
{
	/* Omit check for speed up */
	return state->priv->integrated_properties;
}

/**
 * cdn_integrator_state_direct_properties:
 * @state: A #CdnIntegratorState
 *
 * Get non-integrated properties which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_direct_properties (CdnIntegratorState *state)
{
	/* Omit check for speed up */
	return state->priv->direct_properties;
}

/**
 * cdn_integrator_state_integrated_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on integrated properties.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList of #CdnEdgeAction
 *
 **/
const GSList *
cdn_integrator_state_integrated_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->integrated_edge_actions;
}

/**
 * cdn_integrator_state_phase_integrated_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the integrated edge actions in the current phase.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_phase_integrated_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->phase_integrated_edge_actions;
}

/**
 * cdn_integrator_state_direct_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on non-integrated properties.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList of #CdnEdgeAction
 *
 **/
const GSList *
cdn_integrator_state_direct_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->direct_edge_actions;
}

/**
 * cdn_integrator_state_phase_direct_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the edge actions that act on non-integrated variables in the current
 * phase.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_phase_direct_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->phase_direct_edge_actions;
}

/**
 * cdn_integrator_state_all_properties:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on non-integrated properties.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_all_properties (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->all_properties;
}

/**
 * cdn_integrator_state_inputs:
 * @state: A #CdnIntegratorState
 *
 * Get the input states.
 *
 * Returns: (element-type CdnInput) (transfer none): A #GSList of #CdnInput
 *
 **/
const GSList *
cdn_integrator_state_inputs (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);

	return state->priv->inputs;
}

/**
 * cdn_integrator_state_expressions:
 * @state: A #CdnIntegratorState
 *
 * Get the expressions in the network.
 *
 * Returns: (element-type CdnExpression) (transfer none): A #GSList of #CdnExpression
 *
 **/
const GSList *
cdn_integrator_state_expressions (CdnIntegratorState *state)
{
	return state->priv->expressions;
}

/**
 * cdn_integrator_state_get_object:
 * @state: A #CdnIntegratorState
 *
 * Get the object of the integrator state.
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_integrator_state_get_object (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->object;
}

/**
 * cdn_integrator_state_operators:
 * @state: A #CdnIntegratorState
 *
 * Get the list of operators.
 *
 * Returns: (element-type CdnOperator) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_operators (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->operators;
}

/**
 * cdn_integrator_state_rand_instructions:
 * @state: A #CdnIntegratorState
 *
 * Get the list of rand instructions.
 *
 * Returns: (element-type CdnInstructionRand) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_rand_instructions (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->rand_instructions;
}

/**
 * cdn_integrator_state_rand_expressions:
 * @state: A #CdnIntegratorState
 *
 * Get the list of rand expressions.
 *
 * Returns: (element-type CdnExpression) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_rand_expressions (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->rand_expressions;
}

/**
 * cdn_integrator_state_functions:
 * @state: A #CdnIntegratorState
 *
 * Get the list of functions.
 *
 * Returns: (element-type CdnFunction) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_functions (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->functions;
}

/**
 * cdn_integrator_state_events:
 * @state: A #CdnIntegratorState
 *
 * Get the list of events.
 *
 * Returns: (element-type CdnEvent) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_events (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->events;
}

/**
 * cdn_integrator_state_phase_events:
 * @state: A #CdnIntegratorState
 *
 * Get the list of events in the current phase.
 *
 * Returns: (element-type CdnEvent) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_phase_events (CdnIntegratorState *state)
{
	/* Omit check to speed up */
	return state->priv->phase_events;
}

void
cdn_integrator_state_set_phase (CdnIntegratorState *state,
                                gchar const        *phase)
{
	if (set_phase (state, phase))
	{
		g_object_notify (G_OBJECT (state), "phase");
	}
}

gchar const *
cdn_integrator_state_get_phase (CdnIntegratorState *state)
{
	return state->priv->phase;
}
