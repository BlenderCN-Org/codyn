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
#include "cdn-io.h"
#include "instructions/cdn-instruction-custom-operator.h"
#include "instructions/cdn-instruction-rand.h"
#include "cdn-phaseable.h"
#include "cdn-event.h"

#define CDN_INTEGRATOR_STATE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INTEGRATOR_STATE, CdnIntegratorStatePrivate))

struct _CdnIntegratorStatePrivate
{
	CdnObject *object;

	GSList *integrated_variables;
	GSList *direct_variables;
	GSList *discrete_variables;
	GSList *all_variables;
	GSList *rand_instructions;
	GSList *rand_expressions;

	GSList *integrated_edge_actions;
	GSList *direct_edge_actions;
	GSList *discrete_edge_actions;

	GSList *nodes;
	GSList *operators;
	GSList *functions;

	GSList *io;
	GSList *expressions;
	GSList *events;
	GSList *phase_events;

	GSList *phase_direct_edge_actions;
	GSList *phase_integrated_edge_actions;
	GSList *phase_discrete_edge_actions;

	GHashTable *direct_variables_hash;
	GHashTable *state_hash;
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
	PROP_OBJECT
};

static guint signals[NUM_SIGNALS] = {0,};

typedef struct
{
	CdnVariable *variable;

	GSList      *actions;
	GSList      *phase_actions;
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

	info = g_slice_new0 (DirectInfo);

	info->variable = g_object_ref (variable);

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
	g_slist_free (info->phase_actions);

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
	clear_list (&(state->priv->integrated_variables));
	clear_list (&(state->priv->direct_variables));
	clear_list (&(state->priv->discrete_variables));
	clear_list (&(state->priv->all_variables));

	clear_list (&(state->priv->integrated_edge_actions));
	clear_list (&(state->priv->direct_edge_actions));
	clear_list (&(state->priv->discrete_edge_actions));

	clear_list (&(state->priv->phase_integrated_edge_actions));
	clear_list (&(state->priv->phase_direct_edge_actions));
	clear_list (&(state->priv->phase_discrete_edge_actions));

	clear_list (&(state->priv->io));
	clear_list (&(state->priv->expressions));
	clear_list (&(state->priv->operators));

	clear_list (&(state->priv->rand_expressions));
	clear_list (&(state->priv->rand_instructions));
	clear_list (&(state->priv->functions));
	clear_list (&(state->priv->events));
	clear_list (&(state->priv->phase_events));
	clear_list (&(state->priv->nodes));

	// Clear the table
	g_hash_table_remove_all (state->priv->direct_variables_hash);
	g_hash_table_remove_all (state->priv->state_hash);
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

	if (state->priv->direct_variables_hash)
	{
		g_hash_table_unref (state->priv->direct_variables_hash);
		state->priv->direct_variables_hash = NULL;
	}

	if (state->priv->state_hash)
	{
		g_hash_table_unref (state->priv->state_hash);
		state->priv->state_hash = NULL;
	}

	G_OBJECT_CLASS (cdn_integrator_state_parent_class)->dispose (object);
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
}

static void
release_state_node (CdnNode *node)
{
	g_object_unref (node);
}

static void
object_unref0 (gpointer ptr)
{
	if (ptr != NULL)
	{
		g_object_unref (ptr);
	}
}

static void
release_state_list (GSList *list)
{
	g_slist_foreach (list, (GFunc)object_unref0, NULL);
	g_slist_free (list);
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

	self->priv->state_hash =
		g_hash_table_new_full (g_direct_hash,
		                       g_direct_equal,
		                       (GDestroyNotify)release_state_node,
		                       (GDestroyNotify)release_state_list);
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

		if (cdn_variable_has_flag (target, CDN_VARIABLE_FLAG_DISCRETE))
		{
			state->priv->discrete_edge_actions =
				prepend_gslist_unique (state->priv->discrete_edge_actions,
				                       action);
		}
		else if (cdn_variable_has_flag (target, CDN_VARIABLE_FLAG_INTEGRATED))
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
		CdnVariable *variable = actors->data;

		if (cdn_variable_has_flag (variable, CDN_VARIABLE_FLAG_DISCRETE))
		{
			state->priv->discrete_variables =
				prepend_gslist_unique (state->priv->discrete_variables,
				                       variable);
		}
		else if (cdn_variable_has_flag (variable, CDN_VARIABLE_FLAG_INTEGRATED))
		{
			state->priv->integrated_variables =
				prepend_gslist_unique (state->priv->integrated_variables,
				                       variable);
		}
		else
		{
			state->priv->direct_variables =
				prepend_gslist_unique (state->priv->direct_variables,
				                       variable);
		}

		actors = g_slist_next (actors);
	}
}

static void
collect_variables (CdnIntegratorState *state,
                   CdnObject          *object)
{
	GSList const *props;

	for (props = cdn_object_get_variables (object); props; props = g_slist_next (props))
	{
		state->priv->all_variables =
			prepend_gslist_unique (state->priv->all_variables,
			                       props->data);
	}
}

static void
collect (CdnIntegratorState *state,
         CdnObject          *object)
{
	collect_variables (state, object);

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
		state->priv->nodes =
			g_slist_prepend (state->priv->nodes,
			                 object);

		collect_actors (state, CDN_NODE (object));

		if (cdn_node_has_self_edge (CDN_NODE (object)))
		{
			collect_link (state, cdn_node_get_self_edge (CDN_NODE (object)));
		}
	}

	if (CDN_IS_IO (object))
	{
		state->priv->io =
			prepend_gslist_unique (state->priv->io,
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

static void
sum_values (gdouble       *values,
            gdouble const *s,
            gint const    *indices,
            gint           num,
            gboolean       dosum)
{
	gint i;

	for (i = 0; i < num; ++i)
	{
		gint idx;

		idx = indices ? indices[i] : i;

		if (dosum)
		{
			values[idx] += s[i];
		}
		else
		{
			values[idx] = s[i];
		}
	}
}

static void
evaluate_notify (CdnExpression *expression,
                 DirectInfo    *info)
{
	GSList *item;
	CdnMatrix *update;

	// Update variable cache from the direct actions
	if (!info->phase_actions)
	{
		return;
	}

	cdn_variable_clear_update (info->variable);
	update = cdn_variable_get_update (info->variable);

	for (item = info->phase_actions; item; item = g_slist_next (item))
	{
		CdnEdgeAction *action = item->data;
		CdnExpression *expr = cdn_edge_action_get_equation (action);
		gint const *indices;
		gint num_indices;
		CdnMatrix const *values;

		indices = cdn_edge_action_get_indices (action, &num_indices);
		values = cdn_expression_evaluate_values (expr);

		sum_values (cdn_matrix_get_memory (update),
		            cdn_matrix_get (values),
		            indices,
		            indices ? num_indices : cdn_matrix_size (values),
		            cdn_edge_action_get_adds (action));
	}

	cdn_variable_set_values (info->variable, update);
}

static CdnNode *
find_state_object (CdnIntegratorState  *state,
                   CdnNode             *parent,
                   GSList             **lst)
{
	*lst = NULL;

	while (parent)
	{
		if (g_hash_table_lookup_extended (state->priv->state_hash,
		                                  parent,
		                                  NULL,
		                                  (gpointer *)lst))
		{
			break;
		}

		parent = cdn_object_get_parent (CDN_OBJECT (parent));
	}

	return parent;
}

static gboolean
phase_is_active (CdnPhaseable *ph,
                 gchar const  *state)
{
	// For edge actions also check the edge
	if (CDN_IS_EDGE_ACTION (ph))
	{
		CdnEdge *edge;

		edge = cdn_edge_action_get_edge (CDN_EDGE_ACTION (ph));

		if (!cdn_phaseable_is_active (CDN_PHASEABLE (edge), state))
		{
			return FALSE;
		}
	}

	return cdn_phaseable_is_active (ph, state);
}

static GSList **
get_state_list (CdnIntegratorState *state,
                CdnPhaseable       *ph)
{
	CdnVariable *tv;

	if (CDN_IS_EVENT (ph))
	{
		return &state->priv->phase_events;
	}

	tv = cdn_edge_action_get_target_variable (CDN_EDGE_ACTION (ph));

	if (cdn_variable_has_flag (tv, CDN_VARIABLE_FLAG_DISCRETE))
	{
		return &state->priv->phase_discrete_edge_actions;
	}
	else if (cdn_variable_has_flag (tv, CDN_VARIABLE_FLAG_INTEGRATED))
	{
		return &state->priv->phase_integrated_edge_actions;
	}
	else
	{
		return &state->priv->phase_direct_edge_actions;
	}
}

static void
update_direct_phase (CdnIntegratorState *state,
                     CdnEdgeAction      *action,
                     gboolean            add)
{
	CdnVariable *tv;
	DirectInfo *dinfo;

	// Need to remove from active list in direct
	// info
	tv = cdn_edge_action_get_target_variable (action);

	dinfo = g_hash_table_lookup (state->priv->direct_variables_hash,
	                             tv);

	cdn_expression_force_reset_cache (cdn_variable_get_expression (dinfo->variable));

	if (add)
	{
		dinfo->phase_actions = g_slist_prepend (dinfo->phase_actions,
		                                        action);
	}
	else
	{
		dinfo->phase_actions = g_slist_remove (dinfo->phase_actions,
		                                       action);
	}
}

static void
add_to_state_hash (CdnIntegratorState *state,
                   CdnPhaseable       *ph,
                   CdnNode            *parent)
{
	GHashTable *states;

	states = cdn_phaseable_get_phase_table (ph);

	if (states && g_hash_table_size (states) > 0)
	{
		CdnNode *st;
		GSList *lst;

		st = find_state_object (state, parent, &lst);

		if (st)
		{
			gchar const *nst;

			g_object_ref (ph);

			// Make sure to never change the first element of the
			// list so we don't have to reinsert it in the hash
			lst->next = g_slist_prepend (lst->next, ph);
			nst = cdn_node_get_state (st);

			if (phase_is_active (ph, nst))
			{
				GSList **ptr;

				ptr = get_state_list (state, ph);

				if (ptr == &state->priv->phase_direct_edge_actions)
				{
					update_direct_phase (state,
					                     CDN_EDGE_ACTION (ph),
					                     TRUE);
				}

				*ptr = g_slist_prepend (*ptr, ph);
			}
		}
	}
	else
	{
		GSList **ptr;

		ptr = get_state_list (state, ph);

		if (ptr == &state->priv->phase_direct_edge_actions)
		{
			update_direct_phase (state,
			                     CDN_EDGE_ACTION (ph),
			                     TRUE);
		}

		*ptr = g_slist_prepend (*ptr, ph);
	}
}

static CdnNode *
state_root_for_edge_action (CdnEdgeAction *action)
{
	CdnEdge *edge;
	CdnNode *input;

	edge = cdn_edge_action_get_edge (action);
	input = cdn_edge_get_input (edge);

	return input;
}

static void
extract_state_hash (CdnIntegratorState *state)
{
	GSList *item;

	g_hash_table_remove_all (state->priv->state_hash);

	// Collect all the events on states
	for (item = state->priv->events; item; item = g_slist_next (item))
	{
		CdnEvent *ev = item->data;
		gchar const *st;
		GHashTable *states;
		CdnNode *parent;

		parent = cdn_object_get_parent (CDN_OBJECT (ev));

		if (g_hash_table_lookup (state->priv->state_hash, parent))
		{
			continue;
		}

		st = cdn_event_get_goto_state (ev);
		states = cdn_phaseable_get_phase_table (CDN_PHASEABLE (ev));

		if (st != NULL || (states && g_hash_table_size (states) > 0))
		{
			g_hash_table_insert (state->priv->state_hash,
			                     g_object_ref (parent),
			                     g_slist_prepend (NULL, NULL));
		}
	}

	// Also collect all nodes with have an initial state
	for (item = state->priv->nodes; item; item = g_slist_next (item))
	{
		CdnNode *node = item->data;

		if (cdn_node_get_initial_state (node) != NULL &&
		    !g_hash_table_lookup (state->priv->state_hash, node))
		{
			g_hash_table_insert (state->priv->state_hash,
			                     g_object_ref (node),
			                     g_slist_prepend (NULL, NULL));
		}
	}

	// Now, collect phaseables
	for (item = state->priv->integrated_edge_actions; item; item = g_slist_next (item))
	{
		CdnNode *parent;

		parent = state_root_for_edge_action (item->data);
		add_to_state_hash (state, item->data, parent);
	}

	for (item = state->priv->direct_edge_actions; item; item = g_slist_next (item))
	{
		CdnNode *parent;

		parent = state_root_for_edge_action (item->data);
		add_to_state_hash (state, item->data, parent);
	}

	for (item = state->priv->discrete_edge_actions; item; item = g_slist_next (item))
	{
		CdnNode *parent;

		parent = state_root_for_edge_action (item->data);
		add_to_state_hash (state, item->data, parent);
	}

	for (item = state->priv->events; item; item = g_slist_next (item))
	{
		CdnNode *parent;

		parent = CDN_NODE (cdn_object_get_parent (item->data));
		add_to_state_hash (state, item->data, parent);
	}
}

static gint
compare_events (CdnEvent *a, CdnEvent *b)
{
	CdnNode *pa = cdn_object_get_parent (CDN_OBJECT (a));
	CdnNode *pb = cdn_object_get_parent (CDN_OBJECT (b));

	return (pa < pb ? -1 : (pa > pb ? 1 : 0));
}

static void
sort_events (CdnIntegratorState *state)
{
	state->priv->events =
		g_slist_reverse (state->priv->events);

	state->priv->events =
		g_slist_sort (state->priv->events, (GCompareFunc)compare_events);
}

/**
 * cdn_integrator_state_update:
 * @state: A #CdnIntegratorState
 *
 * Update the integrator state. This recursively goes through all the objects
 * contained in the associated #CdnIntegratorState:object and collects the
 * links and variables that need to be integrated.
 *
 **/
void
cdn_integrator_state_update (CdnIntegratorState *state)
{
	g_return_if_fail (CDN_IS_INTEGRATOR_STATE (state));

	clear_lists (state);

	collect (state, CDN_OBJECT (state->priv->object));

	state->priv->all_variables =
		g_slist_reverse (state->priv->all_variables);

	state->priv->integrated_variables =
		g_slist_reverse (state->priv->integrated_variables);

	state->priv->direct_variables =
		g_slist_reverse (state->priv->direct_variables);

	state->priv->discrete_variables =
		g_slist_reverse (state->priv->discrete_variables);

	state->priv->io =
		g_slist_reverse (state->priv->io);

	sort_events (state);

	/* order the direct link actions based on their dependencies */
	sort_edge_actions (state);

	extract_state_hash (state);

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
 * cdn_integrator_state_integrated_variables:
 * @state: A #CdnIntegratorState
 *
 * Get the integrated variables which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_integrated_variables (CdnIntegratorState *state)
{
	/* Omit check for speed up */
	return state->priv->integrated_variables;
}

/**
 * cdn_integrator_state_direct_variables:
 * @state: A #CdnIntegratorState
 *
 * Get non-integrated variables which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_direct_variables (CdnIntegratorState *state)
{
	/* Omit check for speed up */
	return state->priv->direct_variables;
}

/**
 * cdn_integrator_state_discrete_variables:
 * @state: A #CdnIntegratorState
 *
 * Get the discrete variables which are acted upon by links.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_discrete_variables (CdnIntegratorState *state)
{
	/* Omit check for speed up */
	return state->priv->discrete_variables;
}

/**
 * cdn_integrator_state_integrated_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on integrated variables.
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
 * Get the link actions that act on non-integrated variables.
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
 * cdn_integrator_state_discrete_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on discrete variables.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList of #CdnEdgeAction
 *
 **/
const GSList *
cdn_integrator_state_discrete_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->discrete_edge_actions;
}

/**
 * cdn_integrator_state_phase_discrete_edge_actions:
 * @state: A #CdnIntegratorState
 *
 * Get the discrete edge actions in the current phase.
 *
 * Returns: (element-type CdnEdgeAction) (transfer none): A #GSList
 *
 **/
GSList const *
cdn_integrator_state_phase_discrete_edge_actions (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->phase_discrete_edge_actions;
}

/**
 * cdn_integrator_state_all_variables:
 * @state: A #CdnIntegratorState
 *
 * Get the link actions that act on non-integrated variables.
 *
 * Returns: (element-type CdnVariable) (transfer none): A #GSList of #CdnVariable
 *
 **/
const GSList *
cdn_integrator_state_all_variables (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->all_variables;
}

/**
 * cdn_integrator_state_io:
 * @state: A #CdnIntegratorState
 *
 * Get the io states.
 *
 * Returns: (element-type CdnIo) (transfer none): A #GSList of #CdnIo
 *
 **/
const GSList *
cdn_integrator_state_io (CdnIntegratorState *state)
{
	g_return_val_if_fail (CDN_IS_INTEGRATOR_STATE (state), NULL);

	return state->priv->io;
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

/**
 * cdn_integrator_state_set_state:
 * @state: A #CdnIntegratorState
 * @node: A #CdnNode
 * @st: the state
 * @events_added: (element-type CdnEvent) (transfer container): return value for the events added by changing the state
 * @events_removed: (element-type CdnEvent) (transfer container): return value for the events removed changing the state
 *
 * Change the active state of @node to @st.
 *
 */
void
cdn_integrator_state_set_state (CdnIntegratorState  *state,
                                CdnNode             *node,
                                gchar const         *st,
                                GSList             **events_added,
                                GSList             **events_removed)
{
	gchar const *curst;
	GSList *lst;
	GSList *item;

	// Set the state of @node to st. Update all the phaseables that
	// depend on this state
	curst = cdn_node_get_state (node);
	lst = g_hash_table_lookup (state->priv->state_hash, node);

	if (events_added)
	{
		*events_added = NULL;
	}

	if (events_removed)
	{
		*events_removed = NULL;
	}

	// Note: first item is always NULL
	for (item = lst->next; item; item = g_slist_next (item))
	{
		CdnPhaseable *ph = item->data;
		gboolean activenow;
		gboolean activelater;

		activenow = phase_is_active (ph, curst);
		activelater = phase_is_active (ph, st);

		// See if ph is active now, but not later
		if (activenow && !activelater)
		{
			GSList **ptr;

			// Remove from list
			ptr = get_state_list (state, ph);

			if (ptr == &state->priv->phase_direct_edge_actions)
			{
				update_direct_phase (state, item->data, FALSE);
			}

			*ptr = g_slist_remove (*ptr, ph);

			if (events_removed && CDN_IS_EVENT (item->data))
			{
				*events_removed = g_slist_prepend (*events_removed,
				                                   item->data);
			}
		}
		else if (!activenow && activelater)
		{
			GSList **ptr;

			// Add to list
			ptr = get_state_list (state, ph);

			if (ptr == &state->priv->phase_direct_edge_actions)
			{
				update_direct_phase (state, item->data, TRUE);
			}

			*ptr = g_slist_prepend (*ptr, ph);

			if (events_added && CDN_IS_EVENT (item->data))
			{
				*events_added = g_slist_prepend (*events_added,
				                                 item->data);
			}
		}
	}

	if (events_added)
	{
		*events_added = g_slist_reverse (*events_added);
	}

	if (events_removed)
	{
		*events_removed = g_slist_reverse (*events_removed);
	}

	cdn_node_set_state (node, st);
}
