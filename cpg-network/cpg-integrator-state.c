/*
 * cpg-integrator-state.c
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

#include "cpg-integrator-state.h"
#include "cpg-link.h"
#include "cpg-input.h"
#include "instructions/cpg-instruction-custom-operator.h"

#define CPG_INTEGRATOR_STATE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INTEGRATOR_STATE, CpgIntegratorStatePrivate))

/**
 * SECTION:cpg-integrator-state
 * @short_description: The integrator state
 *
 * The integrator state stores information on which properties need to be
 * integrated and how, based on a root object. It automatically tracks changes
 * in the root object and recalculates the properties that need to be
 * integrated.
 *
 */

struct _CpgIntegratorStatePrivate
{
	CpgObject *object;

	GSList *integrated_properties;
	GSList *direct_properties;
	GSList *all_properties;

	GSList *integrated_link_actions;
	GSList *direct_link_actions;
	GSList *operators;

	GSList *inputs;
	GSList *expressions;
};

G_DEFINE_TYPE (CpgIntegratorState, cpg_integrator_state, G_TYPE_OBJECT)

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

static void
cpg_integrator_state_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_integrator_state_parent_class)->finalize (object);
}

static void
clear_lists (CpgIntegratorState *state)
{
	g_slist_free (state->priv->integrated_properties);
	state->priv->integrated_properties = NULL;

	g_slist_free (state->priv->direct_properties);
	state->priv->direct_properties = NULL;

	g_slist_free (state->priv->all_properties);
	state->priv->all_properties = NULL;

	g_slist_free (state->priv->integrated_link_actions);
	state->priv->integrated_link_actions = NULL;

	g_slist_free (state->priv->direct_link_actions);
	state->priv->direct_link_actions = NULL;

	g_slist_free (state->priv->inputs);
	state->priv->inputs = NULL;

	g_slist_free (state->priv->expressions);
	state->priv->expressions = NULL;

	g_slist_free (state->priv->operators);
	state->priv->operators = NULL;
}

static void
on_object_compiled (CpgIntegratorState *state)
{
	cpg_integrator_state_update (state);
}

static void
set_object (CpgIntegratorState *state,
           CpgObject           *object)
{
	if (state->priv->object)
	{
		g_signal_handlers_disconnect_by_func (state->priv->object,
		                                      on_object_compiled,
		                                      state);

		g_object_unref (state->priv->object);

		state->priv->object = NULL;
	}

	if (object)
	{
		state->priv->object = g_object_ref (object);

		if (cpg_object_is_compiled (CPG_OBJECT (object)))
		{
			cpg_integrator_state_update (state);
		}

		g_signal_connect_swapped (state->priv->object,
		                          "compiled",
		                          G_CALLBACK (on_object_compiled),
		                          state);
	}
}

static void
cpg_integrator_state_dispose (GObject *object)
{
	CpgIntegratorState *state = CPG_INTEGRATOR_STATE (object);

	set_object (state, NULL);

	clear_lists (state);

	G_OBJECT_CLASS (cpg_integrator_state_parent_class)->dispose (object);
}

static void
cpg_integrator_state_set_property (GObject      *object,
                                   guint         prop_id,
                                   const GValue *value,
                                   GParamSpec   *pspec)
{
	CpgIntegratorState *self = CPG_INTEGRATOR_STATE (object);

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
cpg_integrator_state_get_property (GObject    *object,
                                   guint       prop_id,
                                   GValue     *value,
                                   GParamSpec *pspec)
{
	CpgIntegratorState *self = CPG_INTEGRATOR_STATE (object);

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
cpg_integrator_state_class_init (CpgIntegratorStateClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_integrator_state_finalize;
	object_class->dispose = cpg_integrator_state_dispose;

	object_class->get_property = cpg_integrator_state_get_property;
	object_class->set_property = cpg_integrator_state_set_property;

	g_type_class_add_private (object_class, sizeof (CpgIntegratorStatePrivate));

	/**
	 * CpgIntegratorState::updated:
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
	 * CpgIntegratorState:object:
	 *
	 * The object which is integrated
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_integrator_state_init (CpgIntegratorState *self)
{
	self->priv = CPG_INTEGRATOR_STATE_GET_PRIVATE (self);
}

/**
 * cpg_integrator_state_new:
 * @object: A #CpgObject
 *
 * Create a new integrator state for the given object.
 *
 * Returns: A #CpgIntegratorState
 *
 **/
CpgIntegratorState *
cpg_integrator_state_new (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return g_object_new (CPG_TYPE_INTEGRATOR_STATE,
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
collect_link (CpgIntegratorState *state,
              CpgLink            *link)
{
	GSList const *actions = cpg_link_get_actions (link);

	while (actions)
	{
		CpgLinkAction *action = actions->data;

		CpgProperty *target = cpg_link_action_get_target_property (action);

		if (cpg_property_get_integrated (target))
		{
			state->priv->integrated_link_actions =
				prepend_gslist_unique (state->priv->integrated_link_actions,
				                       action);
		}
		else
		{
			state->priv->direct_link_actions =
				prepend_gslist_unique (state->priv->direct_link_actions,
				                       action);
		}

		actions = g_slist_next (actions);
	}
}

static void
collect_actors (CpgIntegratorState *state,
                CpgObject          *object)
{
	GSList const *actors = cpg_object_get_actors (object);

	while (actors)
	{
		CpgProperty *property = actors->data;

		if (cpg_property_get_integrated (property))
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
collect_properties (CpgIntegratorState *state,
                    CpgObject          *object)
{
	GSList const *props;

	for (props = cpg_object_get_properties (object); props; props = g_slist_next (props))
	{
		state->priv->all_properties =
			prepend_gslist_unique (state->priv->all_properties,
			                       props->data);
	}
}

static void
collect_states (CpgIntegratorState *state,
                CpgObject          *object)
{
	collect_properties (state, object);

	if (CPG_IS_LINK (object))
	{
		collect_link (state, CPG_LINK (object));
		return;
	}

	collect_actors (state, object);

	if (CPG_IS_INPUT (object))
	{
		state->priv->inputs =
			prepend_gslist_unique (state->priv->inputs,
			                       object);
	}

	if (CPG_IS_GROUP (object))
	{
		GSList const *children = cpg_group_get_children (CPG_GROUP (object));

		while (children)
		{
			collect_states (state, children->data);
			children = g_slist_next (children);
		}
	}
}

static gint
link_action_compare (CpgLinkAction *a,
                     CpgLinkAction *b)
{
	/* if a depends on b => 1
	   if b depends on a => -1
	   else 0 */
	if (cpg_link_action_depends (a, cpg_link_action_get_target_property (b)))
	{
		return 1;
	}
	else if (cpg_link_action_depends (b, cpg_link_action_get_target_property (a)))
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

static void
sort_link_actions (CpgIntegratorState *state)
{
	state->priv->direct_link_actions =
		g_slist_sort (state->priv->direct_link_actions,
		              (GCompareFunc)link_action_compare);
}

static void
collect_operators (CpgInstructionCustomOperator *instruction,
                   CpgIntegratorState           *state)
{
	state->priv->operators =
		g_slist_prepend (state->priv->operators,
		                 cpg_instruction_custom_operator_get_operator (instruction));
}

static void
collect_expressions (CpgExpression      *expression,
                     CpgIntegratorState *state)
{
	state->priv->expressions =
		g_slist_prepend (state->priv->expressions, expression);

	g_slist_foreach ((GSList *)cpg_expression_get_operators (expression),
	                 (GFunc)collect_operators,
	                 state);
}

/**
 * cpg_integrator_state_update:
 * @state: A #CpgIntegratorState
 *
 * Update the integrator state. This recursively goes through all the objects
 * contained in the associated #CpgIntegratorState:object and collects the
 * links and properties that need to be integrated.
 *
 **/
void
cpg_integrator_state_update (CpgIntegratorState *state)
{
	g_return_if_fail (CPG_IS_INTEGRATOR_STATE (state));

	clear_lists (state);

	collect_states (state, CPG_OBJECT (state->priv->object));

	state->priv->all_properties =
		g_slist_reverse (state->priv->all_properties);

	state->priv->integrated_properties =
		g_slist_reverse (state->priv->integrated_properties);

	state->priv->direct_properties =
		g_slist_reverse (state->priv->direct_properties);

	state->priv->inputs =
		g_slist_reverse (state->priv->inputs);

	/* order the direct link actions based on their dependencies */
	sort_link_actions (state);

	cpg_object_foreach_expression (CPG_OBJECT (state->priv->object),
	                               (CpgForeachExpressionFunc)collect_expressions,
	                               state);

	state->priv->expressions =
		g_slist_reverse (state->priv->expressions);

	g_signal_emit (state, signals[UPDATED], 0);
}

/**
 * cpg_integrator_state_integrated_properties:
 * @state: A #CpgIntegratorState
 *
 * Get the integrated properties which are acted upon by links.
 *
 * Returns: (element-type CpgProperty) (transfer none): A #GSList of #CpgProperty
 *
 **/
const GSList *
cpg_integrator_state_integrated_properties (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->integrated_properties;
}

/**
 * cpg_integrator_state_direct_properties:
 * @state: A #CpgIntegratorState
 *
 * Get non-integrated properties which are acted upon by links.
 *
 * Returns: (element-type CpgProperty) (transfer none): A #GSList of #CpgProperty
 *
 **/
const GSList *
cpg_integrator_state_direct_properties (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->direct_properties;
}

/**
 * cpg_integrator_state_integrated_link_actions:
 * @state: A #CpgIntegratorState
 *
 * Get the link actions that act on integrated properties.
 *
 * Returns: (element-type CpgLinkAction) (transfer none): A #GSList of #CpgLinkAction
 *
 **/
const GSList *
cpg_integrator_state_integrated_link_actions (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->integrated_link_actions;
}

/**
 * cpg_integrator_state_direct_link_actions:
 * @state: A #CpgIntegratorState
 *
 * Get the link actions that act on non-integrated properties.
 *
 * Returns: (element-type CpgLinkAction) (transfer none): A #GSList of #CpgLinkAction
 *
 **/
const GSList *
cpg_integrator_state_direct_link_actions (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->direct_link_actions;
}

/**
 * cpg_integrator_state_all_properties:
 * @state: A #CpgIntegratorState
 *
 * Get the link actions that act on non-integrated properties.
 *
 * Returns: (element-type CpgProperty) (transfer none): A #GSList of #CpgProperty
 *
 **/
const GSList *
cpg_integrator_state_all_properties (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);
	return state->priv->all_properties;
}

/**
 * cpg_integrator_state_inputs:
 * @state: A #CpgIntegratorState
 *
 * Get the input states.
 *
 * Returns: (element-type CpgInput) (transfer none): A #GSList of #CpgInput
 *
 **/
const GSList *
cpg_integrator_state_inputs (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);

	return state->priv->inputs;
}

/**
 * cpg_integrator_state_expressions:
 * @state: A #CpgIntegratorState
 *
 * Get the expressions in the network.
 *
 * Returns: (element-type CpgExpression) (transfer none): A #GSList of #CpgExpression
 *
 **/
const GSList *
cpg_integrator_state_expressions (CpgIntegratorState *state)
{
	return state->priv->expressions;
}

/**
 * cpg_integrator_state_get_object:
 * @state: A #CpgIntegratorState
 *
 * Get the object of the integrator state.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_integrator_state_get_object (CpgIntegratorState *state)
{
	g_return_val_if_fail (CPG_IS_INTEGRATOR_STATE (state), NULL);

	return state->priv->object;
}

const GSList *
cpg_integrator_state_operators (CpgIntegratorState *state)
{
	return state->priv->operators;
}
