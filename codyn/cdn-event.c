#include "cdn-event.h"
#include "cdn-enum-types.h"
#include "cdn-compile-error.h"
#include "cdn-phaseable.h"
#include "cdn-math.h"
#include "instructions/cdn-instruction-function.h"
#include "cdn-expression-tree-iter.h"

#include <math.h>

/**
 * CdnEvent:
 *
 * Event class.
 *
 * The #CdnEvent class is a #CdnNode derivative defining node state transitions
 * under specified conditions (see #cdn_event_set_condition). Each simulation
 * step, all event conditions are checked and when the condition goes from
 * false to true, the event is executed (see #cdn_event_execute).
 *
 * Events can be active only in certain states (see #CdnPhaseable), and can
 * transition states when they are executed (see #cdn_event_set_goto_state).
 * Note that these states are transitioned on the _parent_ #CdnNode, such that
 * multiple event objects can affect the same state. Additionally, states can
 * set variables to certain values when fired (see #cdn_event_add_set_variable).
 *
 * Finally, the occurence of events can be refined by the integrator. When
 * the event approximation (#cdn_event_set_approximation) is set, then the
 * time at which the event occurs will be refined beyond the specified
 * integration time step, until the error of the condition going from false to
 * true is within the specified approximation.
 *
 */

#define CDN_EVENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EVENT, CdnEventPrivate))

typedef CdnEventLogicalNode LogicalNode;

struct _CdnEventLogicalNode
{
	CdnMathFunctionType type;

	// Only for non-terminal nodes
	LogicalNode *left;
	LogicalNode *right;

	// Only for terminal nodes
	CdnExpression *expression;

	gdouble last_distance;
	gdouble value;
};

struct _CdnEventPrivate
{
	CdnExpression *condition;
	LogicalNode *condition_node;

	gdouble approximation;

	GSList *set_variables;
	GHashTable *states;

	gchar *goto_state;

	guint terminal : 1;
};

struct _CdnEventSetVariable
{
	CdnVariable *variable;
	CdnExpression *value;
};

typedef CdnEventSetVariable SetVariable;

static void cdn_phaseable_iface_init (gpointer iface);

static gboolean logical_node_happened (CdnEvent    *event,
                                       LogicalNode *node,
                                       gdouble     *retval,
                                       gboolean     update);

G_DEFINE_TYPE_WITH_CODE (CdnEvent,
                         cdn_event,
                         CDN_TYPE_NODE,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_PHASEABLE,
                                                cdn_phaseable_iface_init))

enum
{
	PROP_0,
	PROP_CONDITION,
	PROP_APPROXIMATION,
	PROP_GOTO_STATE,
	PROP_TERMINAL
};

static LogicalNode *
logical_node_copy (LogicalNode *node,
                   gboolean     copy_expr)
{
	LogicalNode *ret;

	if (node == NULL)
	{
		return NULL;
	}

	ret = g_slice_new0 (LogicalNode);

	ret->left = logical_node_copy (node->left, copy_expr);
	ret->right = logical_node_copy (node->right, copy_expr);

	ret->type = node->type;

	if (node->expression)
	{
		if (copy_expr)
		{
			ret->expression = cdn_expression_copy (node->expression);
		}
		else
		{
			ret->expression = g_object_ref (node->expression);
		}
	}

	ret->value = node->value;
	ret->last_distance = node->last_distance;

	return ret;
}

static LogicalNode *
logical_node_copy_shallow (LogicalNode *node)
{
	return logical_node_copy (node, FALSE);
}

static void
logical_node_free (LogicalNode *node)
{
	if (!node)
	{
		return;
	}

	logical_node_free (node->left);
	logical_node_free (node->right);

	if (node->expression)
	{
		g_object_unref (node->expression);
	}

	g_slice_free (LogicalNode, node);
}

static LogicalNode *
logical_node_new (CdnEvent              *event,
                  CdnExpressionTreeIter *iter,
                  CdnCompileError       *error)
{
	LogicalNode *node = NULL;
	CdnInstruction *instr;

	instr = cdn_expression_tree_iter_get_instruction (iter);

	if (CDN_IS_INSTRUCTION_FUNCTION (instr))
	{
		CdnStackManipulation const *smanip;
		CdnMathFunctionType type;

		type = cdn_instruction_function_get_id (CDN_INSTRUCTION_FUNCTION (instr));
		smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

		switch (type)
		{
		case CDN_MATH_FUNCTION_TYPE_AND:
		case CDN_MATH_FUNCTION_TYPE_OR:
		{
			LogicalNode *left;
			LogicalNode *right;

			// Branch!
			left = logical_node_new (event,
			                         cdn_expression_tree_iter_get_child (iter, 0),
			                         error);

			if (!left)
			{
				return NULL;
			}

			right = logical_node_new (event,
			                          cdn_expression_tree_iter_get_child (iter, 1),
			                          error);

			if (!right)
			{
				logical_node_free (left);
				return NULL;
			}

			node = g_slice_new0 (LogicalNode);

			node->type = type;
			node->left = left;
			node->right = right;
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		{
			CdnInstruction *newinstr;

			// Compute with minus, but swap arguments (and therefore dims)
			CdnStackArg arg[2] = {
				smanip->pop.args[1],
				smanip->pop.args[0],
			};

			CdnStackArgs args = {
				.num = 2,
				.args = arg,
			};

			newinstr = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS,
			                                         NULL,
			                                         &args);

			cdn_expression_tree_iter_set_instruction (iter, newinstr);
			cdn_expression_tree_iter_swap_children (iter, 0, 1);

			node = g_slice_new0 (LogicalNode);
			node->type = type;
			node->expression = cdn_expression_tree_iter_to_expression (iter);

			cdn_mini_object_unref (newinstr);
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
		{
			CdnInstruction *newinstr;

			// Compute with minus
			newinstr = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS,
			                                         NULL,
			                                         &smanip->pop);

			cdn_expression_tree_iter_set_instruction (iter, newinstr);

			node = g_slice_new0 (LogicalNode);
			node->type = type;

			node->expression = cdn_expression_tree_iter_to_expression (iter);

			cdn_mini_object_unref (newinstr);
		}
		break;
		default:
		break;
		}
	}

	if (!node && error && !cdn_compile_error_get_error (error))
	{
		GError *err;

		err = g_error_new (CDN_COMPILE_ERROR_TYPE,
		                   CDN_COMPILE_ERROR_INVALID_STACK,
		                   "The provided condition can only be composed of logical operators, got `%s'",
		                   cdn_expression_tree_iter_to_string (iter));

		cdn_compile_error_set (error,
		                       err,
		                       CDN_OBJECT (event),
		                       NULL,
		                       NULL,
		                       event->priv->condition);
	}

	return node;
}

static gboolean
condition_holds (CdnEvent    *event,
                 LogicalNode *node,
                 gdouble      val)
{
	switch (node->type)
	{
	case CDN_MATH_FUNCTION_TYPE_LESS:
	case CDN_MATH_FUNCTION_TYPE_GREATER:
		return val > 0;
	break;
	case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
	case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		return val >= 0;
	break;
	case CDN_MATH_FUNCTION_TYPE_AND:
	case CDN_MATH_FUNCTION_TYPE_OR:
		return val > 0.5;
	break;
	case CDN_MATH_FUNCTION_TYPE_EQUAL:
		return fabs (val) <= event->priv->approximation;
	default:
	break;
	}

	return FALSE;
}

static gboolean
logical_node_happened_and (CdnEvent    *event,
                           LogicalNode *node,
                           gdouble     *retval,
                           gboolean     update)
{
	gdouble lval;
	gdouble rval;
	gboolean lhap;
	gboolean rhap;

	// At least left OR right need to have happened and
	// if so, the other should at least be TRUE now
	lhap = logical_node_happened (event, node->left, &lval, update);
	rhap = logical_node_happened (event, node->right, &rval, update);

	if (retval)
	{
		*retval = 1;
	}

	if (update)
	{
		node->value = 1;
	}

	node->last_distance = 0;

	if (lhap && rhap)
	{
		// Both happened, use worst case as distance
		node->last_distance = MAX(node->left->last_distance,
		                          node->right->last_distance);

		return TRUE;
	}
	else if (lhap && condition_holds (event, node->right, rval))
	{
		node->last_distance = node->left->last_distance;
		return TRUE;
	}
	else if (rhap && condition_holds (event, node->left, lval))
	{
		node->last_distance = node->right->last_distance;
		return TRUE;
	}
	else
	{
		if (retval)
		{
			*retval = 0;
		}

		if (update)
		{
			node->value = 0;
		}
	}

	return FALSE;
}

static gboolean
logical_node_happened_or (CdnEvent    *event,
                          LogicalNode *node,
                          gdouble     *retval,
                          gboolean     update)
{
	gdouble lval;
	gdouble rval;
	gboolean lhap;
	gboolean rhap;

	// At least left OR right need to have happened
	lhap = logical_node_happened (event, node->left, &lval, update);
	rhap = logical_node_happened (event, node->right, &rval, update);

	if (retval)
	{
		*retval = 1;
	}

	if (update)
	{
		node->value = 1;
	}

	node->last_distance = 0;

	if (lhap && rhap)
	{
		node->last_distance = MAX(node->left->last_distance,
		                          node->right->last_distance);

		return TRUE;
	}
	else if (lhap)
	{
		node->last_distance = node->left->last_distance;
		return TRUE;
	}
	else if (rhap)
	{
		node->last_distance = node->right->last_distance;
		return TRUE;
	}
	else
	{
		if (retval)
		{
			*retval = 0;
		}

		if (update)
		{
			node->value = 0;
		}
	}

	return FALSE;
}

static gboolean
logical_node_happened (CdnEvent    *event,
                       LogicalNode *node,
                       gdouble     *retval,
                       gboolean     update)
{
	gdouble val = 0;

	if (node->type == CDN_MATH_FUNCTION_TYPE_AND)
	{
		return logical_node_happened_and (event, node, retval, update);
	}
	else if (node->type == CDN_MATH_FUNCTION_TYPE_OR)
	{
		return logical_node_happened_or (event, node, retval, update);
	}

	val = cdn_expression_evaluate (node->expression);

	if (retval)
	{
		*retval = val;
	}

	if (update)
	{
		node->value = val;
	}

	node->last_distance = -1;

	switch (node->type)
	{
	case CDN_MATH_FUNCTION_TYPE_LESS:
	case CDN_MATH_FUNCTION_TYPE_GREATER:
	case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
	case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
	case CDN_MATH_FUNCTION_TYPE_EQUAL:
		if (condition_holds (event, node, node->value) ||
		    !condition_holds (event, node, val))
		{
			return FALSE;
		}
	break;
	default:
	break;
	}

	if (node->type == CDN_MATH_FUNCTION_TYPE_EQUAL)
	{
		node->last_distance = 1;
	}
	else
	{
		if (val <= event->priv->approximation)
		{
			node->last_distance = 1;
		}
		else
		{
			// Distance is measured relative to the change occuring
			// between two steps (i.e. a fraction)
			node->last_distance = node->value / (node->value - val);

			if (node->type == CDN_MATH_FUNCTION_TYPE_LESS ||
			    node->type == CDN_MATH_FUNCTION_TYPE_GREATER)
			{
				node->last_distance += 1e-10;
			}
		}
	}

	return node->last_distance >= 0;
}

static void
logical_node_update (CdnEvent    *event,
                     LogicalNode *node)
{
	logical_node_happened (event,
	                       node,
	                       NULL,
	                       TRUE);
}

static SetVariable *
set_variable_new (CdnVariable   *property,
                  CdnExpression *expression)
{
	SetVariable *ret;

	ret = g_slice_new0 ( SetVariable);
	ret->variable = g_object_ref_sink (property);
	ret->value = g_object_ref_sink (expression);

	return ret;
}

static SetVariable *
set_variable_copy (SetVariable *v)
{
	return set_variable_new (v->variable, v->value);
}

static void
set_variable_free (SetVariable *self)
{
	g_object_unref (self->variable);
	g_object_unref (self->value);

	g_slice_free (SetVariable, self);
}

static GHashTable *
cdn_phaseable_get_phase_table_impl (CdnPhaseable *phaseable)
{
	return CDN_EVENT (phaseable)->priv->states;
}

static void
cdn_phaseable_set_phase_table_impl (CdnPhaseable *phaseable,
                                    GHashTable   *table)
{
	CdnEvent *ev;

	ev = CDN_EVENT (phaseable);

	if (ev->priv->states)
	{
		g_hash_table_unref (ev->priv->states);
		ev->priv->states = NULL;
	}

	if (table)
	{
		ev->priv->states = table;
		g_hash_table_ref (table);
	}
}

static void
cdn_phaseable_iface_init (gpointer iface)
{
	CdnPhaseableInterface *phaseable;

	phaseable = iface;

	phaseable->get_phase_table = cdn_phaseable_get_phase_table_impl;
	phaseable->set_phase_table = cdn_phaseable_set_phase_table_impl;
}

static void
cdn_event_finalize (GObject *object)
{
	CdnEvent *self;

	self = CDN_EVENT (object);

	g_free (self->priv->goto_state);

	g_slist_foreach (self->priv->set_variables,
	                 (GFunc)set_variable_free,
	                 NULL);

	g_slist_free (self->priv->set_variables);

	if (self->priv->states)
	{
		g_hash_table_destroy (self->priv->states);
	}

	if (self->priv->condition)
	{
		g_object_unref (self->priv->condition);
	}

	logical_node_free (self->priv->condition_node);

	G_OBJECT_CLASS (cdn_event_parent_class)->finalize (object);
}

static void
set_condition (CdnEvent      *event,
               CdnExpression *condition)
{
	if (event->priv->condition == condition)
	{
		return;
	}

	if (event->priv->condition)
	{
		g_object_unref (event->priv->condition);
		event->priv->condition = NULL;
	}

	if (condition)
	{
		event->priv->condition = g_object_ref_sink (condition);
	}
}

static void
cdn_event_set_property (GObject      *object,
                        guint         prop_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
	CdnEvent *self = CDN_EVENT (object);

	switch (prop_id)
	{
		case PROP_CONDITION:
			set_condition (self, g_value_get_object (value));
			break;
		case PROP_APPROXIMATION:
			self->priv->approximation = g_value_get_double (value);
			break;
		case PROP_GOTO_STATE:
			g_free (self->priv->goto_state);
			self->priv->goto_state = g_value_dup_string (value);
			break;
		case PROP_TERMINAL:
			self->priv->terminal = g_value_get_boolean (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_event_get_property (GObject    *object,
                        guint       prop_id,
                        GValue     *value,
                        GParamSpec *pspec)
{
	CdnEvent *self = CDN_EVENT (object);

	switch (prop_id)
	{
		case PROP_CONDITION:
			g_value_set_object (value, self->priv->condition);
			break;
		case PROP_APPROXIMATION:
			g_value_set_double (value, self->priv->approximation);
			break;
		case PROP_GOTO_STATE:
			g_value_set_string (value, self->priv->goto_state);
			break;
		case PROP_TERMINAL:
			g_value_set_boolean (value, self->priv->terminal);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
extract_condition_parts (CdnEvent        *event,
                         CdnCompileError *error)
{
	LogicalNode *node;
	CdnExpressionTreeIter *iter;

	iter = cdn_expression_tree_iter_new (event->priv->condition);
	node = logical_node_new (event, iter, error);

	if (!node)
	{
		return FALSE;
	}

	event->priv->condition_node = node;
	return TRUE;
}

static gboolean
cdn_event_compile_impl (CdnObject         *object,
                        CdnCompileContext *context,
                        CdnCompileError   *error)
{
	GSList *item;
	CdnEvent *event;

	event = CDN_EVENT (object);

	if (cdn_object_is_compiled (object))
	{
		return TRUE;
	}

	if (!CDN_OBJECT_CLASS (cdn_event_parent_class)->compile (object, context, error))
	{
		return FALSE;
	}

	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	context = CDN_OBJECT_CLASS (cdn_event_parent_class)->get_compile_context (object, context);

	if (!cdn_expression_compile (event->priv->condition, context, error))
	{
		cdn_compile_context_restore (context);
		g_object_unref (context);
		return FALSE;
	}

	cdn_compile_context_restore (context);
	g_object_unref (context);

	if (!extract_condition_parts (event, error))
	{
		return FALSE;
	}

	for (item = event->priv->set_variables; item; item = g_slist_next (item))
	{
		SetVariable *p = item->data;
		CdnCompileContext *ctx;

		ctx = cdn_object_get_compile_context (cdn_variable_get_object (p->variable),
		                                      NULL);

		// Add the event object in the context as well
		CDN_OBJECT_CLASS (cdn_event_parent_class)->get_compile_context (object, ctx);

		if (!cdn_expression_compile (p->value, ctx, error))
		{
			g_object_unref (ctx);
			return FALSE;
		}

		g_object_unref (ctx);
	}

	return TRUE;
}

static CdnVariable *
lookup_set_variable (CdnNode     *n,
                     gchar const *name)
{
	while (n)
	{
		CdnVariable *ret;

		ret = cdn_object_get_variable (CDN_OBJECT (n), name);

		if (ret != NULL)
		{
			return ret;
		}

		n = cdn_object_get_parent (CDN_OBJECT (n));
	}

	return NULL;
}

static void
do_copy (CdnEvent *e,
         CdnEvent *se)
{
	GSList *item;

	if (e->priv->condition)
	{
		g_object_unref (e->priv->condition);
		e->priv->condition = NULL;
	}

	if (e->priv->condition_node)
	{
		logical_node_free (e->priv->condition_node);
		e->priv->condition_node = NULL;
	}

	if (e->priv->goto_state)
	{
		g_free (e->priv->goto_state);
		e->priv->goto_state = NULL;
	}

	g_slist_foreach (e->priv->set_variables, (GFunc)set_variable_free, NULL);
	g_slist_free (e->priv->set_variables);
	e->priv->set_variables = NULL;

	/* Copy expression */
	if (se->priv->condition)
	{
		e->priv->condition = cdn_expression_copy (se->priv->condition);
	}

	/* Copy logical nodes. */
	e->priv->condition_node = logical_node_copy (se->priv->condition_node,
	                                             TRUE);

	/* Copy set variables. */
	for (item = se->priv->set_variables; item; item = g_slist_next (item))
	{
		CdnVariable *v;
		SetVariable *sv;

		sv = item->data;

		v = lookup_set_variable (cdn_object_get_parent (CDN_OBJECT (e)),
		                         cdn_variable_get_name (sv->variable));

		if (v)
		{
			e->priv->set_variables =
				g_slist_prepend (e->priv->set_variables,
				                 set_variable_new (v,
				                                   cdn_expression_copy (sv->value)));
		}
	}

	e->priv->set_variables = g_slist_reverse (e->priv->set_variables);

	e->priv->goto_state = g_strdup (se->priv->goto_state);
	e->priv->approximation = se->priv->approximation;
	e->priv->terminal = se->priv->terminal;

	cdn_phaseable_copy_to (CDN_PHASEABLE (se), CDN_PHASEABLE (e));
}

static void
cdn_event_copy_impl (CdnObject *object,
                     CdnObject *source)
{
	CdnEvent *e = CDN_EVENT (object);
	CdnEvent *se = CDN_EVENT (source);

	CDN_OBJECT_CLASS (cdn_event_parent_class)->copy (object, source);

	do_copy (e, se);
}

static gboolean
cdn_event_apply_template_impl (CdnObject  *object,
                               CdnObject  *templ,
                               GError    **error)
{
	/* Apply condition */
	CdnEvent *e;
	CdnEvent *se;

	if (!CDN_OBJECT_CLASS (cdn_event_parent_class)->apply_template (object, templ, error))
	{
		return FALSE;
	}

	e = CDN_EVENT (object);
	se = CDN_EVENT (templ);

	do_copy (e, se);
	cdn_object_taint (object);

	return TRUE;
}

static void
cdn_event_class_init (CdnEventClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_object_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_event_finalize;

	object_class->get_property = cdn_event_get_property;
	object_class->set_property = cdn_event_set_property;

	cdn_object_class->compile = cdn_event_compile_impl;
	cdn_object_class->copy = cdn_event_copy_impl;
	cdn_object_class->apply_template = cdn_event_apply_template_impl;

	g_type_class_add_private (object_class, sizeof(CdnEventPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_CONDITION,
	                                 g_param_spec_object ("condition",
	                                                      "Condition",
	                                                      "Condition",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_APPROXIMATION,
	                                 g_param_spec_double ("approximation",
	                                                      "Approximation",
	                                                      "Approximation",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_GOTO_STATE,
	                                 g_param_spec_string ("goto-state",
	                                                      "Goto State",
	                                                      "Goto State",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_TERMINAL,
	                                 g_param_spec_boolean ("terminal",
	                                                       "Terminal",
	                                                       "Terminal",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT | G_PARAM_STATIC_STRINGS));
}

static void
cdn_event_init (CdnEvent *self)
{
	self->priv = CDN_EVENT_GET_PRIVATE (self);
}

/**
 * cdn_event_new:
 * @id: the event node id.
 * @condition: the event condition.
 * @approximation: the event approximation.
 *
 * Create a new #CdnEvent. The specified @id is simply the #CdnObject id
 * (note that #CdnEvent derives from #CdnNode). The @condition expression should
 * be a logical expression indicating when the event is triggered.
 *
 * Set @approximation to the maximum allowed error (positive) on the condition
 * for which the event should be triggered. A value of -1 specifies the maximum
 * error and will not refine the time at which the event occured.
 *
 * Returns: a new #CdnEvent.
 *
 **/
CdnEvent *
cdn_event_new (gchar const   *id,
               CdnExpression *condition,
               gdouble        approximation)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (condition), NULL);

	if (approximation < 0)
	{
		approximation = G_MAXDOUBLE;
	}

	return g_object_new (CDN_TYPE_EVENT,
	                     "id",
	                     id,
	                     "condition",
	                     condition,
	                     "approximation",
	                     approximation,
	                     NULL);
}

/**
 * cdn_event_update:
 * @event: the #CdnEvent.
 *
 * Update the event condition value. This is normally called by the integrator
 * and should not be called manually.
 *
 **/
void
cdn_event_update (CdnEvent *event)
{
	logical_node_update (event, event->priv->condition_node);
}

/**
 * cdn_event_get_condition:
 * @event: A #CdnEvent
 *
 * Get the event condition.
 *
 * Returns: (transfer none): A #CdnExpression.
 *
 **/
CdnExpression *
cdn_event_get_condition (CdnEvent *event)
{
	return event->priv->condition;
}

/**
 * cdn_event_get_approximation:
 * @event: the #CdnEvent.
 *
 * Get the maximum error on the condition allowed when the event should be
 * triggered.
 *
 * Returns: the approximation.
 *
 **/
gdouble
cdn_event_get_approximation (CdnEvent *event)
{
	return event->priv->approximation;
}

/**
 * cdn_event_set_condition:
 * @event: the #CdnEvent.
 * @condition: the condition.
 *
 * Set the condition at which the event should be triggered. This condition
 * must be a logical expression.
 *
 */
void
cdn_event_set_condition (CdnEvent      *event,
                         CdnExpression *condition)
{
	g_return_if_fail (CDN_IS_EVENT (event));
	g_return_if_fail (condition == NULL || CDN_IS_EXPRESSION (condition));

	set_condition (event, condition);
}

/**
 * cdn_event_set_approximation:
 * @event: the #CdnEvent.
 * @approximation: the approximation value.
 *
 * Set the maximum error on the condition allowed when the event should be
 * triggered. If do not want the event to be refined, then use the special
 * value -1 for @approximation.
 *
 **/
void
cdn_event_set_approximation (CdnEvent *event,
                             gdouble   approximation)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	if (approximation == -1)
	{
		approximation = G_MAXDOUBLE;
	}

	if (event->priv->approximation != approximation)
	{
		event->priv->approximation = approximation;
		g_object_notify (G_OBJECT (event), "approximation");
	}
}

/**
 * cdn_event_last_distance:
 * @event: the #CdnEvent.
 *
 * Get the last computed distance towards triggering the condition.
 *
 * Returns: the last condition distance.
 *
 **/
gdouble
cdn_event_last_distance (CdnEvent *event)
{
	return event->priv->condition_node->last_distance;
}

/**
 * cdn_event_happened:
 * @event: the #CdnEvent.
 * @dist: (allow-none): return value for the condition distance.
 *
 * Check whether the event has happened in the last timestep. If the event
 * has happened, and @dist is not %NULL, then the error on the event condition
 * is returned in @dist.
 *
 * Returns: %TRUE if the event happened, %FALSE otherwise.
 *
 **/
gboolean
cdn_event_happened (CdnEvent *event,
                    gdouble  *dist)
{
	gboolean ret;

	ret = logical_node_happened (event,
	                             event->priv->condition_node,
	                             NULL,
	                             FALSE);

	if (ret && dist)
	{
		*dist = event->priv->condition_node->last_distance;
	}

	return ret;
}

/**
 * cdn_event_add_set_variable:
 * @event: the #CdnEvent.
 * @variable: the #CdnVariable to set.
 * @value: the expression to set @variable to.
 *
 * Add an action to set @variable to @value when the event is triggered.
 *
 **/
void
cdn_event_add_set_variable (CdnEvent      *event,
                            CdnVariable   *variable,
                            CdnExpression *value)
{
	SetVariable *p;

	g_return_if_fail (CDN_IS_EVENT (event));
	g_return_if_fail (CDN_IS_VARIABLE (variable));
	g_return_if_fail (CDN_IS_EXPRESSION (value));

	p = set_variable_new (variable, value);

	event->priv->set_variables = g_slist_append (event->priv->set_variables,
	                                              p);
}

/**
 * cdn_event_execute:
 * @event: the #CdnEvent.
 *
 * Execute the event. This will set all the variables added with
 * #cdn_event_add_set_variable to their corresponding values. Note that this
 * is normally called by the integrator and does not need to be manually called.
 *
 */
void
cdn_event_execute (CdnEvent *event)
{
	GSList *setprop;
	GSList *vals = NULL;

	for (setprop = event->priv->set_variables; setprop; setprop = g_slist_next (setprop))
	{
		SetVariable *p = setprop->data;

		vals = g_slist_prepend (vals,
		                        (gpointer)cdn_expression_evaluate_values (p->value));
	}

	vals = g_slist_reverse (vals);

	for (setprop = event->priv->set_variables; setprop; setprop = g_slist_next (setprop))
	{
		SetVariable *p = setprop->data;

		cdn_variable_set_values (p->variable, vals->data);

		vals = g_slist_delete_link (vals, vals);
	}
}

/**
 * cdn_event_set_goto_state:
 * @event: the #CdnEvent.
 * @state: the state to transition to.
 *
 * Sets the state to which the parent node transitions when this event
 * is triggered.
 *
 */
void
cdn_event_set_goto_state (CdnEvent           *event,
                          gchar const        *state)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	g_free (event->priv->goto_state);
	event->priv->goto_state = g_strdup (state);

	g_object_notify (G_OBJECT (event), "goto-state");
}

/**
 * cdn_event_get_goto_state:
 * @event: the #CdnEvent.
 *
 * Get the state to which the parent node transitions when the event is
 * triggered. Note that this can return %NULL if the event doesn't cause a
 * state transition.
 *
 * Returns: the state to which the event transitions the parent node when triggered.
 *
 **/
gchar const *
cdn_event_get_goto_state (CdnEvent *event)
{
	g_return_val_if_fail (CDN_IS_EVENT (event), NULL);

	return event->priv->goto_state;
}

/**
 * cdn_event_set_terminal:
 * @event: the #CdnEvent.
 *
 * Set whether the event is terminal. Terminal events cause the integration
 * to stop when triggered.
 *
 **/
void
cdn_event_set_terminal (CdnEvent *event,
                        gboolean  terminal)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	if (event->priv->terminal != terminal)
	{
		event->priv->terminal = terminal;

		g_object_notify (G_OBJECT (event), "terminal");
	}
}

/**
 * cdn_event_get_terminal:
 * @event: the #CdnEvent.
 *
 * Get whether the event is terminal.
 *
 * Returns: %TRUE if the event is terminal, %FALSE otherwise.
 *
 **/
gboolean
cdn_event_get_terminal (CdnEvent *event)
{
	return event->priv->terminal;
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_event_logical_node_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnEventLogicalNode",
		                                      (GBoxedCopyFunc)logical_node_copy_shallow,
		                                      (GBoxedFreeFunc)logical_node_free);
	}

	return gtype;
}

/**
 * cdn_event_get_logical_tree:
 * @event: a #CdnEvent.
 *
 * Get the logical tree representation of the event condition.
 *
 * Returns: (transfer none): a #CdnEventLogicalNode.
 *
 **/
CdnEventLogicalNode const *
cdn_event_get_logical_tree (CdnEvent *event)
{
	g_return_val_if_fail (CDN_IS_EVENT (event), NULL);

	return event->priv->condition_node;
}

/**
 * cdn_event_logical_node_get_left:
 * @node: a #CdnEventLogicalNode.
 *
 * Get the left hand side node of the logical node. This is only non %NULL for
 * nodes representing logical AND and OR.
 *
 * Returns: (transfer none): a #CdnEventLogicalNode.
 *
 **/
CdnEventLogicalNode const *
cdn_event_logical_node_get_left (CdnEventLogicalNode const *node)
{
	g_return_val_if_fail (node != NULL, NULL);

	return node->left;
}

/**
 * cdn_event_logical_node_get_right:
 * @node: a #CdnEventLogicalNode.
 *
 * Get the right hand side node of the logical node. This is only non %NULL for
 * nodes representing logical AND and OR.
 *
 * Returns: (transfer none): a #CdnEventLogicalNode.
 *
 **/
CdnEventLogicalNode const *
cdn_event_logical_node_get_right (CdnEventLogicalNode const *node)
{
	g_return_val_if_fail (node != NULL, NULL);

	return node->right;
}

/**
 * cdn_event_logical_node_get_compare_type:
 * @node: a #CdnEventLogicalNode.
 *
 * Get the comparison type represented by the node.
 *
 * Returns: a #CdnMathFunctionType.
 *
 **/
CdnMathFunctionType
cdn_event_logical_node_get_compare_type (CdnEventLogicalNode const *node)
{
	g_return_val_if_fail (node != NULL, 0);

	return node->type;
}

/**
 * cdn_event_logical_node_get_expression:
 * @node: a #CdnEventLogicalNode.
 *
 * Get the expression of the logical node.
 *
 * Returns: (transfer none): a #CdnExpression.
 *
 **/
CdnExpression const *
cdn_event_logical_node_get_expression (CdnEventLogicalNode const *node)
{
	g_return_val_if_fail (node != NULL, NULL);

	return node->expression;
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_event_set_variable_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnEventSetVariable",
		                                      (GBoxedCopyFunc)set_variable_copy,
		                                      (GBoxedFreeFunc)set_variable_free);
	}

	return gtype;
}

/**
 * cdn_event_get_set_variables:
 * @event: a #CdnEvent.
 *
 * Get the variables that are modified when the event fires.
 *
 * Returns: (transfer none) (element-type CdnEventSetVariable): a #GSList of #CdnEventSetVariable.
 *
 **/
GSList const *
cdn_event_get_set_variables (CdnEvent *event)
{
	g_return_val_if_fail (CDN_IS_EVENT (event), NULL);

	return event->priv->set_variables;
}

/**
 * cdn_event_set_variable_get_value:
 * @variable: a #CdnEventSetVariable.
 *
 * Get the expression used to update the variable when the event is fired.
 *
 * Returns: (transfer none): a #CdnExpression.
 *
 **/
CdnExpression *
cdn_event_set_variable_get_value (CdnEventSetVariable *variable)
{
	g_return_val_if_fail (variable != NULL, NULL);

	return variable->value;
}

/**
 * cdn_event_set_variable_get_variable:
 * @variable: a #CdnEventSetVariable.
 *
 * Get the variable set when the event is fired.
 *
 * Returns: (transfer none): a #CdnVariable.
 *
 **/
CdnVariable *
cdn_event_set_variable_get_variable (CdnEventSetVariable *variable)
{
	g_return_val_if_fail (variable != NULL, NULL);

	return variable->variable;
}
