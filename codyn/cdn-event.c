#include "cdn-event.h"
#include "cdn-enum-types.h"
#include "cdn-compile-error.h"
#include "cdn-phaseable.h"
#include "cdn-math.h"
#include "instructions/cdn-instruction-function.h"

#include <math.h>

#define CDN_EVENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EVENT, CdnEventPrivate))

struct _CdnEventPrivate
{
	CdnExpression *condition;
	gdouble approximation;
	gdouble value;

	GSList *set_properties;
	GSList *set_flags;

	GSList *events;
	GHashTable *phases;

	gchar *goto_phase;
	CdnMathFunctionType comparison;

	guint terminal : 1;
};

static void cdn_phaseable_iface_init (gpointer iface);

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
	PROP_GOTO_PHASE,
	PROP_TERMINAL
};

typedef struct
{
	CdnVariable *property;
	CdnExpression *value;
} SetProperty;

static  SetProperty *
set_property_new (CdnVariable   *property,
                  CdnExpression *expression)
{
	SetProperty *ret;

	ret = g_slice_new0 ( SetProperty);
	ret->property = g_object_ref_sink (property);
	ret->value = g_object_ref_sink (expression);

	return ret;
}

static void
set_property_free (SetProperty *self)
{
	g_object_unref (self->property);
	g_object_unref (self->value);

	g_slice_free (SetProperty, self);
}

static GHashTable *
cdn_phaseable_get_phase_table_impl (CdnPhaseable *phaseable)
{
	return CDN_EVENT (phaseable)->priv->phases;
}

static void
cdn_phaseable_set_phase_table_impl (CdnPhaseable *phaseable,
                                    GHashTable   *table)
{
	CdnEvent *ev;

	ev = CDN_EVENT (phaseable);

	if (ev->priv->phases)
	{
		g_hash_table_unref (ev->priv->phases);
		ev->priv->phases = NULL;
	}

	if (table)
	{
		ev->priv->phases = table;
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

	g_free (self->priv->goto_phase);

	g_slist_foreach (self->priv->set_properties,
	                 (GFunc)set_property_free,
	                 NULL);

	if (self->priv->phases)
	{
		g_hash_table_destroy (self->priv->phases);
	}

	g_slist_foreach (self->priv->events,
	                 (GFunc)g_object_unref,
	                 NULL);

	g_object_unref (self->priv->condition);

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
		case PROP_GOTO_PHASE:
			g_free (self->priv->goto_phase);
			self->priv->goto_phase = g_value_dup_string (value);
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
		case PROP_GOTO_PHASE:
			g_value_set_string (value, self->priv->goto_phase);
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
extract_condition_parts_error (CdnEvent        *event,
                               CdnCompileError *error)
{
	GError *err;

	err = g_error_new_literal (CDN_COMPILE_ERROR_TYPE,
	                           CDN_COMPILE_ERROR_INVALID_STACK,
	                           "The provided condition does not have a logical comparison");

	cdn_compile_error_set (error,
	                       err,
	                       CDN_OBJECT (event),
	                       NULL,
	                       NULL,
	                       event->priv->condition);

	g_error_free (err);
	return FALSE;
}

static gboolean
extract_condition_parts (CdnEvent        *event,
                         CdnCompileError *error)
{
	GSList const *instructions;
	GSList *last;
	CdnInstructionFunction *instr;
	GSList *ret = NULL;
	CdnMathFunctionType id;
	CdnStackManipulation const *smanip;

	instructions = cdn_expression_get_instructions (event->priv->condition);

	last = g_slist_last ((GSList *)instructions);

	if (!last)
	{
		return extract_condition_parts_error (event, error);
	}

	instr = last->data;

	if (!CDN_IS_INSTRUCTION_FUNCTION (instr))
	{
		return extract_condition_parts_error (event, error);
	}

	id = cdn_instruction_function_get_id (instr);

	switch (id)
	{
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
		break;
		default:
			return extract_condition_parts_error (event, error);
		break;
	}

	smanip = cdn_instruction_get_stack_manipulation (CDN_INSTRUCTION (instr),
	                                                 NULL);

	while (instructions && instructions->next)
	{
		ret = g_slist_prepend (ret,
		                       cdn_mini_object_ref (instructions->data));

		instructions = g_slist_next (instructions);
	}

	ret = g_slist_prepend (ret,
	                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS,
	                                                     "-",
	                                                     &smanip->pop));

	ret = g_slist_reverse (ret);
	cdn_expression_set_instructions_take (event->priv->condition, ret);
	g_slist_foreach (ret, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (ret);

	event->priv->comparison = id;
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

	for (item = event->priv->set_properties; item; item = g_slist_next (item))
	{
		SetProperty *p = item->data;
		CdnCompileContext *ctx;

		ctx = cdn_object_get_compile_context (cdn_variable_get_object (p->property),
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

static void
cdn_event_class_init (CdnEventClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_object_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_event_finalize;

	object_class->get_property = cdn_event_get_property;
	object_class->set_property = cdn_event_set_property;

	cdn_object_class->compile = cdn_event_compile_impl;

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
	                                 PROP_GOTO_PHASE,
	                                 g_param_spec_string ("goto-phase",
	                                                      "Goto Phase",
	                                                      "Goto phase",
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

CdnEvent *
cdn_event_new (gchar const   *id,
               CdnExpression *condition,
               gdouble        approximation)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (condition), NULL);

	return g_object_new (CDN_TYPE_EVENT,
	                     "id",
	                     id,
	                     "condition",
	                     condition,
	                     "approximation",
	                     approximation,
	                     NULL);
}

void
cdn_event_update (CdnEvent *event)
{
	if (event->priv->condition)
	{
		event->priv->value = cdn_expression_evaluate (event->priv->condition);

		if (event->priv->comparison == CDN_MATH_FUNCTION_TYPE_LESS ||
		    event->priv->comparison == CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL)
		{
			event->priv->value = -event->priv->value;
		}
	}
}

/**
 * cdn_event_get_condition:
 * @event: A #CdnEvent
 *
 * Get the event condition.
 *
 * Returns: (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_event_get_condition (CdnEvent *event)
{
	return event->priv->condition;
}

gdouble
cdn_event_get_approximation (CdnEvent *event)
{
	return event->priv->approximation;
}

void
cdn_event_set_condition (CdnEvent      *event,
                         CdnExpression *condition)
{
	g_return_if_fail (CDN_IS_EVENT (event));
	g_return_if_fail (condition == NULL || CDN_IS_EXPRESSION (condition));

	set_condition (event, condition);
}

void
cdn_event_set_approximation (CdnEvent *event,
                             gdouble   approximation)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	if (event->priv->approximation != approximation)
	{
		event->priv->approximation = approximation;
		g_object_notify (G_OBJECT (event), "approximation");
	}
}

gboolean
cdn_event_happened (CdnEvent *event,
                    gdouble  *dist)
{
	gdouble val;

	val = cdn_expression_evaluate (event->priv->condition);

	if (event->priv->comparison == CDN_MATH_FUNCTION_TYPE_LESS ||
	    event->priv->comparison == CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL)
	{
		val = -val;
	}

	switch (event->priv->comparison)
	{
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_GREATER:
			// From negative to positive
			if (event->priv->value >= 0 || val <= 0)
			{
				return FALSE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
			// From negative to positive
			if (event->priv->value >= 0 || val < 0)
			{
				return FALSE;
			}
		break;
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
			if (fabs (event->priv->value) > event->priv->approximation &&
			    fabs (val) <= event->priv->approximation)
			{
				if (dist)
				{
					*dist = 0;
				}

				return TRUE;
			}
			else
			{
				return FALSE;
			}
		break;
		default:
		break;
	}

	if (dist)
	{
		gdouble df = val - event->priv->value;

		if (df <= event->priv->approximation)
		{
			*dist = 0;
		}
		else
		{
			*dist = -event->priv->value / df;
		}
	}

	return TRUE;
}

void
cdn_event_add_set_variable (CdnEvent      *event,
                            CdnVariable   *property,
                            CdnExpression *value)
{
	SetProperty *p;

	g_return_if_fail (CDN_IS_EVENT (event));
	g_return_if_fail (CDN_IS_VARIABLE (property));
	g_return_if_fail (CDN_IS_EXPRESSION (value));

	p = set_property_new (property, value);

	event->priv->set_properties = g_slist_append (event->priv->set_properties,
	                                              p);
}

static void
execute_set_property (CdnEvent    *event,
                      SetProperty *p)
{
	CdnDimension dim;
	gdouble const *values;

	values = cdn_expression_evaluate_values (p->value, &dim);
	cdn_variable_set_values (p->property, values, &dim);
}

void
cdn_event_execute (CdnEvent *event)
{
	GSList *setprop;

	for (setprop = event->priv->set_properties; setprop; setprop = g_slist_next (setprop))
	{
		execute_set_property (event, setprop->data);
	}
}

void
cdn_event_set_goto_phase (CdnEvent           *event,
                          gchar const        *phase)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	g_free (event->priv->goto_phase);
	event->priv->goto_phase = g_strdup (phase);

	g_object_notify (G_OBJECT (event), "goto-phase");
}

gchar const *
cdn_event_get_goto_phase (CdnEvent *event)
{
	g_return_val_if_fail (CDN_IS_EVENT (event), NULL);

	return event->priv->goto_phase;
}

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

gboolean
cdn_event_get_terminal (CdnEvent *event)
{
	return event->priv->terminal;
}
