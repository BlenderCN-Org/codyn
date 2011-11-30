#include "cpg-event.h"
#include "cpg-enum-types.h"
#include "cpg-compile-error.h"
#include "cpg-symbolic.h"

#define CPG_EVENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EVENT, CpgEventPrivate))

struct _CpgEventPrivate
{
	CpgExpression *condition;
	CpgExpression *condition_dfdt;
	CpgEventDirection direction;
	gdouble value;

	GSList *set_properties;
	GSList *set_flags;

	GSList *events;
};

G_DEFINE_TYPE (CpgEvent, cpg_event, G_TYPE_INITIALLY_UNOWNED)

enum
{
	PROP_0,
	PROP_CONDITION,
	PROP_DIRECTION
};

typedef struct
{
	CpgProperty *property;
	CpgExpression *value;
} SetProperty;

typedef struct
{
	CpgLinkAction *action;

	CpgEventActionFlags flags;
} SetFlags;

static  SetProperty *
set_property_new (CpgProperty   *property,
                  CpgExpression *expression)
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

static SetFlags *
set_flags_new (CpgLinkAction       *action,
               CpgEventActionFlags  flags)
{
	SetFlags *ret;

	ret = g_slice_new0 (SetFlags);

	ret->action = g_object_ref_sink (action);
	ret->flags = flags;

	return ret;
}

static void
set_flags_free (SetFlags *self)
{
	g_object_unref (self->action);
	g_slice_free (SetFlags, self);
}

static void
cpg_event_finalize (GObject *object)
{
	CpgEvent *self;

	self = CPG_EVENT (object);

	g_slist_foreach (self->priv->set_properties,
	                 (GFunc)set_property_free,
	                 NULL);

	g_slist_foreach (self->priv->set_flags,
	                 (GFunc)set_flags_free,
	                 NULL);

	g_slist_foreach (self->priv->events,
	                 (GFunc)g_object_unref,
	                 NULL);

	if (self->priv->condition_dfdt)
	{
		g_object_unref (self->priv->condition_dfdt);
	}

	g_object_unref (self->priv->condition);

	G_OBJECT_CLASS (cpg_event_parent_class)->finalize (object);
}

static void
set_condition (CpgEvent      *event,
               CpgExpression *condition)
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
cpg_event_set_property (GObject      *object,
                        guint         prop_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
	CpgEvent *self = CPG_EVENT (object);

	switch (prop_id)
	{
		case PROP_CONDITION:
			set_condition (self, g_value_get_object (value));
			break;
		case PROP_DIRECTION:
			self->priv->direction = g_value_get_flags (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_event_get_property (GObject    *object,
                        guint       prop_id,
                        GValue     *value,
                        GParamSpec *pspec)
{
	CpgEvent *self = CPG_EVENT (object);

	switch (prop_id)
	{
		case PROP_CONDITION:
			g_value_set_object (value, self->priv->condition);
			break;
		case PROP_DIRECTION:
			g_value_set_flags (value, self->priv->direction);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_event_class_init (CpgEventClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_event_finalize;

	object_class->get_property = cpg_event_get_property;
	object_class->set_property = cpg_event_set_property;


	g_type_class_add_private (object_class, sizeof(CpgEventPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_CONDITION,
	                                 g_param_spec_object ("condition",
	                                                      "Condition",
	                                                      "Condition",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_DIRECTION,
	                                 g_param_spec_flags ("direction",
	                                                     "Direction",
	                                                     "Direction",
	                                                     CPG_TYPE_EVENT_DIRECTION,
	                                                     CPG_EVENT_DIRECTION_POSITIVE,
	                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cpg_event_init (CpgEvent *self)
{
	self->priv = CPG_EVENT_GET_PRIVATE (self);
}

CpgEvent *
cpg_event_new (CpgExpression *condition,
               CpgEventDirection direction)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (condition), NULL);

	return g_object_new (CPG_TYPE_EVENT,
	                     "condition",
	                     condition,
	                     "direction",
	                     direction,
	                     NULL);
}

void
cpg_event_update (CpgEvent *event)
{
	if (event->priv->condition)
	{
		event->priv->value = cpg_expression_evaluate (event->priv->condition);
	}
}

CpgExpression *
cpg_event_get_condition (CpgEvent *event)
{
	return event->priv->condition;
}

CpgExpression *
cpg_event_get_condition_dfdt (CpgEvent *event)
{
	return event->priv->condition_dfdt;
}

CpgEventDirection
cpg_event_get_direction (CpgEvent *event)
{
	return event->priv->direction;
}

gboolean
cpg_event_happened (CpgEvent *event,
                    gdouble  *dist)
{
	gdouble val;

	if ((event->priv->direction & CPG_EVENT_DIRECTION_NEGATIVE) && event->priv->value > 0)
	{
		val = cpg_expression_evaluate (event->priv->condition);

		if (val < 0)
		{
			if (dist)
			{
				gdouble term = (event->priv->value - val);

				if (term > 10e-9)
				{
					*dist = event->priv->value / term;
				}
				else
				{
					*dist = 0;
				}
			}

			return TRUE;
		}
	}
	else if ((event->priv->direction & CPG_EVENT_DIRECTION_POSITIVE) && event->priv->value < 0)
	{
		val = cpg_expression_evaluate (event->priv->condition);

		if (val > 0)
		{
			if (dist)
			{
				gdouble term = val - event->priv->value;

				if (term > 10e-9)
				{
					*dist = event->priv->value / term;
				}
				else
				{
					*dist = 0;
				}
			}

			return TRUE;
		}
	}

	return FALSE;
}

void
cpg_event_add_set_property (CpgEvent      *event,
                            CpgProperty   *property,
                            CpgExpression *value)
{
	SetProperty *p;

	g_return_if_fail (CPG_IS_EVENT (event));
	g_return_if_fail (CPG_IS_PROPERTY (property));
	g_return_if_fail (CPG_IS_EXPRESSION (value));

	p = set_property_new (property, value);

	event->priv->set_properties = g_slist_append (event->priv->set_properties,
	                                              p);
}

void
cpg_event_add_set_link_action_flags (CpgEvent            *event,
                                     CpgLinkAction       *action,
                                     CpgEventActionFlags  flags)
{
	SetFlags *p;

	g_return_if_fail (CPG_IS_EVENT (event));
	g_return_if_fail (CPG_IS_LINK_ACTION (action));

	p = set_flags_new (action, flags);

	event->priv->set_flags = g_slist_append (event->priv->set_flags,
	                                         p);
}

static void
execute_set_property (SetProperty *p)
{
	cpg_property_set_value (p->property,
	                        cpg_expression_evaluate (p->value));
}

static void
execute_set_flags (SetFlags *p)
{
	CpgLinkActionFlags flags;

	flags = cpg_link_action_get_flags (p->action);

	switch (p->flags)
	{
		case CPG_EVENT_ACTION_FLAGS_DISABLE:
			flags |= CPG_LINK_ACTION_FLAG_DISABLED;
		break;
		case CPG_EVENT_ACTION_FLAGS_ENABLE:
			flags &= ~CPG_LINK_ACTION_FLAG_DISABLED;
		break;
		case CPG_EVENT_ACTION_FLAGS_SWITCH:
			if (flags & CPG_LINK_ACTION_FLAG_DISABLED)
			{
				flags &= ~CPG_LINK_ACTION_FLAG_DISABLED;
			}
			else
			{
				flags |= CPG_LINK_ACTION_FLAG_DISABLED;
			}
		break;
	}

	cpg_link_action_set_flags (p->action, flags);
}

void
cpg_event_execute (CpgEvent *event)
{
	g_slist_foreach (event->priv->set_properties,
	                 (GFunc)execute_set_property,
	                 NULL);

	g_slist_foreach (event->priv->set_flags,
	                 (GFunc)execute_set_flags,
	                 NULL);
}

gboolean
cpg_event_compile (CpgEvent          *event,
                   CpgCompileContext *context,
                   CpgCompileError   *error)
{
	GSList *item;

	g_return_val_if_fail (CPG_IS_EVENT (event), FALSE);
	g_return_val_if_fail (CPG_IS_COMPILE_CONTEXT (context), FALSE);
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), FALSE);

	if (!cpg_expression_compile (event->priv->condition, context, error))
	{
		return FALSE;
	}

	if (event->priv->condition_dfdt)
	{
		g_object_unref (event->priv->condition_dfdt);
	}

	// Try to take the time derivative of the condition
	event->priv->condition_dfdt =
		cpg_symbolic_derive (event->priv->condition,
		                     NULL,
		                     NULL,
		                     NULL,
		                     1,
		                     CPG_SYMBOLIC_DERIVE_NONE,
		                     NULL);

	if (event->priv->condition_dfdt)
	{
		g_object_ref_sink (event->priv->condition_dfdt);
	}

	for (item = event->priv->set_properties; item; item = g_slist_next (item))
	{
		SetProperty *p = item->data;
		CpgCompileContext *ctx;

		ctx = cpg_object_get_compile_context (cpg_property_get_object (p->property),
		                                      NULL);

		if (!cpg_expression_compile (p->value, ctx, error))
		{
			g_object_unref (ctx);
			return FALSE;
		}

		g_object_unref (ctx);
	}

	return TRUE;
}
