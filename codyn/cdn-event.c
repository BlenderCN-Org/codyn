#include "cdn-event.h"
#include "cdn-enum-types.h"
#include "cdn-compile-error.h"
#include "cdn-symbolic.h"

#define CDN_EVENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EVENT, CdnEventPrivate))

struct _CdnEventPrivate
{
	CdnExpression *condition;
	CdnEventDirection direction;
	gdouble value;

	GSList *set_properties;
	GSList *set_flags;

	GSList *events;
};

G_DEFINE_TYPE (CdnEvent, cdn_event, G_TYPE_INITIALLY_UNOWNED)

enum
{
	PROP_0,
	PROP_CONDITION,
	PROP_DIRECTION
};

typedef struct
{
	CdnVariable *property;
	CdnExpression *value;
} SetProperty;

typedef struct
{
	CdnEdgeAction *action;

	CdnEventActionFlags flags;
} SetFlags;

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

static SetFlags *
set_flags_new (CdnEdgeAction       *action,
               CdnEventActionFlags  flags)
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
cdn_event_finalize (GObject *object)
{
	CdnEvent *self;

	self = CDN_EVENT (object);

	g_slist_foreach (self->priv->set_properties,
	                 (GFunc)set_property_free,
	                 NULL);

	g_slist_foreach (self->priv->set_flags,
	                 (GFunc)set_flags_free,
	                 NULL);

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
		case PROP_DIRECTION:
			self->priv->direction = g_value_get_flags (value);
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
		case PROP_DIRECTION:
			g_value_set_flags (value, self->priv->direction);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_event_class_init (CdnEventClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_event_finalize;

	object_class->get_property = cdn_event_get_property;
	object_class->set_property = cdn_event_set_property;


	g_type_class_add_private (object_class, sizeof(CdnEventPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_CONDITION,
	                                 g_param_spec_object ("condition",
	                                                      "Condition",
	                                                      "Condition",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_DIRECTION,
	                                 g_param_spec_flags ("direction",
	                                                     "Direction",
	                                                     "Direction",
	                                                     CDN_TYPE_EVENT_DIRECTION,
	                                                     CDN_EVENT_DIRECTION_POSITIVE,
	                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cdn_event_init (CdnEvent *self)
{
	self->priv = CDN_EVENT_GET_PRIVATE (self);
}

CdnEvent *
cdn_event_new (CdnExpression *condition,
               CdnEventDirection direction)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (condition), NULL);

	return g_object_new (CDN_TYPE_EVENT,
	                     "condition",
	                     condition,
	                     "direction",
	                     direction,
	                     NULL);
}

void
cdn_event_update (CdnEvent *event)
{
	if (event->priv->condition)
	{
		event->priv->value = cdn_expression_evaluate (event->priv->condition);
	}
}

CdnExpression *
cdn_event_get_condition (CdnEvent *event)
{
	return event->priv->condition;
}

CdnEventDirection
cdn_event_get_direction (CdnEvent *event)
{
	return event->priv->direction;
}

gboolean
cdn_event_happened (CdnEvent *event,
                    gdouble  *dist)
{
	gdouble val;

	if ((event->priv->direction & CDN_EVENT_DIRECTION_NEGATIVE) && event->priv->value > 0)
	{
		val = cdn_expression_evaluate (event->priv->condition);

		if (val < 0)
		{
			if (dist)
			{
				gdouble term = event->priv->value - val;

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
	else if ((event->priv->direction & CDN_EVENT_DIRECTION_POSITIVE) && event->priv->value < 0)
	{
		val = cdn_expression_evaluate (event->priv->condition);

		if (val > 0)
		{
			if (dist)
			{
				gdouble term = val - event->priv->value;

				if (term > 10e-9)
				{
					*dist = -event->priv->value / term;
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

void
cdn_event_add_set_edge_action_flags (CdnEvent            *event,
                                     CdnEdgeAction       *action,
                                     CdnEventActionFlags  flags)
{
	SetFlags *p;

	g_return_if_fail (CDN_IS_EVENT (event));
	g_return_if_fail (CDN_IS_EDGE_ACTION (action));

	p = set_flags_new (action, flags);

	event->priv->set_flags = g_slist_append (event->priv->set_flags,
	                                         p);
}

static void
execute_set_property (SetProperty *p)
{
	cdn_variable_set_value (p->property,
	                        cdn_expression_evaluate (p->value));
}

static void
execute_set_flags (SetFlags *p)
{
	CdnEdgeActionFlags flags;

	flags = cdn_edge_action_get_flags (p->action);

	switch (p->flags)
	{
		case CDN_EVENT_ACTION_FLAGS_DISABLE:
			flags |= CDN_EDGE_ACTION_FLAG_DISABLED;
		break;
		case CDN_EVENT_ACTION_FLAGS_ENABLE:
			flags &= ~CDN_EDGE_ACTION_FLAG_DISABLED;
		break;
		case CDN_EVENT_ACTION_FLAGS_SWITCH:
			if (flags & CDN_EDGE_ACTION_FLAG_DISABLED)
			{
				flags &= ~CDN_EDGE_ACTION_FLAG_DISABLED;
			}
			else
			{
				flags |= CDN_EDGE_ACTION_FLAG_DISABLED;
			}
		break;
	}

	cdn_edge_action_set_flags (p->action, flags);
}

void
cdn_event_execute (CdnEvent *event)
{
	g_slist_foreach (event->priv->set_properties,
	                 (GFunc)execute_set_property,
	                 NULL);

	g_slist_foreach (event->priv->set_flags,
	                 (GFunc)execute_set_flags,
	                 NULL);
}

gboolean
cdn_event_compile (CdnEvent          *event,
                   CdnCompileContext *context,
                   CdnCompileError   *error)
{
	GSList *item;

	g_return_val_if_fail (CDN_IS_EVENT (event), FALSE);
	g_return_val_if_fail (CDN_IS_COMPILE_CONTEXT (context), FALSE);
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), FALSE);

	if (!cdn_expression_compile (event->priv->condition, context, error))
	{
		return FALSE;
	}

	for (item = event->priv->set_properties; item; item = g_slist_next (item))
	{
		SetProperty *p = item->data;
		CdnCompileContext *ctx;

		ctx = cdn_object_get_compile_context (cdn_variable_get_object (p->property),
		                                      NULL);

		if (!cdn_expression_compile (p->value, ctx, error))
		{
			g_object_unref (ctx);
			return FALSE;
		}

		g_object_unref (ctx);
	}

	return TRUE;
}
