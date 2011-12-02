#include "cdn-event.h"
#include "cdn-enum-types.h"
#include "cdn-compile-error.h"
#include "cdn-phaseable.h"

#define CDN_EVENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EVENT, CdnEventPrivate))

struct _CdnEventPrivate
{
	CdnExpression *condition;
	CdnEventDirection direction;
	gdouble value;

	GSList *set_properties;
	GSList *set_flags;

	GSList *events;
	GHashTable *phases;

	gchar *goto_phase;
};

static void cdn_phaseable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnEvent,
                         cdn_event,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_PHASEABLE,
                                                cdn_phaseable_iface_init))

enum
{
	PROP_0,
	PROP_CONDITION,
	PROP_DIRECTION,
	PROP_GOTO_PHASE
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
		case PROP_DIRECTION:
			self->priv->direction = g_value_get_flags (value);
			break;
		case PROP_GOTO_PHASE:
			g_free (self->priv->goto_phase);
			self->priv->goto_phase = g_value_dup_string (value);
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
		case PROP_GOTO_PHASE:
			g_value_set_string (value, self->priv->goto_phase);
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

	g_object_class_install_property (object_class,
	                                 PROP_GOTO_PHASE,
	                                 g_param_spec_string ("goto-phase",
	                                                      "Goto Phase",
	                                                      "Goto phase",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cdn_event_init (CdnEvent *self)
{
	self->priv = CDN_EVENT_GET_PRIVATE (self);
}

CdnEvent *
cdn_event_new (CdnExpression     *condition,
               CdnEventDirection  direction)
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

static void
execute_set_property (SetProperty *p)
{
	cdn_variable_set_value (p->property,
	                        cdn_expression_evaluate (p->value));
}

void
cdn_event_execute (CdnEvent *event)
{
	g_slist_foreach (event->priv->set_properties,
	                 (GFunc)execute_set_property,
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

void
cdn_event_set_goto_phase (CdnEvent           *event,
                          gchar const        *phase)
{
	g_return_if_fail (CDN_IS_EVENT (event));

	g_free (event->priv->goto_phase);
	event->priv->goto_phase = g_strdup (phase);
}

gchar const *
cdn_event_get_goto_phase (CdnEvent *event)
{
	g_return_val_if_fail (CDN_IS_EVENT (event), NULL);

	return event->priv->goto_phase;
}
