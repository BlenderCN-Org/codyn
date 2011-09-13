#include "cpg-parser-code.h"
#include "cpg-object.h"
#include "cpg-network.h"
#include "cpg-parser-context.h"
#include "cpg-parser.h"
#include "cpg-enum-types.h"

#define CPG_PARSER_CODE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PARSER_CODE, CpgParserCodePrivate))

struct _CpgParserCodePrivate
{
	CpgEmbeddedContext *closure;
	gchar *code;
	CpgParserCodeEvent event;
};

G_DEFINE_TYPE (CpgParserCode, cpg_parser_code, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_CLOSURE,
	PROP_CODE,
	PROP_EVENT
};

static void
cpg_parser_code_finalize (GObject *object)
{
	CpgParserCode *applied;

	applied = CPG_PARSER_CODE (object);

	g_object_unref (applied->priv->closure);
	g_free (applied->priv->code);

	G_OBJECT_CLASS (cpg_parser_code_parent_class)->finalize (object);
}

static void
cpg_parser_code_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CpgParserCode *self = CPG_PARSER_CODE (object);

	switch (prop_id)
	{
		case PROP_CLOSURE:
		{
			CpgEmbeddedContext *closure = g_value_get_object (value);

			if (closure)
			{
				self->priv->closure =
					cpg_embedded_context_copy_top (closure);
			}
			else
			{
				self->priv->closure = cpg_embedded_context_new ();
			}

			break;
		}
		case PROP_CODE:
			self->priv->code = g_value_dup_string (value);
			break;
		case PROP_EVENT:
			self->priv->event = g_value_get_enum (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_parser_code_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CpgParserCode *self = CPG_PARSER_CODE (object);

	switch (prop_id)
	{
		case PROP_CLOSURE:
			g_value_set_object (value, self->priv->closure);
			break;
		case PROP_CODE:
			g_value_set_string (value, self->priv->code);
			break;
		case PROP_EVENT:
			g_value_set_enum (value, self->priv->event);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_parser_code_class_init (CpgParserCodeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_parser_code_finalize;

	object_class->get_property = cpg_parser_code_get_property;
	object_class->set_property = cpg_parser_code_set_property;

	g_type_class_add_private (object_class, sizeof(CpgParserCodePrivate));

	g_object_class_install_property (object_class,
	                                 PROP_CLOSURE,
	                                 g_param_spec_object ("closure",
	                                                      "Closure",
	                                                      "Closure",
	                                                      CPG_TYPE_EMBEDDED_CONTEXT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_CODE,
	                                 g_param_spec_string ("code",
	                                                      "Code",
	                                                      "Code",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (object_class,
	                                 PROP_EVENT,
	                                 g_param_spec_enum ("event",
	                                                    "Event",
	                                                    "Event",
	                                                    CPG_TYPE_PARSER_CODE_EVENT,
	                                                    CPG_PARSER_CODE_EVENT_NONE,
	                                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_parser_code_init (CpgParserCode *self)
{
	self->priv = CPG_PARSER_CODE_GET_PRIVATE (self);
}

CpgParserCode *
cpg_parser_code_new (CpgEmbeddedContext *closure,
                     gchar const        *code,
                     CpgParserCodeEvent  event)

{
	return g_object_new (CPG_TYPE_PARSER_CODE,
	                     "closure", closure,
	                     "code", code,
	                     "event", event,
	                     NULL);
}

/**
 * cpg_parser_code_get_closure:
 * @applied: A #CpgParserCode
 *
 * Get the closure.
 *
 * Returns: (transfer none): A #CpgEmbeddedContext
 *
 **/
CpgEmbeddedContext *
cpg_parser_code_get_closure (CpgParserCode *applied)
{
	g_return_val_if_fail (CPG_IS_PARSER_CODE (applied), NULL);

	return applied->priv->closure;
}

gchar const *
cpg_parser_code_get_code (CpgParserCode *applied)
{
	g_return_val_if_fail (CPG_IS_PARSER_CODE (applied), NULL);

	return applied->priv->code;
}

CpgParserCodeEvent
cpg_parser_code_get_event (CpgParserCode *applied)
{
	g_return_val_if_fail (CPG_IS_PARSER_CODE (applied), CPG_PARSER_CODE_EVENT_NONE);

	return applied->priv->event;
}

static CpgNetwork *
find_network (CpgObject *object)
{
	while (TRUE)
	{
		CpgObject *p;

		p = cpg_object_get_parent (object);

		if (!p)
		{
			break;
		}

		object = p;
	}

	return CPG_IS_NETWORK (object) ? CPG_NETWORK (object) : NULL;
}

gboolean
cpg_parser_code_run (CpgParserCode  *applied,
                     CpgObject       *object,
                     CpgObject       *context,
                     GError         **error)
{
	CpgNetwork *parent;
	CpgParserContext *parser;
	GSList *sels;
	gboolean ret;
	CpgSelection *selection;

	g_return_val_if_fail (CPG_IS_PARSER_CODE (applied), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	parent = find_network (object);

	if (!parent)
	{
		parent = find_network (context);
	}

	parser = cpg_parser_context_new (parent);

	if (CPG_IS_GROUP (object))
	{
		cpg_parser_context_set_start_token (parser, T_START_GROUP);
	}
	else if (CPG_IS_LINK (object))
	{
		cpg_parser_context_set_start_token (parser, T_START_LINK);
	}
	else
	{
		cpg_parser_context_set_start_token (parser, T_START_STATE);
	}

	cpg_parser_context_set_embedded (parser, applied->priv->closure);

	cpg_parser_context_push_input_from_string (parser, applied->priv->code, NULL);

	selection = cpg_selection_new_defines (object,
	                                       cpg_embedded_context_get_expansions (applied->priv->closure),
	                                       cpg_embedded_context_get_defines (applied->priv->closure),
	                                       TRUE);

	sels = g_slist_prepend (NULL, selection);
	cpg_parser_context_push_objects (parser, sels, NULL);
	g_slist_free (sels);

	ret = cpg_parser_context_parse (parser, FALSE, error);
	g_object_unref (parser);

	return ret;
}
