#include "cpg-when-applied.h"
#include "cpg-object.h"
#include "cpg-network.h"
#include "cpg-parser-context.h"

#define CPG_WHEN_APPLIED_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_WHEN_APPLIED, CpgWhenAppliedPrivate))

struct _CpgWhenAppliedPrivate
{
	CpgEmbeddedContext *closure;
	gchar *code;
	gboolean is_apply;
};

G_DEFINE_TYPE (CpgWhenApplied, cpg_when_applied, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_CLOSURE,
	PROP_CODE,
	PROP_IS_APPLY
};

static void
cpg_when_applied_finalize (GObject *object)
{
	CpgWhenApplied *applied;

	applied = CPG_WHEN_APPLIED (object);

	g_object_unref (applied->priv->closure);
	g_free (applied->priv->code);

	G_OBJECT_CLASS (cpg_when_applied_parent_class)->finalize (object);
}

static void
cpg_when_applied_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CpgWhenApplied *self = CPG_WHEN_APPLIED (object);

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
		case PROP_IS_APPLY:
			self->priv->is_apply = g_value_get_boolean (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_when_applied_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CpgWhenApplied *self = CPG_WHEN_APPLIED (object);

	switch (prop_id)
	{
		case PROP_CLOSURE:
			g_value_set_object (value, self->priv->closure);
			break;
		case PROP_CODE:
			g_value_set_string (value, self->priv->code);
			break;
		case PROP_IS_APPLY:
			g_value_set_boolean (value, self->priv->is_apply);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_when_applied_class_init (CpgWhenAppliedClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_when_applied_finalize;

	object_class->get_property = cpg_when_applied_get_property;
	object_class->set_property = cpg_when_applied_set_property;

	g_type_class_add_private (object_class, sizeof(CpgWhenAppliedPrivate));

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
	                                 PROP_IS_APPLY,
	                                 g_param_spec_boolean ("is-apply",
	                                                       "Is Apply",
	                                                       "Is apply",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_when_applied_init (CpgWhenApplied *self)
{
	self->priv = CPG_WHEN_APPLIED_GET_PRIVATE (self);
}

CpgWhenApplied *
cpg_when_applied_new (CpgEmbeddedContext *closure,
                      gchar const        *code,
                      gboolean            isapply)

{
	return g_object_new (CPG_TYPE_WHEN_APPLIED,
	                     "closure", closure,
	                     "code", code,
	                     "is-apply", isapply,
	                     NULL);
}

/**
 * cpg_when_applied_get_closure:
 * @applied: A #CpgWhenApplied
 *
 * Get the closure.
 *
 * Returns: (transfer none): A #CpgEmbeddedContext
 *
 **/
CpgEmbeddedContext *
cpg_when_applied_get_closure (CpgWhenApplied *applied)
{
	g_return_val_if_fail (CPG_IS_WHEN_APPLIED (applied), NULL);

	return applied->priv->closure;
}

gchar const *
cpg_when_applied_get_code (CpgWhenApplied *applied)
{
	g_return_val_if_fail (CPG_IS_WHEN_APPLIED (applied), NULL);

	return applied->priv->code;
}

gboolean
cpg_when_applied_get_is_apply (CpgWhenApplied *applied)
{
	g_return_val_if_fail (CPG_IS_WHEN_APPLIED (applied), FALSE);

	return applied->priv->is_apply;
}

gboolean
cpg_when_applied_run (CpgWhenApplied  *applied,
                      CpgObject       *object,
                      GError         **error)
{
	CpgObject *parent;
	CpgParserContext *parser;
	GSList *sels;
	gboolean ret;
	CpgSelection *selection;

	g_return_val_if_fail (CPG_IS_WHEN_APPLIED (applied), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	parent = object;

	while (TRUE)
	{
		CpgObject *p;

		p = cpg_object_get_parent (parent);

		if (!p)
		{
			break;
		}

		parent = p;
	}

	if (!CPG_IS_NETWORK (parent))
	{
		parent = NULL;
	}

	parser = cpg_parser_context_new (parent ? CPG_NETWORK (parent) : NULL);
	cpg_parser_context_set_embedded (parser, applied->priv->closure);

	cpg_parser_context_push_input_from_string (parser, applied->priv->code, NULL);

	selection = cpg_selection_new_defines (object,
	                                       cpg_embedded_context_get_expansions (applied->priv->closure),
	                                       cpg_embedded_context_get_defines (applied->priv->closure),
	                                       TRUE);

	sels = g_slist_prepend (NULL, selection);
	cpg_parser_context_push_object (parser, sels, NULL);
	g_slist_free (sels);

	ret = cpg_parser_context_parse (parser, FALSE, error);
	g_object_unref (parser);

	return ret;
}
