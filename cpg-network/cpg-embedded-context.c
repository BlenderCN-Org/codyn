#include "cpg-embedded-context.h"
#include "cpg-selector.h"

#define CPG_EMBEDDED_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextPrivate))

struct _CpgEmbeddedContextPrivate
{
	GSList *defines;

	GSList *expansions;
	GSList *numexp;

	gulong marker;
};

G_DEFINE_TYPE (CpgEmbeddedContext, cpg_embedded_context, G_TYPE_OBJECT)

static void
cpg_embedded_context_finalize (GObject *object)
{
	CpgEmbeddedContext *context;

	context = CPG_EMBEDDED_CONTEXT (object);

	while (context->priv->defines)
	{
		cpg_embedded_context_pop_define (context);
	}

	cpg_embedded_context_set_expansions (context, NULL);

	G_OBJECT_CLASS (cpg_embedded_context_parent_class)->finalize (object);
}

static void
cpg_embedded_context_class_init (CpgEmbeddedContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_embedded_context_finalize;

	g_type_class_add_private (object_class, sizeof (CpgEmbeddedContextPrivate));
}

static void
cpg_embedded_context_init (CpgEmbeddedContext *self)
{
	self->priv = CPG_EMBEDDED_CONTEXT_GET_PRIVATE (self);

	cpg_embedded_context_push_define (self);
}

CpgEmbeddedContext *
cpg_embedded_context_new ()
{
	return g_object_new (CPG_TYPE_EMBEDDED_CONTEXT, NULL);
}

void
cpg_embedded_context_define (CpgEmbeddedContext *context,
                             gchar const        *name,
                             gchar const        *value)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (name != NULL);

	if (context->priv->defines)
	{
		g_hash_table_insert (context->priv->defines->data,
		                     g_strdup (name),
		                     g_strdup (value ? value : ""));
	}

	++context->priv->marker;
}

void
cpg_embedded_context_set_expansions (CpgEmbeddedContext *context,
                                     GSList             *expansions)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	while (context->priv->numexp)
	{
		cpg_embedded_context_pop_expansions (context);
	}

	cpg_embedded_context_push_expansions (context, expansions);
	++context->priv->marker;
}

GSList *
cpg_embedded_context_get_expansions (CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return context->priv->expansions;
}

void
cpg_embedded_context_push_expansion (CpgEmbeddedContext *context,
                                     CpgExpansion       *expansion)
{
	GSList *r;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (expansion != NULL);

	r = g_slist_prepend (NULL, expansion);
	cpg_embedded_context_push_expansions (context, r);
	g_slist_free (r);

	++context->priv->marker;
}

void
cpg_embedded_context_push_expansions (CpgEmbeddedContext *context,
                                      GSList             *expansions)
{
	gint i = 0;
	GSList *rev = NULL;
	GSList *last = NULL;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	while (expansions)
	{
		GSList *tmp = g_slist_prepend (NULL, cpg_expansion_copy (expansions->data));

		if (rev == NULL)
		{
			rev = tmp;
		}
		else
		{
			last->next = tmp;
		}

		last = tmp;
		expansions = g_slist_next (expansions);

		++i;
	}

	if (last)
	{
		last->next = context->priv->expansions;
		context->priv->expansions = rev;
		
	}

	context->priv->numexp = g_slist_prepend (context->priv->numexp,
	                                         GINT_TO_POINTER (i));

	++context->priv->marker;
}

void
cpg_embedded_context_pop_expansions (CpgEmbeddedContext *context)
{
	gint num;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (context->priv->numexp);

	num = GPOINTER_TO_INT (context->priv->numexp->data);

	while (num > 0)
	{
		g_object_unref (context->priv->expansions->data);

		context->priv->expansions =
			g_slist_delete_link (context->priv->expansions,
			                     context->priv->expansions);

		--num;
	}

	context->priv->numexp =
		g_slist_delete_link (context->priv->numexp,
		                     context->priv->numexp);

	++context->priv->marker;
}

gchar *
cpg_embedded_context_lookup_define (CpgEmbeddedContext *context,
                                    gchar const        *name)
{
	gchar const *ret;
	GSList *item;

	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (item = context->priv->defines; item; item = g_slist_next (item))
	{
		ret = g_hash_table_lookup (item->data, name);

		if (ret)
		{
			return g_strdup (ret);
		}
	}

	return g_strdup ("");
}

CpgExpansion *
cpg_embedded_context_lookup_expansion (CpgEmbeddedContext *context,
                                       gint                depth)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return g_slist_nth_data (context->priv->expansions, depth);
}

gchar *
cpg_embedded_context_calculate (CpgEmbeddedContext *context,
                                gchar const        *equation)
{
	CpgExpression *expr;
	CpgCompileContext *ctx;
	gchar *ret;

	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (equation != NULL, NULL);

	ctx = cpg_compile_context_new ();
	expr = cpg_expression_new (equation);

	if (cpg_expression_compile (expr, ctx, NULL))
	{
		gdouble val;
		gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

		val = cpg_expression_evaluate (expr);
		g_ascii_dtostr (buf, G_ASCII_DTOSTR_BUF_SIZE, val);

		ret = g_strdup (buf);
	}
	else
	{
		ret = g_strdup ("");
	}

	g_object_unref (expr);
	g_object_unref (ctx);

	return ret;
}

void
cpg_embedded_context_push_define (CpgEmbeddedContext *context)
{
	GHashTable *table;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	table = g_hash_table_new_full (g_str_hash,
	                               g_str_equal,
	                               (GDestroyNotify)g_free,
	                               (GDestroyNotify)g_free);

	context->priv->defines = g_slist_prepend (context->priv->defines,
	                                          table);

	++context->priv->marker;
}

void
cpg_embedded_context_pop_define (CpgEmbeddedContext *context)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	if (!context->priv->defines)
	{
		return;
	}

	g_hash_table_destroy (context->priv->defines->data);
	context->priv->defines = g_slist_delete_link (context->priv->defines,
	                                              context->priv->defines);

	++context->priv->marker;
}

gulong
cpg_embedded_context_get_marker (CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), 0);

	return context->priv->marker;
}
