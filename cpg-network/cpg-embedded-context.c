/*
 * cpg-embedded-context.c
 * This file is part of cpg-network
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

#include "cpg-embedded-context.h"
#include "cpg-selector.h"

#define CPG_EMBEDDED_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EMBEDDED_CONTEXT, CpgEmbeddedContextPrivate))

#define CURRENT_CONTEXT(self) ((Context *)(self->priv->contexts ? self->priv->contexts->data : NULL))

static gulong global_marker = 0;

typedef struct
{
	GHashTable *defines;
	GSList *expansions;

	gulong marker;

	guint copy_defines_on_write : 1;
	guint copy_expansions_on_write : 1;
} Context;

struct _CpgEmbeddedContextPrivate
{
	GSList *contexts;
};

G_DEFINE_TYPE (CpgEmbeddedContext, cpg_embedded_context, G_TYPE_OBJECT)

static Context *
context_new ()
{
	Context *ret;

	ret = g_slice_new0 (Context);
	ret->marker = global_marker;

	return ret;
}

static void
context_free (Context *self)
{
	g_hash_table_unref (self->defines);

	if (!self->copy_expansions_on_write)
	{
		g_slist_foreach (self->expansions, (GFunc)g_object_unref, NULL);
		g_slist_free (self->expansions);
	}

	g_slice_free (Context, self);
}

typedef struct
{
	GHashTable *table;
	gboolean overwrite;
} CopyEntryInfo;

static void
copy_entry (gchar const   *key,
            CpgExpansion  *value,
            CopyEntryInfo *info)
{
	if (info->overwrite || !g_hash_table_lookup (info->table, key))
	{
		g_hash_table_insert (info->table,
		                     g_strdup (key),
		                     cpg_expansion_copy (value));
	}
}

static GHashTable *
hash_table_copy (GHashTable *table)
{
	GHashTable *ret;

	ret = g_hash_table_new_full (g_str_hash,
	                             g_str_equal,
	                             (GDestroyNotify)g_free,
	                             (GDestroyNotify)g_object_unref);

	if (table)
	{
		CopyEntryInfo info = {ret, FALSE};

		g_hash_table_foreach (table, (GHFunc)copy_entry, &info);
	}

	return ret;
}

static void
copy_expansions_on_write (Context *context)
{
	GSList *expansions;

	if (!context->copy_expansions_on_write)
	{
		return;
	}

	expansions = context->expansions;
	context->expansions = NULL;

	while (expansions)
	{
		context->expansions = g_slist_prepend (context->expansions,
		                                       cpg_expansion_copy (expansions->data));

		expansions = g_slist_next (expansions);
	}

	context->expansions = g_slist_reverse (context->expansions);
	context->copy_expansions_on_write = FALSE;
}

static void
copy_defines_on_write (Context *context)
{
	GHashTable *orig;

	if (!context->copy_defines_on_write)
	{
		return;
	}

	orig = context->defines;

	context->defines = hash_table_copy (context->defines);
	context->copy_defines_on_write = FALSE;

	if (orig)
	{
		g_hash_table_unref (orig);
	}
}

static Context *
context_copy (Context  *self,
              gboolean  copy_defines)
{
	Context *ret;

	ret = context_new ();

	if (self)
	{
		ret->copy_defines_on_write = copy_defines;
		ret->copy_expansions_on_write = TRUE;

		ret->expansions = self->expansions;
		ret->defines = self->defines ? g_hash_table_ref (self->defines) : NULL;
	}
	else
	{
		ret->defines = hash_table_copy (NULL);
	}

	return ret;
}

static void
cpg_embedded_context_finalize (GObject *object)
{
	CpgEmbeddedContext *context;

	context = CPG_EMBEDDED_CONTEXT (object);

	while (context->priv->contexts)
	{
		cpg_embedded_context_restore (context);
	}

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

	cpg_embedded_context_save (self);
}

CpgEmbeddedContext *
cpg_embedded_context_new ()
{
	return g_object_new (CPG_TYPE_EMBEDDED_CONTEXT, NULL);
}

/**
 * cpg_embedded_context_copy_top:
 * @context: A #CpgEmbeddedContext
 *
 * Copy the top of the stack in the embedded context into a new
 * embedded context.
 *
 * Returns: (transfer full): A #CpgEmbeddedContext
 *
 **/
CpgEmbeddedContext *
cpg_embedded_context_copy_top (CpgEmbeddedContext *context)
{
	CpgEmbeddedContext *ret;
	Context *ctx;
	Context *cp;

	ret = cpg_embedded_context_new ();

	ctx = CURRENT_CONTEXT (context);

	if (!ctx)
	{
		return ret;
	}

	cp = context_copy (ctx, TRUE);

	/* Make copy now */
	copy_defines_on_write (cp);
	copy_expansions_on_write (cp);

	/* Remove initial context */
	cpg_embedded_context_restore (ret);

	/* Add new top context */
	ret->priv->contexts = g_slist_prepend (ret->priv->contexts,
	                                       cp);

	return ret;
}

void
cpg_embedded_context_save (CpgEmbeddedContext *context)
{
	cpg_embedded_context_save_defines (context, FALSE);
}

void
cpg_embedded_context_save_defines (CpgEmbeddedContext *context,
                                   gboolean            copy_defines)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	ctx = context_copy (CURRENT_CONTEXT (context), copy_defines);

	context->priv->contexts = g_slist_prepend (context->priv->contexts,
	                                           ctx);
}

void
cpg_embedded_context_restore (CpgEmbeddedContext *context)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	if (context->priv->contexts)
	{
		context_free (context->priv->contexts->data);

		context->priv->contexts =
			g_slist_delete_link (context->priv->contexts,
			                     context->priv->contexts);
	}
}

gint
cpg_embedded_context_increment_define (CpgEmbeddedContext *context,
                                       gchar const        *name,
                                       gint                num)
{
	Context *ctx;
	gpointer key;
	gpointer val;
	gint ret;
	gchar *incval;
	CpgExpansion *ex;

	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), 0);
	g_return_val_if_fail (name != NULL, 0);

	ctx = CURRENT_CONTEXT (context);

	if (g_hash_table_lookup_extended (ctx->defines, name, &key, &val))
	{
		ret = (gint)g_ascii_strtod (cpg_expansion_get (val, 0), NULL);
	}
	else
	{
		ret = 0;
	}

	incval = g_strdup_printf ("%d", ret + num);
	ex = cpg_expansion_new_one (incval);

	cpg_embedded_context_add_define (context,
	                                 name,
	                                 ex);

	g_free (incval);
	g_object_unref (ex);

	return ret;
}

void
cpg_embedded_context_add_define (CpgEmbeddedContext *context,
                                 gchar const        *name,
                                 CpgExpansion       *value)
{
	Context *ctx;
	GSList *item;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value == NULL || CPG_IS_EXPANSION (value));

	ctx = CURRENT_CONTEXT (context);

	copy_defines_on_write (ctx);

	g_hash_table_insert (ctx->defines,
	                     g_strdup (name),
	                     value ? cpg_expansion_copy (value) : cpg_expansion_new_one (""));

	for (item = context->priv->contexts->next; item; item = g_slist_next (item))
	{
		Context *c;

		c = item->data;

		if (c->defines != ctx->defines)
		{
			continue;
		}

		c->marker = ++global_marker;
	}

	ctx->marker = ++global_marker;
}

void
cpg_embedded_context_set_selection (CpgEmbeddedContext *context,
                                    CpgSelection       *selection)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTION (selection));

	cpg_embedded_context_set_expansions (context,
	                                     cpg_selection_get_expansions (selection));

	cpg_embedded_context_set_defines (context,
	                                  cpg_selection_get_defines (selection),
	                                  TRUE);
}

void
cpg_embedded_context_set_defines (CpgEmbeddedContext *context,
                                  GHashTable         *defines,
                                  gboolean            inherit)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	if (ctx->defines)
	{
		g_hash_table_unref (ctx->defines);
	}

	ctx->defines = defines ? g_hash_table_ref (defines) : hash_table_copy (NULL);
	ctx->copy_defines_on_write = !inherit;

	ctx->marker = ++global_marker;
}

void
cpg_embedded_context_set_expansions (CpgEmbeddedContext *context,
                                     GSList             *expansions)
{
	Context *ctx;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	if (!ctx->copy_expansions_on_write)
	{
		g_slist_foreach (ctx->expansions, (GFunc)g_object_unref, NULL);
		g_slist_free (ctx->expansions);
	}
	else
	{
		ctx->copy_expansions_on_write = FALSE;
	}

	ctx->expansions = NULL;

	cpg_embedded_context_add_expansions (context, expansions);
}

/**
 * cpg_embedded_context_get_expansions:
 * @context: A #CpgEmbeddedContext
 *
 * Get the list of expansions.
 *
 * Returns: (element-type CpgExpansion) (transfer none): A #GSList of #CpgExpansion
 *
 **/
GSList *
cpg_embedded_context_get_expansions (CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return CURRENT_CONTEXT (context)->expansions;
}

void
cpg_embedded_context_add_selection (CpgEmbeddedContext *context,
                                    CpgSelection       *selection)
{
	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (CPG_IS_SELECTION (selection));

	cpg_embedded_context_add_expansions (context,
	                                     cpg_selection_get_expansions (selection));

	cpg_embedded_context_add_defines (context,
	                                  cpg_selection_get_defines (selection));
}

void
cpg_embedded_context_add_defines (CpgEmbeddedContext *context,
                                  GHashTable         *defines)
{
	CopyEntryInfo info;
	Context *ctx;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	if (!defines)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);
	copy_defines_on_write (ctx);

	info.table = ctx->defines;
	info.overwrite = TRUE;

	g_hash_table_foreach (defines,
	                      (GHFunc)copy_entry,
	                      &info);
}

void
cpg_embedded_context_add_expansion (CpgEmbeddedContext *context,
                                    CpgExpansion       *expansion)
{
	GSList *r;

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (expansion != NULL);

	r = g_slist_prepend (NULL, expansion);

	cpg_embedded_context_add_expansions (context, r);
	g_slist_free (r);
}

void
cpg_embedded_context_add_expansions (CpgEmbeddedContext *context,
                                     GSList             *expansions)
{
	GSList *rev = NULL;
	GSList *last = NULL;
	Context *ctx;

	ctx = CURRENT_CONTEXT (context);

	if (expansions)
	{
		copy_expansions_on_write (ctx);
	}

	g_return_if_fail (CPG_IS_EMBEDDED_CONTEXT (context));

	while (expansions)
	{
		GSList *tmp = g_slist_prepend (NULL,
		                               cpg_expansion_copy (expansions->data));

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
	}

	if (last)
	{
		last->next = ctx->expansions;
		ctx->expansions = rev;
	}

	ctx->marker = ++global_marker;
}

CpgExpansion *
cpg_embedded_context_get_define (CpgEmbeddedContext *context,
                                 gchar const        *name)
{
	CpgExpansion *ret;

	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	ret = g_hash_table_lookup (CURRENT_CONTEXT (context)->defines, name);

	return ret;
}

/**
 * cpg_embedded_context_get_expansion:
 * @context: A #CpgEmbeddedContext
 * @depth: The depth of the context at which to get the expansion
 *
 * Get an expansion at a particular depth.
 *
 * Returns: (transfer none) (allow-none): A #CpgExpansion
 *
 **/
CpgExpansion *
cpg_embedded_context_get_expansion (CpgEmbeddedContext *context,
                                    gint                depth)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return g_slist_nth_data (CURRENT_CONTEXT (context)->expansions, depth);
}

gchar *
cpg_embedded_context_calculate (CpgEmbeddedContext  *context,
                                gchar const         *equation,
                                GError             **error)
{
	CpgExpression *expr;
	CpgCompileContext *ctx;
	gchar *ret = NULL;
	CpgCompileError *err;

	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (equation != NULL, NULL);

	ctx = cpg_compile_context_new ();
	expr = cpg_expression_new (equation);

	err = cpg_compile_error_new ();

	if (cpg_expression_compile (expr, ctx, err))
	{
		gdouble val;
		gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

		val = cpg_expression_evaluate (expr);
		g_ascii_dtostr (buf, G_ASCII_DTOSTR_BUF_SIZE, val);

		ret = g_strdup (buf);
	}
	else if (error)
	{
		*error = g_error_copy (cpg_compile_error_get_error (err));
	}

	g_object_unref (err);

	g_object_unref (expr);
	g_object_unref (ctx);

	return ret;
}

gulong
cpg_embedded_context_get_marker (CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), 0);

	return CURRENT_CONTEXT (context)->marker;
}

/**
 * cpg_embedded_context_get_defines:
 * @context: A #CpgEmbeddedContext
 *
 * Get the defines.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cpg_embedded_context_get_defines (CpgEmbeddedContext *context)
{
	g_return_val_if_fail (CPG_IS_EMBEDDED_CONTEXT (context), NULL);

	return CURRENT_CONTEXT (context)->defines;
}
