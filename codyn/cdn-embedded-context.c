/*
 * cdn-embedded-context.c
 * This file is part of codyn
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

#include "cdn-embedded-context.h"
#include "cdn-selector.h"

#define CDN_EMBEDDED_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EMBEDDED_CONTEXT, CdnEmbeddedContextPrivate))

#define CURRENT_CONTEXT(self) ((Context *)(self->priv->contexts ? self->priv->contexts->data : NULL))

static gulong global_marker = 0;

typedef struct
{
	GHashTable *defines;
	GSList *expansions;
	CdnSelection *selection;

	gulong marker;

	guint copy_defines_on_write : 1;
	guint copy_expansions_on_write : 1;
} Context;

struct _CdnEmbeddedContextPrivate
{
	GSList *contexts;
};

G_DEFINE_TYPE (CdnEmbeddedContext, cdn_embedded_context, G_TYPE_OBJECT)

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
		g_slist_foreach (self->expansions, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (self->expansions);
	}

	if (self->selection)
	{
		g_object_unref (self->selection);
	}

	g_slice_free (Context, self);
}

typedef struct
{
	GHashTable *table;
	CdnSelection *selection;
	gboolean overwrite;
} CopyEntryInfo;

static void
copy_entry (gchar const   *key,
            CdnExpansion  *value,
            CopyEntryInfo *info)
{
	if (info->overwrite || !g_hash_table_lookup (info->table, key))
	{
		if (info->selection)
		{
			cdn_selection_add_define (info->selection,
			                          key,
			                          value);
		}
		else
		{
			g_hash_table_insert (info->table,
			                     g_strdup (key),
			                     cdn_expansion_copy (value));
		}
	}
}

static GHashTable *
hash_table_copy (GHashTable *table)
{
	GHashTable *ret;

	ret = g_hash_table_new_full (g_str_hash,
	                             g_str_equal,
	                             (GDestroyNotify)g_free,
	                             (GDestroyNotify)cdn_expansion_unref);

	if (table)
	{
		CopyEntryInfo info = {ret, NULL, FALSE};

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
		                                       cdn_expansion_copy (expansions->data));

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

		if (self->selection)
		{
			ret->selection = cdn_selection_copy_defines (self->selection,
			                                             copy_defines);
		}
	}
	else
	{
		ret->defines = hash_table_copy (NULL);
	}

	return ret;
}

static void
cdn_embedded_context_finalize (GObject *object)
{
	CdnEmbeddedContext *context;

	context = CDN_EMBEDDED_CONTEXT (object);

	while (context->priv->contexts)
	{
		cdn_embedded_context_restore (context);
	}

	G_OBJECT_CLASS (cdn_embedded_context_parent_class)->finalize (object);
}

static void
cdn_embedded_context_class_init (CdnEmbeddedContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_embedded_context_finalize;

	g_type_class_add_private (object_class, sizeof (CdnEmbeddedContextPrivate));
}

static void
cdn_embedded_context_init (CdnEmbeddedContext *self)
{
	self->priv = CDN_EMBEDDED_CONTEXT_GET_PRIVATE (self);

	cdn_embedded_context_save (self);
}

CdnEmbeddedContext *
cdn_embedded_context_new ()
{
	return g_object_new (CDN_TYPE_EMBEDDED_CONTEXT, NULL);
}

/**
 * cdn_embedded_context_copy_top:
 * @context: A #CdnEmbeddedContext
 *
 * Copy the top of the stack in the embedded context into a new
 * embedded context.
 *
 * Returns: (transfer full): A #CdnEmbeddedContext
 *
 **/
CdnEmbeddedContext *
cdn_embedded_context_copy_top (CdnEmbeddedContext *context)
{
	CdnEmbeddedContext *ret;
	Context *ctx;
	Context *cp;

	ret = cdn_embedded_context_new ();

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
	cdn_embedded_context_restore (ret);

	/* Add new top context */
	ret->priv->contexts = g_slist_prepend (ret->priv->contexts,
	                                       cp);

	return ret;
}

void
cdn_embedded_context_save (CdnEmbeddedContext *context)
{
	cdn_embedded_context_save_defines (context, FALSE);
}

void
cdn_embedded_context_save_defines (CdnEmbeddedContext *context,
                                   gboolean            copy_defines)
{
	Context *ctx;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	ctx = context_copy (CURRENT_CONTEXT (context), copy_defines);

	context->priv->contexts = g_slist_prepend (context->priv->contexts,
	                                           ctx);
}

void
cdn_embedded_context_restore (CdnEmbeddedContext *context)
{
	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	if (context->priv->contexts)
	{
		context_free (context->priv->contexts->data);

		context->priv->contexts =
			g_slist_delete_link (context->priv->contexts,
			                     context->priv->contexts);
	}
}

gint
cdn_embedded_context_increment_define (CdnEmbeddedContext *context,
                                       gchar const        *name,
                                       gint                num)
{
	CdnExpansion *val;
	gint ret;
	gchar *incval;
	CdnExpansion *ex;

	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), 0);
	g_return_val_if_fail (name != NULL, 0);

	val = cdn_embedded_context_get_define (context, name);

	if (val)
	{
		ret = (gint)g_ascii_strtod (cdn_expansion_get (val, 0), NULL);
	}
	else
	{
		ret = 0;
	}

	incval = g_strdup_printf ("%d", ret + num);
	ex = cdn_expansion_new_one (incval);

	cdn_embedded_context_add_define (context,
	                                 name,
	                                 ex);

	g_free (incval);
	cdn_expansion_unref (ex);

	return ret;
}

void
cdn_embedded_context_add_define (CdnEmbeddedContext *context,
                                 gchar const        *name,
                                 CdnExpansion       *value)
{
	Context *ctx;
	GSList *item;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	ctx = CURRENT_CONTEXT (context);

	if (ctx->selection)
	{
		cdn_selection_add_define (ctx->selection,
		                          name,
		                          value);
	}
	else
	{
		copy_defines_on_write (ctx);

		g_hash_table_insert (ctx->defines,
		                     g_strdup (name),
		                     value ? cdn_expansion_copy (value) : cdn_expansion_new_one (""));
	}

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
cdn_embedded_context_set_selection (CdnEmbeddedContext *context,
                                    CdnSelection       *selection)
{
	Context *ctx;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTION (selection));

	cdn_embedded_context_set_expansions (context,
	                                     cdn_selection_get_expansions (selection));

	cdn_embedded_context_set_defines (context,
	                                  NULL,
	                                  TRUE);

	ctx = CURRENT_CONTEXT (context);
	ctx->selection = g_object_ref (selection);
}

void
cdn_embedded_context_set_defines (CdnEmbeddedContext *context,
                                  GHashTable         *defines,
                                  gboolean            inherit)
{
	Context *ctx;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	if (ctx->selection)
	{
		g_object_unref (ctx->selection);
		ctx->selection = NULL;
	}

	if (ctx->defines)
	{
		g_hash_table_unref (ctx->defines);
	}

	ctx->defines = defines ? g_hash_table_ref (defines) : hash_table_copy (NULL);
	ctx->copy_defines_on_write = !inherit;

	ctx->marker = ++global_marker;
}

/**
 * cdn_embedded_context_set_expansions:
 * @context: A #CdnEmbeddedContext
 * @expansions: (element-type CdnExpansion): A #GSList of #CdnExpansion
 *
 * Description.
 *
 **/
void
cdn_embedded_context_set_expansions (CdnEmbeddedContext *context,
                                     GSList             *expansions)
{
	Context *ctx;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	ctx = CURRENT_CONTEXT (context);

	if (!ctx->copy_expansions_on_write)
	{
		g_slist_foreach (ctx->expansions, (GFunc)cdn_expansion_unref, NULL);
		g_slist_free (ctx->expansions);
	}
	else
	{
		ctx->copy_expansions_on_write = FALSE;
	}

	ctx->expansions = NULL;

	cdn_embedded_context_add_expansions (context, expansions);
}

/**
 * cdn_embedded_context_get_expansions:
 * @context: A #CdnEmbeddedContext
 *
 * Get the list of expansions.
 *
 * Returns: (element-type CdnExpansion) (transfer none): A #GSList of #CdnExpansion
 *
 **/
GSList *
cdn_embedded_context_get_expansions (CdnEmbeddedContext *context)
{
	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), NULL);

	return CURRENT_CONTEXT (context)->expansions;
}

void
cdn_embedded_context_add_selection (CdnEmbeddedContext *context,
                                    CdnSelection       *selection)
{
	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (CDN_IS_SELECTION (selection));

	cdn_embedded_context_add_expansions (context,
	                                     cdn_selection_get_expansions (selection));

	cdn_embedded_context_add_defines (context,
	                                  cdn_selection_get_defines (selection));
}

void
cdn_embedded_context_add_defines (CdnEmbeddedContext *context,
                                  GHashTable         *defines)
{
	CopyEntryInfo info;
	Context *ctx;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	if (!defines)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);
	copy_defines_on_write (ctx);

	info.selection = ctx->selection;
	info.table = ctx->selection ? cdn_selection_get_defines (ctx->selection) : ctx->defines;
	info.overwrite = TRUE;

	g_hash_table_foreach (defines,
	                      (GHFunc)copy_entry,
	                      &info);
}

void
cdn_embedded_context_add_expansion (CdnEmbeddedContext *context,
                                    CdnExpansion       *expansion)
{
	GSList *r;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));
	g_return_if_fail (expansion != NULL);

	r = g_slist_prepend (NULL, expansion);

	cdn_embedded_context_add_expansions (context, r);
	g_slist_free (r);
}

/**
 * cdn_embedded_context_add_expansions:
 * @context: A #CdnEmbeddedContext
 * @expansions: (element-type CdnExpansion): A #GSList of #CdnExpansion
 *
 * Add a list of expansions to the context.
 *
 **/
void
cdn_embedded_context_add_expansions (CdnEmbeddedContext *context,
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

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	while (expansions)
	{
		GSList *tmp = g_slist_prepend (NULL,
		                               cdn_expansion_copy (expansions->data));

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

/**
 * cdn_embedded_context_get_define:
 * @context: A #CdnEmbeddedContext
 * @name: The name of the define
 *
 * Get the expansion for a particular define name.
 *
 * Returns: (transfer none): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_embedded_context_get_define (CdnEmbeddedContext *context,
                                 gchar const        *name)
{
	CdnExpansion *ret;
	Context *ctx;

	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	ctx = CURRENT_CONTEXT (context);

	if (ctx->selection)
	{
		ret = cdn_selection_get_define (ctx->selection, name);
	}
	else
	{
		ret = g_hash_table_lookup (ctx->defines, name);
	}

	return ret;
}

/**
 * cdn_embedded_context_get_expansion:
 * @context: A #CdnEmbeddedContext
 * @depth: The depth of the context at which to get the expansion
 *
 * Get an expansion at a particular depth.
 *
 * Returns: (transfer none) (allow-none): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_embedded_context_get_expansion (CdnEmbeddedContext *context,
                                    gint                depth)
{
	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), NULL);

	return g_slist_nth_data (CURRENT_CONTEXT (context)->expansions, depth);
}

gchar *
cdn_embedded_context_calculate (CdnEmbeddedContext  *context,
                                gchar const         *equation,
                                GError             **error)
{
	CdnExpression *expr;
	CdnCompileContext *ctx;
	gchar *ret = NULL;
	CdnCompileError *err;

	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), NULL);
	g_return_val_if_fail (equation != NULL, NULL);

	ctx = cdn_compile_context_new ();
	expr = cdn_expression_new (equation);

	err = cdn_compile_error_new ();

	if (cdn_expression_compile (expr, ctx, err))
	{
		gdouble val;
		gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

		val = cdn_expression_evaluate (expr);
		g_ascii_dtostr (buf, G_ASCII_DTOSTR_BUF_SIZE, val);

		ret = g_strdup (buf);
	}
	else if (error)
	{
		*error = g_error_copy (cdn_compile_error_get_error (err));
	}

	g_object_unref (err);

	g_object_unref (expr);
	g_object_unref (ctx);

	return ret;
}

gulong
cdn_embedded_context_get_marker (CdnEmbeddedContext *context)
{
	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), 0);

	return CURRENT_CONTEXT (context)->marker;
}

/**
 * cdn_embedded_context_get_defines:
 * @context: A #CdnEmbeddedContext
 *
 * Get the defines.
 *
 * Returns: (transfer none): A #GHashTable
 *
 **/
GHashTable *
cdn_embedded_context_get_defines (CdnEmbeddedContext *context)
{
	Context *ctx;

	g_return_val_if_fail (CDN_IS_EMBEDDED_CONTEXT (context), NULL);

	ctx = CURRENT_CONTEXT (context);

	if (ctx->selection)
	{
		return cdn_selection_get_defines (ctx->selection);
	}
	else
	{
		return ctx->defines;
	}
}

void
cdn_embedded_context_set_copy_defines_on_write (CdnEmbeddedContext *context)
{
	GHashTable *defs = NULL;
	GSList *item;

	g_return_if_fail (CDN_IS_EMBEDDED_CONTEXT (context));

	item = context->priv->contexts;

	while (TRUE)
	{
		Context *ctx = item->data;
	
		if (defs == NULL || ctx->defines == defs)
		{
			ctx->copy_defines_on_write = TRUE;
			defs = ctx->defines;
		}
		else
		{
			break;
		}

		item = g_slist_next (item);
	}
}
