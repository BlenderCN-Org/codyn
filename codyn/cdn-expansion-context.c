/*
 * cdn-expansion-context.c
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

#include "cdn-expansion-context.h"
#include "cdn-selector.h"

#include <glib/gprintf.h>

struct _CdnExpansionContext
{
	CdnExpansionContext *parent;
	guint ref_count;

	GHashTable *defines;
	GPtrArray *expansions;

	gulong marker;
};

G_DEFINE_BOXED_TYPE (CdnExpansionContext,
                     cdn_expansion_context,
                     cdn_expansion_context_ref,
                     cdn_expansion_context_unref)

CdnExpansionContext *
cdn_expansion_context_new (CdnExpansionContext *parent)
{
	CdnExpansionContext *ret;

	ret = g_slice_new0 (CdnExpansionContext);
	ret->ref_count = 1;

	if (parent)
	{
		if (!parent->defines && !parent->expansions)
		{
			ret->parent = cdn_expansion_context_ref (parent->parent);
		}
		else
		{
			ret->parent = cdn_expansion_context_ref (parent);
		}
	}

	return ret;
}

CdnExpansionContext *
cdn_expansion_context_new_unreffed (CdnExpansionContext *parent)
{
	CdnExpansionContext *ret;

	ret = cdn_expansion_context_new (parent);
	ret->ref_count = 0;

	return ret;
}

CdnExpansionContext *
cdn_expansion_context_ref (CdnExpansionContext *self)
{
	if (self)
	{
		g_atomic_int_inc ((gint *)&self->ref_count);
	}

	return self;
}

void
cdn_expansion_context_unref (CdnExpansionContext *self)
{
	if (!g_atomic_int_dec_and_test ((gint *)&self->ref_count))
	{
		return;
	}

	if (self->parent)
	{
		cdn_expansion_context_unref (self->parent);
		self->parent = NULL;
	}

	if (self->defines)
	{
		g_hash_table_unref (self->defines);
		self->defines = NULL;
	}

	if (self->expansions)
	{
		g_ptr_array_free (self->expansions, TRUE);
		self->expansions = NULL;
	}

	g_slice_free (CdnExpansionContext, self);
}

static GHashTable *
ensure_defines (CdnExpansionContext *context)
{
	if (!context->defines)
	{
		context->defines = g_hash_table_new_full (g_str_hash,
		                                          g_str_equal,
		                                          (GDestroyNotify)g_free,
		                                          (GDestroyNotify)cdn_expansion_unref);
	}

	return context->defines;
}

static CdnExpansion *
context_get_define (CdnExpansionContext *context,
                    gchar const         *name,
                    gboolean            *mine)
{
	CdnExpansionContext *current;

	current = context;

	if (mine)
	{
		*mine = FALSE;
	}

	while (current)
	{
		CdnExpansion *ret;

		if (current->defines)
		{
			ret = g_hash_table_lookup (current->defines, name);

			if (ret)
			{
				if (mine && current == context)
				{
					*mine = TRUE;
				}

				return ret;
			}
		}

		current = current->parent;
	}

	g_hash_table_insert (ensure_defines (context),
	                     g_strdup (name),
	                     NULL);

	return NULL;
}

gint
cdn_expansion_context_increment_define (CdnExpansionContext *context,
                                        gchar const         *name,
                                        gint                 exidx,
                                        gint                 num)
{
	CdnExpansion *val;
	gint ret;
	gchar *incval;
	gboolean mine;

	g_return_val_if_fail (context != NULL, 0);
	g_return_val_if_fail (name != NULL, 0);

	val = context_get_define (context, name, &mine);

	if (val)
	{
		ret = (gint)g_ascii_strtod (cdn_expansion_get (val, exidx), NULL);
	}
	else
	{
		ret = 0;
	}

	incval = g_strdup_printf ("%d", ret + num);

	if (val && mine)
	{
		cdn_expansion_set (val, exidx, incval);
	}
	else
	{
		val = cdn_expansion_new_one (incval);
		cdn_expansion_context_add_define (context, name, val);
		cdn_expansion_unref (val);
	}

	g_free (incval);

	++context->marker;
	return ret;
}

void
cdn_expansion_context_add_define (CdnExpansionContext *context,
                                  gchar const         *name,
                                  CdnExpansion        *value)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (name != NULL);
	g_return_if_fail (value != NULL);

	g_hash_table_insert (ensure_defines (context),
	                     g_strdup (name),
	                     value ? cdn_expansion_copy (value) : cdn_expansion_new_one (""));

	++context->marker;
}

static GPtrArray *
ensure_expansions (CdnExpansionContext *context)
{
	if (!context->expansions)
	{
		context->expansions =
			g_ptr_array_new_with_free_func ((GDestroyNotify)cdn_expansion_unref);
	}

	return context->expansions;
}

void
cdn_expansion_context_add_expansion (CdnExpansionContext *context,
                                     CdnExpansion        *expansion)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (expansion != NULL);

	g_ptr_array_add (ensure_expansions (context),
	                 cdn_expansion_ref (expansion));

	++context->marker;
}

void
cdn_expansion_context_remove_expansion (CdnExpansionContext *context,
                                        CdnExpansion        *expansion)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (expansion != NULL);

	if (!context->expansions)
	{
		return;
	}

	if (g_ptr_array_remove (context->expansions, expansion))
	{
		++context->marker;
	}
}

/**
 * cdn_expansion_context_add_expansions:
 * @context: A #CdnExpansionContext
 * @expansions: (element-type CdnExpansion): A #GSList of #CdnExpansion
 *
 * Add expansions to the expansion context.
 *
 **/
void
cdn_expansion_context_add_expansions (CdnExpansionContext *context,
                                      GSList const        *expansions)
{
	GPtrArray *exps;

	g_return_if_fail (context != NULL);

	if (!expansions)
	{
		return;
	}

	exps = ensure_expansions (context);

	while (expansions)
	{
		g_ptr_array_add (exps,
		                 cdn_expansion_ref (expansions->data));

		expansions = g_slist_next (expansions);
	}

	++context->marker;
}

/**
 * cdn_expansion_context_get_define:
 * @context: A #CdnExpansionContext
 * @name: The name of the define
 *
 * Get the expansion for a particular define name.
 *
 * Returns: (transfer none): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_expansion_context_get_define (CdnExpansionContext *context,
                                  gchar const         *name)
{
	g_return_val_if_fail (context != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	return context_get_define (context, name, NULL);
}

/**
 * cdn_expansion_context_get_expansion:
 * @context: A #CdnExpansionContext
 * @depth: The depth of the context at which to get the expansion
 *
 * Get an expansion at a particular depth.
 *
 * Returns: (transfer none) (allow-none): A #CdnExpansion
 *
 **/
CdnExpansion *
cdn_expansion_context_get_expansion (CdnExpansionContext *context,
                                     gint                 depth)
{
	g_return_val_if_fail (context != NULL, NULL);

	while (context)
	{
		if (context->expansions)
		{
			if (depth < context->expansions->len)
			{
				return context->expansions->pdata[context->expansions->len - depth - 1];
			}
			else
			{
				depth -= context->expansions->len;
			}
		}

		context = context->parent;
	}

	return NULL;
}

gchar *
cdn_expansion_context_calculate (CdnExpansionContext  *context,
                                 gchar const          *equation,
                                 GError              **error)
{
	CdnExpression *expr;
	CdnCompileContext *ctx;
	gchar *ret = NULL;
	CdnCompileError *err;

	g_return_val_if_fail (context != NULL, NULL);
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

/**
 * cdn_expansion_context_debug_print: (skip):
 * @context: A #CdnExpansionContext
 * @file: A #FILE
 *
 * Write debug information of the context.
 *
 **/
void
cdn_expansion_context_debug_print (CdnExpansionContext  *context,
                                   FILE                 *file)
{
	CdnExpansionContext *ctx;
	GHashTable *seen;
	GSList *defines = NULL;
	gint i;

	g_return_if_fail (context != NULL);
	g_return_if_fail (file != NULL);

	g_fprintf (file, "[debug] Defines: {");

	seen = g_hash_table_new (g_str_hash, g_str_equal);

	for (ctx = context; ctx; ctx = ctx->parent)
	{
		GList *keys;

		if (!ctx->defines)
		{
			continue;
		}

		keys = g_hash_table_get_keys (ctx->defines);

		while (keys)
		{
			CdnExpansion *ex;

			ex = g_hash_table_lookup (ctx->defines, keys->data);

			if (ex && !g_hash_table_lookup_extended (seen,
			                                         keys->data,
			                                         NULL,
			                                         NULL))
			{
				g_hash_table_insert (seen, keys->data, NULL);

				defines = g_slist_insert_sorted (defines,
				                                 keys->data,
				                                 (GCompareFunc)g_strcmp0);
			}

			keys = g_list_delete_link (keys, keys);
		}
	}

	g_hash_table_unref (seen);

	if (!defines)
	{
		g_fprintf (file, "}\n");
	}
	else
	{
		while (defines)
		{
			g_fprintf (file, "\n  %s: ", (gchar const *)defines->data);

			cdn_expansion_debug_print (cdn_expansion_context_get_define (context, defines->data),
			                           file);

			defines = g_slist_delete_link (defines, defines);
		}

		g_fprintf (file, "\n}\n");
	}

	g_fprintf (file, "[debug] Expansions: [");
	i = 0;

	for (ctx = context; ctx; ctx = ctx->parent)
	{
		gint l;

		if (!ctx->expansions)
		{
			continue;
		}

		for (l = 0; l < ctx->expansions->len; ++l)
		{
			if (i == 0)
			{
				g_fprintf (file, "\n");
			}

			g_fprintf (file, "  %d: ", i + 1);
			cdn_expansion_debug_print (ctx->expansions->pdata[ctx->expansions->len - l - 1], file);
			g_fprintf (file, "\n");

			++i;
		}
	}

	g_fprintf (file, "]\n");
}

gulong
cdn_expansion_context_get_marker (CdnExpansionContext *context)
{
	g_return_val_if_fail (context != NULL, 0);

	return context->marker;
}

/**
 * cdn_expansion_context_get_expansions:
 * @context: A #CdnExpansionContext
 *
 * Get a list of expansions in @context.
 *
 * Returns: (element-type CdnExpansion) (transfer container): a #GSList of #CdnExpansion
 *
 **/
GSList *
cdn_expansion_context_get_expansions (CdnExpansionContext *context)
{
	GSList *ret = NULL;

	g_return_val_if_fail (context != NULL, NULL);

	while (context)
	{
		CdnExpansionContext *ctx;
		gint i;

		ctx = context;
		context = context->parent;

		if (!ctx->expansions)
		{
			continue;
		}

		for (i = 0; i < ctx->expansions->len; ++i)
		{
			ret = g_slist_prepend (ret,
			                       ctx->expansions->pdata[ctx->expansions->len - i - 1]);
		}
	}

	return g_slist_reverse (ret);
}

void
cdn_expansion_context_shared_defines (CdnExpansionContext *context,
                                      CdnExpansionContext *from)
{
	g_return_if_fail (context != NULL);
	g_return_if_fail (from != NULL);

	if (context == from)
	{
		return;
	}

	if (context->defines)
	{
		g_hash_table_unref (context->defines);
	}

	context->defines = g_hash_table_ref (ensure_defines (from));
	++context->marker;
}

void
cdn_expansion_context_foreach_define (CdnExpansionContext *context,
                                      GHFunc               hfunc,
                                      gpointer             userdata)
{
	g_return_if_fail (context != NULL);

	if (hfunc == NULL)
	{
		return;
	}

	while (context)
	{
		if (context->defines)
		{
			g_hash_table_foreach (context->defines,
			                      hfunc,
			                      userdata);
		}

		context = context->parent;
	}
}

CdnExpansionContext *
cdn_expansion_context_get_parent (CdnExpansionContext *context)
{
	g_return_val_if_fail (context != NULL, NULL);

	return context->parent;
}

/**
 * cdn_expansion_context_get_local_expansions:
 * @context: a #CdnExpansionContext.
 *
 * Get the expansions defined in this context but not those in the parent context.
 *
 * Returns: (element-type CdnExpansion) (transfer container): a #GSList of #CdnExpansion
 *
 **/
GSList *
cdn_expansion_context_get_local_expansions (CdnExpansionContext *context)
{
	GSList *ret = NULL;
	gint i;

	g_return_val_if_fail (context, NULL);

	if (!context->expansions)
	{
		return NULL;
	}

	for (i = 0; i < context->expansions->len; ++i)
	{
		ret = g_slist_prepend (ret,
		                       context->expansions->pdata[i]);
	}

	return ret;
}

static void
copy_define (gchar const  *name,
             CdnExpansion *value,
             GHashTable   *dest)
{
	if (value)
	{
		g_hash_table_insert (dest,
		                     g_strdup (name),
		                     value ? cdn_expansion_copy (value) : NULL);
	}
}

void
cdn_expansion_context_add_defines (CdnExpansionContext *context,
                                   GHashTable          *defines)
{
	g_return_if_fail (context != NULL);

	if (!defines || g_hash_table_size (defines) == 0)
	{
		return;
	}

	g_hash_table_foreach (defines,
	                      (GHFunc)copy_define,
	                      ensure_defines (context));

	++context->marker;
}

/**
 * cdn_expansion_context_get_local_defines:
 * @context: a #CdnExpansionContext.
 *
 * Get the defines of this context, but not those in the parent context.
 *
 * Returns: (transfer none) (allow-none): a #GHashTable or %NULL.
 *
 **/
GHashTable *
cdn_expansion_context_get_local_defines (CdnExpansionContext *context)
{
	g_return_val_if_fail (context, NULL);

	return context->defines;
}

static void
do_merge (CdnExpansionContext *context,
          GSList              *start)
{
	while (start)
	{
		CdnExpansionContext *ex = start->data;
		GSList *exps;
		GHashTable *defines;

		exps = cdn_expansion_context_get_local_expansions (ex);

		if (exps)
		{
			cdn_expansion_context_add_expansions (context, exps);
		}

		defines = cdn_expansion_context_get_local_defines (ex);

		if (defines)
		{
			cdn_expansion_context_add_defines (context, defines);
		}

		g_slist_free (exps);
		start = g_slist_delete_link (start, start);
	}

	++context->marker;
}

void
cdn_expansion_context_merge (CdnExpansionContext *context,
                             CdnExpansionContext *other)
{
	CdnExpansionContext *c1;
	CdnExpansionContext *c2;
	GSList *p1 = NULL;
	GSList *p2 = NULL;
	GSList *start;

	g_return_if_fail (context != NULL);

	if (other == NULL)
	{
		return;
	}

	c1 = context;
	c2 = other;

	start = NULL;

	while (c1 || c2)
	{
		if (c1 == c2)
		{
			start = p2;
			break;
		}

		if (c1)
		{
			p1 = g_slist_prepend (p1, c1);
			c1 = cdn_expansion_context_get_parent (c1);
		}

		if (c2)
		{
			p2 = g_slist_prepend (p2, c2);
			c2 = cdn_expansion_context_get_parent (c2);
		}
	}

	if (!start)
	{
		start = p2;

		while (p1 && p2 && p1->data == p2->data)
		{
			p1 = g_slist_delete_link (p1, p1);
			p2 = g_slist_delete_link (p2, p2);

			start = p2;
		}
	}

	// This release start also
	do_merge (context, start);

	g_slist_free (p1);
}

void
cdn_expansion_context_truncate (CdnExpansionContext *context,
                                CdnExpansionContext *parent)
{
	CdnExpansionContext *ctx;

	g_return_if_fail (context != NULL);

	ctx = context;

	if (!parent)
	{
		return;
	}

	while (parent && !parent->defines && !parent->expansions)
	{
		parent = parent->parent;
	}

	while (context && context->parent != parent)
	{
		context = context->parent;
	}

	if (context && context->parent)
	{
		cdn_expansion_context_unref (context->parent);
		context->parent = NULL;
	}

	while (ctx)
	{
		++ctx->marker;

		ctx = ctx->parent;
	}
}

