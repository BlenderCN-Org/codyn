/*
 * cdn-expansion-context-stack.c
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

#include "cdn-expansion-context-stack.h"
#include "cdn-selector.h"

#define CDN_EMBEDDED_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EXPANSION_CONTEXT_STACK, CdnEmbeddedContextPrivate))

#define CURRENT_CONTEXT(self) ((CdnExpansionContext *)(self->priv->contexts ? self->priv->contexts->data : NULL))

struct _CdnEmbeddedContextPrivate
{
	GSList *contexts;
};

G_DEFINE_TYPE (CdnEmbeddedContext, cdn_expansion_context_stack, G_TYPE_OBJECT)

static void
cdn_expansion_context_stack_finalize (GObject *object)
{
	CdnEmbeddedContext *context;

	context = CDN_EXPANSION_CONTEXT_STACK (object);

	while (context->priv->contexts)
	{
		cdn_expansion_context_stack_restore (context);
	}

	G_OBJECT_CLASS (cdn_expansion_context_stack_parent_class)->finalize (object);
}

static void
cdn_expansion_context_stack_class_init (CdnEmbeddedContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_expansion_context_stack_finalize;

	g_type_class_add_private (object_class, sizeof (CdnEmbeddedContextPrivate));
}

static void
cdn_expansion_context_stack_init (CdnEmbeddedContext *self)
{
	self->priv = CDN_EXPANSION_CONTEXT_STACK_GET_PRIVATE (self);

	cdn_expansion_context_stack_save (self);
}

CdnEmbeddedContext *
cdn_expansion_context_stack_new ()
{
	return g_object_new (CDN_TYPE_EXPANSION_CONTEXT_STACK, NULL);
}

void
cdn_expansion_context_stack_save (CdnEmbeddedContext *context)
{
	g_return_if_fail (CDN_IS_EXPANSION_CONTEXT_STACK (context));

	context->priv->contexts = g_slist_prepend (context->priv->contexts,
	                                           cdn_expansion_context_new (CURRENT_CONTEXT (context)));
}

void
cdn_expansion_context_stack_restore (CdnEmbeddedContext *context)
{
	g_return_if_fail (CDN_IS_EXPANSION_CONTEXT_STACK (context));

	if (context->priv->contexts)
	{
		cdn_expansion_context_unref (context->priv->contexts->data);

		context->priv->contexts =
			g_slist_delete_link (context->priv->contexts,
			                     context->priv->contexts);
	}
}

/**
 * cdn_expansion_context_stack_get_context:
 * @context: A #CdnEmbeddedContext
 *
 * Get the current expansion context.
 *
 * Returns: (transfer none): A #CdnExpansionContext
 *
 **/
CdnExpansionContext *
cdn_expansion_context_stack_get_defines (CdnEmbeddedContext *context)
{
	g_return_val_if_fail (CDN_IS_EXPANSION_CONTEXT_STACK (context), NULL);

	return CURRENT_CONTEXT (context);
}
