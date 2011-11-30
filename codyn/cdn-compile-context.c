/*
 * cdn-compile-context.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-compile-context.h"
#include "cdn-object.h"
#include "cdn-variable.h"
#include "cdn-function.h"

/**
 * SECTION:cdn-compile-context
 * @short_description: The expression compile context
 *
 * The compile context provides information for compiling expressions such
 * as the available user defined functions and the objects that can be used
 * to lookup properties used in the expression.
 *
 */

#define CURRENT_CONTEXT(x) (CONTEXT (x->priv->contexts->data))
#define CONTEXT(x) ((Context *)x)

#define CDN_COMPILE_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_COMPILE_CONTEXT, CdnCompileContextPrivate))

typedef struct
{
	GSList *objects;
	GSList *functions;
	gboolean function_priority;
	gboolean function_arg_priority;
} Context;

struct _CdnCompileContextPrivate
{
	GSList *contexts;
};

G_DEFINE_TYPE (CdnCompileContext, cdn_compile_context, G_TYPE_OBJECT)

static void
context_free (Context *context)
{
	g_slist_free (context->objects);
	g_slist_free (context->functions);

	g_slice_free (Context, context);
}

static Context *
context_copy (Context *context)
{
	Context *ctx = g_slice_new0 (Context);

	ctx->objects = g_slist_copy (context->objects);
	ctx->functions = g_slist_copy (context->functions);
	ctx->function_priority = context->function_priority;
	ctx->function_arg_priority = context->function_arg_priority;

	return ctx;
}

static void
cdn_compile_context_finalize (GObject *object)
{
	CdnCompileContext *context = CDN_COMPILE_CONTEXT (object);

	g_slist_foreach (context->priv->contexts, (GFunc)context_free, NULL);
	g_slist_free (context->priv->contexts);

	G_OBJECT_CLASS (cdn_compile_context_parent_class)->finalize (object);
}

static void
cdn_compile_context_class_init (CdnCompileContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_compile_context_finalize;

	g_type_class_add_private (object_class, sizeof(CdnCompileContextPrivate));
}

static void
cdn_compile_context_init (CdnCompileContext *self)
{
	self->priv = CDN_COMPILE_CONTEXT_GET_PRIVATE (self);

	self->priv->contexts = g_slist_prepend (NULL, g_slice_new0 (Context));
}

/**
 * cdn_compile_context_new:
 *
 * Create a new compile context.
 *
 * Returns: A #CdnCompileContext
 *
 **/
CdnCompileContext *
cdn_compile_context_new ()
{
	return g_object_new (CDN_TYPE_COMPILE_CONTEXT, NULL);
}

/**
 * cdn_compile_context_save:
 * @context: A #CdnCompileContext
 *
 * Save the current state of the compile context. You can use this to alter
 * the context and restore it to its previous state later. Calls to this
 * function can be nested, but care should be taken to match each call
 * with #cdn_compile_context_restore.
 *
 **/
void
cdn_compile_context_save (CdnCompileContext *context)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));

	if (!context)
	{
		return;
	}

	context->priv->contexts = g_slist_prepend (context->priv->contexts,
	                                           context_copy (CURRENT_CONTEXT (context)));
}

/**
 * cdn_compile_context_restore:
 * @context: A #CdnCompileContext
 *
 * Restore the previous state of the compile context. Each call to restore
 * must be matched by a previous call to #cdn_compile_context_save.
 *
 **/
void
cdn_compile_context_restore (CdnCompileContext *context)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));

	if (!context)
	{
		return;
	}

	if (!context->priv->contexts || !context->priv->contexts->next)
	{
		return;
	}

	context_free (CURRENT_CONTEXT (context));
	context->priv->contexts = g_slist_delete_link (context->priv->contexts,
	                                               context->priv->contexts);
}

/**
 * cdn_compile_context_prepend_object:
 * @context: A #CdnCompileContext
 * @object: (type CdnObject): A #CdnObject
 *
 * Prepend a context object to the list of context objects.
 *
 **/
void
cdn_compile_context_prepend_object (CdnCompileContext *context,
                                    CdnObject         *object)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (object == NULL || CDN_IS_OBJECT (object));

	if (!context || !object)
	{
		return;
	}

	CURRENT_CONTEXT (context)->objects =
		g_slist_prepend (CURRENT_CONTEXT (context)->objects,
		                 object);
}

/**
 * cdn_compile_context_append_object:
 * @context: A #CdnCompileContext
 * @object: (type CdnObject): A #CdnObject
 *
 * Append a context object to the list of context objects.
 *
 **/
void
cdn_compile_context_append_object (CdnCompileContext *context,
                                   CdnObject         *object)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (object == NULL || CDN_IS_OBJECT (object));

	if (!context || !object)
	{
		return;
	}

	CURRENT_CONTEXT (context)->objects =
		g_slist_append (CURRENT_CONTEXT (context)->objects,
		                object);
}

/**
 * cdn_compile_context_prepend_function:
 * @context: A #CdnCompileContext
 * @function: (type CdnFunction): A #CdnFunction
 *
 * Prepend a context function to the list of context functions.
 *
 **/
void
cdn_compile_context_prepend_function (CdnCompileContext *context,
                                      CdnFunction       *function)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (function == NULL || CDN_IS_FUNCTION (function));

	if (!context || !function)
	{
		return;
	}

	CURRENT_CONTEXT (context)->functions =
		g_slist_prepend (CURRENT_CONTEXT (context)->functions,
		                 function);
}

/**
 * cdn_compile_context_append_function:
 * @context: A #CdnCompileContext
 * @function: (type CdnFunction): A #CdnFunction
 *
 * Append a context function to the list of context functions.
 *
 **/
void
cdn_compile_context_append_function (CdnCompileContext *context,
                                     CdnFunction       *function)
{
	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (function == NULL || CDN_IS_FUNCTION (function));

	if (!context || !function)
	{
		return;
	}

	CURRENT_CONTEXT (context)->functions =
		g_slist_append (CURRENT_CONTEXT (context)->functions,
		                function);
}

/**
 * cdn_compile_context_lookup_variable:
 * @context: A #CdnCompileContext
 * @name: The property name
 *
 * Lookup a property in the list of context objects.
 *
 * Returns: (type CdnVariable) (transfer none): A #CdnVariable or %NULL if
                                                the property could not be found
 *
 **/
CdnVariable *
cdn_compile_context_lookup_variable (CdnCompileContext *context,
                                     const gchar       *name)
{
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (!context)
	{
		return NULL;
	}

	GSList *item;
	Context *ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CdnVariable *property;

		if (!item->data || !CDN_IS_OBJECT (item->data))
		{
			continue;
		}

		property = cdn_object_get_variable (CDN_OBJECT (item->data),
		                                    name);

		if (property)
		{
			return property;
		}
	}

	return NULL;
}

/**
 * cdn_compile_context_get_objects:
 * @context: A #CdnCompileContext
 *
 * Get the list of objects to be considered as context for compiling
 * expressions. This returns the internally used list which should not be
 * modified or freed.
 *
 * Returns: (element-type CdnObject) (transfer none): A #GSList of #CdnObject
 *
 **/
const GSList *
cdn_compile_context_get_objects (CdnCompileContext *context)
{
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), NULL);

	if (!context)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->objects;
}

/**
 * cdn_compile_context_get_functions:
 * @context: A #CdnCompileContext
 *
 * Get the list of custom user functions. This returns the internally used
 * list which should not be modified or freed.
 *
 * Returns: (element-type CdnFunction) (transfer none): A #GSList of #CdnFunction
 *
 **/
const GSList *
cdn_compile_context_get_functions (CdnCompileContext *context)
{
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), NULL);

	if (!context)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->functions;
}

/**
 * cdn_compile_context_lookup_function:
 * @context: A #CdnCompileContext
 * @name: The name of the function
 *
 * Lookup a custom user function.
 *
 * Returns: (type CdnFunction) (transfer none): A #CdnFunction or %NULL if
            the function could not be found
 *
 **/
CdnFunction *
cdn_compile_context_lookup_function (CdnCompileContext *context,
                                     const gchar       *name)
{
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (!context)
	{
		return NULL;
	}

	/* FIXME: bit inefficient */
	GSList *item;
	Context *ctx = CURRENT_CONTEXT (context);

	for (item = ctx->functions; item; item = g_slist_next (item))
	{
		CdnFunction *function;

		if (!item->data)
		{
			continue;
		}

		function = item->data;

		if (g_strcmp0 (cdn_object_get_id (CDN_OBJECT (function)), name) == 0)
		{
			return function;
		}
	}

	return NULL;
}

void
cdn_compile_context_set_function_ref_priority (CdnCompileContext *context,
                                               gboolean           prio)
{
	Context *ctx;

	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));

	if (context == NULL)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	ctx->function_priority = prio;
}

gboolean
cdn_compile_context_get_function_ref_priority (CdnCompileContext *context)
{
	Context *ctx;

	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), FALSE);

	if (context == NULL)
	{
		return FALSE;
	}

	ctx = CURRENT_CONTEXT (context);

	return ctx->function_priority;
}

void
cdn_compile_context_set_function_arg_priority (CdnCompileContext *context,
                                               gboolean           prio)
{
	Context *ctx;

	g_return_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context));

	if (context == NULL)
	{
		return;
	}

	ctx = CURRENT_CONTEXT (context);

	ctx->function_arg_priority = prio;
}

gboolean
cdn_compile_context_get_function_arg_priority (CdnCompileContext *context)
{
	Context *ctx;

	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), FALSE);

	if (context == NULL)
	{
		return FALSE;
	}

	ctx = CURRENT_CONTEXT (context);

	return ctx->function_arg_priority;
}
