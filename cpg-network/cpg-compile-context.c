/*
 * cpg-compile-context.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-compile-context.h"
#include "cpg-ref-counted-private.h"
#include "cpg-object.h"
#include "cpg-property.h"
#include "cpg-function.h"

/**
 * SECTION:compile-context
 * @short_description: The expression compile context
 *
 * The compile context provides information for compiling expressions such
 * as the available user defined functions and the objects that can be used
 * to lookup properties used in the expression.
 *
 */
 
#define CURRENT_CONTEXT(x) (CONTEXT (x->contexts->data))
#define CONTEXT(x) ((Context *)x)

typedef struct
{
	GSList *objects;
	GSList *functions;
} Context;

struct _CpgCompileContext
{
	CpgRefCounted parent;

	GSList *contexts;
};

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

	return ctx;
}

static void
cpg_compile_context_free (CpgCompileContext *context)
{
	g_slist_foreach (context->contexts, (GFunc)context_free, NULL);
	g_slist_free (context->contexts);
}

/**
 * cpg_compile_context_new:
 * 
 * Create a new compile context.
 *
 * Returns: A #CpgCompileContext
 *
 **/
CpgCompileContext *
cpg_compile_context_new (void)
{
	CpgCompileContext *res = g_slice_new0 (CpgCompileContext);
	cpg_ref_counted_init (res, (GDestroyNotify)cpg_compile_context_free);

	res->contexts = g_slist_prepend (NULL, g_slice_new0 (Context));

	return res;
}

GType 
cpg_compile_context_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgCompileContext", cpg_ref_counted_ref, cpg_ref_counted_unref);
	}
	
	return type_id;
}

/**
 * cpg_compile_context_save:
 * @context: A #CpgCompileContext
 * 
 * Save the current state of the compile context. You can use this to alter
 * the context and restore it to its previous state later. Calls to this
 * function can be nested, but care should be taken to match each call
 * with #cpg_compile_context_restore.
 *
 **/
void
cpg_compile_context_save (CpgCompileContext *context)
{
	context->contexts = g_slist_prepend (context->contexts,
	                                     context_copy (CURRENT_CONTEXT (context)));
}

/**
 * cpg_compile_context_restore:
 * @context: A #CpgCompileContext
 * 
 * Restore the previous state of the compile context. Each call to restore
 * must be matched by a previous call to #cpg_compile_context_save.
 *
 **/
void
cpg_compile_context_restore (CpgCompileContext *context)
{
	if (!context->contexts || !context->contexts->next)
	{
		return;
	}

	context_free (CURRENT_CONTEXT (context));
	context->contexts = g_slist_delete_link (context->contexts, context->contexts);
}

/**
 * cpg_compile_context_prepend_object:
 * @context: A #CpgCompileContext
 * @object: A #CpgObject
 * 
 * Prepend a context object to the list of context objects.
 *
 **/
void
cpg_compile_context_prepend_object (CpgCompileContext *context,
                                    CpgObject         *object)
{
	CURRENT_CONTEXT (context)->objects = g_slist_prepend (CURRENT_CONTEXT (context)->objects,
	                                                      object);
}

/**
 * cpg_compile_context_append_object:
 * @context: A #CpgCompileContext
 * @object: A #CpgObject
 * 
 * Append a context object to the list of context objects.
 *
 **/
void
cpg_compile_context_append_object (CpgCompileContext *context,
                                   CpgObject         *object)
{
	CURRENT_CONTEXT (context)->objects = g_slist_append (CURRENT_CONTEXT (context)->objects,
	                                                     object);
}

/**
 * cpg_compile_context_set_functions:
 * @context: A #CpgCompileContext
 * @functions: A #GSList of #CpgFunction
 * 
 * Set the list of user functions available in the compile context. This
 * function makes a copy of the list but not of its members.
 *
 **/
void
cpg_compile_context_set_functions (CpgCompileContext *context,
                                   GSList            *functions)
{
	g_slist_free (CURRENT_CONTEXT (context)->functions);
	CURRENT_CONTEXT (context)->functions = g_slist_copy (functions);
}

/**
 * cpg_compile_context_lookup_property:
 * @context: A #CpgCompileContext
 * @name: The property name
 * 
 * Lookup a property in the list of context objects.
 *
 * Returns: A #CpgProperty or %NULL if the property could not be found
 *
 **/
CpgProperty *
cpg_compile_context_lookup_property (CpgCompileContext *context,
                                     gchar const       *name)
{
	GSList *item;
	Context *ctx = CURRENT_CONTEXT (context);

	for (item = ctx->objects; item; item = g_slist_next (item))
	{
		CpgProperty *property;

		if (!item->data || !CPG_IS_OBJECT (item->data))
		{
			continue;
		}

		property = cpg_object_get_property (CPG_OBJECT (item->data),
		                                    name);

		if (property)
		{
			return property;
		}
	}

	return NULL;
}

/**
 * cpg_compile_context_get_objects:
 * @context: A #CpgCompileContext
 * 
 * Get the list of objects to be considered as context for compiling
 * expressions. This returns the internally used list which should not be
 * modified or freed.
 *
 * Returns: A #GSList of #CpgObject
 *
 **/
GSList *
cpg_compile_context_get_objects (CpgCompileContext *context)
{
	return CURRENT_CONTEXT (context)->objects;
}

/**
 * cpg_compile_context_get_functions:
 * @context: A #CpgCompileContext
 * 
 * Get the list of custom user functions. This returns the internally used
 * list which should not be modified or freed.
 *
 * Returns: A #GSList of #CpgFunction
 *
 **/
GSList *
cpg_compile_context_get_functions (CpgCompileContext *context)
{
	return CURRENT_CONTEXT (context)->functions;
}

/**
 * cpg_compile_context_lookup_function:
 * @context: A #CpgCompileContext
 * @name: The name of the function
 * 
 * Lookup a custom user function.
 *
 * Returns: A #CpgFunction or %NULL if the function could not be found
 *
 **/
CpgFunction *
cpg_compile_context_lookup_function (CpgCompileContext *context,
                                     gchar const       *name)
{
	/* FIXME: bit inefficient */
	GSList *item;
	Context *ctx = CURRENT_CONTEXT (context);

	for (item = ctx->functions; item; item = g_slist_next (item))
	{
		CpgFunction *function;

		if (!item->data)
		{
			continue;
		}

		function = (CpgFunction *)item->data;

		if (g_strcmp0 (cpg_object_get_id (CPG_OBJECT (function)), name) == 0)
		{
			return function;
		}
	}

	return NULL;
}
