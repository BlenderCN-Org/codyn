#include "cpg-compile-context.h"
#include "cpg-object.h"
#include "cpg-property.h"
#include "cpg-function.h"

/**
 * SECTION:cpg-compile-context
 * @short_description: The expression compile context
 *
 * The compile context provides information for compiling expressions such
 * as the available user defined functions and the objects that can be used
 * to lookup properties used in the expression.
 *
 */

#define CURRENT_CONTEXT(x) (CONTEXT (x->priv->contexts->data))
#define CONTEXT(x) ((Context *)x)

#define CPG_COMPILE_CONTEXT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_COMPILE_CONTEXT, CpgCompileContextPrivate))

typedef struct
{
	GSList *objects;
	GSList *functions;
	GSList *operators;
} Context;

struct _CpgCompileContextPrivate
{
	GSList *contexts;
};

G_DEFINE_TYPE (CpgCompileContext, cpg_compile_context, G_TYPE_OBJECT)

static void
context_free (Context *context)
{
	g_slist_free (context->objects);
	g_slist_free (context->functions);
	g_slist_free (context->operators);

	g_slice_free (Context, context);
}

static Context *
context_copy (Context *context)
{
	Context *ctx = g_slice_new0 (Context);

	ctx->objects = g_slist_copy (context->objects);
	ctx->functions = g_slist_copy (context->functions);
	ctx->operators = g_slist_copy (context->operators);

	return ctx;
}

static void
cpg_compile_context_finalize (GObject *object)
{
	CpgCompileContext *context = CPG_COMPILE_CONTEXT (object);

	g_slist_foreach (context->priv->contexts, (GFunc)context_free, NULL);
	g_slist_free (context->priv->contexts);

	G_OBJECT_CLASS (cpg_compile_context_parent_class)->finalize (object);
}

static void
cpg_compile_context_class_init (CpgCompileContextClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_compile_context_finalize;

	g_type_class_add_private (object_class, sizeof(CpgCompileContextPrivate));
}

static void
cpg_compile_context_init (CpgCompileContext *self)
{
	self->priv = CPG_COMPILE_CONTEXT_GET_PRIVATE (self);

	self->priv->contexts = g_slist_prepend (NULL, g_slice_new0 (Context));
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
cpg_compile_context_new ()
{
	return g_object_new (CPG_TYPE_COMPILE_CONTEXT, NULL);
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
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));

	if (!context)
	{
		return;
	}

	context->priv->contexts = g_slist_prepend (context->priv->contexts,
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
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));

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
 * cpg_compile_context_prepend_object:
 * @context: A #CpgCompileContext
 * @object: (type CpgObject): A #CpgObject
 *
 * Prepend a context object to the list of context objects.
 *
 **/
void
cpg_compile_context_prepend_object (CpgCompileContext *context,
                                    CpgObject         *object)
{
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (object == NULL || CPG_IS_OBJECT (object));

	if (!context || !object)
	{
		return;
	}

	CURRENT_CONTEXT (context)->objects =
		g_slist_prepend (CURRENT_CONTEXT (context)->objects,
		                 object);
}

/**
 * cpg_compile_context_append_object:
 * @context: A #CpgCompileContext
 * @object: (type CpgObject): A #CpgObject
 *
 * Append a context object to the list of context objects.
 *
 **/
void
cpg_compile_context_append_object (CpgCompileContext *context,
                                   CpgObject         *object)
{
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));
	g_return_if_fail (object == NULL || CPG_IS_OBJECT (object));

	if (!context || !object)
	{
		return;
	}

	CURRENT_CONTEXT (context)->objects =
		g_slist_append (CURRENT_CONTEXT (context)->objects,
		                object);
}

/**
 * cpg_compile_context_set_functions:
 * @context: A #CpgCompileContext
 * @functions: (element-type CpgFunction) (transfer none): A #GSList of #CpgFunction
 *
 * Set the list of user functions available in the compile context. This
 * function makes a copy of the list but not of its members.
 *
 **/
void
cpg_compile_context_set_functions (CpgCompileContext *context,
                                   GSList const      *functions)
{
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));

	if (!context)
	{
		return;
	}

	g_slist_free (CURRENT_CONTEXT (context)->functions);
	CURRENT_CONTEXT (context)->functions = g_slist_copy ((GSList *)functions);
}

/**
 * cpg_compile_context_set_operators:
 * @context: A #CpgCompileContext
 * @operators: (element-type CpgOperator) (transfer none): A #GSList of #CpgOperator
 *
 * Set the list of operators that can be used in expressions. This function
 * makes a copy of the list, but not of its members.
 *
 **/
void
cpg_compile_context_set_operators (CpgCompileContext *context,
                                   GSList const      *operators)
{
	g_return_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context));

	if (!context)
	{
		return;
	}

	g_slist_free (CURRENT_CONTEXT (context)->operators);
	CURRENT_CONTEXT (context)->operators = g_slist_copy ((GSList *)operators);
}

/**
 * cpg_compile_context_lookup_property:
 * @context: A #CpgCompileContext
 * @name: The property name
 *
 * Lookup a property in the list of context objects.
 *
 * Returns: (type CpgProperty): A #CpgProperty or %NULL if the property could not be found
 *
 **/
CpgProperty *
cpg_compile_context_lookup_property (CpgCompileContext *context,
                                     gchar const       *name)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (!context)
	{
		return NULL;
	}

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
 * Returns: (element-type CpgObject) (transfer none): A #GSList of #CpgObject
 *
 **/
GSList const *
cpg_compile_context_get_objects (CpgCompileContext *context)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);

	if (!context)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->objects;
}

/**
 * cpg_compile_context_get_functions:
 * @context: A #CpgCompileContext
 *
 * Get the list of custom user functions. This returns the internally used
 * list which should not be modified or freed.
 *
 * Returns: (element-type CpgFunction) (transfer none): A #GSList of #CpgFunction
 *
 **/
const GSList *
cpg_compile_context_get_functions (CpgCompileContext *context)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);

	if (!context)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->functions;
}

/**
 * cpg_compile_context_get_operators:
 * @context: A #CpgCompileContext
 *
 * Get the list of operators. This returns the internally used
 * list which should not be modified or freed.
 *
 * Returns: (element-type CpgOperator) (transfer none): A #GSList of #CpgOperator
 *
 **/
GSList const *
cpg_compile_context_get_operators (CpgCompileContext *context)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);

	if (!context)
	{
		return NULL;
	}

	return CURRENT_CONTEXT (context)->operators;
}

/**
 * cpg_compile_context_lookup_function:
 * @context: A #CpgCompileContext
 * @name: The name of the function
 *
 * Lookup a custom user function.
 *
 * Returns: (type CpgFunction): A #CpgFunction or %NULL if the function could not be found
 *
 **/
CpgFunction *
cpg_compile_context_lookup_function (CpgCompileContext *context,
                                     gchar const       *name)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);
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
		CpgFunction *function;

		if (!item->data)
		{
			continue;
		}

		function = item->data;

		if (g_strcmp0 (cpg_object_get_id (CPG_OBJECT (function)), name) == 0)
		{
			return function;
		}
	}

	return NULL;
}

/**
 * cpg_compile_context_lookup_operator:
 * @context: A #CpgCompileContext
 * @name: The name of the operator
 *
 * Lookup an operator by name
 *
 * Returns: A #CpgOperator or %NULL if the operator could not be found
 *
 **/
CpgOperator *
cpg_compile_context_lookup_operator (CpgCompileContext *context,
                                     gchar const       *name)
{
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (!context)
	{
		return NULL;
	}

	GSList *item;
	Context *ctx = CURRENT_CONTEXT (context);

	for (item = ctx->operators; item; item = g_slist_next (item))
	{
		CpgOperator *op;

		if (!item->data)
		{
			continue;
		}

		op = item->data;

		gchar *opname = cpg_operator_get_name (op);
		gboolean equal = (g_strcmp0 (name, opname) == 0);

		g_free (opname);

		if (equal)
		{
			return op;
		}
	}

	return NULL;
}
