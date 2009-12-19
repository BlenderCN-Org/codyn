#include "cpg-compile-context.h"
#include "cpg-ref-counted-private.h"
#include "cpg-object.h"
#include "cpg-property.h"
#include "cpg-function.h"

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

void
cpg_compile_context_save (CpgCompileContext *context)
{
	context->contexts = g_slist_prepend (context->contexts,
	                                     context_copy (CURRENT_CONTEXT (context)));
}

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

void
cpg_compile_context_prepend_object (CpgCompileContext *context,
                                    CpgObject         *object)
{
	CURRENT_CONTEXT (context)->objects = g_slist_prepend (CURRENT_CONTEXT (context)->objects,
	                                                      object);
}

void
cpg_compile_context_append_object (CpgCompileContext *context,
                                   CpgObject         *object)
{
	CURRENT_CONTEXT (context)->objects = g_slist_append (CURRENT_CONTEXT (context)->objects,
	                                                     object);
}

void
cpg_compile_context_set_functions (CpgCompileContext *context,
                                   GSList            *functions)
{
	g_slist_free (CURRENT_CONTEXT (context)->functions);
	CURRENT_CONTEXT (context)->functions = g_slist_copy (functions);
}

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

GSList *
cpg_compile_context_get_objects (CpgCompileContext *context)
{
	return CURRENT_CONTEXT (context)->objects;
}

GSList *
cpg_compile_context_get_functions (CpgCompileContext *context)
{
	return CURRENT_CONTEXT (context)->functions;
}

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
