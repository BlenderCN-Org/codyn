#include "cpg-function.h"
#include "cpg-compile-error.h"
#include "cpg-debug.h"

#define CPG_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION, CpgFunctionPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_EXPRESSION
};

struct _CpgFunctionPrivate
{
	CpgExpression *expression;
	GSList *arguments;

	guint n_arguments;
};

G_DEFINE_TYPE (CpgFunction, cpg_function, CPG_TYPE_OBJECT)

static void
clear_arguments (CpgFunction *function)
{
	g_slist_foreach (function->priv->arguments, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (function->priv->arguments);

	function->priv->arguments = NULL;
	function->priv->n_arguments = 0;
}
static void
cpg_function_finalize (GObject *object)
{
	CpgFunction *self = CPG_FUNCTION (object);

	if (self->priv->expression)
	{
		cpg_ref_counted_unref (self->priv->expression);
	}

	clear_arguments (self);

	G_OBJECT_CLASS (cpg_function_parent_class)->finalize (object);
}

static void
cpg_function_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgFunction *self = CPG_FUNCTION (object);
	
	switch (prop_id)
	{
		case PROP_EXPRESSION:
			if (self->priv->expression)
			{
				cpg_ref_counted_unref (self->priv->expression);
			}

			self->priv->expression = g_value_dup_boxed (value);
			cpg_object_taint (CPG_OBJECT (self));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_function_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CpgFunction *self = CPG_FUNCTION (object);
	
	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_boxed (value, self->priv->expression);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cpg_function_compile_impl (CpgObject         *object,
                           CpgCompileContext *context,
                           CpgCompileError   *error)
{
	CpgFunction *self = CPG_FUNCTION (object);
	GError *gerror = NULL;
	gboolean ret = TRUE;

	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->compile)
	{
		if (!CPG_OBJECT_CLASS (cpg_function_parent_class)->compile (object,
		                                                            context,
		                                                            error))
		{
			return FALSE;
		}
	}

	if (!self->priv->expression)
	{
		return TRUE;
	}

	cpg_compile_context_save (context);
	cpg_compile_context_prepend_object (context, object);

	if (!cpg_expression_compile (self->priv->expression,
		                         context,
		                         &gerror))
	{
		cpg_debug_error ("Error while parsing function expression [%s]<%s>: %s",
		                 cpg_object_get_id (object), 
		                 cpg_expression_get_as_string (self->priv->expression),
		                 gerror->message);

		if (error)
		{
			cpg_compile_error_set (error, gerror, object, NULL, NULL);
		}

		g_error_free (gerror);
		ret = FALSE;
	}

	cpg_compile_context_restore (context);
	return ret;
}

static gdouble
cpg_function_evaluate_impl (CpgFunction *function)
{
	if (function->priv->expression)
	{
		return cpg_expression_evaluate (function->priv->expression);
	}
	else
	{
		return 0;
	}
}

static void
cpg_function_execute_impl (CpgFunction *function, CpgStack *stack)
{
	GSList *item;

	/* Set arguments as properties in the object */
	for (item = function->priv->arguments; item; item = g_slist_next (item))
	{
		cpg_property_set_value ((CpgProperty *)item->data, cpg_stack_pop (stack));
	}

	/* Evaluate the expression */
	if (CPG_FUNCTION_GET_CLASS (function)->evaluate)
	{
		cpg_stack_push (stack, CPG_FUNCTION_GET_CLASS (function)->evaluate (function));
	}
	else
	{
		cpg_stack_push (stack, 0);
	}
}

static void
cpg_function_copy_impl (CpgObject *object,
                        CpgObject *source)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->copy != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_parent_class)->copy (object, source);
	}
	
	// Copy expression
	CpgFunction *source_function = CPG_FUNCTION (source);
	CpgFunction *target = CPG_FUNCTION (object);

	if (source_function->priv->expression)
	{
		CpgExpression *expr = cpg_expression_new (cpg_expression_get_as_string (source_function->priv->expression));

		g_object_set (target, "expression", expr, NULL);
		cpg_ref_counted_unref (expr);
	}

	// Copy arguments
	GSList *item;
	
	for (item = source_function->priv->arguments; item; item = g_slist_next (item))
	{
		CpgProperty *prop = (CpgProperty *)item->data;
		cpg_function_add_argument (target, cpg_property_get_name (prop));
	}
}

static void
cpg_function_reset_impl (CpgObject *object)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->reset != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_parent_class)->reset (object);
	}

	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression)
	{
		cpg_expression_reset (function->priv->expression);
	}
}

static void
cpg_function_reset_cache_impl (CpgObject *object)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->reset_cache != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_parent_class)->reset_cache (object);
	}

	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression)
	{
		cpg_expression_reset_cache (function->priv->expression);
	}
}

static void
cpg_function_class_init (CpgFunctionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_object_class = CPG_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_function_finalize;

	object_class->set_property = cpg_function_set_property;
	object_class->get_property = cpg_function_get_property;

	cpg_object_class->compile = cpg_function_compile_impl;
	cpg_object_class->copy = cpg_function_copy_impl;
	cpg_object_class->reset = cpg_function_reset_impl;
	cpg_object_class->reset_cache = cpg_function_reset_cache_impl;

	klass->execute = cpg_function_execute_impl;
	klass->evaluate = cpg_function_evaluate_impl;

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_boxed ("expression",
	                                                     "Expression",
	                                                     "Expression",
	                                                     CPG_TYPE_EXPRESSION,
	                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
	

	g_type_class_add_private (object_class, sizeof(CpgFunctionPrivate));
}

static void
cpg_function_init (CpgFunction *self)
{
	self->priv = CPG_FUNCTION_GET_PRIVATE (self);
}

CpgFunction *
cpg_function_new (gchar const *name, gchar const *expression, ...)
{
	CpgFunction *ret;
	va_list ap;
	CpgExpression *expr;

	g_return_val_if_fail (name != NULL, NULL);

	if (expression != NULL)
	{
		expr = cpg_expression_new (expression);
	}
	else
	{
		expr = NULL;
	}

	ret = g_object_new (CPG_TYPE_FUNCTION,
	                    "id", name,
	                    "expression", expr,
	                    NULL);

	if (expr != NULL)
	{
		cpg_ref_counted_unref (expr);
	}

	va_start(ap, expression);
	cpg_function_set_argumentsv(ret, ap);
	va_end(ap);

	return ret;
}

void
cpg_function_set_argumentsv (CpgFunction *function, va_list args)
{
	gchar const *arg;

	g_return_if_fail (CPG_IS_FUNCTION (function));

	clear_arguments (function);

	while ((arg = va_arg(args, gchar const *)) != NULL)
	{
		cpg_function_add_argument (function, arg);
	}
}

void
cpg_function_add_argument (CpgFunction *function,
                           gchar const *argument)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_return_if_fail (argument != NULL);

	CpgProperty *property = cpg_object_get_property (CPG_OBJECT (function),
		                                             argument);

	if (property == NULL)
	{
		property = cpg_object_add_property (CPG_OBJECT (function),
		                                    argument, "0", FALSE);
	}

	function->priv->arguments = g_slist_append (function->priv->arguments,
	                                            cpg_ref_counted_ref (property));
	++function->priv->n_arguments;

	cpg_object_taint (CPG_OBJECT (function));
}

void
cpg_function_set_arguments (CpgFunction *function, ...)
{
	va_list ap;

	g_return_if_fail (CPG_IS_FUNCTION (function));

	va_start (ap, function);
	cpg_function_set_argumentsv (function, ap);
	va_end (ap);
}

guint
cpg_function_get_n_arguments (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), 0);
	return function->priv->n_arguments;
}

GSList *
cpg_function_get_arguments (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), NULL);

	return function->priv->arguments;
}

void
cpg_function_execute (CpgFunction *function, CpgStack *stack)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_return_if_fail (stack != NULL);

	if (CPG_FUNCTION_GET_CLASS (function)->execute)
	{
		CPG_FUNCTION_GET_CLASS (function)->execute (function, stack);
	}
}

CpgExpression *
cpg_function_get_expression (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), NULL);

	return function->priv->expression;
}
