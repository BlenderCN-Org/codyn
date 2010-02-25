#include "cpg-function.h"
#include "cpg-compile-error.h"
#include "cpg-debug.h"
#include "cpg-ref-counted-private.h"

/**
 * SECTION:function
 * @short_description: Custom user defined function
 *
 * It is possible to define custom user functions in the network which can
 * then be used from any expression. This class provides the basic
 * user function functionality. User defined functions can have optional
 * arguments with default values and can reference global constants as well
 * as use other user defined functions in their expressions.
 *
 * The #CpgFunction class can be subclassed to provide more specific types
 * of functions. One such example is the #CpgFunctionPolynomial class which
 * can be used to define and evaluate piecewise polynomials.
 *
 */

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
	GList *arguments;

	guint n_arguments;
	guint n_optional;
};

struct _CpgFunctionArgument
{
	CpgRefCounted parent;

	gchar *name;
	gboolean optional;
	gdouble def;

	CpgProperty *property;
};

G_DEFINE_TYPE (CpgFunction, cpg_function, CPG_TYPE_OBJECT)

GType
cpg_function_argument_get_type (void)
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgFunctionArgument",
		                                        cpg_ref_counted_ref,
		                                        cpg_ref_counted_unref);
	}
	
	return type_id;
}

static void
cpg_function_argument_free (CpgFunctionArgument *argument)
{
	g_free (argument->name);

	if (argument->property)
	{
		cpg_ref_counted_unref (argument->property);
	}

	g_slice_free (CpgFunctionArgument, argument);
}

/**
 * cpg_function_argument_new:
 * @name: The argument name
 * @optional: Whether the argument is optional
 * @def: The default value of an optional argument
 * 
 * Create a new function argument.
 *
 * Returns: A #CpgFunctionArgument
 *
 **/
CpgFunctionArgument *
cpg_function_argument_new (gchar const *name,
                           gboolean     optional,
                           gdouble      def)
{
	CpgFunctionArgument *arg = g_slice_new0 (CpgFunctionArgument);

	cpg_ref_counted_init (&(arg->parent), (GDestroyNotify)cpg_function_argument_free);

	arg->name = g_strdup (name);
	arg->optional = optional;
	arg->def = def;

	return arg;
}

static CpgFunctionArgument *
cpg_function_argument_copy (CpgFunctionArgument *argument)
{
	CpgFunctionArgument *ret = cpg_function_argument_new (argument->name,
	                                                      argument->optional,
	                                                      argument->def);

	return ret;
}

static void
cpg_function_finalize (GObject *object)
{
	CpgFunction *self = CPG_FUNCTION (object);

	if (self->priv->expression)
	{
		cpg_ref_counted_unref (self->priv->expression);
	}

	cpg_function_clear_arguments (self);

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
		gdouble ret = cpg_expression_evaluate (function->priv->expression);

		/* Don't cache results from functions */
		cpg_expression_reset_cache (function->priv->expression);
		return ret;
	}
	else
	{
		return 0;
	}
}

static void
cpg_function_execute_impl (CpgFunction *function, CpgStack *stack)
{
	GList *item;
	guint num_provided;

	/* Set arguments as properties in the object */
	if (function->priv->n_optional > 0)
	{
		num_provided = (guint)cpg_stack_pop (stack);
	}
	else
	{
		num_provided = function->priv->n_arguments;
	}

	guint i;
	GList *from = g_list_nth (function->priv->arguments, num_provided - 1);
	item = from;

	/* Set provided arguments */
	for (i = 0; i < num_provided; ++i)
	{
		CpgFunctionArgument *argument = (CpgFunctionArgument *)item->data;

		cpg_property_set_value (argument->property, cpg_stack_pop (stack));
		item = g_list_previous (item);
	}

	/* Set defaults for optional arguments */
	item = from ? g_list_next (from) : function->priv->arguments;
	while (item)
	{
		CpgFunctionArgument *argument = (CpgFunctionArgument *)item->data;

		cpg_property_set_value (argument->property, argument->def);
		item = g_list_next (item);
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
	GList *item;
	
	for (item = source_function->priv->arguments; item; item = g_list_next (item))
	{
		CpgFunctionArgument *orig = (CpgFunctionArgument *)item->data;
		CpgFunctionArgument *argument = cpg_function_argument_copy (orig);

		cpg_function_add_argument (target, argument);
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
cpg_function_constructed (GObject *object)
{
	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression == NULL)
	{
		function->priv->expression = cpg_expression_new ("0");
	}
}

static void
cpg_function_class_init (CpgFunctionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_object_class = CPG_OBJECT_CLASS (klass);
	
	object_class->finalize = cpg_function_finalize;
	object_class->constructed = cpg_function_constructed;

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

/**
 * cpg_function_new:
 * @name: The function name
 * @expression: The function expression
 * 
 * Create a new custom user function. After creation, function arguments
 * can be added to the function using #cpg_function_add_argument.
 *
 * Returns: A #CpgFunction
 *
 **/
CpgFunction *
cpg_function_new (gchar const *name, gchar const *expression)
{
	CpgFunction *ret;
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

	return ret;
}

static gint
find_argument (CpgFunctionArgument *a1,
               CpgFunctionArgument *a2)
{
	return g_strcmp0 (a1->name, a2->name);
}

/**
 * cpg_function_add_argument:
 * @function: A #CpgFunction
 * @argument: A #CpgFunctionArgument
 * 
 * Add a function argument. A proxy property for the argument will be
 * automatically created if it does not exist yet. If the argument already
 * exists it will not be added.
 *
 **/
void
cpg_function_add_argument (CpgFunction         *function,
                           CpgFunctionArgument *argument)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_return_if_fail (argument != NULL);

	CpgProperty *property = cpg_object_get_property (CPG_OBJECT (function),
		                                             argument->name);

	if (property == NULL)
	{
		property = cpg_object_add_property (CPG_OBJECT (function),
		                                    argument->name, "0", FALSE);
	}
	else if (g_list_find_custom (function->priv->arguments,
	                             argument,
	                             (GCompareFunc)find_argument) != NULL)
	{
		return;
	}

	argument->property = cpg_ref_counted_ref (property);

	if (argument->optional)
	{
		/* Just append */
		function->priv->arguments = g_list_append (function->priv->arguments,
		                                            cpg_ref_counted_ref (argument));
	}
	else
	{
		/* Insert before first optional */
		function->priv->arguments = g_list_insert (function->priv->arguments,
		                                            cpg_ref_counted_ref (argument),
		                                            (gint)function->priv->n_optional - 1);
	}
	
	++function->priv->n_arguments;

	if (argument->optional)
	{
		++function->priv->n_optional;
	}

	cpg_object_taint (CPG_OBJECT (function));
}

/**
 * cpg_function_remove_argument:
 * @function: A #CpgFunction
 * @argument: A #CpgFunctionArgument
 * 
 * Remove a function argument.
 *
 **/
void
cpg_function_remove_argument (CpgFunction         *function,
                              CpgFunctionArgument *argument)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_return_if_fail (argument != NULL);

	GList *item = g_list_find (function->priv->arguments, argument);

	if (item == NULL)
	{
		return;
	}

	if (cpg_object_remove_property (CPG_OBJECT (function),
	                                cpg_property_get_name (argument->property),
	                                NULL))
	{
		if (argument->optional)
		{
			--function->priv->n_optional;
		}

		function->priv->arguments = g_list_delete_link (function->priv->arguments,
		                                                 item);

		cpg_ref_counted_unref (argument);
		--function->priv->n_arguments;

		cpg_object_taint (CPG_OBJECT (function));
	}
}

/**
 * cpg_function_get_arguments:
 * @function: A #CpgFunction
 * 
 * Get the list of function arguments. The returned list is used internally
 * and should not be modified or freed.
 *
 * Returns: A #GList
 *
 **/
GList *
cpg_function_get_arguments (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), NULL);

	return function->priv->arguments;
}

/**
 * cpg_function_execute:
 * @function: A #CpgFunction
 * @stack: A #CpgStack
 * 
 * Execute the function. This is used internally when the function needs to be
 * evaluated.
 *
 **/
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

/**
 * cpg_function_get_expression:
 * @function: A #CpgFunction
 * 
 * Get the function expression.
 *
 * Returns: A #CpgExpression
 *
 **/
CpgExpression *
cpg_function_get_expression (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), NULL);

	return function->priv->expression;
}

/**
 * cpg_function_set_expression:
 * @function: A #CpgFunction
 * @expression: A #CpgExpression
 * 
 * Set the function expression.
 *
 **/
void
cpg_function_set_expression (CpgFunction   *function,
                             CpgExpression *expression)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_object_set (G_OBJECT (function), "expression", expression, NULL);
}

/**
 * cpg_function_clear_arguments:
 * @function: A #CpgFunction
 * 
 * Remove all the function arguments.
 *
 **/
void
cpg_function_clear_arguments (CpgFunction *function)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));

	GList *item = function->priv->arguments;
	GList *prev = NULL;

	while (item)
	{
		CpgFunctionArgument *argument = (CpgFunctionArgument *)item->data;

		if (cpg_object_remove_property (CPG_OBJECT (function),
		                                cpg_property_get_name (argument->property),
		                                NULL))
		{
			if (argument->optional)
			{
				--function->priv->n_optional;
			}

			cpg_ref_counted_unref (argument);
			
			function->priv->arguments = g_list_delete_link (function->priv->arguments,
			                                                 item);
			item = prev ? prev->next : function->priv->arguments;
			--function->priv->n_arguments;
		}
		else
		{
			prev = item;
			item = g_list_next (item);
		}
	}

	cpg_object_taint (CPG_OBJECT (function));
}

/**
 * cpg_function_get_n_optional:
 * @function: A #CpgFunction
 * 
 * Get the number of optional arguments. The optional arguments are always
 * at the end of the list of arguments of the function.
 *
 * Returns: the number of optional arguments
 *
 **/
guint
cpg_function_get_n_optional (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), 0);

	return function->priv->n_optional;
}

/**
 * cpg_function_get_n_arguments:
 * @function: A #CpgFunction
 * 
 * Get the number of arguments. This value is cached and is thus faster than
 * using #cpg_function_get_arguments and #g_list_length.
 *
 * Returns: the number of arguments
 *
 **/
guint
cpg_function_get_n_arguments (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), 0);

	return function->priv->n_arguments;
}

/**
 * cpg_function_argument_get_name:
 * @argument: A #CpgFunctionArgument
 * 
 * Get the function name.
 *
 * Returns: the function name
 *
 **/
gchar const *
cpg_function_argument_get_name (CpgFunctionArgument *argument)
{
	return argument->name;
}

/**
 * cpg_function_argument_get_optional:
 * @argument: A #CpgFunctionArgument
 * 
 * Get whether the function argument is optional. If the argument is optional
 * its default value can be obtained with
 * #cpg_function_argument_get_default_value
 *
 * Returns: whether the argument is optional
 *
 **/
gboolean
cpg_function_argument_get_optional (CpgFunctionArgument *argument)
{
	return argument->optional;
}

/**
 * cpg_function_argument_get_default_value:
 * @argument: A #CpgFunctionArgument
 * 
 * Get the function argument default value.
 *
 * Returns: the default value
 *
 **/
gdouble
cpg_function_argument_get_default_value (CpgFunctionArgument *argument)
{
	return argument->def;
}

/**
 * cpg_function_argument_get_property:
 * @argument: A #CpgFunctionArgument
 *
 * Get the underlying property that is the proxy for the function argument.
 * You should generally not need this. It is mainly used in cpgrawc.
 *
 * Returns: the underlying argument property
 *
 **/
CpgProperty *
cpg_function_argument_get_property (CpgFunctionArgument *argument)
{
	return argument->property;
}
