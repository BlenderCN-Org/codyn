/*
 * cpg-function.c
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

#include "cpg-function.h"
#include "cpg-compile-error.h"

/**
 * SECTION:cpg-function
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
 * <refsect2 id="CpgFunction-COPY">
 * <title>CpgFunction Copy Semantics</title>
 * When a function is copied with #cpg_object_copy, the function expression
 * and all the arguments are copied as well.
 * </refsect2>
 *
 */

#define CPG_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION, CpgFunctionPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_EXPRESSION
};

/* signals */
enum
{
	ARGUMENT_ADDED,
	ARGUMENT_REMOVED,
	ARGUMENTS_REORDERED,
	NUM_SIGNALS
};

struct _CpgFunctionPrivate
{
	CpgExpression *expression;
	GList *arguments;

	guint n_arguments;
	guint n_optional;
};

G_DEFINE_TYPE (CpgFunction, cpg_function, CPG_TYPE_OBJECT)

static guint signals[NUM_SIGNALS] = {0,};

GQuark
cpg_function_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_function_error");
	}

	return quark;
}


static void
cpg_function_finalize (GObject *object)
{
	CpgFunction *self = CPG_FUNCTION (object);

	if (self->priv->expression)
	{
		g_object_unref (self->priv->expression);
	}

	cpg_function_clear_arguments (self, NULL);

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
				g_object_unref (self->priv->expression);
			}

			self->priv->expression = g_value_dup_object (value);
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
			g_value_set_object (value, self->priv->expression);
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
		g_warning ("Error while parsing function expression [%s]<%s>: %s",
		           cpg_object_get_id (object),
		           cpg_expression_get_as_string (self->priv->expression),
		           gerror->message);

		if (error)
		{
			cpg_compile_error_set (error,
			                       gerror,
			                       object,
			                       NULL,
			                       NULL,
			                       cpg_expression_get_error_at (self->priv->expression));
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
cpg_function_execute_impl (CpgFunction *function,
                           guint        nargs,
                           CpgStack    *stack)
{
	GList *item;
	guint i;
	GList *from;

	from = g_list_nth (function->priv->arguments, nargs - 1);
	item = from;

	/* Set provided arguments */
	for (i = 0; i < nargs; ++i)
	{
		CpgFunctionArgument *argument = item->data;
		CpgProperty *property = _cpg_function_argument_get_property (argument);

		cpg_property_set_value (property, cpg_stack_pop (stack));
		item = g_list_previous (item);
	}

	/* Set defaults for optional arguments */
	item = from ? g_list_next (from) : function->priv->arguments;

	while (item)
	{
		CpgFunctionArgument *argument = item->data;
		CpgProperty *property = _cpg_function_argument_get_property (argument);

		cpg_property_set_value (property,
		                        cpg_function_argument_get_default_value (argument));

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
		gchar const *str;

		str = cpg_expression_get_as_string (source_function->priv->expression);

		g_object_set (target,
		              "expression", cpg_expression_new (str),
		              NULL);
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
cpg_function_foreach_expression_impl (CpgObject                *object,
                                      CpgForeachExpressionFunc  func,
                                      gpointer                  userdata)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->foreach_expression != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_parent_class)->foreach_expression (object,
		                                                                  func,
		                                                                  userdata);
	}

	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression)
	{
		func (function->priv->expression, userdata);
	}
}

static void
cpg_function_constructed (GObject *object)
{
	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression == NULL)
	{
		function->priv->expression = cpg_expression_new ("0");
		g_object_ref_sink (function->priv->expression);
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
	cpg_object_class->foreach_expression = cpg_function_foreach_expression_impl;

	klass->execute = cpg_function_execute_impl;
	klass->evaluate = cpg_function_evaluate_impl;

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgFunction::argument-added:
	 * @function: a #CpgFunction
	 * @argument: a #CpgFunctionArgument
	 *
	 * Emitted when an argument has been added to the function
	 *
	 **/
	signals[ARGUMENT_ADDED] =
		g_signal_new ("argument-added",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgFunctionClass,
		                               argument_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_FUNCTION_ARGUMENT);

	/**
	 * CpgFunction::argument-removed:
	 * @function: a #CpgFunction
	 * @argument: a #CpgFunctionArgument
	 *
	 * Emitted when an argument has been removed from the function
	 *
	 **/
	signals[ARGUMENT_REMOVED] =
		g_signal_new ("argument-removed",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgFunctionClass,
		                               argument_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_FUNCTION_ARGUMENT);

	/**
	 * Cpgfunction::arguments-reordered:
	 * @function: a #CpgFunction
	 *
	 * Emitted when the order of the function arguments has changed.
	 *
	 **/
	signals[ARGUMENTS_REORDERED] =
		g_signal_new ("arguments-reordered",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgFunctionClass, arguments_reordered),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

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
cpg_function_new (gchar const *name,
                  gchar const *expression)
{
	CpgFunction *ret;
	CpgExpression *expr = NULL;

	g_return_val_if_fail (name != NULL, NULL);

	if (expression != NULL)
	{
		expr = cpg_expression_new (expression);
	}

	ret = g_object_new (CPG_TYPE_FUNCTION,
	                    "id", name,
	                    "expression", expr,
	                    NULL);

	return ret;
}

static gint
find_argument (CpgFunctionArgument *a1,
               CpgFunctionArgument *a2)
{
	return g_strcmp0 (cpg_function_argument_get_name (a1),
	                  cpg_function_argument_get_name (a2));
}

static gboolean
on_argument_invalidate_name (CpgFunctionArgument *argument,
                             gchar const         *name,
                             CpgFunction         *function)
{
	CpgProperty *property = cpg_object_get_property (CPG_OBJECT (function),
	                                                 name);

	CpgProperty *current = _cpg_function_argument_get_property (argument);

	return property && current != property;
}

static void
on_argument_name_changed (CpgFunctionArgument *argument,
                          GParamSpec          *spec,
                          CpgFunction         *function)
{
	gchar const *name = cpg_function_argument_get_name (argument);
	CpgProperty *property = cpg_object_get_property (CPG_OBJECT (function),
	                                                 name);

	CpgProperty *current = _cpg_function_argument_get_property (argument);

	if (property == current)
	{
		return;
	}
	else if (property || !cpg_property_set_name (current, name))
	{
		cpg_function_argument_set_name (argument,
		                                cpg_property_get_name (current));
	}
}

static void
on_argument_optional_changed (CpgFunctionArgument *argument,
                              GParamSpec          *spec,
                              CpgFunction         *function)
{
	gboolean opt = cpg_function_argument_get_optional (argument);

	GList *item;

	/* Get the item which represents the first optional argument at the
	   moment */
	item = g_list_nth (function->priv->arguments,
	                   function->priv->n_arguments - function->priv->n_optional);

	if (item->data != argument)
	{
		/* An argument other than the first optional one has changed
		   it optionality */

		/* First remove the argument from the list of arguments */
		function->priv->arguments =
			g_list_remove (function->priv->arguments,
			               argument);

		if (!opt)
		{
			function->priv->arguments =
				g_list_insert (function->priv->arguments,
				               argument,
				               function->priv->n_optional);
		}
		else
		{
			function->priv->arguments =
				g_list_append (function->priv->arguments,
				               argument);
		}
	}

	if (opt)
	{
		++function->priv->n_optional;
	}
	else
	{
		--function->priv->n_optional;
	}

	g_signal_emit (function, signals[ARGUMENTS_REORDERED], 0);
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
	g_return_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument));
	g_return_if_fail (_cpg_function_argument_get_property (argument) == NULL);

	gchar const *name = cpg_function_argument_get_name (argument);

	CpgProperty *property = cpg_object_get_property (CPG_OBJECT (function),
	                                                 name);

	if (property == NULL)
	{
		/* Add the proxy property */
		property = cpg_property_new (name, "0", CPG_PROPERTY_FLAG_NONE);

		if (!cpg_object_add_property (CPG_OBJECT (function), property, NULL))
		{
			return;
		}
	}
	else if (g_list_find_custom (function->priv->arguments,
	                             argument,
	                             (GCompareFunc)find_argument) != NULL)
	{
		return;
	}

	_cpg_function_argument_set_property (argument, property);

	if (cpg_function_argument_get_optional (argument))
	{
		/* Just append */
		function->priv->arguments = g_list_append (function->priv->arguments,
		                                           g_object_ref_sink (argument));
	}
	else
	{
		/* Insert before first optional */
		function->priv->arguments = g_list_insert (function->priv->arguments,
		                                           g_object_ref_sink (argument),
		                                           (gint)function->priv->n_optional - 1);
	}

	++function->priv->n_arguments;

	if (cpg_function_argument_get_optional (argument))
	{
		++function->priv->n_optional;
	}

	g_signal_connect (argument,
	                  "notify::name",
	                  G_CALLBACK (on_argument_name_changed),
	                  function);

	g_signal_connect (argument,
	                  "invalidate-name",
	                  G_CALLBACK (on_argument_invalidate_name),
	                  function);

	g_signal_connect (argument,
	                  "notify::optional",
	                  G_CALLBACK (on_argument_optional_changed),
	                  function);

	cpg_object_taint (CPG_OBJECT (function));

	g_signal_emit (function, signals[ARGUMENT_ADDED], 0, argument);
}

/**
 * cpg_function_remove_argument:
 * @function: A #CpgFunction
 * @argument: A #CpgFunctionArgument
 * @error: A #GError
 *
 * Remove a function argument.
 *
 * Returns: %TRUE if the argument could be removed, %FALSE otherwise
 *
 **/
gboolean
cpg_function_remove_argument (CpgFunction          *function,
                              CpgFunctionArgument  *argument,
                              GError              **error)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), FALSE);
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), FALSE);

	GList *item = g_list_find (function->priv->arguments, argument);

	if (item == NULL)
	{
		if (error)
		{
			g_set_error (error,
			             cpg_function_error_quark (),
			             CPG_FUNCTION_ERROR_ARGUMENT_NOT_FOUND,
			             "Function argument %s not found",
			             cpg_function_argument_get_name (argument));
		}

		return FALSE;
	}

	if (cpg_object_remove_property (CPG_OBJECT (function),
	                                cpg_function_argument_get_name (argument),
	                                error))
	{
		if (cpg_function_argument_get_optional (argument))
		{
			--function->priv->n_optional;
		}

		function->priv->arguments = g_list_delete_link (function->priv->arguments,
		                                                item);

		_cpg_function_argument_set_property (argument, NULL);

		--function->priv->n_arguments;

		g_signal_handlers_disconnect_by_func (argument,
		                                      on_argument_invalidate_name,
		                                      function);

		g_signal_handlers_disconnect_by_func (argument,
		                                      on_argument_name_changed,
		                                      function);

		g_signal_handlers_disconnect_by_func (argument,
		                                      on_argument_optional_changed,
		                                      function);

		cpg_object_taint (CPG_OBJECT (function));

		g_signal_emit (function, signals[ARGUMENT_REMOVED], 0, argument);

		g_object_unref (argument);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_function_get_arguments:
 * @function: A #CpgFunction
 *
 * Get the list of function arguments. The returned list is used internally
 * and should not be modified or freed.
 *
 * Returns: (element-type CpgFunctionArgument) (transfer none): A #GList
 *
 **/
const GList *
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
cpg_function_execute (CpgFunction *function,
                      guint        nargs,
                      CpgStack    *stack)
{
	g_return_if_fail (CPG_IS_FUNCTION (function));
	g_return_if_fail (stack != NULL);

	if (CPG_FUNCTION_GET_CLASS (function)->execute)
	{
		CPG_FUNCTION_GET_CLASS (function)->execute (function,
		                                            nargs,
		                                            stack);
	}
}

/**
 * cpg_function_get_expression:
 * @function: A #CpgFunction
 *
 * Get the function expression.
 *
 * Returns: (type CpgExpression) (transfer none): A #CpgExpression
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
 * @error: A #GError
 *
 * Remove all the function arguments.
 *
 * Returns: %TRUE if all arguments could be successfully removed, %FALSE otherwise
 *
 **/
gboolean
cpg_function_clear_arguments (CpgFunction  *function,
                              GError      **error)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), FALSE);

	GList *copy = g_list_copy (function->priv->arguments);
	GList *item;

	for (item = copy; item; item = g_list_next (item))
	{
		if (!cpg_function_remove_argument (function, item->data, error))
		{
			g_list_free (copy);
			return FALSE;
		}
	}

	g_list_free (copy);
	return TRUE;
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
