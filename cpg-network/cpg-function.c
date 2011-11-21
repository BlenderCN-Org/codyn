/*
 * cpg-function.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-function.h"
#include "cpg-compile-error.h"
#include "cpg-expression-tree-iter.h"
#include <string.h>

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
	GHashTable *arguments_hash;

	guint n_arguments;
	guint n_optional;
	guint n_implicit;
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

	g_list_foreach (self->priv->arguments, (GFunc)g_object_unref, NULL);
	g_list_free (self->priv->arguments);

	g_hash_table_destroy (self->priv->arguments_hash);

	G_OBJECT_CLASS (cpg_function_parent_class)->finalize (object);
}

static void
cpg_function_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CpgFunction *self = CPG_FUNCTION (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
		{
			CpgExpression *expr;

			if (self->priv->expression)
			{
				g_object_unref (self->priv->expression);
			}

			expr = g_value_get_object (value);

			if (expr)
			{
				self->priv->expression = g_object_ref_sink (expr);
			}
			else
			{
				self->priv->expression = NULL;
			}

			if (self->priv->expression)
			{
				cpg_expression_set_has_cache (self->priv->expression,
				                              FALSE);
			}

			cpg_object_taint (CPG_OBJECT (self));
		}
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
	gboolean ret = TRUE;
	GList *item;

	if (cpg_object_is_compiled (object))
	{
		return TRUE;
	}

	if (context)
	{
		cpg_compile_context_save (context);
		g_object_ref (context);
	}

	context = cpg_object_get_compile_context (object, context);

	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->compile)
	{
		if (!CPG_OBJECT_CLASS (cpg_function_parent_class)->compile (object,
		                                                            context,
		                                                            error))
		{
			g_object_unref (context);
			return FALSE;
		}
	}

	for (item = self->priv->arguments; item; item = g_list_next (item))
	{
		CpgExpression *expr;

		expr = cpg_function_argument_get_default_value (item->data);

		if (expr)
		{
			if (!cpg_expression_compile (expr, context, error))
			{
				if (error)
				{
					cpg_compile_error_set (error,
					                       NULL,
					                       object,
					                       NULL,
					                       NULL,
					                       NULL);
				}

				ret = FALSE;

				break;
			}
		}
	}

	if (!self->priv->expression || !ret)
	{
		cpg_compile_context_restore (context);
		g_object_unref (context);
		return ret;
	}

	if (!cpg_expression_compile (self->priv->expression,
	                             context,
	                             error))
	{
		if (error)
		{
			cpg_compile_error_set (error,
			                       NULL,
			                       object,
			                       NULL,
			                       NULL,
			                       NULL);
		}

		ret = FALSE;
	}

	cpg_compile_context_restore (context);
	g_object_unref (context);

	return ret;
}

static gdouble
cpg_function_evaluate_impl (CpgFunction *function)
{
	if (function->priv->expression)
	{
		gdouble ret = cpg_expression_evaluate (function->priv->expression);
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
		gdouble val;
		CpgProperty *property = _cpg_function_argument_get_property (argument);

		val = cpg_stack_pop (stack);

		cpg_property_set_value (property, val);
		item = g_list_previous (item);
	}

	/* Set defaults for optional arguments */
	item = from ? g_list_next (from) : function->priv->arguments;

	while (item)
	{
		CpgFunctionArgument *argument = item->data;
		CpgProperty *property = _cpg_function_argument_get_property (argument);

		CpgExpression *def;

		def = cpg_function_argument_get_default_value (argument);

		if (def)
		{
			cpg_property_set_value (property,
			                        cpg_expression_evaluate (def));
		}
		else
		{
			cpg_property_set_value (property, 0);
		}

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
		g_object_set (target,
		              "expression",
		              cpg_expression_copy (source_function->priv->expression),
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

	GList *item;

	for (item = function->priv->arguments; item; item = g_list_next (item))
	{
		CpgExpression *def;

		def = cpg_function_argument_get_default_value (item->data);

		if (def != NULL)
		{
			cpg_expression_reset (def);
		}
	}
}

static void
cpg_function_foreach_expression_impl (CpgObject                *object,
                                      CpgForeachExpressionFunc  func,
                                      gpointer                  userdata)
{
	CpgFunction *function = CPG_FUNCTION (object);
	GList *item;

	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->foreach_expression != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_parent_class)->foreach_expression (object,
		                                                                  func,
		                                                                  userdata);
	}

	for (item = function->priv->arguments; item; item = g_list_next (item))
	{
		CpgExpression *def;

		def = cpg_function_argument_get_default_value (item->data);

		if (def != NULL)
		{
			func (def, userdata);
		}
	}

	if (function->priv->expression)
	{
		func (function->priv->expression, userdata);
	}
}

static void
cpg_function_constructed (GObject *object)
{
	CpgFunction *function = CPG_FUNCTION (object);

	if (function->priv->expression != NULL)
	{
		g_object_ref_sink (function->priv->expression);
		g_object_unref (function->priv->expression);
	}
}

static void
cpg_function_template_expression_changed (CpgFunction *function,
                                          GParamSpec  *spec,
                                          CpgFunction *templ)
{
	GSList const *templates;
	GSList *last;

	templates = cpg_object_get_applied_templates (CPG_OBJECT (function));
	last = g_slist_last ((GSList *)templates);

	if (!last)
	{
		return;
	}

	if (templ == last->data)
	{
		g_object_set (function,
		              "expression",
		              cpg_expression_copy (cpg_function_get_expression (templ)),
		              NULL);
	}
}

static CpgFunction *
last_template (CpgFunction *target)
{
	CpgFunction *ret = NULL;
	GSList const *templates;

	templates = cpg_object_get_applied_templates (CPG_OBJECT (target));

	while (templates)
	{
		if (CPG_IS_FUNCTION (templates->data))
		{
			ret = templates->data;
		}

		templates = g_slist_next (templates);
	}

	return ret;
}

static gboolean
is_from_template (CpgFunction *target)
{
	CpgFunction *templ;

	templ = last_template (target);

	if (!templ)
	{
		return target->priv->expression == NULL &&
		       target->priv->n_arguments == 0;
	}

	return cpg_object_equal (CPG_OBJECT (target), CPG_OBJECT (templ));
}

static void
from_template (CpgFunction *target,
               CpgFunction *source)
{
	GList *item;

	g_list_foreach (target->priv->arguments,
	                (GFunc)g_object_unref,
	                NULL);

	g_list_free (target->priv->arguments);
	target->priv->arguments = NULL;

	target->priv->n_arguments = 0;
	target->priv->n_optional = 0;
	target->priv->n_implicit = 0;

	cpg_function_template_expression_changed (target, NULL, source);

	for (item = source->priv->arguments; item; item = g_list_next (item))
	{
		cpg_function_add_argument (target,
		                           cpg_function_argument_copy (item->data));
	}
}

static void
cpg_function_template_argument_added (CpgFunction         *source,
                                      CpgFunctionArgument *arg,
                                      CpgFunction         *target)
{
	if (last_template (target) == source)
	{
		cpg_function_add_argument (target, cpg_function_argument_copy (arg));
	}
}

static gboolean
arguments_equal (CpgFunction         *a,
                 CpgFunction         *b,
                 CpgFunctionArgument *ignore)
{
	GList *arg1;
	GList *arg2;

	arg1 = a->priv->arguments;
	arg2 = a->priv->arguments;

	while (arg1 != NULL && arg2 != NULL)
	{
		CpgFunctionArgument *a1;
		CpgFunctionArgument *a2;

		if ((arg1 == NULL) != (arg2 == NULL))
		{
			return FALSE;
		}

		a1 = arg1->data;
		a2 = arg2->data;

		arg1 = g_list_next (arg1);
		arg2 = g_list_next (arg2);

		if (ignore == a1)
		{
			continue;
		}

		if (g_strcmp0 (cpg_function_argument_get_name (a1),
		               cpg_function_argument_get_name (a2)) != 0)
		{
			return FALSE;
		}

		if (cpg_function_argument_get_explicit (a1) !=
		    cpg_function_argument_get_explicit (a2))
		{
			return FALSE;
		}

		if (cpg_function_argument_get_optional (a1) !=
		    cpg_function_argument_get_optional (a2))
		{
			return FALSE;
		}

		if (cpg_function_argument_get_optional (a1))
		{
			if (!cpg_expression_equal (cpg_function_argument_get_default_value (a1),
			                           cpg_function_argument_get_default_value (a2)))
			{
				return FALSE;
			}
		}
	}

	return TRUE;
}

static void
cpg_function_template_argument_removed (CpgFunction         *source,
                                        CpgFunctionArgument *arg,
                                        CpgFunction         *target)
{
	CpgFunction *templ;
	CpgFunctionArgument *item;

	item = cpg_function_get_argument (target, cpg_function_argument_get_name (arg));

	templ = last_template (target);

	if (templ &&
	    item &&
	    arguments_equal (target, templ, item) &&
	    cpg_expression_equal (target->priv->expression,
	                          templ->priv->expression))
	{
		cpg_function_remove_argument (target,
		                              item,
		                              NULL);
	}
}

static gboolean
cpg_function_apply_template_impl (CpgObject  *object,
                                  CpgObject  *templ,
                                  GError    **error)
{
	CpgFunction *target = NULL;
	CpgFunction *source = NULL;
	gboolean apply;

	target = CPG_FUNCTION (object);

	if (CPG_IS_FUNCTION (templ))
	{
		source = CPG_FUNCTION (templ);
	}

	apply = source && is_from_template (target);

	/* Remove all function arguments */
	if (apply)
	{
		g_list_foreach (target->priv->arguments,
		                (GFunc)g_object_unref,
		                NULL);

		g_list_free (target->priv->arguments);
		target->priv->arguments = NULL;

		target->priv->n_arguments = 0;
		target->priv->n_optional = 0;
		target->priv->n_implicit = 0;
	}

	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->apply_template)
	{
		if (!CPG_OBJECT_CLASS (cpg_function_parent_class)->apply_template (object, templ, error))
		{
			return FALSE;
		}
	}

	if (source)
	{
		g_signal_connect_swapped (source,
		                          "notify::expression",
		                          G_CALLBACK (cpg_function_template_expression_changed),
		                          target);

		g_signal_connect (source,
		                  "argument-added",
		                  G_CALLBACK (cpg_function_template_argument_added),
		                  target);

		g_signal_connect (source,
		                  "argument-removed",
		                  G_CALLBACK (cpg_function_template_argument_removed),
		                  target);
	}

	if (apply)
	{
		from_template (target, source);
	}

	return TRUE;
}


static gboolean
cpg_function_unapply_template_impl (CpgObject  *object,
                                    CpgObject  *templ,
                                    GError    **error)
{
	GSList *last;
	GSList const *templates;
	gboolean waslast = FALSE;
	CpgFunction *target = NULL;
	CpgFunction *source = NULL;

	templates = cpg_object_get_applied_templates (object);
	last = g_slist_last ((GSList *)templates);

	target = CPG_FUNCTION (object);

	/* Remove all function arguments */
	if (CPG_IS_FUNCTION (templ) && last && last->data == templ)
	{
		source = CPG_FUNCTION (templ);

		g_list_foreach (target->priv->arguments,
		                (GFunc)g_object_unref,
		                NULL);

		g_list_free (target->priv->arguments);
		target->priv->arguments = NULL;

		target->priv->n_arguments = 0;
		target->priv->n_optional = 0;
		target->priv->n_implicit = 0;

		waslast = TRUE;
	}

	if (CPG_OBJECT_CLASS (cpg_function_parent_class)->unapply_template)
	{
		if (!CPG_OBJECT_CLASS (cpg_function_parent_class)->unapply_template (object, templ, error))
		{
			return FALSE;
		}
	}

	if (source)
	{
		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cpg_function_template_argument_added),
		                                      object);

		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cpg_function_template_argument_removed),
		                                      object);

		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cpg_function_template_expression_changed),
		                                      object);
	}

	if (waslast)
	{
		templates = cpg_object_get_applied_templates (object);
		last = g_slist_last ((GSList *)templates);

		if (last)
		{
			from_template (source, last->data);
		}
		else
		{
			g_object_set (source, "expression", NULL, NULL);
		}
	}

	return TRUE;
}

static gboolean
cpg_function_equal_impl (CpgObject *a,
                         CpgObject *b)
{
	CpgFunction *fa;
	CpgFunction *fb;

	if (!CPG_OBJECT_CLASS (cpg_function_parent_class)->equal (a, b))
	{
		return FALSE;
	}

	fa = CPG_FUNCTION (a);
	fb = CPG_FUNCTION (b);

	if (!arguments_equal (fa, fb, NULL))
	{
		return FALSE;
	}

	return cpg_expression_equal (fa->priv->expression, fb->priv->expression);
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
	                   function->priv->n_arguments - function->priv->n_optional - function->priv->n_implicit);

	if (item == NULL || item->data != argument)
	{
		/* An argument other than the first optional one has changed
		   it optionality */

		/* First remove the argument from the list of arguments */
		function->priv->arguments =
			g_list_remove (function->priv->arguments,
			               argument);

		if (opt || item)
		{
			function->priv->arguments =
				g_list_insert_before (function->priv->arguments,
				                      item,
				                      argument);
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

static void
cpg_function_argument_added_impl (CpgFunction         *function,
                                  CpgFunctionArgument *argument)
{
	gchar const *name;

	if (!cpg_function_argument_get_explicit (argument))
	{
		/* Just append */
		function->priv->arguments = g_list_append (function->priv->arguments,
		                                           g_object_ref_sink (argument));
	}
	else if (cpg_function_argument_get_optional (argument))
	{
		/* Insert before first implicit */
		gint n;

		n = (gint)function->priv->n_arguments -
		    (gint)function->priv->n_implicit;

		function->priv->arguments = g_list_insert (function->priv->arguments,
		                                           g_object_ref_sink (argument),
		                                           n);
	}
	else
	{
		/* Insert before first optional */
		gint n;

		n = (gint)function->priv->n_arguments -
		    (gint)function->priv->n_optional -
		    (gint)function->priv->n_implicit;

		function->priv->arguments = g_list_insert (function->priv->arguments,
		                                           g_object_ref_sink (argument),
		                                           n);
	}

	name = cpg_function_argument_get_name (argument);

	if (g_str_has_suffix (name, "'"))
	{
		gchar *pname;
		CpgFunctionArgument *parg;

		pname = g_strndup (name, strlen (name) - 1);
		parg = cpg_function_get_argument (function, pname);

		if (parg)
		{
			cpg_property_set_derivative (_cpg_function_argument_get_property (parg),
			                             _cpg_function_argument_get_property (argument));
		}

		g_free (pname);
	}

	g_hash_table_insert (function->priv->arguments_hash,
	                     g_strdup (cpg_function_argument_get_name (argument)),
	                     argument);

	++function->priv->n_arguments;

	if (cpg_function_argument_get_optional (argument))
	{
		++function->priv->n_optional;
	}

	if (!cpg_function_argument_get_explicit (argument))
	{
		++function->priv->n_implicit;
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
	cpg_object_class->apply_template = cpg_function_apply_template_impl;
	cpg_object_class->unapply_template = cpg_function_unapply_template_impl;
	cpg_object_class->equal = cpg_function_equal_impl;

	klass->execute = cpg_function_execute_impl;
	klass->evaluate = cpg_function_evaluate_impl;
	klass->argument_added = cpg_function_argument_added_impl;

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

	self->priv->arguments_hash = g_hash_table_new_full (g_str_hash,
	                                                    g_str_equal,
	                                                    (GDestroyNotify)g_free,
	                                                    NULL);
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
cpg_function_new (gchar const   *name,
                  CpgExpression *expr)
{
	CpgFunction *ret;

	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expr == NULL || CPG_IS_EXPRESSION (expr), NULL);

	if (expr && g_object_is_floating (G_OBJECT (expr)))
	{
		g_object_ref_sink (expr);
	}
	else if (expr)
	{
		g_object_ref (expr);
	}

	ret = g_object_new (CPG_TYPE_FUNCTION,
	                    "id", name,
	                    "expression", expr,
	                    NULL);

	if (expr)
	{
		g_object_unref (expr);
	}

	return ret;
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
		property = cpg_property_new (name,
		                             cpg_expression_new0 (),
		                             CPG_PROPERTY_FLAG_NONE);

		if (!cpg_object_add_property (CPG_OBJECT (function), property, NULL))
		{
			return;
		}
	}
	else if (cpg_function_get_argument (function,
	                                    cpg_function_argument_get_name (argument)))
	{
		return;
	}

	_cpg_function_argument_set_property (argument, property);

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
	gchar const *name;

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

	name = cpg_function_argument_get_name (argument);

	if (cpg_object_remove_property (CPG_OBJECT (function), name, error))
	{
		if (cpg_function_argument_get_optional (argument))
		{
			--function->priv->n_optional;
		}

		if (!cpg_function_argument_get_explicit (argument))
		{
			--function->priv->n_implicit;
		}

		function->priv->arguments = g_list_delete_link (function->priv->arguments,
		                                                item);

		g_hash_table_remove (function->priv->arguments_hash,
		                     cpg_function_argument_get_name (argument));

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

		if (g_str_has_suffix (name, "'"))
		{
			gchar *pname;
			CpgFunctionArgument *parg;

			pname = g_strndup (name, strlen (name) - 1);
			parg = cpg_function_get_argument (function, pname);

			if (parg)
			{
				cpg_property_set_derivative (_cpg_function_argument_get_property (parg),
				                             NULL);
			}

			g_free (pname);
		}

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
	CPG_FUNCTION_GET_CLASS (function)->execute (function,
	                                            nargs,
	                                            stack);
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

/**
 * cpg_function_get_n_implicit:
 * @function: A #CpgFunction
 *
 * Get the number of implicit arguments.
 *
 * Returns: the number of implicit arguments
 *
 **/
guint
cpg_function_get_n_implicit (CpgFunction *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), 0);

	return function->priv->n_implicit;
}

CpgFunctionArgument *
cpg_function_get_argument (CpgFunction *function,
                           gchar const *name)
{
	g_return_val_if_fail (CPG_IS_FUNCTION (function), NULL);

	return g_hash_table_lookup (function->priv->arguments_hash, name);
}
