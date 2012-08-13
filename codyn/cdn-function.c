/*
 * cdn-function.c
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

#include "cdn-function.h"
#include "cdn-compile-error.h"
#include "cdn-expression-tree-iter.h"
#include "instructions/cdn-instruction-rand.h"
#include "instructions/cdn-instruction-variable.h"

#include <string.h>

/**
 * SECTION:cdn-function
 * @short_description: Custom user defined function
 *
 * It is possible to define custom user functions in the network which can
 * then be used from any expression. This class provides the basic
 * user function functionality. User defined functions can reference global
 * constants as well as use other user defined functions in their expressions.
 *
 * The #CdnFunction class can be subclassed to provide more specific types
 * of functions. One such example is the #CdnFunctionPolynomial class which
 * can be used to define and evaluate piecewise polynomials.
 *
 * <refsect2 id="CdnFunction-COPY">
 * <title>CdnFunction Copy Semantics</title>
 * When a function is copied with #cdn_object_copy, the function expression
 * and all the arguments are copied as well.
 * </refsect2>
 *
 */

#define CDN_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_FUNCTION, CdnFunctionPrivate))

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

struct _CdnFunctionPrivate
{
	CdnExpression *expression;
	GSList *dimension_cache;

	GList *arguments;
	GHashTable *arguments_hash;
	GSList *helper_vars;

	guint n_arguments;
	guint n_implicit;

	CdnStackManipulation smanip;

	guint has_dimension : 1;
};

G_DEFINE_TYPE (CdnFunction, cdn_function, CDN_TYPE_OBJECT)

static guint signals[NUM_SIGNALS] = {0,};

GQuark
cdn_function_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_function_error");
	}

	return quark;
}

static void
on_dimension_cache_removed (CdnFunction *function,
                            gpointer     where_the_object_was)
{
	g_object_unref (function);

	function->priv->dimension_cache =
		g_slist_remove (function->priv->dimension_cache,
		                where_the_object_was);
}

static void
cdn_function_finalize (GObject *object)
{
	CdnFunction *self = CDN_FUNCTION (object);

	if (self->priv->expression)
	{
		g_object_unref (self->priv->expression);
	}

	g_slist_free (self->priv->dimension_cache);

	g_list_foreach (self->priv->arguments, (GFunc)g_object_unref, NULL);
	g_list_free (self->priv->arguments);

	g_hash_table_destroy (self->priv->arguments_hash);

	g_slist_foreach (self->priv->helper_vars, (GFunc)g_object_unref, NULL);
	g_slist_free (self->priv->helper_vars);
	self->priv->helper_vars = NULL;

	cdn_stack_manipulation_destroy (&self->priv->smanip);

	G_OBJECT_CLASS (cdn_function_parent_class)->finalize (object);
}

static void
cdn_function_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CdnFunction *self = CDN_FUNCTION (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
		{
			CdnExpression *expr;

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
				cdn_expression_set_has_cache (self->priv->expression,
				                              FALSE);
			}

			cdn_object_taint (CDN_OBJECT (self));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_function_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CdnFunction *self = CDN_FUNCTION (object);

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

static void
extract_helper_vars (CdnFunction *f)
{
	GSList *vars;

	vars = cdn_object_get_variables (CDN_OBJECT (f));

	while (vars)
	{
		gchar const *name;

		name = cdn_variable_get_name (vars->data);

		if (g_hash_table_lookup (f->priv->arguments_hash, name) == NULL)
		{
			f->priv->helper_vars = g_slist_prepend (f->priv->helper_vars,
			                                        g_object_ref (vars->data));
		}

		vars = g_slist_delete_link (vars, vars);
	}

	f->priv->helper_vars = g_slist_reverse (f->priv->helper_vars);
}

static gboolean
argument_is_unused (CdnFunction         *f,
                    CdnFunctionArgument *argument)
{
	CdnVariable *v;
	GSList const *dep;
	CdnExpression *ex;

	v = _cdn_function_argument_get_variable (argument);

	if (!v)
	{
		return FALSE;
	}

	ex = cdn_variable_get_expression (v);

	for (dep = cdn_expression_get_depends_on_me (ex); dep; dep = g_slist_next (dep))
	{
		CdnExpression *d;
		GSList const *h;

		d = dep->data;

		if (d == f->priv->expression)
		{
			return FALSE;
		}

		for (h = f->priv->helper_vars; h; h = g_slist_next (h))
		{
			if (cdn_variable_get_expression (h->data) == d)
			{
				return FALSE;
			}
		}
	}

	return TRUE;
}

static void
mark_unused_arguments (CdnFunction *f)
{
	GList *arg;

	if (!f->priv->expression)
	{
		return;
	}

	for (arg = f->priv->arguments; arg; arg = g_list_next (arg))
	{
		cdn_function_argument_set_unused (arg->data,
		                                  argument_is_unused (f, arg->data));
	}
}

static void
update_stack_manipulation (CdnFunction *f)
{
	if (!f->priv->expression)
	{
		return;
	}

	cdn_expression_get_dimension (f->priv->expression,
	                              &f->priv->smanip.push.dimension);
}

static gboolean
cdn_function_compile_impl (CdnObject         *object,
                           CdnCompileContext *context,
                           CdnCompileError   *error)
{
	CdnFunction *self = CDN_FUNCTION (object);
	gboolean ret = TRUE;
	GList *item;

	if (cdn_object_is_compiled (object))
	{
		return TRUE;
	}

	g_slist_foreach (self->priv->helper_vars, (GFunc)g_object_unref, NULL);
	g_slist_free (self->priv->helper_vars);
	self->priv->helper_vars = NULL;

	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	context = cdn_object_get_compile_context (object, context);

	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->compile)
	{
		if (!CDN_OBJECT_CLASS (cdn_function_parent_class)->compile (object,
		                                                            context,
		                                                            error))
		{
			g_object_unref (context);
			return FALSE;
		}
	}

	if (!self->priv->expression || !ret)
	{
		cdn_compile_context_restore (context);
		g_object_unref (context);
		return ret;
	}

	if (!cdn_expression_compile (self->priv->expression,
	                             context,
	                             error))
	{
		if (error)
		{
			cdn_compile_error_set (error,
			                       NULL,
			                       object,
			                       NULL,
			                       NULL,
			                       NULL);
		}

		ret = FALSE;
	}

	// Compile defaults
	for (item = self->priv->arguments; item; item = g_list_next (item))
	{
		CdnExpression *def;

		if (!ret)
		{
			continue;
		}

		def = cdn_function_argument_get_default_value (item->data);

		if (def)
		{
			if (!cdn_expression_compile (def, context, error))
			{
				if (error)
				{
					cdn_compile_error_set (error,
					                       NULL,
					                       object,
					                       NULL,
					                       NULL,
					                       NULL);
				}

				ret = FALSE;
			}
		}
	}

	cdn_compile_context_restore (context);
	g_object_unref (context);

	extract_helper_vars (self);
	mark_unused_arguments (self);
	update_stack_manipulation (self);

	return ret;
}

static CdnStackManipulation const *
cdn_function_get_stack_manipulation_impl (CdnFunction *function)
{
	return &function->priv->smanip;
}

static void
cdn_function_evaluate_impl (CdnFunction *function,
                            CdnStack    *stack)
{
	if (function->priv->expression)
	{
		CdnDimension dim;
		gint num;
		gint i;
		GSList *item;

		g_slist_foreach ((GSList *)cdn_expression_get_rand_instructions (function->priv->expression),
		                 (GFunc)cdn_instruction_rand_next,
		                 NULL);

		for (item = function->priv->helper_vars; item; item = g_slist_next (item))
		{
			CdnVariable *v = item->data;
			CdnExpression *e;

			e = cdn_variable_get_expression (v);
			cdn_expression_reset_cache (e);

			g_slist_foreach ((GSList *)cdn_expression_get_rand_instructions (e),
			                 (GFunc)cdn_instruction_rand_next,
			                 NULL);
		}

		gdouble const *ret = cdn_expression_evaluate_values (function->priv->expression,
		                                                     &dim);

		num = cdn_dimension_size (&dim);

		for (i = 0; i < num; ++i)
		{
			cdn_stack_push (stack, ret[i]);
		}
	}
	else
	{
		cdn_stack_push (stack, 0);
	}
}

static void
cdn_function_execute_impl (CdnFunction *function,
                           CdnStack    *stack)
{
	GList *item = NULL;
	guint i;
	gint numpop;

	numpop = function->priv->smanip.pop.num;

	if (numpop <= function->priv->n_arguments)
	{
		item = g_list_nth (function->priv->arguments, numpop - 1);
	}

	/* Set provided arguments */
	for (i = 0; i < numpop; ++i)
	{
		CdnFunctionArgument *argument;
		CdnVariable *v;
		CdnDimension dim;
		gdouble *vals;

		argument = item->data;

		v = _cdn_function_argument_get_variable (argument);

		dim = function->priv->smanip.pop.args[i].dimension;
		vals = cdn_stack_popn (stack, cdn_dimension_size (&dim));

		cdn_variable_set_values (v,
		                         vals,
		                         &dim);

		item = g_list_previous (item);
	}

	/* Evaluate the expression */
	CDN_FUNCTION_GET_CLASS (function)->evaluate (function, stack);
}

static void
cdn_function_copy_impl (CdnObject *object,
                        CdnObject *source)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->copy != NULL)
	{
		CDN_OBJECT_CLASS (cdn_function_parent_class)->copy (object, source);
	}

	// Copy expression
	CdnFunction *source_function = CDN_FUNCTION (source);
	CdnFunction *target = CDN_FUNCTION (object);

	if (source_function->priv->expression)
	{
		g_object_set (target,
		              "expression",
		              cdn_expression_copy (source_function->priv->expression),
		              NULL);
	}

	// Copy arguments
	GList *item;

	for (item = source_function->priv->arguments; item; item = g_list_next (item))
	{
		CdnFunctionArgument *orig = (CdnFunctionArgument *)item->data;
		CdnFunctionArgument *argument = cdn_function_argument_copy (orig);

		cdn_function_add_argument (target, argument);
	}

	cdn_stack_manipulation_copy (&target->priv->smanip,
	                             &source_function->priv->smanip);
}

static void
cdn_function_reset_impl (CdnObject *object)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->reset != NULL)
	{
		CDN_OBJECT_CLASS (cdn_function_parent_class)->reset (object);
	}

	CdnFunction *function = CDN_FUNCTION (object);

	if (function->priv->expression)
	{
		cdn_expression_reset (function->priv->expression);
	}
}

static void
cdn_function_foreach_expression_impl (CdnObject                *object,
                                      CdnForeachExpressionFunc  func,
                                      gpointer                  userdata)
{
	CdnFunction *function = CDN_FUNCTION (object);

	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->foreach_expression != NULL)
	{
		CDN_OBJECT_CLASS (cdn_function_parent_class)->foreach_expression (object,
		                                                                  func,
		                                                                  userdata);
	}

	if (function->priv->expression)
	{
		func (function->priv->expression, userdata);
	}
}

static void
cdn_function_constructed (GObject *object)
{
	CdnFunction *function = CDN_FUNCTION (object);

	if (function->priv->expression != NULL)
	{
		g_object_ref_sink (function->priv->expression);
		g_object_unref (function->priv->expression);
	}
}

static void
cdn_function_template_expression_changed (CdnFunction *function,
                                          GParamSpec  *spec,
                                          CdnFunction *templ)
{
	GSList const *templates;
	GSList *last;

	templates = cdn_object_get_applied_templates (CDN_OBJECT (function));
	last = g_slist_last ((GSList *)templates);

	if (!last)
	{
		return;
	}

	if (templ == last->data)
	{
		g_object_set (function,
		              "expression",
		              cdn_expression_copy (cdn_function_get_expression (templ)),
		              NULL);
	}
}

static CdnFunction *
last_template (CdnFunction *target)
{
	CdnFunction *ret = NULL;
	GSList const *templates;

	templates = cdn_object_get_applied_templates (CDN_OBJECT (target));

	while (templates)
	{
		if (CDN_IS_FUNCTION (templates->data))
		{
			ret = templates->data;
		}

		templates = g_slist_next (templates);
	}

	return ret;
}

static gboolean
is_from_template (CdnFunction *target)
{
	CdnFunction *templ;

	templ = last_template (target);

	if (!templ)
	{
		return target->priv->expression == NULL &&
		       target->priv->n_arguments == 0;
	}

	return cdn_object_equal (CDN_OBJECT (target), CDN_OBJECT (templ));
}

static void
from_template (CdnFunction *target,
               CdnFunction *source)
{
	GList *item;

	g_list_foreach (target->priv->arguments,
	                (GFunc)g_object_unref,
	                NULL);

	g_list_free (target->priv->arguments);
	target->priv->arguments = NULL;

	target->priv->n_arguments = 0;
	target->priv->n_implicit = 0;

	cdn_function_template_expression_changed (target, NULL, source);

	for (item = source->priv->arguments; item; item = g_list_next (item))
	{
		cdn_function_add_argument (target,
		                           cdn_function_argument_copy (item->data));
	}
}

static void
cdn_function_template_argument_added (CdnFunction         *source,
                                      CdnFunctionArgument *arg,
                                      CdnFunction         *target)
{
	if (last_template (target) == source)
	{
		cdn_function_add_argument (target, cdn_function_argument_copy (arg));
	}
}

static gboolean
arguments_equal (CdnFunction         *a,
                 CdnFunction         *b,
                 CdnFunctionArgument *ignore)
{
	GList *arg1;
	GList *arg2;

	arg1 = a->priv->arguments;
	arg2 = a->priv->arguments;

	while (arg1 != NULL && arg2 != NULL)
	{
		CdnFunctionArgument *a1;
		CdnFunctionArgument *a2;

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

		if (g_strcmp0 (cdn_function_argument_get_name (a1),
		               cdn_function_argument_get_name (a2)) != 0)
		{
			return FALSE;
		}

		if (cdn_function_argument_get_explicit (a1) !=
		    cdn_function_argument_get_explicit (a2))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
cdn_function_template_argument_removed (CdnFunction         *source,
                                        CdnFunctionArgument *arg,
                                        CdnFunction         *target)
{
	CdnFunction *templ;
	CdnFunctionArgument *item;

	item = cdn_function_get_argument (target, cdn_function_argument_get_name (arg));

	templ = last_template (target);

	if (templ &&
	    item &&
	    arguments_equal (target, templ, item) &&
	    cdn_expression_equal (target->priv->expression,
	                          templ->priv->expression,
	                          TRUE))
	{
		cdn_function_remove_argument (target,
		                              item,
		                              NULL);
	}
}

static gboolean
cdn_function_apply_template_impl (CdnObject  *object,
                                  CdnObject  *templ,
                                  GError    **error)
{
	CdnFunction *target = NULL;
	CdnFunction *source = NULL;
	gboolean apply;

	target = CDN_FUNCTION (object);

	if (CDN_IS_FUNCTION (templ))
	{
		source = CDN_FUNCTION (templ);
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
		target->priv->n_implicit = 0;
	}

	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->apply_template)
	{
		if (!CDN_OBJECT_CLASS (cdn_function_parent_class)->apply_template (object, templ, error))
		{
			return FALSE;
		}
	}

	if (source)
	{
		g_signal_connect_swapped (source,
		                          "notify::expression",
		                          G_CALLBACK (cdn_function_template_expression_changed),
		                          target);

		g_signal_connect (source,
		                  "argument-added",
		                  G_CALLBACK (cdn_function_template_argument_added),
		                  target);

		g_signal_connect (source,
		                  "argument-removed",
		                  G_CALLBACK (cdn_function_template_argument_removed),
		                  target);
	}

	if (apply)
	{
		from_template (target, source);
	}

	return TRUE;
}

static gboolean
cdn_function_unapply_template_impl (CdnObject  *object,
                                    CdnObject  *templ,
                                    GError    **error)
{
	GSList *last;
	GSList const *templates;
	gboolean waslast = FALSE;
	CdnFunction *target = NULL;
	CdnFunction *source = NULL;

	templates = cdn_object_get_applied_templates (object);
	last = g_slist_last ((GSList *)templates);

	target = CDN_FUNCTION (object);

	/* Remove all function arguments */
	if (CDN_IS_FUNCTION (templ) && last && last->data == templ)
	{
		source = CDN_FUNCTION (templ);

		g_list_foreach (target->priv->arguments,
		                (GFunc)g_object_unref,
		                NULL);

		g_list_free (target->priv->arguments);
		target->priv->arguments = NULL;

		target->priv->n_arguments = 0;
		target->priv->n_implicit = 0;

		waslast = TRUE;
	}

	if (CDN_OBJECT_CLASS (cdn_function_parent_class)->unapply_template)
	{
		if (!CDN_OBJECT_CLASS (cdn_function_parent_class)->unapply_template (object, templ, error))
		{
			return FALSE;
		}
	}

	if (source)
	{
		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cdn_function_template_argument_added),
		                                      object);

		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cdn_function_template_argument_removed),
		                                      object);

		g_signal_handlers_disconnect_by_func (target,
		                                      G_CALLBACK (cdn_function_template_expression_changed),
		                                      object);
	}

	if (waslast)
	{
		templates = cdn_object_get_applied_templates (object);
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
cdn_function_equal_impl (CdnObject *a,
                         CdnObject *b)
{
	CdnFunction *fa;
	CdnFunction *fb;

	if (!CDN_OBJECT_CLASS (cdn_function_parent_class)->equal (a, b))
	{
		return FALSE;
	}

	fa = CDN_FUNCTION (a);
	fb = CDN_FUNCTION (b);

	if (!arguments_equal (fa, fb, NULL))
	{
		return FALSE;
	}

	return cdn_expression_equal (fa->priv->expression,
	                             fb->priv->expression,
	                             TRUE);
}

static gboolean
on_argument_invalidate_name (CdnFunctionArgument *argument,
                             gchar const         *name,
                             CdnFunction         *function)
{
	CdnVariable *property = cdn_object_get_variable (CDN_OBJECT (function),
	                                                 name);

	CdnVariable *current = _cdn_function_argument_get_variable (argument);

	return property && current != property;
}

static void
on_argument_name_changed (CdnFunctionArgument *argument,
                          GParamSpec          *spec,
                          CdnFunction         *function)
{
	gchar const *name = cdn_function_argument_get_name (argument);
	CdnVariable *property = cdn_object_get_variable (CDN_OBJECT (function),
	                                                 name);

	CdnVariable *current = _cdn_function_argument_get_variable (argument);

	if (property == current)
	{
		return;
	}
	else if (property || !cdn_variable_set_name (current, name))
	{
		cdn_function_argument_set_name (argument,
		                                cdn_variable_get_name (current));
	}
}

static gchar *
compose_dot (gchar const *name,
             gint         order)
{
	GString *ret;

	if (order == 0)
	{
		return g_strdup (name);
	}

	ret = g_string_new ("");

	g_string_append_unichar (ret, g_utf8_get_char (name));
	g_string_append_unichar (ret, order == 2 ? 776 : 775);
	g_string_append (ret, g_utf8_next_char (name));

	return g_string_free (ret, FALSE);
}

static CdnFunctionArgument *
find_integral_of (CdnFunction *function,
                  gchar const *name)
{
	gint order;
	gchar *nname;
	gchar *cname;
	CdnFunctionArgument *parg;

	nname = cdn_decompose_dot (name, &order);

	if (!nname || order == 0)
	{
		return NULL;
	}

	--order;
	cname = compose_dot (nname, order);

	parg = cdn_function_get_argument (function, cname);

	if (!parg && order > 0)
	{
		gchar *f;

		f = g_strnfill (order, '\'');
		g_free (cname);

		cname = g_strconcat (nname, f, NULL);
		g_free (f);

		parg = cdn_function_get_argument (function, cname);
	}

	g_free (cname);
	g_free (nname);

	return parg;
}

static void
cdn_function_argument_added_impl (CdnFunction         *function,
                                  CdnFunctionArgument *argument)
{
	CdnFunctionArgument *parg;

	if (!cdn_function_argument_get_explicit (argument))
	{
		/* Just append */
		function->priv->arguments = g_list_append (function->priv->arguments,
		                                           g_object_ref_sink (argument));
	}
	else
	{
		/* Insert before first implicit */
		gint n;

		n = (gint)function->priv->n_arguments -
		    (gint)function->priv->n_implicit;

		function->priv->arguments = g_list_insert (function->priv->arguments,
		                                           g_object_ref_sink (argument),
		                                           n);
	}

	parg = find_integral_of (function,
	                         cdn_function_argument_get_name (argument));

	if (parg)
	{
		cdn_variable_set_derivative (_cdn_function_argument_get_variable (parg),
		                             _cdn_function_argument_get_variable (argument));
	}

	g_hash_table_insert (function->priv->arguments_hash,
	                     g_strdup (cdn_function_argument_get_name (argument)),
	                     argument);

	++function->priv->n_arguments;

	if (!cdn_function_argument_get_explicit (argument))
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

	cdn_object_taint (CDN_OBJECT (function));
}

static gboolean
compare_dimensions (CdnFunction        *function,
                    CdnStackArgs const *argdim)
{
	gint i;
	GList *arg = function->priv->arguments;

	if (argdim->num != function->priv->n_arguments)
	{
		return FALSE;
	}

	for (i = function->priv->n_arguments - 1; i >= 0; --i)
	{
		CdnFunctionArgument *a = arg->data;
		CdnDimension dim;

		cdn_function_argument_get_dimension (a, &dim);

		if (dim.rows != argdim->args[i].rows ||
		    dim.columns != argdim->args[i].columns)
		{
			return FALSE;
		}

		arg = g_list_next (arg);
	}

	return TRUE;
}

static void
set_argdim (CdnFunction        *func,
            CdnStackArgs const *argdim)
{
	gint i;
	GList *arg = func->priv->arguments;

	cdn_stack_args_copy (&func->priv->smanip.pop, argdim);

	for (i = func->priv->n_arguments - 1; i >= 0; --i)
	{
		CdnFunctionArgument *a = arg->data;

		cdn_function_argument_set_dimension (a,
		                                     &argdim->args[i].dimension);

		arg = g_list_next (arg);
	}
}

static CdnFunction *
cdn_function_for_dimension_impl (CdnFunction        *function,
                                 CdnStackArgs const *argdim)
{
	if (!function->priv->has_dimension)
	{
		set_argdim (function, argdim);
		function->priv->has_dimension = TRUE;

		// Use this function
		return g_object_ref (function);
	}
	else if (compare_dimensions (function, argdim))
	{
		return g_object_ref (function);
	}
	else
	{
		GSList *c;
		CdnFunction *other;

		for (c = function->priv->dimension_cache; c; c = g_slist_next (c))
		{
			other = c->data;

			if (compare_dimensions (other, argdim))
			{
				return g_object_ref (other);
			}
		}

		// New cache!
		other = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (function)));
		other->priv->has_dimension = TRUE;

		set_argdim (other, argdim);

		function->priv->dimension_cache =
			g_slist_prepend (function->priv->dimension_cache,
			                 other);

		g_object_weak_ref (G_OBJECT (other),
		                   (GWeakNotify)on_dimension_cache_removed,
		                   function);

		cdn_object_taint (CDN_OBJECT (other));
		cdn_modifiable_set_modified (CDN_MODIFIABLE (other->priv->expression),
		                             TRUE);

		g_object_ref (function);
		return g_object_ref (other);
	}
}

static void
substitute_references (CdnVariable *v,
                       GHashTable  *mapping)
{
	CdnExpression *expr;
	GSList const *instr;

	expr = cdn_variable_get_expression (v);

	if (!expr)
	{
		return;
	}

	instr = cdn_expression_get_instructions (expr);

	while (instr)
	{
		CdnInstruction *i = instr->data;

		if (CDN_IS_INSTRUCTION_VARIABLE (i))
		{
			CdnInstructionVariable *vv;
			CdnVariable *var;
			CdnExpressionTreeIter *mapped;

			vv = CDN_INSTRUCTION_VARIABLE (i);
			var = cdn_instruction_variable_get_variable (vv);

			mapped = g_hash_table_lookup (mapping, var);

			if (mapped)
			{
				CdnInstruction *other;
				CdnVariable *otherv;

				other = cdn_expression_tree_iter_get_instruction (mapped);
				otherv = cdn_instruction_variable_get_variable (CDN_INSTRUCTION_VARIABLE (other));

				cdn_instruction_variable_set_variable (vv,
				                                       otherv);
			}
		}

		instr = g_slist_next (instr);
	}
}

static GHashTable *
variables_map (CdnFunction *function,
               CdnFunction *newfunc)
{
	GHashTable *mapping;
	GSList *vars1;
	GSList *vars2;
	GSList *va2;

	mapping = g_hash_table_new_full (g_direct_hash,
	                                 g_direct_equal,
	                                 (GDestroyNotify)g_object_unref,
	                                 (GDestroyNotify)cdn_expression_tree_iter_free);

	vars1 = cdn_object_get_variables (CDN_OBJECT (function));
	va2 = cdn_object_get_variables (CDN_OBJECT (newfunc));
	vars2 = va2;

	while (vars1 && vars2)
	{
		CdnVariable *v1;
		CdnVariable *v2;
		CdnInstruction *instr;

		v1 = vars1->data;
		v2 = vars2->data;

		instr = cdn_instruction_variable_new (v2);

		g_hash_table_insert (mapping,
		                     g_object_ref (v1),
		                     cdn_expression_tree_iter_new_from_instruction (instr));

		cdn_mini_object_unref (instr);

		vars1 = g_slist_delete_link (vars1, vars1);
		vars2 = g_slist_next (vars2);
	}

	g_slist_free (vars1);

	while (va2)
	{
		substitute_references (va2->data, mapping);
		va2 = g_slist_delete_link (va2, va2);
	}

	return mapping;
}

static GHashTable *
create_towards_map (CdnFunction                       *function,
                    CdnFunction                       *newfunc,
                    GSList                            *towards,
                    CdnExpressionTreeIterDeriveFlags   flags,
                    GError                           **error)
{
	GHashTable *ret;
	gboolean ispart;

	ispart = (flags & (CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL |
	                   CDN_EXPRESSION_TREE_ITER_DERIVE_TIME)) != 0;

	ret = g_hash_table_new_full (g_direct_hash,
	                             g_direct_equal,
	                             (GDestroyNotify)g_object_unref,
	                             (GDestroyNotify)cdn_expression_tree_iter_free);

	while (towards)
	{
		CdnFunctionArgument *arg;
		CdnVariable *v;

		arg = towards->data;
		v = _cdn_function_argument_get_variable (arg);

		if (!v)
		{
			g_warning ("Could not get variable from arg: %s",
			           cdn_function_argument_get_name (arg));
		}
		else if (cdn_variable_get_object (v) != CDN_OBJECT (function))
		{
			g_warning ("Arg %s not from func %s",
			           cdn_variable_get_full_name (v),
			           cdn_object_get_full_id_for_display (CDN_OBJECT (function)));
		}

		if (v && cdn_variable_get_object (v) == CDN_OBJECT (function))
		{
			CdnVariable *diff;
			CdnInstruction *instr;
			CdnExpressionTreeIter *iter;

			v = cdn_object_get_variable (CDN_OBJECT (newfunc),
			                             cdn_variable_get_name (v));

			if (ispart)
			{
				g_hash_table_insert (ret, g_object_ref (v), NULL);
			}

			diff = cdn_variable_get_derivative (v);

			if (!diff)
			{
				gchar *argname;
				CdnFunctionArgument *narg;
				CdnExpression *defval;

				argname = g_strconcat (cdn_function_argument_get_name (arg),
				                       "'",
				                       NULL);

				defval = cdn_expression_new ("1");
				cdn_expression_compile (defval, NULL, NULL);

				// Generate a new argument for it with a
				// default value of 1
				narg = cdn_function_argument_new (argname,
				                                  TRUE,
				                                  defval);

				cdn_function_add_argument (newfunc, narg);
				diff = _cdn_function_argument_get_variable (narg);

				g_free (argname);
			}

			instr = cdn_instruction_variable_new (diff);
			iter = cdn_expression_tree_iter_new_from_instruction (instr);
			cdn_mini_object_unref (instr);

			// Map to the derivative
			g_hash_table_insert (ret,
			                     g_object_ref (v),
			                     iter);
		}

		towards = g_slist_next (towards);
	}

	return ret;
}

static GSList *
create_symbols (CdnFunction *function)
{
	GList const *arguments;
	GSList *ret = NULL;

	arguments = function->priv->arguments;

	while (arguments)
	{
		CdnFunctionArgument *arg = arguments->data;

		if (cdn_function_argument_get_explicit (arg))
		{
			ret = g_slist_prepend (ret,
			                       g_object_ref (_cdn_function_argument_get_variable (arg)));
		}

		arguments = g_list_next (arguments);
	}

	return g_slist_reverse (ret);
}

static CdnFunction *
cdn_function_get_derivative_impl (CdnFunction                       *function,
                                  GSList                            *towards,
                                  gint                               order,
                                  CdnExpressionTreeIterDeriveFlags   flags,
                                  GError                           **error)
{
	CdnFunction *ret;
	gchar *name;
	GHashTable *towardsmap;
	CdnExpression *expr;
	GSList *symbols;
	CdnExpressionTreeIter *iter;
	CdnExpressionTreeIter *derived;
	GSList *instructions;
	GHashTable *mapping;

	name = g_strconcat ("d", cdn_object_get_id (CDN_OBJECT (function)), NULL);
	ret = CDN_FUNCTION (cdn_object_copy (CDN_OBJECT (function)));
	cdn_object_set_id (CDN_OBJECT (ret), name);
	g_free (name);

	expr = cdn_function_get_expression (ret);

	if (!expr)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (function));

		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_FUNCTION,
		             "The function `%s' does not have a valid expression",
		             id);

		g_free (id);
		g_object_unref (ret);

		return NULL;
	}

	// This creates a mapping from 'towards' variables to their derivatives.
	// Note that in the case of a partial derivative, no new arguments are
	// mapped as derivation of a partial derivative variable is simply 1
	towardsmap = create_towards_map (function,
	                                 ret,
	                                 towards,
	                                 flags,
	                                 error);

	if (!towardsmap)
	{
		g_object_unref (ret);
		return NULL;
	}

	iter = cdn_expression_tree_iter_new (expr);

	if (!iter)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (function));

		g_set_error (error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_FUNCTION,
		             "The function `%s' does not have valid instructions",
		             id);

		g_hash_table_destroy (towardsmap);

		g_free (id);
		g_object_unref (ret);

		return NULL;
	}

	// Substitute original variables with new variables
	mapping = variables_map (function, ret);
	iter = cdn_expression_tree_iter_substitute_hash (iter, mapping);
	g_hash_table_destroy (mapping);

	symbols = create_symbols (ret);

	derived = cdn_expression_tree_iter_derive (iter,
	                                           symbols,
	                                           towardsmap,
	                                           order,
	                                           flags,
	                                           error);

	cdn_expression_tree_iter_free (iter);

	g_hash_table_destroy (towardsmap);

	g_slist_foreach (symbols, (GFunc)g_object_unref, NULL);
	g_slist_free (symbols);

	if (!derived)
	{
		g_object_unref (ret);
		return NULL;
	}

	instructions = cdn_expression_tree_iter_to_instructions (derived);
	cdn_expression_set_instructions_take (expr, instructions);

	g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instructions);

	cdn_expression_tree_iter_free (derived);

	return ret;
}

static void
cdn_function_class_init (CdnFunctionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_object_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_function_finalize;
	object_class->constructed = cdn_function_constructed;

	object_class->set_property = cdn_function_set_property;
	object_class->get_property = cdn_function_get_property;

	cdn_object_class->compile = cdn_function_compile_impl;
	cdn_object_class->copy = cdn_function_copy_impl;
	cdn_object_class->reset = cdn_function_reset_impl;
	cdn_object_class->foreach_expression = cdn_function_foreach_expression_impl;
	cdn_object_class->apply_template = cdn_function_apply_template_impl;
	cdn_object_class->unapply_template = cdn_function_unapply_template_impl;
	cdn_object_class->equal = cdn_function_equal_impl;

	klass->execute = cdn_function_execute_impl;
	klass->evaluate = cdn_function_evaluate_impl;
	klass->get_stack_manipulation = cdn_function_get_stack_manipulation_impl;
	klass->argument_added = cdn_function_argument_added_impl;
	klass->for_dimension = cdn_function_for_dimension_impl;
	klass->get_derivative = cdn_function_get_derivative_impl;

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CdnFunction::argument-added:
	 * @function: a #CdnFunction
	 * @argument: a #CdnFunctionArgument
	 *
	 * Emitted when an argument has been added to the function
	 *
	 **/
	signals[ARGUMENT_ADDED] =
		g_signal_new ("argument-added",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionClass,
		                               argument_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_FUNCTION_ARGUMENT);

	/**
	 * CdnFunction::argument-removed:
	 * @function: a #CdnFunction
	 * @argument: a #CdnFunctionArgument
	 *
	 * Emitted when an argument has been removed from the function
	 *
	 **/
	signals[ARGUMENT_REMOVED] =
		g_signal_new ("argument-removed",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionClass,
		                               argument_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_FUNCTION_ARGUMENT);

	/**
	 * Cdnfunction::arguments-reordered:
	 * @function: a #CdnFunction
	 *
	 * Emitted when the order of the function arguments has changed.
	 *
	 **/
	signals[ARGUMENTS_REORDERED] =
		g_signal_new ("arguments-reordered",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionClass, arguments_reordered),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	g_type_class_add_private (object_class, sizeof(CdnFunctionPrivate));
}

static void
cdn_function_init (CdnFunction *self)
{
	self->priv = CDN_FUNCTION_GET_PRIVATE (self);

	self->priv->arguments_hash = g_hash_table_new_full (g_str_hash,
	                                                    g_str_equal,
	                                                    (GDestroyNotify)g_free,
	                                                    NULL);

	self->priv->smanip.push.dimension = cdn_dimension_one;
}

/**
 * cdn_function_new:
 * @name: The function name
 * @expression: The function expression
 *
 * Create a new custom user function. After creation, function arguments
 * can be added to the function using #cdn_function_add_argument.
 *
 * Returns: A #CdnFunction
 *
 **/
CdnFunction *
cdn_function_new (gchar const   *name,
                  CdnExpression *expr)
{
	CdnFunction *ret;

	g_return_val_if_fail (name != NULL, NULL);
	g_return_val_if_fail (expr == NULL || CDN_IS_EXPRESSION (expr), NULL);

	if (expr && g_object_is_floating (G_OBJECT (expr)))
	{
		g_object_ref_sink (expr);
	}
	else if (expr)
	{
		g_object_ref (expr);
	}

	ret = g_object_new (CDN_TYPE_FUNCTION,
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
 * cdn_function_add_argument:
 * @function: A #CdnFunction
 * @argument: A #CdnFunctionArgument
 *
 * Add a function argument. A proxy property for the argument will be
 * automatically created if it does not exist yet. If the argument already
 * exists it will not be added.
 *
 **/
void
cdn_function_add_argument (CdnFunction         *function,
                           CdnFunctionArgument *argument)
{
	g_return_if_fail (CDN_IS_FUNCTION (function));
	g_return_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument));
	g_return_if_fail (_cdn_function_argument_get_variable (argument) == NULL);

	gchar const *name = cdn_function_argument_get_name (argument);

	CdnVariable *property = cdn_object_get_variable (CDN_OBJECT (function),
	                                                 name);

	if (property == NULL)
	{
		/* Add the proxy property */
		property = cdn_variable_new (name,
		                             cdn_expression_new0 (),
		                             CDN_VARIABLE_FLAG_IN);

		if (!cdn_object_add_variable (CDN_OBJECT (function), property, NULL))
		{
			return;
		}
	}
	else if (cdn_function_get_argument (function,
	                                    cdn_function_argument_get_name (argument)))
	{
		return;
	}

	_cdn_function_argument_set_variable (argument, property);
	g_signal_emit (function, signals[ARGUMENT_ADDED], 0, argument);
}

/**
 * cdn_function_remove_argument:
 * @function: A #CdnFunction
 * @argument: A #CdnFunctionArgument
 * @error: A #GError
 *
 * Remove a function argument.
 *
 * Returns: %TRUE if the argument could be removed, %FALSE otherwise
 *
 **/
gboolean
cdn_function_remove_argument (CdnFunction          *function,
                              CdnFunctionArgument  *argument,
                              GError              **error)
{
	gchar const *name;

	g_return_val_if_fail (CDN_IS_FUNCTION (function), FALSE);
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), FALSE);

	GList *item = g_list_find (function->priv->arguments, argument);

	if (item == NULL)
	{
		if (error)
		{
			g_set_error (error,
			             cdn_function_error_quark (),
			             CDN_FUNCTION_ERROR_ARGUMENT_NOT_FOUND,
			             "Function argument %s not found",
			             cdn_function_argument_get_name (argument));
		}

		return FALSE;
	}

	name = cdn_function_argument_get_name (argument);

	if (cdn_object_remove_variable (CDN_OBJECT (function), name, error))
	{
		CdnFunctionArgument *parg;

		if (!cdn_function_argument_get_explicit (argument))
		{
			--function->priv->n_implicit;
		}

		function->priv->arguments = g_list_delete_link (function->priv->arguments,
		                                                item);

		g_hash_table_remove (function->priv->arguments_hash,
		                     cdn_function_argument_get_name (argument));

		_cdn_function_argument_set_variable (argument, NULL);

		--function->priv->n_arguments;

		g_signal_handlers_disconnect_by_func (argument,
		                                      on_argument_invalidate_name,
		                                      function);

		g_signal_handlers_disconnect_by_func (argument,
		                                      on_argument_name_changed,
		                                      function);

		parg = find_integral_of (function, name);

		if (parg)
		{
			cdn_variable_set_derivative (_cdn_function_argument_get_variable (parg),
			                             NULL);
		}

		cdn_object_taint (CDN_OBJECT (function));

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
 * cdn_function_get_arguments:
 * @function: A #CdnFunction
 *
 * Get the list of function arguments. The returned list is used internally
 * and should not be modified or freed.
 *
 * Returns: (element-type CdnFunctionArgument) (transfer none): A #GList
 *
 **/
const GList *
cdn_function_get_arguments (CdnFunction *function)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), NULL);

	return function->priv->arguments;
}

/**
 * cdn_function_execute:
 * @function: A #CdnFunction
 * @stack: A #CdnStack
 *
 * Execute the function. This is used internally when the function needs to be
 * evaluated.
 *
 **/
void
cdn_function_execute (CdnFunction *function,
                      CdnStack    *stack)
{
	CDN_FUNCTION_GET_CLASS (function)->execute (function,
	                                            stack);
}

/**
 * cdn_function_get_expression:
 * @function: A #CdnFunction
 *
 * Get the function expression.
 *
 * Returns: (type CdnExpression) (transfer none): A #CdnExpression
 *
 **/
CdnExpression *
cdn_function_get_expression (CdnFunction *function)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), NULL);

	return function->priv->expression;
}

/**
 * cdn_function_set_expression:
 * @function: A #CdnFunction
 * @expression: A #CdnExpression
 *
 * Set the function expression.
 *
 **/
void
cdn_function_set_expression (CdnFunction   *function,
                             CdnExpression *expression)
{
	g_return_if_fail (CDN_IS_FUNCTION (function));

	g_object_set (G_OBJECT (function), "expression", expression, NULL);
}

/**
 * cdn_function_clear_arguments:
 * @function: A #CdnFunction
 * @error: A #GError
 *
 * Remove all the function arguments.
 *
 * Returns: %TRUE if all arguments could be successfully removed, %FALSE otherwise
 *
 **/
gboolean
cdn_function_clear_arguments (CdnFunction  *function,
                              GError      **error)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), FALSE);

	GList *copy = g_list_copy (function->priv->arguments);
	GList *item;

	for (item = copy; item; item = g_list_next (item))
	{
		if (!cdn_function_remove_argument (function, item->data, error))
		{
			g_list_free (copy);
			return FALSE;
		}
	}

	g_list_free (copy);
	return TRUE;
}

/**
 * cdn_function_get_n_arguments:
 * @function: A #CdnFunction
 *
 * Get the number of arguments. This value is cached and is thus faster than
 * using #cdn_function_get_arguments and #g_list_length.
 *
 * Returns: the number of arguments
 *
 **/
guint
cdn_function_get_n_arguments (CdnFunction *function)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), 0);

	return function->priv->n_arguments;
}

/**
 * cdn_function_get_n_implicit:
 * @function: A #CdnFunction
 *
 * Get the number of implicit arguments.
 *
 * Returns: the number of implicit arguments
 *
 **/
guint
cdn_function_get_n_implicit (CdnFunction *function)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), 0);

	return function->priv->n_implicit;
}

/**
 * cdn_function_get_n_optional:
 * @function: a #CdnFunction.
 *
 * Get the number of optional arguments.
 *
 * Returns: the number of optional arguments.
 *
 **/
guint
cdn_function_get_n_optional (CdnFunction *function)
{
	GList *item;
	guint ret = 0;

	g_return_val_if_fail (CDN_IS_FUNCTION (function), 0);

	for (item = function->priv->arguments; item; item = g_list_next (item))
	{
		CdnFunctionArgument *arg;

		arg = item->data;

		if (cdn_function_argument_get_default_value (arg))
		{
			++ret;
		}
		else
		{
			ret = 0;
		}
	}

	return ret;
}

/**
 * cdn_function_get_argument:
 * @function: A #CdnFunction
 * @name: The argument name
 *
 * Get an argument by name.
 *
 * Returns: (transfer none): A #CdnFunctionArgument
 *
 **/
CdnFunctionArgument *
cdn_function_get_argument (CdnFunction *function,
                           gchar const *name)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), NULL);

	return g_hash_table_lookup (function->priv->arguments_hash, name);
}

CdnStackManipulation const *
cdn_function_get_stack_manipulation (CdnFunction *function)
{
	return CDN_FUNCTION_GET_CLASS (function)->get_stack_manipulation (function);
}

/**
 * cdn_function_for_dimension:
 * @function: A #CdnFunction
 * @argdim: The argument dimensions
 *
 * Get the function prototype representing this function for the given
 * arguments.
 *
 * Returns: (transfer full): A #CdnFunction
 *
 **/
CdnFunction *
cdn_function_for_dimension (CdnFunction        *function,
                            CdnStackArgs const *argdim)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), NULL);

	return CDN_FUNCTION_GET_CLASS (function)->for_dimension (function, argdim);
}

/**
 * cdn_function_get_derivative:
 * @function: a #CdnFunction.
 * @towards: (element-type CdnFunctionArgument): the variables towards which to derive
 * @order: the order of the derivative
 * @flags: derivation flags
 *
 * Get the derivative of a function. If successfull, a new function will be
 * returned. In the case of a full derivative, new function arguments for
 * each of the variables being derived towards will have been added.
 *
 * Returns: (transfer full): a #CdnFunction.
 *
 **/
CdnFunction *
cdn_function_get_derivative (CdnFunction                       *function,
                             GSList                            *towards,
                             gint                               order,
                             CdnExpressionTreeIterDeriveFlags   flags,
                             GError                           **error)
{
	g_return_val_if_fail (CDN_IS_FUNCTION (function), NULL);

	return CDN_FUNCTION_GET_CLASS (function)->get_derivative (function,
	                                                          towards,
	                                                          order,
	                                                          flags,
	                                                          error);
}
