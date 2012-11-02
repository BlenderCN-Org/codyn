/*
 * cdn-expression.c
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

#include "cdn-expression.h"
#include "cdn-edge.h"
#include "cdn-utils.h"
#include "cdn-tokenizer.h"
#include "cdn-math.h"
#include "cdn-compile-error.h"
#include "cdn-function.h"
#include "cdn-stack-private.h"
#include "cdn-operators.h"
#include "cdn-variable.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-node.h"
#include "cdn-io.h"
#include "cdn-debug.h"

#include <codyn/instructions/cdn-instructions.h>

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>
#include <unistd.h>
#include <glib/gprintf.h>

/**
 * SECTION:cdn-expression
 * @short_description: Mathematical expression evaluation
 *
 * A #CdnExpression contains a mathematical expression. The expression in
 * string format can be compiled and evaluated. At the compilation phase,
 * a list of #CdnObject is provided as a context in which variables are mapped
 * to #CdnVariable in this context.
 *
 */

#define CDN_EXPRESSION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_EXPRESSION, CdnExpressionPrivate))

struct _CdnExpressionPrivate
{
	// Expression to evaluate
	gchar *expression;

	GSList *instructions;
	GSList *rand_instructions;

	CdnStack output;
	CdnStackArg retdim;

	GSList *depends_on;
	GSList *depends_on_me;

	CdnMatrix cached_output;

	gint error_at;
	GSList *error_start;

	gpointer cache_userdata;
	CdnExpressionCacheNotify cache_notify;
	GDestroyNotify cache_destroy_notify;

	gpointer evaluate_userdata;
	CdnExpressionCacheNotify evaluate_notify;
	GDestroyNotify evaluate_destroy_notify;

	guint cached : 1;
	guint prevent_cache_reset : 1;
	guint modified : 1;
	guint once : 1;
	guint has_cache : 1;
	guint pinned_sparsity : 1;
	guint compiling : 1;
};

typedef struct
{
	gchar const **buffer;
	CdnCompileContext *context;

	CdnCompileError *error;
	GSList *stack;
} ParserContext;

static gboolean validate_stack (CdnExpression *expression,
                                ParserContext *context,
                                gboolean onlydim);

static gboolean parse_variable (CdnExpression *expression,
                                gchar const   *propname,
                                ParserContext *context);

static gboolean reset_cache (CdnExpression *expression,
                             gboolean       dimschanged);

static void cdn_modifiable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnExpression,
                         cdn_expression,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_MODIFIABLE,
                                                cdn_modifiable_iface_init))

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_VALUE,
	PROP_HAS_CACHE,
	PROP_MODIFIED
};

static int parse_expression (CdnExpression *expression,
                             ParserContext *context,
                             gint           priority,
                             gint           left_assoc);

static void
cdn_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static void
push_error_start (CdnExpression *expression,
                  ParserContext *ctx)
{
	expression->priv->error_start = g_slist_prepend (expression->priv->error_start,
	                                                 GINT_TO_POINTER (*(ctx->buffer) - expression->priv->expression + 1));
}

static void
pop_error_start (CdnExpression *expression,
                  ParserContext *ctx)
{
	expression->priv->error_start = g_slist_delete_link (expression->priv->error_start,
	                                                     expression->priv->error_start);
}

static void
instructions_free (CdnExpression *expression)
{
	while (expression->priv->instructions)
	{
		GSList *deps;
		GSList *dep;

		deps = cdn_instruction_get_dependencies (expression->priv->instructions->data);

		for (dep = deps; dep; dep = g_slist_next (dep))
		{
			CdnExpression *other = dep->data;

			other->priv->depends_on_me =
				g_slist_remove (other->priv->depends_on_me,
				                expression);
		}

		g_slist_free (deps);

		cdn_mini_object_unref (expression->priv->instructions->data);

		expression->priv->instructions =
			g_slist_delete_link (expression->priv->instructions,
			                     expression->priv->instructions);
	}

	g_slist_free (expression->priv->depends_on);
	expression->priv->depends_on = NULL;

	g_slist_free (expression->priv->rand_instructions);
	expression->priv->rand_instructions = NULL;

	expression->priv->cached = FALSE;

	if (!expression->priv->modified)
	{
		expression->priv->modified = TRUE;
		g_object_notify (G_OBJECT (expression), "modified");
	}
}

static gchar *
normalize_expression (gchar const *expression)
{
	if (!expression)
	{
		return NULL;
	}

	GString *ret = g_string_new ("");
	gboolean prev_space = FALSE;
	gboolean isstart = TRUE;

	while (*expression)
	{
		gunichar c = g_utf8_get_char (expression);

		if (g_unichar_type (c) == G_UNICODE_LINE_SEPARATOR)
		{
			c = ' ';
		}

		gboolean isspace = g_unichar_isspace (c);

		if (prev_space && !isspace && !isstart)
		{
			g_string_append_c (ret, ' ');
		}

		prev_space = isspace;

		if (!isspace)
		{
			g_string_append_unichar (ret, c);
			isstart = FALSE;
		}

		expression = g_utf8_next_char (expression);
	}

	return g_string_free (ret, FALSE);
}

static void
set_expression (CdnExpression *expression,
                gchar const   *value,
                gboolean       notify)
{
	g_free (expression->priv->expression);

	expression->priv->expression = normalize_expression (value);
	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));

	if (notify)
	{
		g_object_notify (G_OBJECT (expression), "expression");
	}
}

static void
reset_depending_cache (CdnExpression *expression,
                       gboolean       dimschanged)
{
	GSList *item;

	for (item = expression->priv->depends_on_me; item; item = g_slist_next (item))
	{
		CdnExpression *dep = item->data;
		gboolean realchanged = dimschanged;

		// Reset the cache of any expressions that depend on this
		// expression
		if (dimschanged && !dep->priv->prevent_cache_reset)
		{
			CdnDimension olddim;

			olddim = dep->priv->retdim.dimension;

			// We need to revalidate our stack here
			validate_stack (dep, NULL, TRUE);

			realchanged = !cdn_dimension_equal (&olddim,
			                                    &dep->priv->retdim.dimension);
		}

		reset_cache (dep, realchanged);
	}

	if (expression->priv->cache_notify)
	{
		expression->priv->cache_notify (expression,
		                                expression->priv->cache_userdata);
	}
}

static void
set_values (CdnExpression       *expression,
            gdouble       const *values,
            CdnDimension  const *dimension)
{
	gboolean dimschanged;

	dimschanged = !cdn_dimension_equal (&expression->priv->cached_output.dimension,
	                                    dimension);

	cdn_matrix_set (&expression->priv->cached_output,
	                values,
	                dimension);

	if (expression->priv->has_cache || dimschanged)
	{
		// Reset the cache of any expression that depends on this expression
		reset_depending_cache (expression, dimschanged);
	}
}

static void
cdn_expression_finalize (GObject *object)
{
	CdnExpression *expression;

	expression = CDN_EXPRESSION (object);

	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));
	g_free (expression->priv->expression);

	cdn_expression_set_cache_notify (expression,
	                                 NULL,
	                                 NULL,
	                                 NULL);

	cdn_expression_set_evaluate_notify (expression,
	                                    NULL,
	                                    NULL,
	                                    NULL);

	cdn_matrix_destroy (&expression->priv->cached_output);

	G_OBJECT_CLASS (cdn_expression_parent_class)->finalize (object);
}

static void
set_has_cache (CdnExpression *expression,
               gboolean       cache)
{
	if (cache == expression->priv->has_cache)
	{
		return;
	}

	expression->priv->has_cache = cache;

	if (!cache)
	{
		gboolean dimschanged;

		dimschanged = expression->priv->cached &&
		              !cdn_dimension_equal (&expression->priv->cached_output.dimension,
		                                    &expression->priv->retdim.dimension);

		expression->priv->cached = FALSE;

		reset_depending_cache (expression,
		                       dimschanged);
	}

	g_object_notify (G_OBJECT (expression), "has-cache");
}

static void
cdn_expression_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CdnExpression *self = CDN_EXPRESSION (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			set_expression (self, g_value_get_string (value), TRUE);
		break;
		case PROP_VALUE:
		{
			gdouble v = g_value_get_double (value);

			set_values (self, &v, cdn_dimension_onep);
			self->priv->prevent_cache_reset = TRUE;
		}
		break;
		case PROP_HAS_CACHE:
			set_has_cache (self, g_value_get_boolean (value));
		break;
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_expression_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CdnExpression *self = CDN_EXPRESSION (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_string (value, self->priv->expression);
		break;
		case PROP_VALUE:
			g_value_set_double (value, cdn_expression_evaluate (self));
		break;
		case PROP_HAS_CACHE:
			g_value_set_boolean (value, cdn_expression_get_has_cache (self));
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_expression_class_init (CdnExpressionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_expression_finalize;

	object_class->get_property = cdn_expression_get_property;
	object_class->set_property = cdn_expression_set_property;

	g_type_class_add_private (object_class, sizeof(CdnExpressionPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_string ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      NULL,
	                                                      G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
	                                 PROP_VALUE,
	                                 g_param_spec_double ("value",
	                                                      "Value",
	                                                      "Value",
	                                                      -G_MAXDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
	                                 PROP_HAS_CACHE,
	                                 g_param_spec_boolean ("has-cache",
	                                                       "Has Cache",
	                                                       "Has cache",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE));

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");
}

static void
cdn_expression_init (CdnExpression *self)
{
	self->priv = CDN_EXPRESSION_GET_PRIVATE (self);

	self->priv->has_cache = TRUE;
	self->priv->cached_output.dimension.rows = 1;
	self->priv->cached_output.dimension.columns = 1;

	cdn_stack_init (&(self->priv->output), 0);
}

/**
 * cdn_expression_get_as_string:
 * @expression: a #CdnExpression
 *
 * Get the string representation of the expression
 *
 * Returns: the string representation of the expression
 *
 **/
const gchar *
cdn_expression_get_as_string (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	return expression->priv->expression;
}

/**
 * cdn_expression_set_from_string:
 * @expression: a #CdnExpression
 * @value: the value
 *
 * Set a new expression for @expression
 *
 **/
void
cdn_expression_set_from_string (CdnExpression *expression,
                                const gchar   *value)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	set_expression (expression, value, TRUE);
}

/**
 * cdn_expression_new:
 * @expression: an expression
 *
 * Create a new #CdnExpression containing the expression @expression
 *
 * Returns: a new #CdnExpression
 *
 **/
CdnExpression *
cdn_expression_new (const gchar *expression)
{
	CdnExpression *ret;

	ret = g_object_new (CDN_TYPE_EXPRESSION, NULL);
	set_expression (ret, expression, FALSE);

	return ret;
}

/**
 * cdn_expression_new0:
 *
 * Create a new expression representing the value 0.
 *
 * Returns: (transfer full): A #CdnExpression
 *
 **/
CdnExpression *
cdn_expression_new0 ()
{
	CdnExpression *ret;

	ret = cdn_expression_new ("0");

	ret->priv->instructions = g_slist_prepend (NULL,
	                                           cdn_instruction_number_new_from_string ("0"));

	validate_stack (ret, NULL, FALSE);
	ret->priv->modified = FALSE;

	return ret;
}

/**
 * cdn_expression_new_number:
 * @number: a number
 *
 * Create a new expression representing the number.
 *
 * Returns: (transfer full): A #CdnExpression
 *
 **/
CdnExpression *
cdn_expression_new_number (gdouble number)
{
	CdnExpression *ret;
	gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

	g_ascii_dtostr (buf, G_ASCII_DTOSTR_BUF_SIZE, number);
	ret = cdn_expression_new (buf);

	ret->priv->instructions = g_slist_prepend (NULL,
	                                           cdn_instruction_number_new_from_string (buf));

	validate_stack (ret, NULL, FALSE);
	ret->priv->modified = FALSE;

	return ret;
}

static gboolean
parser_failed_error (CdnExpression *expression,
                     ParserContext *context,
                     GError        *error)
{
	if (context->error != NULL && cdn_compile_error_get_error (context->error) == NULL)
	{
		cdn_compile_error_set (context->error,
		                       error,
		                       NULL,
		                       NULL,
		                       NULL,
		                       expression);
	}

	return FALSE;
}

static gboolean
parser_failed (CdnExpression *expression,
               ParserContext *context,
               gint           code,
               gchar const   *format,
               ...)
{
	if (context->error != NULL && cdn_compile_error_get_error (context->error) == NULL)
	{
		GError *err = NULL;
		va_list ap;
		va_start (ap, format);

		gchar *message = g_strdup_vprintf (format, ap);
		va_end (ap);

		g_set_error (&err,
		             CDN_COMPILE_ERROR_TYPE,
		             code,
		             "%s",
		             message);

		g_free (message);

		parser_failed_error (expression, context, err);
		g_error_free (err);
	}

	return FALSE;
}

static gboolean
instructions_push (CdnExpression  *expression,
                   CdnInstruction *next,
                   ParserContext  *context)
{
	if (!next)
	{
		return FALSE;
	}

	expression->priv->instructions = g_slist_prepend (expression->priv->instructions,
	                                                  next);

	if (context)
	{
		CdnStackManipulation const *smanip;
		GSList *ret = NULL;
		gint consume;
		GError *error = NULL;

		cdn_instruction_set_location (next,
		                              cdn_expression_get_error_start (expression),
		                              *(context->buffer) - expression->priv->expression + 1);

		if (CDN_IS_INSTRUCTION_VARIABLE (next))
		{
			CdnVariable *variable;
			CdnExpression *expr;

			// Make sure the variable is current compiled so that the
			// stack manipulation is correctly estimated
			variable = cdn_instruction_variable_get_variable (CDN_INSTRUCTION_VARIABLE (next));
			expr = cdn_variable_get_expression (variable);

			if (expr->priv->modified)
			{
				if (!cdn_variable_compile (variable,
				                           context ? context->error : NULL))
				{
					return FALSE;
				}
			}
		}

		smanip = cdn_instruction_get_stack_manipulation (next, &error);

		if (!smanip && error)
		{
			parser_failed_error (expression, context, error);
			g_error_free (error);

			return FALSE;
		}

		consume = smanip->pop.num;

		ret = g_slist_prepend (NULL, next);

		while (consume)
		{
			ret = g_slist_concat (context->stack->data, ret);

			context->stack = g_slist_delete_link (context->stack,
			                                      context->stack);

			--consume;
		}

		context->stack = g_slist_prepend (context->stack,
		                                  ret);
	}

	return TRUE;
}

static CdnInstruction *
instructions_pop (CdnExpression *expression)
{
	CdnInstruction *inst = NULL;

	if (expression->priv->instructions)
	{
		inst = expression->priv->instructions->data;
	}

	expression->priv->instructions =
		g_slist_delete_link (expression->priv->instructions,
		                     expression->priv->instructions);

	return inst;
}

static CdnEdge *
find_link (CdnCompileContext *context)
{
	GSList const *objects = cdn_compile_context_get_objects (context);

	while (objects)
	{
		if (objects->data && CDN_IS_EDGE (objects->data))
		{
			return CDN_EDGE (objects->data);
		}

		objects = g_slist_next (objects);
	}

	return NULL;
}

static gboolean
parse_link_property (CdnExpression  *expression,
                     gchar const    *id,
                     gchar const    *propname,
                     CdnEdge        *link,
                     ParserContext  *context)
{
	CdnVariable *property = NULL;
	CdnInstructionVariableBinding binding = 0;

	if (strcmp (id, "input") == 0)
	{
		property = cdn_object_get_variable (CDN_OBJECT (cdn_edge_get_input (link)),
		                                    propname);

		binding = CDN_INSTRUCTION_VARIABLE_BINDING_INPUT;
	}
	else if (strcmp (id, "output") == 0)
	{
		property = cdn_object_get_variable (CDN_OBJECT (cdn_edge_get_output (link)),
		                                    propname);

		binding = CDN_INSTRUCTION_VARIABLE_BINDING_OUTPUT;
	}

	if (!property)
	{
		return FALSE;
	}

	return instructions_push (expression,
	                          cdn_instruction_variable_new_with_binding (property,
	                                                                     binding),
	                          context);
}

static gboolean
parse_context_combined_property (CdnExpression *expression,
                                 gchar const   *id,
                                 gchar const   *propid,
                                 ParserContext *context)
{
	gchar *combined;
	CdnVariable *prop;

	combined = g_strconcat (id, ".", propid, NULL);
	prop = cdn_compile_context_lookup_variable (context->context, combined);
	g_free (combined);

	if (prop)
	{
		return instructions_push (expression,
		                          cdn_instruction_variable_new (prop),
		                          context);
	}

	return FALSE;
}

static gboolean
parse_context_property (CdnExpression *expression,
                        gchar const   *id,
                        gchar const   *propid,
                        ParserContext *context)
{
	GSList const *objs;

	objs = cdn_compile_context_get_objects (context->context);

	while (objs)
	{
		CdnObject *o = objs->data;

		if (g_strcmp0 (id, cdn_object_get_id (o)) == 0)
		{
			CdnVariable *prop;

			prop = cdn_object_get_variable (o, propid);

			if (prop)
			{
				return instructions_push (expression,
				                          cdn_instruction_variable_new (prop),
				                          context);
			}
		}

		if (CDN_IS_NODE (o))
		{
			CdnObject *c;

			c = cdn_node_get_child (CDN_NODE (o), id);

			if (c)
			{
				CdnVariable *prop;

				if (CDN_IS_IO (c) && !cdn_object_is_compiled (c))
				{
					if (!cdn_object_compile (c, NULL, context->error))
					{
						return FALSE;
					}
				}

				prop = cdn_object_get_variable (c, propid);

				if (prop)
				{
					return instructions_push (expression,
					                          cdn_instruction_variable_new (prop),
					                          context);
				}
			}
		}

		objs = g_slist_next (objs);
	}

	return FALSE;
}

static CdnFunction *
parse_context_function (CdnExpression *expression,
                        gchar const   *id,
                        gchar const   *propid,
                        ParserContext *context)
{
	GSList const *objs;

	objs = cdn_compile_context_get_objects (context->context);

	while (objs)
	{
		CdnObject *o = objs->data;
		CdnObject *c;
		CdnObject *f;

		objs = g_slist_next (objs);

		if (!CDN_IS_NODE (o))
		{
			continue;
		}

		c = cdn_node_get_child (CDN_NODE (o), id);

		if (!c || !CDN_IS_NODE (c))
		{
			continue;
		}

		f = cdn_node_get_child (CDN_NODE (c), propid);

		if (f && CDN_IS_FUNCTION (f))
		{
			return CDN_FUNCTION (f);
		}
	}

	return NULL;
}

static gboolean
parse_dot_variable (CdnExpression *expression,
                    gchar const   *id,
                    gchar const   *propid,
                    ParserContext *context)
{
	CdnEdge *link;
	gboolean ret = FALSE;

	link = find_link (context->context);

	if (link)
	{
		ret = parse_link_property (expression,
		                           id,
		                           propid,
		                           link,
		                           context);
	}

	if (!ret)
	{
		ret = parse_context_combined_property (expression,
		                                       id,
		                                       propid,
		                                       context);
	}

	if (!ret)
	{
		ret = parse_context_property (expression,
		                              id,
		                              propid,
		                              context);
	}

	if (!ret)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
		               "Link property `%s.%s' could not be found",
		               id, propid);
	}

	return ret;
}

static CdnVariable *
lookup_variable (CdnExpression *expression,
                 ParserContext *context,
                 gchar const   *propname)
{
	CdnVariable *ret;

	ret = cdn_compile_context_lookup_variable (context->context,
	                                           propname);

	return ret;
}

static gboolean
parse_function_arguments (CdnExpression *expression,
                          ParserContext *context,
                          gint          *numargs)
{
	// parse arguments
	CdnToken *next = cdn_tokenizer_peek (*(context->buffer));
	gboolean loopit = TRUE;

	if (numargs)
	{
		*numargs = 0;
	}

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_NODE_END)
	{
		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		loopit = FALSE;
	}
	else
	{
		cdn_token_free (next);
	}

	while (loopit)
	{
		if (!parse_expression (expression, context, -1, 0))
		{
			return FALSE;
		}

		if (numargs)
		{
			++*numargs;
		}

		// see what's next
		next = cdn_tokenizer_peek (*(context->buffer));

		if (!next || !CDN_TOKEN_IS_OPERATOR (next))
		{
			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_INVALID_TOKEN,
			                      "Expected `operator', but got %s",
			                      next ? next->text : "(nothing)");
		}

		CdnTokenOperatorType type = CDN_TOKEN_OPERATOR (next)->type;

		if (type == CDN_TOKEN_OPERATOR_TYPE_NODE_END)
		{
			cdn_token_free (next);
			cdn_token_free (cdn_tokenizer_next (context->buffer));
			break;
		}
		else if (type != CDN_TOKEN_OPERATOR_TYPE_COMMA)
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected `,' but got %s",
			               next->text);
			cdn_token_free (next);
			return FALSE;
		}

		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));
	}

	return TRUE;
}

static gboolean
recurse_compile (CdnExpression *expression,
                 ParserContext *context,
                 CdnFunction   *f)
{
	// Recursively compile it here
	if (!cdn_object_is_compiled (CDN_OBJECT (f)))
	{
		if (!cdn_object_compile (CDN_OBJECT (f), NULL, context->error))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
get_argdim (CdnExpression *expression,
            ParserContext *context,
            gint           numargs,
            CdnStackArgs  *args)
{
	GSList *stack;
	gint i;

	stack = context->stack;

	cdn_stack_args_init (args, numargs);

	for (i = 0; i < numargs; ++i)
	{
		CdnInstruction *instr = g_slist_last (stack->data)->data;
		CdnStackManipulation const *manip;

		manip = cdn_instruction_get_stack_manipulation (instr, NULL);

		cdn_stack_arg_copy (&args->args[i], &manip->push);

		stack = g_slist_next (stack);
	}
}

static void
swap_arguments (CdnExpression *expression,
                ParserContext *context)
{
	// Swap the last two arguments on the stack
	GSList *first = context->stack->data;
	GSList *second = context->stack->next->data;
	GSList *fi;
	GSList *si;
	GSList *tmp;

	context->stack->data = second;
	context->stack->next->data = first;

	// Then also on the instruction set
	fi = g_slist_nth (expression->priv->instructions, g_slist_length (first) - 1);
	si = g_slist_nth (fi, g_slist_length (second));

	tmp = fi->next;
	fi->next = si->next;
	si->next = expression->priv->instructions;

	expression->priv->instructions = tmp;
}

static GSList *
make_zeros (CdnDimension const *dimension)
{
	gint n;
	GSList *ret = NULL;

	n = cdn_dimension_size (dimension);

	if (n != 1)
	{
		CdnStackArgs args;
		gint i;

		cdn_stack_args_init (&args, n);

		for (i = 0; i < n; ++i)
		{
			args.args[i].rows = 1;
			args.args[i].columns = 1;

			cdn_stack_arg_set_sparsity_one (&args.args[i], 0);

			ret = g_slist_prepend (ret,
			                       cdn_instruction_number_new_from_string ("0"));
		}

		ret = g_slist_prepend (ret,
		                       cdn_instruction_matrix_new (&args, dimension));
	}
	else
	{
		ret = g_slist_prepend (ret,
		                       cdn_instruction_number_new_from_string ("0"));
	}

	return g_slist_reverse (ret);
}

static CdnInstruction *
zeros_macro (CdnExpression *expression,
             ParserContext *context)
{
	GSList *second = NULL;
	GSList *first = NULL;
	CdnDimension dimension;
	GSList *ptr;

	if (context->stack)
	{
		second = context->stack->data;
	}

	if (context->stack->next)
	{
		first = context->stack->next->data;
	}

	if (!second || !first ||
	    second->next || first->next ||
	    !CDN_IS_INSTRUCTION_NUMBER (second->data) ||
	    !CDN_IS_INSTRUCTION_NUMBER (first->data))
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `zeros' function can only be called with 2 number arguments");

		return NULL;
	}

	dimension.rows = (gint)rint (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (first->data)));
	dimension.columns = (gint)rint (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (second->data)));

	instructions_pop (expression);
	instructions_pop (expression);

	g_slist_free (context->stack->next->data);
	g_slist_free (context->stack->data);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	ptr = make_zeros (&dimension);

	while (ptr)
	{
		if (ptr->next)
		{
			instructions_push (expression,
			                   ptr->data,
			                   context);

			ptr = g_slist_delete_link (ptr, ptr);
		}
		else
		{
			CdnInstruction *ret;

			ret = ptr->data;
			ptr = g_slist_delete_link (ptr, ptr);

			return ret;
		}
	}

	return NULL;
}

static CdnInstruction *
eye_macro (CdnExpression *expression,
           ParserContext *context)
{
	GSList *first = NULL;
	gint num;

	if (context->stack)
	{
		first = context->stack->data;
	}

	if (!first ||
	    first->next ||
	    !CDN_IS_INSTRUCTION_NUMBER (first->data))
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `eye' function can only be called with 1 number argument");

		return NULL;
	}

	num = (gint)rint (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (first->data)));

	instructions_pop (expression);
	g_slist_free (context->stack->data);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	// Add identity matrix
	if (num != 1)
	{
		gint r;
		gint i = 0;
		CdnStackArgs args;
		CdnDimension dim;

		cdn_stack_args_init (&args, num * num);

		dim.rows = num;
		dim.columns = num;

		for (r = 0; r < num; ++r)
		{
			gint c;

			for (c = 0; c < num; ++c)
			{
				gchar const *s;

				s = (c == r ? "1" : "0");

				instructions_push (expression,
				                   cdn_instruction_number_new_from_string (s),
				                   context);

				args.args[i].dimension = cdn_dimension_one;

				if (c != r)
				{
					cdn_stack_arg_set_sparsity_one (&args.args[i], 0);
				}

				++i;
			}
		}

		return cdn_instruction_matrix_new (&args,
		                                   &dim);
	}
	else
	{
		return cdn_instruction_number_new_from_string ("1");
	}
}

static CdnInstruction *
length_macro (CdnExpression *expression,
              ParserContext *context)
{
	GSList *first = NULL;
	gchar *s;
	CdnStackArgs args;
	CdnInstruction *ret;

	if (context->stack)
	{
		first = context->stack->data;
	}

	if (!first || first->next)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `length' function expects 1 argument");

		return NULL;
	}

	get_argdim (expression, context, 1, &args);

	while (first)
	{
		instructions_pop (expression);
		first = g_slist_delete_link (first, first);
	}

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	s = g_strdup_printf ("%d", cdn_dimension_size (&args.args[0].dimension));

	cdn_stack_args_destroy (&args);
	ret = cdn_instruction_number_new_from_string (s);
	g_free (s);

	return ret;
}

static CdnInstruction *
size_macro (CdnExpression *expression,
            ParserContext *context)
{
	GSList *first = NULL;
	gchar *sr;
	gchar *sc;
	CdnStackArgs args;
	CdnStackArg nargs[2] = {CDN_STACK_ARG_EMPTY, CDN_STACK_ARG_EMPTY};
	CdnDimension dim;

	if (context->stack)
	{
		first = context->stack->data;
	}

	if (!first || first->next)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `size' function expects 1 argument");

		return NULL;
	}

	get_argdim (expression, context, 1, &args);

	while (first)
	{
		instructions_pop (expression);
		first = g_slist_delete_link (first, first);
	}

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	sr = g_strdup_printf ("%d", args.args[0].rows);
	sc = g_strdup_printf ("%d", args.args[0].columns);

	instructions_push (expression,
	                   cdn_instruction_number_new_from_string (sr),
	                   context);

	instructions_push (expression,
	                   cdn_instruction_number_new_from_string (sc),
	                   context);

	g_free (sr);
	g_free (sc);

	cdn_stack_args_destroy (&args);

	args.num = 2;
	args.args = nargs;

	args.args[0].dimension = cdn_dimension_one;
	args.args[1].dimension = cdn_dimension_one;

	dim.rows = 1;
	dim.columns = 2;

	return cdn_instruction_matrix_new (&args, &dim);
}

static void
swap_arguments_index (CdnExpression *expression,
                      ParserContext *context,
                      gint           numargs)
{
	// Swap the last two arguments on the stack
	GSList *first = context->stack->data;
	GSList *second = context->stack->next->data;
	GSList *third = numargs > 2 ? context->stack->next->next->data : NULL;
	GSList *fi;
	GSList *si;
	GSList *tmp;

	if (numargs == 2)
	{
		swap_arguments (expression, context);
		return;
	}

	context->stack->data = third;
	context->stack->next->data = first;
	context->stack->next->next->data = second;

	// Then also on the instruction set
	fi = g_slist_nth (expression->priv->instructions, g_slist_length (first) + g_slist_length (second) - 1);
	si = g_slist_nth (fi, g_slist_length (third));

	tmp = fi->next;
	fi->next = si->next;
	si->next = expression->priv->instructions;

	expression->priv->instructions = tmp;
}

static void
swap_arguments_last_to_first (CdnExpression *expression,
                              ParserContext *context)
{
	GSList *first = context->stack->data;
	GSList *second = context->stack->next->data;
	GSList *third = context->stack->next->next->data;

	GSList *firstinstr;
	GSList *secondinstr;
	GSList *thirdinstr;
	GSList *start;

	// Swaps b, c, a to a, b, c
	context->stack->data = second;
	context->stack->next->data = third;
	context->stack->next->next->data = first;

	// Point to item in list being the LAST instruction of the set
	firstinstr = g_slist_nth (expression->priv->instructions, g_slist_length (first) - 1);
	secondinstr = g_slist_nth (firstinstr, g_slist_length (second));
	thirdinstr = g_slist_nth (secondinstr, g_slist_length (third));

	// Swap sublists around
	start = expression->priv->instructions;

	expression->priv->instructions = firstinstr->next;
	firstinstr->next = thirdinstr->next;
	thirdinstr->next = start;
}

static GSList *
pop_stack (CdnExpression *expression,
           ParserContext *context)
{
	GSList *instrs;
	GSList *tmp;
	GSList *nth;

	instrs = context->stack->data;

	// Pop these instructions
	nth = g_slist_nth (expression->priv->instructions,
	                   g_slist_length (instrs) - 1);

	tmp = expression->priv->instructions;
	expression->priv->instructions = nth->next;

	nth->next = NULL;
	g_slist_free (tmp);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	return instrs;
}

static gboolean
wrap_dotted (CdnExpression  *expression,
             ParserContext  *context,
             gchar const    *dotname,
             gint            order)
{
	CdnOperator *op;
	CdnExpression *expr;
	GSList *instrs;
	gchar *orders;
	GError *err = NULL;
	gboolean ret;
	CdnStackArgs nargs = {0,};

	// Create new expression from stack
	if (!context->stack->data)
	{
		return FALSE;
	}

	instrs = pop_stack (expression, context);

	expr = cdn_expression_new (dotname);

	cdn_expression_set_instructions_take (expr, instrs);
	g_slist_foreach (instrs, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instrs);

	instrs = g_slist_prepend (NULL, expr);

	orders = g_strdup_printf ("%d", order);
	expr = cdn_expression_new (orders);
	cdn_expression_compile (expr, NULL, NULL);
	instrs = g_slist_append (instrs, expr);

	op = cdn_operators_instantiate ("dt",
	                                (GSList const **)&instrs,
	                                1,
	                                NULL,
	                                0,
	                                &nargs,
	                                context->context,
	                                &err);

	g_slist_free (instrs);

	if (!op)
	{
		parser_failed_error (expression,
		                     context,
		                     err);

		return FALSE;
	}

	ret = instructions_push (expression,
	                         cdn_instruction_custom_operator_new (op),
	                         context);

	g_object_unref (op);

	return ret;
}

#define print_instructions(instrs) \
{ \
	GSList *__tmp = instrs; \
	while (__tmp) \
	{ \
		g_printf ("%s", cdn_instruction_to_string (__tmp->data)); \
\
		__tmp = g_slist_next (__tmp); \
\
		if (__tmp) \
		{ \
			g_print (" →  "); \
		} \
	} \
\
	g_print ("\n"); \
}

static void
replace_arg_with_zeros (CdnExpression      *expression,
                        ParserContext      *context,
                        gint                i,
                        CdnDimension const *dimension)
{
	GSList *instrptr;
	GSList *stackptr;
	gint num;

	instrptr = NULL;
	stackptr = context->stack;

	while (i > 0)
	{
		if (!instrptr)
		{
			instrptr = g_slist_nth (expression->priv->instructions,
			                        g_slist_length (stackptr->data) - 1);
		}
		else
		{
			instrptr = g_slist_nth (instrptr,
			                        g_slist_length (stackptr->data));
		}

		stackptr = g_slist_next (stackptr);
		--i;
	}

	num = g_slist_length (stackptr->data);

	g_slist_free (stackptr->data);
	stackptr->data = make_zeros (dimension);

	for (i = 0; i < num; ++i)
	{
		expression->priv->instructions =
			g_slist_delete_link (expression->priv->instructions,
			                     instrptr ? instrptr->next : expression->priv->instructions);
	}

	stackptr = stackptr->data;

	while (stackptr)
	{
		GSList *next;

		if (instrptr)
		{
			next = instrptr->next;

			instrptr->next = g_slist_prepend (NULL, stackptr->data);
			instrptr->next->next = next;

			next = instrptr;
		}
		else
		{
			next = expression->priv->instructions;
			expression->priv->instructions = g_slist_prepend (NULL, stackptr->data);
			expression->priv->instructions->next = next;

			next = NULL;
		}

		instrptr = next;
		stackptr = g_slist_next (stackptr);
	}
}

static void
instructions_push_all (CdnExpression *expression,
                       GSList const  *instrs,
                       ParserContext *context)
{
	while (instrs)
	{
		instructions_push (expression,
		                   cdn_mini_object_copy (instrs->data),
		                   context);

		instrs = g_slist_next (instrs);
	}
}

static gboolean
parse_function (CdnExpression *expression,
                gchar const   *name,
                gchar const   *cname,
                ParserContext *context)
{
	/* Try custom function first */
	CdnFunction *function;
	guint fid = 0;
	gint arguments = 0;
	gint n_implicit = 0;
	gint n_optional = 0;
	gboolean ret = TRUE;
	gboolean isrand = FALSE;
	CdnInstruction *instruction = NULL;
	gchar *dotname = NULL;
	gint order = 1;
	gboolean dotted = FALSE;

	if (!cname)
	{
		function = cdn_compile_context_lookup_function (context->context,
		                                                name);

		if (!function)
		{
			dotname = cdn_decompose_dot (name, &order);
		}

	}
	else
	{
		function = parse_context_function (expression, name, cname, context);

		if (!function)
		{
			dotname = cdn_decompose_dot (cname, &order);
		}
	}

	/* Try builtin function */
	if (function == NULL)
	{
		if (cname == NULL)
		{
			// Special case 'rand' here
			if (g_strcmp0 (name, "rand") == 0)
			{
				isrand = TRUE;
				arguments = -1;
			}
			else
			{
				fid = cdn_math_function_lookup (name, &arguments);

				if (!fid && dotname)
				{
					dotted = TRUE;

					function = cdn_compile_context_lookup_function (context->context,
					                                                dotname);
				}

				if (!function && !fid)
				{
					dotted = TRUE;

					fid = cdn_math_function_lookup (dotname,
					                                &arguments);
				}

				if (!function && !fid)
				{
					return parser_failed (expression,
					                      context,
					                      CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND,
					                      "Function %s could not be found",
					                      name);
				}
			}
		}
		else
		{
			if (dotname)
			{
				dotted = TRUE;
				function = parse_context_function (expression,
				                                   name,
				                                   dotname,
				                                   context);
			}

			if (!function)
			{
				return parser_failed (expression,
				                      context,
				                      CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND,
				                      "Function %s.%s could not be found",
				                      name, cname);
			}
		}
	}

	if (function)
	{
		n_implicit = (gint)cdn_function_get_n_implicit (function);
		arguments = (gint)cdn_function_get_n_arguments (function);
		n_optional = (gint)cdn_function_get_n_optional (function);
	}

	// parse arguments
	gint numargs = 0;

	if (!parse_function_arguments (expression, context, &numargs))
	{
		return FALSE;
	}

	if ((function != NULL && (numargs > (arguments - n_implicit) ||
	                          numargs < (arguments - n_implicit - n_optional))) ||
	                          (function == NULL && arguments != -1 && numargs != arguments))
	{
		return parser_failed (expression,
		                      context,
		                      CDN_COMPILE_ERROR_MAXARG,
		                      "Expected number of arguments (%d) for function `%s' does not match (got %d)",
		                      arguments - n_implicit,
		                      name,
		                      numargs);
	}

	if (function)
	{
		GList const *ar;
		GList *start;
		CdnExpression *defval;

		ar = cdn_function_get_arguments (function);

		/* Set optional arguments */
		start = g_list_nth ((GList *)ar, numargs);

		while (start &&
		       cdn_function_argument_get_explicit (start->data) &&
		       (defval = cdn_function_argument_get_default_value (start->data)))
		{
			GSList const *instrs;

			// Compile the default value if needed
			instrs = cdn_expression_get_instructions (defval);

			if (!instrs)
			{
				CdnCompileContext *ctx;

				ctx = cdn_object_get_compile_context (CDN_OBJECT (function),
				                                      NULL);

				ret = cdn_expression_compile (defval,
				                              ctx,
				                              context->error);

				g_object_unref (ctx);

				if (!ret)
				{
					break;
				}

				instrs = cdn_expression_get_instructions (defval);
			}

			instructions_push_all (expression, instrs, context);
			start = g_list_next (start);
		}

		/* Now lookup implicit arguments */
		start = g_list_nth ((GList *)ar, arguments - n_implicit);

		while (ret && start)
		{
			CdnFunctionArgument *a;
			gchar const *aname;
			CdnVariable *prop;
			gchar const *ptr;

			a = start->data;

			aname = cdn_function_argument_get_name (a);

			start = g_list_next (start);
			++numargs;

			if ((ptr = strchr (aname, '.')) != NULL)
			{
				gchar *id;

				id = g_strndup (aname, ptr - aname);

				if (parse_dot_variable (expression, id, ptr + 1, context))
				{
					g_free (id);
					continue;
				}

				g_free (id);
			}

			prop = lookup_variable (expression, context, aname);

			if (!prop)
			{
				return parser_failed (expression,
				                      context,
				                      CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
				                      "The implicit property `%s' for function `%s' is not found",
				                      aname,
				                      name);
			}

			if (!instructions_push (expression,
			                        cdn_instruction_variable_new (prop),
			                        context))
			{
				ret = FALSE;
				break;
			}
		}
	}

	if (function == NULL && ret)
	{
		if (isrand)
		{
			CdnStackArgs args;

			get_argdim (expression, context, numargs, &args);

			instruction = cdn_instruction_rand_new (&args);
			cdn_stack_args_destroy (&args);

			// Rand of 1 or 2 arguments (i.e. with bounds) are
			// injected with a lerp function
			if (numargs == 1)
			{
				// Push rand instruction
				instructions_push (expression,
				                   instruction,
				                   context);

				// Swap upper bound and rand
				swap_arguments (expression, context);

				// Push the lower bound
				instructions_push (expression,
				                   cdn_instruction_number_new_from_string ("0"),
				                   context);

				// Swap upper and lower bound
				swap_arguments (expression, context);
			}
			else if (numargs == 2)
			{
				// Push rand instruction
				instructions_push (expression,
				                   instruction,
				                   context);

				swap_arguments_last_to_first (expression, context);
			}

			if (numargs > 0)
			{
				// Make the 'real' instruction the lerp
				get_argdim (expression, context, 3, &args);

				instruction = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LERP,
				                                            NULL,
				                                            &args);

				cdn_stack_args_destroy (&args);
			}
		}
		else
		{
			CdnStackArgs args;

			if (fid == CDN_MATH_FUNCTION_TYPE_LINSOLVE)
			{
				// For linsolve, we swap the arguments A and B
				// because that's the way the linsolve math
				// function wants the arguments, but it's less
				// intuitive to have the user specify them
				// in that order
				swap_arguments (expression, context);
			}
			else if (fid == CDN_MATH_FUNCTION_TYPE_INDEX ||
			         fid == CDN_MATH_FUNCTION_TYPE_SLINSOLVE)
			{
				// For index, we move the first argument to
				// the last (i.e. the thing to index)
				// because math wants it this way (it's)
				// more efficient
				swap_arguments_index (expression,
				                      context,
				                      numargs);
			}

			switch (fid)
			{
				case CDN_MATH_FUNCTION_TYPE_ZEROS:
					instruction = zeros_macro (expression,
					                           context);
				break;
				case CDN_MATH_FUNCTION_TYPE_EYE:
					instruction = eye_macro (expression,
					                         context);
				break;
				case CDN_MATH_FUNCTION_TYPE_LENGTH:
					instruction = length_macro (expression,
					                            context);
				break;
				case CDN_MATH_FUNCTION_TYPE_SIZE:
					instruction = size_macro (expression,
					                          context);
				break;
				break;
				default:
					get_argdim (expression,
					            context,
					            numargs,
					            &args);

					instruction =
						cdn_instruction_function_new (fid,
						                              name,
						                              &args);

					cdn_stack_args_destroy (&args);
				break;
			}
		}
	}
	else if (ret)
	{
		CdnStackArgs args;

		get_argdim (expression, context, arguments, &args);

		function = cdn_function_for_dimension (function,
		                                       &args);

		// Recursively compile the function here
		ret = recurse_compile (expression, context, function);

		if (ret)
		{
			GSList const *randargs;
			CdnStackManipulation const *smanip;

			smanip = cdn_function_get_stack_manipulation (function);

			instruction = cdn_instruction_custom_function_new (function,
			                                                   &smanip->pop);

			// If needed, copy rand instructions used in the function
			// as arguments to the function call
			randargs = _cdn_function_get_rand_arguments (function);

			while (randargs)
			{
				CdnExpression *def;

				def = cdn_function_argument_get_default_value (randargs->data);

				instructions_push_all (expression,
				                       cdn_expression_get_instructions (def),
				                       context);

				randargs = g_slist_next (randargs);
			}

			GList const *arg;
			gint i = arguments - 1;

			// Replace unused function arguments with 0, haha!
			for (arg = cdn_function_get_arguments (function); arg; arg = g_list_next (arg))
			{
				if (cdn_function_argument_get_unused (arg->data))
				{
					replace_arg_with_zeros (expression,
					                        context,
					                        i,
					                        &args.args[i].dimension);
				}

				if (i == 0)
				{
					// This happens only when we added args
					// for the bubbled rand instructions
					break;
				}

				--i;
			}
		}

		g_object_unref (function);
		cdn_stack_args_destroy (&args);
	}

	if (instruction)
	{
		instructions_push (expression, instruction, context);

		if (dotted)
		{
			gchar *text;
			gchar const *ptr;

			ptr = *context->buffer - cdn_expression_get_error_start (expression);

			text = g_strndup (ptr, *context->buffer - ptr);

			ret = wrap_dotted (expression,
			                   context,
			                   text,
			                   order);

			g_free (text);
		}

		return ret;
	}

	g_free (dotname);
	return FALSE;
}

static void
free_expressions (GSList *lst)
{
	g_slist_foreach (lst, (GFunc)g_object_unref, NULL);
	g_slist_free (lst);
}

static GSList **
convert_2dim_slist (GSList const *lst,
                    gint         *num)
{
	GSList **ret;
	gint i;

	*num = g_slist_length ((GSList *)lst);

	ret = g_new0 (GSList *, *num);

	for (i = 0; i < *num; ++i)
	{
		ret[i] = lst->data;
		lst = g_slist_next (lst);
	}

	return ret;
}

static gboolean
parse_matrix_row (CdnExpression *expression,
                  ParserContext *context,
                  gboolean      *isend,
                  CdnDimension  *dimension,
                  gint          *numpop)
{
	*isend = FALSE;

	CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

	if (!next)
	{
		return FALSE;
	}

	*isend = CDN_TOKEN_IS_OPERATOR (next) &&
	         CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END;

	cdn_token_free (next);

	if (*isend)
	{
		return TRUE;
	}

	*numpop = 0;
	dimension->rows = 0;
	dimension->columns = 0;

	while (TRUE)
	{
		CdnStackArgs args;

		if (dimension->rows > 1 && *numpop > 1)
		{
			// Here we will need to concat the rows with a concat
			// operation, so we will insert a matrix operation here
			// if needed
			get_argdim (expression, context, *numpop, &args);

			instructions_push (expression,
			                   cdn_instruction_matrix_new (&args,
			                                               dimension),
			                   context);

			*numpop = 1;
			cdn_stack_args_destroy (&args);
		}

		if (!parse_expression (expression, context, -1, 0))
		{
			return FALSE;
		}

		get_argdim (expression, context, 1, &args);
		++*numpop;

		// Check argument consistency
		if (dimension->rows != 0)
		{
			if (dimension->rows != args.args[0].rows)
			{
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
				               "Cannot concatenate %d row%s with %d row%s",
				               dimension->rows,
				               dimension->rows > 1 ? "s" : "",
				               args.args[0].rows,
				               args.args[0].rows > 1 ? "s" : "");

				cdn_stack_args_destroy (&args);
				return FALSE;
			}
			else if (dimension->rows != 1)
			{
				CdnStackArgs nargs;
				CdnStackArg ar[2] = {CDN_STACK_ARG_EMPTY, CDN_STACK_ARG_EMPTY};

				nargs.args = ar;
				nargs.num = 2;

				ar[0].dimension = *dimension;
				ar[1].dimension = args.args[0].dimension;

				// Here we have multiple rows concatenated with
				// multiple rows. We are going to implement this
				// using a hcat operator
				instructions_push (expression,
				                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_HCAT,
				                                                 "hcat",
				                                                 &nargs),
				                   context);

				*numpop = 1;
			}

			dimension->columns += args.args[0].columns;
		}
		else
		{
			*dimension = args.args[0].dimension;
		}

		cdn_stack_args_destroy (&args);

		CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

		if (!next)
		{
			return FALSE;
		}

		CdnTokenOperatorType type = CDN_TOKEN_OPERATOR_TYPE_NONE;

		if (CDN_TOKEN_IS_OPERATOR (next))
		{
			type = CDN_TOKEN_OPERATOR (next)->type;
		}

		cdn_token_free (next);

		// Check end of the line
		if (type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END ||
		    type == CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON)
		{
			cdn_token_free (cdn_tokenizer_next (context->buffer));
			*isend = (type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END);

			return TRUE;
		}
		else if (type == CDN_TOKEN_OPERATOR_TYPE_COMMA)
		{
			// Consume
			cdn_token_free (cdn_tokenizer_next (context->buffer));
		}
	}
}

static gboolean
parse_matrix (CdnExpression *expression,
              ParserContext *context)
{
	CdnDimension tdim = CDN_DIMENSION (0, 0);
	gint tnumpop = 0;
	CdnStackArgs args;

	while (TRUE)
	{
		gboolean isend = TRUE;
		CdnDimension dimension = CDN_DIMENSION (0, 0);
		gint numpop = 0;

		if (!parse_matrix_row (expression,
		                       context,
		                       &isend,
		                       &dimension,
		                       &numpop))
		{
			return FALSE;
		}

		if (tdim.rows == 0)
		{
			tdim.rows = dimension.rows;
			tdim.columns = dimension.columns;

			tnumpop = numpop;
		}
		else if (tdim.columns != dimension.columns)
		{
			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			                      "Cannot concatenate %d column%s with %d column%s",
			                      tdim.columns,
			                      tdim.columns > 1 ? "s" : "",
			                      dimension.columns,
			                      dimension.columns > 1 ? "s" : "");
		}
		else
		{
			tnumpop += numpop;
			tdim.rows += dimension.rows;
		}

		if (isend)
		{
			break;
		}
	}

	get_argdim (expression, context, tnumpop, &args);

	// note that popdims memory is consumed by the matrix instruction
	// and does not need to be freed
	instructions_push (expression,
	                   cdn_instruction_matrix_new (&args,
	                                               &tdim),
	                   context);

	cdn_stack_args_destroy (&args);

	return TRUE;
}

static gboolean
parse_indexing (CdnExpression *expression,
                ParserContext *context)
{
	CdnStackArgs args;
	gint numargs = 0;
	CdnTokenOperatorType type;

	// Indexing is done using one or two arguments, which can be matrices
	// themselves
	CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));
	}
	else
	{
		cdn_token_free (next);

		if (!parse_expression (expression, context, -1, 0))
		{
			return FALSE;
		}

		++numargs;

		next = cdn_tokenizer_peek (*(context->buffer));

		if (CDN_TOKEN_IS_OPERATOR (next))
		{
			type = CDN_TOKEN_OPERATOR (next)->type;
			cdn_token_free (next);

			if (type != CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
			{
				if (type == CDN_TOKEN_OPERATOR_TYPE_COMMA)
				{
					cdn_token_free (cdn_tokenizer_next (context->buffer));
				}

				if (!parse_expression (expression, context, -1, 0))
				{
					return FALSE;
				}

				++numargs;
			}
		}
		else
		{
			cdn_token_free (next);

			if (!parse_expression (expression, context, -1, 0))
			{
				return FALSE;
			}

			++numargs;
		}
	}

	next = cdn_tokenizer_next (context->buffer);
	type = CDN_TOKEN_OPERATOR (next)->type;

	if (type != CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		cdn_token_free (next);

		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "Expected `]' but got `%s'",
		               next->text ? next->text : NULL);

		return FALSE;
	}

	swap_arguments_index (expression, context, numargs + 1);

	get_argdim (expression, context, numargs + 1, &args);

	instructions_push (expression,
	                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_INDEX,
	                                                 "index",
	                                                 &args),
	                   context);

	cdn_stack_args_destroy (&args);

	cdn_token_free (next);

	return TRUE;
}

static gboolean
parse_constant (CdnExpression  *expression,
                gchar const    *name,
                ParserContext  *context)
{
	gboolean found = FALSE;
	gdouble val;
	CdnInstruction *instr;

	val = cdn_math_constant_lookup (name, &found);

	if (!found)
	{
		return FALSE;
	}

	instr = cdn_instruction_number_new (val);
	cdn_instruction_number_set_representation (CDN_INSTRUCTION_NUMBER (instr), name);

	instructions_push (expression,
	                   instr,
	                   context);

	return TRUE;
}

static gboolean
parse_identifier_as_variable (CdnExpression *expression,
                              gchar const   *id,
                              ParserContext *context)
{
	gboolean ret;

	ret = parse_variable (expression, id, context);

	if (!ret)
	{
		// try parsing constants
		ret = parse_constant (expression, id, context);

		if (!ret && !cdn_compile_error_get_error (context->error))
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_TOKEN,
			               "Could not find variable or constant `%s'",
			               id);
		}
	}

	return ret;
}

static gboolean
parse_custom_operator (CdnExpression *expression,
                       gchar const   *name,
                       ParserContext *context)
{
	CdnOperatorClass *klass;
	CdnOperator *op;
	gboolean isref;
	GSList *expressions = NULL;
	GSList *multiexpr = NULL;
	gint num_arguments = 0;
	gboolean isdiff;
	CdnToken *next;
	gboolean loopit = TRUE;
	gchar const *expr_start;
	gchar const *expr_end;
	GSList *indices = NULL;
	GSList **multiret;
	CdnInstruction *instruction;
	GSList **exprs;
	gint num_exprs;
	GSList **inds;
	gint num_inds;
	gboolean islinsolve;
	GError *err = NULL;
	CdnStackArgs args;
	gint newnum;

	klass = cdn_operators_find_class (name);

	if (klass == NULL)
	{
		return parse_identifier_as_variable (expression, name, context);
	}
	else
	{
		// Consume the bracket
		cdn_token_free (cdn_tokenizer_next (context->buffer));
	}

	// parse arguments
	next = cdn_tokenizer_peek (*(context->buffer));

	expr_start = *(context->buffer);
	expr_end = expr_start;

	multiret = &multiexpr;

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		next = cdn_tokenizer_peek (*(context->buffer));

		if (!next || !CDN_TOKEN_IS_OPERATOR (next) ||
		    CDN_TOKEN_OPERATOR (next)->type != CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
		{
			loopit = FALSE;
		}
		else
		{
			cdn_token_free (cdn_tokenizer_next (context->buffer));
			multiret = &indices;
		}

		cdn_token_free (next);
	}
	else
	{
		cdn_token_free (next);
	}

	cdn_compile_context_save (context->context);

	isdiff = CDN_IS_OPERATOR_PDIFF_CLASS (klass) ||
	         CDN_IS_OPERATOR_DIFF_CLASS (klass);

	islinsolve = CDN_IS_OPERATOR_LINSOLVE_CLASS (klass);

	if ((isdiff || islinsolve) && multiret == &multiexpr)
	{
		cdn_compile_context_set_function_ref_priority (context->context,
		                                               TRUE);

		cdn_compile_context_set_function_arg_priority (context->context,
		                                               TRUE);
	}

	while (loopit)
	{
		/* Mark where it starts */
		GSList *start = expression->priv->instructions;

		if (!parse_expression (expression, context, -1, 0))
		{
			return FALSE;
		}

		expr_end = *(context->buffer);
		GSList *newinst = NULL;

		if (isdiff && multiret == &multiexpr)
		{
			cdn_compile_context_set_function_ref_priority (context->context,
			                                               FALSE);
		}

		while (expression->priv->instructions != start)
		{
			CdnInstruction *inst = instructions_pop (expression);

			/* This is a bit of a hack, but we are going
			   to add the function object as a final context.
			   This is mostly for the diff type operators to
			   compile properly (resolve arguments)... */
			if ((isdiff || islinsolve) && multiret == &multiexpr &&
			    (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (inst) ||
			     CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst)))
			{
				CdnFunction *func = NULL;

				if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (inst))
				{
					CdnInstructionCustomFunctionRef *f;

					f = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (inst);
					func = cdn_instruction_custom_function_ref_get_function (f);
				}
				else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst))
				{
					CdnInstructionCustomOperatorRef *f;
					CdnOperator *op;

					f = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (inst);
					op = cdn_instruction_custom_operator_ref_get_operator (f);

					func = cdn_operator_get_primary_function (op);
				}

				if (func)
				{
					cdn_compile_context_append_object (context->context,
					                                   CDN_OBJECT (func));
				}
			}

			newinst = g_slist_prepend (newinst, inst);
		}

		gchar *t = g_strndup (expr_start, expr_end - expr_start);
		CdnExpression *sub = cdn_expression_new (t);

		g_free (t);
		cdn_expression_set_instructions_take (sub, newinst);

		g_slist_foreach (newinst, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (newinst);

		g_object_ref_sink (sub);
		expressions = g_slist_prepend (expressions, sub);

		// see what's next
		next = cdn_tokenizer_peek (*(context->buffer));

		if (!next || !CDN_TOKEN_IS_OPERATOR (next))
		{
			g_slist_foreach (expressions, (GFunc)g_object_unref, NULL);
			g_slist_free (expressions);

			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_INVALID_TOKEN,
			                      "Expected `,' or `]', but got %s",
			                      next ? next->text : "(nothing)");
		}

		CdnTokenOperatorType type = CDN_TOKEN_OPERATOR (next)->type;

		if (type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
		{
			cdn_token_free (next);
			cdn_token_free (cdn_tokenizer_next (context->buffer));

			if (multiret == &multiexpr)
			{
				next = cdn_tokenizer_peek (*(context->buffer));

				if (next && CDN_TOKEN_IS_OPERATOR (next) &&
				    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START)
				{
					if (islinsolve)
					{
						cdn_compile_context_set_function_ref_priority (context->context,
						                                               FALSE);
					}

					*multiret = g_slist_prepend (*multiret,
					                             g_slist_reverse (expressions));

					expressions = NULL;
					multiexpr = g_slist_reverse (multiexpr);

					multiret = &indices;
				}
				else
				{
					cdn_token_free (next);
					break;
				}
			}
			else
			{
				break;
			}
		}
		else if (type != CDN_TOKEN_OPERATOR_TYPE_COMMA &&
		         type != CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON)
		{
			g_slist_foreach (expressions, (GFunc)g_object_unref, NULL);
			g_slist_free (expressions);

			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected `,' but got %s",
			               next->text);

			cdn_token_free (next);
			return FALSE;
		}

		if (type == CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON)
		{
			*multiret = g_slist_prepend (*multiret,
			                             g_slist_reverse (expressions));

			expressions = NULL;
		}

		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		expr_start = *(context->buffer);
	}

	*multiret = g_slist_prepend (*multiret,
	                             g_slist_reverse (expressions));

	*multiret = g_slist_reverse (*multiret);

	cdn_compile_context_restore (context->context);

	next = cdn_tokenizer_peek (*(context->buffer));

	isref = TRUE;

	if (next &&
	    CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_NODE_START)
	{
		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		isref = FALSE;

		if (!parse_function_arguments (expression, context, &num_arguments))
		{
			return FALSE;
		}
	}
	else
	{
		cdn_token_free (next);
	}

	exprs = convert_2dim_slist (multiexpr, &num_exprs);
	inds = convert_2dim_slist (indices, &num_inds);

	get_argdim (expression, context, num_arguments, &args);

	op = cdn_operators_instantiate (name,
	                                (GSList const **)exprs,
	                                num_exprs,
	                                (GSList const **)inds,
	                                num_inds,
	                                &args,
	                                context->context,
	                                &err);

	cdn_stack_args_destroy (&args);

	if (!op && context->error)
	{
		cdn_compile_error_set (context->error,
		                       err,
		                       NULL,
		                       NULL,
		                       NULL,
		                       expression);
	}

	if (err)
	{
		g_error_free (err);
	}

	g_slist_foreach (multiexpr, (GFunc)free_expressions, NULL);
	g_slist_free (multiexpr);

	g_slist_foreach (indices, (GFunc)free_expressions, NULL);
	g_slist_free (indices);

	g_free (exprs);
	g_free (inds);

	if (!op)
	{
		return FALSE;
	}

	newnum = num_arguments;

	if (!isref)
	{
		CdnFunction *func;

		func = cdn_operator_get_primary_function (op);

		// We need to add implicit arguments on the stack here
		if (func)
		{
			GList *start;
			gint extra = 0;
			CdnStackArgs args;
			gint i;

			start = (GList *)cdn_function_get_arguments (func);

			start = g_list_nth ((GList *)start,
			                   num_arguments);

			while (start)
			{
				CdnFunctionArgument *a;
				CdnExpression *defval;
				gboolean isexplicit;

				a = start->data;
				start = g_list_next (start);

				isexplicit = cdn_function_argument_get_explicit (a);

				if (isexplicit && (defval = cdn_function_argument_get_default_value (a)))
				{
					GSList const *instrs;

					// Compile the default value if needed
					instrs = cdn_expression_get_instructions (defval);

					if (!instrs)
					{
						CdnCompileContext *ctx;
						gboolean ret;

						ctx = cdn_object_get_compile_context (CDN_OBJECT (func),
						                                      NULL);

						ret = cdn_expression_compile (defval,
						                              ctx,
						                              context->error);

						g_object_unref (ctx);

						if (!ret)
						{
							return FALSE;
						}

						instrs = cdn_expression_get_instructions (defval);
					}

					while (instrs)
					{
						++newnum;

						if (!instructions_push (expression,
						                        cdn_mini_object_copy (instrs->data),
						                        context))
						{
							return FALSE;
						}

						instrs = g_slist_next (instrs);
					}

				}
				else if (!isexplicit)
				{
					gchar const *aname;
					CdnVariable *prop;
					gchar const *ptr;

					aname = cdn_function_argument_get_name (a);
					++extra;

					if ((ptr = strchr (aname, '.')) != NULL)
					{
						gchar *id;

						id = g_strndup (aname, ptr - aname);

						if (parse_dot_variable (expression, id, ptr + 1, context))
						{
							g_free (id);
							continue;
						}

						g_free (id);
					}

					prop = lookup_variable (expression, context, aname);

					if (!prop)
					{
						return parser_failed (expression,
						                      context,
						                      CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
						                      "The implicit property `%s' for function `%s' is not found",
						                      aname,
						                      name);
					}

					if (!instructions_push (expression,
					                        cdn_instruction_variable_new (prop),
					                        context))
					{
						return FALSE;
					}

					++newnum;
				}
			}

			get_argdim (expression, context, newnum, &args);

			if (newnum > num_arguments)
			{
				_cdn_operator_set_arguments_dimension (op, &args);
			}

			start = (GList *)cdn_function_get_arguments (func);
			i = newnum - 1;

			while (start)
			{
				if (cdn_function_argument_get_unused (start->data))
				{
					replace_arg_with_zeros (expression,
					                        context,
					                        i,
					                        &args.args[i].dimension);
				}

				--i;
				start = g_list_next (start);
			}

			cdn_stack_args_destroy (&args);
		}
	}

	if (isref)
	{
		instruction = cdn_instruction_custom_operator_ref_new (op);
		g_object_unref (op);
	}
	else
	{
		instruction = cdn_instruction_custom_operator_new (op);
		g_object_unref (op);
	}

	instructions_push (expression, instruction, context);

	return TRUE;
}

static gboolean
parse_ternary_operator (CdnExpression     *expression,
                        CdnTokenOperator  *token,
                        ParserContext     *context)
{
	CdnStackArgs args;

	if (!parse_expression (expression, context, token->priority, token->left_assoc))
	{
		return FALSE;
	}

	// next token should be :
	CdnToken *next = cdn_tokenizer_peek (*context->buffer);

	if (!next)
	{
		return parser_failed (expression,
		                      context,
		                      CDN_COMPILE_ERROR_INVALID_TOKEN,
		                      "Expected `:' but got (nothing)");
	}

	gboolean istern = CDN_TOKEN_IS_OPERATOR (next) &&
	                  CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;

	if (!istern)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected `:' but got `%s'",
		               next->text);

		cdn_token_free (next);
		return FALSE;
	}

	cdn_token_free (cdn_tokenizer_next (context->buffer));
	CdnTokenOperator *op = CDN_TOKEN_OPERATOR (next);

	// do next expression
	if (!parse_expression (expression,
	                       context,
	                       op->priority,
	                       op->left_assoc))
	{
		cdn_token_free (next);
		return FALSE;
	}

	get_argdim (expression, context, 3, &args);

	instructions_push (expression,
	                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TERNARY,
	                                                 "?:",
	                                                 &args),
	                   context);

	cdn_stack_args_destroy (&args);

	return TRUE;
}

static gboolean
parse_group (CdnExpression *expression,
             ParserContext *context)
{
	if (!parse_expression (expression, context, -1, 0))
	{
		return FALSE;
	}

	CdnToken *next = cdn_tokenizer_peek (*context->buffer);
	gboolean groupend = next && (CDN_TOKEN_IS_OPERATOR (next) ||
	                             CDN_TOKEN_OPERATOR (next)->type != CDN_TOKEN_OPERATOR_TYPE_NODE_END);

	if (!groupend)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected `)' but got %s",
		               next ? next->text : "(nothing)");

		cdn_token_free (next);
		return FALSE;
	}

	cdn_token_free (next);
	cdn_token_free (cdn_tokenizer_next (context->buffer));

	return TRUE;
}

static gboolean
parse_unary_operator (CdnExpression *expression,
                      CdnToken      *token,
                      ParserContext *context)
{
	CdnTokenOperator *op = CDN_TOKEN_OPERATOR (token);
	gboolean ret = TRUE;
	CdnInstruction *inst = NULL;
	CdnStackArgs args = {0,};

	// handle group
	switch (op->type)
	{
		case CDN_TOKEN_OPERATOR_TYPE_NODE_START:
			cdn_token_free (cdn_tokenizer_next (context->buffer));
			return parse_group (expression, context);
		case CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START:
			cdn_token_free (cdn_tokenizer_next (context->buffer));
			return parse_matrix (expression, context);
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
		case CDN_TOKEN_OPERATOR_TYPE_NEGATE:
		break;
		default:
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected unary operator (-, +, !, ~) but got `%s'",
			               op->parent.text);
			ret = FALSE;
	}

	if (ret)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));
		ret = parse_expression (expression, context, 8, 1);
	}

	if (!ret)
	{
		return ret;
	}

	get_argdim (expression, context, 1, &args);

	switch (op->type)
	{
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
		{
			if (CDN_IS_INSTRUCTION_NUMBER (expression->priv->instructions->data))
			{
				inst = instructions_pop (expression);

				cdn_instruction_number_set_value (CDN_INSTRUCTION_NUMBER (inst),
				                                  cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (inst)) * -1);

				g_slist_free (context->stack->data);

				context->stack = g_slist_delete_link (context->stack,
				                                      context->stack);
			}
			else
			{
				inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
				                                     NULL,
				                                     &args);
			}
		}
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
		break;
		case CDN_TOKEN_OPERATOR_TYPE_NEGATE:
		{
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_NEGATE,
			                                     NULL,
			                                     &args);
		}
		break;
		default:
		break;
	}

	cdn_stack_args_destroy (&args);

	if (ret && inst)
	{
		instructions_push (expression, inst, context);
	}

	return ret;
}

static gboolean
parse_prime (CdnExpression *expression,
             ParserContext *context)
{
	GSList *instr;
	CdnOperator *op;
	GSList *exprs;
	CdnExpression *expr;
	GSList *item;
	GError *err = NULL;
	gboolean ret;
	CdnStackArgs nargs = {0,};

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "The prime operator can only appear after an expression");

		return FALSE;
	}

	for (item = instr; item; item = g_slist_next (item))
	{
		// Pop em
		instructions_pop (expression);
	}

	expr = cdn_expression_new0 ();
	cdn_expression_set_instructions_take (expr, instr);

	g_slist_foreach (instr, (GFunc)cdn_mini_object_unref, instr);
	g_slist_free (instr);

	exprs = g_slist_prepend (NULL, expr);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	op = cdn_operators_instantiate ("dt",
	                                (GSList const **)&exprs,
	                                1,
	                                NULL,
	                                0,
	                                &nargs,
	                                context->context,
	                                &err);

	if (!op && context->error)
	{
		cdn_compile_error_set (context->error,
		                       err,
		                       NULL,
		                       NULL,
		                       NULL,
		                       expression);
	}

	if (err)
	{
		g_error_free (err);
	}

	g_slist_free (exprs);

	if (!op)
	{
		return FALSE;
	}

	ret = instructions_push (expression,
	                         cdn_instruction_custom_operator_new (op),
	                         context);

	g_object_unref (op);

	return ret;
}

static gboolean
parse_transpose (CdnExpression *expression,
                 ParserContext *context)
{
	GSList *instr;
	gboolean ret;
	CdnStackArgs args;

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "The transpose operator can only appear after an expression");

		return FALSE;
	}

	get_argdim (expression, context, 1, &args);

	ret = instructions_push (expression,
	                         cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TRANSPOSE,
	                                                       "transpose",
	                                                       &args),
	                         context);

	cdn_stack_args_destroy (&args);
	return ret;
}

static gboolean
parse_square (CdnExpression *expression,
              ParserContext *context)
{
	GSList *instr;
	gboolean ret;
	CdnStackArgs args;

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "The transpose operator can only appear after an expression");

		return FALSE;
	}

	ret = instructions_push (expression,
	                         cdn_instruction_number_new_from_string ("2"),
	                         context);

	if (!ret)
	{
		return FALSE;
	}

	get_argdim (expression, context, 2, &args);

	ret = instructions_push (expression,
	                         cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                       "^",
	                                                       &args),
	                         context);

	cdn_stack_args_destroy (&args);
	return ret;
}

static gboolean
parse_operator (CdnExpression *expression,
                CdnToken      *token,
                ParserContext *context)
{
	CdnTokenOperator *op = CDN_TOKEN_OPERATOR (token);
	CdnStackArgs args;

	// handle ternary
	if (op->type == CDN_TOKEN_OPERATOR_TYPE_TERNARY_TRUE)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		return parse_ternary_operator (expression, CDN_TOKEN_OPERATOR (token), context);
	}
	else if (op->type == CDN_TOKEN_OPERATOR_TYPE_PRIME)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		// The prime makes a dt operator of the current stack
		return parse_prime (expression, context);
	}
	else if (op->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		return parse_indexing (expression, context);
	}
	else if (op->type == CDN_TOKEN_OPERATOR_TYPE_TRANSPOSE)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		return parse_transpose (expression, context);
	}
	else if (op->type == CDN_TOKEN_OPERATOR_TYPE_SQUARE)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		return parse_square (expression, context);
	}

	// consume token
	cdn_token_free (cdn_tokenizer_next (context->buffer));

	if (!parse_expression (expression, context, op->priority, op->left_assoc))
	{
		return FALSE;
	}

	CdnInstruction *inst = NULL;

	get_argdim (expression, context, 2, &args);

	switch (op->type)
	{
		// arithmetic
		case CDN_TOKEN_OPERATOR_TYPE_MULTIPLY:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MULTIPLY, "*", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_EMULTIPLY:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_EMULTIPLY, "*", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_DIVIDE:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_DIVIDE, "/", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MODULO:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MODULO, "%", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS, "+", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS, "-", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_POWER:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER, "^", &args);
		break;
		// logical
		case CDN_TOKEN_OPERATOR_TYPE_GREATER:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_GREATER, ">", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LESS, "<", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL, ">=", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL, "<=", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_EQUAL, "==", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_NEQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_NEQUAL, "!=", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_OR:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_OR, "||", &args);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_AND:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_AND, "&&", &args);
		break;
		default:
			cdn_stack_args_destroy (&args);
			return FALSE;
		break;
	}

	cdn_stack_args_destroy (&args);
	return instructions_push (expression, inst, context);
}

static gboolean
parse_variable (CdnExpression *expression,
                gchar const   *propname,
                ParserContext *context)
{
	CdnVariable *property;
	CdnFunction *f;
	gboolean prio;
	gboolean ret = TRUE;
	gchar *dotname;
	gint order = 1;
	gboolean dotted = FALSE;

	gchar *nname = g_strdup (propname);

	if (cdn_compile_context_get_function_arg_priority (context->context))
	{
		while (TRUE)
		{
			CdnToken *next = cdn_tokenizer_peek (*context->buffer);

			if (next &&
			    CDN_TOKEN_IS_OPERATOR (next) &&
			    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_PRIME)
			{
				gchar *tmp;
				tmp = nname;

				nname = g_strconcat (tmp, "'", NULL);
				g_free (tmp);

				cdn_token_free (next);
				cdn_token_free (cdn_tokenizer_next (context->buffer));
			}
			else
			{
				cdn_token_free (next);
				break;
			}
		}
	}

	property = lookup_variable (expression, context, nname);
	f = cdn_compile_context_lookup_function (context->context, nname);
	prio = cdn_compile_context_get_function_ref_priority (context->context);

	dotname = cdn_decompose_dot (nname, &order);

	if (dotname && !property && (!f || !prio))
	{
		dotted = TRUE;

		property = lookup_variable (expression,
		                            context,
		                            dotname);
	}

	if (f && (prio || (!prio && !property)))
	{
		instructions_push (expression,
		                   cdn_instruction_custom_function_ref_new (f),
		                   context);
	}
	else if (property)
	{
		ret = instructions_push (expression,
		                         cdn_instruction_variable_new (property),
		                         context);

		if (ret && dotted)
		{
			ret = wrap_dotted (expression,
			                   context,
			                   dotname,
			                   order);
		}
	}
	else
	{
		ret = FALSE;
	}

	g_free (dotname);
	g_free (nname);

	return ret;
}

static gboolean
parse_number (CdnExpression   *expression,
              CdnTokenNumber  *token,
              ParserContext  *context)
{
	CdnInstruction *instr;

	instr = cdn_instruction_number_new (token->value);
	cdn_instruction_number_set_representation (CDN_INSTRUCTION_NUMBER (instr),
	                                           token->parent.text);

	instructions_push (expression,
	                   instr,
	                   context);

	return TRUE;
}

static gboolean
parse_dot_token (CdnExpression *expression,
                 gchar const   *id,
                 ParserContext *context)
{
	gboolean ret = TRUE;
	CdnToken *next = cdn_tokenizer_next (context->buffer);
	gchar *cname;

	if (!CDN_TOKEN_IS_IDENTIFIER (next))
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected identifier for property");

		cdn_token_free (next);
		return FALSE;
	}

	cname = g_strdup (CDN_TOKEN_IDENTIFIER (next)->identifier);
	next = cdn_tokenizer_peek (*(context->buffer));

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_NODE_START)
	{
		cdn_token_free (cdn_tokenizer_next (context->buffer));
		ret = parse_function (expression, id, cname, context);
	}
	else
	{
		// Resolve property
		ret = parse_dot_variable (expression,
		                          id,
		                          cname,
		                          context);
	}

	cdn_token_free (next);
	g_free (cname);

	return ret;
}

static gboolean
parse_identifier (CdnExpression      *expression,
                  CdnTokenIdentifier *token,
                  ParserContext      *context)
{
	gchar *id = token->identifier;
	gboolean ret = FALSE;

	// consume token and peek the next to see if the identifier is a function
	// call
	cdn_token_free (cdn_tokenizer_next (context->buffer));
	CdnToken *next = cdn_tokenizer_peek (*context->buffer);

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
		CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_NODE_START)
	{
		// consume peeked group start
		cdn_token_free (cdn_tokenizer_next (context->buffer));
		ret = parse_function (expression, id, NULL, context);
	}
	else if (next && CDN_TOKEN_IS_OPERATOR (next) &&
		CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START)
	{
		ret = parse_custom_operator (expression, id, context);
	}
	else if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	         CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_DOT)
	{
		// consume peeked dot
		cdn_token_free (cdn_tokenizer_next (context->buffer));

		ret = parse_dot_token (expression, id, context);
	}
	else
	{
		ret = parse_identifier_as_variable (expression, id, context);
	}

	cdn_token_free (next);
	return ret;
}

static gboolean
parse_expression (CdnExpression   *expression,
                  ParserContext   *context,
                  gint             priority,
                  gint             left_assoc)
{
	// peek next token
	CdnToken *token;
	gboolean ret = FALSE;
	gint num = 0;

	cdn_tokenizer_ensure_skip_space (context->buffer);
	push_error_start (expression, context);

	while ((token = cdn_tokenizer_peek (*context->buffer)))
	{
		ret = TRUE;

		switch (token->type)
		{
			case CDN_TOKEN_TYPE_NUMBER:
				if (num != 0)
				{
					cdn_token_free (token);
					pop_error_start (expression,
					                 context);

					return TRUE;
				}

				// simply push a number on the stack
				ret = parse_number (expression,
				                    CDN_TOKEN_NUMBER (token),
				                    context);
			break;
			case CDN_TOKEN_TYPE_IDENTIFIER:
			{
				if (num != 0)
				{
					cdn_token_free (token);
					pop_error_start (expression,
					                 context);

					return TRUE;
				}

				ret = parse_identifier (expression,
				                        CDN_TOKEN_IDENTIFIER (token),
				                        context);

				cdn_token_free (token);
				token = NULL;
			}
			break;
			case CDN_TOKEN_TYPE_OPERATOR:
			{
				CdnTokenOperator *op = CDN_TOKEN_OPERATOR (token);

				// group end
				if (op->type == CDN_TOKEN_OPERATOR_TYPE_NODE_END ||
				    op->type == CDN_TOKEN_OPERATOR_TYPE_COMMA ||
				    op->type == CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE ||
				    op->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END ||
				    op->type == CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON)
				{
					cdn_token_free (token);

					pop_error_start (expression, context);
					return TRUE;
				}

				if (num == 0)
				{
					ret = parse_unary_operator (expression,
					                            token,
					                            context);

					if (ret)
					{
						cdn_token_free (token);
						token = NULL;
					}
				}
				else if (op->priority < priority ||
					 (op->priority == priority && left_assoc))
				{
					// Do not handle the operator here yet
					cdn_token_free (token);

					pop_error_start (expression, context);
					return TRUE;
				}
				else
				{
					ret = parse_operator (expression,
					                      token,
					                      context);

					if (ret)
					{
						cdn_token_free (token);
						token = NULL;
					}
				}
			}
			break;
			case CDN_TOKEN_TYPE_NONE:
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_TOKEN,
				               "Uknown token");
				ret = FALSE;
			break;
		}

		if (token)
		{
			// consume token
			if (ret)
			{
				cdn_token_free (cdn_tokenizer_next (context->buffer));
			}

			cdn_token_free (token);
		}

		++num;

		if (ret == FALSE)
		{
			break;
		}
	}

	if (!ret && context->error && !cdn_compile_error_get_error (context->error))
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected expression but got (nothing)");
	}

	if (ret)
	{
		pop_error_start (expression, context);
	}

	return ret;
}

static gint
calculate_stack_manipulation (CdnStackManipulation const *smanip,
                              gint                       *tmpspace,
                              gint                       *numpopped)
{
	gint ret;
	gint i;

	*numpopped = 0;

	for (i = 0; i < smanip->pop.num; ++i)
	{
		*numpopped += cdn_dimension_size (&smanip->pop.args[i].dimension);
	}

	ret = -*numpopped + cdn_dimension_size (&smanip->push.dimension);

	*tmpspace = smanip->extra_space;
	return ret;
}

//#define PRINT_STACK

static gboolean
validate_stack (CdnExpression *expression,
                ParserContext *context,
                gboolean       dimonly)
{
	GSList *item;
	gint stack = 0;
	gint maxstack = 1;
	CdnDimension dim;
	gint tmpspace = 0;

	if (!dimonly)
	{
		g_slist_free (expression->priv->depends_on);
		expression->priv->depends_on = NULL;

		// check for empty instruction set
		if (!expression->priv->instructions)
		{
			instructions_push (expression,
			                   cdn_instruction_number_new_from_string ("0"),
			                   NULL);
		}
	}

	dim = expression->priv->retdim.dimension;

#ifdef PRINT_STACK
	g_printf ("\n\nValidating stack for: %s\n", expression->priv->expression);
#endif

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CdnInstruction *inst = item->data;
		GSList *deps;
		GSList *dep;
		CdnStackManipulation const *smanip;
		GError *error = NULL;
		gint nst;
		gint numpopped;

		// Don't allow references to be on the instruction set
		if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst) ||
		    CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (inst))
		{
			gint start;
			gint end;

			cdn_instruction_get_location (inst, &start, &end);

			expression->priv->error_start =
				g_slist_prepend (expression->priv->error_start,
				                 GINT_TO_POINTER (start));

			expression->priv->error_at = end;

			if (context)
			{
				error = g_error_new (CDN_COMPILE_ERROR_TYPE,
				                     CDN_COMPILE_ERROR_INVALID_STACK,
				                     "Expression contains references instead of function calls");

				cdn_compile_error_set (context->error,
				                       error,
				                       NULL,
				                       NULL,
				                       NULL,
				                       expression);

				g_error_free (error);
			}

			return FALSE;
		}

		smanip = cdn_instruction_get_stack_manipulation (inst, &error);

		if (!smanip && context)
		{
			cdn_compile_error_set (context->error,
			                       error,
			                       NULL,
			                       NULL,
			                       NULL,
			                       expression);

			g_error_free (error);

			return FALSE;
		}
		else if (!smanip)
		{
			g_warning ("%s", error->message);
		}

#ifdef PRINT_STACK
		g_printf ("  %-20s (", cdn_instruction_to_string (inst));
#endif

		nst = calculate_stack_manipulation (smanip, &tmpspace, &numpopped);

#ifdef PRINT_STACK
		gint i;

		for (i = 0; i < smanip->pop.num; ++i)
		{
			if (i != 0)
			{
				g_print (", ");
			}

			g_printf ("%d-by-%d",
			           smanip->pop.args[i].rows,
			           smanip->pop.args[i].columns);
		}

		g_printf (") [%d-by-%d] →  ",
		           smanip->push.rows,
		           smanip->push.columns);

		g_printf ("%d (+%d)\n", stack, tmpspace + nst);
#endif

		if (expression->priv->pinned_sparsity)
		{
			expression->priv->retdim.dimension = smanip->push.dimension;
		}
		else
		{
			cdn_stack_arg_copy (&expression->priv->retdim, &smanip->push);
		}

		if (stack + nst <= 0 || numpopped > stack)
		{
			return FALSE;
		}

		if (!dimonly)
		{
			deps = cdn_instruction_get_dependencies (inst);

			for (dep = deps; dep; dep = g_slist_next (dep))
			{
				CdnExpression *other = dep->data;

				expression->priv->depends_on =
					g_slist_prepend (expression->priv->depends_on,
					                 other);

				other->priv->depends_on_me =
					g_slist_prepend (other->priv->depends_on_me,
					                 expression);
			}

			g_slist_free (deps);

			if (CDN_IS_INSTRUCTION_RAND (inst))
			{
				expression->priv->rand_instructions =
					g_slist_prepend (expression->priv->rand_instructions,
					                 inst);
			}
		}

		if (stack + MAX(tmpspace, nst) > maxstack)
		{
			maxstack = stack + MAX(tmpspace, nst);
		}

		stack += nst;
	}

	cdn_stack_resize (&(expression->priv->output), maxstack);
	cdn_stack_reset (&(expression->priv->output));

	if (dimonly &&
	    cdn_dimension_equal (&dim, &expression->priv->retdim.dimension))
	{
		// Do nothing
		return TRUE;
	}

	if (stack != cdn_dimension_size (&expression->priv->retdim.dimension))
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
empty_expression (CdnExpression *expression)
{
	gchar const *buffer = expression->priv->expression;

	while (buffer && *buffer)
	{
		if (!g_ascii_isspace (*buffer))
		{
			return FALSE;
		}

		++buffer;
	}

	return TRUE;
}

/**
 * cdn_expression_compile:
 * @expression: a #CdnExpression
 * @context: the evaluation context
 * @error: a #GError
 *
 * Compile the expression. The context is a list of #CdnObject from which
 * properties can be looked up (such as global constants, or from/to objects).
 * If there were any errors during compilation, @error will be set accordingly
 *
 * Returns: %TRUE if the expression compiled successfully, %FALSE otherwise
 *
 **/
gboolean
cdn_expression_compile (CdnExpression     *expression,
                        CdnCompileContext *context,
                        CdnCompileError   *error)
{
	CdnDimension olddim;
	gboolean wasmod;
	gboolean dimschanged;
	gchar *buffer = expression->priv->expression;
	ParserContext ctx = {(gchar const **)&buffer, context, error};
	gboolean ret;

	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), FALSE);

	if (!expression->priv->modified)
	{
		return TRUE;
	}

	if (expression->priv->compiling)
	{
		parser_failed (expression,
		               &ctx,
		               CDN_COMPILE_ERROR_VARIABLE_RECURSE,
		               "Infinite recursion in variable expression");

		return FALSE;
	}

	expression->priv->compiling = TRUE;

	cdn_expression_get_dimension (expression, &olddim);
	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));

	expression->priv->error_at = 0;
	expression->priv->error_start = NULL;

	if (empty_expression (expression))
	{
		instructions_push (expression,
		                   cdn_instruction_number_new (0.0),
		                   NULL);
		ret = TRUE;
	}
	else
	{
		ret = parse_expression (expression, &ctx, -1, 0);

		if (!ret)
		{
			expression->priv->error_at = *(ctx.buffer) - expression->priv->expression + 1;
		}
	}

	if (!ret)
	{
		instructions_free (expression);
		expression->priv->compiling = FALSE;

		return FALSE;
	}
	else
	{
		// reverse instructions
		expression->priv->instructions =
			g_slist_reverse (expression->priv->instructions);

		// Validate the stack here
		if (!validate_stack (expression, &ctx, FALSE))
		{
			instructions_free (expression);
			expression->priv->compiling = FALSE;

			return parser_failed (expression,
			                      &ctx,
			                      CDN_COMPILE_ERROR_INVALID_STACK,
			                      "Invalid stack produced. This usually indicates a problem in the parser");
		}
	}

	wasmod = expression->priv->modified;

	if (wasmod)
	{
		expression->priv->modified = FALSE;
	}

	dimschanged = !cdn_dimension_equal (&olddim,
	                                    &expression->priv->retdim.dimension);

	if (!reset_cache (expression, dimschanged))
	{
		// If not, at least reset the depending cache
		reset_depending_cache (expression, dimschanged);
	}

	if (wasmod)
	{
		// This should also trigger recompilation of things
		// that depend on this (like custom operators)
		g_object_notify (G_OBJECT (expression), "modified");
	}

	expression->priv->compiling = FALSE;
	return TRUE;
}

/**
 * cdn_expression_set_instructions:
 * @expression: A #CdnExpression
 * @instructions: (element-type CdnInstruction) (transfer full): A #GSList of #CdnInstruction
 *
 * Set the instructions used to evaluate the expression. You should never have
 * to use this function. It's main purpose is for optimization of expressions
 * in cdnrawc.
 *
 * Returns: %TRUE if the new instruction set is valid, %FALSE otherwise
 *
 **/
void
cdn_expression_set_instructions (CdnExpression *expression,
                                 GSList const  *instructions)
{
	GSList *copy = NULL;

	while (instructions)
	{
		copy = g_slist_prepend (copy,
		                        cdn_mini_object_copy (instructions->data));

		instructions = g_slist_next (instructions);
	}

	copy = g_slist_reverse (copy);

	cdn_expression_set_instructions_take (expression, copy);

	g_slist_foreach (copy, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (copy);
}

/**
 * cdn_expression_set_instructions_take:
 * @expression: a #CdnExpression.
 * @instructions: (element-type CdnInstruction): a #GSList.
 *
 * Set the instructions used to evaluate the expression. You should never have
 * to use this function. It's main purpose is for optimization of expressions
 * in cdnrawc. This method differs from set_instructions in that it simply
 * refs the instructions whereas #cdn_expression_set_instructions copies
 * the instructions.
 *
 **/
void
cdn_expression_set_instructions_take (CdnExpression *expression,
                                      GSList        *instructions)
{
	CdnDimension olddim;
	gboolean dimschanged;

	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	if (expression->priv->instructions == instructions)
	{
		return;
	}

	cdn_expression_get_dimension (expression, &olddim);

	instructions_free (expression);
	cdn_stack_destroy (&(expression->priv->output));

	expression->priv->instructions = g_slist_copy (instructions);
	g_slist_foreach (expression->priv->instructions, (GFunc)cdn_mini_object_ref, NULL);

	// Validate the stack here
	validate_stack (expression, NULL, FALSE);

	cdn_expression_recalculate_sparsity (expression);

	// We are going to reset the expression completely here
	expression->priv->prevent_cache_reset = FALSE;

	dimschanged = !cdn_dimension_equal (&olddim, &expression->priv->retdim.dimension);

	reset_cache (expression, dimschanged);

	expression->priv->prevent_cache_reset = expression->priv->once;

	if (expression->priv->modified)
	{
		expression->priv->modified = FALSE;
		g_object_notify (G_OBJECT (expression), "modified");
	}
}

/**
 * cdn_expression_set_value:
 * @expression: a #CdnExpression
 * @value: a value
 *
 * Sets the cached/instant value of an expression. If the expression is
 * reset, this value will no longer be used and the expression will be
 * evaluated as normal
 *
 **/
void
cdn_expression_set_value (CdnExpression  *expression,
                          gdouble         value)
{
	set_values (expression, &value, cdn_dimension_onep);
	expression->priv->prevent_cache_reset = TRUE;
}

/**
 * cdn_expression_set_values:
 * @expression: A #CdnExpression
 * @values: the new values
 *
 * Set expression value.
 *
 **/
void
cdn_expression_set_values (CdnExpression       *expression,
                           CdnMatrix     const *values)
{
	set_values (expression, cdn_matrix_get (values), &values->dimension);
	expression->priv->prevent_cache_reset = TRUE;
}

/**
 * cdn_expression_evaluate:
 * @expression: a #CdnExpression
 *
 * Get the result of evaluating the expression. If the expression is not yet
 * compiled, 0.0 is returned. The result of the evaluation is cached in
 * the expression. Make sure to call cdn_expression_reset_cache to clear the
 * cache if needed
 *
 * Returns: the result of evaluating the expression
 *
 **/
gdouble
cdn_expression_evaluate (CdnExpression *expression)
{
	CdnMatrix const *ret;

	ret = cdn_expression_evaluate_values (expression);
	return ret ? *cdn_matrix_get (ret) : 0;
}

static void
set_cache_from_stack (CdnExpression *expression)
{
	set_values (expression,
	            cdn_stack_ptr (&(expression->priv->output)),
	            &expression->priv->retdim.dimension);
}

/**
 * cdn_expression_evaluate_values:
 * @expression: a #CdnExpression
 *
 * Returns: the result of evaluating the expression
 *
 **/
CdnMatrix const *
cdn_expression_evaluate_values (CdnExpression *expression)
{
	/* Omit type check to increase speed */
	if (!expression)
	{
		return NULL;
	}

	if (expression->priv->cached)
	{
		return &expression->priv->cached_output;
	}

	if (expression->priv->evaluate_notify)
	{
		expression->priv->evaluate_notify (expression,
		                                   expression->priv->evaluate_userdata);

		if (expression->priv->cached)
		{
			return &expression->priv->cached_output;
		}
	}

	GSList *item;
	CdnStack *stack = &(expression->priv->output);

	cdn_stack_reset (stack);

	if (expression->priv->output.size == 0)
	{
		g_warning ("Stack size should not be 0 (%s)!",
		           expression->priv->expression);

		return NULL;
	}

	if (!expression->priv->instructions)
	{
		g_warning ("No instructions found, maybe the expression was not parsed? (%s)",
		           expression->priv->expression);

		return NULL;
	}

	if (cdn_debug_is_enabled (CDN_DEBUG_MATH))
	{
		CdnExpressionTreeIter *iter;

		iter = cdn_expression_tree_iter_new (expression);

		cdn_debug_message (DEBUG_MATH,
		                   "Evaluating: %p: %s",
		                   expression,
		                   cdn_expression_tree_iter_to_string (iter));

		cdn_debug_push_indent ();

		cdn_expression_tree_iter_free (iter);
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		cdn_instruction_execute (item->data, stack);
	}

	if (cdn_debug_is_enabled (CDN_DEBUG_MATH))
	{
		cdn_debug_pop_indent ();
	}

	if (cdn_stack_count (&(expression->priv->output)) !=
	    cdn_dimension_size (&expression->priv->retdim.dimension))
	{
		g_warning ("Invalid output stack after evaluating: `%s' (expected %d but got %d)",
		           expression->priv->expression,
		           cdn_dimension_size (&expression->priv->retdim.dimension),
		           cdn_stack_count (&(expression->priv->output)));

		return NULL;
	}

	set_cache_from_stack (expression);
	expression->priv->cached = expression->priv->has_cache;

	if (cdn_debug_is_enabled (CDN_DEBUG_MATH))
	{
		CdnMatrix const *mat;
		gdouble const *vals;

		mat = &expression->priv->cached_output;
		vals = cdn_matrix_get (mat);

		if (cdn_dimension_is_one (&mat->dimension))
		{
			cdn_debug_message (DEBUG_MATH, "Evaluated: %g", *vals);
		}
		else
		{
			gint i;
			GString *msg;

			msg = g_string_new ("Evaluated: [");

			for (i = 0; i < cdn_matrix_size (mat); ++i)
			{
				if (i != 0)
				{
					g_string_append (msg, ", ");
				}

				g_string_append_printf (msg, "%g", vals[i]);
			}

			cdn_debug_message (DEBUG_MATH, "%s]", msg->str);
			g_string_free (msg, TRUE);
		}
	}

	return &expression->priv->cached_output;
}

static gboolean
reset_cache (CdnExpression *expression,
             gboolean       dimschanged)
{
	if (expression->priv->prevent_cache_reset)
	{
		return FALSE;
	}

	/* Omit type check to increase speed */
	if (expression->priv->cached || dimschanged)
	{
		// Disable the cache, next evaluate will recalculate the
		// expression
		expression->priv->cached = FALSE;

		// Reset the caches of all the expressions that depend on this
		// expression
		reset_depending_cache (expression, dimschanged);
		return TRUE;
	}

	return FALSE;
}

/**
 * cdn_expression_reset_cache:
 * @expression: a #CdnExpression
 *
 * Resets the possibly cached result of the value
 *
 **/
void
cdn_expression_reset_cache (CdnExpression *expression)
{
	reset_cache (expression, FALSE);
}

void
_cdn_expression_reset_rand_cache (CdnExpression *expression)
{
	expression->priv->cached = TRUE;
	reset_cache (expression, FALSE);
}

void
cdn_expression_force_reset_cache (CdnExpression *expression)
{
	gboolean prevent;

	prevent = expression->priv->prevent_cache_reset;
	expression->priv->prevent_cache_reset = FALSE;

	reset_cache (expression, FALSE);

	expression->priv->prevent_cache_reset = prevent;
}

static gboolean
expression_depends_on (CdnExpression *expression,
                       CdnExpression *depends_on,
                       gboolean       checkme)
{
	GSList *item;

	if (checkme && expression == depends_on)
	{
		return TRUE;
	}

	for (item = expression->priv->depends_on; item; item = g_slist_next (item))
	{
		if (expression_depends_on (item->data, depends_on, TRUE))
		{
			return TRUE;
		}
	}

	return FALSE;
}

gboolean
cdn_expression_depends_on (CdnExpression *expression,
                           CdnExpression *depends_on)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);

	return expression_depends_on (expression, depends_on, FALSE);
}

/**
 * cdn_expression_get_dependencies:
 * @expression: a #CdnExpression
 *
 * Get a list of #CdnExpression on which the expression depends. The list is owned
 * by @expression and should not be freed or modified
 *
 * Returns: (element-type CdnExpression) (transfer none): a list of #CdnExpression
 *
 **/
const GSList *
cdn_expression_get_dependencies (CdnExpression *expression)
{
	/* Omit type check to increase speed */
	return expression->priv->depends_on;
}

/**
 * cdn_expression_get_variable_dependencies:
 * @expression: A #CdnExpression
 *
 * Get a list of variables on which the expression depends directly.
 *
 * Returns: (element-type CdnVariable) (transfer container): a #GSList
 *
 **/
GSList *
cdn_expression_get_variable_dependencies (CdnExpression *expression)
{
	GSList *item;
	GSList *ret = NULL;

	/* Omit type check to increase speed */
	for (item = expression->priv->instructions; item; item = g_slist_next (item))
	{
		CdnInstruction *instr = item->data;

		if (CDN_IS_INSTRUCTION_VARIABLE (instr))
		{
			CdnVariable *v;

			v = cdn_instruction_variable_get_variable (item->data);

			ret = g_slist_prepend (ret, v);
		}
	}

	return g_slist_reverse (ret);
}


/**
 * cdn_expression_get_depends_on_me:
 * @expression: a #CdnExpression
 *
 * Get a list of #CdnExpression which depend on @expression. The list is owned
 * by @expression and should not be freed or modified
 *
 * Returns: (element-type CdnExpression) (transfer none): a list of #CdnExpression
 *
 **/
const GSList *
cdn_expression_get_depends_on_me (CdnExpression *expression)
{
	/* Omit type check to increase speed */
	return expression->priv->depends_on_me;
}

/**
 * cdn_expression_reset:
 * @expression: a #CdnExpression
 *
 * Resets the expression
 *
 **/
void
cdn_expression_reset (CdnExpression *expression)
{
	GSList *item;

	/* Omit type check to increase speed */
	expression->priv->prevent_cache_reset = FALSE;

	// Reset the cache to go back to original settings
	reset_cache (expression,
	             expression->priv->cached &&
	             !cdn_dimension_equal (&expression->priv->retdim.dimension,
	                                   &expression->priv->cached_output.dimension));

	if (expression->priv->once)
	{
		expression->priv->prevent_cache_reset = TRUE;
	}

	item = expression->priv->instructions;

	while (item)
	{
		if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (item->data))
		{
			CdnOperator *op;

			op = cdn_instruction_custom_operator_get_operator (item->data);

			if (op)
			{
				cdn_operator_reset (op);
			}
		}

		item = g_slist_next (item);
	}
}

/**
 * cdn_expression_get_instructions:
 * @expression: a #CdnExpression
 *
 * Get list of #CdnInstruction. The list is owned by @expression and should
 * not be freed or modified
 *
 * Returns: (element-type CdnInstructionBoxed) (transfer none): list of #CdnInstruction
 *
 **/
const GSList *
cdn_expression_get_instructions (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	return expression->priv->instructions;
}

/**
 * cdn_expression_get_rand_instructions:
 * @expression: A #CdnExpression
 *
 * Get the random instructions in this expression.
 *
 * Returns: (element-type CdnInstructionRand) (transfer none): A #GSList of #CdnInstructionRand
 *
 **/
GSList const *
cdn_expression_get_rand_instructions (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	return expression->priv->rand_instructions;
}

/**
 * cdn_expression_equal:
 * @expression: a #CdnExpression
 * @other: a #CdnExpression
 * @asstring: whether the expression is equal in string representation
 *
 * Get whether two expressions are equal. If the expressions are compiled, they
 * are evaluated for equality by means of their instructions. Otherwise the
 * comparison is done on their string representations
 *
 * Returns: %TRUE if the expressions are equal, %FALSE otherwise
 *
 **/
gboolean
cdn_expression_equal (CdnExpression *expression,
                      CdnExpression *other,
                      gboolean       asstring)
{
	if ((expression == NULL) != (other == NULL))
	{
		return FALSE;
	}

	if (expression == NULL && other == NULL)
	{
		return TRUE;
	}

	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (CDN_IS_EXPRESSION (other), FALSE);

	if (!expression->priv->instructions || !other->priv->instructions)
	{
		return g_strcmp0 (expression->priv->expression,
		                  other->priv->expression) == 0;
	}

	if (g_slist_length (expression->priv->instructions) !=
	    g_slist_length (other->priv->instructions))
	{
		return FALSE;
	}

	// Compare instructions
	GSList *e1 = expression->priv->instructions;
	GSList *e2 = other->priv->instructions;

	while (e1)
	{
		if (!cdn_instruction_equal (e1->data, e2->data, asstring))
		{
			return FALSE;
		}

		e1 = g_slist_next (e1);
		e2 = g_slist_next (e2);
	}

	return TRUE;
}

/**
 * cdn_expression_get_once:
 * @expression: A #CdnExpression
 *
 * Get whether the expression is only evaluated once.
 *
 * Returns: %TRUE if the expression is only evaluated once, %FALSE otherwise.
 *
 **/
gboolean
cdn_expression_get_once (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);

	return expression->priv->once;
}

/**
 * cdn_expression_set_once:
 * @expression: A #CdnExpression
 * @instant: Whether the expression should be constant
 *
 * When an expression is "once", its value will not change.
 *
 **/
void
cdn_expression_set_once (CdnExpression *expression,
                         gboolean       once)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	expression->priv->once = once;
	expression->priv->prevent_cache_reset |= once;
}

/**
 * cdn_expression_copy:
 * @expression: A #CdnExpression
 *
 * Create a copy of a #CdnExpression.
 *
 * Returns: (transfer full): A #CdnExpression
 *
 **/
CdnExpression *
cdn_expression_copy (CdnExpression *expression)
{
	CdnExpression *ret;
	GSList *instr;

	g_return_val_if_fail (expression == NULL || CDN_IS_EXPRESSION (expression), NULL);

	if (expression == NULL)
	{
		return NULL;
	}

	ret = cdn_expression_new (expression->priv->expression);

	ret->priv->cached = expression->priv->cached;
	ret->priv->prevent_cache_reset = expression->priv->prevent_cache_reset;

	cdn_stack_arg_copy (&ret->priv->retdim, &expression->priv->retdim);

	cdn_stack_resize (&ret->priv->output, expression->priv->output.size);
	cdn_stack_reset (&ret->priv->output);

	cdn_matrix_copy (&ret->priv->cached_output,
	                 &expression->priv->cached_output);

	ret->priv->modified = expression->priv->modified;
	ret->priv->has_cache = expression->priv->has_cache;
	ret->priv->once = expression->priv->once;
	ret->priv->pinned_sparsity = expression->priv->pinned_sparsity;

	instr = expression->priv->instructions;

	while (instr)
	{
		instructions_push (ret,
		                   cdn_mini_object_copy (instr->data),
		                   NULL);

		instr = g_slist_next (instr);
	}

	ret->priv->instructions =
		g_slist_reverse (ret->priv->instructions);

	validate_stack (ret, NULL, FALSE);
	return ret;
}

/**
 * cdn_expression_get_error_at:
 * @expression: A #CdnExpression
 *
 * Get the character position in the expression at which an error occurred
 * while compiling the expression
 *
 * Returns: the character position at which an error occurred
 *
 **/
gint
cdn_expression_get_error_at (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), 0);

	return expression->priv->error_at;
}

/**
 * cdn_expression_get_error_start:
 * @expression: A #CdnExpression
 *
 * Get the character position in the expression at which an error started
 * while compiling the expression
 *
 * Returns: the character position at which an error started
 *
 **/
gint
cdn_expression_get_error_start (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), 0);

	return expression->priv->error_start ? GPOINTER_TO_INT (expression->priv->error_start->data) : expression->priv->error_at;
}

gboolean
cdn_expression_get_has_cache (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), TRUE);

	return expression->priv->has_cache;
}

void
cdn_expression_set_has_cache (CdnExpression *expression,
                              gboolean       cache)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	set_has_cache (expression, cache);
}

/**
 * cdn_expression_get_dimension:
 * @expression: a #CdnExpression.
 * @dimension: (out): return value for the expression dimension.
 *
 * Get the dimension of the expression.
 *
 * Returns: %TRUE if the expression has a known dimension, %FALSE otherwise.
 *
 **/
gboolean
cdn_expression_get_dimension (CdnExpression *expression,
                              CdnDimension  *dimension)
{
	if (expression->priv->cached)
	{
		*dimension = expression->priv->cached_output.dimension;
		return TRUE;
	}

	*dimension = expression->priv->retdim.dimension;
	return !expression->priv->modified;
}

void
cdn_expression_set_cache_notify (CdnExpression            *expression,
                                 CdnExpressionCacheNotify  notify,
                                 gpointer                  userdata,
                                 GDestroyNotify            destroy_notify)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	if (expression->priv->cache_destroy_notify)
	{
		expression->priv->cache_destroy_notify (expression->priv->cache_userdata);
	}

	expression->priv->cache_destroy_notify = destroy_notify;
	expression->priv->cache_userdata = userdata;
	expression->priv->cache_notify = notify;
}

void
cdn_expression_set_evaluate_notify (CdnExpression               *expression,
                                    CdnExpressionEvaluateNotify  notify,
                                    gpointer                     userdata,
                                    GDestroyNotify               destroy_notify)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	if (expression->priv->evaluate_destroy_notify)
	{
		expression->priv->evaluate_destroy_notify (expression->priv->evaluate_userdata);
	}

	expression->priv->evaluate_destroy_notify = destroy_notify;
	expression->priv->evaluate_userdata = userdata;
	expression->priv->evaluate_notify = notify;
}

/**
 * cdn_expression_get_stack_size:
 * @expression: a #CdnExpression.
 *
 * Get the size of the stack needed to evaluate the expression.
 *
 * Returns: the required stack size to evaluate @expression.
 *
 **/
guint
cdn_expression_get_stack_size (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), 0);

	return cdn_stack_size (&expression->priv->output);
}

gboolean
cdn_expression_is_cached (CdnExpression *expression)
{
	return expression->priv->cached;
}

/**
 * cdn_expression_get_stack_arg:
 * @expression: a #CdnExpression.
 *
 * Get the stack return argument of the expression.
 *
 * Returns: (transfer none): a #CdnStackArg.
 *
 **/
CdnStackArg *
cdn_expression_get_stack_arg (CdnExpression *expression)
{
	return &expression->priv->retdim;
}

typedef struct
{
	guint *sparsity;
	guint num_sparse;
} SparsityInfo;

void
cdn_expression_set_pinned_sparsity (CdnExpression *expression,
                                    gboolean       pinned)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	expression->priv->pinned_sparsity = pinned;
}

gboolean
cdn_expression_get_pinned_sparsity (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);

	return expression->priv->pinned_sparsity;
}

void
cdn_expression_recalculate_sparsity (CdnExpression *expression)
{
	GQueue q;
	GSList const *instr;
	SparsityInfo *info;

	if (!expression || expression->priv->pinned_sparsity)
	{
		return;
	}

	g_queue_init (&q);

	for (instr = expression->priv->instructions; instr; instr = g_slist_next (instr))
	{
		CdnInstruction *i = instr->data;
		CdnStackManipulation const *smanip;

		smanip = cdn_instruction_get_stack_manipulation (i, NULL);

		if (smanip->pop.num > 0)
		{
			guint idx;

			for (idx = 0; idx < smanip->pop.num; ++idx)
			{
				info = g_queue_pop_head (&q);

				cdn_stack_arg_set_sparsity (&smanip->pop.args[idx],
				                            info->sparsity,
				                            info->num_sparse);

				g_slice_free (SparsityInfo, info);
			}
		}

		cdn_instruction_recalculate_sparsity (i);

		info = g_slice_new (SparsityInfo);

		info->sparsity = smanip->push.sparsity;
		info->num_sparse = smanip->push.num_sparse;

		g_queue_push_head (&q, info);
	}

	info = g_queue_pop_head (&q);

	cdn_stack_arg_set_sparsity (&expression->priv->retdim,
	                            info->sparsity,
	                            info->num_sparse);

	g_slice_free (SparsityInfo, info);
}

void
_cdn_expression_transfer_dependencies (CdnExpression *expression,
                                       CdnExpression *transfer_to)
{
	GSList *item;

	g_return_if_fail (CDN_IS_EXPRESSION (expression));
	g_return_if_fail (transfer_to == NULL || CDN_IS_EXPRESSION (transfer_to));

	// Transfer dependencies from other expressions on @expression to
	// dependencies on @transfer_to
	for (item = expression->priv->depends_on_me; item; item = g_slist_next (item))
	{
		CdnExpression *other;
		GSList *found;

		other = item->data;
		found = g_slist_find (other->priv->depends_on, expression);

		if (found)
		{
			if (transfer_to)
			{
				// Replace the dependency with transfer_to
				found->data = transfer_to;

				// Also add the dependencies back on transfer_to
				if (!g_slist_find (transfer_to->priv->depends_on_me, other))
				{
					transfer_to->priv->depends_on_me =
						g_slist_prepend (transfer_to->priv->depends_on_me,
						                 other);
				}
			}
			else
			{
				other->priv->depends_on =
					g_slist_remove_link (other->priv->depends_on,
					                     found);
			}
		}
	}
}
