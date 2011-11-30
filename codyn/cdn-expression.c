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

#include <codyn/instructions/cdn-instructions.h>

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>

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

	GSList *depends_on;
	GSList *depends_on_me;

	gdouble cached_output;
	gint error_at;
	GSList *error_start;

	guint cached : 1;
	guint prevent_cache_reset : 1;
	guint modified : 1;
	guint once : 1;
	guint has_cache : 1;
};

typedef struct
{
	gchar const **buffer;
	CdnCompileContext *context;

	CdnCompileError *error;
	GSList *stack;
} ParserContext;

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

		cdn_mini_object_free (expression->priv->instructions->data);

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
                gchar const   *value)
{
	g_free (expression->priv->expression);

	expression->priv->expression = normalize_expression (value);
	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));

	g_object_notify (G_OBJECT (expression), "expression");
}

static void
reset_depending_cache (CdnExpression *expression)
{
	GSList *item;

	for (item = expression->priv->depends_on_me; item; item = g_slist_next (item))
	{
		cdn_expression_reset_cache (item->data);
	}
}

static void
set_value (CdnExpression *expression,
           gdouble        value)
{
	expression->priv->cached = TRUE;
	expression->priv->prevent_cache_reset = TRUE;

	expression->priv->cached_output = value;

	if (expression->priv->has_cache)
	{
		// Reset the cache of any expression that depends on this expression
		reset_depending_cache (expression);
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
		expression->priv->cached = FALSE;
		reset_depending_cache (expression);
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
			set_expression (self, g_value_get_string (value));
		break;
		case PROP_VALUE:
			set_value (self, g_value_get_double (value));
		break;
		case PROP_HAS_CACHE:
			set_has_cache (self, g_value_get_boolean (value));
		break;
		case PROP_MODIFIED:
			self->priv->modified = self->priv->modified;
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
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

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
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");
}

static void
cdn_expression_init (CdnExpression *self)
{
	self->priv = CDN_EXPRESSION_GET_PRIVATE (self);

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

	set_expression (expression, value);
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
	return g_object_new (CDN_TYPE_EXPRESSION,
	                     "expression", expression,
	                     NULL);
}

CdnExpression *
cdn_expression_new0 ()
{
	CdnExpression *ret;

	ret = cdn_expression_new ("0");
	ret->priv->instructions = g_slist_prepend (NULL,
	                                           cdn_instruction_number_new_from_string ("0"));

	return ret;
}

static void
instructions_push (CdnExpression  *expression,
                   CdnInstruction *next,
                   ParserContext  *context)
{
	expression->priv->instructions = g_slist_prepend (expression->priv->instructions,
	                                                  next);

	if (context)
	{
		gint cnt = cdn_instruction_get_stack_count (next);
		gint consume = -cnt + 1;
		GSList *ret = NULL;

		ret = g_slist_prepend (NULL, next);

		while (consume > 0)
		{
			ret = g_slist_concat (context->stack->data, ret);

			context->stack = g_slist_delete_link (context->stack,
			                                      context->stack);

			--consume;
		}

		context->stack = g_slist_prepend (context->stack,
		                                  ret);
	}
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

		cdn_compile_error_set (context->error,
		                       err,
		                       NULL,
		                       NULL,
		                       NULL,
		                       expression);
	}

	return FALSE;
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

	if (strcmp (id, "from") == 0)
	{
		property = cdn_object_get_variable (CDN_OBJECT (cdn_edge_get_from (link)),
		                                    propname);

		binding = CDN_INSTRUCTION_VARIABLE_BINDING_FROM;
	}
	else if (strcmp (id, "to") == 0)
	{
		property = cdn_object_get_variable (CDN_OBJECT (cdn_edge_get_to (link)),
		                                    propname);

		binding = CDN_INSTRUCTION_VARIABLE_BINDING_TO;
	}

	if (!property)
	{
		return FALSE;
	}

	instructions_push (expression,
	                   cdn_instruction_variable_new_with_binding (property,
	                                                              binding),
	                   context);

	return TRUE;
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
		instructions_push (expression,
		                   cdn_instruction_variable_new (prop),
		                   context);

		return TRUE;
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
				instructions_push (expression,
				                   cdn_instruction_variable_new (prop),
				                   context);

				return TRUE;
			}
		}

		if (CDN_IS_NODE (o))
		{
			CdnObject *c;

			c = cdn_node_get_child (CDN_NODE (o), id);

			if (c)
			{
				CdnVariable *prop;

				prop = cdn_object_get_variable (c, propid);

				if (prop)
				{
					instructions_push (expression,
					                   cdn_instruction_variable_new (prop),
					                   context);

					return TRUE;
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
parse_dot_property (CdnExpression *expression,
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
	gint n_optional = 0;
	gint n_implicit = 0;
	gboolean ret = TRUE;
	gboolean isrand = FALSE;

	if (!cname)
	{
		function = cdn_compile_context_lookup_function (context->context, name);
	}
	else
	{
		function = parse_context_function (expression, name, cname, context);
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

				if (!fid)
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
			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND,
			                      "Function %s.%s could not be found",
			                      name, cname);
		}
	}
	else
	{
		n_optional = (gint)cdn_function_get_n_optional (function);
		n_implicit = (gint)cdn_function_get_n_implicit (function);
		arguments = (gint)cdn_function_get_n_arguments (function);
	}

	// parse arguments
	gint numargs = 0;

	if (!parse_function_arguments (expression, context, &numargs))
	{
		return FALSE;
	}

	if ((function != NULL && (numargs > (arguments - n_implicit) || numargs < (arguments - n_optional - n_implicit))) ||
	    (function == NULL && arguments != -1 && numargs != arguments))
	{
		return parser_failed (expression,
		                      context,
		                      CDN_COMPILE_ERROR_MAXARG,
		                      "Expected number of arguments (%d|%d) for function `%s' does not match (got %d)",
		                      arguments - n_implicit - n_optional,
		                      n_optional,
		                      name,
		                      numargs);
	}

	/* Now lookup implicit arguments */
	if (function)
	{
		GList const *ar;
		GList *start;

		ar = cdn_function_get_arguments (function);

		/* Set defaults for the rest of the optional arguments on the stack */
		if (numargs < arguments - n_implicit)
		{
			start = g_list_nth ((GList *)ar, numargs);

			while (start)
			{
				CdnFunctionArgument *a;
				CdnExpression *expr;
				GSList *inst;

				a = start->data;

				if (!cdn_function_argument_get_optional (a))
				{
					break;
				}

				/* Inline the expression here */
				expr = cdn_function_argument_get_default_value (a);

				/* TODO: actually, the context is really not
				   correct here */
				if (!cdn_expression_compile (expr,
				                             context->context,
				                             context->error))
				{
					return FALSE;
				}

				for (inst = expr->priv->instructions; inst; inst = g_slist_next (inst))
				{
					instructions_push (expression,
					                   CDN_INSTRUCTION (cdn_mini_object_copy (inst->data)),
					                   context);
				}

				start = g_list_next (start);
				++numargs;
			}
		}

		start = g_list_nth ((GList *)ar, arguments - n_implicit);

		while (start)
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

				if (parse_dot_property (expression, id, ptr + 1, context))
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

			instructions_push (expression,
			                   cdn_instruction_variable_new (prop),
			                   context);
		}
	}

	CdnInstruction *instruction;

	if (function == NULL)
	{
		if (isrand)
		{
			instruction = cdn_instruction_rand_new (numargs);
		}
		else
		{
			instruction = cdn_instruction_function_new (fid,
			                                            name,
			                                            numargs);
		}
	}
	else
	{
		instruction = cdn_instruction_custom_function_new (function,
		                                                   numargs);

		ret = recurse_compile (expression, context, function);
	}

	instructions_push (expression, instruction, context);
	return ret;
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

	klass = cdn_operators_find_class (name);

	if (klass == NULL)
	{
		return parser_failed (expression,
		                      context,
		                      CDN_COMPILE_ERROR_OPERATOR_NOT_FOUND,
		                      "Custom operator %s could not be found",
		                      name);
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
		_cdn_expression_set_instructions_take (sub, newinst);

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

	op = cdn_operators_instantiate (name,
	                                (GSList const **)exprs,
	                                num_exprs,
	                                (GSList const **)inds,
	                                num_inds,
	                                num_arguments,
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

	if (!isref)
	{
		CdnFunction *func;

		func = cdn_operator_get_primary_function (op);

		// We need to add implicit arguments and optional arguments
		// on the stack here
		if (func)
		{
			GList *start;
			gint extra = 0;

			start = (GList *)cdn_function_get_arguments (func);

			start = g_list_nth ((GList *)start,
			                   num_arguments);

			while (start)
			{
				CdnFunctionArgument *a;
				CdnExpression *expr;
				GSList *inst;

				a = start->data;
				start = g_list_next (start);

				if (!cdn_function_argument_get_explicit (a))
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

						if (parse_dot_property (expression, id, ptr + 1, context))
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

					instructions_push (expression,
					                   cdn_instruction_variable_new (prop),
					                   context);
				}
				else if (cdn_function_argument_get_optional (a))
				{
					/* Inline the expression here */
					expr = cdn_function_argument_get_default_value (a);
					++extra;

					/* TODO: actually, the context is really not
					   correct here */
					if (!cdn_expression_compile (expr,
					                             context->context,
					                             context->error))
					{
						return FALSE;
					}

					for (inst = expr->priv->instructions; inst; inst = g_slist_next (inst))
					{
						instructions_push (expression,
						                   CDN_INSTRUCTION (cdn_mini_object_copy (inst->data)),
						                   context);
					}
				}
			}

			_cdn_operator_set_num_arguments (op,
			                                 cdn_operator_get_num_arguments (op) + extra);
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

	instructions_push (expression,
	                   cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_TERNARY),
	                                                 "?:",
	                                                 3),
	                   context);

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

	// handle group
	if (op->type == CDN_TOKEN_OPERATOR_TYPE_NODE_START)
	{
		cdn_token_free (cdn_tokenizer_next (context->buffer));
		return parse_group (expression, context);
	}

	switch (op->type)
	{
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_UNARY_MINUS), "-", 1);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
		break;
		case CDN_TOKEN_OPERATOR_TYPE_NEGATE:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_NEGATE), "!", 1);
		break;
		default:
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected unary operator (-, +, !) but got `%s'",
			               op->parent.text);
			ret = FALSE;
		break;
	}

	if (ret)
	{
		// consume token
		cdn_token_free (cdn_tokenizer_next (context->buffer));
		ret = parse_expression (expression, context, 1000, 1);
	}

	if (ret && inst)
	{
		if (op->type == CDN_TOKEN_OPERATOR_TYPE_MINUS &&
		    CDN_IS_INSTRUCTION_NUMBER (expression->priv->instructions->data))
		{
			cdn_mini_object_free (CDN_MINI_OBJECT (inst));
			inst = instructions_pop (expression);

			cdn_instruction_number_set_value (CDN_INSTRUCTION_NUMBER (inst),
			                                  cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (inst)) * -1);

			g_slist_free (context->stack->data);

			context->stack = g_slist_delete_link (context->stack,
			                                      context->stack);
		}

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
	_cdn_expression_set_instructions_take (expr, instr);

	exprs = g_slist_prepend (NULL, expr);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	op = cdn_operators_instantiate ("df_dt",
	                                (GSList const **)&exprs,
	                                1,
	                                NULL,
	                                0,
	                                0,
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

	instructions_push (expression,
	                   cdn_instruction_custom_operator_new (op),
	                   context);

	g_object_unref (op);

	return TRUE;
}

static gboolean
parse_operator (CdnExpression *expression,
                CdnToken      *token,
                ParserContext *context)
{
	CdnTokenOperator *op = CDN_TOKEN_OPERATOR (token);

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

		// The prime makes a df_dt operator of the current stack
		return parse_prime (expression, context);
	}

	CdnInstruction *inst = NULL;

	switch (op->type)
	{
		// arithmetic
		case CDN_TOKEN_OPERATOR_TYPE_MULTIPLY:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_MULTIPLY), "*", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_DIVIDE:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_DIVIDE), "/", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MODULO:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_MODULO), "%", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_PLUS), "+", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_MINUS), "-", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_POWER:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_POWER), "**", 2);
		break;

		// logical
		case CDN_TOKEN_OPERATOR_TYPE_GREATER:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_GREATER), ">", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_LESS), "<", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL), ">=", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_LESS_OR_EQUAL), "<=", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_EQUAL:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_EQUAL), "==", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_OR:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_OR), "||", 2);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_AND:
			inst = cdn_instruction_operator_new (cdn_math_operator_lookup (CDN_MATH_OPERATOR_TYPE_AND), "&&", 2);
		break;
		default:
			return FALSE;
		break;
	}

	// consume token
	cdn_token_free (cdn_tokenizer_next (context->buffer));

	if (!parse_expression (expression, context, op->priority, op->left_assoc))
	{
		return FALSE;
	}

	instructions_push (expression, inst, context);

	return TRUE;
}

static gboolean
parse_property (CdnExpression *expression,
                gchar const   *propname,
                ParserContext *context)
{
	CdnVariable *property;
	CdnFunction *f;
	gboolean prio;
	gboolean ret = TRUE;

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

	if (f && (prio || (!prio && !property)))
	{
		instructions_push (expression,
		                   cdn_instruction_custom_function_ref_new (f),
		                   context);

		ret = recurse_compile (expression, context, f);
	}
	else if (property)
	{
		instructions_push (expression,
		                   cdn_instruction_variable_new (property),
		                   context);
	}
	else
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
		               "Property `%s' not found",
		               nname);

		ret = FALSE;
	}

	g_free (nname);

	return ret;
}

static gboolean
parse_constant (CdnExpression  *expression,
                gchar const    *name,
                ParserContext  *context)
{
	gboolean found = FALSE;

	cdn_math_constant_lookup (name, &found);

	if (!found)
	{
		return FALSE;
	}

	instructions_push (expression,
	                   cdn_instruction_constant_new (name),
	                   context);

	return TRUE;
}

static gboolean
parse_number (CdnExpression   *expression,
              CdnTokenNumber  *token,
               ParserContext  *context)
{
	instructions_push (expression,
	                   cdn_instruction_number_new (token->value),
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
		ret = parse_dot_property (expression,
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
		cdn_token_free (cdn_tokenizer_next (context->buffer));
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
		// try to parse property
		ret = parse_property (expression, id, context);

		if (!ret)
		{
			// try parsing constants
			ret = parse_constant (expression, id, context);

			if (!ret)
			{
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_TOKEN,
				               "Could not find property or constant `%s'",
				               id);
			}
		}
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

	push_error_start (expression, context);

	while ((token = cdn_tokenizer_peek (*context->buffer)))
	{
		ret = TRUE;

		switch (token->type)
		{
			case CDN_TOKEN_TYPE_NUMBER:
				// simply push a number on the stack
				ret = parse_number (expression,
				                    CDN_TOKEN_NUMBER (token),
				                    context);
			break;
			case CDN_TOKEN_TYPE_IDENTIFIER:
			{
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
		else
		{
			pop_error_start (expression, context);
			push_error_start (expression, context);
		}
	}

	if (!ret && context->error && !cdn_compile_error_get_error (context->error))
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected expression but got (nothing)");
	}

	return ret;
}

static gboolean
validate_stack (CdnExpression *expression)
{
	GSList *item;
	gint stack = 0;
	gint maxstack = 1;

	g_slist_free (expression->priv->depends_on);
	expression->priv->depends_on = NULL;

	// check for empty instruction set
	if (!expression->priv->instructions)
	{
		instructions_push (expression,
		                   cdn_instruction_number_new (0.0),
		                   NULL);
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CdnInstruction *inst = item->data;
		GSList *deps;
		GSList *dep;

		stack += cdn_instruction_get_stack_count (inst);

		if (stack <= 0)
		{
			break;
		}

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

		if (stack > maxstack)
		{
			maxstack = stack;
		}
	}

	if (stack != 1)
	{
		return FALSE;
	}

	cdn_stack_destroy (&(expression->priv->output));
	cdn_stack_init (&(expression->priv->output), maxstack);

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
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), FALSE);

	if (!expression->priv->modified)
	{
		return TRUE;
	}

	gchar *buffer = expression->priv->expression;

	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));
	ParserContext ctx = {(gchar const **)&buffer, context, error};
	gboolean ret;

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
		return FALSE;
	}
	else
	{
		// reverse instructions
		expression->priv->instructions =
			g_slist_reverse (expression->priv->instructions);

		if (!validate_stack (expression))
		{
			instructions_free (expression);

			return parser_failed (expression,
			                      &ctx,
			                      CDN_COMPILE_ERROR_INVALID_STACK,
			                      "Invalid stack produced. This usually indicates a problem in the parser");
		}
	}

	cdn_expression_reset_cache (expression);

	if (expression->priv->modified)
	{
		expression->priv->modified = FALSE;

		// This should also trigger recompilation of things
		// that depend on this (like custom operators)
		g_object_notify (G_OBJECT (expression), "modified");
	}

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

	_cdn_expression_set_instructions_take (expression, copy);
}

void
_cdn_expression_set_instructions_take (CdnExpression *expression,
                                       GSList        *instructions)
{
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	instructions_free (expression);

	cdn_stack_destroy (&(expression->priv->output));

	cdn_expression_reset_cache (expression);
	expression->priv->modified = FALSE;

	g_object_notify (G_OBJECT (expression), "modified");

	expression->priv->instructions = instructions;

	if (!expression->priv->instructions)
	{
		instructions_push (expression,
		                   cdn_instruction_number_new (0.0),
		                   NULL);
	}

	validate_stack (expression);
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
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	set_value (expression, value);
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
	/* Omit type check to increase speed */
	if (!expression)
	{
		return 0.0;
	}

	if (expression->priv->cached)
	{
		return expression->priv->cached_output;
	}

	GSList *item;
	CdnStack *stack = &(expression->priv->output);

	cdn_stack_reset (stack);

	if (expression->priv->output.size == 0)
	{
		g_warning ("Stack size should not be 0 (%s)!",
		           expression->priv->expression);
		return 0.0;
	}

	if (!expression->priv->instructions)
	{
		g_warning ("No instructions found, maybe the expression was not parsed? (%s)",
		           expression->priv->expression);
		return 0.0;
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		cdn_instruction_execute (item->data, stack);
	}

	if (cdn_stack_count (&(expression->priv->output)) != 1)
	{
		g_warning ("Invalid output stack after evaluating: `%s'",
		           expression->priv->expression);

		return NAN;
	}

	if (expression->priv->has_cache)
	{
		expression->priv->cached = TRUE;
	}

	expression->priv->cached_output = cdn_stack_pop (&(expression->priv->output));

	return expression->priv->cached_output;
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
	/* Omit type check to increase speed */
	if (!expression->priv->prevent_cache_reset)
	{
		expression->priv->cached = FALSE;

		reset_depending_cache (expression);
	}
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
 * Returns: (element-type CdnExpression) (transfer container): a list of #CdnExpression
 *
 **/
const GSList *
cdn_expression_get_dependencies (CdnExpression *expression)
{
	/* Omit type check to increase speed */
	return expression->priv->depends_on;
}

/**
 * cdn_expression_get_depends_on_me:
 * @expression: a #CdnExpression
 *
 * Get a list of #CdnExpression which depend on @expression. The list is owned
 * by @expression and should not be freed or modified
 *
 * Returns: (element-type CdnExpression) (transfer container): a list of #CdnExpression
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
	/* Omit type check to increase speed */
	expression->priv->cached = FALSE;

	if (!expression->priv->once)
	{
		expression->priv->prevent_cache_reset = FALSE;
	}

	if (!expression->priv->modified)
	{
		expression->priv->modified = TRUE;
		g_object_notify (G_OBJECT (expression), "modified");
	}
}

/**
 * cdn_expression_get_instructions:
 * @expression: a #CdnExpression
 *
 * Get list of #CdnInstruction. The list is owned by @expression and should
 * not be freed or modified
 *
 * Returns: (element-type CdnInstruction) (transfer none): list of #CdnInstruction
 *
 **/
const GSList *
cdn_expression_get_instructions (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	return expression->priv->instructions;
}

const GSList *
cdn_expression_get_rand_instructions (CdnExpression *expression)
{
	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	return expression->priv->rand_instructions;
}

/**
 * cdn_expression_equal:
 * @expression: a #CdnExpression
 * @other: a #CdnExpression
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
                      CdnExpression *other)
{
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
		if (!cdn_instruction_equal (e1->data, e2->data))
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
	ret->priv->cached_output = expression->priv->cached_output;
	ret->priv->modified = expression->priv->modified;
	ret->priv->has_cache = expression->priv->has_cache;
	ret->priv->once = expression->priv->once;

	instr = expression->priv->instructions;

	while (instr)
	{
		instructions_push (ret,
		                   CDN_INSTRUCTION (cdn_mini_object_copy (CDN_MINI_OBJECT (instr->data))),
		                   NULL);

		instr = g_slist_next (instr);
	}

	ret->priv->instructions =
		g_slist_reverse (ret->priv->instructions);

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
