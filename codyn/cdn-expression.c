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

	guint indexing : 1;
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

#define dump_context(s, expression, context) \
do { \
	GSList *__stack = context->stack; \
	gint __i = g_slist_length (__stack); \
\
	g_printf("\n[%s:%d] %s:\n", __PRETTY_FUNCTION__, __LINE__, s); \
\
	while (__stack) \
	{ \
		CdnExpressionTreeIter *__iter; \
\
		__iter = cdn_expression_tree_iter_new_from_instructions (__stack->data); \
		g_printf ("  %2d) %s\n", __i, cdn_expression_tree_iter_to_string_dbg (__iter)); \
\
		cdn_expression_tree_iter_free (__iter); \
\
		--__i; \
		__stack = g_slist_next (__stack); \
	} \
} while (0)

#define dump_instructions(s, instructions) \
do { \
	GSList *__i; \
\
	g_printf("\n[%s:%d] %s:\n", __PRETTY_FUNCTION__, __LINE__, s); \
	for (__i = (instructions); __i; __i = __i->next) { \
		g_printf("  %s\n", cdn_instruction_to_string (__i->data)); \
	} \
} while (0)

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

	dimschanged = (expression->priv->cached &&
	               !cdn_dimension_equal (&expression->priv->cached_output.dimension,
	                                     dimension)) ||
	              (!expression->priv->cached &&
	               !cdn_dimension_equal (&expression->priv->retdim.dimension,
	                                     dimension));

	expression->priv->cached = TRUE;

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

static GSList *
stack_push (GSList                     *stack,
            CdnInstruction             *instruction,
            CdnStackManipulation const *smanip)
{
	GSList *ret;
	gint consume;

	ret = g_slist_prepend (NULL, instruction);

	if (!smanip)
	{
		smanip = cdn_instruction_get_stack_manipulation (instruction, NULL);
	}

	consume = smanip->pop.num;

	while (consume)
	{
		ret = g_slist_concat (stack->data, ret);

		stack = g_slist_delete_link (stack, stack);
		--consume;
	}

	stack = g_slist_prepend (stack, ret);
	return stack;
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

		context->stack = stack_push (context->stack, next, smanip);
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
find_context_function (CdnExpression *expression,
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

static gboolean
is_numeric (GSList const *instructions)
{
	while (instructions)
	{
		if (!(CDN_IS_INSTRUCTION_NUMBER (instructions->data) ||
		      CDN_IS_INSTRUCTION_MATRIX (instructions->data) ||
		      CDN_IS_INSTRUCTION_INDEX (instructions->data)))
		{
			return FALSE;
		}

		instructions = g_slist_next (instructions);
	}

	return TRUE;
}

static CdnDimension
instruction_get_dimension (CdnInstruction *i)
{
	CdnStackManipulation const *smanip;

	smanip = cdn_instruction_get_stack_manipulation (i, NULL);
	return smanip->push.dimension;
}

static gint
instructions_to_indices (GSList const *instructions,
                         gint         *ret,
                         gint          num)
{
	gint idx = 0;

	while (instructions)
	{
		CdnInstruction *n = instructions->data;

		if (CDN_IS_INSTRUCTION_NUMBER (n))
		{
			if (idx >= num)
			{
				return -1;
			}

			gdouble v = cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (n));
			*ret++ = (gint)(v + 0.5);

			++idx;
		}
		else if (CDN_IS_INSTRUCTION_INDEX (n))
		{
			CdnInstructionIndex *i;

			i = CDN_INSTRUCTION_INDEX (n);

			if (!cdn_instruction_index_write_indices (i,
			                                          ret,
			                                          num - idx))
			{
				return -1;
			}
			else
			{
				CdnStackManipulation const *smanip;

				smanip = cdn_instruction_get_stack_manipulation (n, NULL);

				idx += cdn_dimension_size (&smanip->push.dimension);
			}
		}
		else if (!CDN_IS_INSTRUCTION_MATRIX (n))
		{
			return -1;
		}

		instructions = g_slist_next (instructions);
	}

	return idx;
}

static CdnStackArg const *
instruction_get_push_arg (CdnInstruction *i)
{
	CdnStackManipulation const *smanip;

	smanip = cdn_instruction_get_stack_manipulation (i, NULL);
	return &smanip->push;
}

static CdnStackArgs const *
instruction_get_pop_args (CdnInstruction *i)
{
	CdnStackManipulation const *smanip;

	smanip = cdn_instruction_get_stack_manipulation (i, NULL);
	return &smanip->pop;
}

static void
free_popped_stack (GSList *i)
{
	g_slist_foreach (i, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (i);
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

static CdnInstruction *
zeros_macro (CdnExpression *expression,
             ParserContext *context,
             gint           numargs)
{
	GSList *columns = NULL;
	CdnDimension dimension = CDN_DIMENSION(1, 1);
	GSList *ptr;

	if (context->stack)
	{
		columns = context->stack->data;
		dimension = instruction_get_dimension (g_slist_last (columns)->data);
	}

	if (numargs == 1 && cdn_dimension_size (&dimension) == 2)
	{
		gint indices[2] = {0, 0};
		GSList *i;

		i = pop_stack (expression, context);

		if (instructions_to_indices (i, indices, 2) == -1)
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The `zeros` function expects 2 number arguments or a 1-by-2 number vector");

			free_popped_stack (i);
			return NULL;
		}

		free_popped_stack (i);

		dimension.rows = indices[0];
		dimension.columns = indices[1];
	}
	else if (numargs == 2)
	{
		GSList *rows = NULL;
		GSList *i;

		if (context->stack && context->stack->next)
		{
			rows = context->stack->next->data;
		}

		if (!columns || !rows ||
		    columns->next || rows->next ||
		    !CDN_IS_INSTRUCTION_NUMBER (rows->data) ||
		    !CDN_IS_INSTRUCTION_NUMBER (rows->data))
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The `zeros' function expects 2 number arguments or a 1-by-2 number vector");

			return NULL;
		}

		dimension.rows = (gint) (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (rows->data)) + 0.5);
		dimension.columns = (gint) (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (columns->data)) + 0.5);

		i = pop_stack (expression, context);
		g_slist_foreach (i, (GFunc)cdn_mini_object_unref, NULL);

		i = pop_stack (expression, context);
		g_slist_foreach (i, (GFunc)cdn_mini_object_unref, NULL);
	}
	else
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `zeros` function expects 2 number arguments or a 1-by-2 number vector");

		return NULL;
	}

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
           ParserContext *context,
           gint           numargs)
{
	GSList *first = NULL;
	gint num;

	if (numargs != 1)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `eye' function expects 1 argument, not %d",
		               numargs);

		return NULL;
	}

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
		               "The `eye' function expects 1 number argument");

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
		gint c;
		gint i = 0;
		CdnStackArgs args;
		CdnDimension dim;

		cdn_stack_args_init (&args, num * num);

		dim.rows = num;
		dim.columns = num;

		for (c = 0; c < num; ++c)
		{
			gint r;

			for (r = 0; r < num; ++r)
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
              ParserContext *context,
              gint           numargs)
{
	GSList *first = NULL;
	gchar *s;
	CdnStackArgs args;
	CdnInstruction *ret;

	if (numargs != 1)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `length' function expects 1 argument, not %d",
		               numargs);

		return NULL;
	}

	first = context->stack->data;

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
            ParserContext *context,
            gint           numargs)
{
	if (numargs < 1 || numargs > 2)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The `size' function expects 1 or 2 arguments, not %d",
		               numargs);

		return NULL;
	}

	if (numargs == 2)
	{
		GSList *idx = pop_stack (expression, context);
		gint i;
		CdnDimension dim;
		GSList *d;

		if (idx->next || !CDN_IS_INSTRUCTION_NUMBER (idx->data))
		{
			free_popped_stack (idx);

			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The second argument of the `size' function must be a number");

			return NULL;
		}

		i = (gint)(cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (idx->data)) + 0.5);
		free_popped_stack(idx);

		if (i != 0 && i != 1)
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The second argument of the `size' function must be either 0 or 1");

			return NULL;
		}

		d = pop_stack (expression, context);
		dim = instruction_get_dimension (g_slist_last (d)->data);
		free_popped_stack (d);

		return cdn_instruction_number_new (dim.dims[i]);
	}
	else
	{
		gchar *sr;
		gchar *sc;
		CdnStackArgs args;
		CdnStackArg nargs[2] = {CDN_STACK_ARG_EMPTY, CDN_STACK_ARG_EMPTY};
		GSList *i;
		CdnDimension dim;

		i = pop_stack (expression, context);
		dim = instruction_get_dimension (g_slist_last (i)->data);
		free_popped_stack (i);

		sr = g_strdup_printf ("%d", dim.rows);
		sc = g_strdup_printf ("%d", dim.columns);

		instructions_push (expression,
		                   cdn_instruction_number_new_from_string (sr),
		                   context);

		instructions_push (expression,
		                   cdn_instruction_number_new_from_string (sc),
		                   context);

		g_free (sr);
		g_free (sc);

		args.num = 2;
		args.args = nargs;

		args.args[0].dimension = cdn_dimension_one;
		args.args[1].dimension = cdn_dimension_one;

		dim.rows = 1;
		dim.columns = 2;

		return cdn_instruction_matrix_new (&args, &dim);
	}
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

static CdnStackArg const *
get_last_stack_arg (ParserContext *context)
{
	return instruction_get_push_arg (g_slist_last (context->stack->data)->data);
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
		function = find_context_function (expression, name, cname, context);

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
				function = find_context_function (expression,
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
			gboolean only_local;

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

			only_local = cdn_compile_context_get_only_local_variables (context->context);
			cdn_compile_context_set_only_local_variables (context->context, FALSE);

			prop = lookup_variable (expression, context, aname);

			cdn_compile_context_set_only_local_variables (context->context, only_local);

			if (!prop)
			{
				return parser_failed (expression,
				                      context,
				                      CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND,
				                      "The implicit variable `%s' for function `%s' is not found",
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
					                           context,
					                           numargs);
				break;
				case CDN_MATH_FUNCTION_TYPE_EYE:
					instruction = eye_macro (expression,
					                         context,
					                         numargs);
				break;
				case CDN_MATH_FUNCTION_TYPE_LENGTH:
					instruction = length_macro (expression,
					                            context,
					                            numargs);
				break;
				case CDN_MATH_FUNCTION_TYPE_SIZE:
					instruction = size_macro (expression,
					                          context,
					                          numargs);
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

static void
instructions_push_list (CdnExpression *expression,
                        ParserContext *context,
                        GSList        *instructions)
{
	while (instructions)
	{
		instructions_push (expression, instructions->data, context);
		instructions = g_slist_delete_link (instructions, instructions);
	}
}

static GSList *
parse_matrix_decompose_variable (CdnInstruction *instruction,
                                 GSList         *instructions)
{
	// Use slicing on the variable
	CdnStackArg const *arg;
	guint *slice;
	CdnDimension dim;
	gint rows;
	gint columns;
	gint r;
	gint c;
	GSList *ret = NULL;

	arg = instruction_get_push_arg (instruction);

	rows = arg->dimension.rows;
	columns = arg->dimension.columns;

	slice = g_new0 (guint, rows);

	// Resulting slice dimensions
	dim.rows = rows;
	dim.columns = 1;

	// Prepare initial slice
	for (r = 0; r < rows; ++r)
	{
		slice[r] = (guint)r;
	}

	// For each column, append a variable slice instruction
	for (c = 0; c < columns; ++c)
	{
		// Append new instructions with the slices
		CdnInstructionVariable *v;

		v = cdn_mini_object_copy (instruction);

		cdn_instruction_variable_apply_slice (v,
		                                      slice,
		                                      rows,
		                                      &dim);

		// we prepend here, and reverse in the end
		ret = g_slist_prepend (ret, v);

		// Compute next column slice
		for (r = 0; r < rows; ++r)
		{
			slice[r] += rows;
		}
	}

	g_free (slice);

	g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instructions);

	// keep result instructions in correct order
	return g_slist_reverse (ret);
}

static GSList *
parse_matrix_decompose_matrix (CdnInstruction *instruction,
                               GSList         *instructions,
                               GSList         *instructionl)
{
	// Check if individual values can be decomposed
	CdnStackArgs const *args;
	gint i;

	// Decompose the individual values of the matrix, if possible
	args = instruction_get_pop_args (instruction);

	for (i = 0; i < args->num; ++i)
	{
		// Can only decompose single column values
		if (args->args[i].dimension.columns != 1)
		{
			return NULL;
		}
	}

	// To do this, we strip off the matrix instruction
	cdn_mini_object_unref (instruction);
	return g_slist_delete_link (instructions, instructionl);
}

/*
 * parse_matrix_decompose will try to decompose the last expression on the
 * stack into single column expressions if possible.
 */
static GSList *
parse_matrix_decompose (GSList *instructions)
{
	CdnInstruction *last;
	GSList *lastl;

	lastl = g_slist_last (instructions);
	last = lastl->data;

	if (instruction_get_dimension (last).columns == 1)
	{
		// No need to decompose single column
		return instructions;
	}

	if (CDN_IS_INSTRUCTION_VARIABLE (last))
	{
		return parse_matrix_decompose_variable (last, instructions);
	}
	else if (CDN_IS_INSTRUCTION_MATRIX (last))
	{
		return parse_matrix_decompose_matrix (last, instructions, lastl);
	}
	else
	{
		// Can't do it!
		return NULL;
	}
}

typedef struct
{
	CdnDimension dimension;
	gint numpop;
	GSList *arguments;
} MatrixRow;

static MatrixRow *
matrix_row_new ()
{
	return g_slice_new0 (MatrixRow);
}

static void
matrix_row_free (MatrixRow *row)
{
	while (row->arguments)
	{
		GSList *a = row->arguments->data;

		g_slist_foreach (a, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (a);

		row->arguments = g_slist_delete_link (row->arguments,
		                                      row->arguments);
	}

	g_slice_free (MatrixRow, row);
}

/*
 * parse_matrix_row parses column expressions until a full row of columns is
 * parsed. Note that expressions parsed can be multidimensional and this function
 * will check of the concatenated rows are the same. For example:
 *
 * 1) [[1;2;3], [4;5;6]]
 * 2) [[1;2;3], [5;6]]
 *
 * Where the outer matrix is parsed in one time by this function. The first
 * example has consistent rows, but the second example does not. Therefore this
 * function returns FALSE (and sets an appropriate error) when trying to parse
 * the second example.
 */
static MatrixRow *
parse_matrix_row (CdnExpression *expression,
                  ParserContext *context,
                  gboolean      *isend)
{
	MatrixRow *ret;

	*isend = FALSE;

	CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

	if (!next)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "Unexpected end of expression (expected ; or ])");

		return NULL;
	}

	*isend = CDN_TOKEN_IS_OPERATOR (next) &&
	         CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END;

	cdn_token_free (next);

	ret = matrix_row_new ();

	if (*isend)
	{
		return ret;
	}

	while (TRUE)
	{
		CdnStackArg const *arg;

		// Parse one expression
		if (!parse_expression (expression, context, -1, 0))
		{
			return NULL;
		}

		// Get argdim of the last expression to extract the dimension
		// of the expression
		arg = get_last_stack_arg (context);

		/* Check argument consistency. This function is parsing one row
		 * in the sense of the matrix syntax (i.e. until ] or ;). The
		 * invididual arguments being parsed can still be multi row
		 * themselves. Here we simply check of we concatenate columns
		 * with the same number of rows.
		 */
		if (ret->dimension.rows != 0)
		{
			if (ret->dimension.rows != arg->rows)
			{
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
				               "Cannot concatenate %d row%s with %d row%s",
				               ret->dimension.rows,
				               ret->dimension.rows > 1 ? "s" : "",
				               arg->rows,
				               arg->rows > 1 ? "s" : "");

				matrix_row_free (ret);
				return NULL;
			}

			ret->dimension.columns += arg->columns;
		}
		else
		{
			// Initial dimensions
			ret->dimension = arg->dimension;
		}

		ret->arguments = g_slist_prepend (ret->arguments,
		                                  pop_stack (expression, context));

		++ret->numpop;

		// Check next token for the end of the "row"
		CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

		if (!next)
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "Unexpected end of expression (expected ; or ])");

			matrix_row_free (ret);
			return NULL;
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

			break;
		}
		else if (type == CDN_TOKEN_OPERATOR_TYPE_COMMA)
		{
			// Consume comma token and continue parsing the row
			cdn_token_free (cdn_tokenizer_next (context->buffer));
		}
	}

	// store arguments in order of occurence
	ret->arguments = g_slist_reverse (ret->arguments);
	return ret;
}

static GSList *
matrix_reorder_flat (GSList              *ret,
                     CdnDimension        *retdim,
                     GSList             **m,
                     gint                 mrstride,
                     gint                *rowsize,
                     CdnDimension const  *dim)
{
	gint c;
	gint i = 0;
	gint mi = 0;
	CdnStackArgs args;
	gint r;
	gint rrows = 0;
	CdnDimension mdim;

	cdn_stack_args_init (&args, cdn_dimension_size (dim));

	for (c = 0; c < dim->columns; ++c)
	{
		for (r = 0; r < dim->rows; ++r)
		{
			ret = g_slist_concat (m[mi + r], ret);

			args.args[i].dimension.rows = rowsize[r];
			args.args[i].dimension.columns = 1;

			if (c == 0)
			{
				rrows += rowsize[r];
			}

			++i;
		}

		mi += mrstride;
	}

	mdim.rows = rrows;
	mdim.columns = dim->columns;

	// Insert matrix instruction
	ret = g_slist_prepend (ret, cdn_instruction_matrix_new (&args,
	                                                        &mdim));

	if (retdim->columns != 0)
	{
		CdnStackArg arg[2] = {
			CDN_STACK_ARG(retdim->rows, retdim->columns),
			CDN_STACK_ARG(dim->rows, dim->columns)
		};

		CdnStackArgs vargs = {
			.num = 2,
			.args = arg,
		};

		// Also insert a vcat here
		ret = g_slist_prepend (ret,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_VCAT,
		                                                     NULL,
		                                                     &vargs));
	}
	else
	{
		retdim->columns = dim->columns;
	}

	retdim->rows += rrows;

	cdn_stack_args_destroy (&args);
	return ret;
}

/* matrix_vcat will concatenate single parsed matrix rows in one matrix
 * expression. Its purpose is to perform the vcat inline if possible
 * (on single column values) or to insert vcat() calls otherwise.
 */
static void
matrix_vcat (CdnExpression      *expression,
             ParserContext      *context,
             GSList             *rows,
             CdnDimension const *dim)
{
	GSList **m;
	gint *rowsize;
	gint mrow = 0;
	GSList *vcat = NULL;
	GSList *ret = NULL;
	CdnDimension retdim = CDN_DIMENSION(0, 0);

	m = g_new0 (GSList *, cdn_dimension_size (dim));
	rowsize = g_new0 (gint, dim->rows);

	while (rows)
	{
		MatrixRow *row = rows->data;
		gboolean is_single = TRUE;
		GSList *stack = NULL;

		rowsize[mrow] = row->dimension.rows;

		/* Try to decompose all the arguments that make up the row in
		 * single valued columns, otherwise we can't rearrange them.
		 */
		while (row->arguments)
		{
			GSList *arg = row->arguments->data;
			GSList *newinstr;

			row->arguments = g_slist_delete_link (row->arguments,
			                                      row->arguments);

			newinstr = parse_matrix_decompose (arg);

			if (newinstr == NULL)
			{
				is_single = FALSE;
				newinstr = arg;
			}

			// Push instructions on the row stack
			while (newinstr)
			{
				stack = stack_push (stack, newinstr->data, NULL);
				newinstr = g_slist_delete_link (newinstr, newinstr);
			}
		}

		if (is_single)
		{
			gint c;
			gint nr = 0;

			stack = g_slist_reverse (stack);
			c = mrow;

			// Fill one row of <m> from the stack
			while (stack)
			{
				// note we reverse the stack list here because
				// we keep values in <m> in reversed order so
				// we can more efficiently concat them later
				GSList *rev = g_slist_reverse (stack->data);
				CdnDimension d;

				// Accumulate instructions per row block
				m[c] = g_slist_concat (rev, m[c]);

				// Check if we should go to the next column
				d = instruction_get_dimension (rev->data);

				nr += d.rows;

				if (nr == row->dimension.rows)
				{
					// advance!
					c += dim->rows;
					nr = 0;
				}

				stack = g_slist_delete_link (stack, stack);
			}

			++mrow;
		}
		else
		{
			CdnStackArgs args;
			gint i = 0;
			gboolean isone;

			if (mrow > 0)
			{
				CdnDimension fd = CDN_DIMENSION(mrow, dim->columns);

				/* We have been collecting things to be
				 * reordered. Since we now need a vcat for the
				 * next row, process the builtup matrix and
				 * add it to the result.
				 */
				ret = matrix_reorder_flat (ret,
				                           &retdim,
				                           m,
				                           dim->rows,
				                           rowsize,
				                           &fd);

				mrow = 0;

				// Clear memory
				memset (m, 0, sizeof (GSList *) * cdn_dimension_size (dim));
				memset (rowsize, 0, sizeof (gint) * dim->rows);
			}

			cdn_stack_args_init (&args, g_slist_length (stack));

			vcat = NULL;
			isone = !stack->next;

			// Collect all instructions
			while (stack)
			{
				CdnInstruction *last = g_slist_last (stack->data)->data;

				args.args[i].dimension = instruction_get_dimension (last);

				// note we reverse the stack instructions here
				// because we build a reversed order result
				vcat = g_slist_concat (vcat, g_slist_reverse (stack->data));
				stack = g_slist_delete_link (stack, stack);
			}

			// Add matrix instruction to group it all together if needed
			if (!isone)
			{
				CdnInstruction *mi;

				mi = cdn_instruction_matrix_new (&args,
				                                 &row->dimension);

				vcat = g_slist_prepend (vcat, mi);
			}

			if (retdim.rows > 0)
			{
				CdnStackArg varg[2] = {
					CDN_STACK_ARG(0, 0),
					CDN_STACK_ARG(0, 0)
				};

				CdnStackArgs vargs = {
					.num = 2,
					.args = varg
				};

				CdnInstruction *i;

				i = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_VCAT,
				                                  NULL,
				                                  &vargs);

				// Add vcat instruction
				vcat = g_slist_prepend (vcat, i);

				retdim.rows += row->dimension.rows;
			}
			else
			{
				retdim = row->dimension;
			}

			ret = g_slist_concat (vcat, ret);
			cdn_stack_args_destroy (&args);
		}

		matrix_row_free (row);
		rows = g_slist_delete_link (rows, rows);
	}

	// rearrange the last remaining rows if any
	if (mrow > 0)
	{
		CdnDimension fd = CDN_DIMENSION(mrow, dim->columns);

		/* We have been collecting things to be
		 * reordered. Since we now need a vcat for the
		 * next row, process the builtup matrix and
		 * add it to the result.
		 */
		ret = matrix_reorder_flat (ret,
		                           &retdim,
		                           m,
		                           dim->rows,
		                           rowsize,
		                           &fd);
	}

	// ret was kept in reverse order, but instructions_push_list expects
	// it in normal order, so reverse ret first
	ret = g_slist_reverse (ret);

	instructions_push_list (expression, context, ret);

	g_free (m);
	g_free (rowsize);
}

static gboolean
parse_matrix (CdnExpression *expression,
              ParserContext *context)
{
	CdnDimension tdim = CDN_DIMENSION (0, 0);
	gint tnumpop = 0;
	GSList *rows = NULL;

	while (TRUE)
	{
		MatrixRow *row;
		gboolean isend;

		row = parse_matrix_row (expression,
		                        context,
		                        &isend);

		if (row == NULL)
		{
			g_slist_foreach (rows, (GFunc)matrix_row_free, NULL);
			g_slist_free (rows);
			return FALSE;
		}

		rows = g_slist_prepend (rows, row);

		if (tdim.rows == 0)
		{
			// First time. Just store the dimensions and number of
			// expressions consumed
			tdim.rows = row->dimension.rows;
			tdim.columns = row->dimension.columns;

			tnumpop = row->numpop;
		}
		else if (tdim.columns != row->dimension.columns)
		{
			gint cols = row->dimension.columns;

			// Check for consistent columns concatenation between
			// rows parsed
			g_slist_foreach (rows, (GFunc)matrix_row_free, NULL);
			g_slist_free (rows);

			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			                      "Cannot concatenate %d column%s with %d column%s",
			                      tdim.columns,
			                      tdim.columns > 1 ? "s" : "",
			                      cols,
			                      cols > 1 ? "s" : "");
		}
		else
		{
			tnumpop += row->numpop;
			tdim.rows += row->dimension.rows;
		}

		if (isend)
		{
			break;
		}
	}

	// Store rows in order of occurance
	rows = g_slist_reverse (rows);

	/* vcat when there is more than one parsed row and the resulting number
	 * of columns is larger than 1.
	 */
	if (rows->next && tdim.columns > 1)
	{
		/* Here we need to vcat all the rows together. We are going to
		 * try to do this inline right here as much as possible, by
		 * rearranging expressions on the stack as needed. If such
		 * rearrangement is not possible, a 'vcat' call will be inserted
		 * which dynamically rearranges the terms when the expression
		 * is executed.
		 */
		matrix_vcat (expression, context, rows, &tdim);
	}
	else
	{
		// Push back arguments on the stack
		while (rows)
		{
			MatrixRow *row = rows->data;

			while (row->arguments)
			{
				instructions_push_list (expression,
				                        context,
				                        row->arguments->data);

				row->arguments = g_slist_delete_link (row->arguments,
				                                      row->arguments);
			}

			matrix_row_free (row);
			rows = g_slist_delete_link (rows, rows);
		}

		if (tnumpop > 1)
		{
			CdnStackArgs args;

			// Wrap in matrix instruction
			get_argdim (expression, context, tnumpop, &args);

			instructions_push (expression,
			                   cdn_instruction_matrix_new (&args,
			                                               &tdim),
			                   context);

			cdn_stack_args_destroy (&args);
		}
	}

	return TRUE;
}

static gboolean
is_index_range (CdnInstruction       *instruction,
                CdnInstructionIndex **retval)
{
	if (!CDN_IS_INSTRUCTION_INDEX (instruction))
	{
		return FALSE;
	}

	*retval = CDN_INSTRUCTION_INDEX (instruction);

	return cdn_instruction_index_get_index_type (*retval) ==
	       CDN_INSTRUCTION_INDEX_TYPE_RANGE;
}

static CdnInstruction *
make_static_index (CdnExpression *expression,
                   ParserContext *context,
                   gint           nargs)
{
	gint *indices = NULL;
	gint n;
	CdnStackArg const *d3;
	gboolean continuous;
	gint offset;
	CdnDimension retdim;
	gint i;

	if (nargs == 2)
	{
		GSList *i1;
		GSList *i2;
		CdnDimension d1;
		CdnDimension d2;
		gint *rows;
		gint *cols;
		gint i;
		CdnInstructionIndex *r1;
		CdnInstructionIndex *r2;
		gboolean isi1;
		gboolean isi2;

		i2 = pop_stack (expression, context);
		i1 = pop_stack (expression, context);

		d3 = get_last_stack_arg (context);

		isi1 = is_index_range (i1->data, &r1);
		isi2 = is_index_range (i2->data, &r2);

		if (isi1 && isi2)
		{
			CdnIndexRange const *rows;
			CdnIndexRange const *columns;

			rows = cdn_instruction_index_get_range (r1);
			columns = cdn_instruction_index_get_range (r2);

			return cdn_instruction_index_new_range_block (rows,
			                                              columns,
			                                              d3);
		}
		else if (isi1 && CDN_IS_INSTRUCTION_NUMBER (i2->data))
		{
			CdnIndexRange const *rows;
			CdnIndexRange columns = {0, 1, 0};

			rows = cdn_instruction_index_get_range (r1);

			columns.start = (gint)(cdn_instruction_number_get_value (i2->data) + 0.5);
			columns.end = columns.start + 1;

			return cdn_instruction_index_new_range_block (rows,
			                                              &columns,
			                                              d3);
		}
		else if (isi2 && CDN_IS_INSTRUCTION_NUMBER (i1->data))
		{
			CdnIndexRange const *columns;
			CdnIndexRange rows = {0, 1, 0};

			columns = cdn_instruction_index_get_range (r2);

			rows.start = (gint)(cdn_instruction_number_get_value (i1->data) + 0.5);
			rows.end = rows.start + 1;

			return cdn_instruction_index_new_range_block (&rows,
			                                              columns,
			                                              d3);
		}

		d1 = instruction_get_dimension (g_slist_last (i1)->data);
		d2 = instruction_get_dimension (g_slist_last (i2)->data);

		if (!cdn_dimension_equal (&d1, &d2))
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The dimensions of the two index arguments are different (%d-by-%d and %d-by-%d)",
			               d1.rows, d1.columns,
			               d2.rows, d2.columns);

			return NULL;
		}

		n = cdn_dimension_size (&d1);

		rows = g_new (gint, n);
		cols = g_new (gint, n);

		instructions_to_indices (i1, rows, n);
		instructions_to_indices (i2, cols, n);

		// Double indexing. i1 represents rows, i2 represents columns.
		// Note column-major ordering
		indices = g_new0 (gint, n);

		for (i = 0; i < n; ++i)
		{
			indices[i] = rows[i] + d3->dimension.rows * cols[i];
		}

		free_popped_stack (i1);
		free_popped_stack (i2);

		retdim = d1;
	}
	else
	{
		GSList *i;
		CdnDimension d;
		CdnInstructionIndex *idx;

		// First check for existing index.
		i = pop_stack (expression, context);
		d3 = get_last_stack_arg (context);

		idx = (CdnInstructionIndex *)i->data;

		if (i->next == NULL && CDN_IS_INSTRUCTION_INDEX (i->data) &&
		    cdn_instruction_index_get_index_type (idx) ==
		    CDN_INSTRUCTION_INDEX_TYPE_RANGE)
		{
			CdnInstruction *ret;

			ret = cdn_instruction_index_new_range (cdn_instruction_index_get_range (idx),
			                                       d3);

			free_popped_stack (i);
			return ret;
		}

		// Single indexing. linear index
		d = instruction_get_dimension (g_slist_last (i)->data);

		n = cdn_dimension_size (&d);
		indices = g_new0 (gint, n);

		instructions_to_indices (i, indices, n);
		free_popped_stack (i);

		retdim = d;
	}

	continuous = TRUE;
	offset = 0;

	// indices now contains a list of n linear indices into the remaining
	// thing on the stack. See if it fits.
	for (i = 0; i < n; ++i)
	{
		if (indices[i] >= cdn_dimension_size (&d3->dimension))
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "The specified indices are out of bounds");

			return NULL;
		}

		if (i != 0 && offset + 1 != indices[i])
		{
			continuous = FALSE;
		}
		else
		{
			offset = indices[i];
		}
	}

	if (continuous)
	{
		gint start = indices[0];
		g_free (indices);

		return cdn_instruction_index_new_offset (start,
		                                         &retdim,
		                                         d3);
	}
	else
	{
		// NOTE: the instruction takes ownership of the indices memory
		return cdn_instruction_index_new (indices,
		                                  &retdim,
		                                  d3);
	}
}

static gboolean
parse_indexing (CdnExpression *expression,
                ParserContext *context)
{
	CdnTokenOperatorType type;
	gint numargs = 0;

	// Indexing is done using one or two arguments, which can be matrices
	// themselves, or ranges
	CdnToken *next = cdn_tokenizer_peek (*(context->buffer));

	if (next && CDN_TOKEN_IS_OPERATOR (next) &&
	    CDN_TOKEN_OPERATOR (next)->type == CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		cdn_token_free (next);
		cdn_token_free (cdn_tokenizer_next (context->buffer));
	}
	else
	{
		context->indexing = TRUE;
		cdn_token_free (next);

		if (!parse_expression (expression, context, -1, 0))
		{
			context->indexing = FALSE;
			return FALSE;
		}

		++numargs;

		next = cdn_tokenizer_peek (*(context->buffer));

		// Check for a comma. In that case the indexing is a row/column
		// index.
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
					context->indexing = FALSE;
					return FALSE;
				}

				++numargs;
			}
		}
		else
		{
			parser_failed (expression,
			               context,
			               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			               "Expected `]' or `,' but got `%s'",
			               next->text ? next->text : NULL);

			cdn_token_free (next);
			context->indexing = FALSE;

			return FALSE;
		}
	}

	next = cdn_tokenizer_next (context->buffer);
	type = CDN_TOKEN_OPERATOR (next)->type;

	context->indexing = FALSE;

	if (type != CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "Expected `]' but got `%s'",
		               next->text ? next->text : NULL);

		cdn_token_free (next);

		return FALSE;
	}

	if (is_numeric (context->stack->data) &&
	    (numargs == 1 || is_numeric (context->stack->next->data)))
	{
		CdnInstruction *i;

		i = make_static_index (expression, context, numargs);

		if (i == NULL)
		{
			return FALSE;
		}

		instructions_push (expression, i, context);
	}
	else
	{
		CdnStackArgs args;

		swap_arguments_index (expression, context, numargs + 1);
		get_argdim (expression, context, numargs + 1, &args);

		instructions_push (expression,
		                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_INDEX,
		                                                 "index",
		                                                 &args),
		                   context);

		cdn_stack_args_destroy (&args);
	}

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

static CdnInstruction *
indexing_range (CdnExpression *expression,
                ParserContext *context,
                CdnStackArgs  *args)
{
	CdnInstructionNumber *n1;
	CdnIndexRange range;
	GSList *second;
	GSList *first;

	second = pop_stack (expression, context);
	first = pop_stack (expression, context);

	if (second->next ||
	    first->next ||
	    !CDN_IS_INSTRUCTION_NUMBER (first->data) ||
	    !(CDN_IS_INSTRUCTION_NUMBER (second->data) || (CDN_IS_INSTRUCTION_INDEX (second->data) && cdn_instruction_index_get_index_type (second->data) == CDN_INSTRUCTION_INDEX_TYPE_RANGE)))
	{
		free_popped_stack (second);
		free_popped_stack (first);

		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		               "The arguments for the range operator `:' can only be single numerical arguments");

		return NULL;
	}

	n1 = CDN_INSTRUCTION_NUMBER (first->data);
	range.start = (gint)(cdn_instruction_number_get_value (n1) + 0.5);

	if (CDN_IS_INSTRUCTION_NUMBER (second->data))
	{
		CdnInstructionNumber *n2;

		n2 = CDN_INSTRUCTION_NUMBER (second->data);

		range.end = (gint)(cdn_instruction_number_get_value (n2) + 0.5);
		range.step = 1;
	}
	else
	{
		CdnInstructionIndex *idx = CDN_INSTRUCTION_INDEX (second->data);
		CdnIndexRange const *sr;;

		sr = cdn_instruction_index_get_range (idx);

		range.end = sr->end;
		range.step = sr->start;
	}

	free_popped_stack (second);
	free_popped_stack (first);

	return cdn_instruction_index_new_range (&range, NULL);
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
		case CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE:
			if (context->indexing)
			{
				inst = indexing_range (expression,
				                       context,
				                       &args);
			}
		break;
		default:
		break;
	}

	cdn_stack_args_destroy (&args);

	if (inst)
	{
		return instructions_push (expression, inst, context);
	}
	else
	{
		return FALSE;
	}
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
				    (op->type == CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE && !context->indexing) ||
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
	ParserContext ctx = {(gchar const **)&buffer, context, error, FALSE};
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

/**
 * cdn_expression_sum:
 * @expressions: (element-type CdnExpression): a #GSList of #CdnExpression.
 *
 * Create a new expression which represents the sum of the given expressions.
 *
 * Returns: (transfer full): a #CdnExpression.
 *
 **/
CdnExpression *
cdn_expression_sum (GSList const *expressions)
{
	GSList const *item;
	GSList *instructions = NULL;
	CdnStackArg const *lastarg = NULL;
	GString *srep;
	CdnExpression *ret;

	if (!expressions)
	{
		return cdn_expression_new0 ();
	}

	if (!expressions->next)
	{
		return cdn_expression_copy (expressions->data);
	}

	srep = g_string_new ("");

	for (item = expressions; item; item = g_slist_next (item))
	{
		CdnExpression *e = item->data;
		GSList const *instrs;
		CdnStackArg *sarg;

		if (e->priv->modified)
		{
			g_string_free (srep, TRUE);
			g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);

			return NULL;
		}

		if (item != expressions)
		{
			g_string_append (srep, " + ");
		}

		g_string_append (srep, e->priv->expression);

		for (instrs = e->priv->instructions; instrs; instrs = g_slist_next (instrs))
		{
			instructions = g_slist_prepend (instructions,
			                                cdn_mini_object_copy (instrs->data));
		}

		sarg = cdn_expression_get_stack_arg (e);

		if (lastarg != NULL)
		{
			CdnInstruction *i;
			CdnStackManipulation const *smanip;

			CdnStackArg arg2[2] = {
				*sarg,
				*lastarg,
			};

			CdnStackArgs args = {
				2,
				arg2,
			};

			// Add plus operator
			i = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
			                                  NULL,
			                                  &args);

			instructions = g_slist_prepend (instructions, i);

			smanip = cdn_instruction_get_stack_manipulation (i, NULL);

			if (!smanip)
			{
				g_string_free (srep, TRUE);
				g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);

				return NULL;
			}

			lastarg = &smanip->push;
		}
		else
		{
			lastarg = sarg;
		}
	}

	instructions = g_slist_reverse (instructions);
	ret = cdn_expression_new (srep->str);
	g_string_free (srep, TRUE);

	cdn_expression_set_instructions_take (ret, instructions);
	return ret;
}
