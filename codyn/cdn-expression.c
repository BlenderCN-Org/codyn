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

#include <execinfo.h>

#include <codyn/instructions/cdn-instructions.h>

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>
#include <unistd.h>

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
	gint retdims[2];

	GSList *depends_on;
	GSList *depends_on_me;

	union
	{
		gdouble cached_output;
		gdouble *cached_output_multi;
	};

	gint cached_dims[2];

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

static void reset_cache (CdnExpression *expression,
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
			gint oldr = dep->priv->retdims[0];
			gint oldc = dep->priv->retdims[1];

			// We need to revalidate our stack here
			validate_stack (dep, NULL, TRUE);

			realchanged = (dep->priv->retdims[0] != oldr ||
			               dep->priv->retdims[1] != oldc);
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
set_values (CdnExpression *expression,
            gdouble const *values,
            gint           numr,
            gint           numc)
{
	gint cursize;
	gint newsize;
	gint oldr = 0;
	gint oldc = 0;
	gboolean dimschanged;

	cdn_expression_get_dimension (expression, &oldr, &oldc);

	expression->priv->cached = TRUE;

	cursize = expression->priv->cached_dims[0] * expression->priv->cached_dims[1];
	newsize = numr * numc;

	if (newsize == 1)
	{
		if (cursize != 1)
		{
			g_free (expression->priv->cached_output_multi);
		}

		expression->priv->cached_output = values ? *values : 0;
	}
	else
	{
		if (cursize != newsize)
		{
			if (cursize != 1)
			{
				expression->priv->cached_output_multi =
					g_realloc (expression->priv->cached_output_multi,
					           sizeof (gdouble) * newsize);
			}
			else
			{
				expression->priv->cached_output_multi =
					g_new (gdouble, newsize);
			}
		}

		if (expression->priv->cached_output_multi != values)
		{
			if (values)
			{
				memcpy (expression->priv->cached_output_multi,
				        values,
				        sizeof (gdouble) * newsize);
			}
			else
			{
				memset (expression->priv->cached_output_multi, 0, sizeof (gdouble) * newsize);
			}
		}
	}

	expression->priv->cached_dims[0] = numr;
	expression->priv->cached_dims[1] = numc;

	dimschanged = (numr != oldr || numc != oldc);

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
		              (expression->priv->cached_dims[0] !=
		               expression->priv->retdims[0] ||
		               expression->priv->cached_dims[1] !=
		               expression->priv->retdims[1]);

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

			set_values (self, &v, 1, 1);
			self->priv->prevent_cache_reset = TRUE;
		}
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

	cdn_stack_init (&(self->priv->output), 0);

	self->priv->cached_dims[0] = 0;
	self->priv->cached_dims[1] = 0;
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

		consume = smanip->num_pop;

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

static gint *
get_argdim (CdnExpression *expression,
            ParserContext *context,
            gint           numargs)
{
	gint *ret = NULL;
	GSList *stack;
	gint i;

	stack = context->stack;

	ret = g_new (gint, numargs * 2);

	for (i = 0; i < numargs; ++i)
	{
		CdnInstruction *instr = g_slist_last (stack->data)->data;
		CdnStackManipulation const *manip;

		manip = cdn_instruction_get_stack_manipulation (instr, NULL);

		ret[i * 2] = manip->push_dims ? manip->push_dims[0] : 1;
		ret[i * 2 + 1] = manip->push_dims ? manip->push_dims[1] : 1;

		stack = g_slist_next (stack);
	}

	return ret;
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

static CdnInstruction *
zeros_macro (CdnExpression *expression,
             ParserContext *context)
{
	GSList *second = NULL;
	GSList *first = NULL;
	gint numr;
	gint numc;
	gint i;
	gint n;

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

	numr = (gint)rint (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (first->data)));
	numc = (gint)rint (cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (second->data)));

	n = numr * numc;

	instructions_pop (expression);
	instructions_pop (expression);

	g_slist_free (context->stack->next->data);
	g_slist_free (context->stack->data);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	// Add zeros and insert instruction for matrix
	if (n != 1)
	{
		gint *dims;

		dims = g_new0 (gint, n * 2);

		for (i = 0; i < n; ++i)
		{
			dims[i * 2] = 1;
			dims[i * 2 + 1] = 1;

			instructions_push (expression,
			                   cdn_instruction_number_new_from_string ("0"),
			                   context);
		}

		return cdn_instruction_matrix_new (n,
		                                   dims,
		                                   numr,
		                                   numc);
	}
	else
	{
		return cdn_instruction_number_new_from_string ("0");
	}
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
		gint *dims;
		gint i = 0;

		dims = g_new0 (gint, num * num * 2);

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

				dims[i] = 1;
				dims[i + 1] = 1;

				i += 2;
			}
		}

		return cdn_instruction_matrix_new (num * num,
		                                   dims,
		                                   num,
		                                   num);
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
	gint *argdim;
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

	argdim = get_argdim (expression, context, 1);

	while (first)
	{
		instructions_pop (expression);
		first = g_slist_delete_link (first, first);
	}

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	s = g_strdup_printf ("%d", argdim[0] * argdim[1]);

	g_free (argdim);

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
	gint *argdim;
	gint *dims;

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

	argdim = get_argdim (expression, context, 1);

	while (first)
	{
		instructions_pop (expression);
		first = g_slist_delete_link (first, first);
	}

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	sr = g_strdup_printf ("%d", argdim[0]);
	sc = g_strdup_printf ("%d", argdim[1]);

	g_free (argdim);

	instructions_push (expression,
	                   cdn_instruction_number_new_from_string (sr),
	                   context);

	instructions_push (expression,
	                   cdn_instruction_number_new_from_string (sc),
	                   context);

	g_free (sr);
	g_free (sc);

	dims = g_new (gint, 4);

	dims[0] = 1;
	dims[1] = 1;
	dims[2] = 1;
	dims[3] = 1;

	return cdn_instruction_matrix_new (2,
	                                   dims,
	                                   1,
	                                   2);
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

static GSList *
pop_stack (CdnExpression *expression,
           ParserContext *context)
{
	GSList *instrs;
	GSList *tmp;
	GSList *nth;

	instrs = g_slist_last (context->stack)->data;

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
	                                0,
	                                NULL,
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

static gchar *
decompose_dot (gchar const *name,
               gint        *order)
{
	gunichar next;

	next = g_utf8_get_char (g_utf8_next_char (name));

#if GLIB_MINOR_VERSION >= 30
	gunichar a;
	gunichar b;

	if (g_unichar_decompose (g_utf8_get_char (name), &a, &b) &&
	    (b == 775 || b == 776))
	{
		GString *dc;

		dc = g_string_sized_new (strlen (name));
		g_string_append_unichar (dc, a);
		g_string_append (dc, g_utf8_next_char (name));

		if (b == 775)
		{
			*order = 1;
		}
		else
		{
			*order = 2;
		}

		return g_string_free (dc, FALSE);
	}
	else
#endif
	if (next == 775 || next == 776)
	{
		GString *dc;

		dc = g_string_sized_new (strlen (name));
		g_string_append_unichar (dc, g_utf8_get_char (name));
		g_string_append (dc, g_utf8_next_char (g_utf8_next_char (name)));

		if (next == 775)
		{
			*order = 1;
		}
		else
		{
			*order = 2;
		}

		return g_string_free (dc, FALSE);
	}

	return NULL;
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
			dotname = decompose_dot (name, &order);
		}

	}
	else
	{
		function = parse_context_function (expression, name, cname, context);

		if (!function)
		{
			dotname = decompose_dot (cname, &order);
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

	if ((function != NULL && (numargs > (arguments - n_implicit) || numargs < (arguments - n_implicit - n_optional))) ||
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

			while (instrs)
			{
				instructions_push (expression,
				                   cdn_mini_object_copy (instrs->data),
				                   context);

				instrs = g_slist_next (instrs);
			}

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
			gint *argdim;

			argdim = get_argdim (expression, context, numargs);

			instruction = cdn_instruction_rand_new (numargs, argdim);
			g_free (argdim);
		}
		else
		{
			gint *argdim;

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
					argdim = get_argdim (expression,
					                     context,
					                     numargs);

					instruction =
						cdn_instruction_function_new (fid,
						                              name,
						                              numargs,
						                              argdim);

					g_free (argdim);
				break;
			}
		}
	}
	else if (ret)
	{
		gint *argdim;

		argdim = get_argdim (expression, context, arguments);

		function = cdn_function_for_dimension (function,
		                                       arguments,
		                                       argdim);

		instruction = cdn_instruction_custom_function_new (function,
		                                                   arguments,
		                                                   argdim);

		g_free (argdim);

		// Recursively compile the function here
		ret = recurse_compile (expression, context, function);

		g_object_unref (function);
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
                  gint          *numr,
                  gint          *numc,
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
	*numr = 0;
	*numc = 0;

	while (TRUE)
	{
		gint *argdim;

		if (*numr > 1 && *numpop > 1)
		{
			// Here we will need to concat the rows with a concat
			// operation, so we will insert a matrix operation here
			// if needed
			gint *popdims;

			popdims = get_argdim (expression, context, *numpop);

			// Note that the popdims memory is consumed by the
			// matrix instruction and does not need to be freed
			instructions_push (expression,
			                   cdn_instruction_matrix_new (*numpop,
			                                               popdims,
			                                               *numr,
			                                               *numc),
			                   context);

			*numpop = 1;
		}

		if (!parse_expression (expression, context, -1, 0))
		{
			return FALSE;
		}

		argdim = get_argdim (expression, context, 1);
		++*numpop;

		// Check argument consistency
		if (*numr != 0)
		{
			if (*numr != argdim[0])
			{
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
				               "Cannot concatenate %d row%s with %d row%s",
				               *numr, *numr > 1 ? "s" : "", argdim[0], argdim[0] > 1 ? "s" : "");

				return FALSE;
			}
			else if (*numr != 1)
			{
				gint dims[4] = {*numr, *numc, argdim[0], argdim[1]};

				// Here we have multiple rows concatenated with
				// multiple rows. We are going to implement this
				// using a hcat operator
				instructions_push (expression,
				                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_HCAT,
				                                                 "hcat",
				                                                 2,
				                                                 dims),
				                   context);

				*numpop = 1;
			}

			*numc += argdim[1];
		}
		else
		{
			*numr = argdim[0];
			*numc = argdim[1];
		}

		g_free (argdim);

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
	gint tnumr = 0;
	gint tnumc = 0;
	gint tnumpop = 0;
	gint *popdims;

	while (TRUE)
	{
		gboolean isend = TRUE;
		gint numr = 0;
		gint numc = 0;
		gint numpop = 0;

		if (!parse_matrix_row (expression,
		                       context,
		                       &isend,
		                       &numr,
		                       &numc,
		                       &numpop))
		{
			return FALSE;
		}

		if (tnumr == 0)
		{
			tnumr = numr;
			tnumc = numc;
			tnumpop = numpop;
		}
		else if (tnumc != numc)
		{
			return parser_failed (expression,
			                      context,
			                      CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			                      "Cannot concatenate %d column%s with %d column%s",
			                      tnumc, tnumc > 1 ? "s" : "", numc, numc > 1 ? "s" : "");
		}
		else
		{
			tnumpop += numpop;
			tnumr += numr;
		}

		if (isend)
		{
			break;
		}
	}

	popdims = get_argdim (expression, context, tnumpop);

	// note that popdims memory is consumed by the matrix instruction
	// and does not need to be freed
	instructions_push (expression,
	                   cdn_instruction_matrix_new (tnumpop,
	                                               popdims,
	                                               tnumr,
	                                               tnumc),
	                   context);

	return TRUE;
}

static gboolean
parse_indexing (CdnExpression *expression,
                ParserContext *context)
{
	gint *argdim;
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

	argdim = get_argdim (expression, context, numargs + 1);

	instructions_push (expression,
	                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_INDEX,
	                                                 "index",
	                                                 numargs + 1,
	                                                 argdim),
	                   context);

	g_free (argdim);

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

		if (!ret)
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
	gint *argdim;
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

	argdim = get_argdim (expression, context, num_arguments);

	op = cdn_operators_instantiate (name,
	                                (GSList const **)exprs,
	                                num_exprs,
	                                (GSList const **)inds,
	                                num_inds,
	                                num_arguments,
	                                argdim,
	                                context->context,
	                                &err);

	g_free (argdim);

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
		}
	}

	if (newnum > num_arguments)
	{
		gint *argdim;

		argdim = get_argdim (expression, context, newnum);
		_cdn_operator_set_num_arguments (op, newnum, argdim);
		g_free (argdim);
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
	gint *argdim;

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

	argdim = get_argdim (expression, context, 3);

	instructions_push (expression,
	                   cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TERNARY,
	                                                 "?:",
	                                                 3,
	                                                 argdim),
	                   context);

	g_free (argdim);

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
	gint *argdim = NULL;

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
		case CDN_TOKEN_OPERATOR_TYPE_TILDE:
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
		ret = parse_expression (expression, context, 1000, 1);
	}

	argdim = get_argdim (expression, context, 1);

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
				                                     "-",
				                                     1,
				                                     argdim);
			}
		}
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
		break;
		case CDN_TOKEN_OPERATOR_TYPE_NEGATE:
		{
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_NEGATE,
			                                     "!",
			                                     1,
			                                     argdim);
		}
		break;
		case CDN_TOKEN_OPERATOR_TYPE_TILDE:
		{
			if (!argdim || (argdim[0] * argdim[1]) != 3)
			{
				parser_failed (expression,
				               context,
				               CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
				               "Skew symmetric matrix operator (~) is only defined for vectors 1-by-3 (got %d-by-%d)", argdim ? argdim[0] : 1, argdim ? argdim[1] : 1);

				ret = FALSE;
			}
			else
			{
				inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TILDE,
				                                     "~",
				                                     1,
				                                     argdim);
			}
		}
		break;
		default:
		break;
	}

	g_free (argdim);

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
	                                0,
	                                NULL,
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
	gint *argdim;

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "The transpose operator can only appear after an expression");

		return FALSE;
	}

	argdim = get_argdim (expression, context, 1);

	ret = instructions_push (expression,
	                         cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TRANSPOSE,
	                                                       "transpose",
	                                                       1,
	                                                       argdim),
	                         context);

	g_free (argdim);
	return ret;
}

static gboolean
parse_square (CdnExpression *expression,
              ParserContext *context)
{
	GSList *instr;
	gboolean ret;
	gint *argdim;

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (expression,
		               context,
		               CDN_COMPILE_ERROR_INVALID_TOKEN,
		               "The transpose operator can only appear after an expression");

		return FALSE;
	}

	argdim = get_argdim (expression, context, 1);

	ret = instructions_push (expression,
	                         cdn_instruction_number_new_from_string ("2"),
	                         context);

	g_free (argdim);

	if (!ret)
	{
		return FALSE;
	}

	argdim = get_argdim (expression, context, 2);

	ret = instructions_push (expression,
	                         cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                       "^",
	                                                       2,
	                                                       argdim),
	                         context);

	g_free (argdim);
	return ret;
}

static gboolean
parse_operator (CdnExpression *expression,
                CdnToken      *token,
                ParserContext *context)
{
	CdnTokenOperator *op = CDN_TOKEN_OPERATOR (token);
	gint *argdim;

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

	argdim = get_argdim (expression, context, 2);

	switch (op->type)
	{
		// arithmetic
		case CDN_TOKEN_OPERATOR_TYPE_MULTIPLY:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MULTIPLY, "*", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_DIVIDE:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_DIVIDE, "/", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MODULO:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MODULO, "%", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_PLUS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS, "+", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS, "-", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_POWER:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER, "^", 2, argdim);
		break;
		// logical
		case CDN_TOKEN_OPERATOR_TYPE_GREATER:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_GREATER, ">", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LESS, "<", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL, ">=", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL, "<=", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_EQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_EQUAL, "==", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_NEQUAL:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_NEQUAL, "!=", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_OR:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_OR, "||", 2, argdim);
		break;
		case CDN_TOKEN_OPERATOR_TYPE_AND:
			inst = cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_AND, "&&", 2, argdim);
		break;
		default:
			g_free (argdim);
			return FALSE;
		break;
	}

	g_free (argdim);
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

	dotname = decompose_dot (nname, &order);

	if (dotname && !property && (!f || !prio))
	{
		dotted = TRUE;

		property = lookup_variable (expression,
		                            context,
		                            dotname);
	}

	if (f && (prio || (!prio && !property)))
	{
		ret = recurse_compile (expression, context, f);

		if (ret)
		{
			instructions_push (expression,
			                   cdn_instruction_custom_function_ref_new (f),
			                   context);
		}
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
                              gint                       *tmpspace)
{
	gint ret = 0;
	gint i;

	if (smanip->pop_dims)
	{
		for (i = 0; i < smanip->num_pop; ++i)
		{
			ret -= smanip->pop_dims[i * 2] * smanip->pop_dims[i * 2 + 1];
		}
	}
	else
	{
		ret -= smanip->num_pop;
	}

	if (smanip->push_dims)
	{
		for (i = 0; i < smanip->num_push; ++i)
		{
			ret += smanip->push_dims[i * 2] * smanip->push_dims[i * 2 + 1];
		}
	}
	else
	{
		ret += smanip->num_push;
	}

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
	gint numr;
	gint numc;
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

	numr = expression->priv->retdims[0];
	numc = expression->priv->retdims[1];

#ifdef PRINT_STACK
	g_message ("\n\nValidating stack for: %s", expression->priv->expression);
#endif

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CdnInstruction *inst = item->data;
		GSList *deps;
		GSList *dep;
		CdnStackManipulation const *smanip;
		GError *error = NULL;
		gint nst;

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
		g_message ("%s", cdn_instruction_to_string (inst));
#endif

		nst = calculate_stack_manipulation (smanip, &tmpspace);

#ifdef PRINT_STACK
		g_message ("%s", cdn_instruction_to_string (inst));

		gint i;

		for (i = 0; i < smanip->num_pop; ++i)
		{
			g_message ("  -(%d, %d)",
			           smanip->pop_dims ? smanip->pop_dims[i * 2] : 1,
			           smanip->pop_dims ? smanip->pop_dims[i * 2 + 1] : 1);
		}

		for (i = 0; i < smanip->num_push; ++i)
		{
			g_message ("  +(%d, %d)",
			           smanip->push_dims ? smanip->push_dims[i * 2] : 1,
			           smanip->push_dims ? smanip->push_dims[i * 2 + 1] : 1);
		}

		g_message ("Stack size is now: %d (+%d)", stack, tmpspace + nst);
#endif

		if (smanip->push_dims)
		{
			expression->priv->retdims[0] = smanip->push_dims[0];
			expression->priv->retdims[1] = smanip->push_dims[1];
		}
		else
		{
			expression->priv->retdims[0] = 1;
			expression->priv->retdims[1] = 1;
		}

		if (stack + nst <= 0)
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
	    numr == expression->priv->retdims[0] &&
	    numc == expression->priv->retdims[1])
	{
		// Do nothing
		return TRUE;
	}

	if (stack != expression->priv->retdims[0] * expression->priv->retdims[1])
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
	gint oldr;
	gint oldc;

	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), FALSE);

	if (!expression->priv->modified)
	{
		return TRUE;
	}

	cdn_expression_get_dimension (expression, &oldr, &oldc);

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

		// Validate the stack here
		if (!validate_stack (expression, &ctx, FALSE))
		{
			instructions_free (expression);

			return parser_failed (expression,
			                      &ctx,
			                      CDN_COMPILE_ERROR_INVALID_STACK,
			                      "Invalid stack produced. This usually indicates a problem in the parser");
		}
	}

	reset_cache (expression, oldr != expression->priv->retdims[0] ||
	                         oldc != expression->priv->retdims[1]);

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

	cdn_expression_set_instructions_take (expression, copy);

	g_slist_foreach (copy, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (copy);
}

/**
 * cdn_expression_set_instructions_take:
 * @expression: a #CdnExpression.
 * @instructions: a #GSList.
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
	gint oldr = 0;
	gint oldc = 0;
	gboolean dimschanged;

	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	if (expression->priv->instructions == instructions)
	{
		return;
	}

	cdn_expression_get_dimension (expression, &oldr, &oldc);

	instructions_free (expression);
	cdn_stack_destroy (&(expression->priv->output));

	expression->priv->instructions = g_slist_copy (instructions);
	g_slist_foreach (expression->priv->instructions, (GFunc)cdn_mini_object_ref, NULL);

	// Validate the stack here
	validate_stack (expression, NULL, FALSE);

	// We are going to reset the expression completely here
	expression->priv->prevent_cache_reset = FALSE;

	dimschanged = (oldr != expression->priv->retdims[0] ||
	               oldc != expression->priv->retdims[1]);

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
	set_values (expression, &value, 1, 1);
	expression->priv->prevent_cache_reset = TRUE;

}

void
cdn_expression_set_values (CdnExpression *expression,
                           gdouble const *values,
                           gint           numr,
                           gint           numc)
{
	set_values (expression, values, numr, numc);
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
	gdouble const *ret;

	ret = cdn_expression_evaluate_values (expression,
	                                      NULL,
	                                      NULL);

	return ret ? *ret : 0.0;
}

static gdouble const *
values_from_cache (CdnExpression *expression,
                   gint          *numr,
                   gint          *numc)
{
	if (numr)
	{
		*numr = expression->priv->cached_dims[0];
	}

	if (numc)
	{
		*numc = expression->priv->cached_dims[1];
	}

	if (expression->priv->cached_dims[0] == 1 &&
	    expression->priv->cached_dims[1] == 1)
	{
		return &(expression->priv->cached_output);
	}
	else
	{
		return expression->priv->cached_output_multi;
	}
}

static void
set_cache_from_stack (CdnExpression *expression)
{
	set_values (expression,
	            cdn_stack_ptr (&(expression->priv->output)),
	            expression->priv->retdims[0],
	            expression->priv->retdims[1]);
}

/**
 * cdn_expression_evaluate_values: (skip)
 * @expression: a #CdnExpression
 *
 * Returns: the result of evaluating the expression
 *
 **/
gdouble const *
cdn_expression_evaluate_values (CdnExpression *expression,
                                gint          *numr,
                                gint          *numc)
{
	/* Omit type check to increase speed */
	if (!expression)
	{
		return NULL;
	}

	if (expression->priv->cached)
	{
		return values_from_cache (expression, numr, numc);
	}

	if (expression->priv->evaluate_notify)
	{
		expression->priv->evaluate_notify (expression,
		                                   expression->priv->evaluate_userdata);

		if (expression->priv->cached)
		{
			return values_from_cache (expression, numr, numc);
		}
	}

	GSList *item;
	CdnStack *stack = &(expression->priv->output);

	cdn_stack_reset (stack);

	if (numr)
	{
		*numr = 0;
	}

	if (numc)
	{
		*numc = 0;
	}

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

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		cdn_instruction_execute (item->data, stack);
	}

	if (cdn_stack_count (&(expression->priv->output)) !=
	    expression->priv->retdims[0] * expression->priv->retdims[1])
	{
		g_warning ("Invalid output stack after evaluating: `%s' (expected %d but got %d)",
		           expression->priv->expression,
		           expression->priv->retdims[0] * expression->priv->retdims[1],
		           cdn_stack_count (&(expression->priv->output)));

		return NULL;
	}

	set_cache_from_stack (expression);

	if (expression->priv->has_cache)
	{
		expression->priv->cached = TRUE;
	}

	return values_from_cache (expression, numr, numc);
}

/**
 * cdn_expression_evaluate_values_flat:
 * @expression: a #CdnExpression.
 * @num: (out caller-allocates): return value of the number of values.
 *
 * Get the values of the expression as a flat array. This is only really
 * useful for bindings because #cdn_expression_evaluate_values is difficult
 * to bind with gobject introspection.
 *
 * Returns: (array length=num) the expression values.
 *
 **/
gdouble const *
cdn_expression_evaluate_values_flat (CdnExpression *expression,
                                     gint          *num)
{
	gint numr;
	gint numc;
	gdouble const *ret;

	ret = cdn_expression_evaluate_values (expression, &numr, &numc);

	if (num)
	{
		*num = numr * numc;
	}

	return ret;
}

static void
reset_cache (CdnExpression *expression,
             gboolean       dimschanged)
{
	if (expression->priv->prevent_cache_reset)
	{
		return;
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
	}
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
	reset_cache (expression, expression->priv->cached &&
	                         (expression->priv->retdims[0] !=
	                          expression->priv->cached_dims[0] ||
	                          expression->priv->retdims[1] !=
	                          expression->priv->cached_dims[1]));

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
 * Returns: (element-type CdnInstruction) (transfer none): list of #CdnInstruction
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

	ret->priv->cached_dims[0] = expression->priv->cached_dims[0];
	ret->priv->cached_dims[1] = expression->priv->cached_dims[1];

	ret->priv->retdims[0] = expression->priv->retdims[0];
	ret->priv->retdims[1] = expression->priv->retdims[1];

	cdn_stack_resize (&ret->priv->output, expression->priv->output.size);
	cdn_stack_reset (&ret->priv->output);

	if (ret->priv->cached_dims[0] == 1 && ret->priv->cached_dims[1] == 1)
	{
		ret->priv->cached_output = expression->priv->cached_output;
	}
	else
	{
		gint num = ret->priv->cached_dims[0] * ret->priv->cached_dims[1];
		gint i;

		ret->priv->cached_output_multi = g_new (gdouble, num);

		for (i = 0; i < num; ++i)
		{
			ret->priv->cached_output_multi[i] =
				expression->priv->cached_output_multi[i];
		}
	}

	ret->priv->modified = expression->priv->modified;
	ret->priv->has_cache = expression->priv->has_cache;
	ret->priv->once = expression->priv->once;

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

gboolean
cdn_expression_get_dimension (CdnExpression *expression,
                              gint          *numr,
                              gint          *numc)
{
	if (expression->priv->cached)
	{
		if (numr)
		{
			*numr = expression->priv->cached_dims[0];
		}

		if (numc)
		{
			*numc = expression->priv->cached_dims[1];
		}

		return TRUE;
	}

	if (numr)
	{
		*numr = expression->priv->retdims[0];
	}

	if (numc)
	{
		*numc = expression->priv->retdims[1];
	}

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

gdouble *
cdn_expression_get_cache (CdnExpression *expression,
                          gint          *numr,
                          gint          *numc)
{
	if ((expression->priv->retdims[0] > 1 ||
	     expression->priv->retdims[1] > 1))
	{
		if ((expression->priv->cached_dims[0] <= 1 &&
		     expression->priv->cached_dims[1] <= 1) ||
		    !expression->priv->cached_output_multi)
		{
			expression->priv->cached_output_multi =
				g_new0 (gdouble,
				        expression->priv->retdims[0] *
				        expression->priv->retdims[1]);

			expression->priv->cached_dims[0] =
				expression->priv->retdims[0];

			expression->priv->cached_dims[1] =
				expression->priv->retdims[1];
		}

		if (numr)
		{
			*numr = expression->priv->cached_dims[0];
		}

		if (numc)
		{
			*numc = expression->priv->cached_dims[1];
		}

		return expression->priv->cached_output_multi;
	}

	if (numr)
	{
		*numr = 0;
	}

	if (numc)
	{
		*numc = 0;
	}

	return NULL;
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
