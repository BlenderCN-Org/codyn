/*
 * cpg-expression.c
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

#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-tokenizer.h"
#include "cpg-math.h"
#include "cpg-compile-error.h"
#include "cpg-function.h"
#include "cpg-stack-private.h"
#include "cpg-operators.h"
#include "cpg-property.h"
#include "cpg-expression-tree-iter.h"

#include <cpg-network/instructions/cpg-instructions.h>

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>

/**
 * SECTION:cpg-expression
 * @short_description: Mathematical expression evaluation
 *
 * A #CpgExpression contains a mathematical expression. The expression in
 * string format can be compiled and evaluated. At the compilation phase,
 * a list of #CpgObject is provided as a context in which variables are mapped
 * to #CpgProperty in this context.
 *
 */

#define CPG_EXPRESSION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_EXPRESSION, CpgExpressionPrivate))

struct _CpgExpressionPrivate
{
	// Expression to evaluate
	gchar *expression;

	GSList *instructions;
	GSList *variadic_instructions;
	GSList *operator_instructions;

	CpgStack output;

	GSList *dependencies;

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
	CpgCompileContext *context;

	GError **error;
	GSList *stack;
} ParserContext;

static void cpg_modifiable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgExpression,
                         cpg_expression,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_MODIFIABLE,
                                                cpg_modifiable_iface_init))

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_VALUE,
	PROP_HAS_CACHE,
	PROP_MODIFIED
};

static int parse_expression (CpgExpression *expression,
                             ParserContext *context,
                             gint           priority,
                             gint           left_assoc);

static void
cpg_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static void
push_error_start (CpgExpression *expression,
                  ParserContext *ctx)
{
	expression->priv->error_start = g_slist_prepend (expression->priv->error_start,
	                                                 GINT_TO_POINTER (*(ctx->buffer) - expression->priv->expression + 1));
}

static void
pop_error_start (CpgExpression *expression,
                  ParserContext *ctx)
{
	expression->priv->error_start = g_slist_delete_link (expression->priv->error_start,
	                                                     expression->priv->error_start);
}

static void
instructions_free (CpgExpression *expression)
{
	g_slist_foreach (expression->priv->instructions,
	                 (GFunc)cpg_mini_object_free,
	                 NULL);

	g_slist_free (expression->priv->instructions);
	expression->priv->instructions = NULL;

	g_slist_free (expression->priv->variadic_instructions);
	expression->priv->variadic_instructions = NULL;

	g_slist_free (expression->priv->operator_instructions);
	expression->priv->operator_instructions = NULL;

	g_slist_free (expression->priv->dependencies);
	expression->priv->dependencies = NULL;

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
set_expression (CpgExpression *expression,
                gchar const   *value)
{
	g_free (expression->priv->expression);

	expression->priv->expression = normalize_expression (value);
	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));

	g_object_notify (G_OBJECT (expression), "expression");
}

static void
set_value (CpgExpression *expression,
           gdouble        value)
{
	expression->priv->cached = TRUE;
	expression->priv->prevent_cache_reset = TRUE;

	expression->priv->cached_output = value;
}

static void
cpg_expression_finalize (GObject *object)
{
	CpgExpression *expression;

	expression = CPG_EXPRESSION (object);

	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));
	g_free (expression->priv->expression);

	G_OBJECT_CLASS (cpg_expression_parent_class)->finalize (object);
}

static void
set_has_cache (CpgExpression *expression,
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
		cpg_expression_reset_variadic (expression);
	}

	g_object_notify (G_OBJECT (expression), "has-cache");
}

static void
cpg_expression_set_property (GObject      *object,
                             guint         prop_id,
                             const GValue *value,
                             GParamSpec   *pspec)
{
	CpgExpression *self = CPG_EXPRESSION (object);

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
cpg_expression_get_property (GObject    *object,
                             guint       prop_id,
                             GValue     *value,
                             GParamSpec *pspec)
{
	CpgExpression *self = CPG_EXPRESSION (object);

	switch (prop_id)
	{
		case PROP_EXPRESSION:
			g_value_set_string (value, self->priv->expression);
		break;
		case PROP_VALUE:
			g_value_set_double (value, cpg_expression_evaluate (self));
		break;
		case PROP_HAS_CACHE:
			g_value_set_boolean (value, cpg_expression_get_has_cache (self));
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
cpg_expression_class_init (CpgExpressionClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_expression_finalize;

	object_class->get_property = cpg_expression_get_property;
	object_class->set_property = cpg_expression_set_property;

	g_type_class_add_private (object_class, sizeof(CpgExpressionPrivate));

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
cpg_expression_init (CpgExpression *self)
{
	self->priv = CPG_EXPRESSION_GET_PRIVATE (self);

	cpg_stack_init (&(self->priv->output), 0);
}

/**
 * cpg_expression_get_as_string:
 * @expression: a #CpgExpression
 *
 * Get the string representation of the expression
 *
 * Returns: the string representation of the expression
 *
 **/
const gchar *
cpg_expression_get_as_string (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);

	return expression->priv->expression;
}

/**
 * cpg_expression_set_from_string:
 * @expression: a #CpgExpression
 * @value: the value
 *
 * Set a new expression for @expression
 *
 **/
void
cpg_expression_set_from_string (CpgExpression *expression,
                                const gchar   *value)
{
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	set_expression (expression, value);
}

/**
 * cpg_expression_new:
 * @expression: an expression
 *
 * Create a new #CpgExpression containing the expression @expression
 *
 * Returns: a new #CpgExpression
 *
 **/
CpgExpression *
cpg_expression_new (const gchar *expression)
{
	return g_object_new (CPG_TYPE_EXPRESSION,
	                     "expression", expression,
	                     NULL);
}

CpgExpression *
cpg_expression_new0 ()
{
	CpgExpression *ret;

	ret = cpg_expression_new ("0");
	ret->priv->instructions = g_slist_prepend (NULL,
	                                           cpg_instruction_number_new_from_string ("0"));

	return ret;
}

static void
instructions_push (CpgExpression  *expression,
                   CpgInstruction *next,
                   ParserContext  *context)
{
	expression->priv->instructions = g_slist_prepend (expression->priv->instructions,
	                                                  next);

	if (CPG_IS_INSTRUCTION_VARIADIC_FUNCTION (next))
	{
		expression->priv->variadic_instructions =
			g_slist_prepend (expression->priv->variadic_instructions,
			                 next);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (next))
	{
		expression->priv->operator_instructions =
			g_slist_prepend (expression->priv->operator_instructions,
			                 next);
	}

	if (context)
	{
		gint cnt = cpg_instruction_get_stack_count (next);
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

static CpgInstruction *
instructions_pop (CpgExpression *expression)
{
	CpgInstruction *inst = NULL;

	if (expression->priv->instructions)
	{
		inst = expression->priv->instructions->data;
	}

	expression->priv->instructions =
		g_slist_delete_link (expression->priv->instructions,
	                             expression->priv->instructions);

	if (CPG_IS_INSTRUCTION_VARIADIC_FUNCTION (inst))
	{
		expression->priv->variadic_instructions =
			g_slist_remove (expression->priv->variadic_instructions,
			                inst);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (inst) ||
	         CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst))
	{
		expression->priv->operator_instructions =
			g_slist_remove (expression->priv->operator_instructions,
			                inst);
	}

	return inst;
}

static gboolean
parser_failed (ParserContext *context,
               gint           code,
               gchar const   *format,
               ...)
{
	if (context->error != NULL)
	{
		va_list ap;
		va_start (ap, format);

		gchar *message = g_strdup_vprintf (format, ap);
		va_end (ap);

		if (*context->error)
		{
			g_error_free (*context->error);
			*context->error = NULL;
		}

		g_set_error (context->error,
		             CPG_COMPILE_ERROR_TYPE,
		             code,
		             "%s",
		             message);

		g_free (message);
	}

	return FALSE;
}

static CpgLink *
find_link (CpgCompileContext *context)
{
	GSList const *objects = cpg_compile_context_get_objects (context);

	while (objects)
	{
		if (objects->data && CPG_IS_LINK (objects->data))
		{
			return CPG_LINK (objects->data);
		}

		objects = g_slist_next (objects);
	}

	return NULL;
}

static gboolean
parse_link_property (CpgExpression  *expression,
                     gchar const    *id,
                     gchar const    *propname,
                     CpgLink        *link,
                     ParserContext  *context)
{
	CpgProperty *property = NULL;
	CpgInstructionPropertyBinding binding = 0;

	if (strcmp (id, "from") == 0)
	{
		property = cpg_object_get_property (cpg_link_get_from (link), propname);
		binding = CPG_INSTRUCTION_PROPERTY_BINDING_FROM;
	}
	else if (strcmp (id, "to") == 0)
	{
		property = cpg_object_get_property (cpg_link_get_to (link), propname);
		binding = CPG_INSTRUCTION_PROPERTY_BINDING_TO;
	}

	if (!property)
	{
		return FALSE;
	}

	instructions_push (expression,
	                   cpg_instruction_property_new_with_binding (property,
	                                                              binding),
	                   context);

	return TRUE;
}

static gboolean
parse_context_combined_property (CpgExpression *expression,
                                 gchar const   *id,
                                 gchar const   *propid,
                                 ParserContext *context)
{
	gchar *combined;
	CpgProperty *prop;

	combined = g_strconcat (id, ".", propid, NULL);
	prop = cpg_compile_context_lookup_property (context->context, combined);
	g_free (combined);

	if (prop)
	{
		instructions_push (expression,
		                   cpg_instruction_property_new (prop),
		                   context);

		return TRUE;
	}

	return FALSE;
}

static gboolean
parse_context_property (CpgExpression *expression,
                        gchar const   *id,
                        gchar const   *propid,
                        ParserContext *context)
{
	GSList const *objs;

	objs = cpg_compile_context_get_objects (context->context);

	while (objs)
	{
		CpgObject *o = objs->data;

		if (g_strcmp0 (id, cpg_object_get_id (o)) == 0)
		{
			CpgProperty *prop;

			prop = cpg_object_get_property (o, propid);

			if (prop)
			{
				instructions_push (expression,
				                   cpg_instruction_property_new (prop),
				                   context);

				return TRUE;
			}
		}

		objs = g_slist_next (objs);
	}

	return FALSE;
}

static gboolean
parse_dot_property (CpgExpression *expression,
                    gchar const   *id,
                    gchar const   *propid,
                    ParserContext *context)
{
	CpgLink *link;
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
		parser_failed (context,
		               CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
		               "Link property `%s.%s' could not be found",
		               id, propid);
	}

	return ret;
}

static CpgProperty *
lookup_property (CpgExpression *expression,
                 ParserContext *context,
                 gchar const   *propname)
{
	CpgProperty *ret;

	ret = cpg_compile_context_lookup_property (context->context,
	                                           propname);

	return ret;
}

static gboolean
parse_function_arguments (CpgExpression *expression,
                          ParserContext *context,
                          gint          *numargs)
{
	// parse arguments
	CpgToken *next = cpg_tokenizer_peek (*(context->buffer));
	gboolean loopit = TRUE;

	if (numargs)
	{
		*numargs = 0;
	}

	if (next && CPG_TOKEN_IS_OPERATOR (next) &&
	    CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_END)
	{
		cpg_token_free (next);
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		loopit = FALSE;
	}
	else
	{
		cpg_token_free (next);
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
		next = cpg_tokenizer_peek (*(context->buffer));

		if (!next || !CPG_TOKEN_IS_OPERATOR (next))
		{
			return parser_failed (context,
			                      CPG_COMPILE_ERROR_INVALID_TOKEN,
			                      "Expected `operator', but got %s",
			                      next ? next->text : "(nothing)");
		}

		CpgTokenOperatorType type = CPG_TOKEN_OPERATOR (next)->type;

		if (type == CPG_TOKEN_OPERATOR_TYPE_GROUP_END)
		{
			cpg_token_free (next);
			cpg_token_free (cpg_tokenizer_next (context->buffer));
			break;
		}
		else if (type != CPG_TOKEN_OPERATOR_TYPE_COMMA)
		{
			parser_failed (context,
			               CPG_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected `,' but got %s",
			               next->text);
			cpg_token_free (next);
			return FALSE;
		}

		cpg_token_free (next);
		cpg_token_free (cpg_tokenizer_next (context->buffer));
	}

	return TRUE;
}

static gboolean
parse_function (CpgExpression *expression,
                gchar const   *name,
                ParserContext *context)
{
	/* Try custom function first */
	CpgFunction *function = cpg_compile_context_lookup_function (context->context,
	                                                             name);
	guint fid = 0;
	gint arguments = 0;
	gint n_optional = 0;
	gint n_implicit = 0;

	/* Try builtin function */
	if (function == NULL)
	{
		fid = cpg_math_function_lookup (name, &arguments);

		if (!fid)
		{
			return parser_failed (context,
			                      CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND,
			                      "Function %s could not be found",
			                      name);
		}
	}
	else
	{
		n_optional = (gint)cpg_function_get_n_optional (function);
		n_implicit = (gint)cpg_function_get_n_implicit (function);
		arguments = (gint)cpg_function_get_n_arguments (function);
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
		return parser_failed (context,
		                      CPG_COMPILE_ERROR_MAXARG,
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

		ar = cpg_function_get_arguments (function);

		/* Set defaults for the rest of the optional arguments on the stack */
		if (numargs < arguments - n_implicit)
		{
			start = g_list_nth ((GList *)ar, numargs);

			while (start)
			{
				CpgFunctionArgument *a;
				CpgExpression *expr;
				GSList *inst;

				a = start->data;

				if (!cpg_function_argument_get_optional (a))
				{
					break;
				}

				/* Inline the expression here */
				expr = cpg_function_argument_get_default_value (a);

				/* TODO: actually, the context is really not
				   correct here */
				if (!cpg_expression_compile (expr,
				                             context->context,
				                             context->error))
				{
					return FALSE;
				}

				for (inst = expr->priv->instructions; inst; inst = g_slist_next (inst))
				{
					instructions_push (expression,
					                   CPG_INSTRUCTION (cpg_mini_object_copy (inst->data)),
					                   context);
				}

				start = g_list_next (start);
				++numargs;
			}
		}

		start = g_list_nth ((GList *)ar, arguments - n_implicit);

		while (start)
		{
			CpgFunctionArgument *a;
			gchar const *aname;
			CpgProperty *prop;
			gchar const *ptr;

			a = start->data;

			aname = cpg_function_argument_get_name (a);

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

			prop = lookup_property (expression, context, aname);

			if (!prop)
			{
				return parser_failed (context,
				                      CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
				                      "The implicit property `%s' for function `%s' is not found",
				                      aname,
				                      name);
			}

			instructions_push (expression,
			                   cpg_instruction_property_new (prop),
			                   context);
		}
	}

	CpgInstruction *instruction;

	if (function == NULL)
	{
		if (cpg_math_function_is_constant (fid))
		{
			instruction = cpg_instruction_function_new (fid,
			                                            name,
			                                            numargs);
		}
		else
		{
			instruction = cpg_instruction_variadic_function_new (fid,
			                                                     name,
			                                                     numargs);
		}
	}
	else
	{
		instruction = cpg_instruction_custom_function_new (function,
		                                                   numargs);
	}

	instructions_push (expression, instruction, context);
	return TRUE;
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
parse_custom_operator (CpgExpression *expression,
                       gchar const   *name,
                       ParserContext *context)
{
	CpgOperatorClass *klass;
	CpgOperator *op;
	gboolean isref;
	GSList *expressions = NULL;
	GSList *multiexpr = NULL;
	gint num_arguments = 0;
	gboolean isdiff;
	CpgToken *next;
	gboolean loopit = TRUE;
	gchar const *expr_start;
	gchar const *expr_end;
	GSList *indices = NULL;
	GSList **multiret;
	CpgInstruction *instruction;
	GSList **exprs;
	gint num_exprs;
	GSList **inds;
	gint num_inds;
	gboolean islinsolve;

	klass = cpg_operators_find_class (name);

	if (klass == NULL)
	{
		return parser_failed (context,
		                      CPG_COMPILE_ERROR_OPERATOR_NOT_FOUND,
		                      "Custom operator %s could not be found",
		                      name);
	}

	// parse arguments
	next = cpg_tokenizer_peek (*(context->buffer));

	expr_start = *(context->buffer);
	expr_end = expr_start;

	multiret = &multiexpr;

	if (next && CPG_TOKEN_IS_OPERATOR (next) &&
	    CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_OPERATOR_END)
	{
		cpg_token_free (next);
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		next = cpg_tokenizer_peek (*(context->buffer));

		if (!next || !CPG_TOKEN_IS_OPERATOR (next) ||
		    CPG_TOKEN_OPERATOR (next)->type != CPG_TOKEN_OPERATOR_TYPE_OPERATOR_END)
		{
			loopit = FALSE;
		}
		else
		{
			cpg_token_free (cpg_tokenizer_next (context->buffer));
			multiret = &indices;
		}

		cpg_token_free (next);
	}
	else
	{
		cpg_token_free (next);
	}

	cpg_compile_context_save (context->context);

	isdiff = CPG_IS_OPERATOR_PDIFF_CLASS (klass) ||
	         CPG_IS_OPERATOR_DIFF_CLASS (klass);

	islinsolve = CPG_IS_OPERATOR_LINSOLVE_CLASS (klass);

	if ((isdiff || islinsolve) && multiret == &multiexpr)
	{
		cpg_compile_context_set_function_ref_priority (context->context,
		                                               TRUE);

		cpg_compile_context_set_function_arg_priority (context->context,
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
			cpg_compile_context_set_function_ref_priority (context->context,
			                                               FALSE);
		}

		while (expression->priv->instructions != start)
		{
			CpgInstruction *inst = instructions_pop (expression);

			/* This is a bit of a hack, but we are going
			   to add the function object as a final context.
			   This is mostly for the diff type operators to
			   compile properly (resolve arguments)... */
			if ((isdiff || islinsolve) && multiret == &multiexpr &&
			    (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (inst) ||
			     CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst)))
			{
				CpgFunction *func = NULL;

				if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (inst))
				{
					CpgInstructionCustomFunctionRef *f;

					f = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (inst);
					func = cpg_instruction_custom_function_ref_get_function (f);
				}
				else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (inst))
				{
					CpgInstructionCustomOperatorRef *f;
					CpgOperator *op;

					f = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (inst);
					op = cpg_instruction_custom_operator_ref_get_operator (f);

					func = cpg_operator_get_primary_function (op);
				}

				if (func)
				{
					cpg_compile_context_append_object (context->context,
					                                   CPG_OBJECT (func));
				}
			}

			newinst = g_slist_prepend (newinst, inst);
		}

		gchar *t = g_strndup (expr_start, expr_end - expr_start);
		CpgExpression *sub = cpg_expression_new (t);

		g_free (t);
		_cpg_expression_set_instructions_take (sub, newinst);

		g_object_ref_sink (sub);
		expressions = g_slist_prepend (expressions, sub);

		// see what's next
		next = cpg_tokenizer_peek (*(context->buffer));

		if (!next || !CPG_TOKEN_IS_OPERATOR (next))
		{
			g_slist_foreach (expressions, (GFunc)g_object_unref, NULL);
			g_slist_free (expressions);

			return parser_failed (context,
			                      CPG_COMPILE_ERROR_INVALID_TOKEN,
			                      "Expected `,' or `]', but got %s",
			                      next ? next->text : "(nothing)");
		}

		CpgTokenOperatorType type = CPG_TOKEN_OPERATOR (next)->type;

		if (type == CPG_TOKEN_OPERATOR_TYPE_OPERATOR_END)
		{
			cpg_token_free (next);
			cpg_token_free (cpg_tokenizer_next (context->buffer));

			if (multiret == &multiexpr)
			{
				next = cpg_tokenizer_peek (*(context->buffer));

				if (next && CPG_TOKEN_IS_OPERATOR (next) &&
				    CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_OPERATOR_START)
				{
					if (islinsolve)
					{
						cpg_compile_context_set_function_ref_priority (context->context,
						                                               FALSE);
					}

					*multiret = g_slist_prepend (*multiret,
					                             g_slist_reverse (expressions));

					expressions = NULL;
					multiexpr = g_slist_reverse (multiexpr);

					multiret = &indices;

					cpg_token_free (cpg_tokenizer_next (context->buffer));
					cpg_token_free (next);
				}
				else
				{
					cpg_token_free (next);
					break;
				}
			}
			else
			{
				break;
			}
		}
		else if (type != CPG_TOKEN_OPERATOR_TYPE_COMMA &&
		         type != CPG_TOKEN_OPERATOR_TYPE_SEMI_COLON)
		{
			g_slist_foreach (expressions, (GFunc)g_object_unref, NULL);
			g_slist_free (expressions);

			parser_failed (context,
			               CPG_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected `,' but got %s",
			               next->text);

			cpg_token_free (next);
			return FALSE;
		}

		if (type == CPG_TOKEN_OPERATOR_TYPE_SEMI_COLON)
		{
			*multiret = g_slist_prepend (*multiret,
			                             g_slist_reverse (expressions));

			expressions = NULL;
		}

		cpg_token_free (next);
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		expr_start = *(context->buffer);
	}

	*multiret = g_slist_prepend (*multiret,
	                             g_slist_reverse (expressions));

	*multiret = g_slist_reverse (*multiret);

	cpg_compile_context_restore (context->context);

	next = cpg_tokenizer_peek (*(context->buffer));

	isref = TRUE;

	if (next &&
	    CPG_TOKEN_IS_OPERATOR (next) &&
	    CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		cpg_token_free (next);
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		isref = FALSE;

		if (!parse_function_arguments (expression, context, &num_arguments))
		{
			return FALSE;
		}
	}
	else
	{
		cpg_token_free (next);
	}

	exprs = convert_2dim_slist (multiexpr, &num_exprs);
	inds = convert_2dim_slist (indices, &num_inds);

	op = cpg_operators_instantiate (name,
	                                (GSList const **)exprs,
	                                num_exprs,
	                                (GSList const **)inds,
	                                num_inds,
	                                num_arguments,
	                                context->error);

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
		CpgFunction *func;

		func = cpg_operator_get_primary_function (op);

		// We need to add implicit arguments and optional arguments
		// on the stack here
		if (func)
		{
			GList *start;
			gint extra = 0;

			start = (GList *)cpg_function_get_arguments (func);

			start = g_list_nth ((GList *)start,
			                   num_arguments);

			while (start)
			{
				CpgFunctionArgument *a;
				CpgExpression *expr;
				GSList *inst;

				a = start->data;
				start = g_list_next (start);

				if (!cpg_function_argument_get_explicit (a))
				{
					gchar const *aname;
					CpgProperty *prop;
					gchar const *ptr;

					aname = cpg_function_argument_get_name (a);
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

					prop = lookup_property (expression, context, aname);

					if (!prop)
					{
						return parser_failed (context,
						                      CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
						                      "The implicit property `%s' for function `%s' is not found",
						                      aname,
						                      name);
					}

					instructions_push (expression,
					                   cpg_instruction_property_new (prop),
					                   context);
				}
				else if (cpg_function_argument_get_optional (a))
				{
					/* Inline the expression here */
					expr = cpg_function_argument_get_default_value (a);
					++extra;

					/* TODO: actually, the context is really not
					   correct here */
					if (!cpg_expression_compile (expr,
					                             context->context,
					                             context->error))
					{
						return FALSE;
					}

					for (inst = expr->priv->instructions; inst; inst = g_slist_next (inst))
					{
						instructions_push (expression,
						                   CPG_INSTRUCTION (cpg_mini_object_copy (inst->data)),
						                   context);
					}
				}
			}

			_cpg_operator_set_num_arguments (op,
			                                 cpg_operator_get_num_arguments (op) + extra);
		}
	}

	if (isref)
	{
		instruction = cpg_instruction_custom_operator_ref_new (op);
		g_object_unref (op);
	}
	else
	{
		instruction = cpg_instruction_custom_operator_new (op);
		g_object_unref (op);
	}

	instructions_push (expression, instruction, context);

	return TRUE;
}

static gboolean
parse_ternary_operator (CpgExpression     *expression,
                        CpgTokenOperator  *token,
                        ParserContext     *context)
{
	if (!parse_expression (expression, context, token->priority, token->left_assoc))
	{
		return FALSE;
	}

	// next token should be :
	CpgToken *next = cpg_tokenizer_peek (*context->buffer);

	if (!next)
	{
		return parser_failed (context,
		                      CPG_COMPILE_ERROR_INVALID_TOKEN,
		                      "Expected `:' but got (nothing)");
	}

	gboolean istern = CPG_TOKEN_IS_OPERATOR (next) &&
	                  CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;

	if (!istern)
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected `:' but got `%s'",
		               next->text);

		cpg_token_free (next);
		return FALSE;
	}

	cpg_token_free (cpg_tokenizer_next (context->buffer));
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (next);

	// do next expression
	if (!parse_expression (expression,
	                       context,
	                       op->priority,
	                       op->left_assoc))
	{
		cpg_token_free (next);
		return FALSE;
	}

	instructions_push (expression,
	                   cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_TERNARY),
	                                                 "?:",
	                                                 3),
	                   context);

	return TRUE;
}

static gboolean
parse_group (CpgExpression *expression,
             ParserContext *context)
{
	if (!parse_expression (expression, context, -1, 0))
	{
		return FALSE;
	}

	CpgToken *next = cpg_tokenizer_peek (*context->buffer);
	gboolean groupend = next && (CPG_TOKEN_IS_OPERATOR (next) ||
	                             CPG_TOKEN_OPERATOR (next)->type != CPG_TOKEN_OPERATOR_TYPE_GROUP_END);

	if (!groupend)
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected `)' but got %s",
		               next ? next->text : "(nothing)");

		cpg_token_free (next);
		return FALSE;
	}

	cpg_token_free (next);
	cpg_token_free (cpg_tokenizer_next (context->buffer));

	return TRUE;
}

static gboolean
parse_unary_operator (CpgExpression *expression,
                      CpgToken      *token,
                      ParserContext *context)
{
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (token);
	gboolean ret = TRUE;
	CpgInstruction *inst = NULL;

	// handle group
	if (op->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		return parse_group (expression, context);
	}

	switch (op->type)
	{
		case CPG_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS), "-", 1);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_PLUS:
		break;
		case CPG_TOKEN_OPERATOR_TYPE_NEGATE:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_NEGATE), "!", 1);
		break;
		default:
			parser_failed (context,
			               CPG_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected unary operator (-, +, !) but got `%s'",
			               op->parent.text);
			ret = FALSE;
		break;
	}

	if (ret)
	{
		// consume token
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		ret = parse_expression (expression, context, 1000, 1);
	}

	if (ret && inst)
	{
		instructions_push (expression, inst, context);
	}

	return ret;
}

static gboolean
parse_prime (CpgExpression *expression,
             ParserContext *context)
{
	GSList *instr;
	CpgOperator *op;
	GSList *exprs;
	CpgExpression *expr;
	GSList *item;

	instr = context->stack ? context->stack->data : NULL;

	if (!instr)
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_INVALID_TOKEN,
		               "The prime operator can only appear after an expression");

		return FALSE;
	}

	for (item = instr; item; item = g_slist_next (item))
	{
		// Pop em
		instructions_pop (expression);
	}

	expr = cpg_expression_new0 ();
	_cpg_expression_set_instructions_take (expr, instr);

	exprs = g_slist_prepend (NULL, expr);

	context->stack = g_slist_delete_link (context->stack,
	                                      context->stack);

	op = cpg_operators_instantiate ("df_dt",
	                                (GSList const **)&exprs,
	                                1,
	                                NULL,
	                                0,
	                                0,
	                                context->error);

	g_slist_free (exprs);

	if (!op)
	{
		return FALSE;
	}

	instructions_push (expression,
	                   cpg_instruction_custom_operator_new (op),
	                   context);

	g_object_unref (op);

	return TRUE;
}

static gboolean
parse_operator (CpgExpression *expression,
                CpgToken      *token,
                ParserContext *context)
{
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (token);

	// handle ternary
	if (op->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_TRUE)
	{
		// consume token
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		return parse_ternary_operator (expression, CPG_TOKEN_OPERATOR (token), context);
	}
	else if (op->type == CPG_TOKEN_OPERATOR_TYPE_PRIME)
	{
		// consume token
		cpg_token_free (cpg_tokenizer_next (context->buffer));

		// The prime makes a df_dt operator of the current stack
		return parse_prime (expression, context);
	}

	CpgInstruction *inst = NULL;

	switch (op->type)
	{
		// arithmetic
		case CPG_TOKEN_OPERATOR_TYPE_MULTIPLY:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_MULTIPLY), "*", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_DIVIDE:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_DIVIDE), "/", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_MODULO:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_MODULO), "%", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_PLUS:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_PLUS), "+", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_MINUS:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_MINUS), "-", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_POWER:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_POWER), "**", 2);
		break;

		// logical
		case CPG_TOKEN_OPERATOR_TYPE_GREATER:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_GREATER), ">", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_LESS:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_LESS), "<", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL), ">=", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL), "<=", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_EQUAL:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_EQUAL), "==", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_OR:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_OR), "||", 2);
		break;
		case CPG_TOKEN_OPERATOR_TYPE_AND:
			inst = cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_AND), "&&", 2);
		break;
		default:
			return FALSE;
		break;
	}

	// consume token
	cpg_token_free (cpg_tokenizer_next (context->buffer));

	if (!parse_expression (expression, context, op->priority, op->left_assoc))
	{
		return FALSE;
	}

	instructions_push (expression, inst, context);

	return TRUE;
}

static gboolean
parse_property (CpgExpression *expression,
                gchar const   *propname,
                ParserContext *context)
{
	CpgProperty *property;
	CpgFunction *f;
	gboolean prio;
	gboolean ret = TRUE;

	gchar *nname = g_strdup (propname);

	if (cpg_compile_context_get_function_arg_priority (context->context))
	{
		while (TRUE)
		{
			CpgToken *next = cpg_tokenizer_peek (*context->buffer);

			if (next &&
			    CPG_TOKEN_IS_OPERATOR (next) &&
			    CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_PRIME)
			{
				gchar *tmp;
				tmp = nname;

				nname = g_strconcat (tmp, "'", NULL);
				g_free (tmp);

				cpg_token_free (next);
				cpg_token_free (cpg_tokenizer_next (context->buffer));
			}
			else
			{
				cpg_token_free (next);
				break;
			}
		}
	}

	property = lookup_property (expression, context, nname);
	f = cpg_compile_context_lookup_function (context->context, nname);
	prio = cpg_compile_context_get_function_ref_priority (context->context);

	if (f && (prio || (!prio && !property)))
	{
		instructions_push (expression,
		                   cpg_instruction_custom_function_ref_new (f),
		                   context);
	}
	else if (property)
	{
		instructions_push (expression,
		                   cpg_instruction_property_new (property),
		                   context);
	}
	else
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
		               "Property `%s' not found",
		               nname);

		ret = FALSE;
	}

	g_free (nname);

	return ret;
}

static gboolean
parse_constant (CpgExpression  *expression,
                gchar const    *name,
                ParserContext  *context)
{
	gboolean found = FALSE;

	cpg_math_constant_lookup (name, &found);

	if (!found)
	{
		return FALSE;
	}

	instructions_push (expression,
	                   cpg_instruction_constant_new (name),
	                   context);

	return TRUE;
}

static gboolean
parse_number (CpgExpression   *expression,
              CpgTokenNumber  *token,
               ParserContext  *context)
{
	instructions_push (expression,
	                   cpg_instruction_number_new (token->value),
	                   context);

	return TRUE;
}

static gboolean
parse_identifier (CpgExpression      *expression,
                  CpgTokenIdentifier *token,
                  ParserContext      *context)
{
	gchar *id = token->identifier;
	gboolean ret = FALSE;

	// consume token and peek the next to see if the identifier is a function
	// call
	cpg_token_free (cpg_tokenizer_next (context->buffer));
	CpgToken *next = cpg_tokenizer_peek (*context->buffer);

	if (next && CPG_TOKEN_IS_OPERATOR (next) &&
		CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		// consume peeked group start
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		ret = parse_function (expression, id, context);
	}
	else if (next && CPG_TOKEN_IS_OPERATOR (next) &&
		CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_OPERATOR_START)
	{
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		ret = parse_custom_operator (expression, id, context);
	}
	else if (next && CPG_TOKEN_IS_OPERATOR (next) &&
	         CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_DOT)
	{
		// consume peeked dot
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		CpgToken *propname = cpg_tokenizer_next (context->buffer);

		if (CPG_TOKEN_IS_IDENTIFIER (propname))
		{
			gchar const *propid = CPG_TOKEN_IDENTIFIER (propname)->identifier;

			ret = parse_dot_property (expression, id, propid, context);
		}
		else
		{
			parser_failed (context,
			               CPG_COMPILE_ERROR_INVALID_TOKEN,
			               "Expected identifier for property");
		}

		cpg_token_free (propname);
	}
	else
	{
		// try to parse property
		ret = parse_property (expression, id, context);

		if (!ret)
		{
			if (context->error && *(context->error))
			{
				g_error_free (*(context->error));
				*(context->error) = NULL;
			}

			// try parsing constants
			ret = parse_constant (expression, id, context);

			if (!ret)
			{
				parser_failed (context,
				               CPG_COMPILE_ERROR_INVALID_TOKEN,
				               "Could not find property or constant `%s'",
				               id);
			}
		}
	}

	cpg_token_free (next);
	return ret;
}

static gboolean
parse_expression (CpgExpression   *expression,
                  ParserContext   *context,
                  gint             priority,
                  gint             left_assoc)
{
	// peek next token
	CpgToken *token;
	gboolean ret = FALSE;
	gint num = 0;

	push_error_start (expression, context);

	while ((token = cpg_tokenizer_peek (*context->buffer)))
	{
		ret = TRUE;

		switch (token->type)
		{
			case CPG_TOKEN_TYPE_NUMBER:
				// simply push a number on the stack
				ret = parse_number (expression,
				                    CPG_TOKEN_NUMBER (token),
				                    context);
			break;
			case CPG_TOKEN_TYPE_IDENTIFIER:
			{
				ret = parse_identifier (expression,
				                        CPG_TOKEN_IDENTIFIER (token),
				                        context);

				cpg_token_free (token);
				token = NULL;
			}
			break;
			case CPG_TOKEN_TYPE_OPERATOR:
			{
				CpgTokenOperator *op = CPG_TOKEN_OPERATOR (token);

				// group end
				if (op->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_END ||
				    op->type == CPG_TOKEN_OPERATOR_TYPE_COMMA ||
				    op->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE ||
				    op->type == CPG_TOKEN_OPERATOR_TYPE_OPERATOR_END ||
				    op->type == CPG_TOKEN_OPERATOR_TYPE_SEMI_COLON)
				{
					cpg_token_free (token);

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
						cpg_token_free (token);
						token = NULL;
					}
				}
				else if (op->priority < priority ||
					 (op->priority == priority && left_assoc))
				{
					// Do not handle the operator here yet
					cpg_token_free (token);

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
						cpg_token_free (token);
						token = NULL;
					}
				}
			}
			break;
			case CPG_TOKEN_TYPE_NONE:
				parser_failed (context,
				               CPG_COMPILE_ERROR_INVALID_TOKEN,
				               "Uknown token");
				ret = FALSE;
			break;
		}

		if (token)
		{
			// consume token
			if (ret)
			{
				cpg_token_free (cpg_tokenizer_next (context->buffer));
			}

			cpg_token_free (token);
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

	if (!ret && context->error && !*context->error)
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected expression but got (nothing)");
	}

	return ret;
}

static gboolean
validate_stack (CpgExpression *expression)
{
	GSList *item;
	gint stack = 0;
	gint maxstack = 1;

	// check for empty instruction set
	if (!expression->priv->instructions)
	{
		instructions_push (expression,
		                   cpg_instruction_number_new (0.0),
		                   NULL);
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CpgInstruction *inst = item->data;
		stack += cpg_instruction_get_stack_count (inst);

		if (stack <= 0)
		{
			break;
		}

		expression->priv->dependencies =
			g_slist_concat (expression->priv->dependencies,
			                cpg_instruction_get_dependencies (inst));

		if (stack > maxstack)
		{
			maxstack = stack;
		}
	}

	expression->priv->dependencies = g_slist_reverse (expression->priv->dependencies);

	if (stack != 1)
	{
		return FALSE;
	}

	cpg_stack_destroy (&(expression->priv->output));
	cpg_stack_init (&(expression->priv->output), maxstack);

	return TRUE;
}

static gboolean
empty_expression (CpgExpression *expression)
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
 * cpg_expression_reset_variadic:
 * @expression: A #CpgExpression
 *
 * Reset the cache of the variadic functions (such as rand()). You normally
 * do not need to call this function directly.
 *
 **/
void
cpg_expression_reset_variadic (CpgExpression *expression)
{
	/* Omit type check to increase speed */
	GSList *item;

	for (item = expression->priv->variadic_instructions; item; item = g_slist_next (item))
	{
		cpg_instruction_variadic_function_reset_cache (item->data);
	}

	for (item = expression->priv->operator_instructions; item; item = g_slist_next (item))
	{
		CpgInstructionCustomOperator *op = item->data;
		cpg_operator_reset_variadic (cpg_instruction_custom_operator_get_operator (op));
	}
}

/**
 * cpg_expression_compile:
 * @expression: a #CpgExpression
 * @context: the evaluation context
 * @error: a #GError
 *
 * Compile the expression. The context is a list of #CpgObject from which
 * properties can be looked up (such as global constants, or from/to objects).
 * If there were any errors during compilation, @error will be set accordingly
 *
 * Returns: %TRUE if the expression compiled successfully, %FALSE otherwise
 *
 **/
gboolean
cpg_expression_compile (CpgExpression      *expression,
                        CpgCompileContext  *context,
                        GError            **error)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (context == NULL || CPG_IS_COMPILE_CONTEXT (context), FALSE);

	if (!expression->priv->modified)
	{
		return TRUE;
	}

	gchar *buffer = expression->priv->expression;

	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));
	ParserContext ctx = {(gchar const **)&buffer, context, error};
	gboolean ret;

	expression->priv->error_at = 0;
	expression->priv->error_start = NULL;

	if (empty_expression (expression))
	{
		instructions_push (expression,
		                   cpg_instruction_number_new (0.0),
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

	g_slist_free (expression->priv->dependencies);
	expression->priv->dependencies = NULL;

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

		expression->priv->variadic_instructions =
			g_slist_reverse (expression->priv->variadic_instructions);

		expression->priv->operator_instructions =
			g_slist_reverse (expression->priv->operator_instructions);

		if (!validate_stack (expression))
		{
			instructions_free (expression);

			return parser_failed (&ctx,
			                      CPG_COMPILE_ERROR_INVALID_STACK,
			                      "Invalid stack produced. This usually indicates a problem in the parser");
		}
	}

	expression->priv->cached = FALSE;

	if (expression->priv->modified)
	{
		expression->priv->modified = FALSE;
		g_object_notify (G_OBJECT (expression), "modified");
	}

	return TRUE;
}

/**
 * cpg_expression_set_instructions:
 * @expression: A #CpgExpression
 * @instructions: (element-type CpgInstruction) (transfer full): A #GSList of #CpgInstruction
 *
 * Set the instructions used to evaluate the expression. You should never have
 * to use this function. It's main purpose is for optimization of expressions
 * in cpgrawc.
 *
 * Returns: %TRUE if the new instruction set is valid, %FALSE otherwise
 *
 **/
void
cpg_expression_set_instructions (CpgExpression *expression,
                                 GSList const  *instructions)
{
	GSList *copy = NULL;

	while (instructions)
	{
		copy = g_slist_prepend (copy,
		                        cpg_mini_object_copy (instructions->data));

		instructions = g_slist_next (instructions);
	}

	copy = g_slist_reverse (copy);

	_cpg_expression_set_instructions_take (expression, copy);
}

void
_cpg_expression_set_instructions_take (CpgExpression *expression,
                                       GSList        *instructions)
{
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));

	expression->priv->cached = FALSE;

	if (expression->priv->modified)
	{
		expression->priv->modified = FALSE;
		g_object_notify (G_OBJECT (expression), "modified");
	}

	g_slist_free (expression->priv->dependencies);
	expression->priv->dependencies = NULL;

	expression->priv->instructions = instructions;

	if (!expression->priv->instructions)
	{
		instructions_push (expression,
		                   cpg_instruction_number_new (0.0),
		                   NULL);
	}

	GSList *item;

	for (item = expression->priv->instructions; item; item = g_slist_next (item))
	{
		CpgInstruction *inst = item->data;

		if (CPG_IS_INSTRUCTION_VARIADIC_FUNCTION (inst))
		{
			expression->priv->variadic_instructions =
				g_slist_prepend (expression->priv->variadic_instructions,
				                 inst);
		}
		else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (inst))
		{
			expression->priv->operator_instructions =
				g_slist_prepend (expression->priv->operator_instructions,
				                 inst);
		}
	}

	expression->priv->variadic_instructions =
		g_slist_reverse (expression->priv->variadic_instructions);

	expression->priv->operator_instructions =
		g_slist_reverse (expression->priv->operator_instructions);

	validate_stack (expression);
}

/**
 * cpg_expression_set_value:
 * @expression: a #CpgExpression
 * @value: a value
 *
 * Sets the cached/instant value of an expression. If the expression is
 * reset, this value will no longer be used and the expression will be
 * evaluated as normal
 *
 **/
void
cpg_expression_set_value (CpgExpression  *expression,
                          gdouble         value)
{
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	set_value (expression, value);
}

/**
 * cpg_expression_evaluate:
 * @expression: a #CpgExpression
 *
 * Get the result of evaluating the expression. If the expression is not yet
 * compiled, 0.0 is returned. The result of the evaluation is cached in
 * the expression. Make sure to call cpg_expression_reset_cache to clear the
 * cache if needed
 *
 * Returns: the result of evaluating the expression
 *
 **/
gdouble
cpg_expression_evaluate (CpgExpression *expression)
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
	CpgStack *stack = &(expression->priv->output);

	cpg_stack_reset (stack);

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
		cpg_instruction_execute (item->data, stack);
	}

	if (cpg_stack_count (&(expression->priv->output)) != 1)
	{
		fprintf (stderr,
		         "Invalid output stack after evaluating: %s!\n",
		         expression->priv->expression);

		return NAN;
	}

	if (expression->priv->has_cache)
	{
		expression->priv->cached = TRUE;
	}
	else
	{
		cpg_expression_reset_variadic (expression);
	}

	expression->priv->cached_output = cpg_stack_pop (&(expression->priv->output));

	return expression->priv->cached_output;
}

/**
 * cpg_expression_reset_cache:
 * @expression: a #CpgExpression
 *
 * Resets the possibly cached result of the value
 *
 **/
void
cpg_expression_reset_cache (CpgExpression *expression)
{
	/* Omit type check to increase speed */
	if (!expression->priv->prevent_cache_reset)
	{
		expression->priv->cached = FALSE;
	}

	GSList *item;

	for (item = expression->priv->operator_instructions; item; item = g_slist_next (item))
	{
		CpgInstructionCustomOperator *op = item->data;
		cpg_operator_reset_cache (cpg_instruction_custom_operator_get_operator (op));
	}
}

/**
 * cpg_expression_get_dependencies:
 * @expression: a #CpgExpression
 *
 * Get a list of #CpgProperty on which the expression depends. The list is owned
 * by @expression and should not be freed or modified
 *
 * Returns: (element-type CpgProperty) (transfer container): a list of #CpgProperty
 *
 **/
const GSList *
cpg_expression_get_dependencies (CpgExpression *expression)
{
	/* Omit type check to increase speed */
	return expression->priv->dependencies;
}

/**
 * cpg_expression_reset:
 * @expression: a #CpgExpression
 *
 * Resets all the expression flags (cache, instant)
 *
 **/
void
cpg_expression_reset (CpgExpression *expression)
{
	/* Omit type check to increase speed */
	expression->priv->cached = FALSE;

	if (!expression->priv->once)
	{
		expression->priv->prevent_cache_reset = FALSE;
	}

	GSList *item;

	for (item = expression->priv->operator_instructions; item; item = g_slist_next (item))
	{
		CpgInstructionCustomOperator *op = item->data;

		cpg_operator_reset (cpg_instruction_custom_operator_get_operator (op));
	}

	if (!expression->priv->modified)
	{
		expression->priv->modified = TRUE;
		g_object_notify (G_OBJECT (expression), "modified");
	}
}

/**
 * cpg_expression_get_instructions:
 * @expression: a #CpgExpression
 *
 * Get list of #CpgInstruction. The list is owned by @expression and should
 * not be freed or modified
 *
 * Returns: (element-type CpgInstruction) (transfer none): list of #CpgInstruction
 *
 **/
const GSList *
cpg_expression_get_instructions (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);

	return expression->priv->instructions;
}

/**
 * cpg_expression_equal:
 * @expression: a #CpgExpression
 * @other: a #CpgExpression
 *
 * Get whether two expressions are equal. If the expressions are compiled, they
 * are evaluated for equality by means of their instructions. Otherwise the
 * comparison is done on their string representations
 *
 * Returns: %TRUE if the expressions are equal, %FALSE otherwise
 *
 **/
gboolean
cpg_expression_equal (CpgExpression *expression,
                      CpgExpression *other)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), FALSE);
	g_return_val_if_fail (CPG_IS_EXPRESSION (other), FALSE);

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
		if (!cpg_instruction_equal (e1->data, e2->data))
		{
			return FALSE;
		}

		e1 = g_slist_next (e1);
		e2 = g_slist_next (e2);
	}

	return TRUE;
}

/**
 * cpg_expression_get_once:
 * @expression: A #CpgExpression
 *
 * Get whether the expression is only evaluated once.
 *
 * Returns: %TRUE if the expression is only evaluated once, %FALSE otherwise.
 *
 **/
gboolean
cpg_expression_get_once (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), FALSE);

	return expression->priv->once;
}

/**
 * cpg_expression_set_once:
 * @expression: A #CpgExpression
 * @instant: Whether the expression should be constant
 *
 * When an expression is "once", its value will not change.
 *
 **/
void
cpg_expression_set_once (CpgExpression *expression,
                         gboolean       once)
{
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	expression->priv->once = once;
	expression->priv->prevent_cache_reset |= once;
}

/**
 * cpg_expression_copy:
 * @expression: A #CpgExpression
 *
 * Create a copy of a #CpgExpression.
 *
 * Returns: (transfer full): A #CpgExpression
 *
 **/
CpgExpression *
cpg_expression_copy (CpgExpression *expression)
{
	CpgExpression *ret;
	GSList *instr;

	g_return_val_if_fail (expression == NULL || CPG_IS_EXPRESSION (expression), NULL);

	if (expression == NULL)
	{
		return NULL;
	}

	ret = cpg_expression_new (expression->priv->expression);

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
		                   CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instr->data))),
		                   NULL);

		instr = g_slist_next (instr);
	}

	ret->priv->instructions =
		g_slist_reverse (ret->priv->instructions);

	ret->priv->variadic_instructions =
		g_slist_reverse (ret->priv->variadic_instructions);

	ret->priv->operator_instructions =
		g_slist_reverse (ret->priv->operator_instructions);

	return ret;
}

/**
 * cpg_expression_get_error_at:
 * @expression: A #CpgExpression
 *
 * Get the character position in the expression at which an error occurred
 * while compiling the expression
 *
 * Returns: the character position at which an error occurred
 *
 **/
gint
cpg_expression_get_error_at (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), 0);

	return expression->priv->error_at;
}

/**
 * cpg_expression_get_error_start:
 * @expression: A #CpgExpression
 *
 * Get the character position in the expression at which an error started
 * while compiling the expression
 *
 * Returns: the character position at which an error started
 *
 **/
gint
cpg_expression_get_error_start (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), 0);

	return expression->priv->error_start ? GPOINTER_TO_INT (expression->priv->error_start->data) : expression->priv->error_at;
}

const GSList *
cpg_expression_get_operators (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);

	return expression->priv->operator_instructions;
}

gboolean
cpg_expression_get_has_cache (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), TRUE);

	return expression->priv->has_cache;
}

void
cpg_expression_set_has_cache (CpgExpression *expression,
                              gboolean       cache)
{
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	set_has_cache (expression, cache);
}
