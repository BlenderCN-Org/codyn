#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-tokenizer.h"
#include "cpg-math.h"
#include "cpg-debug.h"
#include "cpg-ref-counted-private.h"
#include "cpg-compile-error.h"
#include "cpg-function.h"
#include "cpg-instructions.h"

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

enum
{
	CPG_EXPRESSION_FLAG_NONE = 0,
	CPG_EXPRESSION_FLAG_CACHED = 1 << 0,
	CPG_EXPRESSION_FLAG_INSTANT = 1 << 1
};

struct _CpgExpressionPrivate
{
	// Expression to evaluate
	gchar *expression;

	GSList *instructions;
	CpgStack output;

	GSList *dependencies;

	gdouble cached_output;
	guint flags;
};

typedef struct
{
	gchar const **buffer;
	CpgCompileContext *context;

	GError **error;
} ParserContext;

G_DEFINE_TYPE (CpgExpression, cpg_expression, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_EXPRESSION,
	PROP_VALUE
};

static int parse_expression (CpgExpression *expression, ParserContext *context, gint priority, gint left_assoc);

static void
instructions_free (CpgExpression *expression)
{
	g_slist_foreach (expression->priv->instructions,
	                 (GFunc)cpg_instruction_free,
	                 NULL);

	g_slist_free (expression->priv->instructions);

	expression->priv->instructions = NULL;

	g_slist_free (expression->priv->dependencies);
	expression->priv->dependencies = NULL;

	// reset cached and instant flags
	expression->priv->flags = CPG_EXPRESSION_FLAG_NONE;
}

static void
set_expression (CpgExpression *expression,
                gchar const   *value)
{
	g_free (expression->priv->expression);

	expression->priv->expression = g_strdup (value);
	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));

	g_object_notify (G_OBJECT (expression), "expression");
}

static void
set_value (CpgExpression *expression,
           gdouble        value)
{
	expression->priv->flags = CPG_EXPRESSION_FLAG_INSTANT |
	                          CPG_EXPRESSION_FLAG_CACHED;

	expression->priv->cached_output = value;

	g_object_notify (G_OBJECT (expression), "value");
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
gchar const *
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
                                gchar const   *value)
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
cpg_expression_new (gchar const *expression)
{
	return g_object_new (CPG_TYPE_EXPRESSION,
	                     "expression", expression,
	                     NULL);
}

static void
instructions_push (CpgExpression   *expression,
                   CpgInstruction  *next)
{
	expression->priv->instructions = g_slist_prepend (expression->priv->instructions,
	                                                  next);
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
		arguments = (gint)cpg_function_get_n_arguments (function);
	}

	// parse arguments
	gint numargs = 0;
	CpgToken *next = cpg_tokenizer_peek (*(context->buffer));
	gboolean loopit = TRUE;

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

		++numargs;

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

	if ((function != NULL && (numargs > arguments || numargs < arguments - n_optional)) ||
	    (function == NULL && arguments != -1 && numargs != arguments))
	{
		return parser_failed (context,
		                      CPG_COMPILE_ERROR_MAXARG,
		                      "Number of arguments (%d) for function `%s' does not match (got %d)",
		                      numargs,
		                      arguments);
	}

	if (arguments == -1 || n_optional > 0)
	{
		instructions_push (expression, cpg_instruction_number_new ((gdouble)numargs));
	}

	CpgInstruction *instruction;

	if (function == NULL)
	{
		instruction = cpg_instruction_function_new (fid,
		                                            name,
		                                            numargs,
		                                            cpg_math_function_is_variable (fid));
	}
	else
	{
		instruction = cpg_instruction_custom_function_new (function,
		                                                   numargs);
	}

	instructions_push (expression, instruction);
	return TRUE;
}

static gboolean
parse_ternary_operator (CpgExpression     *expression,
                        CpgTokenOperator  *token,
                        ParserContext     *context)
{
	if (!parse_expression (expression, context, token->priority, token->left_assoc))
		return FALSE;

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
	                                                 3));

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
		instructions_push (expression, inst);
	}

	return ret;
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

	instructions_push (expression, inst);

	return TRUE;
}

static gboolean
parse_property (CpgExpression *expression,
                gchar         *propname,
                ParserContext *context)
{
	CpgProperty *property = cpg_compile_context_lookup_property (context->context,
	                                                             propname);

	cpg_debug_expression ("Parsing property: %s", propname);

	if (!property)
	{
		cpg_debug_expression ("Property %s not found", propname);

		return parser_failed (context,
		                      CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
		                      "Property `%s' not found",
		                      propname);
	}

	instructions_push (expression, cpg_instruction_property_new (property, 0));
	return TRUE;
}

static CpgLink *
find_link (CpgCompileContext *context)
{
	GSList *objects = cpg_compile_context_get_objects (context);

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
                     CpgLink        *link)
{
	CpgProperty *property = NULL;
	CpgInstructionBinding binding = 0;

	if (strcmp (id, "from") == 0)
	{
		property = cpg_object_get_property (cpg_link_get_from (link), propname);
		binding = CPG_INSTRUCTION_BINDING_FROM;
	}
	else if (strcmp (id, "to") == 0)
	{
		property = cpg_object_get_property (cpg_link_get_to (link), propname);
		binding = CPG_INSTRUCTION_BINDING_TO;
	}

	if (!property)
	{
		return FALSE;
	}

	instructions_push (expression, cpg_instruction_property_new (property, binding));
	return TRUE;
}

static gboolean
parse_context_property (CpgExpression *expression,
                        gchar const   *id,
                        gchar const   *propid,
                        ParserContext *context)
{
	GSList *objs = cpg_compile_context_get_objects (context->context);

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
				                   cpg_instruction_property_new (prop,
				                                                 CPG_INSTRUCTION_BINDING_NONE));
				return TRUE;
			}
		}

		objs = g_slist_next (objs);
	}

	return FALSE;
}

static gboolean
parse_constant (CpgExpression  *expression,
                gchar const    *name)
{
	gboolean found = FALSE;
	gdouble val = cpg_math_constant_lookup (name, &found);

	if (!found)
	{
		return FALSE;
	}

	instructions_push (expression, cpg_instruction_number_new (val));
	return TRUE;
}

static gboolean
parse_number (CpgExpression   *expression,
              CpgTokenNumber  *token)
{
	instructions_push (expression, cpg_instruction_number_new (token->value));

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
	CpgLink *link;

	if (next && CPG_TOKEN_IS_OPERATOR (next) &&
		CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		// consume peeked group start
		cpg_token_free (cpg_tokenizer_next (context->buffer));
		ret = parse_function (expression, id, context);
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
			link = find_link (context->context);

			if (link)
			{
				ret = parse_link_property (expression,
				                           id,
				                           propid,
				                           link);
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
				               "Link property `%s' could not be found",
				               propname->text);
			}
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
			ret = parse_constant (expression, id);

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
	static gint depth = 0;

	// peek next token
	CpgToken *token;
	gboolean ret = FALSE;
	gint num = 0;

	cpg_debug_expression ("Parse begin (%d): %s", ++depth, *context->buffer);

	while ((token = cpg_tokenizer_peek (*context->buffer)))
	{
		ret = TRUE;

		cpg_debug_expression ("Parsing next: (%d) %d, %s",
		                      depth,
		                      num,
		                      *context->buffer);

		switch (token->type)
		{
			case CPG_TOKEN_TYPE_NUMBER:
				// simply push a number on the stack
				ret = parse_number (expression, CPG_TOKEN_NUMBER (token));
			break;
			case CPG_TOKEN_TYPE_IDENTIFIER:
			{
				ret = parse_identifier (expression, CPG_TOKEN_IDENTIFIER (token), context);
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
				    op->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE)
				{
					cpg_token_free (token);
					cpg_debug_expression ("Parse end group (%d): %s",
					                      depth--,
					                      *context->buffer);

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
					cpg_debug_expression ("Parse end op (%d): %s",
					                      depth--,
					                      *context->buffer);
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
	}

	if (!ret && context->error && !*context->error)
	{
		parser_failed (context,
		               CPG_COMPILE_ERROR_INVALID_TOKEN,
		               "Expected expression but got (nothing)");
	}

	cpg_debug_expression ("Parse end (%d): %s", depth--, *context->buffer);
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
		instructions_push (expression, cpg_instruction_number_new (0.0));
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CpgInstruction *inst = item->data;

		switch (inst->type)
		{
			case CPG_INSTRUCTION_TYPE_OPERATOR:
			case CPG_INSTRUCTION_TYPE_FUNCTION:
			{
				CpgInstructionFunction *i = (CpgInstructionFunction *)inst;
				stack -= i->arguments + (i->variable ? 1 : 0);

				if (stack < 0)
					return 0;

				++stack;
			}
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
				expression->priv->dependencies =
					g_slist_prepend (expression->priv->dependencies,
					                 ((CpgInstructionProperty *)inst)->property);

				++stack;
			break;
			case CPG_INSTRUCTION_TYPE_NUMBER:
				// increase stack here
				++stack;
			break;
			case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
			{
				CpgInstructionCustomFunction *i = (CpgInstructionCustomFunction *)inst;

				stack -= i->arguments + (cpg_function_get_n_optional (i->function) > 0 ? 1 : 0);

				if (stack < 0)
				{
					return 0;
				}

				++stack;
			}
			break;
			case CPG_INSTRUCTION_TYPE_NONE:
			break;
		}

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
	gchar *buffer = expression->priv->expression;

	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));
	expression->priv->flags &= ~CPG_EXPRESSION_FLAG_CACHED;

	ParserContext ctx = {(gchar const **)&buffer, context, error};
	gboolean ret;

	if (empty_expression (expression))
	{
		instructions_push (expression, cpg_instruction_number_new (0.0));
		ret = TRUE;
	}
	else
	{
		cpg_debug_expression ("Starting to parse: %s", expression->priv->expression);
		ret = parse_expression (expression, &ctx, -1, 0);
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
		expression->priv->instructions = g_slist_reverse (expression->priv->instructions);

		if (!validate_stack (expression))
		{
			instructions_free (expression);

			return parser_failed (&ctx,
			                      CPG_COMPILE_ERROR_INVALID_STACK,
			                      "Invalid stack produced. This usually indicates a problem in the parser");
		}
	}

	return TRUE;
}

/**
 * cpg_expression_set_instructions:
 * @expression: A #CpgExpression
 * @instructions: A #GSList of #CpgInstruction
 *
 * Set the instructions used to evaluate the expression. You should never have
 * to use this function. It's main purpose is for optimization of expressions
 * in cpgrawc.
 *
 * Returns: %TRUE if the new instruction set is valid, %FALSE otherwise
 *
 **/
gboolean
cpg_expression_set_instructions (CpgExpression *expression,
                                 GSList        *instructions)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), FALSE);

	instructions_free (expression);

	cpg_stack_destroy (&(expression->priv->output));
	expression->priv->flags &= ~CPG_EXPRESSION_FLAG_CACHED;

	g_slist_free (expression->priv->dependencies);
	expression->priv->dependencies = NULL;

	expression->priv->instructions = instructions;

	if (!expression->priv->instructions)
	{
		instructions_push (expression, cpg_instruction_number_new (0.0));
	}

	if (!validate_stack (expression))
	{
		return FALSE;
	}

	return TRUE;
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
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), 0);

	if (!expression)
	{
		return 0.0;
	}

	cpg_debug_evaluate ("Evaluating expression: %s", expression->priv->expression);

	if (expression->priv->flags & CPG_EXPRESSION_FLAG_CACHED)
	{
		cpg_debug_evaluate ("Returning from cached: %f", expression->priv->cached_output);
		return expression->priv->cached_output;
	}

	GSList *item;
	cpg_stack_reset (&(expression->priv->output));

	if (expression->priv->output.size == 0)
	{
		cpg_debug_error ("Stack size should not be 0 (%s)!", expression->priv->expression);
		return 0.0;
	}

	if (!expression->priv->instructions)
	{
		fprintf (stderr, "No instructions found, maybe the expression was not parsed?");
		return 0.0;
	}

	for (item = expression->priv->instructions; item; item = g_slist_next(item))
	{
		CpgInstruction *instruction = item->data;

		switch (instruction->type)
		{
			case CPG_INSTRUCTION_TYPE_NUMBER:
				cpg_stack_push (&(expression->priv->output), ((CpgInstructionNumber *)instruction)->value);
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
			{
				CpgInstructionProperty *property = (CpgInstructionProperty *)instruction;
				cpg_stack_push (&(expression->priv->output), cpg_property_get_value (property->property));
			}
			break;
			case CPG_INSTRUCTION_TYPE_FUNCTION:
				cpg_math_function_execute (((CpgInstructionFunction *)instruction)->id, &(expression->priv->output));
			break;
			case CPG_INSTRUCTION_TYPE_OPERATOR:
				cpg_math_operator_execute (((CpgInstructionFunction *)instruction)->id, &(expression->priv->output));
			break;
			case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
				cpg_function_execute (((CpgInstructionCustomFunction *)instruction)->function, &(expression->priv->output));
			break;
			default:
			break;
		}
	}

	if (cpg_stack_count (&(expression->priv->output)) != 1)
	{
		fprintf (stderr, "Invalid output stack after evaluating: %s!\n", expression->priv->expression);
		return NAN;
	}

	expression->priv->flags |= CPG_EXPRESSION_FLAG_CACHED;
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
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	if (!(expression->priv->flags & CPG_EXPRESSION_FLAG_INSTANT))
	{
		expression->priv->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
	}
}

/**
 * cpg_expression_get_dependencies:
 * @expression: a #CpgExpression
 *
 * Get a list of #CpgProperty on which the expression depends. The list is owned
 * by @expression and should not be freed or modified
 *
 * Returns: a list of #CpgProperty
 *
 **/
GSList const *
cpg_expression_get_dependencies (CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);

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
	expression->priv->flags = CPG_EXPRESSION_FLAG_NONE;
}

/**
 * cpg_expression_get_instructions:
 * @expression: a #CpgExpression
 *
 * Get list of #CpgInstruction. The list is owned by @expression and should
 * not be freed or modified
 *
 * Returns: list of #CpgInstruction
 *
 **/
GSList const *
cpg_expression_get_instructions(CpgExpression *expression)
{
	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);

	return expression->priv->instructions;
}

static gboolean
instructions_equal (CpgInstruction *i1,
                    CpgInstruction *i2)
{
	if (i1->type != i2->type)
	{
		return FALSE;
	}

	switch (i1->type)
	{
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *p1 = (CpgInstructionProperty *)i1;
			CpgInstructionProperty *p2 = (CpgInstructionProperty *)i2;

			if (p1->binding != p2->binding)
			{
				return FALSE;
			}

			return (g_strcmp0 (cpg_property_get_name (p1->property),
			                   cpg_property_get_name (p2->property)) == 0);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		case CPG_INSTRUCTION_TYPE_FUNCTION:
			return ((CpgInstructionFunction *)i1)->id ==
			       ((CpgInstructionFunction *)i2)->id;
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
			return ((CpgInstructionNumber *)i1)->value ==
			       ((CpgInstructionNumber *)i2)->value;
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
			return ((CpgInstructionCustomFunction *)i1)->function ==
			       ((CpgInstructionCustomFunction *)i2)->function;
		break;
		default:
			return FALSE;
		break;
	}
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
		if (!instructions_equal ((CpgInstruction *)e1->data, (CpgInstruction *)e2->data))
		{
			return FALSE;
		}

		e1 = g_slist_next (e1);
		e2 = g_slist_next (e2);
	}

	return TRUE;
}
