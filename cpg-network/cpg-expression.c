/*
 * cpg-expression.c
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

#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-tokenizer.h"
#include "cpg-math.h"
#include "cpg-debug.h"
#include "cpg-ref-counted-private.h"
#include "cpg-compile-error.h"
#include "cpg-function.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>

/**
 * SECTION:expression
 * @short_description: Mathematical expression evaluation
 *
 * A #CpgExpression contains a mathematical expression. The expression in
 * string format can be compiled and evaluated. At the compilation phase,
 * a list of #CpgObject is provided as a context in which variables are mapped
 * to #CpgProperty in this context.
 *
 */

enum
{
	CPG_EXPRESSION_FLAG_NONE = 0,
	CPG_EXPRESSION_FLAG_CACHED = 1 << 0,
	CPG_EXPRESSION_FLAG_INSTANT = 1 << 1
};

struct _CpgExpression
{
	CpgRefCounted parent;

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

static int parse_expression (CpgExpression *expression, ParserContext *context, gint priority, gint left_assoc);

GType
cpg_expression_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
		type_id = g_boxed_type_register_static ("CpgExpression", 
		                                        cpg_ref_counted_ref, 
		                                        cpg_ref_counted_unref);
	
	return type_id;
}

static CpgInstruction *
cpg_instruction_initialize (CpgInstruction *instruction)
{
	return instruction;
}

#define instruction_new(Type) ((Type *)cpg_instruction_initialize ((CpgInstruction *)g_slice_new0 (Type)))

/**
 * cpg_instruction_copy:
 * @instruction: a #CpgInstruction
 *
 * Create a copy of a #CpgInstruction
 *
 * Return value: a new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_copy (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *property = (CpgInstructionProperty *)instruction;
			return cpg_instruction_property_new (property->property, property->binding);
		}
		break;
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		{
			CpgInstructionFunction *function = (CpgInstructionFunction *)instruction;
			return cpg_instruction_function_new (function->id,
			                                     function->name,
			                                     function->arguments,
			                                     function->variable);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *function = (CpgInstructionFunction *)instruction;
			return cpg_instruction_operator_new (function->id,
			                                     function->name,
			                                     function->arguments);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
		{
			CpgInstructionNumber *number = (CpgInstructionNumber *)instruction;
			return cpg_instruction_number_new (number->value);
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *function = (CpgInstructionCustomFunction *)instruction;
			return cpg_instruction_custom_function_new (function->function,
			                                            function->arguments);
		}
		break;
		default:
			return NULL;
		break;
	}
}

/**
 * cpg_instruction_function_new:
 * @id: function id
 * @name: function name
 * @arguments: the number of arguments this function takes
 * @variable: if the number of arguments is variable for this function
 *
 * Creates a new function call instruction. The id @id of the instruction
 * specifies which function the instruction represents (see 
 * #CpgMathFunctionType). If @variable is %TRUE, the instruction expects the 
 * first value on the stack, when the instruction is executed, to be the number 
 * of arguments to process.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_function_new (guint         id,
                              gchar const  *name,
                              gint          arguments,
                              gboolean      variable)
{
	CpgInstructionFunction *res = instruction_new (CpgInstructionFunction);

	res->parent.type = CPG_INSTRUCTION_TYPE_FUNCTION;
	res->id = id;
	res->name = g_strdup (name);
	res->arguments = arguments;
	res->variable = variable;
	
	return (CpgInstruction *)res;
}

/**
 * cpg_instruction_number_new:
 * @value: the numeric value
 *
 * Creates a new number instruction. This is the most basic instruction, which
 * simply pushes a number onto the stack when executed.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_number_new (gdouble value)
{
	CpgInstructionNumber *res = instruction_new (CpgInstructionNumber);
	res->parent.type = CPG_INSTRUCTION_TYPE_NUMBER;
	res->value = value;
	
	return (CpgInstruction *)res;
}

/**
 * cpg_instruction_operator_new:
 * @id: operator id
 * @name: operator name
 * @arguments: the number of arguments this operator takes
 *
 * Creates a new operator instruction. The id @id of the instruction
 * specifies which operator the instruction represents (see 
 * #CpgMathOperatorType).
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_operator_new (guint         id,
                              gchar const  *name,
                              gint          arguments)
{
	CpgInstruction *res = cpg_instruction_function_new (id, name, arguments, cpg_math_operator_is_variable (id));
	res->type = CPG_INSTRUCTION_TYPE_OPERATOR;
	
	return res;
}

/**
 * cpg_instruction_property_new:
 * @property: a #CpgProperty
 * @binding: the property binding
 *
 * Creates a new property call instruction. When the instruction is executed,
 * the property value expression is evaluated and its return value is pushed
 * on the stack.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_property_new (CpgProperty           *property,
                              CpgInstructionBinding  binding)
{
	CpgInstructionProperty *res = instruction_new (CpgInstructionProperty);
	res->parent.type = CPG_INSTRUCTION_TYPE_PROPERTY;
	
	res->property = property;
	res->binding = binding;

	_cpg_property_use (property);
	cpg_ref_counted_ref (property);
	
	return (CpgInstruction *)res;
}

/**
 * cpg_instruction_custom_function_new:
 * @function: A #CpgFunction
 * @arguments: The number of arguments this function takes
 * 
 * Creates a new custom function call instruction. If the number of arguments
 * (@arguments) is not equal to the number of arguments @function takes, the
 * instruction expects the first value on the stack, when the instruction is
 * executed, to be the number of arguments to process.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_custom_function_new (CpgFunction *function,
                                     gint         arguments)
{
	CpgInstructionCustomFunction *res = instruction_new (CpgInstructionCustomFunction);

	res->parent.type = CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION;
	res->function = g_object_ref (function);
	res->arguments = arguments;

	return (CpgInstruction *)res;
}

/**
 * cpg_instruction_free:
 * @instruction: a #CpgInstruction
 *
 * Free previously created #CpgInstruction object
 *
 **/
void
cpg_instruction_free (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *prop = (CpgInstructionProperty *)instruction;

			_cpg_property_unuse (prop->property);
			cpg_ref_counted_unref (prop->property);

			g_slice_free (CpgInstructionProperty, prop);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
			g_slice_free (CpgInstructionNumber, 
			              (CpgInstructionNumber *)instruction);
		break;
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *func = (CpgInstructionFunction *)instruction;

			g_free (func->name);
			g_slice_free (CpgInstructionFunction, func);
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *func = (CpgInstructionCustomFunction *)instruction;
			g_object_unref (func->function);
			g_slice_free (CpgInstructionCustomFunction, func);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NONE:
		break;
	}
}

static void
instructions_free (CpgExpression *expression)
{
	g_slist_foreach(expression->instructions, (GFunc)cpg_instruction_free, NULL);
	g_slist_free(expression->instructions);

	expression->instructions = NULL;

	g_slist_free(expression->dependencies);
	expression->dependencies = NULL;

	// reset cached and instant flags
	expression->flags = CPG_EXPRESSION_FLAG_NONE;
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
	return expression->expression;
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
	g_free (expression->expression);
	
	expression->expression = g_strdup (value);
	instructions_free (expression);
	
	cpg_stack_destroy (&(expression->output));
}

static void
cpg_expression_free (CpgExpression *expression)
{
	instructions_free (expression);

	cpg_stack_destroy (&(expression->output));
	g_free (expression->expression);
	
	g_slice_free (CpgExpression, expression);
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
	CpgExpression *res = g_slice_new0 (CpgExpression);
	
	cpg_ref_counted_init (res, (GDestroyNotify)cpg_expression_free);

	res->expression = g_strdup (expression);
	cpg_stack_init (&(res->output), 0);

	return res;
}

static void
instructions_push (CpgExpression   *expression,
                   CpgInstruction  *next)
{
	expression->instructions = g_slist_prepend (expression->instructions, next);
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
		                      name,
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
	
	gboolean istern = CPG_TOKEN_IS_OPERATOR (next) && CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;
	
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
	if (!parse_expression (expression, context, op->priority, op->left_assoc))
	{
		cpg_token_free (next);
		return FALSE;
	}
	
	instructions_push (expression, cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_TERNARY), "?:", 3));
	return TRUE;
}

static gboolean
parse_group (CpgExpression *expression,
             ParserContext *context)
{
	if (!parse_expression (expression, context, -1, 0))
		return FALSE;
	
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
		instructions_push (expression, inst);
	
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
		return FALSE;
	
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
		return FALSE;

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
		
		cpg_debug_expression ("Parsing next: (%d) %d, %s", depth, num, *context->buffer);
	
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
				if (op->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_END || op->type == CPG_TOKEN_OPERATOR_TYPE_COMMA || op->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE)
				{
					cpg_token_free (token);
					cpg_debug_expression ("Parse end group (%d): %s", depth--, *context->buffer);
					return TRUE;
				}
				
				if (num == 0)
				{
					ret = parse_unary_operator (expression, token, context);
					
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
					cpg_debug_expression ("Parse end op (%d): %s", depth--, *context->buffer);
					return TRUE;
				}
				else
				{
					ret = parse_operator (expression, token, context);
					
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
				cpg_token_free (cpg_tokenizer_next (context->buffer));

			cpg_token_free (token);
		}
		
		++num;
		
		if (ret == FALSE)
			break;
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
	if (!expression->instructions)
		instructions_push (expression, cpg_instruction_number_new (0.0));
	
	for (item = expression->instructions; item; item = g_slist_next(item))
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
				expression->dependencies = g_slist_prepend (expression->dependencies, ((CpgInstructionProperty *)inst)->property);
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
			maxstack = stack;
	}
	
	expression->dependencies = g_slist_reverse (expression->dependencies);
	
	if (stack != 1)
		return FALSE;
	
	cpg_stack_destroy (&(expression->output));
	cpg_stack_init (&(expression->output), maxstack);
	
	return TRUE;
}

static gboolean
empty_expression (CpgExpression *expression)
{
	gchar const *buffer = expression->expression;
	
	while (buffer && *buffer)
	{
		if (!g_ascii_isspace (*buffer))
			return FALSE;
		
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
	gchar *buffer = expression->expression;
	
	instructions_free (expression);
	
	cpg_stack_destroy (&(expression->output));
	expression->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
	
	ParserContext ctx = {(gchar const **)&buffer, context, error};
	gboolean ret;
	
	if (empty_expression (expression))
	{
		instructions_push (expression, cpg_instruction_number_new (0.0));
		ret = TRUE;
	}
	else
	{
		cpg_debug_expression ("Starting to parse: %s", expression->expression);
		ret = parse_expression (expression, &ctx, -1, 0);
	}

	g_slist_free (expression->dependencies);
	expression->dependencies = NULL;

	if (!ret)
	{
		instructions_free (expression);
		return FALSE;
	}
	else
	{
		// reverse instructions
		expression->instructions = g_slist_reverse (expression->instructions);

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
	instructions_free (expression);
	
	cpg_stack_destroy (&(expression->output));
	expression->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
	
	g_slist_free (expression->dependencies);
	expression->dependencies = NULL;
	
	expression->instructions = instructions;
	
	if (!expression->instructions)
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
	// this can be instant and lets also set the cache already
	expression->flags = CPG_EXPRESSION_FLAG_INSTANT | CPG_EXPRESSION_FLAG_CACHED;
	expression->cached_output = value;
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
	if (!expression)
		return 0.0;
	
	cpg_debug_evaluate ("Evaluating expression: %s", expression->expression);
		
	if (expression->flags & CPG_EXPRESSION_FLAG_CACHED)
	{
		cpg_debug_evaluate ("Returning from cached: %f", expression->cached_output);
		return expression->cached_output;
	}

	GSList *item;
	cpg_stack_reset (&(expression->output));
	
	if (expression->output.size == 0)
	{
		cpg_debug_error ("Stack size should not be 0 (%s)!", expression->expression);
		return 0.0;
	}
	
	if (!expression->instructions)
	{
		fprintf (stderr, "No instructions found, maybe the expression was not parsed?");
		return 0.0;
	}
	
	for (item = expression->instructions; item; item = g_slist_next(item))
	{
		CpgInstruction *instruction = item->data;

		switch (instruction->type)
		{
			case CPG_INSTRUCTION_TYPE_NUMBER:
				cpg_stack_push (&(expression->output), ((CpgInstructionNumber *)instruction)->value);
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
			{
				CpgInstructionProperty *property = (CpgInstructionProperty *)instruction;
				cpg_stack_push (&(expression->output), cpg_property_get_value (property->property));
			}
			break;
			case CPG_INSTRUCTION_TYPE_FUNCTION:
				cpg_math_function_execute (((CpgInstructionFunction *)instruction)->id, &(expression->output));
			break;
			case CPG_INSTRUCTION_TYPE_OPERATOR:
				cpg_math_operator_execute (((CpgInstructionFunction *)instruction)->id, &(expression->output));
			break;
			case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
				cpg_function_execute (((CpgInstructionCustomFunction *)instruction)->function, &(expression->output));
			break;
			default:
			break;
		}
	}
	
	if (cpg_stack_count (&(expression->output)) != 1)
	{
		fprintf (stderr, "Invalid output stack after evaluating: %s!\n", expression->expression);
		return NAN;
	}
	
	expression->flags |= CPG_EXPRESSION_FLAG_CACHED;
	expression->cached_output = cpg_stack_pop (&(expression->output));
	
	return expression->cached_output;
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
	if (!(expression->flags & CPG_EXPRESSION_FLAG_INSTANT))
		expression->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
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
GSList *
cpg_expression_get_dependencies (CpgExpression *expression)
{
	return expression->dependencies;
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
	expression->flags = CPG_EXPRESSION_FLAG_NONE;
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
GSList *
cpg_expression_get_instructions(CpgExpression *expression)
{
	return expression->instructions;
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
	if (!expression->instructions || !other->instructions)
	{
		return g_strcmp0 (expression->expression, other->expression) == 0;
	}
	
	if (g_slist_length (expression->instructions) != g_slist_length (other->instructions))
	{
		return FALSE;
	}
	
	// Compare instructions
	GSList *e1 = expression->instructions;
	GSList *e2 = other->instructions;
	
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

/**
 * cpg_instruction_to_string:
 * @instruction: A #CpgInstruction
 * 
 * Get the string representation of an instruction.
 *
 * Returns: The string representation of the instruction.
 *
 **/
gchar *
cpg_instruction_to_string (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		{
			CpgInstructionFunction *inst = (CpgInstructionFunction *)instruction;
			return g_strdup_printf ("FUN (%s)", inst->name);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
		{
			CpgInstructionNumber *inst = (CpgInstructionNumber *)instruction;
			return g_strdup_printf ("NUM (%f)", inst->value);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *inst = (CpgInstructionFunction *)instruction;
			return g_strdup_printf ("OP  (%s)", inst->name);
		}
		break;
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *inst = (CpgInstructionProperty *)instruction;
			return g_strdup_printf ("PRP (%s.%s)",
			                        cpg_object_get_id (cpg_property_get_object (inst->property)),
			                        cpg_property_get_name (inst->property));
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *inst = (CpgInstructionCustomFunction *)instruction;
			return g_strdup_printf ("FNC (%s)", cpg_object_get_id (CPG_OBJECT (inst->function)));
		}
		break;
		default:
			return g_strdup ("NON");
		break;
	}
}
