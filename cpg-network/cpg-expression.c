#include "cpg-expression.h"
#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-tokenizer.h"
#include "cpg-math.h"
#include "cpg-debug.h"
#include "cpg-types.h"
#include "cpg-ref-counted-private.h"

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <glib.h>

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

static int parse_expression (CpgExpression *expression, gchar const **buffer, GSList *context, gint priority, gint left_assoc);

GType
cpg_expression_get_type ()
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
		type_id = g_boxed_type_register_static ("CpgExpression", cpg_ref_counted_ref, cpg_ref_counted_unref);
	
	return type_id;
}

static CpgInstruction *
cpg_instruction_initialize (CpgInstruction *instruction)
{
	return instruction;
}

#define instruction_new(Type) ((Type *)cpg_instruction_initialize ((CpgInstruction *)g_new (Type, 1)))

static char *
instruction_tos (CpgInstruction *inst)
{
	gchar *res = g_new (gchar, 1024);
	*res = '\0';
	
	switch (inst->type)
	{
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		{
			CpgInstructionFunction *i = (CpgInstructionFunction *)inst;
			snprintf (res, 1024, "FUNC (%s, %d)", i->name, i->arguments);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *i = (CpgInstructionFunction *)inst;
			snprintf (res, 1024, "OP (%s, %d)", i->name, i->arguments);
		}
		break;
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *i = (CpgInstructionProperty *)inst;
			snprintf (res, 1024, "PROP (%s)", cpg_property_get_name (i->property));
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
		{
			CpgInstructionNumber *i = (CpgInstructionNumber *)inst;
			snprintf (res, 1024, "NUM (%f)", i->value);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NONE:
		break;
	}
	
	return res;
}

static CpgInstruction *
cpg_instruction_function_new (guint         id,
                              gchar const  *name,
                              gint          arguments,
                              gint          vargs)
{
	CpgInstructionFunction *res = instruction_new (CpgInstructionFunction);

	res->parent.type = CPG_INSTRUCTION_TYPE_FUNCTION;
	res->id = id;
	res->name = g_strdup (name);
	res->arguments = arguments;
	res->vargs = vargs;
	
	return (CpgInstruction *)res;
}

static CpgInstruction *
cpg_instruction_number_new (gdouble value)
{
	CpgInstructionNumber *res = instruction_new (CpgInstructionNumber);
	res->parent.type = CPG_INSTRUCTION_TYPE_NUMBER;
	res->value = value;
	
	return (CpgInstruction *)res;
}

static CpgInstruction *
cpg_instruction_operator_new (guint         id,
                              gchar const  *name,
                              gint          arguments)
{
	CpgInstruction *res = cpg_instruction_function_new (id, name, arguments, 0);
	res->type = CPG_INSTRUCTION_TYPE_OPERATOR;
	
	return res;
}

static CpgInstruction *
cpg_instruction_property_new (CpgProperty *property)
{
	CpgInstructionProperty *res = instruction_new (CpgInstructionProperty);
	res->parent.type = CPG_INSTRUCTION_TYPE_PROPERTY;
	
	res->property = property;
	
	return (CpgInstruction *)res;
}

static void
cpg_instruction_free (CpgInstruction *instruction)
{
	if (instruction->type == CPG_INSTRUCTION_TYPE_FUNCTION ||
		instruction->type == CPG_INSTRUCTION_TYPE_OPERATOR)
	{
		g_free (((CpgInstructionFunction *)instruction)->name);
	}

	g_free (instruction);
}

static void
instructions_free (CpgExpression *expression)
{
	g_slist_foreach(expression->instructions, (GFunc)cpg_instruction_free, NULL);
	g_slist_free(expression->instructions);

	expression->instructions = NULL;

	g_slist_free(expression->dependencies);

	// reset cached and instant flags
	expression->flags = CPG_EXPRESSION_FLAG_NONE;
}

gchar const *
cpg_expression_get_as_string (CpgExpression *expression)
{
	return expression->expression;
}

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
	expression->instructions = g_slist_prepend(expression->instructions, next);
}

static gboolean
parse_function (CpgExpression   *expression,
                gchar const     *name,
                gchar const    **buffer,
                GSList          *context)
{
	// do function lookup
	gint arguments;
	guint id = cpg_math_function_lookup (name, &arguments);
	
	if (!id)
		return 0;
	
	// parse arguments
	gint numargs = 0;
	
	while (TRUE)
	{
		if (!parse_expression (expression, buffer, context, -1, 0))
			return 0;
		
		++numargs;

		// see what's next
		CpgToken *next = cpg_tokenizer_peek (*buffer);
		
		if (!next || !CPG_TOKEN_IS_OPERATOR (next))
			return 0;

		CpgTokenOperatorType type = CPG_TOKEN_OPERATOR (next)->type;
		cpg_token_free (next);

		if (type == CPG_TOKEN_OPERATOR_TYPE_GROUP_END)
		{
			cpg_token_free (cpg_tokenizer_next (buffer));
			break;
		}
		else if (type != CPG_TOKEN_OPERATOR_TYPE_COMMA)
		{
			return FALSE;
		}
		
		cpg_token_free (cpg_tokenizer_next (buffer));
	}
	
	if (arguments != -1 && numargs > arguments)
		return FALSE;
	
	if (arguments == -1)
		instructions_push (expression, cpg_instruction_number_new ((gdouble)numargs));

	instructions_push (expression, cpg_instruction_function_new (id, name, numargs, arguments == -1));
	return TRUE;
}

static gboolean
parse_ternary_operator (CpgExpression     *expression, 
					   CpgTokenOperator  *token, 
					   gchar const      **buffer, 
					   GSList            *context)
{
	if (!parse_expression (expression, buffer, context, token->priority, token->left_assoc))
		return FALSE;
	
	// next token should be :
	CpgToken *next = cpg_tokenizer_peek (*buffer);
	
	if (!next)
		return FALSE;
	
	gboolean istern = CPG_TOKEN_IS_OPERATOR (next) && CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;
	
	if (!istern)
	{
		cpg_token_free (next);
		return FALSE;
	}

	cpg_token_free (cpg_tokenizer_next (buffer));
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (next);
	
	// do next expression
	if (!parse_expression (expression, buffer, context, op->priority, op->left_assoc))
	{
		cpg_token_free (next);
		return FALSE;
	}
	
	instructions_push (expression, cpg_instruction_operator_new (cpg_math_operator_lookup (CPG_MATH_OPERATOR_TYPE_TERNARY), "?:", 3));
	return TRUE;
}

static gboolean
parse_group (CpgExpression   *expression,
             gchar const    **buffer,
             GSList          *context)
{
	if (!parse_expression (expression, buffer, context, -1, 0))
		return FALSE;
	
	CpgToken *next = cpg_tokenizer_peek (*buffer);
	gboolean groupend = next && (CPG_TOKEN_IS_OPERATOR (next) ||
				   CPG_TOKEN_OPERATOR (next)->type != CPG_TOKEN_OPERATOR_TYPE_GROUP_END);

	cpg_token_free (next);

	if (!groupend)
		return FALSE;
	
	cpg_token_free (cpg_tokenizer_next (buffer));
	return TRUE;
}

static gboolean
parse_unary_operator (CpgExpression   *expression,
                      CpgToken        *token,
                      gchar const    **buffer,
                      GSList          *context)
{
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (token);
	gboolean ret = TRUE;
	CpgInstruction *inst = NULL;

	// handle group
	if (op->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		cpg_token_free (cpg_tokenizer_next (buffer));
		return parse_group (expression, buffer, context);
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
			ret = FALSE;
		break;
	}
	
	if (ret)
	{
		// consume token
		cpg_token_free (cpg_tokenizer_next (buffer));
		ret = parse_expression (expression, buffer, context, 1000, 1);
	}
	
	if (ret && inst)
		instructions_push (expression, inst);
	
	return ret;
}

static gboolean
parse_operator (CpgExpression   *expression,
                CpgToken        *token,
                gchar const    **buffer,
                GSList          *context)
{
	CpgTokenOperator *op = CPG_TOKEN_OPERATOR (token);
	
	// handle ternary
	if (op->type == CPG_TOKEN_OPERATOR_TYPE_TERNARY_TRUE)
	{
		// consume token
		cpg_token_free (cpg_tokenizer_next (buffer));

		return parse_ternary_operator (expression, CPG_TOKEN_OPERATOR (token), buffer, context);
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
	
	// parse second part of binary operator
	if (!inst)
		return FALSE;
		
	// consume token
	cpg_token_free (cpg_tokenizer_next (buffer));

	if (!parse_expression (expression, buffer, context, op->priority, op->left_assoc))
		return FALSE;
	
	instructions_push (expression, inst);
	
	return TRUE;
}

static gboolean
parse_property (CpgExpression  *expression,
                gchar          *propname,
                GSList         *context)
{
	CpgProperty *property = NULL;
	
	cpg_debug_expression ("Parsing property: %s", propname);
	
	// iterate over contexts
	while (context && !property)
	{
		if (context->data)
			property = cpg_object_get_property (CPG_OBJECT (context->data), propname);
		
		context = context->next;
	}
	
	if (!property)
	{
		cpg_debug_expression ("Property %s not found", propname);
		return FALSE;
	}
	
	instructions_push (expression, cpg_instruction_property_new (property));
	return TRUE;
}

static CpgLink *
find_link (GSList *context)
{
	while (context)
	{
		if (context->data && CPG_IS_LINK (context->data))
			return CPG_LINK (context->data);
		
		context = g_slist_next (context);
	}
	
	return NULL;
}

static gboolean
parse_link_property (CpgExpression  *expression,
                     gchar          *id,
                     gchar          *propname,
                     CpgLink        *link)
{
	CpgProperty *property = NULL;

	if (strcmp (id, "from") == 0)
		property = cpg_object_get_property (cpg_link_get_from (link), propname);
	else if (strcmp (id, "to") == 0)
		property = cpg_object_get_property (cpg_link_get_to (link), propname);
	
	if (!property)
		return FALSE;
	
	instructions_push (expression, cpg_instruction_property_new (property));
	return TRUE;
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
parse_identifier (CpgExpression        *expression,
                  CpgTokenIdentifier   *token,
                  gchar const         **buffer,
                  GSList               *context)
{
	gchar *id = token->identifier;
	gboolean ret = FALSE;
	
	// consume token and peek the next to see if the identifier is a function
	// call
	cpg_token_free (cpg_tokenizer_next (buffer));
	CpgToken *next = cpg_tokenizer_peek (*buffer);
	CpgLink *link;

	if (next && CPG_TOKEN_IS_OPERATOR (next) && 
		CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_GROUP_START)
	{
		// consume peeked group start
		cpg_token_free (cpg_tokenizer_next (buffer));
		ret = parse_function (expression, id, buffer, context);
	}
	else if (next && CPG_TOKEN_IS_OPERATOR (next) &&
			 CPG_TOKEN_OPERATOR (next)->type == CPG_TOKEN_OPERATOR_TYPE_DOT && (link = find_link (context)))
	{
		// consume peeked dot
		cpg_token_free (cpg_tokenizer_next (buffer));
		CpgToken *propname = cpg_tokenizer_next (buffer);
	
		if (CPG_TOKEN_IS_IDENTIFIER (propname))
			ret = parse_link_property (expression, id, CPG_TOKEN_IDENTIFIER (propname)->identifier, link);
	
		cpg_token_free (propname);
	}
	else
	{
		// try to parse property
		ret = parse_property (expression, id, context);
	
		if (!ret)
		{	
			// try parsing constants
			ret = parse_constant (expression, id);
		}
	}

	cpg_token_free (next);
	return ret;
}

static gboolean
parse_expression (CpgExpression   *expression,
                  gchar const    **buffer,
                  GSList          *context,
                  gint             priority,
                  gint             left_assoc)
{
	static gint depth = 0;
	
	// peek next token
	CpgToken *token;
	gboolean ret = FALSE;
	gint num = 0;
	
	cpg_debug_expression ("Parse begin (%d): %s", ++depth, *buffer);
	
	while ((token = cpg_tokenizer_peek (*buffer)))
	{
		if (!token)
			break;
		
		ret = TRUE;
		
		cpg_debug_expression ("Parsing next: (%d) %d, %s", depth, num, *buffer);
	
		switch (token->type)
		{
			case CPG_TOKEN_TYPE_NUMBER:
				// simply push a number on the stack
				ret = parse_number (expression, CPG_TOKEN_NUMBER (token));
			break;
			case CPG_TOKEN_TYPE_IDENTIFIER:
			{
				ret = parse_identifier (expression, CPG_TOKEN_IDENTIFIER (token), buffer, context);
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
					cpg_debug_expression ("Parse end group (%d): %s", depth--, *buffer);
					return TRUE;
				}
				
				if (num == 0)
				{
					ret = parse_unary_operator (expression, token, buffer, context);
					
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
					cpg_debug_expression ("Parse end op (%d): %s", depth--, *buffer);
					return TRUE;
				}
				else
				{
					ret = parse_operator (expression, token, buffer, context);
					
					if (ret)
					{
						cpg_token_free (token);
						token = NULL;
					}
				}
			}
			break;
			case CPG_TOKEN_TYPE_NONE:
				ret = FALSE;
			break;
		}
	
		if (token)
		{
			// consume token
			if (ret)
				cpg_token_free (cpg_tokenizer_next (buffer));

			cpg_token_free (token);
		}
		
		++num;
		
		if (ret == FALSE)
			break;
	}
	
	cpg_debug_expression ("Parse end (%d): %s", depth--, *buffer);
	
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
	
	g_slist_free (expression->dependencies);
	expression->dependencies = NULL;
	
	for (item = expression->instructions; item; item = g_slist_next(item))
	{
		CpgInstruction *inst = item->data;

		switch (inst->type)
		{
			case CPG_INSTRUCTION_TYPE_OPERATOR:
			case CPG_INSTRUCTION_TYPE_FUNCTION:
			{
				CpgInstructionFunction *i = (CpgInstructionFunction *)inst;
				stack -= i->arguments + i->vargs;
				
				if (stack < 0)
					return 0;
				
				// TODO: add number of return values to function instruction
				++stack;
			}
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
				expression->dependencies = g_slist_append (expression->dependencies, ((CpgInstructionProperty *)inst)->property);
				++stack;
			break;
			case CPG_INSTRUCTION_TYPE_NUMBER:
				// increase stack here
				++stack;
			break;
			case CPG_INSTRUCTION_TYPE_NONE:
			break;
		}
		
		if (stack > maxstack)
			maxstack = stack;
	}
	
	if (stack != 1)
		return FALSE;
	
	cpg_stack_destroy (&(expression->output));
	cpg_stack_init (&(expression->output), maxstack);
	
	return TRUE;
}

 gboolean
cpg_expression_compile (CpgExpression   *expression,
                        GSList          *context,
                        gchar          **error)
{
	gchar *buffer = expression->expression;
	
	instructions_free (expression);
	
	cpg_stack_destroy (&(expression->output));
	expression->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
	
	gboolean ret = parse_expression (expression, (gchar const **)&buffer, context, -1, 0);
	
	if (!ret)
	{
		instructions_free (expression);

		// could not parse full thing
		if (error)
			*error = g_strdup_printf ("Invalid token at: %s", buffer ? buffer : "");
		
		return FALSE;
	}
	else
	{
		// reverse instructions
		expression->instructions = g_slist_reverse (expression->instructions);

		if (!validate_stack (expression))
		{
			if (error)
			{
				gchar msg[4096];
				GSList *item;
				gchar *ptr;
				
				snprintf (msg, 4096, "Invalid stack produced: \n\t%s\n\t", expression->expression);
				ptr = msg + strlen (msg);

				for (item = expression->instructions; item; item = g_slist_next(item))
				{
					gchar *res = instruction_tos (item->data);
					snprintf (ptr, msg + 4096 - ptr, "%s ", res);
					ptr += strlen (res) + 1;
					g_free (res);
				}
				
				*error = g_strdup (msg);
				instructions_free (expression);
			}
			
			return FALSE;
		}
	}
	
	return TRUE;
}

 void
cpg_expression_set_value (CpgExpression  *expression,
                          gdouble         value)
{
	// this can be instant and lets also set the cache already
	expression->flags = CPG_EXPRESSION_FLAG_INSTANT | CPG_EXPRESSION_FLAG_CACHED;
	expression->cached_output = value;
}

gdouble
cpg_expression_evaluate (CpgExpression *expression)
{
	if (!expression)
		return 0.0;
		
	if (expression->flags & CPG_EXPRESSION_FLAG_CACHED)
		return expression->cached_output;
	
	GSList *item;
	cpg_stack_reset (&(expression->output));
	
	if (expression->output.size == 0)
	{
		cpg_debug_error ("Stack size should not be 0!");
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
				cpg_stack_push (&(expression->output), ((CpgInstructionNumber *)instruction)->value, NULL);
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
			{
				CpgInstructionProperty *property = (CpgInstructionProperty *)instruction;
				cpg_stack_push (&(expression->output), cpg_property_get_value (property->property), NULL);
			}
			break;
			case CPG_INSTRUCTION_TYPE_FUNCTION:
				cpg_math_function_execute (((CpgInstructionFunction *)instruction)->id, &(expression->output), NULL);
			break;
			case CPG_INSTRUCTION_TYPE_OPERATOR:
				cpg_math_operator_execute (((CpgInstructionFunction *)instruction)->id, &(expression->output), NULL);
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
	expression->cached_output = cpg_stack_pop (&(expression->output), NULL);
	
	return expression->cached_output;
}

 void
cpg_expression_print_instructions (CpgExpression  *expression,
                                   FILE           *f)
{
	GSList *item;
	
	for (item = expression->instructions; item; item = g_slist_next(item))
	{
		gchar *res = instruction_tos(item->data);
		
		fprintf (f, "%s ", res);
		g_free (res);
	}
	
	fprintf (f, "\n");;
}

void
cpg_expression_reset_cache (CpgExpression *expression)
{
	if (!(expression->flags & CPG_EXPRESSION_FLAG_INSTANT))
		expression->flags &= ~CPG_EXPRESSION_FLAG_CACHED;
}

GSList *
cpg_expression_get_dependencies (CpgExpression *expression)
{
	return expression->dependencies;
}

void
cpg_expression_reset (CpgExpression *expression)
{
	expression->flags = CPG_EXPRESSION_FLAG_NONE;
}

GSList *
cpg_expression_get_instructions(CpgExpression *expression)
{
	return expression->instructions;
}
