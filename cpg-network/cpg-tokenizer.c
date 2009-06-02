#include "cpg-tokenizer.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <glib.h>

typedef struct
{
	gint priority;
	gint left_assoc;
} OperatorProperties;

static OperatorProperties operator_properties[] = 
{
	{0, 0}, // CPG_TOKEN_OPERATOR_TYPE_NONE,
	
	// arithmetic operators
	{0, 0}, // CPG_TOKEN_OPERATOR_TYPE_ARITHMETIC,
	{7, 1}, // CPG_TOKEN_OPERATOR_TYPE_MULTIPLY,
	{7, 1}, // CPG_TOKEN_OPERATOR_TYPE_DIVIDE,
	{7, 1}, // CPG_TOKEN_OPERATOR_TYPE_MODULO,
	{6, 1}, // CPG_TOKEN_OPERATOR_TYPE_PLUS,
	{6, 1}, // CPG_TOKEN_OPERATOR_TYPE_MINUS,
	{9, 0}, // CPG_TOKEN_OPERATOR_TYPE_POWER,
	
	// logical operators
	{0, 0}, // CPG_TOKEN_OPERATOR_TYPE_LOGICAL,
	{8, 0}, // CPG_TOKEN_OPERATOR_TYPE_NEGATE,
	{5, 1}, // CPG_TOKEN_OPERATOR_TYPE_GREATER,
	{5, 1}, // CPG_TOKEN_OPERATOR_TYPE_LESS,
	{5, 1}, // CPG_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL,
	{5, 1}, // CPG_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL,
	{4, 1}, // CPG_TOKEN_OPERATOR_TYPE_EQUAL,
	{2, 1}, // CPG_TOKEN_OPERATOR_TYPE_OR,
	{3, 1}, // CPG_TOKEN_OPERATOR_TYPE_AND,
	
	// ternary operator
	{1, 0}, // CPG_TOKEN_OPERATOR_TYPE_TERNARY,
	{1, 0}, // CPG_TOKEN_OPERATOR_TYPE_TERNARY_TRUE,
	{1, 0}, // CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE,
	
	// group 'operator'
	{0, 0}, // CPG_TOKEN_OPERATOR_TYPE_GROUP,
	{10, 1}, // CPG_TOKEN_OPERATOR_TYPE_GROUP_START,
	{10, 1}, // CPG_TOKEN_OPERATOR_TYPE_GROUP_END,

	{10, 1}  // CPG_TOKEN_OPERATOR_TYPE_COMMA
};

static void
skip_whitespace (gchar const **buffer)
{
	while (g_ascii_isspace (**buffer))
		++*buffer;
}

static gint
buffer_peek (gchar const  *buffer,
             gint          at)
{
	return strlen (buffer) <= at ? '\0' : buffer[at];
}

/* parse number value */
CpgToken *
cpg_tokenizer_parse_number (gchar const **buffer)
{
	// parse leading numbers
	gchar const *start = *buffer;
	
	while (g_ascii_isdigit (*(++*buffer)))
	;
	
	if (**buffer == '.')
	{
		while (g_ascii_isdigit (*(++*buffer)))
		;
	}
	
	CpgTokenNumber *res = g_slice_new (CpgTokenNumber);
	res->parent.type = CPG_TOKEN_TYPE_NUMBER;

	res->parent.text = g_strndup ((gchar *)start, *buffer - start);
	res->value = g_ascii_strtod (res->parent.text, NULL);
	
	if (*start == '.')
	{
		while (res->value > 0)
			res->value = res->value / 10.0;
	}
	
	return (CpgToken *)res;
}

/* parse identifier */
CpgToken *
cpg_tokenizer_parse_identifier (gchar const **buffer)
{
	gchar const *start = *buffer;

	while (isalnum (*(++*buffer)) || **buffer == '_')
	;
	
	CpgTokenIdentifier *res = g_slice_new (CpgTokenIdentifier);
	res->parent.type = CPG_TOKEN_TYPE_IDENTIFIER;
	res->parent.text = g_strndup (start, *buffer - start);
	res->identifier = res->parent.text;

	return (CpgToken *)res;
}

/* check for operator */
gboolean
isoperator (gint c)
{
	switch (c)
	{
		case '*':
		case '-':
		case '+':
		case '.':
		case '/':
		case '%':
		case '!':
		case '<':
		case '>':
		case '=':
		case '?':
		case ':':
		case '&':
		case '|':
		case '(':
		case ')':
		case ',':
			return TRUE;
	}
	
	return FALSE;
}

/* parse operator */
CpgToken *
cpg_tokenizer_parse_operator (gchar const **buffer)
{
	gint c = **buffer;
	gint n = buffer_peek (*buffer, 1);
	CpgTokenOperatorType type = CPG_TOKEN_OPERATOR_TYPE_NONE;

	gchar const *start = *buffer;
	
	// skip buffer 2 places to handle double char operators, then when it
	// is not a double char, shift buffer one back, its the easiest way
	*buffer += 2;
	
	// first find double char operators
	if (c == '*' && n == '*')
		type = CPG_TOKEN_OPERATOR_TYPE_POWER;
	else if (c == '=' && n == '=')
		type = CPG_TOKEN_OPERATOR_TYPE_EQUAL;
	else if (c == '>' && n == '=')
		type = CPG_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL;
	else if (c == '<' && n == '=')
		type = CPG_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL;
	else if (c == '|' && n == '|')
		type = CPG_TOKEN_OPERATOR_TYPE_OR;
	else if (c == '&' && n == '&')
		type = CPG_TOKEN_OPERATOR_TYPE_AND;
	else
	{
		--*buffer;
		
		switch (c)
		{
			case '*':
				type = CPG_TOKEN_OPERATOR_TYPE_MULTIPLY;
			break;
			case '/':
				type = CPG_TOKEN_OPERATOR_TYPE_DIVIDE;
			break;
			case '%':
				type = CPG_TOKEN_OPERATOR_TYPE_MODULO;
			break;
			case '-':
				type = CPG_TOKEN_OPERATOR_TYPE_MINUS;
			break;
			case '+':
				type = CPG_TOKEN_OPERATOR_TYPE_PLUS;
			break;
			case '!':
				type = CPG_TOKEN_OPERATOR_TYPE_NEGATE;
			break;
			case '>':
				type = CPG_TOKEN_OPERATOR_TYPE_GREATER;
			break;
			case '<':
				type = CPG_TOKEN_OPERATOR_TYPE_LESS;
			break;
			case '?':
				type = CPG_TOKEN_OPERATOR_TYPE_TERNARY_TRUE;
			break;
			case ':':
				type = CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;
			break;
			case '(':
				type = CPG_TOKEN_OPERATOR_TYPE_GROUP_START;
			break;
			case ')':
				type = CPG_TOKEN_OPERATOR_TYPE_GROUP_END;
			break;
			case ',':
				type = CPG_TOKEN_OPERATOR_TYPE_COMMA;
			break;
			case '.':
				type = CPG_TOKEN_OPERATOR_TYPE_DOT;
			break;
		}
	}
	
	if (type == CPG_TOKEN_OPERATOR_TYPE_NONE)
		return NULL;
	
	CpgTokenOperator *res = g_slice_new0 (CpgTokenOperator);
	res->parent.type = CPG_TOKEN_TYPE_OPERATOR;
	res->parent.text = g_strndup (start, *buffer - start);
	
	res->type = type;
	res->priority = operator_properties[type].priority;
	res->left_assoc = operator_properties[type].left_assoc;
	
	return (CpgToken *)res;
}

CpgToken *
cpg_tokenizer_peek (gchar const *buffer)
{
	return cpg_tokenizer_next (&buffer);
}

CpgToken *
cpg_tokenizer_next (gchar const **buffer)
{
	if (*buffer)
		skip_whitespace (buffer);
		
	if (*buffer == NULL || **buffer == '\0')
	{
		*buffer = NULL;
		return NULL;
	}
	
	CpgToken *res = NULL;
	
	// check for number
	if (isdigit (**buffer) || (**buffer == '.' && isdigit (buffer_peek (*buffer, 1))))
	{
		res = cpg_tokenizer_parse_number (buffer);
	}
	// check for identifier
	else if (isalpha (**buffer) || **buffer == '_')
	{
		res = cpg_tokenizer_parse_identifier (buffer);
	}
	// check for operator
	else if (isoperator (**buffer))
	{
		res = cpg_tokenizer_parse_operator (buffer);
	}

	return res;
}

void
cpg_token_free (CpgToken *token)
{
	if (!token)
		return;
	
	g_free (token->text);

	switch (token->type)
	{
		case CPG_TOKEN_TYPE_IDENTIFIER:
			g_slice_free (CpgTokenIdentifier, (CpgTokenIdentifier *)token);
		break;
		case CPG_TOKEN_TYPE_NUMBER:
			g_slice_free (CpgTokenNumber, (CpgTokenNumber *)token);
		break;
		case CPG_TOKEN_TYPE_OPERATOR:
			g_slice_free (CpgTokenOperator, (CpgTokenOperator *)token);
		break;
		default:
		break;
	}
}
