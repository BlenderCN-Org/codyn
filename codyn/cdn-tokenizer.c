/*
 * cdn-tokenizer.c
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

#include "cdn-tokenizer.h"
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
	{0, 0}, // CDN_TOKEN_OPERATOR_TYPE_NONE,

	// arithmetic operators
	{0, 0}, // CDN_TOKEN_OPERATOR_TYPE_ARITHMETIC,
	{7, 1}, // CDN_TOKEN_OPERATOR_TYPE_MULTIPLY,
	{7, 1}, // CDN_TOKEN_OPERATOR_TYPE_DIVIDE,
	{7, 1}, // CDN_TOKEN_OPERATOR_TYPE_MODULO,
	{6, 1}, // CDN_TOKEN_OPERATOR_TYPE_PLUS,
	{6, 1}, // CDN_TOKEN_OPERATOR_TYPE_MINUS,
	{9, 0}, // CDN_TOKEN_OPERATOR_TYPE_POWER,

	// logical operators
	{0, 0}, // CDN_TOKEN_OPERATOR_TYPE_LOGICAL,
	{8, 0}, // CDN_TOKEN_OPERATOR_TYPE_NEGATE,
	{5, 1}, // CDN_TOKEN_OPERATOR_TYPE_GREATER,
	{5, 1}, // CDN_TOKEN_OPERATOR_TYPE_LESS,
	{5, 1}, // CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL,
	{5, 1}, // CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL,
	{4, 1}, // CDN_TOKEN_OPERATOR_TYPE_EQUAL,
	{4, 1}, // CDN_TOKEN_OPERATOR_TYPE_NEQUAL,
	{2, 1}, // CDN_TOKEN_OPERATOR_TYPE_OR,
	{3, 1}, // CDN_TOKEN_OPERATOR_TYPE_AND,

	// ternary operator
	{1, 0}, // CDN_TOKEN_OPERATOR_TYPE_TERNARY,
	{1, 0}, // CDN_TOKEN_OPERATOR_TYPE_TERNARY_TRUE,
	{1, 0}, // CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE,

	// group 'operator'
	{0, 0}, // CDN_TOKEN_OPERATOR_TYPE_NODE,
	{10, 1}, // CDN_TOKEN_OPERATOR_TYPE_NODE_START,
	{10, 1}, // CDN_TOKEN_OPERATOR_TYPE_NODE_END,

	{10, 1}, // CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START,
	{10, 1}, // CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END,

	{10, 1},  // CDN_TOKEN_OPERATOR_TYPE_COMMA
	{10, 1},  // CDN_TOKEN_OPERATOR_TYPE_DOT
	{10, 1},  // CDN_TOKEN_OPERATOR_TYPE_PRIME
	{10, 1},  // CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON
	{10, 1},  // CDN_TOKEN_OPERATOR_TYPE_TRANSPOSE
	{10, 1}  // CDN_TOKEN_OPERATOR_TYPE_SQUARE
};

#define skip_while(buffer, code)					\
{									\
	gunichar c;							\
									\
	while ((c = g_utf8_get_char (buffer)) && (code))		\
	{								\
		buffer = g_utf8_next_char (buffer);			\
	}								\
}

static void
skip_whitespace (gchar const **buffer)
{
	skip_while (*buffer, g_unichar_isspace (c));
}

static gunichar
buffer_peek (gchar const  *buffer,
             gint          at)
{
	return g_utf8_strlen (buffer, -1) <= at ? 0 : g_utf8_get_char (g_utf8_offset_to_pointer (buffer, at));
}

/* parse number value */
static CdnToken *
cdn_tokenizer_parse_number (gchar const **buffer)
{
	// parse leading numbers
	gchar const *start = *buffer;

	skip_while (*buffer, g_unichar_isdigit (c));

	if (**buffer == '.')
	{
		++*buffer;
		skip_while (*buffer, g_unichar_isdigit (c));
	}

	// Scientific notation
	if ((**buffer == 'e' || **buffer == 'E') && *(*buffer + 1))
	{
		gchar const *next = *buffer + 1;

		if (*next == '+' || *next == '-')
		{
			++next;
		}

		if (*next && g_unichar_isdigit (g_utf8_get_char (next)))
		{
			skip_while (next, g_unichar_isdigit (c));

			*buffer = next;
		}
	}

	CdnTokenNumber *res = g_slice_new (CdnTokenNumber);
	res->parent.type = CDN_TOKEN_TYPE_NUMBER;

	res->parent.text = g_strndup ((gchar *)start, *buffer - start);
	res->value = g_ascii_strtod (res->parent.text, NULL);

	return (CdnToken *)res;
}

static gboolean
is_ident (gunichar c)
{
	return (g_unichar_isalpha (c) ||
	        c == '_' ||
	        (c >= 0x2200 && c <= 0x22ff) ||
	        (c >= 0x370 && c <= 0x3ff) ||
	        (c >= 0x0300 && c <= 0x03ff)) &&
	        c != 7488 && c != 178;
}

gboolean
cdn_tokenizer_validate_identifier (const gchar *identifier)
{
	if (!identifier || !*identifier || !is_ident (g_utf8_get_char (identifier)))
	{
		return FALSE;
	}

	while (*identifier)
	{
		gunichar c = g_utf8_get_char (identifier);

		if (!(is_ident (c) || g_unichar_isdigit (c) || c == '.' || c == '\''))
		{
			return FALSE;
		}

		identifier = g_utf8_next_char (identifier);
	}

	return TRUE;
}

/* parse identifier */
static CdnToken *
cdn_tokenizer_parse_identifier (gchar const **buffer)
{
	gchar const *start = *buffer;

	skip_while (*buffer, is_ident (c) || g_unichar_isdigit (c));

	CdnTokenIdentifier *res = g_slice_new (CdnTokenIdentifier);
	res->parent.type = CDN_TOKEN_TYPE_IDENTIFIER;
	res->parent.text = g_strndup (start, *buffer - start);
	res->identifier = res->parent.text;

	return (CdnToken *)res;
}

/* check for operator */
static gboolean
isoperator (gunichar c)
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
		case '[':
		case ']':
		case '\'':
		case ';':
		case '^':
		case '~':
		case 7488: /* ᵀ for transpose */
		case 178: /* ² for square */
		case 8729: /* ∙ for multiply */
			return TRUE;
	}

	return FALSE;
}

/* parse operator */
static CdnToken *
cdn_tokenizer_parse_operator (gchar const **buffer)
{
	gunichar c = g_utf8_get_char (*buffer);
	gunichar n = buffer_peek (*buffer, 1);

	CdnTokenOperatorType type = CDN_TOKEN_OPERATOR_TYPE_NONE;

	gchar const *start = *buffer;

	// skip buffer 2 places to handle double char operators, then when it
	// is not a double char, shift buffer one back, its the easiest way
	*buffer += 2;

	// first find double char operators
	if (c == '*' && n == '*')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_POWER;
	}
	else if (c == '=' && n == '=')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_EQUAL;
	}
	else if (c == '>' && n == '=')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL;
	}
	else if (c == '<' && n == '=')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL;
	}
	else if (c == '|' && n == '|')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_OR;
	}
	else if (c == '&' && n == '&')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_AND;
	}
	else if (c == '!' && n == '=')
	{
		type = CDN_TOKEN_OPERATOR_TYPE_NEQUAL;
	}
	else
	{
		*buffer = g_utf8_next_char (*buffer - 2);

		switch (c)
		{
			case '*':
				type = CDN_TOKEN_OPERATOR_TYPE_MULTIPLY;
			break;
			case '/':
				type = CDN_TOKEN_OPERATOR_TYPE_DIVIDE;
			break;
			case '%':
				type = CDN_TOKEN_OPERATOR_TYPE_MODULO;
			break;
			case '-':
				type = CDN_TOKEN_OPERATOR_TYPE_MINUS;
			break;
			case '+':
				type = CDN_TOKEN_OPERATOR_TYPE_PLUS;
			break;
			case '!':
				type = CDN_TOKEN_OPERATOR_TYPE_NEGATE;
			break;
			case '>':
				type = CDN_TOKEN_OPERATOR_TYPE_GREATER;
			break;
			case '<':
				type = CDN_TOKEN_OPERATOR_TYPE_LESS;
			break;
			case '?':
				type = CDN_TOKEN_OPERATOR_TYPE_TERNARY_TRUE;
			break;
			case ':':
				type = CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE;
			break;
			case '(':
				type = CDN_TOKEN_OPERATOR_TYPE_NODE_START;
			break;
			case ')':
				type = CDN_TOKEN_OPERATOR_TYPE_NODE_END;
			break;
			case ',':
				type = CDN_TOKEN_OPERATOR_TYPE_COMMA;
			break;
			case '.':
				type = CDN_TOKEN_OPERATOR_TYPE_DOT;
			break;
			case '\'':
				type = CDN_TOKEN_OPERATOR_TYPE_PRIME;
			break;
			case '[':
				type = CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START;
			break;
			case ']':
				type = CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END;
			break;
			case ';':
				type = CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON;
			break;
			case '^':
				type = CDN_TOKEN_OPERATOR_TYPE_POWER;
			break;
			case 8729: /* ∙ for multiply */
				type = CDN_TOKEN_OPERATOR_TYPE_MULTIPLY;
			break;
			case 7488: /* ᵀ for transpose */
				type = CDN_TOKEN_OPERATOR_TYPE_TRANSPOSE;
			break;
			case 178: /* ² for square */
				type = CDN_TOKEN_OPERATOR_TYPE_SQUARE;
			break;
		}
	}

	if (type == CDN_TOKEN_OPERATOR_TYPE_NONE)
	{
		return NULL;
	}

	CdnTokenOperator *res = g_slice_new0 (CdnTokenOperator);
	res->parent.type = CDN_TOKEN_TYPE_OPERATOR;
	res->parent.text = g_strndup (start, *buffer - start);

	res->type = type;
	res->priority = operator_properties[type].priority;
	res->left_assoc = operator_properties[type].left_assoc;

	return (CdnToken *)res;
}

CdnToken *
cdn_tokenizer_peek (gchar const *buffer)
{
	return cdn_tokenizer_next (&buffer);
}

void
cdn_tokenizer_ensure_skip_space (const gchar **buffer)
{
	if (*buffer)
	{
		skip_whitespace (buffer);
	}
}

CdnToken *
cdn_tokenizer_next (gchar const **buffer)
{
	cdn_tokenizer_ensure_skip_space (buffer);

	if (*buffer == NULL || **buffer == '\0')
	{
		*buffer = NULL;
		return NULL;
	}

	CdnToken *res = NULL;
	gunichar c = g_utf8_get_char (*buffer);

	// check for number
	if (g_unichar_isdigit (c) || (c == '.' && g_unichar_isdigit (buffer_peek (*buffer, 1))))
	{
		res = cdn_tokenizer_parse_number (buffer);
	}
	// check for operator
	else if (isoperator (c))
	{
		res = cdn_tokenizer_parse_operator (buffer);
	}
	// check for identifier
	else if (is_ident (c))
	{
		res = cdn_tokenizer_parse_identifier (buffer);
	}

	return res;
}

void
cdn_token_free (CdnToken *token)
{
	if (!token)
	{
		return;
	}

	g_free (token->text);

	switch (token->type)
	{
		case CDN_TOKEN_TYPE_IDENTIFIER:
			g_slice_free (CdnTokenIdentifier, (CdnTokenIdentifier *)token);
		break;
		case CDN_TOKEN_TYPE_NUMBER:
			g_slice_free (CdnTokenNumber, (CdnTokenNumber *)token);
		break;
		case CDN_TOKEN_TYPE_OPERATOR:
			g_slice_free (CdnTokenOperator, (CdnTokenOperator *)token);
		break;
		default:
		break;
	}
}
