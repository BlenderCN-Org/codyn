/*
 * cdn-tokenizer.h
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

#ifndef __CDN_TOKENIZER_H__
#define __CDN_TOKENIZER_H__

#include <glib.h>

G_BEGIN_DECLS

#define CDN_TOKEN_IS_NUMBER(x) (x->type == CDN_TOKEN_TYPE_NUMBER)
#define CDN_TOKEN_IS_IDENTIFIER(x) (x->type == CDN_TOKEN_TYPE_IDENTIFIER)
#define CDN_TOKEN_IS_OPERATOR(x) (x->type == CDN_TOKEN_TYPE_OPERATOR)

#define CDN_TOKEN_NUMBER(x) ((CdnTokenNumber *)x)
#define CDN_TOKEN_IDENTIFIER(x) ((CdnTokenIdentifier *)x)
#define CDN_TOKEN_OPERATOR(x) ((CdnTokenOperator *)x)

typedef enum
{
	CDN_TOKEN_TYPE_NONE,
	CDN_TOKEN_TYPE_NUMBER,
	CDN_TOKEN_TYPE_IDENTIFIER,
	CDN_TOKEN_TYPE_OPERATOR
} CdnTokenType;

#define CDN_TOKEN_OPERATOR_TYPE_IS_ARITHMETIC(x) (x > CDN_TOKEN_OPERATOR_TYPE_ARITHMETIC && x < CDN_TOKEN_OPERATOR_TYPE_LOGICAL)
#define CDN_TOKEN_OPERATOR_TYPE_IS_LOGICAL(x) (x > CDN_TOKEN_OPERATOR_TYPE_LOGICAL && x < CDN_TOKEN_OPERATOR_TYPE_TERNARY)
#define CDN_TOKEN_OPERATOR_TYPE_IS_TERNARY(x) (x > CDN_TOKEN_OPERATOR_TYPE_TERNARY && x < CDN_TOKEN_OPERATOR_TYPE_NODE)
#define CDN_TOKEN_OPERATOR_TYPE_IS_NODE(x) (x > CDN_TOKEN_OPERATOR_TYPE_NODE)

typedef enum
{
	CDN_TOKEN_OPERATOR_TYPE_NONE,
	
	// arithmetic operators
	CDN_TOKEN_OPERATOR_TYPE_ARITHMETIC,
	CDN_TOKEN_OPERATOR_TYPE_MULTIPLY,
	CDN_TOKEN_OPERATOR_TYPE_DIVIDE,
	CDN_TOKEN_OPERATOR_TYPE_MODULO,
	CDN_TOKEN_OPERATOR_TYPE_PLUS,
	CDN_TOKEN_OPERATOR_TYPE_MINUS,
	CDN_TOKEN_OPERATOR_TYPE_POWER,
	CDN_TOKEN_OPERATOR_TYPE_TILDE,
	
	// logical operators
	CDN_TOKEN_OPERATOR_TYPE_LOGICAL,
	CDN_TOKEN_OPERATOR_TYPE_NEGATE,
	CDN_TOKEN_OPERATOR_TYPE_GREATER,
	CDN_TOKEN_OPERATOR_TYPE_LESS,
	CDN_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL,
	CDN_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL,
	CDN_TOKEN_OPERATOR_TYPE_EQUAL,
	CDN_TOKEN_OPERATOR_TYPE_OR,
	CDN_TOKEN_OPERATOR_TYPE_AND,
	
	// ternary operator
	CDN_TOKEN_OPERATOR_TYPE_TERNARY,
	CDN_TOKEN_OPERATOR_TYPE_TERNARY_TRUE,
	CDN_TOKEN_OPERATOR_TYPE_TERNARY_FALSE,
	
	// group 'operator'
	CDN_TOKEN_OPERATOR_TYPE_NODE,
	CDN_TOKEN_OPERATOR_TYPE_NODE_START,
	CDN_TOKEN_OPERATOR_TYPE_NODE_END,

	CDN_TOKEN_OPERATOR_TYPE_OPERATOR_START,
	CDN_TOKEN_OPERATOR_TYPE_OPERATOR_END,

	CDN_TOKEN_OPERATOR_TYPE_COMMA,
	CDN_TOKEN_OPERATOR_TYPE_DOT,
	CDN_TOKEN_OPERATOR_TYPE_PRIME,
	CDN_TOKEN_OPERATOR_TYPE_SEMI_COLON,
	CDN_TOKEN_OPERATOR_TYPE_TRANSPOSE,
	CDN_TOKEN_OPERATOR_TYPE_SQUARE
} CdnTokenOperatorType;

typedef struct
{
	CdnTokenType type;
	gchar *text;
} CdnToken;

typedef struct
{
	CdnToken parent;
	gdouble value;
} CdnTokenNumber;

typedef struct
{
	CdnToken parent;
	gchar *identifier;
} CdnTokenIdentifier;

typedef struct
{
	CdnToken parent;
	CdnTokenOperatorType type;
	gint priority;
	gint left_assoc;
} CdnTokenOperator;

CdnToken *cdn_tokenizer_next (const gchar **buffer);
CdnToken *cdn_tokenizer_peek (const gchar *buffer);

gboolean cdn_tokenizer_validate_identifier (const gchar *identifier);

void cdn_token_free (CdnToken *token);

G_END_DECLS

#endif /* __CDN_TOKENIZER_H__ */

