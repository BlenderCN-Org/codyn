/*
 * cpg-tokenizer.h
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

#ifndef __CPG_TOKENIZER_H__
#define __CPG_TOKENIZER_H__

#include <glib.h>

G_BEGIN_DECLS

#define CPG_TOKEN_IS_NUMBER(x) (x->type == CPG_TOKEN_TYPE_NUMBER)
#define CPG_TOKEN_IS_IDENTIFIER(x) (x->type == CPG_TOKEN_TYPE_IDENTIFIER)
#define CPG_TOKEN_IS_OPERATOR(x) (x->type == CPG_TOKEN_TYPE_OPERATOR)

#define CPG_TOKEN_NUMBER(x) ((CpgTokenNumber *)x)
#define CPG_TOKEN_IDENTIFIER(x) ((CpgTokenIdentifier *)x)
#define CPG_TOKEN_OPERATOR(x) ((CpgTokenOperator *)x)

typedef enum
{
	CPG_TOKEN_TYPE_NONE,
	CPG_TOKEN_TYPE_NUMBER,
	CPG_TOKEN_TYPE_IDENTIFIER,
	CPG_TOKEN_TYPE_OPERATOR
} CpgTokenType;

#define CPG_TOKEN_OPERATOR_TYPE_IS_ARITHMETIC(x) (x > CPG_TOKEN_OPERATOR_TYPE_ARITHMETIC && x < CPG_TOKEN_OPERATOR_TYPE_LOGICAL)
#define CPG_TOKEN_OPERATOR_TYPE_IS_LOGICAL(x) (x > CPG_TOKEN_OPERATOR_TYPE_LOGICAL && x < CPG_TOKEN_OPERATOR_TYPE_TERNARY)
#define CPG_TOKEN_OPERATOR_TYPE_IS_TERNARY(x) (x > CPG_TOKEN_OPERATOR_TYPE_TERNARY && x < CPG_TOKEN_OPERATOR_TYPE_GROUP)
#define CPG_TOKEN_OPERATOR_TYPE_IS_GROUP(x) (x > CPG_TOKEN_OPERATOR_TYPE_GROUP)

typedef enum
{
	CPG_TOKEN_OPERATOR_TYPE_NONE,
	
	// arithmetic operators
	CPG_TOKEN_OPERATOR_TYPE_ARITHMETIC,
	CPG_TOKEN_OPERATOR_TYPE_MULTIPLY,
	CPG_TOKEN_OPERATOR_TYPE_DIVIDE,
	CPG_TOKEN_OPERATOR_TYPE_MODULO,
	CPG_TOKEN_OPERATOR_TYPE_PLUS,
	CPG_TOKEN_OPERATOR_TYPE_MINUS,
	CPG_TOKEN_OPERATOR_TYPE_POWER,
	
	// logical operators
	CPG_TOKEN_OPERATOR_TYPE_LOGICAL,
	CPG_TOKEN_OPERATOR_TYPE_NEGATE,
	CPG_TOKEN_OPERATOR_TYPE_GREATER,
	CPG_TOKEN_OPERATOR_TYPE_LESS,
	CPG_TOKEN_OPERATOR_TYPE_GREATER_OR_EQUAL,
	CPG_TOKEN_OPERATOR_TYPE_LESS_OR_EQUAL,
	CPG_TOKEN_OPERATOR_TYPE_EQUAL,
	CPG_TOKEN_OPERATOR_TYPE_OR,
	CPG_TOKEN_OPERATOR_TYPE_AND,
	
	// ternary operator
	CPG_TOKEN_OPERATOR_TYPE_TERNARY,
	CPG_TOKEN_OPERATOR_TYPE_TERNARY_TRUE,
	CPG_TOKEN_OPERATOR_TYPE_TERNARY_FALSE,
	
	// group 'operator'
	CPG_TOKEN_OPERATOR_TYPE_GROUP,
	CPG_TOKEN_OPERATOR_TYPE_GROUP_START,
	CPG_TOKEN_OPERATOR_TYPE_GROUP_END,

	CPG_TOKEN_OPERATOR_TYPE_OPERATOR_START,
	CPG_TOKEN_OPERATOR_TYPE_OPERATOR_END,

	CPG_TOKEN_OPERATOR_TYPE_COMMA,
	CPG_TOKEN_OPERATOR_TYPE_DOT,
	CPG_TOKEN_OPERATOR_TYPE_PRIME,
	CPG_TOKEN_OPERATOR_TYPE_SEMI_COLON
} CpgTokenOperatorType;

typedef struct
{
	CpgTokenType type;
	gchar *text;
} CpgToken;

typedef struct
{
	CpgToken parent;
	gdouble value;
} CpgTokenNumber;

typedef struct
{
	CpgToken parent;
	gchar *identifier;
} CpgTokenIdentifier;

typedef struct
{
	CpgToken parent;
	CpgTokenOperatorType type;
	gint priority;
	gint left_assoc;
} CpgTokenOperator;

CpgToken *cpg_tokenizer_next (const gchar **buffer);
CpgToken *cpg_tokenizer_peek (const gchar *buffer);

gboolean cpg_tokenizer_validate_identifier (const gchar *identifier);

void cpg_token_free (CpgToken *token);

G_END_DECLS

#endif /* __CPG_TOKENIZER_H__ */

