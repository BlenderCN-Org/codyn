%{

#include "cpg-parser-private.h"

static void yyerror (CpgParserContext *context, char *s);

#define append_array(array, type, value, code)				\
{									\
	GArray *arret = array;						\
									\
	if (arret == NULL)						\
	{								\
		arret = g_array_new (FALSE, FALSE, sizeof (type));	\
	}								\
									\
	g_array_append_val (arret, value);				\
									\
	code;								\
}

static CpgFunctionPolynomialPiece *create_polynomial_piece (gdouble  start,
                                                            gdouble  end,
                                                            GArray  *coefficients);

static CpgFunctionArgument *create_function_argument (gchar const *name,
                                                      gboolean     is_optional,
                                                      gdouble      default_value);

%}

%token T_KEY_IN T_KEY_INTEGRATED T_KEY_ONCE T_KEY_OUT

%token T_KEY_STATE T_KEY_LINK T_KEY_NETWORK T_KEY_TEMPLATE T_KEY_FUNCTION T_KEY_INTERFACE T_KEY_IMPORT T_KEY_INPUT_FILE T_KEY_POLYNOMIAL T_KEY_FROM T_KEY_TO T_KEY_PIECE

%token <numf> T_DOUBLE
%token <id> T_IDENTIFIER
%token <id> T_ANY
%token <id> T_SELECTOR_NTH

%type <flags> flags
%type <flags> flag

%type <property> property_def
%type <array> double_list
%type <piece> polynomial_piece
%type <array> polynomial_pieces
%type <array> function_argument_list
%type <array> selector_contents
%type <id> selector_item
%type <argument> function_argument

%type <object> scope_end

%parse-param {CpgParserContext *context}

%start document

%destructor { cpg_parser_context_pop_scope (context); } scope_start
%destructor { cpg_parser_context_pop_scope (context); } state_scope_start
%destructor { cpg_parser_context_pop_scope (context); } link_scope_start
%destructor { cpg_parser_context_pop_scope (context); } network_scope_start

%destructor { cpg_parser_context_pop_template (context); } template_identifier

%expect 5

%%

document
	:
	| document toplevel
	;

toplevel
	: network
	| template
	| state
	| link
	| function
	| polynomial
	| coupling
	| import
	;

network
	: T_KEY_NETWORK network_scope_start properties scope_end
	;

network_scope_start
	: '{'					{ cpg_parser_context_push_scope (context, NULL, CPG_PARSER_CONTEXT_SCOPE_NETWORK); }
	;

template
	: template_identifier state
	| template_identifier link
	;

template_identifier
	: T_KEY_TEMPLATE 			{ cpg_parser_context_push_template (context); }
	;

state
	: T_KEY_STATE T_IDENTIFIER state_scope_start state_contents scope_end
	| T_KEY_STATE T_IDENTIFIER ':' identifier_list state_scope_start state_contents scope_end
	;

state_scope_start
	: '{'					{ cpg_parser_context_push_scope (context, $<id>-1, CPG_PARSER_CONTEXT_SCOPE_STATE); }
	;

scope_start
	: '{'					{ cpg_parser_context_push_scope (context, NULL, CPG_PARSER_CONTEXT_SCOPE_NONE); }
	;

scope_end
	: '}' 					{ $$ = cpg_parser_context_pop_scope (context); }
	;

link
	: T_KEY_LINK T_IDENTIFIER link_scope_start link_contents scope_end connect_link
	| T_KEY_LINK T_IDENTIFIER ':' identifier_list link_scope_start link_contents scope_end connect_link
	;

connect_link
	:
	| T_KEY_FROM identifier_or_selector T_KEY_TO identifier_or_selector
						{ cpg_parser_context_link (context, cpg_object_get_id ($<object>-1), $2, $4); }

identifier_or_selector
	: selector
	| T_IDENTIFIER				{ $$ = cpg_object_get_id ($<object>-1); }
	;

link_scope_start
	: '{'					{ cpg_parser_context_push_scope (context, $<id>-1, CPG_PARSER_CONTEXT_SCOPE_LINK); }
	;

function
	: T_KEY_FUNCTION T_IDENTIFIER '(' function_argument_list ')' scope_start T_ANY optional_semi scope_end
						{ cpg_parser_context_add_function (context, $2, $7, $4); }
	;

polynomial
	: T_KEY_POLYNOMIAL T_IDENTIFIER '(' ')' scope_start polynomial_pieces scope_end
						{ cpg_parser_context_add_polynomial (context, $2, $6); }
	;

polynomial_pieces
	: { $$ = NULL; }
	| polynomial_pieces polynomial_piece { append_array ($1, CpgFunctionPolynomialPiece *, $2, $$ = arret); }
	;

polynomial_piece
	: T_KEY_PIECE T_KEY_FROM T_DOUBLE T_KEY_TO T_DOUBLE '=' double_list ';'
						{ $$ = create_polynomial_piece ($3, $5, $7); }
	;

double_list
	: T_DOUBLE				{ append_array (NULL, gdouble, $1, $$ = arret); }
	| double_list ',' T_DOUBLE		{ append_array ($1, gdouble, $3, $$ = arret); }
	;

optional_semi
	:
	';'
	;

identifier_list
	: T_IDENTIFIER				{ cpg_parser_context_add_identifier (context, $1); }
	| identifier_list ',' T_IDENTIFIER	{ cpg_parser_context_add_identifier (context, $3); }
	;

function_argument_list
	: function_argument			{ append_array (NULL, CpgFunctionArgument *, $1, $$ = arret); }
	| function_argument_list ',' function_argument { append_array ($1, CpgFunctionArgument *, $3, $$ = arret); }
	;

function_argument
	: T_IDENTIFIER '=' T_DOUBLE		{ $$ = create_function_argument ($1, TRUE, $3); }
	| T_IDENTIFIER				{ $$ = create_function_argument ($1, FALSE, 0.0); }
	;

state_contents
	:
	| state_contents property
	| state_contents state
	| state_contents link
	| state_contents interface
	| state_contents coupling
	;

interface
	: T_KEY_INTERFACE '{' interface_contents '}'
	;

interface_contents
	:
	| interface_contents interface_item
	;

interface_item
	: T_IDENTIFIER '=' T_IDENTIFIER		{ cpg_parser_context_add_interface (context, $1, $3); }
	;

properties
	:
	| properties property
	;

link_contents
	:
	| link_contents action
	| link_contents property
	;

property
	: flags property_def			{ cpg_property_set_flags ($2, $1); }
	| property_def
	;

property_def
	: T_IDENTIFIER '=' T_ANY ';'		{ $$ = cpg_parser_context_add_property (context, $1, $3); }
	;

flags
	: 					{ $$ = 0; }
	| flags flag 				{ $$ = $1 | $2; }
	;

flag
	: T_KEY_IN				{ $$ = CPG_PROPERTY_FLAG_IN; }
	| T_KEY_OUT				{ $$ = CPG_PROPERTY_FLAG_OUT; }
	| T_KEY_INTEGRATED			{ $$ = CPG_PROPERTY_FLAG_INTEGRATED; }
	| T_KEY_ONCE				{ $$ = CPG_PROPERTY_FLAG_ONCE; }
	;

action
	: T_IDENTIFIER '<' T_ANY ';'		{ cpg_parser_context_add_action (context, $1, $3); }
	;

selector
	: '[' selector_contents ']';

selector_contents
	:					{ $$ = NULL; }
	| selector_contents ':' selector_item	{ append_array ($1, CpgSelector *, $3, $$ = arret); }
	;

selector_item
	: T_IDENTIFIER '(' T_SELECTOR_NTH ')'	{ $$ = g_strconcat ($1, $3, NULL); }
	| T_IDENTIFIER				{ $$ = g_strdup ($1); }
	;

coupling
	: T_KEY_LINK T_IDENTIFIER T_KEY_FROM T_IDENTIFIER T_KEY_TO T_IDENTIFIER
						{ cpg_parser_context_link (context, $2, $4, $6); }
	;

import
	: T_KEY_TEMPLATE T_KEY_IMPORT T_IDENTIFIER T_KEY_FROM T_IDENTIFIER
						{ cpg_parser_context_import (context, $3, $5, TRUE); }

	| T_KEY_IMPORT T_IDENTIFIER T_KEY_FROM T_IDENTIFIER
						{ cpg_parser_context_import (context, $2, $4, FALSE); }
	;

%%

static void
yyerror (CpgParserContext *context, char *s)
{
	cpg_parser_context_error (context, yyget_lineno (), s);
}

static CpgFunctionPolynomialPiece *
create_polynomial_piece (gdouble  start,
                         gdouble  end,
                         GArray  *coefficients)
{
	guint len;
	gdouble *coefs;

	len = (guint)(coefficients ? coefficients->len : 0);

	if (coefficients && len > 0)
	{
		guint i;

		coefs = g_new (gdouble, len);

		for (i = 0; i < len; ++i)
		{
			coefs[i] = g_array_index (coefficients, gdouble, i);
		}
	}

	return g_object_ref_sink (cpg_function_polynomial_piece_new (start,
	                                                             end,
	                                                             coefs,
	                                                             len));
}

static CpgFunctionArgument *
create_function_argument (gchar const *name,
                          gboolean     is_optional,
                          gdouble      default_value)
{
	return g_object_ref_sink (cpg_function_argument_new (name,
	                                                     is_optional,
	                                                     default_value));
}
