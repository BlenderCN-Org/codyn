%{

#include "cpg-parser-context.h"
#include "cpg-parser.h"

static void cpg_parser_error (YYLTYPE *locp, CpgParserContext *context, char *s);
int cpg_parser_lex(YYSTYPE *lvalp, YYLTYPE *llocp, void *scanner);

#define scanner (cpg_parser_context_get_scanner (context))

#define append_array(array, type, value, code)				\
{									\
	GArray *arret = array;						\
	type theval = value;						\
									\
	if (arret == NULL)						\
	{								\
		arret = g_array_new (FALSE, FALSE, sizeof (type));	\
	}								\
									\
	g_array_append_val (arret, theval);				\
									\
	code;								\
}

#define errb								\
	if (cpg_parser_context_get_error (context) != NULL)		\
	{								\
		YYERROR;						\
	}

static CpgFunctionPolynomialPiece *create_polynomial_piece (gdouble  start,
                                                            gdouble  end,
                                                            GArray  *coefficients);

static CpgFunctionArgument *create_function_argument (gchar const *name,
                                                      gboolean     is_optional,
                                                      gdouble      default_value);

%}

%token T_KEY_IN T_KEY_INTEGRATED T_KEY_ONCE T_KEY_OUT

%token T_KEY_STATE T_KEY_LINK T_KEY_NETWORK T_KEY_FUNCTION T_KEY_INTERFACE T_KEY_IMPORT T_KEY_INPUT_FILE T_KEY_POLYNOMIAL T_KEY_FROM T_KEY_TO T_KEY_PIECE T_KEY_TEMPLATES T_KEY_DEFINE T_KEY_BIDIRECTIONAL T_KEY_ALL T_KEY_INTEGRATOR T_KEY_GROUP T_KEY_LAYOUT

%token <num> T_KEY_LEFT_OF T_KEY_RIGHT_OF T_KEY_BELOW T_KEY_ABOVE
%type <num> relation
%type <num> relation_item
%type <num> relation_all

%token <numf> T_DOUBLE
%token <numf> T_INTEGER
%token <id> T_IDENTIFIER
%token <id> T_STRING

%token T_STRING_BEGIN
%token T_STRING_END

%token T_REGEX_BEGIN
%token T_REGEX_END

%token T_EQUATION_BEGIN
%token T_EQUATION_END

%token T_START_DOCUMENT
%token T_START_SELECTOR

%token <id> T_DEFINED
%token <id> T_EQUATION
%token <id> T_EXPANSION

%type <flags> flags
%type <flags> flag

%type <id> selector_pseudo_identifier

%type <array> double_list
%type <array> pseudo_args_list
%type <piece> polynomial_piece
%type <array> polynomial_pieces
%type <array> function_argument_list
%type <array> selector_list
%type <argument> function_argument
%type <selector> selector

%type <object> link
%type <object> state
%type <id> identifier_or_string
%type <id> expanded_string
%type <id> expanded_regex
%type <id> string_contents
%type <id> equation_contents

%type <num> link_flags

%type <array> link_connect
%type <array> templated

%define api.pure
%name-prefix="cpg_parser_"

%parse-param {CpgParserContext *context}
%lex-param {void *scanner}
%error-verbose

%locations
%defines

%union
{
	char *id;
	CpgProperty *property;
	CpgPropertyFlags flags;
	gdouble numf;
	gint num;
	GSList *list;
	GArray *array;
	CpgFunctionPolynomialPiece *piece;
	CpgFunctionArgument *argument;
	CpgObject *object;
	CpgSelector *selector;
	GString *string;
}

%start choose_parser

%expect 31

%%

choose_parser
	: T_START_DOCUMENT document
	| T_START_SELECTOR selector
	;

document
	:
	| document toplevel
	;

toplevel
	: network
	| state
	| group
	| link
	| function
	| polynomial
	| import
	| templates
	| define
	| layout
	;

network
	: T_KEY_NETWORK
	  '{'				{ cpg_parser_context_push_network (context); }
	  network_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

network_contents
	:
	| network_contents property
	| network_contents integrator
	;

integrator
	: T_KEY_INTEGRATOR '=' expanded_string	{ cpg_parser_context_set_integrator (context, $3); }
	;

define
	: T_KEY_DEFINE T_IDENTIFIER '=' expanded_string { cpg_parser_context_define (context, $2, $4); }
	;

expanded_regex
	: T_REGEX_BEGIN string_contents T_REGEX_END	{ $$ = $2; }
	;

equation
	: T_EQUATION_BEGIN equation_contents T_EQUATION_END
	;

equation_contents
	:						{ $$ = g_strdup (""); }
	| equation_contents T_EQUATION			{ $$ = g_strconcat ($1, $2, NULL); }
	| equation_contents T_DEFINED			{ $$ = g_strconcat ($1, cpg_parser_context_embed_define (context, $2), NULL); }
	| equation_contents T_EXPANSION			{ $$ = g_strconcat ($1, cpg_parser_context_embed_expansion (context, $2), NULL); }
	;

expanded_string
	: T_STRING_BEGIN string_contents T_STRING_END	{ $$ = $2; }
	| T_INTEGER					{ $$ = g_strdup_printf ("%d", (gint)$1); }
	| T_DOUBLE					{ $$ = g_strdup_printf ("%f", $1); }
	| T_DEFINED					{ $$ = cpg_parser_context_embed_define (context, $1); }
	| equation					{ $$ = cpg_parser_context_embed_equation (context, $1); }
	| T_EXPANSION					{ $$ = cpg_parser_context_embed_expansion (context, $1); }
	| T_STRING					{ $$ = $1; }
	;

string_contents
	:				{ $$ = g_strdup (""); }
	| string_contents T_STRING	{ $$ = g_strconcat ($1, $2, NULL); }
	| string_contents T_DEFINED	{ $$ = g_strconcat ($1, cpg_parser_context_embed_define (context, $2), NULL); }
	| string_contents equation	{ $$ = g_strconcat ($1, cpg_parser_context_embed_equation (context, $2), NULL); }
	| string_contents T_EXPANSION	{ $$ = g_strconcat ($1, cpg_parser_context_embed_expansion (context, $2), NULL); }
	;

templates
	: T_KEY_TEMPLATES
	  '{'				{ cpg_parser_context_push_templates (context); }
	  template_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

template_item
	: state
	| link
	| group
	| import
	;

template_contents
	:
	| template_contents template_item;
	;

templated
	:				{ $$ = NULL; }
	| ':' selector_list		{ $$ = $2; }
	;

state
	: T_KEY_STATE
	  expanded_string
	  templated
	  '{' 				{ cpg_parser_context_push_state (context, $2, $3); errb }
	  state_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

group
	: T_KEY_GROUP
	  expanded_string
	  templated
	  '{' 				{ cpg_parser_context_push_group (context, $2, $3); errb }
	  group_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

link_flags
	:					{ $$ = 0; }
	| link_flags T_KEY_ALL			{ $$ = $1 | CPG_PARSER_CONTEXT_LINK_FLAG_ALL; }
	| link_flags T_KEY_BIDIRECTIONAL	{ $$ = $1 | CPG_PARSER_CONTEXT_LINK_FLAG_BIDIRECTIONAL; }
	;

link_connect
	:				{ $$ = NULL; }
	| T_KEY_FROM selector T_KEY_TO selector
					{ append_array (NULL, CpgSelector *, $2, $$ = arret);
					  append_array ($$, CpgSelector *, $4, $$ = arret);
					}
	;

link
	: link_flags
	  T_KEY_LINK
	  expanded_string
	  link_connect
	  templated
	  '{'	 			{ cpg_parser_context_push_link (context, $3, $5, $1, $4); errb }
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

function
	: T_KEY_FUNCTION
	  T_IDENTIFIER
	  '('
	  function_argument_list
	  ')'
	  '{'
	  expanded_string
	  '}'				{ cpg_parser_context_add_function (context, $2, $7, $4); errb }
	;

polynomial
	: T_KEY_POLYNOMIAL
	  T_IDENTIFIER
	  '('
	  ')'
	  '{'
	  polynomial_pieces
	  '}'				{ cpg_parser_context_add_polynomial (context, $2, $6); errb }
	;

polynomial_pieces
	: 				{ $$ = NULL; }
	| polynomial_pieces polynomial_piece
					{ append_array ($1, CpgFunctionPolynomialPiece *, $2, $$ = arret); }
	;

polynomial_piece
	: T_KEY_PIECE T_KEY_FROM T_DOUBLE T_KEY_TO T_DOUBLE '=' double_list
					{ $$ = create_polynomial_piece ($3, $5, $7); }
	;

double_list
	: T_DOUBLE			{ append_array (NULL, gdouble, $1, $$ = arret); }
	| T_INTEGER			{ append_array (NULL, gdouble, $1, $$ = arret); }
	| double_list ',' T_DOUBLE	{ append_array ($1, gdouble, $3, $$ = arret); }
	| double_list ',' T_INTEGER	{ append_array ($1, gdouble, $3, $$ = arret); }
	;

selector_list
	: selector			{ append_array (NULL, CpgSelector *, $1, $$ = arret); }
	| selector_list ',' selector	{ append_array ($1, CpgSelector *, $3, $$ = arret); }
	;

function_argument_list
	: function_argument		{ append_array (NULL, CpgFunctionArgument *, $1, $$ = arret); }
	| function_argument_list ',' function_argument
					{ append_array ($1, CpgFunctionArgument *, $3, $$ = arret); }
	;

function_argument
	: T_IDENTIFIER '=' T_DOUBLE	{ $$ = create_function_argument ($1, TRUE, $3); }
	| T_IDENTIFIER			{ $$ = create_function_argument ($1, FALSE, 0.0); }
	;

state_contents
	:
	| state_contents property
	;

group_item
	: property
	| state
	| link
	| interface
	| group
	;

group_contents
	:
	| group_contents group_item
	;

interface
	: T_KEY_INTERFACE '{' interface_contents '}'
	;

interface_contents
	:
	| interface_contents interface_item
	;

interface_item
	: T_IDENTIFIER '=' selector	{ cpg_parser_context_add_interface (context, $1, $3); errb }
	;

link_contents
	:
	| link_contents action
	| link_contents property
	;

identifier_or_string
	: T_IDENTIFIER
	| expanded_string
	;

property
	: flags identifier_or_string '=' expanded_string
					{ cpg_parser_context_add_property (context, $2, $4, $1); errb }
	| identifier_or_string '=' expanded_string
					{ cpg_parser_context_add_property (context, $1, $3, CPG_PROPERTY_FLAG_NONE); errb }
	;

flags
	: 				{ $$ = 0; }
	| flags flag 			{ $$ = $1 | $2; }
	;

flag
	: T_KEY_IN			{ $$ = CPG_PROPERTY_FLAG_IN; }
	| T_KEY_OUT			{ $$ = CPG_PROPERTY_FLAG_OUT; }
	| T_KEY_INTEGRATED		{ $$ = CPG_PROPERTY_FLAG_INTEGRATED; }
	| T_KEY_ONCE			{ $$ = CPG_PROPERTY_FLAG_ONCE; }
	;

action
	: T_IDENTIFIER '<' '=' expanded_string
					{ cpg_parser_context_add_action (context, $1, $4); errb }
	;

selector
	: selector_pseudo nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	| selector_identifier nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	| selector_regex nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	;

selector_identifier
	: identifier_or_string 		{ cpg_parser_context_push_selector (context, $1); errb }
	;

selector_regex
	: expanded_regex		{ cpg_parser_context_push_selector_regex (context, $1); errb }
	;

nested_selector
	:
	| nested_selector '.' selector_identifier
	| nested_selector '.' selector_regex
	| nested_selector selector_pseudo
	;

pseudo_args_list
	:					{ $$ = NULL; }
	| pseudo_args_list expanded_string	{ append_array ($1, gchar *, $2, $$ = arret); }
	;

selector_pseudo_identifier
	: T_IDENTIFIER			{ $$ = $1; }
	| T_KEY_FROM			{ $$ = g_strdup ("from"); }
	| T_KEY_TEMPLATES		{ $$ = g_strdup ("templates"); }
	| expanded_string		{ $$ = $1; }
	;

selector_pseudo
	: ':' selector_pseudo_identifier '(' pseudo_args_list ')'
					{ cpg_parser_context_push_selector_pseudo (context, $2, $4); errb }
	| ':' selector_pseudo_identifier		{ cpg_parser_context_push_selector_pseudo (context, $2, NULL); errb }
	;

import
	: T_KEY_IMPORT T_IDENTIFIER T_KEY_FROM expanded_string
					{ cpg_parser_context_import (context, $2, $4); errb }
	;

layout
	: T_KEY_LAYOUT
	  '{'			{ cpg_parser_context_push_layout (context); }
	  layout_items
	  '}'
	;

relation_item
	: T_KEY_LEFT_OF		{ $$ = CPG_LAYOUT_RELATION_LEFT_OF; }
	| T_KEY_RIGHT_OF	{ $$ = CPG_LAYOUT_RELATION_RIGHT_OF; }
	| T_KEY_ABOVE		{ $$ = CPG_LAYOUT_RELATION_ABOVE; }
	| T_KEY_BELOW		{ $$ = CPG_LAYOUT_RELATION_BELOW; }
	;

relation
	: relation_item			{ $$ = $1; }
	| relation relation_item	{ $$ = $1 | $2; }
	;

relation_all
	:				{ $$ = FALSE; }
	| T_KEY_ALL			{ $$ = TRUE; }
	;

layout_item
	: selector
	  relation
	  relation_all
	  selector		{ cpg_parser_context_add_layout (context, $2, $1, $4, $3); }
	| selector
	  T_KEY_AT
	  '('
	  expanded_string
	  ','
	  expanded_string
	  ')'
	;

layout_items
	:
	| layout_items layout_item
	;

%%

static void
yyerror (YYLTYPE *locp, CpgParserContext *context, char *s)
{
	cpg_parser_context_error (context,
	                          s);
}

static CpgFunctionPolynomialPiece *
create_polynomial_piece (gdouble  start,
                         gdouble  end,
                         GArray  *coefficients)
{
	guint len;
	gdouble *coefs = NULL;

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
