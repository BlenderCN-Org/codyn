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

static CpgFunctionArgument *create_function_argument (CpgEmbeddedString *name,
                                                      gboolean     is_optional,
                                                      gdouble      default_value);

%}

%token T_KEY_IN T_KEY_INTEGRATED T_KEY_ONCE T_KEY_OUT

%token T_KEY_STATE T_KEY_LINK T_KEY_NETWORK T_KEY_FUNCTIONS T_KEY_INTERFACE T_KEY_IMPORT T_KEY_INPUT_FILE T_KEY_POLYNOMIAL T_KEY_FROM T_KEY_TO T_KEY_PIECE T_KEY_TEMPLATES T_KEY_DEFINES T_KEY_INTEGRATOR T_KEY_GROUP T_KEY_LAYOUT T_KEY_AT T_KEY_OF T_KEY_ON T_KEY_INCLUDE T_KEY_DEBUG T_KEY_PROPERTY T_KEY_DELETE T_KEY_ACTION T_KEY_OR T_KEY_ROOT T_KEY_CHILDREN T_KEY_PARENT T_KEY_FIRST T_KEY_LAST T_KEY_SUBSET T_KEY_SIBLINGS T_KEY_STATES T_KEY_LINKS T_KEY_COUNT T_KEY_SELF T_KEY_CONTEXT T_KEY_AS T_KEY_EACH T_KEY_PROXY T_KEY_BIDIRECTIONAL T_KEY_OBJECTS T_KEY_GROUPS T_KEY_PROPERTIES T_KEY_ACTIONS T_KEY_IF

%token <num> T_KEY_LEFT_OF T_KEY_RIGHT_OF T_KEY_BELOW T_KEY_ABOVE
%type <num> relation
%type <num> relation_item

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

%type <num> selector_pseudo_simple_key
%type <num> selector_pseudo_selector_key
%type <num> selector_pseudo_nth_key

%type <list> selector_pseudo_nth_args
%type <list> selector_pseudo_nth_args_rev
%type <list> selector_pseudo_selector_args
%type <list> selector_pseudo_selector_args_rev

%type <string> identifier_or_string_or_nothing

%token <num> T_INDIRECTION_BEGIN
%token T_INDIRECTION_END

%token T_INDIRECTION_EMBEDDING_BEGIN

%token T_START_DOCUMENT
%token T_START_SELECTOR

%type <num> property_flag_sign
%type <flags> property_flags
%type <flags> property_flags_contents
%type <num> property_flag

%type <selector> layout_relative

%type <array> double_list
%type <piece> polynomial_piece
%type <array> polynomial_pieces
%type <array> function_argument_list
%type <array> function_argument_list_or_empty
%type <array> template_list
%type <argument> function_argument
%type <selector> selector
%type <selector> strict_selector
%type <selector> selector_self

%type <string> identifier_or_string
%type <string> string
%type <string> regex
%type <string> equation
%type <string> indirection
%type <string> identifier
%type <string> value_as_string
%type <string> double
%type <string> integer

%type <list> attributes
%type <list> attributes_contents
%type <attribute> attribute_contents
%type <attribute> attribute_proxy
%type <attribute> attribute_each
%type <attribute> attribute_bidirectional
%type <attribute> attribute_if

%type <list> state
%type <list> define_values

%type <array> link_connect
%type <array> link_connect_fast
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
	gchar *id;
	CpgProperty *property;
	gdouble numf;
	gint num;
	GSList *list;
	GArray *array;
	CpgFunctionPolynomialPiece *piece;
	CpgFunctionArgument *argument;
	gpointer object;
	CpgSelector *selector;
	CpgEmbeddedString *string;
	CpgAttribute *attribute;

	struct
	{
		gint parent;
		gint idx;
	} ref;

	struct
	{
		CpgPropertyFlags add;
		CpgPropertyFlags remove;
	} flags;
}

%start choose_parser

%expect 15

%%

choose_parser
	: T_START_DOCUMENT document_contents
	| T_START_SELECTOR selector_parse
	;

document_contents
	:
	| document_contents document_item
	;

document_item
	: network
	| state
	| group
	| link
	| functions
	| import
	| templates
	| layout
	| integrator
	| include
	| delete
	| delete_context
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  document_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

include_path
	: T_STRING_BEGIN
	  string_contents
	  T_STRING_END			{ errb; }
	;

include
	: T_KEY_INCLUDE include_path
	;

network
	: attributes
	  T_KEY_NETWORK
	  '{'				{ cpg_parser_context_push_network (context, $1); }
	  network_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

common_scopes
	: define
	| debug
	;

network_item
	: property
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  network_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

network_contents
	:
	| network_contents network_item
	;

integrator_property
	: identifier_or_string '=' value_as_string { cpg_parser_context_add_integrator_property (context, $1, $3); }
	;

integrator_item
	: integrator_property
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  integrator_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

integrator_contents
	:
	| integrator_contents integrator_item
	;

integrator
	: attributes
	  T_KEY_INTEGRATOR
	  '{'				{ cpg_parser_context_push_integrator (context, $1); }
	  integrator_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

define_values
	: value_as_string				{ $$ = g_slist_prepend (NULL, $1); }
	| define_values T_KEY_OR value_as_string	{ $$ = g_slist_prepend ($1, $3); }
	;

define_item
	: '{' identifier_or_string '}' '=' define_values
					{ cpg_parser_context_define (context,
					                             $2,
					                             g_slist_reverse ($5),
					                             TRUE,
					                             FALSE); }
	| identifier_or_string '=' define_values
					{ cpg_parser_context_define (context,
					                             $1,
					                             g_slist_reverse ($3),
					                             FALSE,
					                             FALSE); }
	| '{' identifier_or_string '}' '?' '=' define_values
					{ cpg_parser_context_define (context,
					                             $2,
					                             g_slist_reverse ($6),
					                             TRUE,
					                             TRUE); }
	| identifier_or_string '?' '=' define_values
					{ cpg_parser_context_define (context,
					                             $1,
					                             g_slist_reverse ($4),
					                             FALSE,
					                             TRUE); }
	| debug
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  define_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

define_contents
	:
	| define_contents define_item
	;

define
	: attributes
	  T_KEY_DEFINES
	  '{'				{ cpg_parser_context_push_define (context, $1); }
	  define_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

templates
	: attributes
	  T_KEY_TEMPLATES
	  '{'				{ cpg_parser_context_push_templates (context, $1); }
	  template_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

template_item
	: state
	| link
	| group
	| import
	| common_scopes
	| layout
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  template_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

template_contents
	:
	| template_contents template_item;
	;

	
template_list
	: ':' selector			{ append_array (NULL, CpgSelector *, $2, $$ = arret); }
	| template_list ':' selector	{ append_array ($1, CpgSelector *, $3, $$ = arret); }
	;

templated
	:				{ $$ = NULL; }
	| template_list			{ $$ = $1; }
	;

identifier_or_string_or_nothing
	:				{ $$ = NULL; }
	| identifier_or_string		{ $$ = $1; }
	;

state
	: attributes
	  T_KEY_STATE
	  identifier_or_string_or_nothing
	  templated
	  '{' 				{ cpg_parser_context_push_state (context, $3, $4, $1); errb }
	  state_contents
	  '}'				{ $$ = cpg_parser_context_pop (context); errb }
	| attributes
	  T_KEY_STATE
	  strict_selector		{ cpg_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CPG_SELECTOR_TYPE_LINK,
	                                                                     $1); }
	  '{'
	  state_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

group
	: attributes
	  T_KEY_GROUP
	  identifier_or_string_or_nothing
	  templated
	  '{' 				{ cpg_parser_context_push_group (context, $3, $4, $1); errb }
	  group_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	| attributes
	  T_KEY_GROUP
	  strict_selector		{ cpg_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CPG_SELECTOR_TYPE_GROUP,
	                                                                     $1); }
	  '{'
	  group_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

attribute_proxy
	: T_KEY_PROXY			{ $$ = cpg_attribute_new ("proxy"); }
	| T_KEY_PROXY '(' ')'		{ $$ = cpg_attribute_new ("proxy"); }
	;

attribute_each
	: T_KEY_EACH '(' value_as_string ')'
					{ $$ = cpg_attribute_newv ("each", $3, NULL); }
	;

attribute_bidirectional
	: T_KEY_BIDIRECTIONAL		{ $$ = cpg_attribute_new ("bidirectional"); }
	| T_KEY_BIDIRECTIONAL '(' ')'	{ $$ = cpg_attribute_new ("bidirectional"); }
	| T_KEY_BIDIRECTIONAL '(' value_as_string ',' value_as_string ')'
					{ $$ = cpg_attribute_newv ("bidirectional", $3, $5, NULL); }
	;

attribute_if
	: T_KEY_IF '(' value_as_string ')' { $$ = cpg_attribute_newv ("if", $3, NULL); }
	| T_KEY_IF '(' strict_selector ')' { $$ = cpg_attribute_newv ("if", $3, NULL); }
	;

attribute_contents
	: attribute_proxy
	| attribute_each
	| attribute_bidirectional
	| attribute_if
	;

attributes_contents
	: attribute_contents		{$$ = g_slist_prepend (NULL, $1); }
	| attributes_contents ',' attribute_contents
					{ $$ = g_slist_prepend ($1, $3); }
	;

attributes
	:				{ $$ = NULL; }
	| '[' ']'			{ $$ = NULL; }
	| '[' attributes_contents ']'	{ $$ = g_slist_reverse ($2); }
	;

link_connect
	:				{ $$ = NULL; }
	| T_KEY_FROM selector T_KEY_TO selector
					{ append_array (NULL, CpgSelector *, $2, $$ = arret);
					  append_array ($$, CpgSelector *, $4, $$ = arret);
					}
	| T_KEY_ON selector		{ append_array (NULL, CpgSelector *, $2, $$ = arret); }
	;

link_connect_fast
	: selector T_KEY_TO selector	{ append_array (NULL, CpgSelector *, $1, $$ = arret);
					  append_array ($$, CpgSelector *, $3, $$ = arret);
					}
	| T_KEY_ON selector		{ append_array (NULL, CpgSelector *, $2, $$ = arret); }
	;

link
	: attributes
	  T_KEY_LINK
	  identifier_or_string
	  link_connect
	  templated
	  '{'	 			{ cpg_parser_context_push_link (context, $3, $5, $1, $4); errb }
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	| attributes
	  T_KEY_LINK
	  link_connect_fast
	  templated
	  '{'				{ cpg_parser_context_push_link (context, NULL, $4, $1, $3); errb }
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	| attributes
	  T_KEY_LINK
	  templated
	  '{'				{ cpg_parser_context_push_link (context, NULL, $3, $1, NULL); errb }
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	| attributes
	  T_KEY_LINK
	  strict_selector		{ cpg_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CPG_SELECTOR_TYPE_LINK,
	                                                                     $1); }
	  '{'
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

function_polynomial
	: attributes
	  T_KEY_POLYNOMIAL
	  identifier_or_string
	  '('
	  ')'
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  polynomial_pieces
	  '}'				{ cpg_parser_context_add_polynomial (context, $3, $8); errb
	                                  cpg_parser_context_pop (context); errb }
	;

function_argument_list_or_empty
	:				{ $$ = NULL; }
	| function_argument_list	{ $$ = $1; }
	;

function_custom
	: attributes
	  identifier_or_string
	  '('
	  function_argument_list_or_empty
	  ')'
	  '='				{ cpg_parser_context_push_scope (context, $1); }
	  value_as_string		{ cpg_parser_context_add_function (context, $2, $8, $4); errb
	                                  cpg_parser_context_pop (context); errb }
	;

function_item
	: function_custom
	| function_polynomial
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  function_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

function_contents
	:
	| function_contents function_item
	;

functions
	: attributes
	  T_KEY_FUNCTIONS
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  function_contents
	  '}'				{ cpg_parser_context_pop (context); }
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

function_argument_list
	: function_argument		{ append_array (NULL, CpgFunctionArgument *, $1, $$ = arret); }
	| function_argument_list ',' function_argument
					{ append_array ($1, CpgFunctionArgument *, $3, $$ = arret); }
	;

function_argument
	: identifier_or_string '=' T_DOUBLE	{ $$ = create_function_argument ($1, TRUE, $3); }
	| identifier_or_string '=' T_INTEGER	{ $$ = create_function_argument ($1, TRUE, $3); }
	| identifier_or_string			{ $$ = create_function_argument ($1, FALSE, 0.0); }
	;

state_item
	: property
	| common_scopes
	| layout
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  state_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

state_contents
	:
	| state_contents state_item
	;

group_item
	: property
	| state
	| link
	| interface
	| group
	| common_scopes
	| layout
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  group_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

group_contents
	:
	| group_contents group_item
	;

interface
	: attributes
	  T_KEY_INTERFACE
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  interface_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

interface_contents
	:
	| interface_contents interface_item
	;

interface_property
	: identifier_or_string '=' selector	{ cpg_parser_context_add_interface (context, $1, $3); errb }
	;

interface_item
	: interface_property
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  interface_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

link_item
	: action
	| property
	| common_scopes
	| layout
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  link_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

link_contents
	:
	| link_contents link_item
	;

identifier_or_string
	: identifier
	| string
	| indirection
	;

property
	: identifier_or_string '=' value_as_string '<' '=' value_as_string property_flags
					{ cpg_parser_context_add_property (context, $1, $3, $7.add, $7.remove, $6); errb }
	| identifier_or_string '=' value_as_string property_flags
					{ cpg_parser_context_add_property (context, $1, $3, $4.add, $4.remove, NULL); errb }
	;

property_flag_sign
	:				{ $$ = 1; }
	| '-'				{ $$ = 0; }
	| '+'				{ $$ = 1; }
	;

property_flags_contents
	: property_flag_sign property_flag	{ $$.add = 0; $$.remove = 0; ($1 ? (($$.add) = $2) : (($$.remove) = $2)); }
	| property_flags_contents property_flag_sign property_flag
					{ $2 ? (($$.add) |= $3) : (($$.remove) |= $3); }
	;

property_flags
	: 				{ $$.add = 0; $$.remove = 0; }
	| '|' property_flags_contents	{ $$ = $2; }
	;

property_flag
	: T_KEY_IN			{ $$ = CPG_PROPERTY_FLAG_IN; }
	| T_KEY_OUT			{ $$ = CPG_PROPERTY_FLAG_OUT; }
	| T_KEY_INTEGRATED		{ $$ = CPG_PROPERTY_FLAG_INTEGRATED; }
	| T_KEY_ONCE			{ $$ = CPG_PROPERTY_FLAG_ONCE; }
	;

action
	: identifier_or_string '<' '=' value_as_string
					{ cpg_parser_context_add_action (context, $1, $4); errb }
	;

selector_item
	: selector_pseudo
	| selector_identifier
	| selector_regex
	;

selector_items
	: selector_item
	| selector_items '.'		{ cpg_parser_context_push_selector_pseudo (context,
	                                                                           CPG_SELECTOR_PSEUDO_TYPE_CHILDREN,
	                                                                           NULL); errb }
	  selector_item
	| selector_items '|' selector_item
	;

selector
	: selector_items		{ $$ = cpg_parser_context_pop_selector (context); errb;
	                                  cpg_selector_prepend_pseudo ($$,
	                                                               CPG_SELECTOR_PSEUDO_TYPE_CHILDREN,
	                                                               NULL); }
	| strict_selector
	;

selector_self
	: selector_items		{ $$ = cpg_parser_context_pop_selector (context); errb; }
	| strict_selector
	;

strict_selector
	: '|' selector_items		{ $$ = cpg_parser_context_pop_selector (context); errb }
	| '.'				{ cpg_parser_context_push_selector_pseudo (context,
	                                                                           CPG_SELECTOR_PSEUDO_TYPE_CHILDREN, NULL); errb }
	  selector_items		{ $$ = cpg_parser_context_pop_selector (context); errb }
	;

selector_parse
	: selector_items
	| '|' selector_items
	| '.' selector_items		{ cpg_parser_context_push_selector_pseudo (context,
	                                                                           CPG_SELECTOR_PSEUDO_TYPE_CHILDREN, NULL); errb }
	;

selector_identifier
	: identifier_or_string 		{ cpg_parser_context_push_selector_identifier (context, $1); errb }
	;

selector_regex
	: regex				{ cpg_parser_context_push_selector_regex (context, $1); errb }
	;

selector_pseudo_simple_key
	: T_KEY_ROOT				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_ROOT; }
	| T_KEY_PARENT				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_PARENT; }
	| T_KEY_FIRST				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_FIRST; }
	| T_KEY_LAST				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_LAST; }
	| T_KEY_STATES				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_STATES; }
	| T_KEY_LINKS				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_LINKS; }
	| T_KEY_TEMPLATES			{ $$ = CPG_SELECTOR_PSEUDO_TYPE_TEMPLATES; }
	| T_KEY_COUNT				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_COUNT; }
	| T_KEY_SELF				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_SELF; }
	| T_KEY_DEBUG				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_DEBUG; }
	| T_KEY_CHILDREN			{ $$ = CPG_SELECTOR_PSEUDO_TYPE_CHILDREN; }
	| T_KEY_GROUPS				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_GROUPS; }
	| T_KEY_OBJECTS				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_OBJECTS; }
	| T_KEY_PROPERTIES			{ $$ = CPG_SELECTOR_PSEUDO_TYPE_PROPERTIES; }
	| T_KEY_ACTIONS				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_ACTIONS; }
	| T_KEY_FUNCTIONS			{ $$ = CPG_SELECTOR_PSEUDO_TYPE_FUNCTIONS; }
	;

selector_pseudo_simple
	: selector_pseudo_simple_key		{ cpg_parser_context_push_selector_pseudo (context, $1, NULL); }
	;

selector_pseudo_selector_key
	: T_KEY_FROM				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_FROM; }
	| T_KEY_TO				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_TO; }
	;

selector_pseudo_nth_key
	: T_KEY_SIBLINGS			{ $$ = CPG_SELECTOR_PSEUDO_TYPE_SIBLINGS; }
	| T_KEY_SUBSET				{ $$ = CPG_SELECTOR_PSEUDO_TYPE_SUBSET; }
	;

selector_pseudo_nth_args_rev
	: value_as_string			{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_nth_args_rev ',' value_as_string
						{ $$ = g_slist_prepend ($1, $3); }
	;

selector_pseudo_nth_args
	: selector_pseudo_nth_args_rev		{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_selector_args_rev
	: selector_self				{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_selector_args_rev ',' { cpg_parser_context_push_selector (context); }
	  selector_self				{ $$ = g_slist_prepend ($1, $4); }
	;

selector_pseudo_selector_args
	: selector_pseudo_selector_args_rev	{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_with_args
	: selector_pseudo_selector_key
	  '('					{ cpg_parser_context_push_selector (context); }
	  selector_pseudo_selector_args
	  ')'					{ cpg_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           $4); }
	| selector_pseudo_nth_key '(' selector_pseudo_nth_args ')'
						{ cpg_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           $3); }
	| selector_pseudo_nth_key '(' ')'
						{ cpg_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	| selector_pseudo_nth_key
						{ cpg_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	;

selector_pseudo
	: selector_pseudo_simple
	| selector_pseudo_with_args
	;

import
	: T_KEY_IMPORT value_as_string T_KEY_AS identifier_or_string
					{ cpg_parser_context_import (context, $4, $2); errb }
	;

layout
	: attributes
	  T_KEY_LAYOUT
	  '{'			{ cpg_parser_context_push_layout (context, $1); }
	  layout_contents
	  '}'			{ cpg_parser_context_pop (context); }
	|
	  attributes
	  T_KEY_LAYOUT          { cpg_parser_context_push_layout (context, $1); }
	  layout_item           { cpg_parser_context_pop (context); }
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

layout_relative
	:				{ $$ = NULL; }
	| T_KEY_OF selector		{ $$ = $2; }
	;

layout_item_relative
	: selector
	  relation
	  selector			{ cpg_parser_context_add_layout (context, $2, $1, $3); }
	| relation
	  selector			{ cpg_parser_context_add_layout (context, $1, NULL, $2); }
	;

layout_item_absolute
	: selector
	  T_KEY_AT
	  '('
	  value_as_string
	  ','
	  value_as_string
	  ')'
	  layout_relative		{ cpg_parser_context_add_layout_position (context, $1, $4, $6, $8); }
	| T_KEY_AT
	  '('
	  value_as_string
	  ','
	  value_as_string
	  ')'
	  layout_relative		{ cpg_parser_context_add_layout_position (context, NULL, $3, $5, $7); }
	;

layout_item
	: layout_item_relative
	| layout_item_absolute
	;

layout_item_or_others
	: layout_item
	| common_scopes
	| attributes
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  layout_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

layout_contents
	:
	| layout_contents layout_item_or_others
	;

identifier
	: T_IDENTIFIER			{ $$ = cpg_embedded_string_new_from_string ($1); }
	;

double
	: T_DOUBLE			{ $$ = cpg_embedded_string_new_from_double ($1); }
	;

integer
	: T_INTEGER			{ $$ = cpg_embedded_string_new_from_integer ($1); }
	;

value_as_string
	: string
	| equation
	| indirection
	| identifier
	| integer
	| double
	;

string_item
	: equation_inside
	| indirection_inside
	| T_STRING			{ cpg_embedded_string_add_text (cpg_parser_context_peek_string (context), $1); }
	;

string_contents
	:
	| string_contents string_item
	;

string
	: T_STRING_BEGIN		{ cpg_parser_context_push_string (context); }
	  string_contents
	  T_STRING_END			{ $$ = cpg_parser_context_pop_string (context); }
	;

equation
	: T_EQUATION_BEGIN		{
						cpg_embedded_string_push (cpg_parser_context_push_string (context),
						                          CPG_EMBEDDED_STRING_NODE_EQUATION,
						                          0);
					}
	  string_contents
	  T_EQUATION_END		{ $$ = cpg_embedded_string_pop (cpg_parser_context_pop_string (context)); }
	;

indirection_embedding
	: T_INDIRECTION_EMBEDDING_BEGIN
	  string_contents
	;

indirection_contents
	: T_STRING			{ cpg_embedded_string_add_text (cpg_parser_context_peek_string (context),
	                                                                $1); }
	| T_STRING 			{ cpg_embedded_string_add_text (cpg_parser_context_peek_string (context),
	                                                                $1); }
	  indirection_embedding
	| indirection_embedding
	;

indirection
	: T_INDIRECTION_BEGIN		{
						cpg_embedded_string_push (cpg_parser_context_push_string (context),
						                          CPG_EMBEDDED_STRING_NODE_INDIRECTION,
						                          $1);
					}
	  indirection_contents
	  T_INDIRECTION_END		{ $$ = cpg_embedded_string_pop (cpg_parser_context_pop_string (context)); }
	;

regex
	: T_REGEX_BEGIN			{ cpg_parser_context_push_string (context); }
	  string_contents
	  T_REGEX_END			{ $$ = cpg_parser_context_pop_string (context); }
	;

equation_item
	: string_item
	| T_IDENTIFIER			{ cpg_embedded_string_push (cpg_parser_context_peek_string (context),
	                                                            CPG_EMBEDDED_STRING_NODE_INDIRECTION,
	                                                            0);
	                                  cpg_embedded_string_add_text (cpg_parser_context_peek_string (context),
	                                                                $1);
	                                  cpg_embedded_string_pop (cpg_parser_context_peek_string (context)); }
	;

equation_contents
	:
	| equation_contents equation_item
	;

equation_inside
	: T_EQUATION_BEGIN		{ cpg_embedded_string_push (cpg_parser_context_peek_string (context),
	                                                            CPG_EMBEDDED_STRING_NODE_EQUATION,
	                                                            0);}
	  equation_contents
	  T_EQUATION_END		{ cpg_embedded_string_pop (cpg_parser_context_peek_string (context)); }
	;

indirection_inside
	: T_INDIRECTION_BEGIN		{ cpg_embedded_string_push (cpg_parser_context_peek_string (context),
	                                                            CPG_EMBEDDED_STRING_NODE_INDIRECTION,
	                                                            $1);}
	  indirection_contents
	  T_INDIRECTION_END		{ cpg_embedded_string_pop (cpg_parser_context_peek_string (context)); }
	;

debug
	: T_KEY_DEBUG strict_selector		{ cpg_parser_context_debug_selector (context, $2); }
	| T_KEY_DEBUG value_as_string		{ cpg_parser_context_debug_string (context, $2); }
	| T_KEY_DEBUG T_KEY_CONTEXT		{ cpg_parser_context_debug_context (context); }
	;

delete_item
	: selector		{ cpg_parser_context_delete_selector (context, $1); errb }
	| common_scopes
	| attributes
	  '{'			{ cpg_parser_context_push_scope (context, $1); }
	  delete_contents
	  '}'			{ cpg_parser_context_pop (context); }
	;

delete
	: T_KEY_DELETE delete_item
	;

delete_contents
	:
	| delete_contents delete_item;

delete_context
	: attributes
	  T_KEY_DELETE
	  '{'				{ cpg_parser_context_push_scope (context, $1); }
	  delete_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

%%

static void
yyerror (YYLTYPE *locp, CpgParserContext *context, char *s)
{
	cpg_parser_context_set_error (context, s);
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
create_function_argument (CpgEmbeddedString *name,
                          gboolean     is_optional,
                          gdouble      default_value)
{
	return g_object_ref_sink (cpg_function_argument_new (cpg_embedded_string_expand (name, NULL),
	                                                     is_optional,
	                                                     default_value));
}
