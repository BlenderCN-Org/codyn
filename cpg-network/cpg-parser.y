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

%token T_KEY_STATE T_KEY_LINK T_KEY_NETWORK T_KEY_FUNCTION T_KEY_INTERFACE T_KEY_IMPORT T_KEY_INPUT_FILE T_KEY_POLYNOMIAL T_KEY_FROM T_KEY_TO T_KEY_PIECE T_KEY_TEMPLATES T_KEY_DEFINE T_KEY_INTEGRATOR T_KEY_GROUP T_KEY_LAYOUT T_KEY_AT T_KEY_OF T_KEY_ON T_KEY_PROXY T_KEY_INCLUDE T_KEY_DEBUG T_KEY_SELECTOR T_KEY_PROPERTY T_KEY_DELETE T_KEY_ACTION T_KEY_OR

%token <num> T_KEY_LEFT_OF T_KEY_RIGHT_OF T_KEY_BELOW T_KEY_ABOVE
%type <num> relation
%type <num> relation_item

%type <id> pseudo_identifier

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

%type <num> selector_type

%token <num> T_INDIRECTION_BEGIN
%token T_INDIRECTION_END

%token T_INDIRECTION_EMBEDDING_BEGIN

%token T_START_DOCUMENT
%token T_START_SELECTOR

%type <flags> property_flags
%type <flags> property_flags_contents
%type <flags> property_flag

%type <selector> layout_relative

%type <string> selector_pseudo_identifier

%type <array> double_list
%type <array> pseudo_args_list
%type <piece> polynomial_piece
%type <array> polynomial_pieces
%type <array> function_argument_list
%type <array> template_list
%type <array> pseudo_args
%type <argument> function_argument
%type <selector> selector

%type <string> identifier_or_string
%type <string> string
%type <string> regex
%type <string> equation
%type <string> indirection
%type <string> identifier
%type <string> value_as_string
%type <string> double
%type <string> integer

%type <list> attribute_arguments
%type <list> attribute_arguments_list
%type <list> attributes
%type <list> attributes_contents
%type <attribute> attribute_contents

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
	CpgPropertyFlags flags;
	gdouble numf;
	gint num;
	GSList *list;
	GArray *array;
	CpgFunctionPolynomialPiece *piece;
	CpgFunctionArgument *argument;
	CpgObject *object;
	CpgSelector *selector;
	CpgEmbeddedString *string;
	CpgAttribute *attribute;

	struct
	{
		gint parent;
		gint idx;
	} ref;
}

%start choose_parser

%expect 10

%%

choose_parser
	: T_START_DOCUMENT document
	| T_START_SELECTOR selector_parse
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
	| import
	| templates
	| define
	| layout
	| integrator
	| include
	| debug
	| delete
	| delete_context
	;

include_path
	: T_STRING_BEGIN
	  string_contents
	  T_STRING_END
	;

include
	: T_KEY_INCLUDE include_path
	;

network
	: T_KEY_NETWORK
	  '{'				{ cpg_parser_context_push_network (context); }
	  network_contents
	  '}'				{ cpg_parser_context_pop (context); }
	;

network_item
	: property
	| debug
	;

network_contents
	:
	| network_contents network_item
	;

integrator_item
	: identifier_or_string '=' value_as_string { cpg_parser_context_add_integrator_property (context, $1, $3); }
	;

integrator_contents
	:
	| integrator_contents integrator_item
	;

integrator
	: T_KEY_INTEGRATOR
	  '{'				{ cpg_parser_context_push_integrator (context); }
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
					                             TRUE); }
	| identifier_or_string '=' define_values
					{ cpg_parser_context_define (context,
					                             $1,
					                             g_slist_reverse ($3),
					                             FALSE); }
	| debug
	;

define_contents
	:
	| define_contents define_item
	;

define
	: T_KEY_DEFINE
	'{'
	define_contents
	'}'
	| T_KEY_DEFINE define_item
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
	| debug
	| layout
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

state
	: T_KEY_STATE
	  identifier_or_string
	  templated
	  '{' 				{ cpg_parser_context_push_state (context, $2, $3); errb }
	  state_contents
	  '}'				{ $$ = cpg_parser_context_pop (context); errb }
	;

state_in_group
	: '[' T_KEY_PROXY ']' state	{ cpg_parser_context_set_proxy (context, $4); errb }
	| state
	;

group
	: T_KEY_GROUP
	  identifier_or_string
	  templated
	  '{' 				{ cpg_parser_context_push_group (context, $2, $3); errb }
	  group_contents
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

attribute_arguments_list
	: value_as_string		{ $$ = g_slist_prepend (NULL, $1); }
	| attribute_arguments_list ',' value_as_string
					{ $$ = g_slist_prepend ($1, $3); }
	;

attribute_arguments
	:				{ $$ = NULL; }
	| '(' ')'			{ $$ = NULL; }
	| '(' attribute_arguments_list ')'
					{ $$ = $2; }
	;

attribute_contents
	: identifier_or_string attribute_arguments
					{ $$ = cpg_attribute_new (cpg_embedded_string_expand ($1, cpg_parser_context_get_embedded (context)),
					                          $2); }
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
	  '}'				{ cpg_parser_context_pop (context); errb }
	;

function_polynomial
	: T_KEY_POLYNOMIAL
	  identifier_or_string
	  '('
	  ')'
	  '{'
	  polynomial_pieces
	  '}'				{ cpg_parser_context_add_polynomial (context, $2, $6); errb }
	;

function_custom
	: identifier_or_string
	  '('
	  function_argument_list
	  ')'
	  '{'
	  value_as_string
	  '}'				{ cpg_parser_context_add_function (context, $1, $6, $3); errb }
	;

function_item
	: function_custom
	| function_polynomial
	;

function_items
	:
	| function_items function_item
	;

function
	: T_KEY_FUNCTION function_item
	| T_KEY_FUNCTION
	  '{'
	  function_items
	  '}'
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
	| identifier_or_string			{ $$ = create_function_argument ($1, FALSE, 0.0); }
	;

state_item
	: property
	| define
	| debug
	| layout
	;

state_contents
	:
	| state_contents state_item
	;

group_item
	: property
	| state_in_group
	| link
	| interface
	| group
	| define
	| debug
	| layout
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
	: identifier_or_string '=' selector	{ cpg_parser_context_add_interface (context, $1, $3); errb }
	;

link_item
	: action
	| property
	| define
	| debug
	| layout
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
	: identifier_or_string '=' value_as_string property_flags
					{ cpg_parser_context_add_property (context, $1, $3, $4); errb }
	;

property_flags_contents
	: property_flag			{ $$ = $1; }
	| property_flags property_flag	{ $$ = $1 | $2; }
	;

property_flags
	: 				{ $$ = 0; }
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

selector
	: selector_pseudo nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	| selector_identifier nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	| selector_regex nested_selector
					{ $$ = cpg_parser_context_pop_selector (context); errb }
	;

selector_parse
	: selector_pseudo nested_selector
	| selector_identifier nested_selector
	| selector_regex nested_selector
	;

selector_identifier
	: identifier_or_string 		{ cpg_parser_context_push_selector (context, $1, FALSE); errb }
	;

selector_regex
	: regex				{ cpg_parser_context_push_selector_regex (context, $1, FALSE); errb }
	;

selector_identifier_set
	: string			{ cpg_parser_context_push_selector (context, $1, TRUE); errb }
	;

selector_regex_set
	: regex				{ cpg_parser_context_push_selector_regex (context, $1, TRUE); errb }
	;

nested_selector
	:
	| nested_selector '.' selector_identifier
	| nested_selector '.' selector_regex
	| nested_selector '|' selector_identifier_set
	| nested_selector '|' selector_regex_set
	| nested_selector selector_pseudo
	;

pseudo_args
	: value_as_string			{ append_array (NULL, CpgEmbeddedString *, $1, $$ = arret); }
	| pseudo_args ',' value_as_string	{ append_array ($1, CpgEmbeddedString *, $3, $$ = arret); }
	;

pseudo_args_list
	:					{ $$ = NULL; }
	| pseudo_args				{ $$ = $1; }
	;

pseudo_identifier
	: T_IDENTIFIER			{ $$ = $1; }
	| T_KEY_TEMPLATES		{ $$ = g_strdup ("templates"); }
	;

selector_pseudo_identifier
	: '|' pseudo_identifier		{ $$ = cpg_embedded_string_new_from_string ($2); }
	;

selector_pseudo
	: selector_pseudo_identifier '(' pseudo_args_list ')'
					{ cpg_parser_context_push_selector_pseudo (context, $1, $3); errb }
	| selector_pseudo_identifier		{ cpg_parser_context_push_selector_pseudo (context, $1, NULL); errb }
	;

import
	: T_KEY_IMPORT identifier_or_string T_KEY_FROM value_as_string
					{ cpg_parser_context_import (context, $2, $4); errb }
	;

layout
	: T_KEY_LAYOUT
	  '{'			{ cpg_parser_context_push_layout (context); }
	  layout_items
	  '}'			{ cpg_parser_context_pop_layout (context); }
	| T_KEY_LAYOUT          { cpg_parser_context_push_layout (context); }
	  layout_item           { cpg_parser_context_pop_layout (context); }
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

layout_item_or_define
	: layout_item
	| define
	;

layout_items
	:
	| layout_items layout_item_or_define
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

selector_type
	:				{ $$ = CPG_SELECTOR_TYPE_OBJECT; }
	| T_KEY_STATE			{ $$ = CPG_SELECTOR_TYPE_STATE; }
	| T_KEY_LINK			{ $$ = CPG_SELECTOR_TYPE_LINK; }
	| T_KEY_GROUP			{ $$ = CPG_SELECTOR_TYPE_GROUP; }
	| T_KEY_ACTION			{ $$ = CPG_SELECTOR_TYPE_ACTION; }
	| T_KEY_PROPERTY		{ $$ = CPG_SELECTOR_TYPE_PROPERTY; }
	| T_KEY_FUNCTION		{ $$ = CPG_SELECTOR_TYPE_FUNCTION; }
	;

debug
	: T_KEY_DEBUG T_KEY_SELECTOR selector_type selector	{ cpg_parser_context_debug_selector (context, $3, $4); }
	| T_KEY_DEBUG value_as_string				{ cpg_parser_context_debug_string (context, $2); }
	;

delete_item
	: selector_type selector	{ cpg_parser_context_delete_selector (context, $1, $2); }
	;

delete
	: T_KEY_DELETE delete_item
	;

delete_items
	:
	| delete_items delete_item;

delete_context
	: T_KEY_DELETE
	  '{'
	  delete_items
	  '}'
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
