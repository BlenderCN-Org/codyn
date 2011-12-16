%{

#include "cdn-parser-context.h"
#include "cdn-parser.h"

static void cdn_parser_error (YYLTYPE *locp, CdnParserContext *context, char const *s);
int cdn_parser_lex(YYSTYPE *lvalp, YYLTYPE *llocp, void *scanner);

#define scanner (cdn_parser_context_get_scanner (context))

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
	if (cdn_parser_context_get_error (context) != NULL)		\
	{								\
		YYERROR;						\
	}

static CdnFunctionPolynomialPiece *create_polynomial_piece (gchar const *start,
                                                            gchar const *end,
                                                            GArray  *coefficients);

%}

%token T_KEY_IN T_KEY_INTEGRATED T_KEY_ONCE T_KEY_OUT

%token T_KEY_EDGE T_KEY_FUNCTIONS T_KEY_INTERFACE T_KEY_IMPORT T_KEY_INPUT_FILE T_KEY_POLYNOMIAL T_KEY_FROM T_KEY_TO T_KEY_PIECE T_KEY_TEMPLATES T_KEY_TEMPLATES_ROOT T_KEY_DEFINES T_KEY_INTEGRATOR T_KEY_NODE T_KEY_LAYOUT T_KEY_AT T_KEY_OF T_KEY_ON T_KEY_INCLUDE T_KEY_DEBUG T_KEY_DEBUG_PRINT T_KEY_VARIABLE T_KEY_DELETE T_KEY_ACTION T_KEY_ROOT T_KEY_CHILDREN T_KEY_PARENT T_KEY_FIRST T_KEY_LAST T_KEY_SUBSET T_KEY_SIBLINGS T_KEY_EDGES T_KEY_COUNT T_KEY_SELF T_KEY_CONTEXT T_KEY_AS T_KEY_EACH T_KEY_PROXY T_KEY_BIDIRECTIONAL T_KEY_OBJECTS T_KEY_NODES T_KEY_IMPORTS T_KEY_VARIABLES T_KEY_ACTIONS T_KEY_IF T_KEY_SETTINGS T_KEY_NAME T_KEY_DESCENDANTS T_KEY_ANCESTORS T_KEY_UNIQUE T_KEY_IS_EMPTY T_KEY_REMOVE T_KEY_NO_SELF T_KEY_PROBABILITY T_KEY_FROM_SET T_KEY_TYPE T_KEY_PARSE T_KEY_HAS_FLAG T_KEY_HAS_TEMPLATE T_KEY_HAS_TAG T_KEY_TAG T_KEY_ALL T_KEY_APPLY T_KEY_UNAPPLY T_KEY_REVERSE T_KEY_WITH T_KEY_OBJECT T_STRING_REDUCE_BEGIN T_STRING_REDUCE_END T_STRING_MAP_BEGIN T_STRING_MAP_END T_CONDITION_BEGIN T_CONDITION_END T_KEY_DISABLED T_KEY_WHEN T_KEY_SOURCE T_KEY_SINK T_KEY_SOURCE_NAME T_KEY_SINK_NAME T_KEY_PHASE T_KEY_TERMINATE T_KEY_ANY T_KEY_SET

%token <num> T_KEY_LEFT_OF T_KEY_RIGHT_OF T_KEY_BELOW T_KEY_ABOVE
%type <num> relation
%type <num> relation_item

%type <num> when_direction

%token <id> T_DOUBLE
%token <id> T_INTEGER

%token <id> T_IDENTIFIER
%token <id> T_STRING
%token T_ANNOTATION_START T_ANNOTATION_END

%token T_EOF

%token T_STRING_BEGIN
%token T_STRING_END

%token T_REGEX_BEGIN
%token T_REGEX_END

%token T_EQUATION_BEGIN
%token T_EQUATION_END

%type <num> selector_pseudo_simple_key
%type <num> selector_pseudo_simple_key_real
%type <num> selector_pseudo_selector_key
%type <num> selector_pseudo_selector_key_real
%type <num> selector_pseudo_strargs_key
%type <num> selector_pseudo_strargs_key_real

%type <list> selector_pseudo_strargs_args
%type <list> selector_pseudo_strargs_args_rev
%type <list> selector_pseudo_selector_args
%type <list> selector_pseudo_selector_args_rev

%type <string> selector_pseudo_hasflag_arg
%type <list> selector_pseudo_hasflag_args
%type <list> selector_pseudo_hasflag_args_rev

%type <object> selector_or_string_list_item
%type <list> selector_or_string_list
%type <list> selector_or_string_list_rev

%type <string> identifier_or_string_or_nothing

%token <num> T_INDIRECTION_BEGIN
%token T_INDIRECTION_END

%token T_INDIRECTION_EMBEDDING_BEGIN
%token T_INDIRECTION_EMEDDING_END

%token T_START_DOCUMENT
%token T_START_SELECTOR
%token T_START_NODE
%token T_START_EDGE

%type <num> variable_flag_sign
%type <flags> variable_flags
%type <flags> variable_flags_strict
%type <flags> variable_flags_contents
%type <num> variable_flag
%type <num> assign_optional

%type <selector> layout_relative
%type <num> layout_item_separator

%type <array> double_list
%type <piece> polynomial_piece
%type <list> polynomial_pieces
%type <list> polynomial_pieces_rev

%type <string> whenfrom

%type <argspec> function_argument
%type <list> function_argument_list
%type <list> function_argument_list_rev
%type <list> function_argument_list_or_empty

%type <list> function_argument_implicit
%type <argspec> function_argument_impl
%type <list> function_argument_list_impl
%type <list> function_argument_list_impl_rev
%type <list> function_argument_list_or_empty_impl

%type <list> template_list_more
%type <list> template_list_rev
%type <list> template_list

%type <selector> selector
%type <selector> selector_explicit
%type <selector> selector_non_ambiguous
%type <selector> selector_as_pseudo_arg
%type <selector> selector_non_ambiguous_as_pseudo_arg

%type <num> selector_pseudo_mixargs_key
%type <object> selector_pseudo_mixargs_arg
%type <list> selector_pseudo_mixargs_args
%type <list> selector_pseudo_mixargs_args_rev

%type <string> phase

%type <string> identifier_or_string
%type <string> identifier_or_string_item
%type <string> string
%type <string> regex
%type <string> equation
%type <string> condition
%type <string> indirection
%type <string> identifier
%type <string> value_as_string
%type <string> double
%type <string> integer

%type <string> constraint

%type <list> attributes
%type <list> attributes_strict
%type <list> attributes_contents
%type <attribute> attribute_contents
%type <attribute> attribute_proxy
%type <attribute> attribute_each
%type <attribute> attribute_bidirectional
%type <attribute> attribute_if
%type <attribute> attribute_with
%type <attribute> attribute_no_self
%type <attribute> attribute_probability
%type <attribute> attribute_tag

%type <list> string_list
%type <list> string_list_rev

%type <object> define_value

%type <list> edge_connect
%type <list> edge_connect_fast
%type <list> templated

%type <num> repeated_prime

%type <multiassign> multi_assign_identifier

%define api.pure
%name-prefix="cdn_parser_"

%parse-param {CdnParserContext *context}
%lex-param {void *scanner}
%error-verbose

%locations
%defines

%union
{
	gchar *id;
	CdnVariable *variable;
	gint num;
	GSList *list;
	GArray *array;
	CdnFunctionPolynomialPiece *piece;
	CdnFunctionArgument *argument;
	gpointer object;
	CdnSelector *selector;
	CdnEmbeddedString *string;
	CdnAttribute *attribute;
	CdnFunctionArgumentSpec *argspec;

	struct
	{
		gint parent;
		gint idx;
	} ref;

	struct
	{
		gint add;
		gint remove;
	} flags;

	struct
	{
		CdnEmbeddedString *name;
		CdnEmbeddedString *count;
	} multiassign;
}

%start choose_parser

%expect 15

%%

choose_parser
	:
	| T_START_DOCUMENT document_contents
	| T_START_SELECTOR selector_parse T_EOF
	| T_START_NODE node_contents
	| T_START_EDGE edge_contents
	;

document_contents
	:
	| document_contents document_item
	;

document_item
	: node_item_general
	| templates
	| integrator
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  document_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

eof
	: T_EOF
	;

include
	: attributes
	  T_KEY_INCLUDE
	  value_as_string	{ cdn_parser_context_include (context, $3, $1); errb }
	;

parse
	: attributes
	  T_KEY_PARSE
	  value_as_string
	  '{'				{ cdn_parser_context_push_define (context, $1); }
	  define_contents
	  '}'				{ cdn_parser_context_push_input_from_path (context, $3, $1); errb;
	                                  cdn_parser_context_pop (context); }
	;

common_scopes
	: define
	| debug
	| include
	| parse
	| actions
	| eof
	| annotation
	;

annotation
	: T_ANNOTATION_START		{ cdn_parser_context_push_string (context); }
	  string_contents
	  T_ANNOTATION_END		{ cdn_parser_context_push_annotation (context,
	                                                                      cdn_parser_context_pop_string (context)); }
	;

action_apply
	: T_KEY_APPLY selector T_KEY_TO selector
					{ cdn_parser_context_apply_template (context,
					                                     $2, $4); }
	| T_KEY_APPLY selector		{ cdn_parser_context_apply_template (context,
	                                                                     $2, NULL); }
	;

action_unapply
	: T_KEY_UNAPPLY selector T_KEY_FROM selector
					{ cdn_parser_context_unapply_template (context,
					                                       $2, $4); }
	| T_KEY_UNAPPLY selector	{ cdn_parser_context_unapply_template (context,
					                                       $2, NULL); }
	;

actions
	: action_apply
	| action_unapply
	;

integrator_variable
	: identifier_or_string '=' value_as_string { cdn_parser_context_add_integrator_variable (context, $1, $3); }
	;

integrator_item
	: integrator_variable
	| common_scopes
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  integrator_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

integrator_contents
	:
	| integrator_contents integrator_item
	;

integrator
	: attributes
	  T_KEY_INTEGRATOR
	  '{'				{ cdn_parser_context_push_integrator (context, $1); }
	  integrator_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

when_direction
	:				{ $$ = CDN_EVENT_DIRECTION_NEGATIVE; }
	| '='				{ $$ = CDN_EVENT_DIRECTION_ZERO; }
	| '>'				{ $$ = CDN_EVENT_DIRECTION_POSITIVE; }
	| '<'				{ $$ = CDN_EVENT_DIRECTION_NEGATIVE; }
	| '<' '>'			{ $$ = CDN_EVENT_DIRECTION_POSITIVE |
	                                       CDN_EVENT_DIRECTION_NEGATIVE; }
	;

when_item
	: T_KEY_SET selector '=' value_as_string	{ cdn_parser_context_add_event_set_variable (context, $2, $4); }
	| node_item
	;

when_contents
	:
	| when_contents when_item
	;

whenfrom
	: T_KEY_ANY			{ $$ = NULL; }
	| value_as_string		{ $$ = $1; }
	;

when
	: T_KEY_PHASE
	  whenfrom
	  T_KEY_TO
	  value_as_string
	  T_KEY_WHEN
	  when_direction
	  value_as_string
	  '{'				{ cdn_parser_context_push_event (context, $2, $4, $7, $6, NULL, NULL); }
	  when_contents
	  '}'				{ cdn_parser_context_pop (context); }
	| T_KEY_PHASE
	  whenfrom
	  T_KEY_TO
	  T_KEY_TERMINATE
	  T_KEY_WHEN
	  when_direction
	  value_as_string
	  '{'				{ cdn_parser_context_push_event (context, $2, NULL, $7, $6, NULL, NULL); }
	  when_contents
	  '}'
	;

define_value
	: value_as_string		{ $$ = $1; }
	| selector_non_ambiguous	{ $$ = $1; }
	;

define_item
	: multi_assign_identifier '=' define_value
					{ cdn_parser_context_define (context,
					                             $1.name,
					                             $3,
					                             FALSE,
					                             $1.count); }
	| multi_assign_identifier '?' '=' define_value
					{ cdn_parser_context_define (context,
					                             $1.name,
					                             $4,
					                             TRUE,
					                             $1.count); }
	| common_scopes
	| attributes_strict
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  define_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

define_contents
	:
	| define_contents define_item
	;

define
	: attributes
	  T_KEY_DEFINES
	  '{'				{ cdn_parser_context_push_define (context, $1); }
	  define_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

templates
	: attributes
	  T_KEY_TEMPLATES
	  '{'				{ cdn_parser_context_push_templates (context, $1); }
	  template_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

template_item
	: edge
	| object
	| node
	| import
	| common_scopes
	| layout
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  template_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

template_contents
	:
	| template_contents template_item;
	;

template_list_more
	: ',' selector
					{ $$ = g_slist_prepend (NULL, $2); }
	| template_list_more ',' selector
					{ $$ = g_slist_prepend ($1, $3); }
	;

template_list_rev
	: ':' selector template_list_more { $$ = g_slist_append ($3, $2); }
	| ':' selector			{ $$ = g_slist_prepend (NULL, $2); }
	;

template_list
	: template_list_rev		{ $$ = g_slist_reverse ($1); }
	;

templated
	:				{ $$ = NULL; }
	| template_list			{ $$ = $1; }
	;

identifier_or_string_or_nothing
	:				{ $$ = NULL; }
	| identifier_or_string		{ $$ = $1; }
	;

input_file_setting
	: value_as_string '=' value_as_string
					{ cdn_parser_context_set_input_file_setting (context, $1, $3); }
	;

input_file_settings_contents
	:
	| input_file_settings_contents input_file_setting
	;

input_file_settings
	: T_KEY_SETTINGS
	  '{'
	  input_file_settings_contents
	  '}'
	;

input_file_item
	: input_file_settings
	| variable
	| common_scopes
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  input_file_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

input_file_contents
	:
	| input_file_contents input_file_item;

input_file
	: attributes
	  T_KEY_INPUT_FILE
	  value_as_string
	  T_KEY_AS
	  identifier_or_string
	  '{'				{ cdn_parser_context_push_input_file (context, $5, $3, $1); errb }
	  input_file_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

node
	: attributes
	  T_KEY_NODE
	  identifier_or_string_or_nothing
	  templated
	  '{' 				{ cdn_parser_context_push_node (context, $3, $4, $1); errb }
	  node_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	| attributes
	  T_KEY_NODE
	  selector_non_ambiguous
	  templated
	  '{'				{ cdn_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CDN_SELECTOR_TYPE_NODE,
	                                                                     $4,
	                                                                     $1); errb }
	  node_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	;

selector_or_string_list_item
	: value_as_string		{ $$ = $1; }
	| selector_non_ambiguous	{ $$ = $1; }
	;

selector_or_string_list_rev
	: selector_or_string_list_item	{ $$ = g_slist_prepend (NULL, $1); }
	| selector_or_string_list
	  ','
	  selector_or_string_list_item	{ $$ = g_slist_prepend ($1, $3); }
	;

selector_or_string_list
	: selector_or_string_list_rev	{ $$ = g_slist_reverse ($1); }
	;

attribute_proxy
	: T_KEY_PROXY			{ $$ = cdn_attribute_new ("proxy"); }
	| T_KEY_PROXY '(' ')'		{ $$ = cdn_attribute_new ("proxy"); }
	;

attribute_each
	: T_KEY_EACH '(' ')'		{ $$ = cdn_attribute_new ("each"); }
	| T_KEY_EACH
	  '('
	  selector_or_string_list
	  ')'				{ $$ = cdn_attribute_new ("each");
					  cdn_attribute_set_arguments ($$, $3); }
	;

attribute_bidirectional
	: T_KEY_BIDIRECTIONAL		{ $$ = cdn_attribute_new ("bidirectional"); }
	| T_KEY_BIDIRECTIONAL '(' ')'	{ $$ = cdn_attribute_new ("bidirectional"); }
	| T_KEY_BIDIRECTIONAL '(' value_as_string ',' value_as_string ')'
					{ $$ = cdn_attribute_newv ("bidirectional", $3, $5, NULL); }
	;

attribute_if
	: T_KEY_IF '(' ')'		{ $$ = cdn_attribute_new ("if"); }
	| T_KEY_IF
	  '('
	  selector_or_string_list
	  ')'				{ $$ = cdn_attribute_new ("if");
					  cdn_attribute_set_arguments ($$, $3); }
	;

attribute_with
	: T_KEY_WITH '(' selector ')'	{ $$ = cdn_attribute_newv ("with", $3, NULL); }
	;

string_list_rev
	: value_as_string			{ $$ = g_slist_prepend (NULL, $1); }
	| string_list ',' value_as_string	{ $$ = g_slist_prepend ($1, $3); }
	;

string_list
	: string_list_rev			{ $$ = g_slist_reverse ($1); }
	;

attribute_no_self
	: T_KEY_NO_SELF '(' ')' { $$ = cdn_attribute_new ("no-self"); }
	| T_KEY_NO_SELF { $$ = cdn_attribute_newv ("no-self", NULL); }
	;

attribute_probability
	: T_KEY_PROBABILITY '(' value_as_string ')' { $$ = cdn_attribute_newv ("probability", $3, NULL); }
	;

attribute_tag
	: T_KEY_TAG '(' ')'		{ $$ = cdn_attribute_new ("tag"); }
	| T_KEY_TAG '(' string_list ')' { $$ = cdn_attribute_new ("tag");
	                                  cdn_attribute_set_arguments ($$, $3); }
	;

attribute_contents
	: attribute_proxy
	| attribute_each
	| attribute_bidirectional
	| attribute_if
	| attribute_with
	| attribute_no_self
	| attribute_probability
	| attribute_tag
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

attributes_strict
	: '[' ']'			{ $$ = NULL; }
	| '[' attributes_contents ']'	{ $$ = g_slist_reverse ($2); }
	;

edge_connect_fast
	: T_KEY_FROM selector T_KEY_TO selector
					{ $$ = g_slist_prepend (NULL, GINT_TO_POINTER (0));
					  $$ = g_slist_prepend ($$, $4);
					  $$ = g_slist_prepend ($$, $2);
					}
	| T_KEY_ALL selector		{ $$ = g_slist_prepend (NULL, GINT_TO_POINTER (0));
					  $$ = g_slist_prepend ($$, NULL);
					  $$ = g_slist_prepend ($$, $2);
					}
	| T_KEY_ON selector		{ $$ = g_slist_prepend (NULL, GINT_TO_POINTER (1));
					  $$ = g_slist_prepend ($$, NULL);
					  $$ = g_slist_prepend ($$, $2);
					}
	;

edge_connect
	:				{ $$ = NULL; }
	| edge_connect_fast		{ $$ = $1; }
	;

phase
	:				{ $$ = NULL; }
	| T_KEY_PHASE value_as_string	{ $$ = $2; }
	;

edge
	: attributes
	  T_KEY_EDGE
	  identifier_or_string
	  edge_connect
	  phase
	  templated
	  '{'	 			{ cdn_parser_context_push_edge (context,
	                                                                $3,
	                                                                $6,
	                                                                $1,
	                                                                $4,
	                                                                $5); errb }
	  edge_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	| attributes
	  T_KEY_EDGE
	  edge_connect_fast
	  phase
	  templated
	  '{'				{ cdn_parser_context_push_edge (context,
	                                                                NULL,
	                                                                $5,
	                                                                $1,
	                                                                $3,
	                                                                $4); errb }
	  edge_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	| attributes
	  T_KEY_EDGE
	  phase
	  templated
	  '{'				{ cdn_parser_context_push_edge (context,
	                                                                NULL,
	                                                                $4,
	                                                                $1,
	                                                                NULL,
	                                                                $3); errb }
	  edge_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	| attributes
	  T_KEY_EDGE
	  selector_non_ambiguous
	  templated
	  '{'				{ cdn_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CDN_SELECTOR_TYPE_EDGE,
	                                                                     $4,
	                                                                     $1); }
	  edge_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	;

object
	: attributes
	  T_KEY_OBJECT
	  selector_non_ambiguous
	  templated
	  '{'				{ cdn_parser_context_push_selection (context,
	                                                                     $3,
	                                                                     CDN_SELECTOR_TYPE_OBJECT,
	                                                                     $1,
	                                                                     $4); }
	  object_contents
	  '}'				{ cdn_parser_context_pop (context); errb }
	;

function_polynomial
	: attributes
	  T_KEY_POLYNOMIAL
	  identifier_or_string
	  '('
	  ')'
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  polynomial_pieces
	  '}'				{ cdn_parser_context_add_polynomial (context, $3, $8, $1); errb
	                                  cdn_parser_context_pop (context); errb }
	;

function_argument_list_or_empty
	:				{ $$ = NULL; }
	| function_argument_list	{ $$ = $1; }
	;

function_argument_list_or_empty_impl
	:				{ $$ = NULL; }
	| function_argument_list_impl	{ $$ = $1; }
	;

function_argument_implicit
	:				{ $$ = NULL; }
	| '(' function_argument_list_or_empty_impl ')'
					{ $$ = $2; }
	;

function_helper
	:
	| '{'
	  object_contents
	  '}'
	;

function_custom
	: attributes
	  identifier_or_string
	  '('
	  function_argument_list_or_empty
	  ')'
	  function_argument_implicit
	  assign_optional
	  value_as_string		{ cdn_parser_context_push_function (context,
	                                                                    $2,
	                                                                    g_slist_concat ($4, $6),
	                                                                    $8,
	                                                                    $7,
	                                                                    $1); errb }
	  function_helper	        { cdn_parser_context_pop (context); errb }
	;

function
	: function_custom
	| function_polynomial
	;

polynomial_pieces_rev
	: 				{ $$ = NULL; }
	| polynomial_pieces polynomial_piece
					{ $$ = g_slist_prepend ($1, $2); }
	;

polynomial_pieces
	: polynomial_pieces_rev		{ $$ = g_slist_reverse ($1); }
	;

polynomial_piece
	: T_KEY_PIECE T_KEY_FROM T_DOUBLE T_KEY_TO T_DOUBLE '=' double_list
					{ $$ = create_polynomial_piece ($3, $5, $7); }
	;

double_list
	: T_DOUBLE			{ append_array (NULL, gchar *, $1, $$ = arret); }
	| T_INTEGER			{ append_array (NULL, gchar *, $1, $$ = arret); }
	| double_list ',' T_DOUBLE	{ append_array ($1, gchar *, $3, $$ = arret); }
	| double_list ',' T_INTEGER	{ append_array ($1, gchar *, $3, $$ = arret); }
	;

function_argument_impl
	: identifier_or_string			{ $$ = cdn_function_argument_spec_new ($1, FALSE); }
	| T_KEY_FROM '.' identifier_or_string	{ $$ = cdn_function_argument_spec_new (cdn_embedded_string_prepend_text ($3, "from."), FALSE); }
	| T_KEY_TO '.' identifier_or_string	{ $$ = cdn_function_argument_spec_new (cdn_embedded_string_prepend_text ($3, "to."), FALSE); }
	;

function_argument_list_impl_rev
	: function_argument_impl		{ $$ = g_slist_prepend (NULL, $1); }
	| function_argument_list_impl
	  ','
	  function_argument_impl		{ $$ = g_slist_prepend ($1, $3); }
	;

function_argument_list_impl
	: function_argument_list_impl_rev	{ $$ = g_slist_reverse ($1); }
	;

function_argument_list_rev
	: function_argument			{ $$ = g_slist_prepend (NULL, $1); }
	| function_argument_list
	  ','
	  function_argument			{ $$ = g_slist_prepend ($1, $3); }
	;

function_argument_list
	: function_argument_list_rev	{ $$ = g_slist_reverse ($1); }
	;

function_argument
	: identifier_or_string			        { $$ = cdn_function_argument_spec_new ($1, TRUE); }
	;

object_item
	: variable
	| common_scopes
	| layout
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  object_contents
	  '}'				{ cdn_parser_context_pop (context); }
	| when
	;

object_contents
	:
	| object_contents object_item
	;

node_item_general
	: variable
	| object
	| edge
	| interface
	| node
	| common_scopes
	| layout
	| function
	| input_file
	| import
	| delete
	| delete_context
	| when
	;

node_item
	: node_item_general
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  node_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

node_contents
	:
	| node_contents node_item
	;

interface
	: attributes
	  T_KEY_INTERFACE
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  interface_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

interface_contents
	:
	| interface_contents interface_item
	;

interface_variable
	: attributes
	  identifier_or_string
	  assign_optional
	  identifier_or_string
	  T_KEY_IN
	  identifier_or_string	{ cdn_parser_context_add_interface (context,
	                                                            $2,
	                                                            $6,
	                                                            $4,
	                                                            $3,
	                                                            $1); errb }
	;

interface_item
	: interface_variable
	| common_scopes
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  interface_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

edge_item
	: action
	| variable
	| common_scopes
	| layout
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  edge_contents
	  '}'				{ cdn_parser_context_pop (context); }
	| when
	;

edge_contents
	:
	| edge_contents edge_item
	;

identifier_or_string_item
	: identifier
	| string
	| indirection
	;

identifier_or_string
	: identifier_or_string_item repeated_prime
	                                { $$ = $1;
	                                  {
	                                    int i;

	                                    for (i = 0; i < $2; ++i)
	                                    {
	                                      cdn_embedded_string_add_text ($$, "'");
	                                    }
	                                  }
	                                }
	| identifier_or_string_item	{ $$ = $1; }
	;

assign_optional
	: '='				{ $$ = FALSE; }
	| '?' '='			{ $$ = TRUE; }
	;

multi_assign_identifier
	: identifier_or_string ',' identifier_or_string
					{ $$.name = $1; $$.count = $3; }
	| identifier_or_string		{ $$.name = $1; $$.count = NULL; }
	;

constraint
	:				{ $$ = NULL; }
	| '(' value_as_string ')'	{ $$ = $2; }
	| '(' value_as_string ',' value_as_string ')' { $$ = cdn_embedded_string_new ();
	                                                cdn_embedded_string_add_text ($$, "clip(");
	                                                cdn_embedded_string_push ($$, CDN_EMBEDDED_STRING_NODE_INDIRECTION, 1);
	                                                cdn_embedded_string_add_text ($$, "0");
	                                                cdn_embedded_string_pop ($$);
	                                                cdn_embedded_string_add_text ($$, ", ");
	                                                cdn_embedded_string_add_string ($$, $2);
	                                                cdn_embedded_string_add_text ($$, ", ");
	                                                cdn_embedded_string_add_string ($$, $4);
	                                                cdn_embedded_string_add_text ($$, ")"); }
	| '(' value_as_string ':' value_as_string ')' { $$ = cdn_embedded_string_new ();
	                                                cdn_embedded_string_add_text ($$, "cycle(");
	                                                cdn_embedded_string_push ($$, CDN_EMBEDDED_STRING_NODE_INDIRECTION, 1);
	                                                cdn_embedded_string_add_text ($$, "0");
	                                                cdn_embedded_string_pop ($$);
	                                                cdn_embedded_string_add_text ($$, ", ");
	                                                cdn_embedded_string_add_string ($$, $2);
	                                                cdn_embedded_string_add_text ($$, ", ");
	                                                cdn_embedded_string_add_string ($$, $4);
	                                                cdn_embedded_string_add_text ($$, ")"); }
	;

variable
	: attributes
	  multi_assign_identifier
	  assign_optional
	  value_as_string
	  variable_flags
	  constraint
					{ cdn_parser_context_add_variable (context,
					                                   $2.name,
					                                   $2.count,
					                                   $4,
					                                   $5.add,
					                                   $5.remove,
					                                   $1,
					                                   $3,
					                                   $6); errb }
	| attributes
	  multi_assign_identifier
	  variable_flags_strict
					{ cdn_parser_context_add_variable (context,
					                                   $2.name,
					                                   $2.count,
					                                   NULL,
					                                   $3.add,
					                                   $3.remove,
					                                   $1,
					                                   FALSE,
					                                   NULL); errb }
	;

variable_flag_sign
	:				{ $$ = 1; }
	| '-'				{ $$ = 0; }
	| '+'				{ $$ = 1; }
	;

variable_flags_contents
	: variable_flag_sign variable_flag	{ $$.add = 0; $$.remove = 0; ($1 ? (($$.add) = $2) : (($$.remove) = $2)); }
	| variable_flags_contents variable_flag_sign variable_flag
					{ $2 ? (($$.add) |= $3) : (($$.remove) |= $3); }
	;

variable_flags_strict
	: '|' variable_flags_contents	{ $$ = $2; }
	;

variable_flags
	: 				{ $$.add = 0; $$.remove = 0; }
	| variable_flags_strict
	;

variable_flag
	: T_KEY_IN			{ $$ = CDN_VARIABLE_FLAG_IN; }
	| T_KEY_OUT			{ $$ = CDN_VARIABLE_FLAG_OUT; }
	| T_KEY_INTEGRATED		{ $$ = CDN_VARIABLE_FLAG_INTEGRATED; }
	| T_KEY_ONCE			{ $$ = CDN_VARIABLE_FLAG_ONCE; }
	;

action
	: attributes
	  identifier_or_string
	  '<' '='
	  value_as_string
	  phase
					{ cdn_parser_context_add_action (context, $2, $5, $1, $6); errb }
	;

selector_item_non_ambiguous
	: selector_pseudo
	| selector_regex
	;

selector_item
	: selector_identifier
	| selector_item_non_ambiguous
	;

selector_items
	: selector_item
	| selector_items '.'		{ cdn_parser_context_push_selector_pseudo (context,
	                                                                           CDN_SELECTOR_PSEUDO_TYPE_CHILDREN,
	                                                                           NULL); errb }
	  selector_item
	| selector_items '|' selector_item
	;

selector_explicit
	: '|'				{ cdn_selector_set_implicit_children (cdn_parser_context_peek_selector (context), TRUE); }
	  selector_items		{ $$ = cdn_parser_context_pop_selector (context); errb }
	;

selector
	: selector_items		{ $$ = cdn_parser_context_pop_selector (context); errb;
	                                  cdn_selector_set_implicit_children ($$, TRUE); }
	| selector_explicit
	;

selector_as_pseudo_arg
	: selector			{ $$ = $1; cdn_selector_set_implicit_children ($1, FALSE); }
	;

selector_non_ambiguous_beginning
	: selector_item_non_ambiguous
	| selector_item_non_ambiguous
	  '|'
	  selector_items
	| selector_item_non_ambiguous
	  '.'				{ cdn_parser_context_push_selector_pseudo (context,
	                                                                           CDN_SELECTOR_PSEUDO_TYPE_CHILDREN,
	                                                                           NULL); errb }
	  selector_items
	;


selector_non_ambiguous
	: selector_explicit		{ $$ = $1; }
	| selector_non_ambiguous_beginning
					{ $$ = cdn_parser_context_pop_selector (context); errb;
					  cdn_selector_set_implicit_children ($$, TRUE); }
	;

selector_non_ambiguous_as_pseudo_arg
	: selector_non_ambiguous	{ $$ = $1; cdn_selector_set_implicit_children ($1, FALSE); }
	;

selector_parse_contents
	: selector_items
	| '|' selector_items
	;

selector_parse
	: selector_parse_contents	{ cdn_selector_set_implicit_children (cdn_parser_context_peek_selector (context), TRUE); }
	;

selector_identifier
	:				{ cdn_parser_context_begin_selector_item (context); }
	  identifier_or_string 		{ cdn_parser_context_push_selector_identifier (context, $2); errb }
	;

selector_regex
	: regex				{ cdn_parser_context_push_selector_regex (context, $1); errb }
	;

selector_pseudo_simple_key_real
	: T_KEY_ROOT				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_ROOT; }
	| T_KEY_TEMPLATES_ROOT			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES_ROOT; }
	| T_KEY_PARENT				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_PARENT; }
	| T_KEY_FIRST				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_FIRST; }
	| T_KEY_LAST				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_LAST; }
	| T_KEY_EDGES				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_EDGES; }
	| T_KEY_TEMPLATES			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_TEMPLATES; }
	| T_KEY_NAME				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_NAME; }
	| T_KEY_SELF				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_SELF; }
	| T_KEY_FROM_SET			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_FROM_SET; }
	| T_KEY_TYPE				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_TYPE; }
	| T_KEY_CHILDREN			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_CHILDREN; }
	| T_KEY_DESCENDANTS			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_DESCENDANTS; }
	| T_KEY_UNIQUE				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_UNIQUE; }
	| T_KEY_ANCESTORS			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_ANCESTORS; }
	| T_KEY_NODES				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_NODES; }
	| T_KEY_IMPORTS				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_IMPORTS; }
	| T_KEY_OBJECTS				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_OBJECTS; }
	| T_KEY_VARIABLES			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_VARIABLES; }
	| T_KEY_ACTIONS				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_ACTIONS; }
	| T_KEY_FUNCTIONS			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_FUNCTIONS; }
	| T_KEY_REVERSE				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_REVERSE; }
	| T_KEY_SOURCE				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_FROM; }
	| T_KEY_SINK				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_TO; }
	| T_KEY_SOURCE_NAME			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_SOURCE_NAME; }
	| T_KEY_SINK_NAME			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_SINK_NAME; }
	;

selector_pseudo_simple_key
	: selector_pseudo_simple_key_real	{ cdn_parser_context_begin_selector_item (context); }
	;

selector_pseudo_simple
	: selector_pseudo_simple_key		{ cdn_parser_context_push_selector_pseudo (context, $1, NULL); }
	;

selector_pseudo_selector_key_real
	: T_KEY_HAS_TEMPLATE			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_HAS_TEMPLATE; }
	;

selector_pseudo_selector_key
	: selector_pseudo_selector_key_real	{ cdn_parser_context_begin_selector_item (context); }
	;

selector_pseudo_hasflag_arg
	: variable_flag				{ $$ = cdn_embedded_string_new_from_string (cdn_variable_flags_to_string ($1, CDN_VARIABLE_FLAG_NONE)); }
	| value_as_string			{ $$ = $1; }
	;

selector_pseudo_hasflag_args_rev
	: selector_pseudo_hasflag_arg		{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_hasflag_args_rev ',' selector_pseudo_hasflag_arg
						{ $$ = g_slist_prepend ($1, $3); }
	;

selector_pseudo_hasflag_args
	: selector_pseudo_hasflag_args_rev	{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_hasflag
	: T_KEY_HAS_FLAG
	  '('
	  selector_pseudo_hasflag_args
	  ')'					{ cdn_parser_context_push_selector_pseudo (context,
	                                                                                   CDN_SELECTOR_PSEUDO_TYPE_HAS_FLAG,
	                                                                                   $3);}
	;

selector_pseudo_strargs_key_real
	: T_KEY_SIBLINGS			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_SIBLINGS; }
	| T_KEY_SUBSET				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_SUBSET; }
	| T_KEY_DEBUG				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_DEBUG; }
	| T_KEY_HAS_TAG				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_HAS_TAG; }
	;

selector_pseudo_strargs_key
	: selector_pseudo_strargs_key_real	{ cdn_parser_context_begin_selector_item (context); }
	;

selector_pseudo_strargs_args_rev
	: value_as_string			{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_strargs_args_rev ',' value_as_string
						{ $$ = g_slist_prepend ($1, $3); }
	;

selector_pseudo_strargs_args
	: selector_pseudo_strargs_args_rev	{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_selector_args_rev
	: selector_as_pseudo_arg		{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_selector_args_rev ',' { cdn_parser_context_push_selector (context); }
	  selector_as_pseudo_arg		{ $$ = g_slist_prepend ($1, $4); }
	;

selector_pseudo_selector_args
	: selector_pseudo_selector_args_rev	{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_mixargs_key
	: T_KEY_IF				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_IF; }
	| T_KEY_REMOVE				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_REMOVE; }
	| T_KEY_IS_EMPTY			{ $$ = CDN_SELECTOR_PSEUDO_TYPE_IS_EMPTY; }
	| T_KEY_COUNT				{ $$ = CDN_SELECTOR_PSEUDO_TYPE_COUNT; }
	;

selector_pseudo_mixargs_arg
	: value_as_string			{ $$ = $1; }
	| selector_non_ambiguous_as_pseudo_arg	{ $$ = $1;
						  cdn_parser_context_push_selector (context); }
	;

selector_pseudo_mixargs_args_rev
	: selector_pseudo_mixargs_arg		{ $$ = g_slist_prepend (NULL, $1); }
	| selector_pseudo_mixargs_args ',' selector_pseudo_mixargs_arg
						{ $$ = g_slist_prepend ($1, $3); }
	;

selector_pseudo_mixargs_args
	: selector_pseudo_mixargs_args_rev	{ $$ = g_slist_reverse ($1); }
	;

selector_pseudo_with_args
	: selector_pseudo_selector_key
	  '('					{ cdn_parser_context_push_selector (context); }
	  selector_pseudo_selector_args
	  ')'					{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           $4); }
	| selector_pseudo_strargs_key '(' selector_pseudo_strargs_args ')'
						{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           $3); }
	| selector_pseudo_strargs_key '(' ')'
						{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	| selector_pseudo_strargs_key
						{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	| selector_pseudo_mixargs_key
	  '('					{ cdn_parser_context_push_selector (context); }
	  selector_pseudo_mixargs_args
	  ')'
						{ cdn_parser_context_pop_selector (context);
						  cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           $4); }
	| selector_pseudo_mixargs_key '(' ')'
						{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	| selector_pseudo_mixargs_key
						{ cdn_parser_context_push_selector_pseudo (context,
						                                           $1,
						                                           NULL); }
	| selector_pseudo_hasflag
	;

selector_pseudo
	: selector_pseudo_simple
	| selector_pseudo_with_args
	;

import
	: attributes
	  T_KEY_IMPORT
	  value_as_string
	  T_KEY_AS
	  identifier_or_string	{ cdn_parser_context_import (context, $5, $3, $1); errb }
	;

layout
	: attributes
	  T_KEY_LAYOUT
	  '{'			{ cdn_parser_context_push_layout (context, $1); }
	  layout_contents
	  '}'			{ cdn_parser_context_pop (context); }
	|
	  attributes
	  T_KEY_LAYOUT          { cdn_parser_context_push_layout (context, $1); }
	  layout_item           { cdn_parser_context_pop (context); }
	;

relation_item
	: T_KEY_LEFT_OF		{ $$ = CDN_LAYOUT_RELATION_LEFT_OF; }
	| T_KEY_RIGHT_OF	{ $$ = CDN_LAYOUT_RELATION_RIGHT_OF; }
	| T_KEY_ABOVE		{ $$ = CDN_LAYOUT_RELATION_ABOVE; }
	| T_KEY_BELOW		{ $$ = CDN_LAYOUT_RELATION_BELOW; }
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
	  selector			{ cdn_parser_context_add_layout (context, $2, $1, $3); errb }
	| relation
	  selector			{ cdn_parser_context_add_layout (context, $1, NULL, $2); errb }
	;

layout_item_separator
	: ','				{ $$ = TRUE; }
	| ':'				{ $$ = FALSE; }
	;

layout_item_absolute
	: selector
	  T_KEY_AT
	  value_as_string
	  layout_item_separator
	  value_as_string
	  layout_relative		{ cdn_parser_context_add_layout_position (context, $1, $3, $5, $6, $4); errb }
	| T_KEY_AT
	  value_as_string
	  layout_item_separator
	  value_as_string
	  layout_relative		{ cdn_parser_context_add_layout_position (context, NULL, $2, $4, $5, $3); errb }
	;

layout_item
	: layout_item_relative
	| layout_item_absolute
	;

layout_item_or_others
	: layout_item
	| common_scopes
	| attributes
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  layout_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

layout_contents
	:
	| layout_contents layout_item_or_others
	;

repeated_prime
	: '\''				{ $$ = 1; }
	| repeated_prime '\''		{ $$ = $$ + 1; }
	;

identifier
	: T_IDENTIFIER			{ $$ = cdn_embedded_string_new_from_string ($1); }
	;

double
	: T_DOUBLE			{ $$ = cdn_embedded_string_new_from_string ($1); }
	;

integer
	: T_INTEGER			{ $$ = cdn_embedded_string_new_from_string ($1); }
	;

value_as_string
	: string
	| equation
	| condition
	| indirection
	| identifier
	| integer
	| double
	;

string_item
	: equation_inside
	| condition_inside
	| indirection_inside
	| reduce_inside
	| map_inside
	| T_STRING			{ cdn_embedded_string_add_text (cdn_parser_context_peek_string (context), $1); }
	| '{'				{ cdn_embedded_string_push_brace (cdn_parser_context_peek_string (context)); }
	| '}'				{ cdn_embedded_string_pop_brace (cdn_parser_context_peek_string (context)); }
	;

string_contents
	:
	| string_contents string_item
	;

string
	: T_STRING_BEGIN		{ cdn_parser_context_push_string (context); }
	  string_contents
	  T_STRING_END			{ $$ = cdn_parser_context_pop_string (context); }
	;

condition
	: T_CONDITION_BEGIN		{
						cdn_embedded_string_push (cdn_parser_context_push_string (context),
						                          CDN_EMBEDDED_STRING_NODE_CONDITION,
						                          0);

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents ','		{
						cdn_embedded_string_pop (cdn_parser_context_peek_string (context));

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents ','		{
						cdn_embedded_string_pop (cdn_parser_context_peek_string (context));

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents
	  T_CONDITION_END		{ $$ = cdn_embedded_string_pop (cdn_embedded_string_pop (cdn_parser_context_pop_string (context))); }
	;

equation
	: T_EQUATION_BEGIN		{
						cdn_embedded_string_push (cdn_parser_context_push_string (context),
						                          CDN_EMBEDDED_STRING_NODE_EQUATION,
						                          0);
					}
	  string_contents
	  T_EQUATION_END		{ $$ = cdn_embedded_string_pop (cdn_parser_context_pop_string (context)); }
	;

indirection_embedding_item
	: T_INDIRECTION_EMBEDDING_BEGIN
	  string_contents
	  T_INDIRECTION_EMEDDING_END
	;

indirection_embedding
	: indirection_embedding_item
	| indirection_embedding indirection_embedding_item
	;

indirection_contents
	: T_STRING			{ cdn_embedded_string_add_text (cdn_parser_context_peek_string (context),
	                                                                $1); }
	| T_STRING 			{ cdn_embedded_string_add_text (cdn_parser_context_peek_string (context),
	                                                                $1); }
	  indirection_embedding
	| indirection_embedding
	;

indirection
	: T_INDIRECTION_BEGIN		{
						cdn_embedded_string_push (cdn_parser_context_push_string (context),
						                          CDN_EMBEDDED_STRING_NODE_INDIRECTION,
						                          $1);
					}
	  indirection_contents
	  T_INDIRECTION_END		{ $$ = cdn_embedded_string_pop (cdn_parser_context_pop_string (context)); }
	;

regex
	: T_REGEX_BEGIN			{ cdn_parser_context_push_string (context); cdn_parser_context_begin_selector_item (context); }
	  string_contents
	  T_REGEX_END			{ $$ = cdn_parser_context_pop_string (context); }
	;

equation_item
	: string_item
	| T_IDENTIFIER			{ cdn_embedded_string_push (cdn_parser_context_peek_string (context),
	                                                            CDN_EMBEDDED_STRING_NODE_INDIRECTION,
	                                                            0);
	                                  cdn_embedded_string_add_text (cdn_parser_context_peek_string (context),
	                                                                $1);
	                                  cdn_embedded_string_pop (cdn_parser_context_peek_string (context)); }
	;

equation_contents
	:
	| equation_contents equation_item
	;

equation_inside
	: T_EQUATION_BEGIN		{ cdn_embedded_string_push (cdn_parser_context_peek_string (context),
	                                                            CDN_EMBEDDED_STRING_NODE_EQUATION,
	                                                            0);}
	  equation_contents
	  T_EQUATION_END		{ cdn_embedded_string_pop (cdn_parser_context_peek_string (context)); }
	;

condition_inside
	: T_CONDITION_BEGIN		{
						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_CONDITION,
						                          0);

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents ','		{
						cdn_embedded_string_pop (cdn_parser_context_peek_string (context));

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents ','	{
						cdn_embedded_string_pop (cdn_parser_context_peek_string (context));

						cdn_embedded_string_push (cdn_parser_context_peek_string (context),
						                          CDN_EMBEDDED_STRING_NODE_TEXT,
						                          0);
					}
	  string_contents
	  T_CONDITION_END		{ cdn_embedded_string_pop (cdn_embedded_string_pop (cdn_parser_context_peek_string (context))); }
	;

map_item
	: equation_inside
	| condition_inside
	| indirection_inside
	| T_STRING			{ cdn_embedded_string_add_text (cdn_parser_context_peek_string (context), $1); }
	;

map_contents
	:
	| map_contents map_item
	;

map_inside
	: T_STRING_MAP_BEGIN		{ cdn_embedded_string_push (cdn_parser_context_peek_string (context),
	                                                            CDN_EMBEDDED_STRING_NODE_MAP,
	                                                            0);}
	  map_contents
	  T_STRING_MAP_END		{ cdn_embedded_string_pop (cdn_parser_context_peek_string (context)); }
	;

reduce_item
	: equation_inside
	| condition_inside
	| indirection_inside
	| T_STRING			{ cdn_embedded_string_add_text (cdn_parser_context_peek_string (context), $1); }
	;

reduce_contents
	:
	| reduce_contents reduce_item
	;

reduce_inside
	: T_STRING_REDUCE_BEGIN		{ cdn_embedded_string_push (cdn_parser_context_peek_string (context),
	                                                            CDN_EMBEDDED_STRING_NODE_REDUCE,
	                                                            0);}
	  reduce_contents
	  T_STRING_REDUCE_END		{ cdn_embedded_string_pop (cdn_parser_context_peek_string (context)); }
	;

indirection_inside
	: T_INDIRECTION_BEGIN		{ cdn_embedded_string_push (cdn_parser_context_peek_string (context),
	                                                            CDN_EMBEDDED_STRING_NODE_INDIRECTION,
	                                                            $1);}
	  indirection_contents
	  T_INDIRECTION_END		{ cdn_embedded_string_pop (cdn_parser_context_peek_string (context)); }
	;

debug
	: T_KEY_DEBUG_PRINT selector_non_ambiguous	{ cdn_parser_context_debug_selector (context, $2); }
	| T_KEY_DEBUG_PRINT value_as_string		{ cdn_parser_context_debug_string (context, $2); }
	| T_KEY_DEBUG_PRINT T_KEY_CONTEXT		{ cdn_parser_context_debug_context (context); }
	;

delete_item
	: selector		{ cdn_parser_context_delete_selector (context, $1); errb }
	| common_scopes
	| attributes
	  '{'			{ cdn_parser_context_push_scope (context, $1); }
	  delete_contents
	  '}'			{ cdn_parser_context_pop (context); }
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
	  '{'				{ cdn_parser_context_push_scope (context, $1); }
	  delete_contents
	  '}'				{ cdn_parser_context_pop (context); }
	;

%%

static void
yyerror (YYLTYPE *locp, CdnParserContext *context, char const *s)
{
	cdn_parser_context_set_error (context, s);
}

static CdnFunctionPolynomialPiece *
create_polynomial_piece (gchar const *start,
                         gchar const *end,
                         GArray      *coefficients)
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
			coefs[i] = g_ascii_strtod (g_array_index (coefficients, gchar *, i), NULL);
		}
	}

	return g_object_ref_sink (cdn_function_polynomial_piece_new (g_ascii_strtod (start, NULL),
	                                                             g_ascii_strtod (end, NULL),
	                                                             coefs,
	                                                             len));
}
