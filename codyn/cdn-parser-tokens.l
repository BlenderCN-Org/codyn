%{
#include "cdn-parser-context.h"
#include "cdn-parser.h"

static gchar *unquote_string (gchar const *s, gint len, gchar const *escape);
static void *__top_state;

void cdn_parser_set_column (int column_no, yyscan_t yyscanner);
int cdn_parser_get_column (yyscan_t yyscanner);

static void
advance_column (CdnParserContext *context, gint offset)
{
	gint yyccolumn;

	cdn_parser_context_get_column (context, NULL, &yyccolumn);
	cdn_parser_context_set_column (context, yyccolumn + 1, yyccolumn + offset);
}

#define YY_EXTRA_TYPE CdnParserContext *
#define YY_USER_ACTION								\
	advance_column (yyextra, yyleng);					\
	cdn_parser_context_set_token (yyextra, yytext);

#define YY_INPUT(buf,result,max_size)						\
	{									\
		result = cdn_parser_context_read (yyextra, buf, max_size);	\
										\
		if (result == 0)						\
		{								\
			result = YY_NULL;					\
		}								\
	}

#define HANDLE_NEWLINE								\
	cdn_parser_context_set_line (yyextra, yytext + 1, yylineno);		\
	cdn_parser_context_set_column (yyextra, 0, 0);				\
										\
	yyless (1);

#define ECHO

void cdn_parser_tokens_push_input (gpointer scanner);

%}

UniAnyChar [\xC2-\xDF][\x80-\xBF]|\xE0[\xA0-\xBF][\x80-\xBF]|[\xE1-\xEF][\x80-\xBF][\x80-\xBF]
IDENTIFIER ({UniAnyChar}|[a-z_])([a-z0-9_-]|{UniAnyChar})*

%option yylineno
%option noyywrap
%option noinput
%option nounput
%option reentrant
%option prefix="cdn_parser_"
%option bison-bridge bison-locations
%option outfile="lex.yy.c"
%option stack
%option case-insensitive

%x incl
%x instring
%x inreduce
%x inmap
%x inregex
%x annotation
%x annotation_start
%x comment
%x equation
%x condition
%x indirection
%x indirection_embedding
%x inclstart
%x onlyselector
%x indirection_had_some
%x firstline

%%

%{
	if (cdn_parser_context_get_start_token (yyextra))
	{
		yy_push_state (firstline, yyscanner);
	}

	/* To not get a compiler warning about defined but not used... */
	__top_state = yy_top_state;
%}

<firstline>.*|\n			{
					cdn_parser_context_set_line (yyextra, yytext, yylineno);
					cdn_parser_context_set_column (yyextra, 0, 0);

					yyless (0);

					yy_pop_state (yyscanner);

					if (cdn_parser_context_get_start_token (yyextra) == T_START_SELECTOR)
					{
						yy_push_state (onlyselector, yyscanner);
					}

					return cdn_parser_context_steal_start_token (yyextra);
				}

[ \t;]				;
\n.*				{
					HANDLE_NEWLINE
				}

"##"				{
					yy_push_state (annotation, yyscanner);
					return T_ANNOTATION_START;
				}

<annotation>(\\["@$\\]|[^"@$\\\n])+	{
					yylval->id = g_strdup (yytext);
					return T_STRING;
				}

<annotation>[\t ]*\n.*		{
					HANDLE_NEWLINE
					yy_push_state (annotation_start, yyscanner);
				}
<annotation_start>[\t ]*## {
					yy_pop_state (yyscanner);

					yylval->id = g_strdup ("\n");
					return T_STRING;
				}

<annotation_start>[\t ]*\n.*	{
					HANDLE_NEWLINE

					yy_pop_state (yyscanner);
					yy_pop_state (yyscanner);

					return T_ANNOTATION_END;
				}

<annotation_start><<EOF>>	{
					yy_pop_state (yyscanner);
					yy_pop_state (yyscanner);

					return T_ANNOTATION_END;
				}
<annotation_start>.		{
					yyless (0);

					yy_pop_state (yyscanner);
					yy_pop_state (yyscanner);

					return T_ANNOTATION_END;
				}

"#"				{
					yy_push_state (comment, yyscanner);
					return T_ANNOTATION_START;
				}

<comment>[^\n]*$		{
					yy_pop_state (yyscanner);
					return T_ANNOTATION_END;
				}

<<EOF>>				{
					if (cdn_parser_context_get_first_eof (yyextra))
					{
						cdn_parser_context_set_first_eof (yyextra, FALSE);
						return T_EOF;
					}
					else
					{
						cdn_parser_context_pop_input (yyextra);
						yypop_buffer_state (yyscanner);

						if (!YY_CURRENT_BUFFER)
						{
							yyterminate ();
						}
					}
				}

"include"			return T_KEY_INCLUDE;
"require"			return T_KEY_REQUIRE;
"parse"				return T_KEY_PARSE;
"object"			return T_KEY_OBJECT;
"edge"				return T_KEY_EDGE;
"integrated"			return T_KEY_INTEGRATED;
"in"				return T_KEY_IN;
"out"				return T_KEY_OUT;
"once"				return T_KEY_ONCE;
"discrete"			return T_KEY_DISCRETE;
"interface"			return T_KEY_INTERFACE;
"polynomial"			return T_KEY_POLYNOMIAL;
"import"			return T_KEY_IMPORT;
"link-library"			return T_KEY_LINK_LIBRARY;
"from"				return T_KEY_FROM;
"to"				return T_KEY_TO;
"piece"				return T_KEY_PIECE;
"defines"			return T_KEY_DEFINES;
"integrator"			return T_KEY_INTEGRATOR;
"node"				return T_KEY_NODE;
"layout"			return T_KEY_LAYOUT;
"at"				return T_KEY_AT;
"of"				return T_KEY_OF;
"on"				return T_KEY_ON;
"all"				return T_KEY_ALL;
"action"			return T_KEY_ACTION;
"delete"			return T_KEY_DELETE;
"debug-print"			return T_KEY_DEBUG_PRINT;
"apply"				return T_KEY_APPLY;
"unapply"			return T_KEY_UNAPPLY;
"with"				return T_KEY_WITH;
"when"				return T_KEY_WHEN;
"state"				return T_KEY_STATE;
"event"				return T_KEY_EVENT;
"terminate"			return T_KEY_TERMINATE;
"any"				return T_KEY_ANY;
"set"				return T_KEY_SET;
"io"				return T_KEY_IO;
"within"			return T_KEY_WITHIN;
"initial-state"			return T_KEY_INITIAL_STATE;

<INITIAL,onlyselector>"templates"		return T_KEY_TEMPLATES;
<INITIAL,onlyselector>"templates-root"		return T_KEY_TEMPLATES_ROOT;
<INITIAL,onlyselector>"applied-templates"	return T_KEY_APPLIED_TEMPLATES;
<INITIAL,onlyselector>"root"			return T_KEY_ROOT;
<INITIAL,onlyselector>"debug"			return T_KEY_DEBUG;
<INITIAL,onlyselector>"children"		return T_KEY_CHILDREN;
<INITIAL,onlyselector>"parent"			return T_KEY_PARENT;
<INITIAL,onlyselector>"first"			return T_KEY_FIRST;
<INITIAL,onlyselector>"last"			return T_KEY_LAST;
<INITIAL,onlyselector>"subset"			return T_KEY_SUBSET;
<INITIAL,onlyselector>"edges"			return T_KEY_EDGES;
<INITIAL,onlyselector>"siblings"		return T_KEY_SIBLINGS;
<INITIAL,onlyselector>"unique"			return T_KEY_UNIQUE;
<INITIAL,onlyselector>"count"			return T_KEY_COUNT;
<INITIAL,onlyselector>"name"			return T_KEY_NAME;
<INITIAL,onlyselector>"self"			return T_KEY_SELF;
<INITIAL,onlyselector>"nodes"			return T_KEY_NODES;
<INITIAL,onlyselector>"imports"			return T_KEY_IMPORTS;
<INITIAL,onlyselector>"objects"			return T_KEY_OBJECTS;
<INITIAL,onlyselector>"variables"		return T_KEY_VARIABLES;
<INITIAL,onlyselector>"actions"			return T_KEY_ACTIONS;
<INITIAL,onlyselector>"functions"		return T_KEY_FUNCTIONS;
<INITIAL,onlyselector>"not"			return T_KEY_NOT;
<INITIAL,onlyselector>"if"			return T_KEY_IF;
<INITIAL,onlyselector>"ifstr"			return T_KEY_IFSTR;
<INITIAL,onlyselector>"notstr"			return T_KEY_NOTSTR;
<INITIAL,onlyselector>"from-set"		return T_KEY_FROM_SET;
<INITIAL,onlyselector>"type"			return T_KEY_TYPE;
<INITIAL,onlyselector>"has-flag"		return T_KEY_HAS_FLAG;
<INITIAL,onlyselector>"has-template"		return T_KEY_HAS_TEMPLATE;
<INITIAL,onlyselector>"reverse"			return T_KEY_REVERSE;
<INITIAL,onlyselector>"input"			return T_KEY_INPUT;
<INITIAL,onlyselector>"output"			return T_KEY_OUTPUT;
<INITIAL,onlyselector>"inputs"			return T_KEY_INPUTS;
<INITIAL,onlyselector>"outputs"			return T_KEY_OUTPUTS;
<INITIAL,onlyselector>"input-name"		return T_KEY_INPUT_NAME;
<INITIAL,onlyselector>"output-name"		return T_KEY_OUTPUT_NAME;
<INITIAL,onlyselector>"recurse"			return T_KEY_RECURSE;
<INITIAL,onlyselector>"append-context"		return T_KEY_APPEND_CONTEXT;
<INITIAL,onlyselector>"reduce"			return T_KEY_REDUCE;
<INITIAL,onlyselector>"sort"			return T_KEY_SORT;
<INITIAL,onlyselector>"xor"			return T_KEY_XOR;

"context"			return T_KEY_CONTEXT;
"as"				return T_KEY_AS;
"bidirectional"			return T_KEY_BIDIRECTIONAL;
"settings"			return T_KEY_SETTINGS;
"no-self"			return T_KEY_NO_SELF;
"probability"			return T_KEY_PROBABILITY;

<INITIAL,onlyselector>{IDENTIFIER} {
					yylval->id = g_strdup (yytext);
					return T_IDENTIFIER;
				}

[-+]?[0-9]+((\.|eE[-+]?)[0-9]*) {
					yylval->id = g_strdup (yytext);
					return T_DOUBLE;
				}

[-+]?[0-9]+			{
					yylval->id = g_strdup (yytext);
					return T_INTEGER;
				}

<onlyselector,INITIAL>"\""	{
					yy_push_state (instring, yyscanner);
					return T_STRING_BEGIN;
				}

<onlyselector,INITIAL>"/"	{
					yy_push_state (inregex, yyscanner);
					return T_REGEX_BEGIN;
				}

<indirection>"["		{
					yy_pop_state (yyscanner);
					yy_push_state (indirection_had_some, yyscanner);
					yy_push_state (indirection_embedding, yyscanner);
					return T_INDIRECTION_EMBEDDING_BEGIN;
				}

<indirection_had_some>"["	{
					yy_push_state (indirection_embedding, yyscanner);
					return T_INDIRECTION_EMBEDDING_BEGIN;
				}

<indirection_had_some>\n.*	{
					yyless (0);

					yy_pop_state (yyscanner);
					return T_INDIRECTION_END;
				}
<indirection_had_some>([@]+|[$][(]|.?|$$\()	{
					yy_pop_state (yyscanner);

					advance_column (yyextra, -yyleng);

					yyless(0);

					return T_INDIRECTION_END;
				}

<*>"@"+				{
					yy_push_state (indirection, yyscanner);
					yylval->num = yyleng - 1;

					return T_INDIRECTION_BEGIN;
				}

<indirection_embedding>(\\[]@$\\]|[^]@$\\])+ {
					yylval->id = unquote_string (yytext, yyleng, "@]$");
					return T_STRING;
				}

<indirection>({IDENTIFIER}|[0-9]+|[~*])	{
					yylval->id = g_strdup (yytext);
					yy_pop_state (yyscanner);
					yy_push_state (indirection_had_some, yyscanner);

					return T_STRING;
}

<*>"$$("			{
					cdn_parser_context_push_equation (yyextra);
					yy_push_state (condition, yyscanner);

					return T_CONDITION_BEGIN;
				}

<*>"$("				{
					cdn_parser_context_push_equation (yyextra);
					yy_push_state (equation, yyscanner);

					return T_EQUATION_BEGIN;
				}

<inreduce>[},|]			{
					yy_pop_state (yyscanner);
					yyless (0);

					return T_STRING_REDUCE_END;
				}

<inmap>[},|]			{
					yy_pop_state (yyscanner);
					yyless (0);

					return T_STRING_MAP_END;
				}

<instring,incl>(\\["@$\\|{}]|{UniAnyChar}|[^"@$\\|{}])+ {
					yylval->id = unquote_string (yytext, yyleng, "@\"$|{}");
					return T_STRING;
				}

<inreduce,inmap>(\\["@$\\}|,]|[^"@$\\|},])+ {
					yylval->id = unquote_string (yytext, yyleng, "@$}|,");
					return T_STRING;
				}

<equation>(\\[)@$(\\]|[^)@$(\\])+ {
					yylval->id = unquote_string (yytext, yyleng, "@)$(");
					return T_STRING;
				}

<condition>(\\[)@$(\\,]|[^)@$(\\,])+ {
					yylval->id = unquote_string (yytext, yyleng, "@)$(,");
					return T_STRING;
				}

<condition>,			{
					if (cdn_parser_context_peek_equation_depth (yyextra) == 1)
					{
						return ',';
					}
					else
					{
						yylval->id = g_strdup (yytext);
						return T_STRING;
					}
				}

<inregex>(\\[/@$\\]|[^/@$\\])+	{
					yylval->id = unquote_string (yytext, yyleng, "@/$\\");
					return T_STRING;
				}

<instring>"\""			{
					yy_pop_state (yyscanner);
					return T_STRING_END;
				}

<inregex>"/"			{
					yy_pop_state (yyscanner);
					return T_REGEX_END;
				}

<indirection_embedding>"]"	{
					yy_pop_state (yyscanner);

					return T_INDIRECTION_EMEDDING_END;
				}

<equation>"("			{
					yylval->id = g_strdup (yytext);
					cdn_parser_context_push_equation_depth (yyextra);

					return T_STRING;
				}

<condition>[([]			{
					yylval->id = g_strdup (yytext);
					cdn_parser_context_push_equation_depth (yyextra);

					return T_STRING;
				}

<condition>[)\]]		{
					if (cdn_parser_context_pop_equation_depth (yyextra))
					{
						yy_pop_state (yyscanner);
						return T_CONDITION_END;
					}
					else
					{
						yylval->id = g_strdup (yytext);
						return T_STRING;
					}
				}

<equation>")"			{
					if (cdn_parser_context_pop_equation_depth (yyextra))
					{
						yy_pop_state (yyscanner);
						return T_EQUATION_END;
					}
					else
					{
						yylval->id = g_strdup (yytext);
						return T_STRING;
					}
				}

<instring,incl>"|"		{
					if (cdn_embedded_string_brace_level (cdn_parser_context_peek_string (yyextra)))
					{
						yy_push_state (inmap, yyscanner);
						return T_STRING_MAP_BEGIN;
					}
					else
					{
						yylval->id = g_strdup (yytext);
						return T_STRING;
					}
				}

<instring,incl>"||"		{
					if (cdn_embedded_string_brace_level (cdn_parser_context_peek_string (yyextra)))
					{
						yy_push_state (inreduce, yyscanner);
						return T_STRING_REDUCE_BEGIN;
					}
					else
					{
						yylval->id = g_strdup (yytext);
						return T_STRING;
					}
				}

<*>"@{"				{
					yylval->id = g_strdup ("@");
					yyless (1);
					return T_STRING;
				}
<*>"{"				return '{';
<*>"}"				return '}';
<onlyselector,INITIAL>"("	return '(';
<onlyselector,INITIAL>")"	return ')';
":"				return ':';
"="				return '=';
"<"				return '<';
">"				return '>';
<onlyselector,INITIAL>","	return ',';
<onlyselector,INITIAL>"."	return '.';
<onlyselector,INITIAL>"|"	return '|';
"["				return '[';
"]"				return ']';
"-"				return '-';
"+"				return '+';
"?"				return '?';
"'"				return '\'';
"$"				return '$';

%%

static gchar *
unquote_string (gchar const *s,
                gint         len,
                gchar const *escape)
{
	GString *ret = g_string_new ("");

	while (*s && len > 0)
	{
		gunichar c = g_utf8_get_char (s);
		gchar *nptr = g_utf8_next_char (s);
		gunichar n = g_utf8_get_char (nptr);

		if (c == '\\' && len > 1 && (g_utf8_strchr (escape, -1, n) != NULL || n == '\\'))
		{
			nptr = g_utf8_next_char (nptr);
			c = n;

			--len;
		}

		g_string_append_unichar (ret, c);
		s = nptr;

		--len;
	}

	return g_string_free (ret, FALSE);
}

void
cdn_parser_tokens_push_input (gpointer scanner)
{
	yypush_buffer_state (yy_create_buffer (NULL, YY_BUF_SIZE, scanner),
	                     scanner);
}
