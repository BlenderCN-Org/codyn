%{
#include "cpg-parser-context.h"
#include "cpg-parser.h"

static gchar *unquote_string (gchar const *s, gint len, gchar const *escape);

void cpg_parser_set_column (int column_no, yyscan_t yyscanner);
int cpg_parser_get_column (yyscan_t yyscanner);

static void
advance_column (CpgParserContext *context, gint offset)
{
	gint yyccolumn;

	cpg_parser_context_get_column (context, NULL, &yyccolumn);
	cpg_parser_context_set_column (context, yyccolumn + 1, yyccolumn + offset);
}

#define YY_EXTRA_TYPE CpgParserContext *
#define YY_USER_ACTION								\
	advance_column (yyextra, yyleng);					\
	cpg_parser_context_set_token (yyextra, yytext);

#define YY_INPUT(buf,result,max_size)						\
	{									\
		result = cpg_parser_context_read (yyextra, buf, max_size);	\
										\
		if (result == 0)						\
		{								\
			result = YY_NULL;					\
		}								\
	}

#define HANDLE_NEWLINE								\
	cpg_parser_context_set_line (yyextra, yytext + 1, yylineno);		\
	cpg_parser_context_set_column (yyextra, 0, 0);				\
										\
	cpg_parser_context_remove_record (yyextra, yyleng - 1, 0);		\
	yyless (1);

#define ECHO

void cpg_parser_tokens_push_input (gpointer scanner);

%}

IDENTIFIER [a-z_][a-z0-9_-]*

%option yylineno
%option noyywrap
%option noinput
%option nounput
%option reentrant
%option prefix="cpg_parser_"
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
	if (cpg_parser_context_get_start_token (yyextra))
	{
		yy_push_state (firstline, yyscanner);
	}

	/* To not get a compiler warning about defined but not used... */
	yy_top_state (yyscanner);
%}

<firstline>.*|\n			{
					cpg_parser_context_set_line (yyextra, yytext, yylineno);
					cpg_parser_context_set_column (yyextra, 0, 0);

					yyless (0);
					cpg_parser_context_remove_record (yyextra, yyleng, 0);

					yy_pop_state (yyscanner);

					if (cpg_parser_context_get_start_token (yyextra) == T_START_SELECTOR)
					{
						yy_push_state (onlyselector, yyscanner);
					}

					return cpg_parser_context_steal_start_token (yyextra);
				}

[ \t]				;
\n.*				{
					HANDLE_NEWLINE
				}
"##"[ \t]*			{
					yy_push_state (annotation, yyscanner);
					return T_ANNOTATION_START;
				}
<annotation>\n.*		{
					HANDLE_NEWLINE
					yy_push_state (annotation_start, yyscanner);
				}
<annotation_start>[\t ]*##[\t ]* {
					yy_pop_state (yyscanner);
				}
<annotation_start>.		{
					yyless (0);
					cpg_parser_context_remove_record (yyextra, yyleng, 0);

					yy_pop_state (yyscanner);
					yy_pop_state (yyscanner);

					return T_ANNOTATION_END;
				}

"#"				yy_push_state (comment, yyscanner);
<comment>[^\n]*$		yy_pop_state (yyscanner);

<<EOF>>				{
					if (cpg_parser_context_get_first_eof (yyextra))
					{
						cpg_parser_context_set_first_eof (yyextra, FALSE);
						return T_EOF;
					}
					else
					{
						cpg_parser_context_pop_input (yyextra);
						yypop_buffer_state (yyscanner);

						if (!YY_CURRENT_BUFFER)
						{
							yyterminate ();
						}
					}
				}

"include"			return T_KEY_INCLUDE;
"parse"				return T_KEY_PARSE;
"state"				return T_KEY_STATE;
"object"			return T_KEY_OBJECT;
"link"				return T_KEY_LINK;
"integrated"			return T_KEY_INTEGRATED;
"in"				return T_KEY_IN;
"out"				return T_KEY_OUT;
"once"				return T_KEY_ONCE;
"disabled"			return T_KEY_DISABLED;
"network"			return T_KEY_NETWORK;
"functions"			return T_KEY_FUNCTIONS;
"interface"			return T_KEY_INTERFACE;
"polynomial"			return T_KEY_POLYNOMIAL;
"import"			return T_KEY_IMPORT;
"input-file"			return T_KEY_INPUT_FILE;
"from"				return T_KEY_FROM;
"to"				return T_KEY_TO;
"piece"				return T_KEY_PIECE;
"defines"			return T_KEY_DEFINES;
"integrator"			return T_KEY_INTEGRATOR;
"group"				return T_KEY_GROUP;
"layout"			return T_KEY_LAYOUT;
"at"				return T_KEY_AT;
"above"				return T_KEY_ABOVE;
"below"				return T_KEY_BELOW;
"left-of"			return T_KEY_LEFT_OF;
"right-of"			return T_KEY_RIGHT_OF;
"of"				return T_KEY_OF;
"on"				return T_KEY_ON;
"all"				return T_KEY_ALL;
"action"			return T_KEY_ACTION;
"property"			return T_KEY_PROPERTY;
"delete"			return T_KEY_DELETE;
"debug-print"			return T_KEY_DEBUG_PRINT;
"apply"				return T_KEY_APPLY;
"unapply"			return T_KEY_UNAPPLY;
"with"				return T_KEY_WITH;
"when"				return T_KEY_WHEN;

<INITIAL,onlyselector>"templates"	return T_KEY_TEMPLATES;
<INITIAL,onlyselector>"templates-root"	return T_KEY_TEMPLATES_ROOT;
<INITIAL,onlyselector>"root"		return T_KEY_ROOT;
<INITIAL,onlyselector>"debug"		return T_KEY_DEBUG;
<INITIAL,onlyselector>"children"	return T_KEY_CHILDREN;
<INITIAL,onlyselector>"parent"		return T_KEY_PARENT;
<INITIAL,onlyselector>"first"		return T_KEY_FIRST;
<INITIAL,onlyselector>"last"		return T_KEY_LAST;
<INITIAL,onlyselector>"subset"		return T_KEY_SUBSET;
<INITIAL,onlyselector>"states"		return T_KEY_STATES;
<INITIAL,onlyselector>"links"		return T_KEY_LINKS;
<INITIAL,onlyselector>"siblings"	return T_KEY_SIBLINGS;
<INITIAL,onlyselector>"descendants"	return T_KEY_DESCENDANTS;
<INITIAL,onlyselector>"ancestors"	return T_KEY_ANCESTORS;
<INITIAL,onlyselector>"unique"		return T_KEY_UNIQUE;
<INITIAL,onlyselector>"count"		return T_KEY_COUNT;
<INITIAL,onlyselector>"name"		return T_KEY_NAME;
<INITIAL,onlyselector>"self"		return T_KEY_SELF;
<INITIAL,onlyselector>"groups"		return T_KEY_GROUPS;
<INITIAL,onlyselector>"imports"		return T_KEY_IMPORTS;
<INITIAL,onlyselector>"objects"		return T_KEY_OBJECTS;
<INITIAL,onlyselector>"properties"	return T_KEY_PROPERTIES;
<INITIAL,onlyselector>"actions"		return T_KEY_ACTION;
<INITIAL,onlyselector>"is-empty"	return T_KEY_IS_EMPTY;
<INITIAL,onlyselector>"remove"		return T_KEY_REMOVE;
<INITIAL,onlyselector>"if"		return T_KEY_IF;
<INITIAL,onlyselector>"from-set"	return T_KEY_FROM_SET;
<INITIAL,onlyselector>"type"		return T_KEY_TYPE;
<INITIAL,onlyselector>"has-flag"	return T_KEY_HAS_FLAG;
<INITIAL,onlyselector>"has-template"	return T_KEY_HAS_TEMPLATE;
<INITIAL,onlyselector>"has-tag"		return T_KEY_HAS_TAG;
<INITIAL,onlyselector>"reverse"		return T_KEY_REVERSE;

"context"			return T_KEY_CONTEXT;
"as"				return T_KEY_AS;
"proxy"				return T_KEY_PROXY;
"bidirectional"			return T_KEY_BIDIRECTIONAL;
"each"				return T_KEY_EACH;
"settings"			return T_KEY_SETTINGS;
"no-self"			return T_KEY_NO_SELF;
"probability"			return T_KEY_PROBABILITY;
"tag"				return T_KEY_TAG;

<INITIAL,onlyselector>{IDENTIFIER} {
					yylval->id = g_strdup (yytext);
					return T_IDENTIFIER;
				}

[-+]?[0-9]+((\.|eE[-+]?)[0-9]*) {
					yylval->numf = g_ascii_strtod (yytext, NULL);
					return T_DOUBLE;
				}

[-+]?[0-9]+			{
					yylval->numf = (gint)g_ascii_strtoll (yytext, NULL, 10);
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
					cpg_parser_context_remove_record (yyextra, yyleng, 0);

					yy_pop_state (yyscanner);
					return T_INDIRECTION_END;
				}
<indirection_had_some>([@]+|[$][(]|.?|$$\()	{
					yy_pop_state (yyscanner);

					advance_column (yyextra, -yyleng);

					cpg_parser_context_remove_record (yyextra, yyleng, 0);
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
					cpg_parser_context_push_equation (yyextra);
					yy_push_state (condition, yyscanner);

					return T_CONDITION_BEGIN;
				}

<*>"$("				{
					cpg_parser_context_push_equation (yyextra);
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

<instring,incl>(\\["@$\\|{}]|[^"@$\\|{}])+ {
					yylval->id = unquote_string (yytext, yyleng, "@\"$|{}");
					return T_STRING;
				}

<inreduce,inmap>(\\["@$\\}|,]|[^"@$\\|},])+ {
					yylval->id = unquote_string (yytext, yyleng, "@$}|,");
					return T_STRING;
				}

<annotation>(\\["@$\\]|[^"@$\\\n])+	{
					yylval->id = g_strdup (yytext);
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
					if (cpg_parser_context_peek_equation_depth (yyextra) == 1)
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
					cpg_parser_context_push_equation_depth (yyextra);

					return T_STRING;
				}

<condition>[([]			{
					yylval->id = g_strdup (yytext);
					cpg_parser_context_push_equation_depth (yyextra);

					return T_STRING;
				}

<condition>[)\]]		{
					if (cpg_parser_context_pop_equation_depth (yyextra))
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
					if (cpg_parser_context_pop_equation_depth (yyextra))
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
					if (cpg_embedded_string_brace_level (cpg_parser_context_peek_string (yyextra)))
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
					if (cpg_embedded_string_brace_level (cpg_parser_context_peek_string (yyextra)))
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

%%

static gchar *
unquote_string (gchar const *s, gint len, gchar const *escape)
{
	gchar *ret;
	gchar *ptr;

	ret = g_strndup (s, len);
	ptr = ret;

	while (*s && len > 0)
	{
		if (*s == '\\' && len > 1 && (strchr (escape, *(s + 1)) != NULL || *(s + 1) == '\\'))
		{
			++s;
			--len;
		}

		*ptr++ = *s++;
		--len;
	}

	*ptr = '\0';

	return ret;
}

void
cpg_parser_tokens_push_input (gpointer scanner)
{
	yypush_buffer_state (yy_create_buffer (NULL, YY_BUF_SIZE, scanner),
	                     scanner);
}
