%{
#include "cpg-parser-context.h"
#include "cpg-parser.h"

static gchar *unquote_string (gchar const *s, gint len, gchar const *escape);

void cpg_parser_set_column (int column_no, yyscan_t yyscanner);
int cpg_parser_get_column (yyscan_t yyscanner);

static int yyccolumn = 1;

static void
advance_column (CpgParserContext *context, gint offset)
{
	cpg_parser_context_set_column (context, yyccolumn, yyccolumn + offset - 1);
	yyccolumn += offset;
}

#define YY_EXTRA_TYPE CpgParserContext *
#define YY_USER_ACTION								\
	advance_column (yyextra, yyleng);					\
	cpg_parser_context_set_token (yyextra, yytext);				\

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
	yyccolumn = 1;								\
										\
	cpg_parser_context_set_line (yyextra, yytext + 1, yylineno);		\
	cpg_parser_context_set_column (yyextra, 0, 0);				\
										\
	yyless (1);

%}

STRING "\""(\\["]|[^"])*"\""
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
%x inregex
%x annotation
%x comment
%x equation
%x indirection
%x indirection_embedding
%x inclstart
%x onlyselector
%x indirection_had_some

%%

%{
	if (cpg_parser_context_get_start_token (yyextra))
	{
		if (cpg_parser_context_get_start_token (yyextra) == T_START_SELECTOR)
		{
			yy_push_state (onlyselector, yyscanner);
		}

		return cpg_parser_context_steal_start_token (yyextra);
	}

	/* To not get a compiler warning about defined but not used... */
	yy_top_state (yyscanner);
%}

<onlyselector>{IDENTIFIER}	{
					yylval->id = g_strdup (yytext);
					return T_IDENTIFIER;
				}

[ \t]				;
\n.*				{
					HANDLE_NEWLINE
				}
"##"[ \t]*			{
					yy_push_state (annotation, yyscanner);
				}
<annotation>[^\n]+		{
					cpg_parser_context_push_annotation (yyextra,
					                                    cpg_embedded_string_new_from_string (yytext));
					yy_pop_state (yyscanner);
				}
"#"				yy_push_state (comment, yyscanner);
<comment>[^\n]*$		yy_pop_state (yyscanner);

"include"			{
					yy_push_state (inclstart, yyscanner);
					return T_KEY_INCLUDE;
				}
<inclstart>"\""			{
					yy_pop_state (yyscanner);
					yy_push_state (incl, yyscanner);

					cpg_parser_context_push_string (yyextra);
					return T_STRING_BEGIN;
				}
<incl>"\""			{
					yypush_buffer_state (yy_create_buffer (NULL, YY_BUF_SIZE, yyscanner), yyscanner);
					cpg_parser_context_push_input_from_path (yyextra,
					                                         cpg_parser_context_pop_string (yyextra));

					yy_pop_state (yyscanner);
					return T_STRING_END;
				}

<<EOF>>				{
					yypop_buffer_state (yyscanner);
					cpg_parser_context_pop_input (yyextra);

					if (!YY_CURRENT_BUFFER)
					{
						yyterminate ();
					}
				}

"templates"			return T_KEY_TEMPLATES;
"state"				return T_KEY_STATE;
"link"				return T_KEY_LINK;
"integrated"			return T_KEY_INTEGRATED;
"in"				return T_KEY_IN;
"out"				return T_KEY_OUT;
"once"				return T_KEY_ONCE;
"network"			return T_KEY_NETWORK;
"function"			return T_KEY_FUNCTION;
"interface"			return T_KEY_INTERFACE;
"polynomial"			return T_KEY_POLYNOMIAL;
"import"			return T_KEY_IMPORT;
"input-file"			return T_KEY_INPUT_FILE;
"from"				return T_KEY_FROM;
"to"				return T_KEY_TO;
"piece"				return T_KEY_PIECE;
"define"			return T_KEY_DEFINE;
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
"debug"				return T_KEY_DEBUG;
"selector"			return T_KEY_SELECTOR;
"action"			return T_KEY_ACTION;
"property"			return T_KEY_PROPERTY;
"delete"			return T_KEY_DELETE;
"or"				return T_KEY_OR;
"root"				return T_KEY_ROOT;
"children"			return T_KEY_CHILDREN;
"parent"			return T_KEY_PARENT;
"first-child"			return T_KEY_FIRST_CHILD;
"last-child"			return T_KEY_LAST_CHILD;
"first"				return T_KEY_FIRST;
"last"				return T_KEY_LAST;
"subset"			return T_KEY_SUBSET;
"states"			return T_KEY_STATES;
"links"				return T_KEY_LINKS;
"siblings"			return T_KEY_SIBLINGS;
"count"				return T_KEY_COUNT;
"self"				return T_KEY_SELF;
"context"			return T_KEY_CONTEXT;

{IDENTIFIER}			{
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

"\""				{
					yy_push_state (instring, yyscanner);
					return T_STRING_BEGIN;
				}

"/"				{
					yy_push_state (inregex, yyscanner);
					return T_REGEX_BEGIN;
				}

<indirection,indirection_had_some>"["		{
					yy_push_state (indirection_embedding, yyscanner);
					return T_INDIRECTION_EMBEDDING_BEGIN;
				}

<indirection_had_some>\n.*	{
					HANDLE_NEWLINE

					yy_pop_state (yyscanner);
					return T_INDIRECTION_END;
				}
<indirection_had_some>([@]+|.?)	{
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

<indirection_embedding>(\\[]@$]|[^]@$])+ {
					yylval->id = unquote_string (yytext, yyleng, "@]$");
					return T_STRING;
				}

<indirection>({IDENTIFIER}|[0-9]+|[~*])	{
					yylval->id = g_strdup (yytext);
					yy_pop_state (yyscanner);
					yy_push_state (indirection_had_some, yyscanner);

					return T_STRING;
}

<*>"$("	{
					cpg_parser_context_push_equation (yyextra);
					yy_push_state (equation, yyscanner);

					return T_EQUATION_BEGIN;
				}

<instring,incl>(\\["@$]|[^"@$])+ {
					yylval->id = unquote_string (yytext, yyleng, "@\"$");
					return T_STRING;
				}

<equation>(\\[)@$(]|[^a-z_)@$(])+ {
					yylval->id = unquote_string (yytext, yyleng, "@)$(");
					return T_STRING;
				}


<inregex>(\\[/@$]|[^/@$])+	{
					yylval->id = unquote_string (yytext, yyleng, "@/$");
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
					yy_pop_state (yyscanner);

					return T_INDIRECTION_END;
				}

<equation>"("			{
					yylval->id = g_strdup (yytext);
					cpg_parser_context_push_equation_depth (yyextra);

					return T_STRING;
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

"{"				return '{';
"}"				return '}';
"("				return '(';
")"				return ')';
":"				return ':';
"="				return '=';
"<"				return '<';
","				return ',';
<onlyselector,INITIAL>"."	return '.';
<onlyselector,INITIAL>"|"	return '|';
"["				return '[';
"]"				return ']';

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
