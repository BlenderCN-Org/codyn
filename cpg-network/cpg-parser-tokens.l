%{
#include "cpg-parser-context.h"
#include "cpg-parser.h"

static char *unquote_string (char *s, int len, gchar const *escape);

void cpg_parser_set_column (int column_no, yyscan_t yyscanner);
int cpg_parser_get_column (yyscan_t yyscanner);

static int yyccolumn = 1;

#define YY_EXTRA_TYPE CpgParserContext *
#define YY_USER_ACTION yylloc->first_line = yylloc->last_line = yylineno;	\
	yylloc->first_column = yyccolumn;					\
	yylloc->last_column = yyccolumn + yyleng - 1; 				\
	cpg_parser_context_set_column (yyextra, yyccolumn, yyccolumn + yyleng - 1); \
	cpg_parser_context_set_token (yyextra, yytext);			\
	yyccolumn += yyleng;

#define YY_INPUT(buf,result,max_size)						\
	{									\
		result = cpg_parser_context_read (yyextra, buf, max_size);	\
										\
		if (result == 0)						\
		{								\
			result = YY_NULL;					\
		}								\
	}

#define unputs(s) unputsl(s, strlen (s))

#define unputsl(s, len)								\
	gint i;									\
										\
	for (i = len - 1; i >= 0; ++i)						\
	{									\
		unput (s[i]);							\
	}
%}

STRING "\""(\\["]|[^"])*"\""

%option yylineno
%option noyywrap
%option noinput
%option nounput
%option reentrant
%option prefix="cpg_parser_"
%option bison-bridge bison-locations
%option outfile="lex.yy.c"
%option stack

%x incl
%x instring
%x inregex
%x annotation
%x comment
%x equation

%%

%{
	if (cpg_parser_context_get_start_token (yyextra))
	{
		return cpg_parser_context_steal_start_token (yyextra);
	}

	/* To not get a compiler warning about defined but not used... */
	yy_top_state (yyscanner);
%}

[ \t]				;
\n.*				{
					yyccolumn = 1;

					cpg_parser_context_set_line (yyextra, yytext + 1, yylineno);
					cpg_parser_context_set_column (yyextra, 0, 0);
					yyless (1);
				}
"##"[ \t]*			yy_push_state (annotation, yyscanner);
<annotation>[^\n]*$		{
					cpg_parser_context_push_annotation (yyextra, yytext);
					yy_pop_state (yyscanner);
				}
"#"				yy_push_state (comment, yyscanner);
<comment>[^\n]*$		yy_pop_state (yyscanner);

"include"			yy_push_state (incl, yyscanner);

<incl>[ \t]*			;
<incl>{STRING}			{
					yypush_buffer_state (yy_create_buffer (NULL, YY_BUF_SIZE, yyscanner), yyscanner);
					cpg_parser_context_push_input_from_path (yyextra, yytext);

					yy_pop_state (yyscanner);
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
"bidirectional"			return T_KEY_BIDIRECTIONAL;
"all"				return T_KEY_ALL;
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
"proxy"				return T_KEY_PROXY;

[-+]?[0-9]+((\.|eE[-+]?)[0-9]*) {yylval->numf = g_ascii_strtod (yytext, NULL);
				 return T_DOUBLE;}

[a-zA-Z_][a-zA-Z0-9_-]*	 	{yylval->id = g_strdup (yytext);
				 return T_IDENTIFIER;}

[-+]?[0-9]+			{yylval->numf = (gint)g_ascii_strtoll (yytext, NULL, 10);
				 return T_INTEGER;}

"\""				{
					yy_push_state (instring, yyscanner);
					yylval->string = g_string_new ("");
					return T_STRING_BEGIN;
				}

<instring,inregex,INITIAL>"@{"[a-zA-Z_][a-zA-Z0-9_-]*"}" {
					yylval->id = g_strndup (yytext + 2, yyleng - 3);
					return T_DEFINED;
				}

<instring,inregex,equation,INITIAL>"@"[0-9]+(:[0-9]+)?	{
					yylval->id = g_strdup (yytext + 1);
					return T_EXPANSION;
				}

<instring,inregex,equation,INITIAL>"@"[a-zA-Z_][a-zA-Z0-9_-]*	{
					yylval->id = g_strdup (yytext + 1);
					return T_DEFINED;
				}

<instring,inregex,INITIAL>"@{"	{
					yy_push_state (equation, yyscanner);
					return T_EQUATION_BEGIN;
				}
<equation>"}"			{
					yy_pop_state (yyscanner);
					return T_EQUATION_END;
				}
<equation>(\\[}@]|[^}@])+	{
					yylval->id = unquote_string (yytext, yyleng, "@}");
					return T_EQUATION;
				}
<instring>(\\["@]|[^"@])+	{
					yylval->id = unquote_string (yytext, yyleng, "@\"");
					return T_STRING;
				}
<instring>"\""			{
					yy_pop_state (yyscanner);
					return T_STRING_END;
				}

"/"				{
					yy_push_state (inregex, yyscanner);
					return T_REGEX_BEGIN;
				}

<inregex>(\\[/@]|[^/@])+	{
					yylval->id = unquote_string (yytext, yyleng, "@/");
					return T_STRING;
				}
<inregex>"/"			{
					yy_pop_state (yyscanner);
					return T_REGEX_END;
				}

"{"				return '{';
"}"				return '}';
"("				return '(';
")"				return ')';
":"				return ':';
"="				return '=';
"<"				return '<';
","				return ',';
"."				return '.';
"|"				return '|';
"["				return '[';
"]"				return ']';

%%

static gchar *
unquote_string (gchar *s, gint len, gchar const *escape)
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
