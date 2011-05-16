%{
#include "cpg-parser-context.h"
#include "cpg-parser.h"

static char *unquote_string (char *s, int len, gchar c);

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

%}

%option yylineno
%option noyywrap
%option nounput
%option noinput
%option reentrant
%option prefix="cpg_parser_"
%option bison-bridge bison-locations
%option outfile="lex.yy.c"

%%

[ \t]				;
\n.*				{ yyccolumn = 1;
				  cpg_parser_context_set_line (yyextra, yytext + 1, yylineno);
				  cpg_parser_context_set_column (yyextra, 0, 0);
				  yyless (1);
				}
"#"[^\n]*$			;

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
"attach"			return T_KEY_ATTACH;
"apply"				return T_KEY_APPLY;

[-+]?[0-9]+((\.|eE[-+]?)[0-9]*) {yylval->numf = g_ascii_strtod (yytext, NULL);
				 return T_DOUBLE;}

[a-zA-Z_][a-zA-Z0-9_-]*	 	{yylval->id = g_strdup (yytext);
				 return T_IDENTIFIER;}

[-+]?[0-9]n([-+][0-9])?  	{yylval->id = g_strdup (yytext);
				 return T_SELECTOR_NTH;}

[-+]?[0-9]+			{yylval->numf = (gint)g_ascii_strtoll (yytext, NULL, 10);
				 return T_INTEGER;}

"\""(\\["]|[^"])*"\""		{yylval->id = unquote_string (yytext + 1, yyleng - 2, '"');
				 return T_STRING;}

"/"(\\[/]|[^/])*"/"		{yylval->id = unquote_string (yytext + 1, yyleng - 2, '/');
				 return T_REGEX;}

"{"				return '{';
"}"				return '}';
"("				return '(';
")"				return ')';
":"				return ':';
"="				return '=';
"<"				return '<';
","				return ',';

%%

static gchar *
unquote_string (gchar *s, gint len, gchar c)
{
	gchar *ret;
	gchar *ptr;

	ret = g_strndup (s, len);
	ptr = ret;

	while (*s && len > 0)
	{
		if (*s == '\\' && len > 1 && (*(s + 1) == c || *(s + 1) == '\\'))
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
