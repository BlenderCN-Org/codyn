%{
#include "cpg-parser-private.h"
#include "parser.tab.h"

static char *unquote_string (char *s, int len);

%}

%option yylineno

%%

[ \t\n]				;
"#"[^\n]*$			;

"template"			return T_KEY_TEMPLATE;
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
"import"			return T_KEY_IMPORT;

[-+]?[0-9]+((\.|eE[-+]?)[0-9]*) {yylval.numf = g_ascii_strtod (yytext, NULL);
				 return T_DOUBLE;}

[a-zA-Z_][a-zA-Z0-9.-]* 	{yylval.id = g_strdup (yytext);
				 return T_IDENTIFIER;}

[-+]?[0-9]?n(\s*[-+]\s*[0-9])?  {yylval.id = g_strdup (yytext);
				 return T_SELECTOR_NTH;}

"\""(\\"|[^"])*"\""		{yylval.id = unquote_string (yytext + 1, yyleng - 2);
				 return T_IDENTIFIER;}

"{"				return '{';
"}"				return '}';
"("				return '(';
")"				return ')';
":"				return ':';
"="				return '=';
"<"				return '<';
","				return ',';
":"				return ':';
"["				return '[';
"]"				return ']';

[^;,]*				{yylval.id = (char *)strdup (yytext);
				 return T_ANY;}

%%

static gchar *
unquote_string (gchar *s, gint len)
{
	gchar *ret;
	gchar *ptr;

	ret = g_strndup (s, len);

	while (*s)
	{
		if (*s == '\\' && (*(s + 1) == '"' || *(s + 1) == '\\'))
		{
			++s;
		}

		*ptr++ = *s++;
	}

	*ptr = '\0';
	return ret;
}
