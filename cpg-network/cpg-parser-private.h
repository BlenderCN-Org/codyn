#ifndef __CPG_PARSER_PRIVATE_H__
#define __CPG_PARSER_PRIVATE_H__

#include "cpg-parser-context.h"

G_BEGIN_DECLS

typedef union
{
	char *id;
	CpgProperty *property;
	CpgPropertyFlags flags;
	gdouble numf;
	GSList *list;
	GArray *array;
	CpgFunctionPolynomialPiece *piece;
	CpgFunctionArgument *argument;
	CpgObject *object;
} yystype;

#define YYSTYPE yystype

int yyget_lineno (void);
int yylex();
int yyparse(CpgParserContext *context);

G_END_DECLS

#endif /* __CPG_PARSER_PRIVATE_H__ */

