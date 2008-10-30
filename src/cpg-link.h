#ifndef __CPG_LINK_H__
#define __CPG_LINK_H__

#include "cpg-object.h"
#include "cpg-expression.h"

typedef struct _CpgLink CpgLink;

struct _CpgLink
{
	CpgObject parent;
	
	// from and to objects
	CpgObject *from;
	CpgObject *to;
	
	// list of expressions to evaluate
	CpgExpression **expressions;
	unsigned num_expressions;
};

CpgLink 	*cpg_link_new				(CpgObject *from, CpgObject *to);
void 		 cpg_link_add_expression	(CpgLink *link, CpgProperty *destination, char const *expression);
void 		 cpg_link_free				(CpgLink *link);

#endif /* __CPG_LINK_H__ */
