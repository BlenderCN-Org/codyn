#ifndef __CPG_LINK_PRIVATE_H__
#define __CPG_LINK_PRIVATE_H__

#include "cpg-link.h"

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

#endif /* __CPG_LINK_PRIVATE_H__ */
