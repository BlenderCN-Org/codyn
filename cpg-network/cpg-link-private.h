#ifndef __CPG_LINK_PRIVATE_H__
#define __CPG_LINK_PRIVATE_H__

#include "cpg-link.h"
#include "cpg-object-private.h"

struct _CpgLinkAction
{
	CpgExpression *expression;
	CpgProperty *target;
};

struct _CpgLink
{
	CpgObject parent;
	
	// from and to objects
	CpgObject *from;
	CpgObject *to;
	
	// list of expressions to evaluate
	CpgLinkAction **actions;
	unsigned num_actions;
};

#endif /* __CPG_LINK_PRIVATE_H__ */
