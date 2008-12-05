#ifndef __CPG_SHARED_LINK_H__
#define __CPG_SHARED_LINK_H__

#include "shared/cpg-shared-expression.h"
#include "shared/cpg-shared-utils.h"

typedef struct
{
	CpgSharedPointer target;
	CpgSharedExpression expression;
} CpgSharedLinkAction;

typedef struct
{
	unsigned num_actions;
	CpgSharedPointer actions;
} CpgSharedLink;

#endif /* __CPG_SHARED_LINK_H__ */

