#ifndef __CPG_LINK_H__
#define __CPG_LINK_H__

#include "cpg-object.h"
#include "cpg-expression.h"

typedef struct _CpgLink CpgLink;
typedef struct _CpgLinkAction CpgLinkAction;

CpgLink 		 *cpg_link_new					(char const *id,
												 CpgObject *from, 
												 CpgObject *to);
void 			  cpg_link_free					(CpgLink *link);

CpgObject		 *cpg_link_from					(CpgLink *link);
CpgObject		 *cpg_link_to					(CpgLink *link);

void 			  cpg_link_add_action			(CpgLink *link, 
												 CpgProperty *target, 
												 char const *expression);
CpgLinkAction	**cpg_link_actions				(CpgLink *link, unsigned *size);

CpgExpression	 *cpg_link_action_expression 	(CpgLinkAction *action);
CpgProperty		 *cpg_link_action_target		(CpgLinkAction *action);

#endif /* __CPG_LINK_H__ */
