#ifndef __CPG_LINK_H__
#define __CPG_LINK_H__

#include "cpg-object.h"
#include "cpg-expression.h"

typedef struct _CpgLink CpgLink;

CpgLink 		 *cpg_link_new				(CpgObject *from, CpgObject *to);
void 			  cpg_link_add_action		(CpgLink *link, CpgProperty *destination, char const *expression);
void 			  cpg_link_free				(CpgLink *link);

CpgObject		 *cpg_link_from				(CpgLink *link);
CpgObject		 *cpg_link_to				(CpgLink *link);

CpgExpression	**cpg_link_actions			(CpgLink *link, unsigned *size);

#endif /* __CPG_LINK_H__ */
