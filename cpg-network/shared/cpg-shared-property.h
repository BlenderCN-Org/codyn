#ifndef __CPG_SHARED_PROPERTY_H__
#define __CPG_SHARED_PROPERTY_H__

#include <cpg-network/shared/cpg-shared-expression.h>
#include <cpg-network/shared/cpg-shared-utils.h>

typedef struct
{
	CpgSharedExpression value;
	double update;
	char integrated;
} CpgSharedProperty;

double cpg_shared_property_value				(CpgSharedProperty *property, 
												 void 			   *base);
void cpg_shared_property_set_value				(CpgSharedProperty *property,
												 double				value, 
												 void 			   *base);

#endif /* __CPG_SHARED_PROPERTY_H__ */

