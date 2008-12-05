#include "shared/cpg-shared-property.h"

void
cpg_shared_property_set_value(CpgSharedProperty *property, 
							  double             value,
							  void				*base)
{
	cpg_shared_expression_set_value(&(property->value), value, base);
}

double
cpg_shared_property_value(CpgSharedProperty *property, void *base)
{
	return cpg_shared_expression_evaluate(&(property->value), base);
}
