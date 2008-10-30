#include <string.h>

#include "cpg-property.h"
#include "cpg-expression.h"
#include "cpg-utils.h"

CpgProperty *
cpg_property_new(char const *name, char const *expression, char integrated)
{
	CpgProperty *res = cpg_new1(CpgProperty);
	
	res->name = strdup(name);

	res->value = NULL;
	res->update = 0.0;

	res->integrated = integrated;	
	res->initial = cpg_expression_new(expression);
	
	// set current value to copy of initial value
	res->value = cpg_expression_copy(res->initial);
	
	return res;
}

void
cpg_property_free(CpgProperty *property)
{
	if (!property)
		return;

	cpg_expression_free(property->value);
	cpg_expression_free(property->initial);

	free(property->name);
	free(property);
}
