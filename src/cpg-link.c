#include "cpg-link.h"
#include "cpg-utils.h"

CpgLink *
cpg_link_new(CpgObject *from, CpgObject *to)
{
	CpgLink *res = cpg_new1(CpgLink);
	
	cpg_object_initialize(&res->parent, CPG_OBJECT_TYPE_LINK);
	
	res->from = from;
	res->to = to;

	res->expressions = 0;
	res->num_expressions = 0;
	
	cpg_object_link(res->to, res);
		
	return res;
}

void
cpg_link_add_action(CpgLink *link, CpgProperty *destination, char const *expression)
{
	link->expressions = (CpgExpression **)realloc(link->expressions, sizeof(CpgExpression *) * ++link->num_expressions);
	link->expressions[link->num_expressions - 1] = cpg_expression_new_for_link(link, destination, expression);
	
	// let destination object know that the link has been updated
	cpg_object_update_link(link->to, link);
}

void
cpg_link_free(CpgLink *link)
{
	if (!link)
		return;

	cpg_object_destroy(&link->parent);
	unsigned i;
	
	for (i = 0; i < link->num_expressions; ++i)
		cpg_expression_free(link->expressions[i]);
	
	free(link->expressions);
	free(link);
}
