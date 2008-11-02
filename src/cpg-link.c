#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-link-private.h"

/**
 * cpg_link_new:
 * @from: the #CpgObject which is the link source
 * @to: the #CpgObject which is the link destination
 *
 * Create a new link object with source @from and destination @to. The link
 * will automatically be installed in @to.
 *
 * Return value: the newly created #CpgLink
 **/
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

/**
 * cpg_link_add_action:
 * @link: the #CpgLink
 * @destination: the destination #CpgProperty
 * @expression: the expression to evaluate and push to @destination
 *
 * Add a new action to be performed when the link is evaluated during
 * simulation. An action consists of a destination property and an expression
 * who's result will be pushed in the destination at every simulation step
 *
 **/
void
cpg_link_add_action(CpgLink *link, CpgProperty *destination, char const *expression)
{
	link->expressions = (CpgExpression **)realloc(link->expressions, sizeof(CpgExpression *) * ++link->num_expressions);
	link->expressions[link->num_expressions - 1] = cpg_expression_new_for_link(link, destination, expression);
	
	// let destination object know that the link has been updated
	cpg_object_update_link(link->to, link);
}

/**
 * cpg_link_free:
 * @link: the #CpgLink
 *
 * Destroy the #CpgLink
 *
 **/
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

/**
 * cpg_link_from:
 * @link: the #CpgLink
 *
 * Returns the from #CpgObject of the link
 *
 * Return value: the from #CpgObject
 *
 **/
CpgObject *
cpg_link_from(CpgLink *link)
{
	return link->from;
}

/**
 * cpg_link_to:
 * @link: the #CpgLink
 *
 * Returns the to #CpgObject of the link
 *
 * Return value: the to #CpgObject
 *
 **/
CpgObject *
cpg_link_to(CpgLink *link)
{
	return link->to;
}

/**
 * cpg_link_actions:
 * @link: the #CpgLink
 * @size: return pointer for the number of actions
 *
 * Returns an array of all actions of the link
 *
 * Return value: array of #CpgExpression
 *
 **/
CpgExpression **
cpg_link_actions(CpgLink *link, unsigned *size)
{
	*size = link->num_expressions;
	return link->expressions;
}
