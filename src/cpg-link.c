#include "cpg-link.h"
#include "cpg-utils.h"
#include "cpg-link-private.h"

#include <string.h>

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
cpg_link_new(char const *id, CpgObject *from, CpgObject *to)
{
	CpgLink *res = cpg_new1(CpgLink);
	
	cpg_object_initialize(&res->parent, CPG_OBJECT_TYPE_LINK);
	
	if (id)
		res->parent.id = cpg_strdup(id);
	
	res->from = from;
	res->to = to;

	res->actions = NULL;
	res->num_actions = 0;
	
	// register linkn in target
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
cpg_link_add_action(CpgLink *link, CpgProperty *target, char const *expression)
{
	array_resize(link->actions, CpgLinkAction *, ++link->num_actions);
	
	CpgLinkAction **action = &(link->actions[link->num_actions - 1]);
	*action = cpg_new1(CpgLinkAction);
	
	(*action)->expression = cpg_expression_new(expression);
	
	// target is a borrowed reference
	(*action)->target = target;
	
	// let target object know that link has changed
	cpg_object_update_link(link->to, link);
}

static void
link_action_free(CpgLinkAction *action)
{
	cpg_expression_free(action->expression);
	
	// do not free target, borrowed reference
	free(action);
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
	
	for (i = 0; i < link->num_actions; ++i)
		link_action_free(link->actions[i]);
	
	if (link->actions)
		free(link->actions);

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
 * Return value: array of #CpgLinkAction pointers
 *
 **/
CpgLinkAction **
cpg_link_actions(CpgLink *link, unsigned *size)
{
	*size = link->num_actions;
	return link->actions;
}

/**
 * cpg_link_action_expression:
 * @action: the #CpgLinkAction
 *
 * Returns the expression associated with the action
 *
 * Return value: the #CpgExpression associated with the action. It is owned
 * by the action object and should not be freed.
 *
 **/
CpgExpression *
cpg_link_action_expression(CpgLinkAction *action)
{
	return action->expression;
}

/**
 * cpg_link_action_target:
 * @action: the #CpgLinkAction
 *
 * Returns the target #CpgProperty associated with the action
 *
 * Return value: the target #CpgProperty. It is owned
 * by the action object and should not be freed.
 *
 **/
CpgProperty	*
cpg_link_action_target(CpgLinkAction *action)
{
	return action->target;
}

