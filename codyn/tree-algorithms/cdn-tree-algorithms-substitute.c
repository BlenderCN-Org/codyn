#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"
#include <codyn/cdn-debug.h>

/**
 * cdn_expression_tree_iter_substitute:
 * @iter: the #CdnExpressionTreeIter
 * @variable: the variable to substitute
 * @subst: the expression to substitute with
 *
 * Substitute a variable with a given expression in a tree iter.
 *
 * Returns: (transfer none): @iter
 *
 */
CdnExpressionTreeIter *
cdn_expression_tree_iter_substitute (CdnExpressionTreeIter *iter,
                                     CdnVariable           *variable,
                                     CdnExpressionTreeIter *subst)
{
	GSList *props;

	cdn_debug_message (DEBUG_LINSOLVE, "Substitute {%s} in {%s} with {%s}",
	                   cdn_variable_get_name (variable),
	                   cdn_expression_tree_iter_to_string (iter),
	                   cdn_expression_tree_iter_to_string (subst));

	props = iter_remove_variables (iter, variable, NULL);

	while (props)
	{
		CdnExpressionTreeIter *it = props->data;

		iter_copy_into (subst, it);
		props = g_slist_delete_link (props, props);
	}

	iter_invalidate_cache_down (iter);

	return iter;
}

/**
 * cdn_expression_tree_iter_substitute_hash:
 * @iter: the #CdnExpressionTreeIter
 * @table: (element-type CdnVariable CdnExpressionTreeIter): the variables to substitute
 *
 * Substitute multiple variables from a hash table, #CdnVariable to #CdnExpressionTreeIter.
 *
 * Returns: (transfer none): @iter
 *
 */
CdnExpressionTreeIter *
cdn_expression_tree_iter_substitute_hash (CdnExpressionTreeIter *iter,
                                          GHashTable            *table)
{
	GSList *variables;

	variables = iter_remove_variables (iter, NULL, NULL);

	while (variables)
	{
		CdnExpressionTreeIter *it = variables->data;
		CdnVariable *p;
		CdnExpressionTreeIter *sub;

		p = cdn_instruction_variable_get_variable ((CdnInstructionVariable *)(it->instruction));

		sub = g_hash_table_lookup (table, p);

		if (sub)
		{
			iter_copy_into (sub, it);
		}

		variables = g_slist_delete_link (variables, variables);
	}

	iter_invalidate_cache_down (iter);

	return iter;
}
