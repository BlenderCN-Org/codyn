#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"
#include <codyn/cdn-debug.h>

CdnExpressionTreeIter *
cdn_expression_tree_iter_substitute (CdnExpressionTreeIter *iter,
                                     CdnVariable           *property,
                                     CdnExpressionTreeIter *subst)
{
	GSList *props;

	cdn_debug_message (DEBUG_LINSOLVE, "Substitute {%s} in {%s} with {%s}",
	                   cdn_variable_get_name (property),
	                   cdn_expression_tree_iter_to_string (iter),
	                   cdn_expression_tree_iter_to_string (subst));

	props = iter_remove_variables (iter, property, NULL);

	while (props)
	{
		CdnExpressionTreeIter *it = props->data;

		iter_copy_into (subst, it);
		props = g_slist_delete_link (props, props);
	}

	iter_invalidate_cache_down (iter);

	return iter;
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_substitute_hash (CdnExpressionTreeIter *iter,
                                          GHashTable            *table)
{
	GSList *props;

	props = iter_remove_variables (iter, NULL, NULL);

	while (props)
	{
		CdnExpressionTreeIter *it = props->data;
		CdnVariable *p;
		CdnExpressionTreeIter *sub;

		p = cdn_instruction_variable_get_variable ((CdnInstructionVariable *)(it->instruction));

		sub = g_hash_table_lookup (table, p);

		if (sub)
		{
			iter_copy_into (sub, it);
		}

		props = g_slist_delete_link (props, props);
	}

	iter_invalidate_cache_down (iter);

	return iter;
}
