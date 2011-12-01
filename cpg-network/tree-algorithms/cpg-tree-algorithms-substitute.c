#include "cpg-network/cpg-expression-tree-iter.h"
#include "cpg-tree-algorithms-private.h"
#include <cpg-network/cpg-debug.h>

CpgExpressionTreeIter *
cpg_expression_tree_iter_substitute (CpgExpressionTreeIter *iter,
                                     CpgProperty           *property,
                                     CpgExpressionTreeIter *subst)
{
	GSList *props;

	cpg_debug_message (DEBUG_LINSOLVE, "Substitute {%s} in {%s} with {%s}",
	                   cpg_property_get_name (property),
	                   cpg_expression_tree_iter_to_string (iter),
	                   cpg_expression_tree_iter_to_string (subst));

	props = iter_find_properties (iter, property, NULL);

	while (props)
	{
		CpgExpressionTreeIter *it = props->data;

		iter_copy_into (subst, it);
		props = g_slist_delete_link (props, props);
	}

	iter_invalidate_cache_down (iter);

	return iter;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_substitute_hash (CpgExpressionTreeIter *iter,
                                          GHashTable            *table)
{
	GSList *props;

	props = iter_find_properties (iter, NULL, NULL);

	while (props)
	{
		CpgExpressionTreeIter *it = props->data;
		CpgProperty *p;
		CpgExpressionTreeIter *sub;

		p = cpg_instruction_property_get_property ((CpgInstructionProperty *)(it->instruction));

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
