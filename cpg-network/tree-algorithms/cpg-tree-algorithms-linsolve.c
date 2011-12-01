#include "cpg-network/cpg-expression-tree-iter.h"
#include "cpg-tree-algorithms-private.h"
#include "cpg-network/instructions/cpg-instruction-number.h"
#include <cpg-network/cpg-symbolic.h>
#include <cpg-network/cpg-debug.h>

static CpgExpressionTreeIter *
make_coefficient (CpgExpressionTreeIter  *root,
                  CpgExpressionTreeIter  *child)
{
	CpgExpressionTreeIter *parent;
	gint idx;

	if (!child)
	{
		return NULL;
	}

	if (root == child)
	{
		cpg_expression_tree_iter_free (child);
		return iter_new_numstr ("1");
	}

	parent = child->parent;
	idx = iter_index_of (parent, child);

	if (idx >= 0)
	{
		CpgExpressionTreeIter *brother;

		brother = parent->children[!idx];
		parent->children[!idx] = NULL;
		brother->parent = NULL;

		iter_replace_into (brother, parent);
	}

	return root;
}

static CpgExpressionTreeIter *
sum_terms (GSList *terms,
           GSList *ignore)
{
	CpgExpressionTreeIter *root;
	CpgExpressionTreeIter *cur;
	GSList *item;

	if (!terms || (!terms->next && terms == ignore))
	{
		return NULL;
	}

	if (!terms->next || (!terms->next->next && terms->next == ignore))
	{
		return terms->data;
	}

	if (!terms->next->next && terms == ignore)
	{
		return terms->next->data;
	}

	root = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
	                                                     "+",
	                                                     2),
	                       2);

	cur = root;
	cur->children[0] = terms->data;
	cur->children[0]->parent = cur;

	for (item = terms->next; item; item = g_slist_next (item))
	{
		if (item == ignore)
		{
			continue;
		}

		if (item->next && (item->next != ignore || item->next->next))
		{
			CpgExpressionTreeIter *np;

			np = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
			                                                   "+",
			                                                   2),
			                     2);

			cur->children[1] = np;
			np->parent = cur;

			np->children[0] = item->data;
			np->children[0]->parent = np;

			cur = np;
		}
		else
		{
			cur->children[1] = item->data;
			cur->children[1]->parent = cur;
		}
	}

	return root;
}

static CpgExpressionTreeIter *
solve_coefficient (CpgExpressionTreeIter  *root,
                   CpgExpressionTreeIter  *child,
                   CpgExpressionTreeIter **coefficient,
                   GError                **error)
{
	CpgExpressionTreeIter *parent = child->parent;
	GSList *terms = NULL;
	CpgExpressionTreeIter *prev = child;
	GSList *myterm = NULL;

	while (parent)
	{
		if (iter_is_plus (parent))
		{
			gboolean first = !terms;
			gint i;
			CpgExpressionTreeIter *tmp;

			for (i = 0; i < parent->num_children; ++i)
			{
				if (first || parent->children[i] != prev)
				{
					// Add another term
					terms = g_slist_prepend (terms,
					                         parent->children[i]);

					if (first && parent->children[i] == prev)
					{
						myterm = terms;
					}
				}

				if (parent->children[i])
				{
					parent->children[i]->parent = NULL;
					parent->children[i] = NULL;
				}
			}

			tmp = parent;
			parent = parent->parent;
			iter_replace (tmp, NULL);
			prev = NULL;

			if (tmp == root)
			{
				root = NULL;
			}

			cpg_expression_tree_iter_free (tmp);
		}
		else
		{
			gboolean ismult = iter_is_multiply (parent);
			gboolean isdiv = !ismult && iter_is_divide (parent);

			gint idx = iter_index_of (parent, prev);

			if (!ismult && (!isdiv || idx == 1))
			{
				g_set_error (error,
				             CPG_SYMBOLIC_ERROR,
				             CPG_SYMBOLIC_ERROR_UNSUPPORTED,
				             "Expression `%s' for linear solve is not linear in `%s'",
				             cpg_expression_tree_iter_to_string (root),
				             cpg_expression_tree_iter_to_string (child));

				// TODO: cleanup
				return NULL;
			}

			// Replace ourselves (prev) in parent with NULL
			if (terms)
			{
				GSList *item;

				if (parent->children[idx])
				{
					parent->children[idx]->parent = NULL;
				}

				parent->children[idx] = NULL;

				// We need to apply this to our current terms
				for (item = terms; item; item = g_slist_next (item))
				{
					CpgExpressionTreeIter *cp;

					cp = cpg_expression_tree_iter_copy (parent);

					cp->children[idx] = item->data;
					cp->children[idx]->parent = cp;

					item->data = cp;
				}
			}

			prev = parent;
			parent = parent->parent;
		}
	}

	*coefficient = make_coefficient (myterm ? myterm->data : NULL, child);
	cpg_expression_tree_iter_free (root);

	if (!terms)
	{
		// nothing to separate, no plus operators where encountered
		return NULL;
	}

	root = sum_terms (terms, myterm);

	g_slist_free (terms);
	return root;
}

/**
 * cpg_expression_tree_iter_solve_for:
 * @iter: A #CpgExpressionTreeIter
 * @prop: A #CpgProperty
 *
 * NOTE: This is only supposed to work for canonical expressions linear in
 * @prop
 *
 * Returns: A #CpgExpressionTreeIter
 *
 **/
CpgExpressionTreeIter *
cpg_expression_tree_iter_solve_for (CpgExpressionTreeIter  *iter,
                                    CpgProperty            *prop,
                                    GError                **error)
{
	GSList *props;
	GSList *item;
	CpgExpressionTreeIter *inv;
	GError *err = NULL;
	gboolean retval = TRUE;
	GSList *coefs = NULL;
	CpgExpressionTreeIter *summed;
	CpgExpressionTreeIter *div;

	g_return_val_if_fail (CPG_IS_PROPERTY (prop), NULL);

	// We are going to do a solve iter for prop assuming F(iter) = 0
	props = iter_find_properties (iter, prop, NULL);

	cpg_debug_message (DEBUG_LINSOLVE, "Solving for {%s}: {%s}",
	                   cpg_property_get_name (prop),
	                   cpg_expression_tree_iter_to_string (iter));

	if (!props)
	{
		g_set_error (error,
		             CPG_SYMBOLIC_ERROR,
		             CPG_SYMBOLIC_ERROR_INVALID,
		             "Expression {%s} cannot be solved towards {%s}",
		             cpg_expression_tree_iter_to_string (iter),
		             cpg_property_get_name (prop));

		cpg_expression_tree_iter_free (iter);
		return NULL;
	}

	cpg_debug_message (DEBUG_LINSOLVE,
	                   "Found: %d properties",
	                   g_slist_length (props));

	// Now factor out for each of the instances of prop
	for (item = props; item; item = g_slist_next (item))
	{
		CpgExpressionTreeIter *child = item->data;
		CpgExpressionTreeIter *coef = NULL;

		// Separate child (prop) from the expression, the result is
		// a new expression without the coefficient on prop
		iter = solve_coefficient (iter, child, &coef, &err);

		if (err)
		{
			g_propagate_error (error, err);
			retval = FALSE;
			break;
		}

		cpg_debug_message (DEBUG_LINSOLVE, "Coefficient: {%s} (rest: {%s})",
		                   cpg_expression_tree_iter_to_string (coef),
		                   cpg_expression_tree_iter_to_string (iter));

		coefs = g_slist_prepend (coefs, coef);
	}

	g_slist_free (props);

	if (!retval)
	{
		g_slist_foreach (coefs, (GFunc)cpg_expression_tree_iter_free, NULL);
		g_slist_free (coefs);
		cpg_expression_tree_iter_free (iter);

		return NULL;
	}

	if (!iter)
	{
		// This means that there were only coefficients, and that means
		// no RHS, so return just 0
		g_slist_foreach (coefs, (GFunc)cpg_expression_tree_iter_free, NULL);
		g_slist_free (coefs);

		return iter_new_numstr ("0");
	}

	// Now we have the expression 'iter' and a set of coefficients 'coefs'
	// We need to add the coefs together, negate it and divide the 'iter'
	// expression with it
	summed = cpg_expression_tree_iter_simplify (sum_terms (coefs, NULL));
	iter = cpg_expression_tree_iter_simplify (iter);

	// Now divide inv by summed
	div = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_DIVIDE,
	                                                    "/",
	                                                    2),
	                      2);

	div->children[0] = iter;
	div->children[0]->parent = div;

	div->children[1] = summed;
	div->children[1]->parent = div;

	inv = iter_new_sized (cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	                                                    "*",
	                                                    2),
	                      2);

	inv->children[0] = iter_new_numstr ("-1");
	inv->children[0]->parent = inv;

	inv->children[1] = div;
	inv->children[1]->parent = inv;

	inv = cpg_expression_tree_iter_simplify (inv);

	cpg_debug_message (DEBUG_LINSOLVE, "Solved for {%s}: {%s}\n",
	                   cpg_property_get_name (prop),
	                   cpg_expression_tree_iter_to_string (inv));

	iter_invalidate_cache_down (inv);

	return inv;
}
