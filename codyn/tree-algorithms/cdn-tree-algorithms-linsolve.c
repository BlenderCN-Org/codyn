#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"
#include "codyn/instructions/cdn-instruction-number.h"
#include <codyn/cdn-symbolic.h>
#include <codyn/cdn-debug.h>

static CdnExpressionTreeIter *
make_coefficient (CdnExpressionTreeIter  *root,
                  CdnExpressionTreeIter  *child)
{
	CdnExpressionTreeIter *parent;
	gint idx;

	if (!child)
	{
		return NULL;
	}

	if (root == child)
	{
		cdn_expression_tree_iter_free (child);
		return iter_new_numstr ("1");
	}

	parent = child->parent;
	idx = iter_index_of (parent, child);

	if (idx >= 0)
	{
		CdnExpressionTreeIter *brother;

		brother = parent->children[!idx];
		parent->children[!idx] = NULL;
		brother->parent = NULL;

		iter_replace_into (brother, parent);
	}

	return root;
}

static CdnExpressionTreeIter *
sum_terms (GSList *terms,
           GSList *ignore)
{
	CdnExpressionTreeIter *root;
	CdnExpressionTreeIter *cur;
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

	/* TODO: set the argdim */
	root = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
	                                                     "+",
	                                                     2,
	                                                     NULL),
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
			CdnExpressionTreeIter *np;

			/* TODO: argdim */
			np = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
			                                                   "+",
			                                                   2,
			                                                   NULL),
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

static CdnExpressionTreeIter *
solve_coefficient (CdnExpressionTreeIter  *root,
                   CdnExpressionTreeIter  *child,
                   CdnExpressionTreeIter **coefficient,
                   GError                **error)
{
	CdnExpressionTreeIter *parent = child->parent;
	GSList *terms = NULL;
	CdnExpressionTreeIter *prev = child;
	GSList *myterm = NULL;

	while (parent)
	{
		if (iter_is_plus (parent))
		{
			gboolean first = !terms;
			gint i;
			CdnExpressionTreeIter *tmp;

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

			cdn_expression_tree_iter_free (tmp);
		}
		else
		{
			gboolean ismult = iter_is_multiply (parent);
			gboolean isdiv = !ismult && iter_is_divide (parent);

			gint idx = iter_index_of (parent, prev);

			if (!ismult && (!isdiv || idx == 1))
			{
				g_set_error (error,
				             CDN_SYMBOLIC_ERROR,
				             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
				             "Expression `%s' for linear solve is not linear in `%s'",
				             cdn_expression_tree_iter_to_string (root),
				             cdn_expression_tree_iter_to_string (child));

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
					CdnExpressionTreeIter *cp;

					cp = cdn_expression_tree_iter_copy (parent);

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
	cdn_expression_tree_iter_free (root);

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
 * cdn_expression_tree_iter_solve_for:
 * @iter: A #CdnExpressionTreeIter
 * @prop: A #CdnVariable
 *
 * NOTE: This is only supposed to work for canonical expressions linear in
 * @prop
 *
 * Returns: A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_solve_for (CdnExpressionTreeIter  *iter,
                                    CdnVariable            *prop,
                                    GError                **error)
{
	GSList *props;
	GSList *item;
	CdnExpressionTreeIter *inv;
	GError *err = NULL;
	gboolean retval = TRUE;
	GSList *coefs = NULL;
	CdnExpressionTreeIter *summed;
	CdnExpressionTreeIter *div;

	g_return_val_if_fail (CDN_IS_VARIABLE (prop), NULL);

	// We are going to do a solve iter for prop assuming F(iter) = 0
	props = iter_remove_variables (iter, prop, NULL);

	cdn_debug_message (DEBUG_LINSOLVE, "Solving for {%s}: {%s}",
	                   cdn_variable_get_name (prop),
	                   cdn_expression_tree_iter_to_string (iter));

	if (!props)
	{
		g_set_error (error,
		             CDN_SYMBOLIC_ERROR,
		             CDN_SYMBOLIC_ERROR_INVALID,
		             "Expression {%s} cannot be solved towards {%s}",
		             cdn_expression_tree_iter_to_string (iter),
		             cdn_variable_get_name (prop));

		cdn_expression_tree_iter_free (iter);
		return NULL;
	}

	cdn_debug_message (DEBUG_LINSOLVE,
	                   "Found: %d properties",
	                   g_slist_length (props));

	// Now factor out for each of the instances of prop
	for (item = props; item; item = g_slist_next (item))
	{
		CdnExpressionTreeIter *child = item->data;
		CdnExpressionTreeIter *coef = NULL;

		// Separate child (prop) from the expression, the result is
		// a new expression without the coefficient on prop
		iter = solve_coefficient (iter, child, &coef, &err);

		if (err)
		{
			g_propagate_error (error, err);
			retval = FALSE;
			break;
		}

		cdn_debug_message (DEBUG_LINSOLVE, "Coefficient: {%s} (rest: {%s})",
		                   cdn_expression_tree_iter_to_string (coef),
		                   cdn_expression_tree_iter_to_string (iter));

		coefs = g_slist_prepend (coefs, coef);
	}

	g_slist_free (props);

	if (!retval)
	{
		g_slist_foreach (coefs, (GFunc)cdn_expression_tree_iter_free, NULL);
		g_slist_free (coefs);
		cdn_expression_tree_iter_free (iter);

		return NULL;
	}

	if (!iter)
	{
		// This means that there were only coefficients, and that means
		// no RHS, so return just 0
		g_slist_foreach (coefs, (GFunc)cdn_expression_tree_iter_free, NULL);
		g_slist_free (coefs);

		return iter_new_numstr ("0");
	}

	// Now we have the expression 'iter' and a set of coefficients 'coefs'
	// We need to add the coefs together, negate it and divide the 'iter'
	// expression with it
	summed = cdn_expression_tree_iter_simplify (sum_terms (coefs, NULL));
	iter = cdn_expression_tree_iter_simplify (iter);

	// Now divide inv by summed
	/* TODO: argdim */
	div = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_DIVIDE,
	                                                    "/",
	                                                    2,
	                                                    NULL),
	                      2);

	div->children[0] = iter;
	div->children[0]->parent = div;

	div->children[1] = summed;
	div->children[1]->parent = div;

	/* TODO: argdim */
	inv = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                                    "*",
	                                                    2,
	                                                    NULL),
	                      2);

	inv->children[0] = iter_new_numstr ("-1");
	inv->children[0]->parent = inv;

	inv->children[1] = div;
	inv->children[1]->parent = inv;

	inv = cdn_expression_tree_iter_simplify (inv);

	cdn_debug_message (DEBUG_LINSOLVE, "Solved for {%s}: {%s}\n",
	                   cdn_variable_get_name (prop),
	                   cdn_expression_tree_iter_to_string (inv));

	iter_invalidate_cache_down (inv);

	return inv;
}
