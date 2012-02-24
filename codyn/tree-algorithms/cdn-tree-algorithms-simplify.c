#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"
#include "codyn/cdn-stack-private.h"
#include <codyn/cdn-debug.h>

static gboolean iter_simplify (CdnExpressionTreeIter *iter,
                               gboolean               simplify_children);

static gboolean simplify_power (CdnExpressionTreeIter *iter);
static gboolean simplify_plus (CdnExpressionTreeIter *iter);
static gboolean simplify_multiply (CdnExpressionTreeIter *iter);

static gboolean
cmp_double (gdouble a, gdouble b)
{
	return fabs (a - b) < 10e-9;
}

static gboolean
simplify_function (CdnExpressionTreeIter *iter)
{
	CdnInstructionFunction *f;
	gint id;
	gint i;
	CdnStack stack;

	f = (CdnInstructionFunction *)(iter->instruction);
	id = cdn_instruction_function_get_id (f);

	if (cdn_math_function_is_variable (id))
	{
		return FALSE;
	}

	cdn_stack_init (&stack, iter->num_children + 1);

	// Check if all arguments are numeric
	for (i = 0; i < iter->num_children; ++i)
	{
		gdouble num;

		if (!iter_is_number (iter->children[i], &num))
		{
			cdn_stack_destroy (&stack);
			return FALSE;
		}

		cdn_stack_push (&stack, num);
	}

	cdn_math_function_execute (id,
	                           iter->num_children,
	                           cdn_instruction_function_get_arguments_dimension (f),
	                           &stack);

	for (i = 0; i < iter->num_children; ++i)
	{
		cdn_expression_tree_iter_free (iter->children[i]);
	}

	g_free (iter->children);
	iter->children = NULL;
	iter->num_children = 0;

	cdn_mini_object_unref (CDN_MINI_OBJECT (iter->instruction));
	iter->instruction = cdn_instruction_number_new (cdn_stack_pop (&stack));

	cdn_stack_destroy (&stack);

	iter_canonical_resort (iter);
	return TRUE;
}

static CdnExpressionTreeIter *
multiply_make_powers_terms (CdnExpressionTreeIter  *iter,
                            CdnExpressionTreeIter **powerand)
{
	if (iter_is_power (iter))
	{
		*powerand = iter->children[1];
		return iter->children[0];
	}
	else
	{
		*powerand = NULL;
		return iter;
	}
}

static CdnExpressionTreeIter *
iter_new_plus (CdnExpressionTreeIter *c1,
               CdnExpressionTreeIter *c2)
{
	CdnExpressionTreeIter *ret;

	if (!c1 && !c2)
	{
		return iter_new_numstr ("0");
	}
	else if (!c1)
	{
		return c2;
	}
	else if (!c2)
	{
		return c1;
	}

	/* TODO: argdim */
	ret = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
	                                                    "+",
	                                                    2,
	                                                    NULL),
	                      2);

	iter_set_child (ret, c1, 0);
	iter_set_child (ret, c2, 1);

	if (iter_canonicalize (ret, FALSE, FALSE))
	{
		iter_simplify (ret, TRUE);
	}
	else
	{
		simplify_plus (ret);
	}

	return ret;
}

static CdnExpressionTreeIter *
iter_new_multiply (CdnExpressionTreeIter *c1,
                   CdnExpressionTreeIter *c2)
{
	CdnExpressionTreeIter *ret;

	if (!c1 && !c2)
	{
		return iter_new_numstr ("0");
	}
	else if (!c1)
	{
		return c2;
	}
	else if (!c2)
	{
		return c1;
	}

	/* TODO: argdim */
	ret = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                                    "*",
	                                                    2,
	                                                    NULL),
	                      2);

	iter_set_child (ret, c1, 0);
	iter_set_child (ret, c2, 1);

	if (iter_canonicalize (ret, FALSE, FALSE))
	{
		iter_simplify (ret, TRUE);
	}
	else
	{
		simplify_multiply (ret);
	}

	return ret;
}

static gboolean
multiply_make_powers (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *t1;
	CdnExpressionTreeIter *p1;
	CdnExpressionTreeIter *t2;
	CdnExpressionTreeIter *p2;
	CdnExpressionTreeIter *plus;
	CdnExpressionTreeIter *power;
	gboolean rism;
	gboolean optim;

	t1 = multiply_make_powers_terms (iter->children[0],
	                                 &p1);

	t2 = iter->children[1];
	rism = FALSE;

	if (iter_is_multiply (iter->children[1]))
	{
		t2 = iter->children[1]->children[0];
		rism = TRUE;
	}

	t2 = multiply_make_powers_terms (t2,
	                                 &p2);

	if (!cdn_expression_tree_iter_equal (t1, t2, FALSE))
	{
		return FALSE;
	}

	// Going to do t1^(p1 + p2)
	if (!p1)
	{
		p1 = iter_new_numstr ("1");
	}

	if (!p2)
	{
		p2 = iter_new_numstr ("1");
	}

	plus = iter_new_plus (p1, p2);

	/* TODO: argdim */
	power = iter_new_sized (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                      "^",
	                                                      2,
	                                                      NULL),
	                        2);

	iter_set_child (power, t1, 0);
	iter_set_child (power, plus, 1);

	optim = simplify_power (power);

	if (rism)
	{
		iter_replace_into (power, iter->children[1]->children[0]);
		iter_canonicalize (iter->children[1], FALSE, FALSE);

		iter_replace_into (iter->children[1], iter);

		if (optim)
		{
			// Recurse
			simplify_multiply (iter);
		}
	}
	else
	{
		iter_replace_into (power, iter);
	}

	return TRUE;
}

static gboolean
simplify_multiply (CdnExpressionTreeIter *iter)
{
	gdouble num1;
	gdouble num2;
	gboolean isnum1;
	gboolean isnum2;

	if (simplify_function (iter))
	{
		return TRUE;
	}

	if (multiply_make_powers (iter))
	{
		return TRUE;
	}

	isnum1 = iter_is_number (iter->children[0], &num1);

	if (!isnum1)
	{
		return FALSE;
	}

	isnum2 = iter_is_multiply (iter->children[1]) &&
	         iter_is_number (iter->children[1]->children[0], &num2);

	if (isnum1 && isnum2)
	{
		CdnInstructionNumber *nl;

		// Here we apply: n1 * n2 * X = n3 * X (with n3 = n1 * n2)
		// i.e. preadding to numeric constants
		nl = (CdnInstructionNumber *)(iter->children[0]->instruction);

		cdn_instruction_number_set_value (nl,
		                                  num1 * num2);

		// Replace the multiply operator on the right, with the right hand
		// side of the operator
		iter_replace_into (iter->children[1]->children[1],
		                   iter->children[1]);

		iter_canonicalize (iter, FALSE, FALSE);

		// Recurse
		simplify_multiply (iter);
	}
	else if (cmp_double (num1, 0))
	{
		// Bubble up the 0
		iter_replace_into (iter->children[0], iter);
	}
	else if (cmp_double (num1, 1))
	{
		// Replace with rhs
		iter_replace_into (iter->children[1], iter);
	}
	else
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
simplify_cossin2_identity (CdnExpressionTreeIter *iter)
{
	gdouble pnum1;
	gdouble pnum2;
	CdnMathFunctionType type1;
	CdnMathFunctionType type2;
	CdnExpressionTreeIter *it2;
	CdnExpressionTreeIter *cmp1;
	CdnExpressionTreeIter *cmp2;
	gdouble num1 = 1;
	gdouble num2 = 1;

	cmp1 = iter->children[0];
	it2 = cmp2 = iter->children[1];

	if (iter_is_plus (cmp2))
	{
		cmp2 = cmp2->children[0];
	}

	if (!(iter_is_power (cmp1) &&
	      iter_is_power (cmp2) &&
	      iter_is_number (cmp1->children[1], &pnum1) &&
	      iter_is_number (cmp2->children[1], &pnum2) &&
	      cmp_double (pnum1, 2) &&
	      cmp_double (pnum2, 2)))
	{
		return FALSE;
	}

	if (iter_is_function (cmp1->children[0], &type1) &&
	    iter_is_function (cmp2->children[0], &type2) &&
	    ((type1 == CDN_MATH_FUNCTION_TYPE_COS && type2 == CDN_MATH_FUNCTION_TYPE_SIN) ||
	     (type1 == CDN_MATH_FUNCTION_TYPE_SIN && type2 == CDN_MATH_FUNCTION_TYPE_COS)))
	{
		gdouble m = num1 * num2;
		CdnExpressionTreeIter *num;

		num = iter_new (cdn_instruction_number_new (m));

		if (iter_is_plus (it2))
		{
			CdnExpressionTreeIter *part;

			part = it2->children[1];
			iter_replace (part, NULL);

			iter_replace_into (iter_new_plus (num,
			                                  part),
			                   iter);
		}
		else
		{
			iter_replace_into (num, iter);
		}

		return TRUE;
	}

	return FALSE;
}

static CdnExpressionTreeIter *
make_rest (CdnExpressionTreeIter *left,
           gboolean               lism)
{
	CdnExpressionTreeIter *lrest = NULL;

	if (left)
	{
		if (lism)
		{
			lrest = left->children[1];

			iter_replace (lrest, NULL);
			iter_replace_into (left->children[0], left);
		}
		else
		{
			CdnExpressionTreeIter *parent;

			lrest = left;
			parent = left->parent;

			iter_replace (left, NULL);

			iter_replace_into (parent->children[0],
			                   parent);
		}
	}

	return lrest;
}

static gboolean
simplify_factorize (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *left;
	CdnExpressionTreeIter *right;
	CdnExpressionTreeIter *lstart;
	CdnExpressionTreeIter *rstart;
	CdnExpressionTreeIter *lrest;
	CdnExpressionTreeIter *rrest;
	CdnExpressionTreeIter *lnum = NULL;
	CdnExpressionTreeIter *rnum = NULL;
	CdnExpressionTreeIter *ret;
	gboolean lism = FALSE;
	gboolean rism = FALSE;

	left = lstart = iter->children[0];
	right = rstart = iter->children[1];

	if (iter_is_plus (iter->children[1]))
	{
		right = rstart = iter->children[1]->children[0];
	}

	// We can factorize an 'addition' if both operands are multiplier trees
	// and they have common heads (handling numbers correctly)
	// Compare head to head, ignoring the first number if there is one
	if (left->num_children &&
	    iter_is_number (left->children[0], NULL))
	{
		lnum = left->children[0];
		left = lstart = left->children[1];
	}

	if (right->num_children &&
	    iter_is_number (right->children[0], NULL))
	{
		rnum = right->children[0];
		right = rstart = right->children[1];
	}

	while (TRUE)
	{
		CdnExpressionTreeIter *cmp1;
		CdnExpressionTreeIter *cmp2;

		lism = iter_is_multiply (left);
		rism = iter_is_multiply (right);

		cmp1 = lism ? left->children[0] : left;
		cmp2 = rism ? right->children[0] : right;

		if (!cdn_expression_tree_iter_equal (cmp1, cmp2, FALSE))
		{
			break;
		}

		left = lism ? left->children[1] : NULL;
		right = rism ? right->children[1] : NULL;

		if (!lism || !rism)
		{
			break;
		}
	}

	// Common part is now from lstart to left and
	// from rstart to right
	if (left == lstart || right == rstart)
	{
		// No common part
		return FALSE;
	}

	// They are both very common
	if (!lnum)
	{
		lnum = iter_new_numstr ("1");
	}

	if (!rnum)
	{
		rnum = iter_new_numstr ("1");
	}

	// com * (lnum * lrest + rnum * rrest)
	lrest = make_rest (left, lism);
	rrest = make_rest (right, rism);

	ret = iter_new_multiply (lstart,
	                         iter_new_plus (iter_new_multiply (lnum, lrest),
	                                        iter_new_multiply (rnum, rrest)));

	if (iter_is_plus (iter->children[1]))
	{
		ret = iter_new_plus (ret,
		                     iter->children[1]->children[1]);
	}

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
simplify_plus (CdnExpressionTreeIter *iter)
{
	gdouble rnum = 0;
	gdouble lnum = 0;
	gboolean islnum;

	// This is for numerical addition when both arguments are numerical
	if (simplify_function (iter))
	{
		return TRUE;
	}

	if (simplify_cossin2_identity (iter))
	{
		return TRUE;
	}

	if (simplify_factorize (iter))
	{
		return TRUE;
	}

	// In canonical form, two numbers in a tree of additions always appear
	// on the left hand side of the first two plus operators in the tree
	islnum = iter_is_number (iter->children[0], &lnum);

	if (!islnum)
	{
		return FALSE;
	}

	if (iter_is_plus (iter->children[1]) &&
	    iter_is_number (iter->children[1]->children[0], &rnum))
	{
		CdnInstructionNumber *nl;

		// Here we apply: n1 + n2 + X = n3 + X
		// i.e. preadding to numeric constants
		nl = (CdnInstructionNumber *)(iter->children[0]->instruction);

		cdn_instruction_number_set_value (nl,
		                                  lnum + rnum);

		// Replace the plus operator on the right, with the right hand
		// side of the operator
		iter_replace_into (iter->children[1]->children[1], iter->children[1]);
		iter_canonicalize (iter->children[1], FALSE, FALSE);
	}
	else if (cmp_double (lnum, 0))
	{
		// Here we apply the rule: 0 + a = a
		iter_replace_into (iter->children[1], iter);
	}
	else
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
simplify_divide (CdnExpressionTreeIter *iter)
{
	gdouble num1;
	gdouble num2;
	gboolean isnum1;
	gboolean isnum2;
	CdnExpressionTreeIter *first;
	CdnExpressionTreeIter *second;

	first = iter->children[0];
	second = iter->children[1];

	isnum2 = iter_is_number (second, &num2);

	if (isnum2 && cmp_double (num2, 0))
	{
		// Division by zero!
		g_warning ("Simplification detected division by zero...");
		return FALSE;
	}

	if (simplify_function (iter))
	{
		return TRUE;
	}

	isnum1 = iter_is_number (first, &num1);

	if ((isnum1 && cmp_double (num1, 0)) ||
	    (isnum2 && cmp_double (num2, 1)))
	{
		// Division of 0 by something is 0 and
		// division of something by 1 is something
		iter_replace_into (first, iter);
	}
	else
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
simplify_power (CdnExpressionTreeIter *iter)
{
	gdouble num1;
	gdouble num2;
	gboolean isnum1;
	gboolean isnum2;
	CdnExpressionTreeIter *first;
	CdnExpressionTreeIter *second;

	if (simplify_function (iter))
	{
		return TRUE;
	}

	first = iter->children[0];
	second = iter->children[1];

	isnum1 = iter_is_number (first, &num1);
	isnum2 = iter_is_number (second, &num2);

	if (isnum1 && cmp_double (num1, 0))
	{
		iter_replace_into (iter_new_numstr ("0"), iter);
	}
	else if (isnum2 && cmp_double (num2, 0))
	{
		iter_replace_into (iter_new_numstr ("1"), iter);
	}
	else if (isnum2 && cmp_double (num2, 1))
	{
		iter_replace_into (iter->children[0], iter);
	}
	else
	{
		return FALSE;
	}

	return TRUE;
}

static gboolean
iter_simplify (CdnExpressionTreeIter *iter,
               gboolean              simplify_children)
{
	gboolean ret = FALSE;

	if (simplify_children)
	{
		gint num;
		gint i;

		num = iter->num_children;

		// First simplify the children
		for (i = 0; i < num; ++i)
		{
			if (iter_simplify (iter->children[i], TRUE))
			{
				ret = TRUE;
			}
		}

		if (ret)
		{
			// Could have resorted the children
			if (iter_canonicalize (iter, FALSE, FALSE))
			{
				// Recurse here
				iter_simplify (iter, TRUE);
				return TRUE;
			}
		}
	}

	// Then simplify the iter itself
	if (iter_is_multiply (iter))
	{
		// Try to premultiply
		if (simplify_multiply (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_plus (iter))
	{
		// Try to preadd
		if (simplify_plus (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_divide (iter))
	{
		if (simplify_divide (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_power (iter))
	{
		if (simplify_power (iter))
		{
			ret = TRUE;
		}
	}
	else if (CDN_IS_INSTRUCTION_FUNCTION (iter->instruction))
	{
		if (simplify_function (iter))
		{
			ret = TRUE;
		}
	}

	return ret;
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_simplify (CdnExpressionTreeIter *iter)
{
	cdn_expression_tree_iter_canonicalize (iter);

	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Simplifying: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	iter_simplify (iter, TRUE);
	iter_invalidate_cache_down (iter);

	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Simplified:  {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	return iter;
}
