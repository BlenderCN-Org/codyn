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
cmp_double_all (gdouble const *a,
                gint num,
                gdouble b)
{
	gint i;

	if (num == 1)
	{
		return cmp_double (a[0], b);
	}

	for (i = 0; i < num; ++i)
	{
		if (!cmp_double (a[i], b))
		{
			return FALSE;
		}
	}

	return TRUE;
}


static gboolean
iter_is_number_matrix (CdnExpressionTreeIter  *iter,
                       gdouble               **ret,
                       gint                   *numr,
                       gint                   *numc)
{
	gdouble num;

	if (iter_is_number (iter, &num))
	{
		*numr = 1;
		*numc = 1;

		*ret = g_new (gdouble, 1);
		**ret = num;

		return TRUE;
	}
	else if (CDN_IS_INSTRUCTION_MATRIX (iter->instruction))
	{
		CdnStackManipulation const *smanip;
		gint i;

		smanip = cdn_instruction_get_stack_manipulation (iter->instruction,
		                                                 NULL);

		cdn_stack_manipulation_get_push_dimension (smanip,
		                                           0,
		                                           numr,
		                                           numc);

		*ret = g_new (gdouble, *numr * *numc);

		for (i = 0; i < iter->num_children; ++i)
		{
			if (!iter_is_number (iter->children[i], &num))
			{
				g_free (*ret);
				*ret = NULL;
				return FALSE;
			}

			(*ret)[i] = num;
		}

		return TRUE;
	}
	else
	{
		*ret = NULL;
	}

	return FALSE;
}

static gint
compute_index (gdouble const *valuesa,
               gdouble const *valuesb,
               gint           idx,
               gint           numcc)
{
	if (valuesb)
	{
		return (gint)rint (valuesa[idx]) * numcc + (gint)rint (valuesb[idx]);
	}

	return (gint)rint (valuesa[idx]);
}

static gboolean
simplify_index (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *last;
	gint numar;
	gint numac;
	gint numcc;
	gint numcr;
	gint num;
	gboolean isnuma;
	gdouble *valuesa;
	gdouble *valuesb = NULL;
	CdnExpressionTreeIter *ret;
	CdnStackManipulation const *smanip;

	last = iter->children[iter->num_children - 1];

	if (!CDN_IS_INSTRUCTION_MATRIX (last->instruction))
	{
		smanip = cdn_instruction_get_stack_manipulation (last->instruction,
		                                                 NULL);

		if (!smanip->push_dims || (smanip->push_dims[0] == 1 && smanip->push_dims[1] == 1))
		{
			iter_replace_into (last, iter);
			return TRUE;
		}

		return FALSE;
	}

	// Check if first (and second) arg are numeric
	isnuma = iter_is_number_matrix (iter->children[0],
	                                &valuesa,
	                                &numar,
	                                &numac);

	if (!isnuma)
	{
		return FALSE;
	}

	if (iter->num_children == 3)
	{
		gint numbr;
		gint numbc;

		// Double index
		if (!iter_is_number_matrix (iter->children[1],
		                            &valuesb,
		                            &numbr,
		                            &numbc))
		{
			g_free (valuesa);
			return FALSE;
		}
	}

	smanip = cdn_instruction_get_stack_manipulation (last->instruction,
	                                                 NULL);

	cdn_stack_manipulation_get_push_dimension (smanip,
	                                           0,
	                                           &numcr,
	                                           &numcc);

	num = numar * numac;

	if (num == 1)
	{
		ret = iter_copy (last->children[compute_index (valuesa,
		                                               valuesb,
		                                               0,
		                                               numcc)]);
	}
	else
	{
		gint *popdims;
		gint i;

		ret = iter_new_sized (NULL, num);
		popdims = g_new0 (gint, num * 2);

		// Single index, sample
		for (i = 0; i < num; ++i)
		{
			gint idx;
			CdnExpressionTreeIter *piter;

			idx = compute_index (valuesa, valuesb, i, numcc);
			piter = last->children[idx];

			iter_set_child (ret, iter_copy (piter), i);

			smanip = cdn_instruction_get_stack_manipulation (piter->instruction,
			                                                 NULL);

			cdn_stack_manipulation_get_push_dimension (smanip,
			                                           0,
			                                           popdims + i * 2,
			                                           popdims + i * 2 + 1);
		}

		ret->instruction = cdn_instruction_matrix_new (num,
		                                               popdims,
		                                               numar,
		                                               numac);
		                                               
	}

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
iter_is_matrix_multiply (CdnExpressionTreeIter *iter)
{
	CdnStackManipulation const *smanipa;
	CdnStackManipulation const *smanipb;

	if (!iter_is_multiply (iter))
	{
		return FALSE;
	}

	smanipa = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                  NULL);

	smanipb = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                  NULL);

	return smanipa->push_dims && smanipb->push_dims &&
	       smanipa->push_dims[1] != 1 &&
	       smanipb->push_dims[0] != 1 &&
	       smanipa->push_dims[1] == smanipb->push_dims[0];
}

static gboolean
simplify_element_wise (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *ret;
	CdnStackManipulation const *smanip;
	gint numr;
	gint numc;
	gint num;
	gint i;
	gint id;

	num = iter->children[0]->num_children;

	smanip = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                 NULL);

	cdn_stack_manipulation_get_push_dimension (smanip, 0, &numr, &numc);

	// Assumption is that iter is a binary operator of two matrices
	ret = iter_new_sized_take (cdn_instruction_matrix_new (num,
	                                                       NULL,
	                                                       numr,
	                                                       numc),
	                           num);

	id = cdn_instruction_function_get_id (CDN_INSTRUCTION_FUNCTION (iter->instruction));

	for (i = 0; i < num; ++i)
	{
		CdnExpressionTreeIter *i1;
		CdnExpressionTreeIter *i2;
		CdnExpressionTreeIter *it;

		i1 = iter->children[0]->children[i];
		i2 = iter->children[1]->children[i];

		it = iter_new_bfunc (id,
		                     i1,
		                     i2,
		                     TRUE,
		                     TRUE);

		if (iter_canonicalize (it, FALSE, FALSE))
		{
			iter_simplify (it, FALSE);
		}

		ret->children[i] = it;
		it->parent = ret;
	}

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
is_single_item_matrix (CdnExpressionTreeIter *iter)
{
	gint i;

	// Assumes iter is a matrix
	for (i = 0; i < iter->num_children; ++i)
	{
		CdnExpressionTreeIter *child;
		CdnStackManipulation const *smanip;

		child = iter->children[i];

		smanip = cdn_instruction_get_stack_manipulation (child->instruction,
		                                                 NULL);

		if (smanip->push_dims && (smanip->push_dims[0] != 1 || smanip->push_dims[1] != 1))
		{
			return FALSE;
		}
	}

	return TRUE;
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

	if (id == CDN_MATH_FUNCTION_TYPE_INDEX)
	{
		return simplify_index (iter);
	}

	if (cdn_math_function_is_variable (id))
	{
		return FALSE;
	}

	// Check if it's an element wise operation on a matrix. If so, we
	// inline the operation
	if (iter->num_children == 2 &&
	    iter_is_matrix (iter->children[0]) &&
	    iter_is_matrix (iter->children[1]) &&
	    is_single_item_matrix (iter->children[0]) &&
	    is_single_item_matrix (iter->children[1]) &&
	    !iter_is_matrix_multiply (iter))
	{
		return simplify_element_wise (iter);
	}

	cdn_expression_tree_iter_initialize_stack (iter, &stack);

	// Check if all arguments are numeric
	for (i = 0; i < iter->num_children; ++i)
	{
		gdouble *values;
		gint numr;
		gint numc;

		if (!iter_is_number_matrix (iter->children[i],
		                            &values,
		                            &numr,
		                            &numc))
		{
			cdn_stack_destroy (&stack);
			return FALSE;
		}

		cdn_stack_pushn (&stack, values, numr * numc);
		g_free (values);
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

	ret = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_PLUS,
	                      c1,
	                      c2,
	                      TRUE,
	                      TRUE);

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

	ret = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                      c1,
	                      c2,
	                      TRUE,
	                      TRUE);

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

	power = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_POWER,
	                        t1,
	                        plus,
	                        TRUE,
	                        TRUE);

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
is_eye (gdouble const *values,
        gint           numr,
        gint           numc)
{
	gint i;

	if (numr != numc)
	{
		return FALSE;
	}

	for (i = 0; i < numr * numc; ++i)
	{
		gboolean isdiag;

		isdiag = (i % numc) == (i / numc);

		if ((isdiag && !cmp_double (values[i], 1)) ||
		    (!isdiag && !cmp_double (values[i], 0)))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
simplify_inline_matrix_multiply (CdnExpressionTreeIter *iter)
{
	CdnStackManipulation const *smanipa;
	CdnStackManipulation const *smanipb;
	gint num1r;
	gint num1c;
	gint num2r;
	gint num2c;
	gint num;
	gint r;
	gint idx;
	CdnExpressionTreeIter *ret;

	if (!iter_is_matrix_multiply (iter) ||
	    !iter_is_matrix (iter->children[0]) ||
	    !iter_is_matrix (iter->children[1]) ||
	    !is_single_item_matrix (iter->children[0]) ||
	    !is_single_item_matrix (iter->children[1]))
	{
		return FALSE;
	}

	// For now, we are simply going to try to inline the matrix
	// multiplication (using naive multiplication) and check if the
	// result is reasonably simplified
	smanipa = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                  NULL);

	smanipb = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                  NULL);

	cdn_stack_manipulation_get_push_dimension (smanipa, 0, &num1r, &num1c);
	cdn_stack_manipulation_get_push_dimension (smanipb, 0, &num2r, &num2c);

	num = num1r * num2c;

	ret = iter_new_sized_take (cdn_instruction_matrix_new (num,
	                                                       NULL,
	                                                       num1r,
	                                                       num2c),
	                           num);

	idx = 0;

	for (r = 0; r < num1r; ++r)
	{
		gint c;

		for (c = 0; c < num2c; ++c)
		{
			gint j;
			CdnExpressionTreeIter *root = NULL;
			CdnExpressionTreeIter *plus;

			if (num1c == 1)
			{
				CdnExpressionTreeIter *prod;

				prod = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
				                       iter->children[0]->children[idx],
				                       iter->children[1]->children[idx],
				                       TRUE,
				                       TRUE);

				iter_set_child (ret, prod, idx);

				++idx;
				continue;
			}

			for (j = 0; j < num1c; ++j)
			{
				CdnExpressionTreeIter *prod;

				prod = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
				                       iter->children[0]->children[r * num1c + j],
				                       iter->children[1]->children[j * num2c + c],
				                       FALSE,
				                       FALSE);

				if (iter_canonicalize (prod, FALSE, FALSE))
				{
					iter_simplify (prod, FALSE);
				}
				else
				{
					simplify_multiply (prod);
				}

				if (j == num1c - 1)
				{
					iter_set_child (plus, prod, 1);
				}
				else
				{
					CdnExpressionTreeIter *np;

					np = iter_new_sized_take (cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
					                                                        NULL,
					                                                        2,
					                                                        NULL),
					                          2);

					iter_set_child (np, prod, 0);

					if (root == NULL)
					{
						root = np;
						plus = root;
					}
					else
					{
						iter_set_child (plus, np, 1);
						plus = np;
					}
				}
			}

			if (iter_canonicalize (root, TRUE, TRUE))
			{
				iter_simplify (root, TRUE);
			}
			else
			{
				simplify_plus (root);
			}

			iter_set_child (ret, root, idx);
			++idx;
		}
	}

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
simplify_multiply (CdnExpressionTreeIter *iter)
{
	gdouble *num1 = NULL;
	gint num1r;
	gint num1c;
	gboolean isnum1;
	gboolean rhsism;
	gboolean iscomm;
	CdnStackManipulation const *smanip;
	gint argdim[4];
	gint outargdim[2];
	gint extraspace;

	if (simplify_function (iter))
	{
		return TRUE;
	}

	if (multiply_make_powers (iter))
	{
		return TRUE;
	}

	if (simplify_inline_matrix_multiply (iter))
	{
		return TRUE;
	}

	isnum1 = iter_is_number_matrix (iter->children[0],
	                                &num1,
	                                &num1r,
	                                &num1c);

	if (!isnum1)
	{
		return FALSE;
	}

	smanip = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                 NULL);

	cdn_stack_manipulation_get_push_dimension (smanip,
	                                           0,
	                                           argdim,
	                                           argdim + 1);

	argdim[2] = num1r;
	argdim[3] = num1c;

	iscomm = cdn_math_function_is_commutative (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                           2,
	                                           argdim);

	cdn_math_function_get_stack_manipulation (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                          2,
	                                          argdim,
	                                          outargdim,
	                                          &extraspace,
	                                          NULL);

	if (iscomm)
	{
		gdouble *num2 = NULL;
		gint num2r;
		gint num2c;
		gboolean isnum2;

		rhsism = iter_is_multiply (iter->children[1]);

		isnum2 = rhsism &&
		         iter_is_number_matrix (iter->children[1]->children[0],
		                                &num2,
		                                &num2r,
		                                &num2c);

		if (isnum2)
		{
			CdnInstructionNumber *nl;
			CdnStack stack = {0,};

			// Here we apply: n1 * n2 * X = n3 * X (with n3 = n1 * n2)
			// i.e. preadding to numeric constants
			cdn_stack_init (&stack,
			                num1r * num1c +
			                num2r * num2c +
			                extraspace);

			cdn_stack_pushn (&stack, num1, num1r * num1c);
			cdn_stack_pushn (&stack, num2, num2r * num2c);

			cdn_math_function_execute (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
			                           2,
			                           argdim,
			                           &stack);

			// Check for single dimensional case which is easy
			if (outargdim[0] == 1 && outargdim[1] == 1 &&
			    num1r == 1 && num1c == 1)
			{
				nl = (CdnInstructionNumber *)(iter->children[0]->instruction);

				cdn_instruction_number_set_value (nl, cdn_stack_pop (&stack));
			}
			else
			{
				gint *popdims;

				popdims = g_new (gint, 4);

				popdims[0] = num2r;
				popdims[1] = num2c;

				popdims[2] = num1r;
				popdims[3] = num1c;

				cdn_mini_object_unref (iter->children[0]->instruction);

				// Create a new matrix instruction
				iter->children[0]->instruction =
					cdn_instruction_matrix_new (2,
					                            popdims,
					                            outargdim[0],
					                            outargdim[1]);
			}

			// Replace the multiply operator on the right, with the right hand
			// side of the operator
			iter_replace_into (iter->children[1]->children[1],
			                   iter->children[1]);

			cdn_stack_destroy (&stack);

			iter_canonicalize (iter, FALSE, FALSE);

			// Recurse
			simplify_multiply (iter);

			g_free (num1);
			g_free (num2);
			return TRUE;
		}

		g_free (num2);
	}

	if (cmp_double_all (num1, num1r * num1c, 0))
	{
		// See if we can simply bubble op the 0, or if we need to
		// change the dimensionality
		if (outargdim[0] == num1r && outargdim[1] == num1c)
		{
			iter_replace_into (iter->children[0], iter);
		}
		else
		{
			CdnExpressionTreeIter *zero;
			CdnExpressionTreeIter *mat;
			gint i;
			gint num;

			zero = iter_new_numstr ("0");

			num = outargdim[0] * outargdim[1];
			mat = iter_new_sized_take (cdn_instruction_matrix_new (num,
			                                                       NULL,
			                                                       outargdim[0],
			                                                       outargdim[1]),
			                           num);

			// Create new matrix of zeros
			for (i = 0; i < num; ++i)
			{
				mat->children[i] = i == 0 ? zero : iter_copy (zero);
			}

			iter_replace_into (mat, iter);
		}
	}
	else if (is_eye (num1, num1r, num1c))
	{
		// Replace with rhs
		iter_replace_into (iter->children[1], iter);
	}

	g_free (num1);
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

/**
 * cdn_expression_tree_iter_simplify:
 * @iter: a #CdnExpressionTreeIter
 * 
 * Simplify an expresion. Note that the simplification is done in place, i.e.
 * the same @iter passed to the function is also returned.
 *
 * Returns: (transfer none): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_simplify (CdnExpressionTreeIter *iter)
{
	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Simplify non canonical: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

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
