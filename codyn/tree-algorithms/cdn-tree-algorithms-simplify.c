#include "codyn/cdn-expression-tree-iter.h"
#include "cdn-tree-algorithms-private.h"
#include "codyn/cdn-stack-private.h"
#include <codyn/cdn-debug.h>

static gboolean iter_simplify (CdnExpressionTreeIter *iter,
                               gboolean               simplify_children);

static gboolean simplify_power (CdnExpressionTreeIter *iter);
static gboolean simplify_plus (CdnExpressionTreeIter *iter);
static gboolean simplify_multiply (CdnExpressionTreeIter *iter);
static gboolean simplify_inline_matrix (CdnExpressionTreeIter *iter);

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
                       CdnDimension           *dim)
{
	gdouble num;

	if (iter_is_number (iter, &num))
	{
		dim->rows = 1;
		dim->columns = 1;

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

		*dim = smanip->push.dimension;

		*ret = g_new (gdouble, cdn_dimension_size (dim));

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

		if (smanip->push.rows != 1 || smanip->push.columns != 1)
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
	CdnDimension dim1;
	CdnDimension dim2;
	gint num;
	gint r;
	gint idx;
	CdnExpressionTreeIter *ret;
	gboolean waschanged = FALSE;
	CdnStackArgs args;

	if (!iter_is_matrix_multiply (iter))
	{
		return FALSE;
	}

	if (!iter_is_matrix (iter->children[0]))
	{
		if (simplify_inline_matrix (iter->children[0]))
		{
			waschanged = TRUE;
		}

		if (!iter_is_matrix (iter->children[0]))
		{
			return waschanged;
		}
	}

	if (!iter_is_matrix (iter->children[1]))
	{
		if (simplify_inline_matrix (iter->children[1]))
		{
			waschanged = TRUE;
		}

		if (!iter_is_matrix (iter->children[1]))
		{
			return waschanged;
		}
	}

	if (!is_single_item_matrix (iter->children[0]) ||
	    !is_single_item_matrix (iter->children[1]))
	{
		return waschanged;
	}

	// For now, we are simply going to try to inline the matrix
	// multiplication (using naive multiplication) and check if the
	// result is reasonably simplified
	smanipa = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                  NULL);

	smanipb = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                  NULL);

	dim1 = smanipa->push.dimension;
	dim2 = smanipb->push.dimension;

	num = dim1.rows * dim2.columns;
	ret = iter_new_sized_take (NULL, num);

	idx = 0;

	for (r = 0; r < dim1.rows; ++r)
	{
		gint c;

		for (c = 0; c < dim2.columns; ++c)
		{
			gint j;
			CdnExpressionTreeIter *root = NULL;
			CdnExpressionTreeIter *plus = NULL;

			if (dim1.columns == 1)
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

			for (j = 0; j < dim1.columns; ++j)
			{
				CdnExpressionTreeIter *prod;

				prod = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
				                       iter->children[0]->children[j * dim1.rows + r],
				                       iter->children[1]->children[c * dim2.rows + j],
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

				if (j == dim1.columns - 1)
				{
					iter_set_child (plus, prod, 1);
				}
				else
				{
					CdnExpressionTreeIter *np;

					np = iter_new_sized_take (NULL, 2);

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

			iter_fill_bfunc (root, CDN_MATH_FUNCTION_TYPE_PLUS);

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

	iter_get_stack_args (ret, &args);

	dim1.columns = dim2.columns;

	ret->instruction = cdn_instruction_matrix_new (&args, &dim1);
	cdn_stack_args_destroy (&args);

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
variable_has_actors (CdnVariable *v)
{
	GSList *actions;
	gboolean ret;

	actions = cdn_variable_get_actions (v);
	ret = actions != NULL;
	g_slist_free (actions);

	return ret;
}

static gboolean
simplify_inline_matrix (CdnExpressionTreeIter *iter)
{
	gint i;
	CdnMathFunctionType fid;
	gboolean waschanged = FALSE;
	CdnExpressionTreeIter *mat = NULL;

	if (CDN_IS_INSTRUCTION_VARIABLE (iter->instruction))
	{
		// Check if we can inline the variable
		CdnVariable *v;
		CdnVariableFlags flags;
		CdnExpressionTreeIter *expanded;
		CdnInstructionVariable *vinstr;

		cdn_debug_message (DEBUG_SIMPLIFY,
		                   "Inline matrix var: {%s}",
		                   cdn_expression_tree_iter_to_string (iter));

		vinstr = CDN_INSTRUCTION_VARIABLE (iter->instruction);

		v = cdn_instruction_variable_get_variable (vinstr);
		flags = cdn_variable_get_flags (v);

		if ((flags & (CDN_VARIABLE_FLAG_INTEGRATED | CDN_VARIABLE_FLAG_IN)) ||
		    variable_has_actors (v) ||
		    cdn_instruction_variable_get_slice (vinstr, NULL, NULL) != NULL)
		{
			return FALSE;
		}

		expanded = cdn_expression_tree_iter_new (cdn_variable_get_expression (v));
		simplify_inline_matrix (expanded);

		cdn_debug_message (DEBUG_SIMPLIFY,
		                   "expanded: {%s}",
		                   cdn_expression_tree_iter_to_string (expanded));

		if (CDN_IS_INSTRUCTION_MATRIX (expanded->instruction))
		{
			iter_replace_into (expanded, iter);
			return TRUE;
		}
		else
		{
			return FALSE;
		}
	}

	if (!iter_is_function (iter, &fid) ||
	    iter->num_children == 0 ||
	    iter->num_children > 2)
	{
		return FALSE;
	}

	switch (fid)
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
		case CDN_MATH_FUNCTION_TYPE_SIN:
		case CDN_MATH_FUNCTION_TYPE_COS:
		case CDN_MATH_FUNCTION_TYPE_TAN:
		case CDN_MATH_FUNCTION_TYPE_ASIN:
		case CDN_MATH_FUNCTION_TYPE_ACOS:
		case CDN_MATH_FUNCTION_TYPE_ATAN:
		case CDN_MATH_FUNCTION_TYPE_SQRT:
		case CDN_MATH_FUNCTION_TYPE_INVSQRT:
		case CDN_MATH_FUNCTION_TYPE_EXP:
		case CDN_MATH_FUNCTION_TYPE_FLOOR:
		case CDN_MATH_FUNCTION_TYPE_CEIL:
		case CDN_MATH_FUNCTION_TYPE_ROUND:
		case CDN_MATH_FUNCTION_TYPE_ABS:
		case CDN_MATH_FUNCTION_TYPE_LN:
		case CDN_MATH_FUNCTION_TYPE_LOG10:
		case CDN_MATH_FUNCTION_TYPE_EXP2:
		case CDN_MATH_FUNCTION_TYPE_SINH:
		case CDN_MATH_FUNCTION_TYPE_COSH:
		case CDN_MATH_FUNCTION_TYPE_TANH:
		case CDN_MATH_FUNCTION_TYPE_SIGN:
		case CDN_MATH_FUNCTION_TYPE_CSIGN:
		case CDN_MATH_FUNCTION_TYPE_MINUS:
		case CDN_MATH_FUNCTION_TYPE_PLUS:
		case CDN_MATH_FUNCTION_TYPE_EMULTIPLY:
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
		case CDN_MATH_FUNCTION_TYPE_MODULO:
		case CDN_MATH_FUNCTION_TYPE_POWER:
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_NEQUAL:
		case CDN_MATH_FUNCTION_TYPE_OR:
		case CDN_MATH_FUNCTION_TYPE_AND:
		case CDN_MATH_FUNCTION_TYPE_POW:
		case CDN_MATH_FUNCTION_TYPE_HYPOT:
		case CDN_MATH_FUNCTION_TYPE_SQSUM:
		case CDN_MATH_FUNCTION_TYPE_ATAN2:
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
		break;
		default:
			return FALSE;
	}

	if (fid == CDN_MATH_FUNCTION_TYPE_MULTIPLY &&
	    iter_is_matrix_multiply (iter))
	{
		return simplify_inline_matrix_multiply (iter);
	}

	// Inline children
	for (i = 0; i < iter->num_children; ++i)
	{
		waschanged = simplify_inline_matrix (iter->children[i]);

		if (iter_is_matrix (iter->children[i]))
		{
			mat = iter->children[i];
		}
	}

	if (mat == NULL)
	{
		return waschanged;
	}

	if (iter->num_children == 1)
	{
		// Apply unary operation to each matrix element
		for (i = 0; i < mat->num_children; ++i)
		{
			CdnExpressionTreeIter *cp;

			cp = iter_new_ufunc (fid, mat->children[i], TRUE);

			if (iter_canonicalize (cp, FALSE, FALSE))
			{
				iter_simplify (cp, FALSE);
			}

			iter_set_child (mat, cp, i);
		}

		iter_replace_into (mat, iter);
	}
	else
	{
		// Binary operation, inline into the matrix (which is either
		// on the left or on the right)
		gboolean takea = FALSE;
		gboolean takeb = FALSE;
		CdnExpressionTreeIter *left = NULL;
		CdnExpressionTreeIter *right = NULL;

		if (iter->children[0] == mat)
		{
			right = iter->children[1];
			takea = TRUE;
		}
		else
		{
			left = iter->children[0];
			takeb = TRUE;
		}

		for (i = 0; i < mat->num_children; ++i)
		{
			CdnExpressionTreeIter *cp;

			if (takea)
			{
				left = mat->children[i];
			}
			else
			{
				right = mat->children[i];
			}

			cp = iter_new_bfunc (fid,
			                     left,
			                     right,
			                     takea,
			                     takeb);

			iter_canonicalize (cp, FALSE, FALSE);
			iter_simplify (cp, FALSE);

			iter_set_child (mat, cp, i);
		}

		iter_replace_into (mat, iter);
	}

	return TRUE;
}

static guint *
make_sindex_slice (gdouble      const *valuesa,
                   CdnDimension const *dima)
{
	guint n;
	guint i;
	guint *ret;

	n = cdn_dimension_size (dima);

	ret = g_new (guint, n);

	for (i = 0; i < n; ++i)
	{
		ret[i] = (guint)(valuesa[i] + 0.5);
	}

	return ret;
}

static guint *
make_mindex_slice (gdouble      const *valuesa,
                   CdnDimension const *dima,
                   gdouble      const *valuesb,
                   CdnDimension const *dimb,
                   CdnDimension const *retdim)
{
	// Compose linear indices from the row/column matrices
	guint n;
	guint i;
	guint *ret;

	n = cdn_dimension_size (dima);

	ret = g_new (guint, n);

	for (i = 0; i < n; ++i)
	{
		guint r;
		guint c;

		r = (guint)(valuesa[i] + 0.5);
		c = (guint)(valuesb[i] + 0.5);

		ret[i] = c * retdim->rows + r;
	}

	return ret;
}

static guint *
make_index_slice (gdouble      const *valuesa,
                  CdnDimension const *dima,
                  gdouble      const *valuesb,
                  CdnDimension const *dimb,
                  CdnDimension const *retdim)
{
	if (valuesb)
	{
		return make_mindex_slice (valuesa,
		                          dima,
		                          valuesb,
		                          dimb,
		                          retdim);
	}
	else
	{
		return make_sindex_slice (valuesa,
		                          dima);
	}
}

static gboolean
simplify_index (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *last;
	CdnExpressionTreeIter *ret;
	CdnStackManipulation const *smanip;
	gboolean waschanged;
	guint *lindices;
	gint nindices;
	CdnStackManipulation const *retsmanip;

	last = iter->children[iter->num_children - 1];

	retsmanip = cdn_instruction_get_stack_manipulation (iter->instruction,
	                                                    NULL);

	smanip = cdn_instruction_get_stack_manipulation (last->instruction,
	                                                 NULL);

	waschanged = simplify_inline_matrix (last);

	nindices = cdn_dimension_size (&retsmanip->push.dimension);

	if (CDN_IS_INSTRUCTION_INDEX (iter->instruction))
	{
		gint *lidx;
		gint i;

		lidx = g_new0 (gint, nindices);

		cdn_instruction_index_write_indices (CDN_INSTRUCTION_INDEX (iter->instruction),
		                                     lidx,
		                                     nindices);

		lindices = g_new0 (guint, nindices);

		for (i = 0; i < nindices; i++)
		{
			lindices[i] = (guint)lidx[i];
		}

		g_free (lidx);
	}
	else
	{
		gboolean isnuma;
		gdouble *valuesa;
		gdouble *valuesb = NULL;
		CdnDimension dima;
		CdnDimension dimb;

		// Check if first (and second) arg are numeric
		isnuma = iter_is_number_matrix (iter->children[0],
		                                &valuesa,
		                                &dima);

		if (!isnuma)
		{
			return waschanged;
		}

		if (iter->num_children == 3)
		{
			// Double index
			if (!iter_is_number_matrix (iter->children[1],
			                            &valuesb,
			                            &dimb))
			{
				g_free (valuesa);
				return waschanged;
			}
		}

		lindices = make_index_slice (valuesa,
		                             &dima,
		                             valuesb,
		                             &dimb,
		                             &smanip->push.dimension);

		g_free (valuesa);
		g_free (valuesb);
	}

	if (CDN_IS_INSTRUCTION_VARIABLE (last->instruction))
	{
		cdn_instruction_variable_apply_slice (CDN_INSTRUCTION_VARIABLE (last->instruction),
		                                      lindices,
		                                      cdn_dimension_size (&retsmanip->push.dimension),
		                                      &retsmanip->push.dimension);

		g_free (lindices);

		// Make a variable slice
		iter_replace_into (last, iter);
		return TRUE;
	}
	else if (!iter_is_matrix (last))
	{
		g_free (lindices);
		return waschanged;
	}

	if (nindices == 1)
	{
		if (lindices[0] < last->num_children)
		{
			ret = iter_copy (last->children[lindices[0]]);
		}
		else
		{
			g_free (lindices);
			return FALSE;
		}
	}
	else
	{
		CdnStackArgs args;
		gint i;

		cdn_stack_args_init (&args, nindices);
		ret = iter_new_sized (NULL, nindices);

		for (i = 0; i < nindices; ++i)
		{
			gint idx;
			CdnExpressionTreeIter *piter;

			idx = lindices[i];

			if (idx >= last->num_children)
			{
				cdn_expression_tree_iter_free (ret);
				cdn_stack_args_destroy (&args);

				g_free (lindices);

				return FALSE;
			}

			piter = last->children[idx];

			iter_set_child (ret, iter_copy (piter), i);

			smanip = cdn_instruction_get_stack_manipulation (piter->instruction,
			                                                 NULL);

			cdn_stack_arg_copy (&args.args[i], &smanip->push);
		}

		ret->instruction = cdn_instruction_matrix_new (&args,
		                                               &retsmanip->push.dimension);

		cdn_stack_args_destroy (&args);
	}

	iter_replace_into (ret, iter);
	g_free (lindices);

	return TRUE;
}

static gboolean
simplify_element_wise (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *ret;
	CdnStackManipulation const *smanip;
	gint num;
	gint i;
	gint id;
	CdnStackArgs args;

	num = iter->children[0]->num_children;

	smanip = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                 NULL);

	// Assumption is that iter is a binary operator of two matrices
	ret = iter_new_sized_take (NULL, num);

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

	iter_get_stack_args (ret, &args);
	ret->instruction = cdn_instruction_matrix_new (&args, &smanip->push.dimension);
	cdn_stack_args_destroy (&args);

	iter_replace_into (ret, iter);
	return TRUE;
}

static gboolean
simplify_function (CdnExpressionTreeIter *iter)
{
	CdnInstructionFunction *f;
	gint id;
	gint i;
	CdnStack stack;
	CdnDimension dim;
	CdnStackManipulation const *smanip;
	gint num;

	f = (CdnInstructionFunction *)(iter->instruction);
	id = cdn_instruction_function_get_id (f);

	if (id == CDN_MATH_FUNCTION_TYPE_INDEX)
	{
		return simplify_index (iter);
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

		if (!iter_is_number_matrix (iter->children[i],
		                            &values,
		                            &dim))
		{
			cdn_stack_destroy (&stack);
			return FALSE;
		}

		cdn_stack_pushn (&stack, values, cdn_dimension_size (&dim));
		g_free (values);
	}

	smanip = cdn_instruction_get_stack_manipulation (iter->instruction, NULL);
	dim = smanip->push.dimension;

	num = cdn_dimension_size (&dim);

	cdn_math_function_execute (id,
	                           &smanip->pop,
	                           &stack);

	for (i = 0; i < iter->num_children; ++i)
	{
		cdn_expression_tree_iter_free (iter->children[i]);
	}

	g_free (iter->children);
	iter->children = NULL;
	iter->num_children = 0;

	cdn_mini_object_unref (CDN_MINI_OBJECT (iter->instruction));

	if (num > 1)
	{
		CdnStackArgs args;

		iter->children = g_new0 (CdnExpressionTreeIter *, num);

		for (i = num - 1; i >= 0; --i)
		{
			iter->children[i] = iter_new_take (cdn_instruction_number_new (cdn_stack_pop (&stack)));
			iter->children[i]->parent = iter;
		}

		iter->num_children = num;

		iter_get_stack_args (iter, &args);
		iter->instruction = cdn_instruction_matrix_new (&args, &dim);
		cdn_stack_args_destroy (&args);
	}
	else
	{
		iter->instruction = cdn_instruction_number_new (cdn_stack_pop (&stack));
	}

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
is_eye (gdouble const      *values,
        CdnDimension const *dim)
{
	gint i;
	gint num;

	if (dim->rows != dim->columns)
	{
		return FALSE;
	}

	num = cdn_dimension_size (dim);

	for (i = 0; i < num; ++i)
	{
		gboolean isdiag;

		isdiag = (i % dim->columns) == (i / dim->columns);

		if ((isdiag && !cmp_double (values[i], 1)) ||
		    (!isdiag && !cmp_double (values[i], 0)))
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
simplify_multiply (CdnExpressionTreeIter *iter)
{
	gdouble *num1 = NULL;
	CdnDimension dim1;
	gboolean isnum1;
	gboolean rhsism;
	gboolean iscomm;
	CdnStackManipulation const *smanip;
	CdnStackArgs args;
	CdnStackArg nargs[2] = {CDN_STACK_ARG_EMPTY, CDN_STACK_ARG_EMPTY};
	CdnStackArg outarg = CDN_STACK_ARG_EMPTY;
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

	args.num = 2;
	args.args = nargs;

	isnum1 = iter_is_number_matrix (iter->children[0],
	                                &num1,
	                                &dim1);

	if (!isnum1)
	{
		return FALSE;
	}

	smanip = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                 NULL);

	args.args[0].dimension = smanip->push.dimension;
	args.args[1].dimension = dim1;

	iscomm = cdn_math_function_is_commutative (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                           &args);

	cdn_math_function_get_stack_manipulation (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                          &args,
	                                          &outarg,
	                                          &extraspace,
	                                          NULL);

	if (iscomm)
	{
		gdouble *num2 = NULL;
		CdnDimension dim2;
		gboolean isnum2;

		rhsism = iter_is_multiply (iter->children[1]);

		isnum2 = rhsism &&
		         iter_is_number_matrix (iter->children[1]->children[0],
		                                &num2,
		                                &dim2);

		if (isnum2)
		{
			CdnInstructionNumber *nl;
			CdnStack stack = {0,};

			// Here we apply: n1 * n2 * X = n3 * X (with n3 = n1 * n2)
			// i.e. preadding to numeric constants
			cdn_stack_init (&stack,
			                cdn_dimension_size (&dim1) +
			                cdn_dimension_size (&dim2) +
			                extraspace);

			cdn_stack_pushn (&stack, num1, cdn_dimension_size (&dim1));
			cdn_stack_pushn (&stack, num2, cdn_dimension_size (&dim2));

			args.args[0].dimension = dim2;

			cdn_math_function_execute (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
			                           &args,
			                           &stack);

			// Check for single dimensional case which is easy
			if (cdn_dimension_is_one (&outarg.dimension) &&
			    cdn_dimension_is_one (&dim1))
			{
				if (CDN_IS_INSTRUCTION_NUMBER (iter->children[0]->instruction))
				{
					nl = (CdnInstructionNumber *)(iter->children[0]->instruction);
					cdn_instruction_number_set_value (nl, cdn_stack_pop (&stack));
				}
				else
				{
					iter_replace_into (iter_new_num (cdn_stack_pop (&stack)),
					                   iter->children[0]);
				}
			}
			else
			{
				// Create new numeric matrix
				CdnExpressionTreeIter *nmat;

				nmat = iter_new_number_matrix (cdn_stack_output_ptr (&stack),
				                               &outarg.dimension);

				iter_replace_into (nmat, iter->children[0]);
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

	if (cmp_double_all (num1, cdn_dimension_size (&dim1), 0))
	{
		// See if we can simply bubble op the 0, or if we need to
		// change the dimensionality
		if (cdn_dimension_equal (&outarg.dimension, &dim1))
		{
			iter_replace_into (iter->children[0], iter);
		}
		else
		{
			// Make zero matrix
			iter_replace_into (iter_new_number_matrix (NULL, &outarg.dimension),
			                   iter);
		}
	}
	else if (is_eye (num1, &dim1))
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
make_rest (CdnExpressionTreeIter *left)
{
	CdnExpressionTreeIter *lrest = NULL;

	if (left)
	{
		CdnExpressionTreeIter *parent;

		lrest = left;
		parent = left->parent;

		iter_replace (left, NULL);

		iter_replace_into (parent->children[0],
		                   parent);
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
	    iter_is_multiply (left->children[0]) &&
	    iter_is_number (left->children[0], NULL))
	{
		lnum = left->children[0];
		left = lstart = left->children[1];
	}

	if (right->num_children &&
	    iter_is_multiply (right->children[0]) &&
	    iter_is_number (right->children[0], NULL))
	{
		rnum = right->children[0];
		right = rstart = right->children[1];
	}

	// Walk down the left/right multiplier trees as long as
	// the LHS is equal
	while (TRUE)
	{
		if (iter_is_multiply (left) &&
		    iter_is_multiply (right) &&
		    cdn_expression_tree_iter_equal (left->children[0], right->children[0], FALSE))
		{
			left = left->children[1];
			right = right->children[1];
		}
		else
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
	lrest = make_rest (left);
	rrest = make_rest (right);

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

static void
swap_dim (CdnDimension *a,
          CdnDimension *b)
{
	CdnDimension tmp = *a;

	*a = *b;
	*b = tmp;
}

static void
swap_dblptr (gdouble **a,
             gdouble **b)
{
	gdouble *tmp = *a;

	*a = *b;
	*b = tmp;
}

static void
swaplr (gdouble      **lnums,
        CdnDimension  *ldim,
        gdouble      **rnums,
        CdnDimension  *rdim)
{
	gint lnum;
	gint rnum;

	lnum = cdn_dimension_size (ldim);
	rnum = cdn_dimension_size (rdim);

	if (lnum < rnum)
	{
		swap_dblptr (lnums, rnums);
		swap_dim (ldim, rdim);
	}
}

static gboolean
simplify_plus (CdnExpressionTreeIter *iter)
{
	gboolean islnum;
	gdouble *lrnums;
	CdnDimension ldim;
	gdouble *rrnums = NULL;
	CdnDimension rdim;

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
	islnum = iter_is_number_matrix (iter->children[0],
	                                &lrnums,
	                                &ldim);

	if (!islnum)
	{
		g_free (lrnums);
		return FALSE;
	}

	if (iter_is_plus (iter->children[1]) &&
	    iter_is_number_matrix (iter->children[1]->children[0],
	                           &rrnums,
	                           &rdim))
	{
		CdnInstructionNumber *nl;
		gint lnum;
		gint rnum;

		swaplr (&lrnums, &ldim,
		        &rrnums, &rdim);

		lnum = cdn_dimension_size (&ldim);
		rnum = cdn_dimension_size (&rdim);

		// Here we apply: n1 + n2 + X = n3 + X
		// i.e. preadding to numeric constants
		if (lnum == 1 && rnum == 1)
		{
			if (CDN_IS_INSTRUCTION_NUMBER (iter->children[0]->instruction))
			{
				nl = (CdnInstructionNumber *)(iter->children[0]->instruction);

				cdn_instruction_number_set_value (nl,
				                                  lrnums[0] + rrnums[0]);
			}
			else
			{
				iter_replace_into (iter_new_num (lrnums[0] + rrnums[0]),
				                   iter->children[0]);
			}

			// Replace the plus operator on the right, with the right hand
			// side of the operator
			iter_replace_into (iter->children[1]->children[1],
			                   iter->children[1]);
		}
		else
		{
			gint i;

			for (i = 0; i < lnum; ++i)
			{
				lrnums[i] += rrnums[0];
			}

			iter_replace_into (iter_new_number_matrix (lrnums,
			                                           &ldim),
			                   iter->children[0]);

			iter_replace_into (iter->children[1]->children[1],
			                   iter->children[1]);
		}

		iter_canonicalize (iter->children[1], FALSE, FALSE);
	}
	else if (cmp_double_all (lrnums, cdn_dimension_size (&ldim), 0))
	{
		iter_replace_into (iter->children[1], iter);
		iter_canonicalize (iter, FALSE, FALSE);
	}
	else
	{
		g_free (lrnums);
		g_free (rrnums);

		return FALSE;
	}

	g_free (lrnums);
	g_free (rrnums);
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
simplify_matrix (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter **children = NULL;
	CdnStackManipulation const *smanip;
	CdnDimension dim;
	gint num;
	gint childptr = 0;
	gint i;
	CdnStackArgs args;

	smanip = cdn_instruction_get_stack_manipulation (iter->instruction,
	                                                 NULL);

	dim = smanip->push.dimension;
	num = cdn_dimension_size (&dim);

	if (smanip->pop.num == num)
	{
		return FALSE;
	}

	// Expand concatenated matrix elements into single elements if possible
	for (i = 0; i < iter->num_children; ++i)
	{
		CdnExpressionTreeIter *child;
		gint j;

		child = iter->children[i];

		if (!iter_is_matrix (child))
		{
			if (children)
			{
				children[childptr] = child;
				iter->children[i] = NULL;

				++childptr;
			}

			continue;
		}

		if (!children)
		{
			children = g_new (CdnExpressionTreeIter *, num);

			while (childptr < i)
			{
				children[childptr] = iter->children[childptr];
				iter->children[childptr] = NULL;

				++childptr;
			}
		}

		// Append expansion
		for (j = 0; j < child->num_children; ++j)
		{
			children[childptr] = child->children[j];
			child->children[j] = NULL;

			children[childptr]->parent = iter;

			++childptr;
		}

		iter_set_child (iter, NULL, i);
	}

	if (!children)
	{
		return FALSE;
	}

	g_free (iter->children);

	iter->children = children;
	iter->num_children = childptr;

	cdn_mini_object_unref (iter->instruction);

	iter_get_stack_args (iter, &args);
	iter->instruction = cdn_instruction_matrix_new (&args, &dim);
	cdn_stack_args_destroy (&args);

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
	else if (iter_is_matrix (iter))
	{
		if (simplify_matrix (iter))
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
	else if (CDN_IS_INSTRUCTION_INDEX (iter->instruction))
	{
		if (simplify_index (iter))
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
	                   "Simplifying: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	cdn_debug_push_indent ();
	cdn_expression_tree_iter_canonicalize (iter);
	cdn_debug_pop_indent ();

	iter_simplify (iter, TRUE);
	iter_invalidate_cache_down (iter);

	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Simplified:  {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	return iter;
}
