#include "cdn-tree-algorithms-private.h"
#include <codyn/instructions/cdn-instructions.h>
#include "codyn/cdn-operators.h"
#include <codyn/cdn-debug.h>

static gboolean canonical_multiply (CdnExpressionTreeIter *iter,
                                    gboolean               dodefactor);

static CdnInstruction *
create_bin (CdnStackArg const   *arg1,
            CdnStackArg const   *arg2,
            CdnMathFunctionType  tp)
{
	CdnStackArgs args;
	CdnInstruction *ret;

	cdn_stack_args_init (&args, 2);

	// Note: reverse order of args
	cdn_stack_arg_copy (&args.args[0], arg2);
	cdn_stack_arg_copy (&args.args[1], arg1);

	ret = cdn_instruction_function_new (tp, NULL, &args);

	cdn_stack_args_destroy (&args);

	return ret;
}

static CdnInstruction *
create_multiply (CdnStackArg const *arg1,
                 CdnStackArg const *arg2)
{
	return create_bin (arg1, arg2, CDN_MATH_FUNCTION_TYPE_MULTIPLY);
}

static CdnInstruction *
create_plus (CdnStackArg const *arg1,
             CdnStackArg const *arg2)
{
	return create_bin (arg1, arg2, CDN_MATH_FUNCTION_TYPE_PLUS);
}

static gint
function_id_cos_sin (gint     id,
                     gboolean siniscos)
{
	switch (id)
	{
		case CDN_MATH_FUNCTION_TYPE_COS:
			return CDN_MATH_FUNCTION_TYPE_NUM + 1;
		break;
		case CDN_MATH_FUNCTION_TYPE_SIN:
			if (siniscos)
			{
				return CDN_MATH_FUNCTION_TYPE_NUM + 1;
			}
			else
			{
				return CDN_MATH_FUNCTION_TYPE_NUM + 2;
			}
		break;
	}

	return id;
}

static gint
compare_function (CdnExpressionTreeIter const *iter1,
                  CdnExpressionTreeIter const *iter2,
                  gboolean                     siniscos)
{
	gint i1;
	gint i2;
	CdnInstructionFunction *func1;
	CdnInstructionFunction *func2;

	func1 = (CdnInstructionFunction *)(iter1->instruction);
	func2 = (CdnInstructionFunction *)(iter2->instruction);

	i1 = cdn_instruction_function_get_id (func1);
	i2 = cdn_instruction_function_get_id (func2);

	if (iter_is_function (iter1, NULL))
	{
		// Make sure to sort sin and cos last
		i1 = function_id_cos_sin (i1, siniscos);
		i2 = function_id_cos_sin (i2, siniscos);
	}

	return i1 > i2 ? -1 : (i1 < i2 ? 1 : 0);
}

static gint
compare_number (CdnExpressionTreeIter const *iter1,
                CdnExpressionTreeIter const *iter2)
{
	gdouble n1;
	gdouble n2;
	CdnInstructionNumber *num1;
	CdnInstructionNumber *num2;

	num1 = (CdnInstructionNumber *)(iter1->instruction);
	num2 = (CdnInstructionNumber *)(iter2->instruction);

	n1 = cdn_instruction_number_get_value (num1);
	n2 = cdn_instruction_number_get_value (num2);

	return fabs(n1 - n2) < 10e-9 ? 0 : (n1 > n2 ? 1 : -1);
}

static gint
compare_property (CdnExpressionTreeIter const *iter1,
                  CdnExpressionTreeIter const *iter2)
{
	CdnVariable *p1;
	CdnVariable *p2;
	CdnObject *o1;
	CdnObject *o2;
	CdnInstructionVariable *prop1;
	CdnInstructionVariable *prop2;

	prop1 = (CdnInstructionVariable *)(iter1->instruction);
	prop2 = (CdnInstructionVariable *)(iter2->instruction);

	p1 = cdn_instruction_variable_get_variable (prop1);
	p2 = cdn_instruction_variable_get_variable (prop2);

	o1 = cdn_variable_get_object (p1);
	o2 = cdn_variable_get_object (p2);

	if (o1 != o2)
	{
		return o1 > o2 ? -1 : 1;
	}

	return g_strcmp0 (cdn_variable_get_name (p1),
	                  cdn_variable_get_name (p2));
}

typedef struct
{
	GType type;
	gint id;
} TypeId;

static gint
type_id (CdnInstruction *instr)
{
	TypeId order[] = {
		{CDN_TYPE_INSTRUCTION_NUMBER, 0},
		{CDN_TYPE_INSTRUCTION_VARIABLE, 1},
		{CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, 2},
		{CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, 3},
		{CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, 4},
		{CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, 4},
		{CDN_TYPE_INSTRUCTION_FUNCTION, 6},
		{G_TYPE_INVALID, -1}
	};

	gint i = 0;
	GType t = G_OBJECT_TYPE (instr);

	while (order[i].type != G_TYPE_INVALID)
	{
		if (t == order[i].type)
		{
			return order[i].id;
		}

		++i;
	}

	return -1;
}

static gint
compare_iters (CdnExpressionTreeIter const *iter1,
               CdnExpressionTreeIter const *iter2,
               gboolean                     siniscos)
{
	gint g1;
	gint g2;
	GType type;

	// Sort on instruction type
	g1 = type_id (iter1->instruction);
	g2 = type_id (iter2->instruction);

	if (g1 != g2)
	{
		return g1 < g2 ? -1 : 1;
	}

	type = G_OBJECT_TYPE (iter1->instruction);

	if (type == CDN_TYPE_INSTRUCTION_FUNCTION)
	{
		gint ret;

		ret = compare_function (iter1, iter2, siniscos);

		if (ret == 0)
		{
			gint i;

			// Compare children left to right
			for (i = iter1->num_children - 1; i >= 0; --i)
			{
				ret = compare_iters (iter1->children[i],
				                     iter2->children[i],
				                     siniscos);

				if (ret != 0)
				{
					return ret;
				}
			}
		}

		return ret;
	}
	else if (type == CDN_TYPE_INSTRUCTION_VARIABLE)
	{
		return compare_property (iter1, iter2);
	}
	else if (type == CDN_TYPE_INSTRUCTION_NUMBER)
	{
		return compare_number (iter1, iter2);
	}

	return 0;
}

static GSList *
collect_plus_terms (CdnExpressionTreeIter *iter,
                    GSList                *ret)
{
	if (!iter_is_plus (iter))
	{
		ret = g_slist_prepend (ret, iter);
	}
	else
	{
		ret = collect_plus_terms (iter->children[0],
		                          collect_plus_terms (iter->children[1],
		                                              ret));
	}

	return ret;
}

static CdnExpressionTreeIter *
terminal_part (CdnExpressionTreeIter   *iter,
               CdnMathFunctionType  type)
{
	CdnMathFunctionType tp;

	if (iter_is_function (iter, &tp) && type == tp)
	{
		return iter->children[0];
	}
	else
	{
		return iter;
	}
}

static gboolean
merge_trees_advance (CdnExpressionTreeIter **root,
                     CdnExpressionTreeIter **ptr,
                     CdnExpressionTreeIter *otherptr,
                     CdnExpressionTreeIter  *tleft,
                     CdnExpressionTreeIter  *tright)
{
	iter_set_child (*root, tleft, 0);

	if (tleft == *ptr)
	{
		iter_set_child (*root, otherptr, 1);
		return FALSE;
	}
	else
	{
		CdnExpressionTreeIter *nl;

		nl = (*ptr)->children[1];

		iter_set_child (*ptr, NULL, 1);
		iter_set_child (*root, *ptr, 1);

		*root = *ptr;
		*ptr = nl;
		return TRUE;
	}
}

static gboolean
merge_commutative_operator_trees (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *root;
	CdnExpressionTreeIter *left;
	CdnExpressionTreeIter *right;
	CdnMathFunctionType type;
	CdnInstructionFunction *instr;

	instr = (CdnInstructionFunction *)iter->instruction;
	type = cdn_instruction_function_get_id (instr);

	root = iter;

	left = iter->children[0];
	right = iter->children[1];

	// Precheck if we need to sort this
	if (terminal_part (left, type) == left)
	{
		if (terminal_part (right, type) == right)
		{
			if (compare_iters (left,
			                   right,
			                   type == CDN_MATH_FUNCTION_TYPE_PLUS) <= 0)
			{
				return FALSE;
			}
		}
		else if (compare_iters (left,
		                        right->children[0],
		                        type == CDN_MATH_FUNCTION_TYPE_PLUS) <= 0)
		{
			return FALSE;
		}
	}

	iter_set_child (iter, NULL, 0);
	iter_set_child (iter, NULL, 1);

	while (TRUE)
	{
		CdnExpressionTreeIter *tleft;
		CdnExpressionTreeIter *tright;

		tleft = terminal_part (left, type);
		tright = terminal_part (right, type);

		if (compare_iters (tleft,
		                   tright,
		                   type == CDN_MATH_FUNCTION_TYPE_PLUS) <= 0)
		{
			if (!merge_trees_advance (&root,
			                          &left,
			                          right,
			                          tleft,
			                          tright))
			{
				break;
			}
		}
		else
		{
			if (!merge_trees_advance (&root,
			                          &right,
			                          left,
			                          tright,
			                          tleft))
			{
				break;
			}
		}
	}

	return TRUE;
}

static gboolean
canonical_plus (CdnExpressionTreeIter *iter)
{
	return merge_commutative_operator_trees (iter);
}

static gboolean
defactorize (CdnExpressionTreeIter *iter)
{
	gboolean i1;
	gboolean i2;
	GSList *l1;
	GSList *l2;
	CdnExpressionTreeIter *plus;

	i1 = iter_is_plus (iter->children[0]);
	i2 = iter_is_plus (iter->children[1]);

	if (!(i1 || i2))
	{
		return FALSE;
	}

	// Collect all the terms in left and right that need to be multiplied
	l1 = collect_plus_terms (iter->children[0], NULL);
	l2 = collect_plus_terms (iter->children[1], NULL);

	iter_set_child (iter, NULL, 0);
	iter_set_child (iter, NULL, 1);

	plus = NULL;

	// For each combination of elements in l1 and l2, create a
	// multiplication tree and add the term to the new plus tree in iter
	while (l1)
	{
		GSList *item;

		for (item = l2; item; item = g_slist_next (item))
		{
			CdnExpressionTreeIter *mult;

			mult = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
			                       l1->data,
			                       item->data,
			                       !item->next,
			                       !l1->next);

			// Make sure the new 'mult' is properly canonicalized
			canonical_multiply (mult, TRUE);

			if (!plus)
			{
				plus = mult;
			}
			else
			{
				plus = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_PLUS,
				                       mult,
				                       plus,
				                       TRUE,
				                       TRUE);

				canonical_plus (plus);
			}
		}

		l1 = g_slist_delete_link (l1, l1);
	}

	g_slist_free (l2);
	iter_replace_into (plus, iter);

	return TRUE;
}

static gboolean
canonical_multiply (CdnExpressionTreeIter *iter,
                    gboolean               dodefactor)
{
	if (iter_is_matrix_multiply (iter))
	{
		return FALSE;
	}

	if (!dodefactor || !defactorize (iter))
	{
		return merge_commutative_operator_trees (iter);
	}
	else if (dodefactor)
	{
		return TRUE;
	}

	return FALSE;
}

static gboolean
canonical_unary_minus (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *one;
	CdnExpressionTreeIter *child;
	CdnStackManipulation const *smanip;
	CdnStackArg arg1 = CDN_STACK_ARG (1, 1);

	// -(term) => -1 * term
	cdn_mini_object_unref (CDN_MINI_OBJECT (iter->instruction));

	child = iter->children[0];
	smanip = cdn_instruction_get_stack_manipulation (child->instruction,
	                                                 NULL);

	g_free (iter->children);
	iter->num_children = 2;
	iter->children = g_new0 (CdnExpressionTreeIter *, 2);

	iter->instruction = create_multiply (&arg1,
	                                     &smanip->push);

	one = iter_new_num (-1);

	iter_set_child (iter, one, 0);
	iter_set_child (iter, child, 1);

	return TRUE;
}

static gboolean
canonical_minus (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *one;
	CdnExpressionTreeIter *mult;
	CdnStackManipulation const *smanip1;
	CdnStackManipulation const *smanip2;
	CdnStackArg arg1 = CDN_STACK_ARG (1, 1);

	smanip1 = cdn_instruction_get_stack_manipulation (iter->children[0]->instruction,
	                                                  NULL);

	smanip2 = cdn_instruction_get_stack_manipulation (iter->children[1]->instruction,
	                                                  NULL);

	// t1 - t2 => t1 + -1 * t2
	mult = iter_new_sized (create_multiply (&arg1,
	                                        &smanip2->push),
	                       2);

	one = iter_new (cdn_instruction_number_new (-1));

	iter_set_child (mult, one, 0);

	cdn_mini_object_unref (CDN_MINI_OBJECT (iter->instruction));

	iter->instruction = create_plus (&smanip1->push,
	                                 &smanip2->push);

	iter_set_child (mult, iter->children[1], 1);
	iter_set_child (iter, mult, 1);

	return TRUE;
}

static gint
function_argument_index (CdnFunction *f,
                         CdnVariable *p)
{
	GList const *args;
	gint i = 0;

	args = cdn_function_get_arguments (f);

	while (args)
	{
		if (_cdn_function_argument_get_variable (args->data) == p)
		{
			return i;
		}

		++i;
		args = g_list_next (args);
	}

	return -1;
}

static CdnExpressionTreeIter *
make_index (guint const        *indices,
            CdnDimension const *dim)
{
	gdouble *r;
	guint n;
	guint i;
	CdnExpressionTreeIter *ret;

	n = cdn_dimension_size (dim);

	r = g_new (gdouble, n);

	for (i = 0; i < n; ++i)
	{
		r[i] = (gdouble)indices[i];
	}

	ret = iter_new_number_matrix (r, dim);
	g_free (r);

	return ret;
}

static void
canonical_custom_function_real (CdnExpressionTreeIter *iter,
                                CdnFunction           *f)
{
	CdnExpressionTreeIter *func;
	GQueue q;
	gint i;

	func = cdn_expression_tree_iter_new (cdn_function_get_expression (f));

	cdn_debug_push_indent ();
	cdn_expression_tree_iter_simplify (func);
	cdn_debug_pop_indent ();

	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Inlining function: %s: {%s}",
	                   cdn_object_get_full_id_for_display (CDN_OBJECT (f)),
	                   cdn_expression_tree_iter_to_string (func));

	// Replace all property instructions with the nodes which are the
	// current children of the iter
	g_queue_init (&q);

	g_queue_push_head (&q, func);

	while (!g_queue_is_empty (&q))
	{
		CdnExpressionTreeIter *it;

		it = g_queue_pop_head (&q);

		if (CDN_IS_INSTRUCTION_VARIABLE (it->instruction))
		{
			CdnVariable *prop;
			CdnInstructionVariable *pi;

			pi = (CdnInstructionVariable *)(it->instruction);
			prop = cdn_instruction_variable_get_variable (pi);

			if (cdn_variable_get_object (prop) == CDN_OBJECT (f))
			{
				// Find argument index
				gint idx = function_argument_index (f, prop);
				CdnExpressionTreeIter *cp;
				guint const *slice;
				guint slice_length;
				CdnDimension slice_dim;

				if (idx >= 0)
				{
					cp = iter_copy (iter->children[idx]);
				}
				else
				{
					cp = cdn_expression_tree_iter_new (cdn_variable_get_expression (prop));

					cdn_debug_push_indent ();
					cdn_expression_tree_iter_canonicalize (cp);
					cdn_debug_pop_indent ();
				}

				// Preserve slice
				slice = cdn_instruction_variable_get_slice (pi,
				                                            &slice_length,
				                                            &slice_dim);

				if (slice)
				{
					if (CDN_IS_INSTRUCTION_VARIABLE (cp->instruction))
					{
						CdnInstructionVariable *vv;

						vv = (CdnInstructionVariable *)cp->instruction;

						cdn_instruction_variable_apply_slice (vv,
						                                      slice,
						                                      slice_length,
						                                      &slice_dim);
					}
					else
					{
						cp = iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_INDEX,
						                     make_index (slice, &slice_dim),
						                     cp,
						                     TRUE,
						                     TRUE);
					}
				}

				if (it == func)
				{
					func = cp;
				}

				g_queue_push_head (&q, cp);

				// Replace the iter with child arg
				iter_replace (it, cp);

				cdn_expression_tree_iter_free (it);
			}
		}
		else
		{
			for (i = 0; i < it->num_children; ++i)
			{
				g_queue_push_head (&q, it->children[i]);
			}
		}
	}

	iter_replace_into (func, iter);
}

static gboolean
canonical_custom_function (CdnExpressionTreeIter *iter)
{
	// Custom functions are flattened in canonical form, if possible
	CdnInstructionCustomFunction *instr;
	CdnFunction *func;

	instr = (CdnInstructionCustomFunction *)(iter->instruction);
	func = cdn_instruction_custom_function_get_function (instr);

	if (cdn_function_get_expression (func))
	{
		canonical_custom_function_real (iter, func);
	}

	return TRUE;
}

static gboolean
canonical_custom_operator (CdnExpressionTreeIter *iter)
{
	// Custom operators that support funtions are flattened in canonical form
	CdnInstructionCustomOperator *instr;
	CdnOperator *op;
	CdnFunction *f;

	instr = (CdnInstructionCustomOperator *)(iter->instruction);
	op = cdn_instruction_custom_operator_get_operator (instr);

	f = cdn_operator_get_primary_function (op);

	if (f)
	{
		canonical_custom_function_real (iter, f);
	}

	return TRUE;
}

gboolean
iter_canonicalize (CdnExpressionTreeIter *iter,
                   gboolean               canonicalize_children,
                   gboolean               dodefactor)
{
	gint i;
	gboolean ret = FALSE;

	// Canonicalize the children first
	if (canonicalize_children)
	{
		for (i = 0; i < iter->num_children; ++i)
		{
			if (iter_canonicalize (iter->children[i], TRUE, dodefactor))
			{
				ret = TRUE;
			}
		}
	}

	if (iter_is_minus (iter))
	{
		if (canonical_minus (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_unary_minus (iter))
	{
		if (canonical_unary_minus (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_plus (iter))
	{
		if (canonical_plus (iter))
		{
			ret = TRUE;
		}
	}
	else if (iter_is_multiply (iter))
	{
		if (canonical_multiply (iter, dodefactor))
		{
			ret = TRUE;
		}
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (iter->instruction))
	{
		if (canonical_custom_function (iter))
		{
			ret = TRUE;
		}
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (iter->instruction))
	{
		if (canonical_custom_operator (iter))
		{
			ret = TRUE;
		}
	}

	iter_invalidate_cache_down (iter);
	return ret;
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_canonicalize (CdnExpressionTreeIter *iter)
{
	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Canonicalize:  {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	iter_canonicalize (iter, TRUE, TRUE);
	iter_invalidate_cache_down (iter);

	cdn_debug_message (DEBUG_SIMPLIFY,
	                   "Canonicalized: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	return iter;
}

void
iter_canonical_resort (CdnExpressionTreeIter *iter)
{
	// This function is used when a specific @iter which was embedded in
	// canonical form has changed. @iter is assumed to be still in
	// canonical form (if not, use cdn_expression_tree_iter_canonicalize)
	// before using this function).
	while (iter->parent)
	{
		iter_canonicalize (iter->parent, FALSE, FALSE);
		iter = iter->parent;
	}
}
