#include "cpg-tree-algorithms-private.h"
#include <cpg-network/instructions/cpg-instructions.h>
#include "cpg-network/cpg-operators.h"

static gboolean canonical_multiply (CpgExpressionTreeIter *iter,
                                    gboolean               dodefactor);

static CpgInstruction *
create_multiply ()
{
	return cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	                                     "*",
	                                     2);
}

static CpgInstruction *
create_plus ()
{
	return cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
	                                     "+",
	                                     2);
}

static gint
function_id_cos_sin (gint     id,
                     gboolean siniscos)
{
	switch (id)
	{
		case CPG_MATH_FUNCTION_TYPE_COS:
			return CPG_MATH_FUNCTION_TYPE_NUM + 1;
		break;
		case CPG_MATH_FUNCTION_TYPE_SIN:
			if (siniscos)
			{
				return CPG_MATH_FUNCTION_TYPE_NUM + 1;
			}
			else
			{
				return CPG_MATH_FUNCTION_TYPE_NUM + 2;
			}
		break;
	}

	return id;
}

static gint
compare_function (CpgExpressionTreeIter const *iter1,
                  CpgExpressionTreeIter const *iter2,
                  gboolean                     siniscos)
{
	gint i1;
	gint i2;
	CpgInstructionFunction *func1;
	CpgInstructionFunction *func2;

	func1 = (CpgInstructionFunction *)(iter1->instruction);
	func2 = (CpgInstructionFunction *)(iter2->instruction);

	i1 = cpg_instruction_function_get_id (func1);
	i2 = cpg_instruction_function_get_id (func2);

	if (iter_is_function (iter1, NULL))
	{
		// Make sure to sort sin and cos last
		i1 = function_id_cos_sin (i1, siniscos);
		i2 = function_id_cos_sin (i2, siniscos);
	}

	return i1 > i2 ? -1 : (i1 < i2 ? 1 : 0);
}

static gint
compare_number (CpgExpressionTreeIter const *iter1,
                CpgExpressionTreeIter const *iter2)
{
	gdouble n1;
	gdouble n2;
	CpgInstructionNumber *num1;
	CpgInstructionNumber *num2;

	num1 = (CpgInstructionNumber *)(iter1->instruction);
	num2 = (CpgInstructionNumber *)(iter2->instruction);

	n1 = cpg_instruction_number_get_value (num1);
	n2 = cpg_instruction_number_get_value (num2);

	return fabs(n1 - n2) < 10e-9 ? 0 : (n1 > n2 ? 1 : -1);
}

static gint
compare_property (CpgExpressionTreeIter const *iter1,
                  CpgExpressionTreeIter const *iter2)
{
	CpgProperty *p1;
	CpgProperty *p2;
	CpgObject *o1;
	CpgObject *o2;
	CpgInstructionProperty *prop1;
	CpgInstructionProperty *prop2;

	prop1 = (CpgInstructionProperty *)(iter1->instruction);
	prop2 = (CpgInstructionProperty *)(iter2->instruction);

	p1 = cpg_instruction_property_get_property (prop1);
	p2 = cpg_instruction_property_get_property (prop2);

	o1 = cpg_property_get_object (p1);
	o2 = cpg_property_get_object (p2);

	if (o1 != o2)
	{
		return o1 > o2 ? -1 : 1;
	}

	return g_strcmp0 (cpg_property_get_name (p1),
	                  cpg_property_get_name (p2));
}

typedef struct
{
	GType type;
	gint id;
} TypeId;

static gint
type_id (CpgInstruction *instr)
{
	TypeId order[] = {
		{CPG_TYPE_INSTRUCTION_NUMBER, 0},
		{CPG_TYPE_INSTRUCTION_CONSTANT, 0},
		{CPG_TYPE_INSTRUCTION_PROPERTY, 1},
		{CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, 2},
		{CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, 3},
		{CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, 4},
		{CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, 4},
		{CPG_TYPE_INSTRUCTION_OPERATOR, 5},
		{CPG_TYPE_INSTRUCTION_FUNCTION, 6},
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
compare_iters (CpgExpressionTreeIter const *iter1,
               CpgExpressionTreeIter const *iter2,
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

	if (type == CPG_TYPE_INSTRUCTION_FUNCTION ||
	    type == CPG_TYPE_INSTRUCTION_OPERATOR)
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
	else if (type == CPG_TYPE_INSTRUCTION_PROPERTY)
	{
		return compare_property (iter1, iter2);
	}
	else if (type == CPG_TYPE_INSTRUCTION_NUMBER ||
	         type == CPG_TYPE_INSTRUCTION_CONSTANT)
	{
		return compare_number (iter1, iter2);
	}

	return 0;
}

static GSList *
collect_plus_terms (CpgExpressionTreeIter *iter,
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

static CpgExpressionTreeIter *
terminal_part (CpgExpressionTreeIter *iter,
               CpgMathOperatorType    type)
{
	CpgMathOperatorType tp;

	if (iter_is_operator (iter, &tp) && type == tp)
	{
		return iter->children[0];
	}
	else
	{
		return iter;
	}
}

static gboolean
merge_trees_advance (CpgExpressionTreeIter **root,
                     CpgExpressionTreeIter **ptr,
                     CpgExpressionTreeIter *otherptr,
                     CpgExpressionTreeIter  *tleft,
                     CpgExpressionTreeIter  *tright)
{
	iter_set_child (*root, tleft, 0);

	if (tleft == *ptr)
	{
		iter_set_child (*root, otherptr, 1);
		return FALSE;
	}
	else
	{
		CpgExpressionTreeIter *nl;

		nl = (*ptr)->children[1];

		iter_set_child (*ptr, NULL, 1);
		iter_set_child (*root, *ptr, 1);

		*root = *ptr;
		*ptr = nl;
		return TRUE;
	}
}

static gboolean
merge_commutative_operator_trees (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *root;
	CpgExpressionTreeIter *left;
	CpgExpressionTreeIter *right;
	CpgMathOperatorType type;
	CpgInstructionFunction *instr;

	instr = (CpgInstructionFunction *)iter->instruction;
	type = cpg_instruction_function_get_id (instr);

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
			                   type == CPG_MATH_OPERATOR_TYPE_PLUS) <= 0)
			{
				return FALSE;
			}
		}
		else if (compare_iters (left,
		                        right->children[0],
		                        type == CPG_MATH_OPERATOR_TYPE_PLUS) <= 0)
		{
			return FALSE;
		}
	}

	iter_set_child (iter, NULL, 0);
	iter_set_child (iter, NULL, 1);

	while (TRUE)
	{
		CpgExpressionTreeIter *tleft;
		CpgExpressionTreeIter *tright;

		tleft = terminal_part (left, type);
		tright = terminal_part (right, type);

		if (compare_iters (tleft,
		                   tright,
		                   type == CPG_MATH_OPERATOR_TYPE_PLUS) <= 0)
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
canonical_plus (CpgExpressionTreeIter *iter)
{
	return merge_commutative_operator_trees (iter);
}

static gboolean
defactorize (CpgExpressionTreeIter *iter)
{
	gboolean i1;
	gboolean i2;
	GSList *l1;
	GSList *l2;
	CpgExpressionTreeIter *plus;
	CpgExpressionTreeIter *last;

	i1 = iter_is_plus (iter->children[0]);
	i2 = iter_is_plus (iter->children[1]);

	if (!(i1 || i2))
	{
		return FALSE;
	}

	// Collect all the terms in left and right that need to be multiplied
	l1 = collect_plus_terms (iter->children[0], NULL);
	l2 = collect_plus_terms (iter->children[1], NULL);

	last = iter_new_sized (create_plus (), 2);
	plus = last;

	iter_set_child (iter, NULL, 0);
	iter_set_child (iter, NULL, 1);

	// For each combination of elements in l1 and l2, create a
	// multiplication tree and add the term to the new plus tree in iter
	while (l1)
	{
		GSList *item;

		for (item = l2; item; item = g_slist_next (item))
		{
			CpgExpressionTreeIter *mult;
			CpgInstruction *instr;

			instr = create_multiply ();

			mult = iter_new_sized (instr, 2);

			iter_set_child (mult,
			                item->next ? iter_copy (l1->data) : l1->data,
			                0);

			iter_set_child (mult,
			                l1->next ? iter_copy (item->data) : item->data,
			                1);

			// Make sure the new 'mult' is properly canonicalized
			canonical_multiply (mult, TRUE);

			if (plus == last && plus->children[1] == NULL)
			{
				iter_set_child (plus, mult, 1);
			}
			else if (plus == last && plus->children[0] == NULL)
			{
				iter_set_child (plus, mult, 0);
				canonical_plus (plus);
			}
			else
			{
				CpgExpressionTreeIter *np;

				np = iter_new_sized (create_plus (),
				                     2);

				iter_set_child (np, plus, 1);
				iter_set_child (np, mult, 0);

				plus = np;

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
canonical_multiply (CpgExpressionTreeIter *iter,
                    gboolean               dodefactor)
{
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
canonical_unary_minus (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *one;
	CpgExpressionTreeIter *child;

	// -(term) => -1 * term
	cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
	iter->instruction = create_multiply ();

	child = iter->children[0];
	g_free (iter->children);

	one = iter_new (cpg_instruction_number_new (-1));

	iter->num_children = 2;
	iter->children = g_new (CpgExpressionTreeIter *, 2);

	iter_set_child (iter, one, 0);
	iter_set_child (iter, child, 1);

	return TRUE;
}

static gboolean
canonical_minus (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *one;
	CpgExpressionTreeIter *mult;

	// t1 - t2 => t1 + -1 * t2
	mult = iter_new_sized (create_multiply (), 2);

	one = iter_new (cpg_instruction_number_new (-1));

	iter_set_child (mult, one, 0);

	cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
	iter->instruction = create_plus ();

	iter_set_child (mult, iter->children[1], 1);
	iter_set_child (iter, mult, 1);

	return TRUE;
}

static gint
function_argument_index (CpgFunction *f,
                         CpgProperty *p)
{
	GList const *args;
	gint i = 0;

	args = cpg_function_get_arguments (f);

	while (args)
	{
		if (_cpg_function_argument_get_property (args->data) == p)
		{
			return i;
		}

		++i;
		args = g_list_next (args);
	}

	return -1;
}

static void
canonical_custom_function_real (CpgExpressionTreeIter *iter,
                                CpgFunction           *f)
{
	CpgExpressionTreeIter *func;
	GQueue q;
	gint i;

	func = cpg_expression_tree_iter_new (cpg_function_get_expression (f));
	cpg_expression_tree_iter_canonicalize (func);

	// Replace all property instructions with the nodes which are the
	// current children of the iter
	g_queue_init (&q);

	g_queue_push_head (&q, func);

	while (!g_queue_is_empty (&q))
	{
		CpgExpressionTreeIter *it;

		it = g_queue_pop_head (&q);

		if (CPG_IS_INSTRUCTION_PROPERTY (it->instruction))
		{
			CpgProperty *prop;
			CpgInstructionProperty *pi;

			pi = (CpgInstructionProperty *)(it->instruction);
			prop = cpg_instruction_property_get_property (pi);

			if (cpg_property_get_object (prop) == CPG_OBJECT (f))
			{
				// Find argument index
				gint idx = function_argument_index (f, prop);
				CpgExpressionTreeIter *cp;

				cp = iter_copy (iter->children[idx]);

				if (it == func)
				{
					func = cp;
				}

				// Replace the iter with child arg
				iter_replace (it, cp);
				cpg_expression_tree_iter_free (it);
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
canonical_custom_function (CpgExpressionTreeIter *iter)
{
	// Custom functions are flattened in canonical form
	CpgInstructionCustomFunction *instr;
	CpgFunction *func;

	instr = (CpgInstructionCustomFunction *)(iter->instruction);
	func = cpg_instruction_custom_function_get_function (instr);

	canonical_custom_function_real (iter, func);
	return TRUE;
}

static gboolean
canonical_custom_operator (CpgExpressionTreeIter *iter)
{
	// Custom operators that support funtions are flattened in canonical form
	CpgInstructionCustomOperator *instr;
	CpgOperator *op;
	CpgFunction *f;

	instr = (CpgInstructionCustomOperator *)(iter->instruction);
	op = cpg_instruction_custom_operator_get_operator (instr);

	if (CPG_IS_OPERATOR_DF_DT (op))
	{
		CpgExpressionTreeIter *cp;
		CpgOperatorDfDt *dfdt = CPG_OPERATOR_DF_DT (op);

		cp = cpg_expression_tree_iter_new (cpg_operator_df_dt_get_derived (dfdt));
		cpg_expression_tree_iter_canonicalize (cp);

		// Replace iter with cp
		iter_replace_into (cp, iter);
	}
	else
	{
		f = cpg_operator_get_primary_function (op);

		if (f)
		{
			canonical_custom_function_real (iter, f);
		}
	}

	return TRUE;
}

gboolean
iter_canonicalize (CpgExpressionTreeIter *iter,
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
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (iter->instruction))
	{
		if (canonical_custom_function (iter))
		{
			ret = TRUE;
		}
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (iter->instruction))
	{
		if (canonical_custom_operator (iter))
		{
			ret = TRUE;
		}
	}

	iter_invalidate_cache_down (iter);
	return ret;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_canonicalize (CpgExpressionTreeIter *iter)
{
	iter_canonicalize (iter, TRUE, TRUE);
	iter_invalidate_cache_down (iter);

	return iter;
}

void
iter_canonical_resort (CpgExpressionTreeIter *iter)
{
	// This function is used when a specific @iter which was embedded in
	// canonical form has changed. @iter is assumed to be still in
	// canonical form (if not, use cpg_expression_tree_iter_canonicalize)
	// before using this function).
	while (iter->parent)
	{
		iter_canonicalize (iter->parent, FALSE, FALSE);
		iter = iter->parent;
	}
}
