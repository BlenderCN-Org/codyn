#include "cpg-tree-algorithms-private.h"

CpgExpressionTreeIter *
iter_new_numstr (gchar const *num)
{
	return iter_new (cpg_instruction_number_new_from_string (num));
}

CpgExpressionTreeIter *
iter_copy (CpgExpressionTreeIter *iter)
{
	CpgExpressionTreeIter *cp;
	gint i;

	if (!iter)
	{
		return NULL;
	}

	cp = iter_new_sized (iter->instruction, iter->num_children);

	for (i = 0; i < cp->num_children; ++i)
	{
		cp->children[i] = iter_copy (iter->children[i]);

		if (cp->children[i])
		{
			cp->children[i]->parent = cp;
		}
	}

	return cp;
}

void
iter_invalidate_cache_up (CpgExpressionTreeIter *iter)
{
	if (!iter)
	{
		return;
	}

	iter_invalidate_cache_up (iter->parent);

	g_free (iter->cached_to_string);
	iter->cached_to_string = NULL;
}

void
iter_invalidate_cache_down (CpgExpressionTreeIter *iter)
{
	gint i;

	if (!iter)
	{
		return;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		iter_invalidate_cache_down (iter->children[i]);
	}

	g_free (iter->cached_to_string);
	iter->cached_to_string = NULL;
}

CpgExpressionTreeIter *
iter_new (CpgInstruction *instruction)
{
	CpgExpressionTreeIter *ret;

	ret = g_slice_new0 (CpgExpressionTreeIter);

	ret->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instruction)));

	return ret;
}

CpgExpressionTreeIter *
iter_new_sized (CpgInstruction *instruction, gint num)
{
	CpgExpressionTreeIter *ret;

	ret = iter_new (instruction);
	ret->num_children = num;

	ret->children = g_new0 (CpgExpressionTreeIter *, num);
	return ret;
}

GSList *
iter_find_properties (CpgExpressionTreeIter *iter,
                      CpgProperty           *prop,
                      GSList                *ret)
{
	gint i;

	for (i = 0; i < iter->num_children; ++i)
	{
		ret = iter_find_properties (iter->children[i], prop, ret);
	}

	if (CPG_IS_INSTRUCTION_PROPERTY (iter->instruction))
	{
		CpgInstructionProperty *instr;

		instr = (CpgInstructionProperty *)(iter->instruction);

		if (!prop || cpg_instruction_property_get_property (instr) == prop)
		{
			ret = g_slist_prepend (ret, iter);
		}
	}

	return ret;
}

gint
iter_index_of (CpgExpressionTreeIter *parent,
               CpgExpressionTreeIter *iter)
{
	gint i;

	if (!parent)
	{
		if (!iter)
		{
			return -1;
		}

		parent = iter->parent;
	}

	for (i = 0; i < parent->num_children; ++i)
	{
		if (parent->children[i] == iter)
		{
			return i;
		}
	}

	return -1;
}

void
iter_replace (CpgExpressionTreeIter *iter,
              CpgExpressionTreeIter *other)
{
	gint i;

	if (!iter)
	{
		return;
	}

	if (other)
	{
		iter_replace (other, NULL);
		other->parent = NULL;
	}

	if (!iter->parent)
	{
		return;
	}

	i = iter_index_of (NULL, iter);

	if (i == -1)
	{
		return;
	}

	iter->parent->children[i] = other;

	if (other != NULL)
	{
		other->parent = iter->parent;
	}

	iter->parent = NULL;
}

void
iter_set_child (CpgExpressionTreeIter *parent,
                CpgExpressionTreeIter *child,
                gint                   idx)
{
	if (child)
	{
		iter_replace (child, NULL);
	}

	parent->children[idx] = child;

	if (child)
	{
		child->parent = parent;
	}
}

void
iter_replace_or_copy_into (CpgExpressionTreeIter *cp,
                           CpgExpressionTreeIter *dest,
                           gboolean               make_copy)
{
	gint i;
	CpgExpressionTreeIter **newchildren;

	newchildren = g_new (CpgExpressionTreeIter *, cp->num_children);

	for (i = 0; i < cp->num_children; ++i)
	{
		if (make_copy)
		{
			newchildren[i] = cpg_expression_tree_iter_copy (cp->children[i]);
		}
		else
		{
			newchildren[i] = cp->children[i];
			cp->children[i] = NULL;
		}

		newchildren[i]->parent = dest;
	}

	for (i = 0; i < dest->num_children; ++i)
	{
		if (make_copy || dest->children[i] != cp)
		{
			cpg_expression_tree_iter_free (dest->children[i]);
		}
	}

	g_free (dest->children);

	dest->num_children = cp->num_children;
	dest->children = newchildren;

	cpg_mini_object_free (CPG_MINI_OBJECT (dest->instruction));

	if (make_copy)
	{
		dest->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (cp->instruction)));
	}
	else
	{
		dest->instruction = cp->instruction;
		cp->instruction = NULL;

		cpg_expression_tree_iter_free (cp);
	}
}

void
iter_replace_into (CpgExpressionTreeIter *cp,
                   CpgExpressionTreeIter *dest)
{
	iter_replace_or_copy_into (cp, dest, FALSE);
}

void
iter_copy_into (CpgExpressionTreeIter *cp,
                CpgExpressionTreeIter *dest)
{
	iter_replace_or_copy_into (cp, dest, TRUE);
}

CpgExpressionTreeIter *
iter_brother (CpgExpressionTreeIter *iter)
{
	gint i;

	for (i = 0; i < iter->parent->num_children; ++i)
	{
		if (iter->parent->children[i] != iter)
		{
			return iter->parent->children[i];
		}
	}

	return NULL;
}

gboolean
iter_is_operator (CpgExpressionTreeIter const *iter,
                  CpgMathOperatorType   *type)
{
	if (CPG_IS_INSTRUCTION_OPERATOR (iter->instruction))
	{
		if (type)
		{
			*type = cpg_instruction_function_get_id ((CpgInstructionFunction *)(iter->instruction));
		}

		return TRUE;
	}

	return FALSE;
}

gboolean
iter_is_multiply (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_MULTIPLY;
}

gboolean
iter_is_power (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_POWER;
}

gboolean
iter_is_divide (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_DIVIDE;
}

gboolean
iter_is_plus (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	if (!iter)
	{
		return FALSE;
	}

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_PLUS;
}

gboolean
iter_is_minus (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_MINUS;
}

gboolean
iter_is_unary_minus (CpgExpressionTreeIter const *iter)
{
	CpgMathOperatorType type;

	return iter_is_operator (iter, &type) &&
	       type == CPG_MATH_OPERATOR_TYPE_UNARY_MINUS;
}

gboolean
iter_is_number (CpgExpressionTreeIter const *iter,
                gdouble                     *num)
{
	if (CPG_IS_INSTRUCTION_NUMBER (iter->instruction))
	{
		if (num)
		{
			*num = cpg_instruction_number_get_value ((CpgInstructionNumber *)(iter->instruction));
		}

		return TRUE;
	}

	return FALSE;
}

gboolean
iter_is_function (CpgExpressionTreeIter const *iter,
                  CpgMathFunctionType         *type)
{
	if (!iter_is_operator (iter, NULL) &&
	    CPG_IS_INSTRUCTION_FUNCTION (iter->instruction))
	{
		if (type)
		{
			*type = cpg_instruction_function_get_id ((CpgInstructionFunction *)iter->instruction);
		}

		return TRUE;
	}

	return FALSE;
}
