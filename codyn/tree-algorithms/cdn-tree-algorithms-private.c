#include "cdn-tree-algorithms-private.h"

CdnExpressionTreeIter *
iter_new_numstr (gchar const *num)
{
	return iter_new_take (cdn_instruction_number_new_from_string (num));
}

CdnExpressionTreeIter *
iter_new_num (gdouble value)
{
	return iter_new_take (cdn_instruction_number_new (value));
}

CdnExpressionTreeIter *
iter_copy (CdnExpressionTreeIter *iter)
{
	CdnExpressionTreeIter *cp;
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
iter_invalidate_cache_up (CdnExpressionTreeIter *iter)
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
iter_invalidate_cache_down (CdnExpressionTreeIter *iter)
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

CdnExpressionTreeIter *
iter_new_take (CdnInstruction *instruction)
{
	CdnExpressionTreeIter *ret;

	ret = g_slice_new0 (CdnExpressionTreeIter);

	ret->instruction = instruction;
	return ret;
}

CdnExpressionTreeIter *
iter_new (CdnInstruction *instruction)
{
	return iter_new_take (cdn_mini_object_copy (instruction));
}

CdnExpressionTreeIter *
iter_new_sized (CdnInstruction *instruction,
                gint num)
{
	return iter_new_sized_take (cdn_mini_object_copy (instruction), num);
}

CdnExpressionTreeIter *
iter_new_sized_take (CdnInstruction *instruction,
                     gint num)
{
	CdnExpressionTreeIter *ret;

	ret = iter_new_take (instruction);
	ret->num_children = num;

	ret->children = g_new0 (CdnExpressionTreeIter *, num);
	return ret;
}

GSList *
iter_remove_variables (CdnExpressionTreeIter *iter,
                      CdnVariable           *prop,
                      GSList                *ret)
{
	gint i;

	for (i = 0; i < iter->num_children; ++i)
	{
		ret = iter_remove_variables (iter->children[i], prop, ret);
	}

	if (CDN_IS_INSTRUCTION_VARIABLE (iter->instruction))
	{
		CdnInstructionVariable *instr;

		instr = (CdnInstructionVariable *)(iter->instruction);

		if (!prop || cdn_instruction_variable_get_variable (instr) == prop)
		{
			ret = g_slist_prepend (ret, iter);
		}
	}

	return ret;
}

gint
iter_index_of (CdnExpressionTreeIter *parent,
               CdnExpressionTreeIter *iter)
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
iter_replace (CdnExpressionTreeIter *iter,
              CdnExpressionTreeIter *other)
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
iter_set_child (CdnExpressionTreeIter *parent,
                CdnExpressionTreeIter *child,
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
iter_replace_or_copy_into (CdnExpressionTreeIter *cp,
                           CdnExpressionTreeIter *dest,
                           gboolean               make_copy)
{
	gint i;
	CdnExpressionTreeIter **newchildren;

	newchildren = g_new (CdnExpressionTreeIter *, cp->num_children);

	for (i = 0; i < cp->num_children; ++i)
	{
		if (make_copy)
		{
			newchildren[i] = cdn_expression_tree_iter_copy (cp->children[i]);
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
			cdn_expression_tree_iter_free (dest->children[i]);
		}
	}

	g_free (dest->children);

	dest->num_children = cp->num_children;
	dest->children = newchildren;

	cdn_mini_object_unref (CDN_MINI_OBJECT (dest->instruction));

	if (make_copy)
	{
		dest->instruction = CDN_INSTRUCTION (cdn_mini_object_copy (CDN_MINI_OBJECT (cp->instruction)));
	}
	else
	{
		dest->instruction = cp->instruction;
		cp->instruction = NULL;

		cdn_expression_tree_iter_free (cp);
	}
}

void
iter_replace_into (CdnExpressionTreeIter *cp,
                   CdnExpressionTreeIter *dest)
{
	iter_replace_or_copy_into (cp, dest, FALSE);
}

void
iter_copy_into (CdnExpressionTreeIter *cp,
                CdnExpressionTreeIter *dest)
{
	iter_replace_or_copy_into (cp, dest, TRUE);
}

CdnExpressionTreeIter *
iter_brother (CdnExpressionTreeIter *iter)
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
iter_is_multiply (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_MULTIPLY;
}

gboolean
iter_is_power (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_POWER;
}

gboolean
iter_is_divide (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_DIVIDE;
}

gboolean
iter_is_plus (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	if (!iter)
	{
		return FALSE;
	}

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_PLUS;
}

gboolean
iter_is_minus (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_MINUS;
}

gboolean
iter_is_unary_minus (CdnExpressionTreeIter const *iter)
{
	CdnMathFunctionType type;

	return iter_is_function (iter, &type) &&
	       type == CDN_MATH_FUNCTION_TYPE_UNARY_MINUS;
}

gboolean
iter_is_number (CdnExpressionTreeIter const *iter,
                gdouble                     *num)
{
	if (CDN_IS_INSTRUCTION_NUMBER (iter->instruction))
	{
		if (num)
		{
			*num = cdn_instruction_number_get_value ((CdnInstructionNumber *)(iter->instruction));
		}

		return TRUE;
	}

	return FALSE;
}

static gboolean
cmp_double (gdouble a,
            gdouble b)
{
	return fabs (a - b) <= 10e-13;
}

gboolean
iter_is_natural_number (CdnExpressionTreeIter const *iter,
                        gint                        *num)
{
	gdouble numf;

	if (!iter_is_number (iter, &numf) || numf < 0)
	{
		return FALSE;
	}

	if (cmp_double (rint (numf), numf))
	{
		if (num)
		{
			*num = rint (numf);
		}

		return TRUE;
	}

	return FALSE;
}

gboolean
iter_is_function (CdnExpressionTreeIter const *iter,
                  CdnMathFunctionType         *type)
{
	if (CDN_IS_INSTRUCTION_FUNCTION (iter->instruction))
	{
		if (type)
		{
			*type = cdn_instruction_function_get_id ((CdnInstructionFunction *)iter->instruction);
		}

		return TRUE;
	}

	return FALSE;
}

CdnExpressionTreeIter *
iter_new_bfunc (CdnMathFunctionType    type,
                CdnExpressionTreeIter *a,
                CdnExpressionTreeIter *b,
                gboolean               take_a,
                gboolean               take_b)
{
	gint argdim[4];
	CdnStackManipulation const *smanipa;
	CdnStackManipulation const *smanipb;
	CdnExpressionTreeIter *ret;
	GError *err = NULL;

	smanipa = cdn_instruction_get_stack_manipulation (a->instruction, &err);
	smanipb = cdn_instruction_get_stack_manipulation (b->instruction, NULL);

	argdim[2] = smanipa->push_dims ? smanipa->push_dims[0] : 1;
	argdim[3] = smanipa->push_dims ? smanipa->push_dims[1] : 1;

	argdim[0] = smanipb->push_dims ? smanipb->push_dims[0] : 1;
	argdim[1] = smanipb->push_dims ? smanipb->push_dims[1] : 1;

	ret = iter_new_sized_take (cdn_instruction_function_new (type,
	                                                         NULL,
	                                                         2,
	                                                         argdim),
	                           2);

	ret->children[0] = take_a ? a : iter_copy (a);
	ret->children[1] = take_b ? b : iter_copy (b);

	return ret;
}

CdnExpressionTreeIter *
iter_new_ufunc (CdnMathFunctionType    type,
                CdnExpressionTreeIter *a,
                gboolean               take_a)
{
	CdnStackManipulation const *smanipa;
	CdnExpressionTreeIter *ret;

	smanipa = cdn_instruction_get_stack_manipulation (a->instruction,
	                                                  NULL);

	ret = iter_new_sized_take (cdn_instruction_function_new (type,
	                                                         NULL,
	                                                         1,
	                                                         smanipa->push_dims),
	                           1);

	ret->children[0] = take_a ? a : iter_copy (a);
	return ret;
}
