#include "cpg-expression-tree-iter.h"

#include <cpg-network/instructions/cpg-instructions.h>
#include <cpg-network/cpg-math.h>
#include <math.h>
#include "cpg-stack-private.h"
#include "cpg-operators.h"
#include "cpg-debug.h"
#include "cpg-symbolic.h"
#include "tree-algorithms/cpg-tree-algorithms-private.h"

void
cpg_expression_tree_iter_free (CpgExpressionTreeIter *self)
{
	if (!self)
	{
		return;
	}

	if (self->children)
	{
		gint i;

		for (i = 0; i < self->num_children; ++i)
		{
			cpg_expression_tree_iter_free (self->children[i]);
		}

		g_free (self->children);

		self->children = NULL;
		self->num_children = 0;
	}

	if (self->instruction)
	{
		cpg_mini_object_free (CPG_MINI_OBJECT (self->instruction));
	}

	g_free (self->cached_to_string);
	self->cached_to_string = NULL;

	g_slice_free (CpgExpressionTreeIter, self);
}

static CpgExpressionTreeIter *
tree_iter_new (CpgExpression *expression,
               GSList const  *instructions)
{
	GQueue stack;

	g_return_val_if_fail (expression == NULL || CPG_IS_EXPRESSION (expression), NULL);

	if (!instructions && expression)
	{
		instructions = cpg_expression_get_instructions (expression);
	}

	g_queue_init (&stack);

	while (instructions)
	{
		CpgExpressionTreeIter *iter;
		CpgInstruction *inst;
		gint cnt;
		gint i;

		inst = instructions->data;
		iter = iter_new (inst);

		cnt = -cpg_instruction_get_stack_count (inst) + 1;

		if (cnt > 0)
		{
			iter->children = g_new (CpgExpressionTreeIter *, cnt);
			iter->num_children = cnt;
		}

		for (i = 0; i < cnt; ++i)
		{
			CpgExpressionTreeIter *child;

			child = g_queue_pop_head (&stack);

			child->parent = iter;
			iter->children[cnt - i - 1] = child;
		}

		g_queue_push_head (&stack, iter);
		instructions = g_slist_next (instructions);
	}

	return g_queue_pop_head (&stack);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_new (CpgExpression *expression)
{
	return tree_iter_new (expression, NULL);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_new_from_instructions (GSList const *instructions)
{
	return tree_iter_new (NULL, instructions);
}

CpgInstruction *
cpg_expression_tree_iter_get_instruction (CpgExpressionTreeIter *iter)
{
	return iter->instruction;
}

void
cpg_expression_tree_iter_set_instruction (CpgExpressionTreeIter *iter,
                                          CpgInstruction        *instr)
{
	if (iter->instruction)
	{
		cpg_mini_object_free (CPG_MINI_OBJECT (iter->instruction));
		iter->instruction = NULL;
	}

	if (instr)
	{
		iter->instruction = CPG_INSTRUCTION (cpg_mini_object_copy (CPG_MINI_OBJECT (instr)));
	}

	iter_invalidate_cache_up (iter);
}

gint
cpg_expression_tree_iter_num_children (CpgExpressionTreeIter *iter)
{
	return iter->num_children;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_get_child (CpgExpressionTreeIter *iter,
                                    gint nth)
{
	g_return_val_if_fail (nth >= 0 && nth < iter->num_children, NULL);

	return iter->children[nth];
}

void
cpg_expression_tree_iter_take_child (CpgExpressionTreeIter *iter,
                                     gint                   nth,
                                     CpgExpressionTreeIter *child)
{
	g_return_if_fail (nth >= 0 && nth < iter->num_children);

	cpg_expression_tree_iter_free (iter->children[nth]);

	iter->children[nth] = child;
	child->parent = iter;

	iter_invalidate_cache_up (child);
}

void
cpg_expression_tree_iter_set_child (CpgExpressionTreeIter *iter,
                                    gint                   nth,
                                    CpgExpressionTreeIter *child)
{
	cpg_expression_tree_iter_take_child (iter,
	                                     nth,
	                                     iter_copy (child));
}

static GSList *
iter_to_instructions (CpgExpressionTreeIter *iter,
                      GSList                *ret)
{
	gint i;

	ret = g_slist_prepend (ret, cpg_mini_object_copy (CPG_MINI_OBJECT (iter->instruction)));

	for (i = iter->num_children - 1; i >= 0; --i)
	{
		ret = iter_to_instructions (iter->children[i], ret);
	}

	return ret;
}

GSList *
cpg_expression_tree_iter_to_instructions (CpgExpressionTreeIter *iter)
{
	return iter_to_instructions (iter, NULL);
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_copy (CpgExpressionTreeIter *iter)
{
	return iter_copy (iter);
}

gboolean
cpg_expression_tree_iter_equal (CpgExpressionTreeIter *iter,
                                CpgExpressionTreeIter *other)
{
	gint i;

	if (!cpg_instruction_equal (iter->instruction, other->instruction))
	{
		return FALSE;
	}

	if (iter->num_children != other->num_children)
	{
		return FALSE;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		if (!cpg_expression_tree_iter_equal (iter->children[i], other->children[i]))
		{
			return FALSE;
		}
	}

	return TRUE;
}

CpgExpressionTreeIter *
cpg_expression_tree_iter_new_from_instruction (CpgInstruction *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION (instruction), NULL);

	return iter_new (instruction);
}
