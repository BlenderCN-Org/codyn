#include "cdn-expression-tree-iter.h"

#include <codyn/instructions/cdn-instructions.h>
#include <codyn/cdn-math.h>
#include <math.h>
#include "cdn-stack-private.h"
#include "cdn-operators.h"
#include "cdn-debug.h"
#include "cdn-symbolic.h"
#include "tree-algorithms/cdn-tree-algorithms-private.h"

void
cdn_expression_tree_iter_free (CdnExpressionTreeIter *self)
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
			cdn_expression_tree_iter_free (self->children[i]);
		}

		g_free (self->children);

		self->children = NULL;
		self->num_children = 0;
	}

	if (self->instruction)
	{
		cdn_mini_object_free (CDN_MINI_OBJECT (self->instruction));
	}

	g_free (self->cached_to_string);
	self->cached_to_string = NULL;

	g_slice_free (CdnExpressionTreeIter, self);
}

static CdnExpressionTreeIter *
tree_iter_new (CdnExpression *expression,
               GSList const  *instructions)
{
	GQueue stack;

	g_return_val_if_fail (expression == NULL || CDN_IS_EXPRESSION (expression), NULL);

	if (!instructions && expression)
	{
		instructions = cdn_expression_get_instructions (expression);
	}

	g_queue_init (&stack);

	while (instructions)
	{
		CdnExpressionTreeIter *iter;
		CdnInstruction *inst;
		gint i;
		CdnStackManipulation const *smanip;

		inst = instructions->data;
		iter = iter_new (inst);

		smanip = cdn_instruction_get_stack_manipulation (inst, NULL);

		if (smanip->num_pop > 0)
		{
			iter->children = g_new (CdnExpressionTreeIter *, smanip->num_pop);
			iter->num_children = smanip->num_pop;
		}

		for (i = 0; i < smanip->num_pop; ++i)
		{
			CdnExpressionTreeIter *child;

			child = g_queue_pop_head (&stack);

			child->parent = iter;
			iter->children[smanip->num_pop - i - 1] = child;
		}

		g_queue_push_head (&stack, iter);
		instructions = g_slist_next (instructions);
	}

	return g_queue_pop_head (&stack);
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_new (CdnExpression *expression)
{
	return tree_iter_new (expression, NULL);
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_new_from_instructions (GSList const *instructions)
{
	return tree_iter_new (NULL, instructions);
}

CdnInstruction *
cdn_expression_tree_iter_get_instruction (CdnExpressionTreeIter *iter)
{
	return iter->instruction;
}

void
cdn_expression_tree_iter_set_instruction (CdnExpressionTreeIter *iter,
                                          CdnInstruction        *instr)
{
	if (iter->instruction)
	{
		cdn_mini_object_free (CDN_MINI_OBJECT (iter->instruction));
		iter->instruction = NULL;
	}

	if (instr)
	{
		iter->instruction = CDN_INSTRUCTION (cdn_mini_object_copy (CDN_MINI_OBJECT (instr)));
	}

	iter_invalidate_cache_up (iter);
}

gint
cdn_expression_tree_iter_num_children (CdnExpressionTreeIter *iter)
{
	return iter->num_children;
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_get_child (CdnExpressionTreeIter *iter,
                                    gint nth)
{
	g_return_val_if_fail (nth >= 0 && nth < iter->num_children, NULL);

	return iter->children[nth];
}

void
cdn_expression_tree_iter_take_child (CdnExpressionTreeIter *iter,
                                     gint                   nth,
                                     CdnExpressionTreeIter *child)
{
	g_return_if_fail (nth >= 0 && nth < iter->num_children);

	cdn_expression_tree_iter_free (iter->children[nth]);

	iter->children[nth] = child;
	child->parent = iter;

	iter_invalidate_cache_up (child);
}

void
cdn_expression_tree_iter_set_child (CdnExpressionTreeIter *iter,
                                    gint                   nth,
                                    CdnExpressionTreeIter *child)
{
	cdn_expression_tree_iter_take_child (iter,
	                                     nth,
	                                     iter_copy (child));
}

static GSList *
iter_to_instructions (CdnExpressionTreeIter *iter,
                      GSList                *ret)
{
	gint i;

	ret = g_slist_prepend (ret, cdn_mini_object_copy (CDN_MINI_OBJECT (iter->instruction)));

	for (i = iter->num_children - 1; i >= 0; --i)
	{
		ret = iter_to_instructions (iter->children[i], ret);
	}

	return ret;
}

GSList *
cdn_expression_tree_iter_to_instructions (CdnExpressionTreeIter *iter)
{
	return iter_to_instructions (iter, NULL);
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_copy (CdnExpressionTreeIter *iter)
{
	return iter_copy (iter);
}

gboolean
cdn_expression_tree_iter_equal (CdnExpressionTreeIter *iter,
                                CdnExpressionTreeIter *other)
{
	gint i;

	if (!cdn_instruction_equal (iter->instruction, other->instruction))
	{
		return FALSE;
	}

	if (iter->num_children != other->num_children)
	{
		return FALSE;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		if (!cdn_expression_tree_iter_equal (iter->children[i], other->children[i]))
		{
			return FALSE;
		}
	}

	return TRUE;
}

CdnExpressionTreeIter *
cdn_expression_tree_iter_new_from_instruction (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return iter_new (instruction);
}
