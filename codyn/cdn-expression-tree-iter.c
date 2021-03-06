#include "cdn-expression-tree-iter.h"

#include <codyn/instructions/cdn-instructions.h>
#include <codyn/cdn-math.h>
#include <math.h>
#include "cdn-stack-private.h"
#include "cdn-operators.h"
#include "cdn-debug.h"
#include "tree-algorithms/cdn-tree-algorithms-private.h"

/**
 * CdnExpressionTreeIter:
 *
 * Expression tree iterator.
 *
 * #CdnExpressionTreeIter is a class which deconstructs a #CdnExpression into
 * a tree of operations. This is a useful representation for manipulating an expression
 * symbolically. Various symbolic operations, such as derivation, are implemented for
 * these tree iters. After the symbolic manipulation, a linear #CdnExpression can
 * be reconstructed from the iter using #cdn_expression_tree_iter_to_expression.
 *
 */

GType
cdn_expression_tree_iter_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnExpressionTreeIter",
		                                      (GBoxedCopyFunc)cdn_expression_tree_iter_copy,
		                                      (GBoxedFreeFunc)cdn_expression_tree_iter_free);
	}

	return gtype;
}

/**
 * cdn_expression_tree_iter_free:
 * @iter: the #CdnExpressionTreeIter
 *
 * Free the expression tree iter.
 *
 */
void
cdn_expression_tree_iter_free (CdnExpressionTreeIter *iter)
{
	if (!iter)
	{
		return;
	}

	if (iter->children)
	{
		gint i;

		for (i = 0; i < iter->num_children; ++i)
		{
			cdn_expression_tree_iter_free (iter->children[i]);
		}

		g_free (iter->children);

		iter->children = NULL;
		iter->num_children = 0;
	}

	if (iter->instruction)
	{
		cdn_mini_object_unref (CDN_MINI_OBJECT (iter->instruction));
	}

	g_free (iter->cached_to_string);
	iter->cached_to_string = NULL;

	g_slice_free (CdnExpressionTreeIter, iter);
}

static CdnExpressionTreeIter *
tree_iter_new (CdnExpression const *expression,
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

		if (smanip->pop.num > 0)
		{
			iter->children = g_new (CdnExpressionTreeIter *, smanip->pop.num);
			iter->num_children = smanip->pop.num;
		}

		for (i = 0; i < smanip->pop.num; ++i)
		{
			CdnExpressionTreeIter *child;

			child = g_queue_pop_head (&stack);

			child->parent = iter;
			iter->children[smanip->pop.num - i - 1] = child;
		}

		g_queue_push_head (&stack, iter);
		instructions = g_slist_next (instructions);
	}

	return g_queue_pop_head (&stack);
}

/**
 * cdn_expression_tree_iter_new:
 * @expression: a #CdnExpression
 *
 * Create a new expression tree iter from an expression.
 *
 * Returns: (transfer full): a new expression tree iter.
 */
CdnExpressionTreeIter *
cdn_expression_tree_iter_new (CdnExpression const *expression)
{
	return tree_iter_new (expression, NULL);
}

/**
 * cdn_expression_tree_iter_new_from_instructions:
 * @instructions: (element-type CdnInstruction): A #GSList
 *
 * Create a new tree iter from a list of instructions.
 *
 * Returns: A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_new_from_instructions (GSList const *instructions)
{
	return tree_iter_new (NULL, instructions);
}

/**
 * cdn_expression_tree_iter_get_instruction:
 * @iter: A #CdnExpressionTreeIter
 *
 * Get the instruction for this tree iter.
 *
 * Returns: (transfer none): A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_expression_tree_iter_get_instruction (CdnExpressionTreeIter *iter)
{
	return iter->instruction;
}

/**
 * cdn_expression_tree_iter_set_instruction:
 * @iter: A #CdnExpressionTreeIter
 * @instr: A #CdnInstruction
 *
 * Set the instruction for this tree iter.
 *
 **/
void
cdn_expression_tree_iter_set_instruction (CdnExpressionTreeIter *iter,
                                          CdnInstruction        *instr)
{
	if (iter->instruction)
	{
		cdn_mini_object_unref (iter->instruction);
		iter->instruction = NULL;
	}

	if (instr)
	{
		iter->instruction = cdn_mini_object_ref (instr);
	}

	iter_invalidate_cache_up (iter);
}

/**
 * cdn_expression_tree_iter_get_num_children:
 * @iter: the #CdnExpressionTreeIter
 *
 * Get the number of children of the tree iter.
 *
 * Returns: the number of children
 *
 */
gint
cdn_expression_tree_iter_get_num_children (CdnExpressionTreeIter *iter)
{
	return iter->num_children;
}

/**
 * cdn_expression_tree_iter_set_num_children:
 * @iter: the #CdnExpressionTreeIter
 * @num: the number of children
 *
 * Reserve space for @num children in the iter. Any existing children
 * will be detroyed first.
 *
 */
void
cdn_expression_tree_iter_set_num_children (CdnExpressionTreeIter *iter,
                                           gint                   num)
{
	g_free (iter->children);
	iter->children = NULL;

	iter->num_children = num;

	if (num > 0)
	{
		iter->children = g_new0 (CdnExpressionTreeIter *, num);
	}
}

/**
 * cdn_expression_tree_iter_get_child:
 * @iter: A #CdnExpressionTreeIter
 * @nth: The index of the child
 *
 * Get the nth child of this tree iter.
 *
 * Returns: (transfer none): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_get_child (CdnExpressionTreeIter *iter,
                                    gint nth)
{
	g_return_val_if_fail (nth >= 0 && nth < iter->num_children, NULL);

	return iter->children[nth];
}

/**
 * cdn_expression_tree_iter_take_child:
 * @iter: A #CdnExpressionTreeIter
 * @nth: The index of the child
 * @child: (transfer full): A #CdnExpressionTreeIter
 *
 * Set the child of the tree iter, without making a copy of @child.
 *
 **/
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

/**
 * cdn_expression_tree_iter_set_child:
 * @iter: A #CdnExpressionTreeIter
 * @nth: The index of the child
 * @child: A #CdnExpressionTreeIter
 *
 * Set the child of the tree iter.
 *
 **/
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

	ret = g_slist_prepend (ret, cdn_mini_object_ref (iter->instruction));

	for (i = iter->num_children - 1; i >= 0; --i)
	{
		ret = iter_to_instructions (iter->children[i], ret);
	}

	return ret;
}

/**
 * cdn_expression_tree_iter_to_instructions:
 * @iter: A #CdnExpressionTreeIter
 *
 * Get the instructions that this tree iter represents (including its children).
 *
 * Returns: (transfer full) (element-type CdnInstructionBoxed): A #GSList
 *
 **/
GSList *
cdn_expression_tree_iter_to_instructions (CdnExpressionTreeIter *iter)
{
	return iter_to_instructions (iter, NULL);
}

/**
 * cdn_expression_tree_iter_copy:
 * @iter: A #CdnExpressionTreeIter
 *
 * Create a copy of the tree iter.
 *
 * Returns: (transfer full): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_copy (CdnExpressionTreeIter *iter)
{
	return iter_copy (iter);
}

/**
 * cdn_expression_tree_iter_equal:
 * @iter: the #CdnExpressionTreeIter
 * @other: the other iter
 * @asstring: whether to compare on string equality
 *
 * Compare two expression tree iters for equality. The two iters are first
 * compared by their instruction and then by their children.
 *
 * Returns: %TRUE if the iters are equal, %FALSE otherwise
 *
 */
gboolean
cdn_expression_tree_iter_equal (CdnExpressionTreeIter *iter,
                                CdnExpressionTreeIter *other,
                                gboolean               asstring)
{
	gint i;

	if (!cdn_instruction_equal (iter->instruction, other->instruction, asstring))
	{
		return FALSE;
	}

	if (iter->num_children != other->num_children)
	{
		return FALSE;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		if (!cdn_expression_tree_iter_equal (iter->children[i],
		                                     other->children[i],
		                                     asstring))
		{
			return FALSE;
		}
	}

	return TRUE;
}

/**
 * cdn_expression_tree_iter_new_from_instruction:
 * @instruction: A #CdnInstruction
 *
 * Create a new tree iter from an instruction.
 *
 * Returns: (transfer full): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_new_from_instruction (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return iter_new (instruction);
}

/**
 * cdn_expression_tree_iter_new_from_instruction_take:
 * @instruction: (transfer full): the instruction
 *
 * Create a new tree iter for a given instruction.
 *
 * Returns: (transfer full): a new #CdnExpressionTreeIter
 *
 */
CdnExpressionTreeIter *
cdn_expression_tree_iter_new_from_instruction_take (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return iter_new_take (instruction);
}

/**
 * cdn_expression_tree_iter_to_expression:
 * @iter: a #CdnExpressionTreeIter.
 *
 * Create an expression from a tree iter.
 *
 * Returns: (transfer full): a #CdnExpression.
 *
 **/
CdnExpression *
cdn_expression_tree_iter_to_expression (CdnExpressionTreeIter *iter)
{
	gchar const *s;
	CdnExpression *ret;
	GSList *newinstr;

	g_return_val_if_fail (iter != NULL, NULL);

	s = cdn_expression_tree_iter_to_string (iter);
	ret = cdn_expression_new (s);

	newinstr = cdn_expression_tree_iter_to_instructions (iter);
	cdn_expression_set_instructions_take (ret, newinstr);

	g_slist_foreach (newinstr, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (newinstr);

	return ret;
}

static gint
calculate_stack_manipulation (CdnStackManipulation const *smanip,
                              gint                       *tmpspace)
{
	gint ret = 0;
	gint i;

	for (i = 0; i < smanip->pop.num; ++i)
	{
		ret -= cdn_dimension_size (&smanip->pop.args[i].dimension);
	}

	ret += cdn_dimension_size (&smanip->push.dimension);

	*tmpspace = smanip->extra_space;
	return ret;
}

/**
 * cdn_expression_tree_iter_initialize_stack:
 * @iter: a #CdnExpressionTreeIter
 * @stack: a #CdnStack
 *
 * Initialize the provided @stack to contain enough space to evaluate @iter.
 *
 **/
void
cdn_expression_tree_iter_initialize_stack (CdnExpressionTreeIter *iter,
                                           CdnStack              *retstack)
{
	gint i;
	gint stack = 0;
	gint maxstack = 1;
	gint tmpspace = 0;
	GSList *instructions = NULL;
	GQueue queue = {0,};

	g_queue_init (&queue);
	g_queue_push_head (&queue, iter);

	while ((iter = g_queue_pop_head (&queue)))
	{
		instructions = g_slist_prepend (instructions,
		                                iter->instruction);

		// Compute stack from right to left
		for (i = iter->num_children - 1; i >= 0; --i)
		{
			g_queue_push_head (&queue, iter->children[i]);
		}
	}

	while (instructions)
	{
		CdnInstruction *inst = instructions->data;
		CdnStackManipulation const *smanip;
		gint nst;
		gint nmst;

		instructions = g_slist_delete_link (instructions, instructions);
		smanip = cdn_instruction_get_stack_manipulation (inst, NULL);

		if (!smanip)
		{
			break;
		}

		nst = calculate_stack_manipulation (smanip, &tmpspace);

		nmst = stack + MAX(tmpspace, nst);

		if (nmst > maxstack)
		{
			maxstack = nmst;
		}

		stack += nst;
	}

	cdn_stack_init (retstack, maxstack);

	g_slist_free (instructions);
	g_queue_clear (&queue);
}

/**
 * cdn_expression_tree_iter_swap_children:
 * @iter: the #CdnExpressionTreeIter
 * @first: the first child index
 * @second: the second child index
 *
 * Swap two children in the tree iter.
 *
 */
void
cdn_expression_tree_iter_swap_children (CdnExpressionTreeIter *iter,
                                        gint                   first,
                                        gint                   second)
{
	CdnExpressionTreeIter *tmp;

	if (first >= iter->num_children || first < 0)
	{
		g_warning ("Tree iter child index out of bounds: %d (max %d)", first, iter->num_children - 1);
		return;
	}

	if (second >= iter->num_children || second < 0)
	{
		g_warning ("Tree iter child index out of bounds: %d (max %d)", second, iter->num_children - 1);
		return;
	}

	tmp = iter->children[first];

	iter->children[first] = iter->children[second];
	iter->children[second] = tmp;
}
