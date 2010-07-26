#include "cpg-stack-private.h"

/**
 * SECTION:cpg-stack
 * @short_description: Simple stack
 *
 * Very simple/efficient stack implementation for double numbers.
 *
 */

/**
 * cpg_stack_init:
 * @stack: A #CpgStack
 * @size: the stack size
 *
 * Initialize the stack to a certain size (useful when a stack is allocated
 * statically).
 *
 **/
void
cpg_stack_init (CpgStack  *stack,
                guint      size)
{
	stack->size = size;

	if (size)
	{
		stack->output = g_new (gdouble, size);
	}
	else
	{
		stack->output = NULL;
	}

	stack->output_ptr = stack->output;
}

/**
 * cpg_stack_new:
 * @size: the stack size
 *
 * Create a new stack with the given size.
 *
 * Returns: A #CpgStack
 *
 **/
CpgStack *
cpg_stack_new (guint size)
{
	CpgStack *ret = g_slice_new (CpgStack);
	cpg_stack_init (ret, size);

	return ret;
}

/**
 * cpg_stack_destroy:
 * @stack: A #CpgStack
 *
 * Destroy the stack. This resizes the stack to 0. To free the whole stack, use
 * @cpg_stack_free.
 *
 **/
void
cpg_stack_destroy (CpgStack *stack)
{
	g_free (stack->output);

	stack->output = NULL;
	stack->output_ptr = NULL;
	stack->size = 0;
}

/**
 * cpg_stack_free:
 * @stack: A #CpgStack
 *
 * Free the stack.
 *
 **/
void
cpg_stack_free (CpgStack *stack)
{
	cpg_stack_destroy (stack);
	g_slice_free (CpgStack, stack);
}

/**
 * cpg_stack_size:
 * @stack: A #CpgStack
 *
 * Get the size of the stack. This is the maximum number of items that the
 * stack can hold. Use #cpg_stack_count to get the number of items currently
 * on the stack
 *
 * Returns: the stack size
 *
 **/
guint
cpg_stack_size (CpgStack *stack)
{
	return stack->size;
}

/**
 * cpg_stack_count:
 * @stack: A #CpgStack
 *
 * Count the number of items on the stack.
 *
 * Returns: the number of items on the stack
 *
 **/
guint
cpg_stack_count (CpgStack *stack)
{
	return stack->output_ptr >= stack->output ? stack->output_ptr - stack->output : 0;
}

/**
 * cpg_stack_push:
 * @stack: A #CpgStack
 * @value: the value
 *
 * Push a value on the stack. The stack will not be automatically resized, thus
 * be sure to know that you are not exceeding the stack size.
 *
 **/
void
cpg_stack_push (CpgStack  *stack,
                gdouble    value)
{
	*(stack->output_ptr++) = value;
}

/**
 * cpg_stack_pop:
 * @stack: A #CpgStack
 *
 * Pop a value of the stack. Note: this function does not check whether there
 * are still values on the stack, be sure to either know, or check the stack
 * yourself.
 *
 * Returns: the last value on the stack
 *
 **/
gdouble
cpg_stack_pop (CpgStack *stack)
{
	return *(--stack->output_ptr);
}

/**
 * cpg_stack_at:
 * @stack: A #CpgStack
 * @idx: The index
 *
 * Get a value from the stack at the specified index @idx.
 *
 * Returns: The stack value at index @idx
 *
 **/
gdouble
cpg_stack_at (CpgStack *stack,
              gint      idx)
{
	return stack->output[idx];
}

/**
 * cpg_stack_reset:
 * @stack: A #CpgStack
 *
 * Reset the stack.
 *
 **/
void
cpg_stack_reset (CpgStack *stack)
{
	stack->output_ptr = stack->output;
}
