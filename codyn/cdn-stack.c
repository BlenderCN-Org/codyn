/*
 * cdn-stack.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-stack-private.h"

/**
 * SECTION:cdn-stack
 * @short_description: Simple stack
 *
 * Very simple/efficient stack implementation for double numbers.
 *
 */

/**
 * cdn_stack_init:
 * @stack: A #CdnStack
 * @size: the stack size
 *
 * Initialize the stack to a certain size (useful when a stack is allocated
 * statically).
 *
 **/
void
cdn_stack_init (CdnStack  *stack,
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
 * cdn_stack_new:
 * @size: the stack size
 *
 * Create a new stack with the given size.
 *
 * Returns: A #CdnStack
 *
 **/
CdnStack *
cdn_stack_new (guint size)
{
	CdnStack *ret = g_slice_new (CdnStack);
	cdn_stack_init (ret, size);

	return ret;
}

/**
 * cdn_stack_destroy:
 * @stack: A #CdnStack
 *
 * Destroy the stack. This resizes the stack to 0. To free the whole stack, use
 * @cdn_stack_free.
 *
 **/
void
cdn_stack_destroy (CdnStack *stack)
{
	g_free (stack->output);

	stack->output = NULL;
	stack->output_ptr = NULL;
	stack->size = 0;
}

/**
 * cdn_stack_free:
 * @stack: A #CdnStack
 *
 * Free the stack.
 *
 **/
void
cdn_stack_free (CdnStack *stack)
{
	cdn_stack_destroy (stack);
	g_slice_free (CdnStack, stack);
}

/**
 * cdn_stack_size:
 * @stack: A #CdnStack
 *
 * Get the size of the stack. This is the maximum number of items that the
 * stack can hold. Use #cdn_stack_count to get the number of items currently
 * on the stack
 *
 * Returns: the stack size
 *
 **/
guint
cdn_stack_size (CdnStack *stack)
{
	return stack->size;
}

/**
 * cdn_stack_count:
 * @stack: A #CdnStack
 *
 * Count the number of items on the stack.
 *
 * Returns: the number of items on the stack
 *
 **/
guint
cdn_stack_count (CdnStack *stack)
{
	return stack->output_ptr >= stack->output ? stack->output_ptr - stack->output : 0;
}

/**
 * cdn_stack_push:
 * @stack: A #CdnStack
 * @value: the value
 *
 * Push a value on the stack. The stack will not be automatically resized, thus
 * be sure to know that you are not exceeding the stack size.
 *
 **/
void
cdn_stack_push (CdnStack  *stack,
                gdouble    value)
{
	*(stack->output_ptr++) = value;
}

/**
 * cdn_stack_pop:
 * @stack: A #CdnStack
 *
 * Pop a value of the stack. Note: this function does not check whether there
 * are still values on the stack, be sure to either know, or check the stack
 * yourself.
 *
 * Returns: the last value on the stack
 *
 **/
gdouble
cdn_stack_pop (CdnStack *stack)
{
	return *(--stack->output_ptr);
}

/**
 * cdn_stack_at:
 * @stack: A #CdnStack
 * @idx: The index
 *
 * Get a value from the stack at the specified index @idx.
 *
 * Returns: The stack value at index @idx
 *
 **/
gdouble
cdn_stack_at (CdnStack *stack,
              gint      idx)
{
	return stack->output[idx];
}

/**
 * cdn_stack_reset:
 * @stack: A #CdnStack
 *
 * Reset the stack.
 *
 **/
void
cdn_stack_reset (CdnStack *stack)
{
	stack->output_ptr = stack->output;
}
