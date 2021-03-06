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
#include <string.h>
#include <stdlib.h>

CdnDimension cdn_dimension_one = {{.dims = {1, 1}}};
CdnDimension *cdn_dimension_onep = &cdn_dimension_one;

/**
 * CdnDimension:
 *
 * A dimension
 *
 * #CdnDimension represents a rows-by-columns dimension.
 *
 */

/**
 * CdnStackArg:
 *
 * A stack argument.
 *
 * #CdnStackArg represents a single argument on the stack. It carries
 * information about the dimensionality of the argument.
 */

/**
 * CdnStackArgs:
 *
 * List of stack arguments
 *
 * #CdnStackArgs contains a list of #CdnStackArg.
 *
 */

/**
 * CdnStackManipulation:
 *
 * Stack manipulation information
 *
 * #CdnStackManipulation contains information on how a particular instruction
 * or function manipulates the stack. It encodes both how many elements are popped
 * as well as pushed onto the stack when executed.
 *
 */

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_stack_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnStack",
		                                      (GBoxedCopyFunc)cdn_stack_copy,
		                                      (GBoxedFreeFunc)cdn_stack_free);
	}

	return gtype;
}

/**
 * cdn_stack_arg_destroy:
 * @arg: the #CdnStackArg
 *
 * Destroy a stack allocated stack arg.
 */
void
cdn_stack_arg_destroy (CdnStackArg *arg)
{
	if (arg == NULL)
	{
		return;
	}
}

static CdnStackArg *
_cdn_stack_arg_copy (CdnStackArg const *arg)
{
	CdnStackArg *ret;

	ret = g_slice_new0 (CdnStackArg);
	cdn_stack_arg_copy (ret, arg);

	return ret;
}

static void
_cdn_stack_arg_free (CdnStackArg *arg)
{
	cdn_stack_arg_destroy (arg);
	g_slice_free (CdnStackArg, arg);
}

static CdnStackArgs *
_cdn_stack_args_copy (CdnStackArgs const *args)
{
	CdnStackArgs *ret;

	ret = g_slice_new0 (CdnStackArgs);
	cdn_stack_args_copy (ret, args);

	return ret;
}

static void
_cdn_stack_args_free (CdnStackArgs *args)
{
	cdn_stack_args_destroy (args);
	g_slice_free (CdnStackArgs, args);
}

static CdnDimension *
_cdn_dimension_copy (CdnDimension const *dim)
{
	CdnDimension *ret;

	ret = g_slice_new0 (CdnDimension);
	*ret = *dim;

	return ret;
}

static void
_cdn_dimension_free (CdnDimension *dim)
{
	g_slice_free (CdnDimension, dim);
}

static CdnStackManipulation *
_cdn_stack_manipulation_copy (CdnStackManipulation const *smanip)
{
	CdnStackManipulation *ret;

	ret = g_slice_new0 (CdnStackManipulation);
	cdn_stack_manipulation_copy (ret, smanip);

	return ret;
}

static void
_cdn_stack_manipulation_free (CdnStackManipulation *smanip)
{
	cdn_stack_manipulation_destroy (smanip);

	g_slice_free (CdnStackManipulation, smanip);
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_stack_manipulation_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnStackManipulation",
		                                      (GBoxedCopyFunc)_cdn_stack_manipulation_copy,
		                                      (GBoxedFreeFunc)_cdn_stack_manipulation_free);
	}

	return gtype;
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_stack_arg_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnStackArg",
		                                      (GBoxedCopyFunc)_cdn_stack_arg_copy,
		                                      (GBoxedFreeFunc)_cdn_stack_arg_free);
	}

	return gtype;
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_stack_args_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnStackArgs",
		                                      (GBoxedCopyFunc)_cdn_stack_args_copy,
		                                      (GBoxedFreeFunc)_cdn_stack_args_free);
	}

	return gtype;
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_dimension_get_type ()
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnDimension",
		                                      (GBoxedCopyFunc)_cdn_dimension_copy,
		                                      (GBoxedFreeFunc)_cdn_dimension_free);
	}

	return gtype;
}

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
 * cdn_stack_copy:
 * @stack: A #CdnStack
 *
 * Create a copy of the stack.
 *
 * Returns: A #CdnStack
 *
 **/
CdnStack *
cdn_stack_copy (CdnStack *stack)
{
	CdnStack *ret = g_slice_new (CdnStack);
	cdn_stack_init (ret, stack->size);

	memcpy (ret->output,
	        stack->output,
	        sizeof (gdouble) * stack->size);

	ret->output_ptr = ret->output + (stack->output_ptr - stack->output);

	return ret;
}

/**
 * cdn_stack_resize:
 * @stack: the #CdnStack
 * @size: the new size
 *
 * Resize the stack to have a capacity of @size. Note that this destroys
 * the contents of the stack if the new size is different from the old
 * size of the stack.
 */
void
cdn_stack_resize (CdnStack *stack,
                  guint     size)
{
	if (size != stack->size)
	{
		stack->output = g_renew (gdouble, stack->output, size);
		memset(stack->output, 0, size);
		stack->size = size;
	}

	cdn_stack_reset (stack);
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
 * cdn_stack_popn:
 * @stack: the #CdnStack
 * @num: the number of elements to pop
 *
 * Pop a number of values from the stack at once. Note: this function does not
 * check whether there are @num values to pop in the first place. Be sure to either
 * know, or check the stack yourself.
 *
 * Returns: the last value on the stack
 *
 */
gdouble *
cdn_stack_popn (CdnStack *stack,
                gint      num)
{
	stack->output_ptr -= num;
	return stack->output_ptr;
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
 * cdn_stack_peek:
 * @stack: the #CdnStack
 *
 * Peek at the last value on the stack. Note that this does not check whether the
 * stack actually contains a value. It is up to the caller to make sure a value
 * exists before peeking.
 *
 * Returns: the last value on the stack.
 *
 */
gdouble
cdn_stack_peek (CdnStack *stack)
{
	return *(stack->output_ptr - 1);
}

/**
 * cdn_stack_set:
 * @stack: the #CdnStack
 * @value: a value
 *
 * Set the last value on the stack to a value. Note that this does not check whether the
 * stack actually contains a value. It is up to the caller to make sure a value
 * exists before setting its value.
 *
 */
void
cdn_stack_set (CdnStack *stack,
               gdouble   value)
{
	*(stack->output_ptr - 1) = value;
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

/**
 * cdn_stack_set_at:
 * @stack: the #CdnStack
 * @idx: the index
 * @value: a value
 *
 * Set the value if the specified indexed item (from the start!) on the stack to a value.
 * Note that this does not check whether @idx is a valid location on the stack.
 *
 */
void
cdn_stack_set_at (CdnStack *stack,
                  gint      idx,
                  gdouble   value)
{
	stack->output[idx] = value;
}

/**
 * cdn_stack_ptr:
 * @stack: the #CdnStack
 *
 * Get the stack pointer pointing to the start of the stack.
 *
 * Returns: (transfer none): the stack pointer
 *
 */
gdouble *
cdn_stack_ptr (CdnStack *stack)
{
	return stack->output;
}

/**
 * cdn_stack_output_ptr:
 * @stack: the #CdnStack
 *
 * Get the stack pointer pointing to the last value of the stack.
 *
 * Returns: (transfer none): the stack pointer
 *
 */
gdouble *
cdn_stack_output_ptr (CdnStack *stack)
{
	return stack->output_ptr;
}

/**
 * cdn_stack_set_output_ptr:
 * @stack: the #CdnStack
 * @ptr: the new output pointer
 *
 * Set the output pointer (i.e. the position of the stack) of the stack.
 */
void
cdn_stack_set_output_ptr (CdnStack *stack,
                          gdouble  *ptr)
{
	if (ptr >= stack->output &&
	    ptr <= stack->output + stack->size)
	{
		stack->output_ptr = ptr;
	}
}

/**
 * cdn_stack_pushn:
 * @stack: the #CdnStack
 * @values: (array length=num): the values to push
 * @num: the number of values
 *
 * Push one or more values on the stack.
 */
void
cdn_stack_pushn (CdnStack      *stack,
                 gdouble const *values,
                 gint           num)
{
	memcpy (stack->output_ptr, values, sizeof (gdouble) * num);
	stack->output_ptr += num;
}

/**
 * cdn_stack_pushni:
 * @stack: the #CdnStack
 * @value: the value to push
 * @num: the number of times to push the value
 *
 * Push the same value one or more times on the stack.
 */
void
cdn_stack_pushni (CdnStack *stack,
                  gdouble   value,
                  gint      num)
{
	while (num-- > 0)
	{
		*stack->output_ptr++ = value;
	}
}

/**
 * cdn_stack_manipulation_get_popn:
 * @smanip: a #CdnStackManipulation.
 * @n: the popped argument index.
 *
 * Get the dimension of a particular popped argument.
 *
 **/
CdnStackArg const *
cdn_stack_manipulation_get_popn (CdnStackManipulation const *smanip,
                                 gint                        n)
{
	if (!smanip || smanip->pop.num <= n)
	{
		return NULL;
	}

	return &smanip->pop.args[n];
}

/**
 * cdn_stack_manipulation_get_pop:
 * @smanip: a #CdnStackManipulation.
 *
 * Get the dimension of a particular popped argument.
 *
 **/
CdnStackArgs const *
cdn_stack_manipulation_get_pop (CdnStackManipulation const *smanip)
{
	if (!smanip)
	{
		return NULL;
	}

	return &smanip->pop;
}

/**
 * cdn_stack_args_get_num:
 * @args: the #CdnStackArgs
 *
 * Get the number of arguments in @args.
 */
guint
cdn_stack_args_get_num (CdnStackArgs const *args)
{
	return args ? args->num : 0;
}

/**
 * cdn_stack_manipulation_get_push:
 * @smanip: a #CdnStackManipulation.
 *
 * Get the dimension of the pushed element.
 *
 **/
CdnStackArg const *
cdn_stack_manipulation_get_push (CdnStackManipulation const *smanip)
{
	if (!smanip)
	{
		return NULL;
	}

	return &smanip->push;
}

/**
 * cdn_stack_arg_copy:
 * @ret: (out): the return value of the copied stack arg
 * @src: the source stack arg to copy
 *
 * Copy the stack arg @src into the stack arg @ret.
 *
 */
void
cdn_stack_arg_copy (CdnStackArg       *ret,
                    CdnStackArg const *src)
{
	ret->rows = src->rows;
	ret->columns = src->columns;
}

/**
 * cdn_stack_arg_size:
 * @arg: the #CdnStackArg
 *
 * Get the size (total number of elements) of @arg.
 */
guint
cdn_stack_arg_size (CdnStackArg const *arg)
{
	return arg ? arg->rows * arg->columns : 1;
}

/**
 * cdn_stack_args_destroy:
 * @args: the #CdnStackArgs
 *
 * Destroy the contents of the provided stack args. Note that this does
 * not free the memory of @args itself.
 */
void
cdn_stack_args_destroy (CdnStackArgs *args)
{
	gint i;

	if (args == NULL)
	{
		return;
	}

	for (i = 0; i < args->num; ++i)
	{
		cdn_stack_arg_destroy (&args->args[i]);
	}

	g_free (args->args);

	args->args = NULL;
	args->num = 0;
}

/**
 * cdn_stack_manipulation_destroy:
 * @smanip: the #CdnStackManipulation
 *
 * Destroy the contents of the provided stack manipulation. Note that this does
 * not free the memory of @smanip itself.
 */
void
cdn_stack_manipulation_destroy (CdnStackManipulation *smanip)
{
	if (smanip == NULL)
	{
		return;
	}

	cdn_stack_args_destroy (&smanip->pop);
	cdn_stack_arg_destroy (&smanip->push);
}

/**
 * cdn_stack_args_new:
 * @num: the number of arguments
 *
 * Create a new #CdnStackArgs and allocate @num arguments. The arguments
 * initially have dimension 1-by-1. It is also possible to allocate
 * #CdnStackArgs on the stack to avoid allocations. To do so, please
 * use #cdn_stack_args_init to initialize the stack allocated #CdnStackArgs
 * appropriately.
 *
 * Returns: (transfer full): a new #CdnStackArgs
 */
CdnStackArgs *
cdn_stack_args_new (gint num)
{
	CdnStackArgs *ret;
	gint i;

	ret = g_slice_new (CdnStackArgs);
	cdn_stack_args_init (ret, num);

	for (i = 0; i < num; ++i)
	{
		ret->args[i].rows = 1;
		ret->args[i].columns = 1;
	}

	return ret;
}

/**
 * cdn_stack_args_init:
 * @args: the #CdnStackArgs
 * @num: the number of arguments
 *
 * Initializes @args to contain @num arguments. Note that the dimensionality
 * of the arguments is initially unspecified and should be set accordingly.
 */
void
cdn_stack_args_init (CdnStackArgs *args,
                     gint          num)
{
	args->num = num;
	args->args = g_new0 (CdnStackArg, num);
}

/**
 * cdn_stack_args_append:
 * @args: the #CdnStackArgs
 * @arg: the #CdnStackArg to add
 *
 * Append @arg to the arguments of @args.
 */
void
cdn_stack_args_append (CdnStackArgs      *args,
                       CdnStackArg const *arg)
{
	++args->num;

	if (args->num == 1)
	{
		args->args = g_new0 (CdnStackArg, 1);
	}
	else
	{
		CdnStackArg empty = CDN_STACK_ARG_EMPTY;

		args->args = g_renew (CdnStackArg, args->args, args->num);
		args->args[args->num - 1] = empty;
	}

	cdn_stack_arg_copy (&args->args[args->num - 1], arg);
}

/**
 * cdn_stack_args_copy:
 * @dest: (out): the destination #CdnStackArgs
 * @src: the source #CdnStackArgs
 *
 * Copy the stack args from @src to @dest.
 */
void
cdn_stack_args_copy (CdnStackArgs       *dest,
                     CdnStackArgs const *src)
{
	gint i;

	for (i = 0; i < dest->num; ++i)
	{
		cdn_stack_arg_destroy (&dest->args[i]);
	}

	g_free (dest->args);

	dest->num = src->num;
	dest->args = g_memdup (src->args, sizeof (CdnStackArg) * dest->num);
}

/**
 * cdn_stack_manipulation_copy:
 * @dest: (out): the destination #CdnStackManipulation
 * @src: the source #CdnStackManipulation
 *
 * Copy the stack manipulation from @src to @dest.
 */
void
cdn_stack_manipulation_copy (CdnStackManipulation       *dest,
                             CdnStackManipulation const *src)
{
	if (dest == src || dest == NULL || src == NULL)
	{
		return;
	}

	cdn_stack_manipulation_destroy (dest);

	cdn_stack_arg_copy (&dest->push, &src->push);
	cdn_stack_args_copy (&dest->pop, &src->pop);

	dest->extra_space = src->extra_space;
}

/**
 * cdn_dimension_is_one:
 * @dim: a #CdnDimension.
 *
 * Check if the dimension is 1 x 1.
 *
 * Returns: %TRUE if the dimension is single, %FALSE otherwise.
 *
 **/
gboolean
cdn_dimension_is_one (CdnDimension const *dim)
{
	if (!dim)
	{
		return TRUE;
	}

	return dim->rows <= 1 && dim->columns <= 1;
}

/**
 * cdn_dimension_equal:
 * @dim: a #CdnDimension.
 * @other: a #CdnDimension.
 *
 * Compare two dimensions for equality.
 *
 * Returns: %TRUE if the dimensions are equal, %FALSE otherwise.
 *
 **/
gboolean
cdn_dimension_equal (CdnDimension const *dim,
                     CdnDimension const *other)
{
	if (!dim || !other)
	{
		return FALSE;
	}

	return dim->rows == other->rows && dim->columns == other->columns;
}

/**
 * cdn_dimension_size:
 * @dim: a #CdnDimension.
 *
 * Get the size of a dimension (i.e. row x columns).
 *
 * Returns: the dimension size.
 *
 **/
gint
cdn_dimension_size (CdnDimension const *dim)
{
	if (!dim)
	{
		return 1;
	}

	return dim->rows * dim->columns;
}

/**
 * cdn_stack_arg_get_dimension:
 * @arg: a #CdnStackArg.
 * @dim: (out): a #CdnDimension return value.
 *
 * Get the dimension of the stack argument.
 *
 **/
void
cdn_stack_arg_get_dimension (CdnStackArg const *arg,
                             CdnDimension      *dim)
{
	if (!dim || !arg)
	{
		return;
	}

	*dim = arg->dimension;
}

