/*
 * cpg-stack.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-stack.h"

#ifndef RTLINUX
void
cpg_stack_init (CpgStack  *stack,
                guint      size)
{
	stack->size = size;
	
	if (size)
		stack->output = g_new (gdouble, size);
	else
		stack->output = NULL;

	stack->output_ptr = stack->output;
}

CpgStack *
cpg_stack_new (guint size)
{
	CpgStack *ret = g_slice_new (CpgStack);
	cpg_stack_init (ret, size);
	
	return ret;
}

void
cpg_stack_destroy (CpgStack *stack)
{
	g_free (stack->output);
	
	stack->output = NULL;
	stack->output_ptr = NULL;
	stack->size = 0;
}

void
cpg_stack_free (CpgStack *stack)
{
	cpg_stack_destroy (stack);
	g_slice_free (CpgStack, stack);
}
#endif

guint
cpg_stack_size (CpgStack *stack)
{
	return stack->size;
}

guint
cpg_stack_count (CpgStack *stack)
{
	return stack->output_ptr >= stack->output ? stack->output_ptr - stack->output : 0;
}

void
cpg_stack_push (CpgStack  *stack,
                gdouble    value)
{
	*(stack->output_ptr++) = value;
}

gdouble
cpg_stack_pop (CpgStack *stack)
{
	return *(--stack->output_ptr);
}

void
cpg_stack_reset (CpgStack *stack)
{
	stack->output_ptr = stack->output;
}
