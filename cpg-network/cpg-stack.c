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
