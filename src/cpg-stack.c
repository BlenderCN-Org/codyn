#include "cpg-stack.h"
#include "cpg-utils.h"

#ifndef RTLINUX
void
cpg_stack_init(CpgStack *stack, unsigned size)
{
	stack->size = size;
	
	if (size)
		stack->output = cpg_new(double, size);
	else
		stack->output = NULL;

	stack->output_ptr = stack->output;
}

CpgStack *
cpg_stack_new(unsigned size)
{
	CpgStack *ret = cpg_new1(CpgStack);
	cpg_stack_init(ret, size);
	
	return ret;
}

void
cpg_stack_destroy(CpgStack *stack)
{
	if (stack->output)
		cpg_free(stack->output);
	
	stack->output = NULL;
	stack->output_ptr = NULL;
	stack->size = 0;
}

void
cpg_stack_free(CpgStack *stack)
{
	cpg_stack_destroy(stack);
	cpg_free(stack);
}
#endif

unsigned
cpg_stack_size(CpgStack *stack)
{
	return stack->size;
}

unsigned
cpg_stack_count(CpgStack *stack)
{
	return stack->output_ptr >= stack->output ? stack->output_ptr - stack->output : 0;
}

void
cpg_stack_push(CpgStack *stack, double value, void *data)
{
	*(stack->output_ptr++) = value;
}

double
cpg_stack_pop(CpgStack *stack, void *data)
{
	return *(--stack->output_ptr);
}

void
cpg_stack_reset(CpgStack *stack)
{
	stack->output_ptr = stack->output;
}
