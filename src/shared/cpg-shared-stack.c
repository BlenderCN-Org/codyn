#include "cpg-stack.h"
#include "cpg-shared-utils.h"

/* CHECK: is this actually correct? */
void
cpg_stack_push(CpgStack *stack, double value, void *base)
{
	*cpg_shared_pointer_base_type(base, stack->output_ptr, double) = value;
	stack->output_ptr = (double *)((unsigned)stack->output_ptr + sizeof(double));
}

/* CHECK: is this actually correct? */
double
cpg_stack_pop(CpgStack *stack, void *base)
{
	double ret = *cpg_shared_pointer_base_type(base, stack->output_ptr, double);
	stack->output_ptr = (double *)((unsigned)stack->output_ptr - sizeof(double));

	return ret;
}

unsigned
cpg_stack_count(CpgStack *stack)
{
	return stack->output_ptr >= stack->output ? (stack->output_ptr - stack->output) / sizeof(double) : 0;
}
