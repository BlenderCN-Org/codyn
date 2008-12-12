#ifndef __CPG_STACK_H__
#define __CPG_STACK_H__

typedef struct _CpgStack
{
	double *output_ptr;
	double *output;
	unsigned size;
} CpgStack;

#ifndef RTLINUX
CpgStack *cpg_stack_new(unsigned size);
void cpg_stack_init(CpgStack *stack, unsigned size);
void cpg_stack_destroy(CpgStack *stack);
void cpg_stack_free(CpgStack *stack);
#endif

unsigned cpg_stack_size(CpgStack *stack);
unsigned cpg_stack_count(CpgStack *stack);

void cpg_stack_push(CpgStack *stack, double value, void *data);
double cpg_stack_pop(CpgStack *stack, void *data);

void cpg_stack_reset(CpgStack *stack);

#endif /* __CPG_STACK_H__ */

