#ifndef __CPG_STACK_H__
#define __CPG_STACK_H__

#include <glib.h>

typedef struct _CpgStack
{
	gdouble *output_ptr;
	gdouble *output;
	guint size;
} CpgStack;

#ifndef RTLINUX
CpgStack *cpg_stack_new(guint size);
void cpg_stack_init(CpgStack *stack, guint size);
void cpg_stack_destroy(CpgStack *stack);
void cpg_stack_free(CpgStack *stack);
#endif

guint cpg_stack_size(CpgStack *stack);
guint cpg_stack_count(CpgStack *stack);

void cpg_stack_push(CpgStack *stack, gdouble value, void *data);
gdouble cpg_stack_pop(CpgStack *stack, void *data);

void cpg_stack_reset(CpgStack *stack);

#endif /* __CPG_STACK_H__ */

