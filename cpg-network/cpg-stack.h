#ifndef __CPG_STACK_H__
#define __CPG_STACK_H__

#include <glib.h>

G_BEGIN_DECLS

typedef struct _CpgStack CpgStack;

CpgStack *cpg_stack_new     (guint     size);
void      cpg_stack_init    (CpgStack *stack,
                             guint     size);

void      cpg_stack_destroy (CpgStack *stack);
void      cpg_stack_free    (CpgStack *stack);
guint     cpg_stack_size    (CpgStack *stack);
guint     cpg_stack_count   (CpgStack *stack);
void      cpg_stack_push    (CpgStack *stack,
                             gdouble   value);

gdouble   cpg_stack_pop     (CpgStack *stack);
void      cpg_stack_reset   (CpgStack *stack);

G_END_DECLS

#endif /* __CPG_STACK_H__ */

