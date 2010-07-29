#ifndef __CPG_UTILS_H__
#define __CPG_UTILS_H__

#include <stdlib.h>
#include <glib-object.h>

#define CPG_FORWARD_DECL(id)    struct _##id
#define array_resize(Ptr, Type, Num) (Ptr = (Type *)realloc(Ptr, sizeof(Type) * (Num)))

gboolean cpg_signal_accumulator_false_handled (GSignalInvocationHint *ihint,
                                               GValue                *return_accu,
                                               const GValue          *handler_return,
                                               gpointer               dummy);


#endif /* __CPG_UTILS_H__ */
