#ifndef __CPG_COMPILE_CONTEXT_H__
#define __CPG_COMPILE_CONTEXT_H__

#include "cpg-ref-counted.h"

struct _CpgObject;
struct _CpgProperty;
struct _CpgFunction;

typedef struct _CpgCompileContext CpgCompileContext;

CpgCompileContext *cpg_compile_context_new (void);
GType cpg_compile_context_get_type (void);

void cpg_compile_context_prepend_object (CpgCompileContext *context,
                                         struct _CpgObject *object);

void cpg_compile_context_save (CpgCompileContext *context);
void cpg_compile_context_restore (CpgCompileContext *context);

void cpg_compile_context_append_object (CpgCompileContext *context,
                                        struct _CpgObject *object);

void cpg_compile_context_set_functions (CpgCompileContext *context,
                                        GSList            *functions);

struct _CpgProperty *cpg_compile_context_lookup_property (CpgCompileContext *context,
                                                          gchar const       *name);

struct _CpgFunction *cpg_compile_context_lookup_function (CpgCompileContext *context,
                                                          gchar const       *name);

GSList *cpg_compile_context_get_objects (CpgCompileContext *context);
GSList *cpg_compile_context_get_functions (CpgCompileContext *context);

#endif /* __CPG_COMPILE_CONTEXT_H__ */

