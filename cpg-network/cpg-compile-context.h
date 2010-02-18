#ifndef __CPG_COMPILE_CONTEXT_H__
#define __CPG_COMPILE_CONTEXT_H__

#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-utils.h>

CPG_FORWARD_DECL (CpgObject);
CPG_FORWARD_DECL (CpgProperty);
CPG_FORWARD_DECL (CpgFunction);

typedef struct _CpgCompileContext CpgCompileContext;

CpgCompileContext *cpg_compile_context_new (void);
GType cpg_compile_context_get_type (void);

void cpg_compile_context_prepend_object (CpgCompileContext *context,
                                         CPG_FORWARD_DECL (CpgObject) *object);

void cpg_compile_context_save (CpgCompileContext *context);
void cpg_compile_context_restore (CpgCompileContext *context);

void cpg_compile_context_append_object (CpgCompileContext *context,
                                        CPG_FORWARD_DECL (CpgObject) *object);

void cpg_compile_context_set_functions (CpgCompileContext *context,
                                        GSList            *functions);

CPG_FORWARD_DECL (CpgProperty) *cpg_compile_context_lookup_property (CpgCompileContext *context,
                                                          gchar const       *name);

CPG_FORWARD_DECL (CpgFunction) *cpg_compile_context_lookup_function (CpgCompileContext *context,
                                                          gchar const       *name);

GSList *cpg_compile_context_get_objects (CpgCompileContext *context);
GSList *cpg_compile_context_get_functions (CpgCompileContext *context);

#endif /* __CPG_COMPILE_CONTEXT_H__ */

