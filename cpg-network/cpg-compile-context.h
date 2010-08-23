/*
 * cpg-compile-context.h
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

#ifndef __CPG_COMPILE_CONTEXT_H__
#define __CPG_COMPILE_CONTEXT_H__

#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

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

G_END_DECLS

#endif /* __CPG_COMPILE_CONTEXT_H__ */

