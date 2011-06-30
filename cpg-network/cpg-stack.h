/*
 * cpg-stack.h
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

gdouble   cpg_stack_at      (CpgStack *stack,
                             gint      idx);

G_END_DECLS

#endif /* __CPG_STACK_H__ */

