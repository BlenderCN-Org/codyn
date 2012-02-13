/*
 * cdn-stack.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_STACK_H__
#define __CDN_STACK_H__

#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _CdnStack CdnStack;

typedef struct
{
	gint num_pop;
	gint *pop_dims;

	gint num_push;
	gint *push_dims;

	gint extra_space;
} CdnStackManipulation;

GType     cdn_stack_get_type ();
GType     cdn_stack_manipulation_get_type ();

CdnStack *cdn_stack_new     (guint     size);
CdnStack *cdn_stack_copy    (CdnStack *stack);

void      cdn_stack_init    (CdnStack *stack,
                             guint     size);

void      cdn_stack_destroy (CdnStack *stack);
void      cdn_stack_free    (CdnStack *stack);
guint     cdn_stack_size    (CdnStack *stack);
guint     cdn_stack_count   (CdnStack *stack);
void      cdn_stack_push    (CdnStack *stack,
                             gdouble   value);

void      cdn_stack_pushn   (CdnStack *stack,
                             gdouble const *values,
                             gint      num);

void      cdn_stack_pushni  (CdnStack *stack,
                             gdouble   value,
                             gint      num);

void      cdn_stack_resize  (CdnStack *stack,
                             guint     size);

gdouble   cdn_stack_pop     (CdnStack *stack);
gdouble  *cdn_stack_popn    (CdnStack *stack,
                             gint      num);

gdouble   cdn_stack_peek    (CdnStack *stack);
void      cdn_stack_set     (CdnStack *stack,
                             gdouble   value);

void      cdn_stack_reset   (CdnStack *stack);

gdouble   cdn_stack_at      (CdnStack *stack,
                             gint      idx);
gdouble  *cdn_stack_ptr     (CdnStack *stack);

gdouble  *cdn_stack_output_ptr     (CdnStack *stack);
void      cdn_stack_set_output_ptr     (CdnStack *stack,
                                        gdouble  *ptr);

void      cdn_stack_set_at  (CdnStack *stack,
                             gint      idx,
                             gdouble   value);

G_END_DECLS

#endif /* __CDN_STACK_H__ */

