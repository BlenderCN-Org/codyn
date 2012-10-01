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

#ifdef __GI_SCANNER__

typedef struct
{
	gint32 rows;
	gint32 columns;
} CdnDimension;

typedef struct
{
	CdnDimension dimension;

	guint *sparsity;
	guint num_sparse;
} CdnStackArg;

#else

typedef struct
{
	union
	{
		struct
		{
			gint32 rows;
			gint32 columns;
		};

		gint32 dims[2];
	};
} CdnDimension;

typedef struct
{
	union
	{
		CdnDimension dimension;

		struct
		{
			gint32 rows;
			gint32 columns;
		};
	};

	guint *sparsity;
	guint num_sparse;
} CdnStackArg;
#endif

typedef struct
{
	gint num;
	CdnStackArg *args;
} CdnStackArgs;

typedef struct
{
	CdnStackArgs pop;
	CdnStackArg push;

	gint extra_space;
} CdnStackManipulation;

#define CDN_DIMENSION(r, c) {{{.rows = r, .columns = (c)}}}
#define CDN_STACK_ARG(r, c) {{.dimension = CDN_DIMENSION (r, c)}, .sparsity = NULL, .num_sparse = 0}
#define CDN_STACK_ARG_EMPTY CDN_STACK_ARG(0, 0)

extern CdnDimension cdn_dimension_one;
extern CdnDimension *cdn_dimension_onep;

GType     cdn_stack_get_type ();
GType     cdn_stack_manipulation_get_type ();
GType     cdn_stack_arg_get_type ();
GType     cdn_stack_args_get_type ();
GType     cdn_dimension_get_type ();

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

CdnStackArg const *
          cdn_stack_manipulation_get_popn (CdnStackManipulation const *smanip,
                                           gint                        n);

CdnStackArgs const *
          cdn_stack_manipulation_get_pop (CdnStackManipulation const *smanip);

CdnStackArg const *
          cdn_stack_manipulation_get_push (CdnStackManipulation const *smanip);

void      cdn_stack_arg_copy           (CdnStackArg       *ret,
                                        CdnStackArg const *src);
void      cdn_stack_arg_destroy        (CdnStackArg       *arg);

guint     cdn_stack_arg_size              (CdnStackArg const *arg);

gboolean  cdn_stack_arg_is_sparse (CdnStackArg const *arg,
                                   guint              idx);

void      cdn_stack_arg_set_sparsity (CdnStackArg *arg,
                                      guint       *sparsity,
                                      guint        num_sparse);

guint const *cdn_stack_arg_get_sparsity (CdnStackArg *arg,
                                         guint       *num_sparse);

void      cdn_stack_arg_set_sparsity_one (CdnStackArg *arg,
                                          guint        sparsity);

void      cdn_stack_manipulation_destroy (CdnStackManipulation *smanip);

void      cdn_stack_manipulation_copy    (CdnStackManipulation       *dest,
                                          CdnStackManipulation const *src);

void      cdn_stack_args_init (CdnStackArgs *args, gint num);

CdnStackArgs *cdn_stack_args_new (gint num);

void      cdn_stack_args_copy (CdnStackArgs       *dest,
                               CdnStackArgs const *src);

guint     cdn_stack_args_get_num (CdnStackArgs const *args);

void      cdn_stack_args_destroy (CdnStackArgs       *args);
void      cdn_stack_args_append (CdnStackArgs       *args,
                                 CdnStackArg const  *arg);

gboolean  cdn_dimension_is_one (CdnDimension const *dim);
gint      cdn_dimension_size   (CdnDimension const *dim);

gboolean  cdn_dimension_equal (CdnDimension const *dim,
                               CdnDimension const *other);

G_END_DECLS

#endif /* __CDN_STACK_H__ */

