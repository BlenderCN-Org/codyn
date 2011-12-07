/*
 * cdn-operators.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_OPERATORS_H__
#define __CDN_OPERATORS_H__

#include <codyn/operators/cdn-operator.h>
#include <codyn/operators/cdn-operator-diff.h>
#include <codyn/operators/cdn-operator-pdiff.h>
#include <codyn/operators/cdn-operator-df-dt.h>
#include <codyn/operators/cdn-operator-simplify.h>
#include <codyn/operators/cdn-operator-linsolve.h>
#include <codyn/operators/cdn-operator-delayed.h>

G_BEGIN_DECLS

void                 cdn_operators_register                   (GType            gtype);
void                 cdn_operators_unregister                 (GType            gtype);
GType                cdn_operators_find                       (gchar const     *name);
CdnOperatorClass    *cdn_operators_find_class                 (gchar const     *name);
CdnOperator         *cdn_operators_instantiate                (gchar const     *name,
                                                               GSList const   **expressions,
                                                               gint             num_expressions,
                                                               GSList const   **indices,
                                                               gint             num_indices,
                                                               gint             num_arguments,
                                                               gint            *argdim,
                                                               GError         **error);
GSList const        *cdn_operators_list                       ();

G_END_DECLS

#endif /* __CDN_OPERATORS_H__ */

