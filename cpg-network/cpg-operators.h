/*
 * cpg-operators.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_OPERATORS_H__
#define __CPG_OPERATORS_H__

#include <cpg-network/cpg-operator.h>

G_BEGIN_DECLS

void                 cpg_operators_register                   (GType            gtype);
void                 cpg_operators_unregister                 (GType            gtype);
GType                cpg_operators_find                       (gchar const     *name);
CpgOperatorClass    *cpg_operators_find_class                 (gchar const     *name);
CpgOperator         *cpg_operators_instantiate                (gchar const     *name,
                                                               GSList const    *expressions);
GSList const        *cpg_operators_list                       ();

G_END_DECLS

#endif /* __CPG_OPERATORS_H__ */

