/*
 * cdn-utils.h
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

#ifndef __CDN_UTILS_H__
#define __CDN_UTILS_H__

#include <stdlib.h>
#include <glib-object.h>

#define CDN_FORWARD_DECL(id)    struct _##id
#define array_resize(Ptr, Type, Num) (Ptr = (Type *)realloc(Ptr, sizeof(Type) * (Num)))

gboolean cdn_signal_accumulator_false_handled (GSignalInvocationHint *ihint,
                                               GValue                *return_accu,
                                               const GValue          *handler_return,
                                               gpointer               dummy);


#endif /* __CDN_UTILS_H__ */
