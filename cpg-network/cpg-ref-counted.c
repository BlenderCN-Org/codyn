/*
 * cpg-ref-counted.c
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

#include "cpg-ref-counted-private.h"

/**
 * SECTION:ref-counted
 * @short_description: A lightweight reference counted object
 *
 * #CpgRefCounted is a very lightweight implementation of a reference counted
 * object, similar to GObject but without all the additional functionality.
 *
 */
 
/**
 * cpg_ref_counted_init:
 * @ref_counted: a #CpgRefCounted
 * @destroy_func: callback to destroy the ref counted object
 *
 * Use this after a new ref counted object has been constructed. This
 * sets the callback to be used when the ref count decreases to 0. The function
 * sets the initial ref count to 1
 *
 **/
void
cpg_ref_counted_init (gpointer       ref_counted,
                      GDestroyNotify destroy_func)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;

	r->ref_count = 1;
	r->destroy_func = destroy_func;
}

/**
 * cpg_ref_counted_ref:
 * @ref_counted: a #CpgRefCounted
 *
 * Increase the ref count on @ref_counted.
 *
 * Returns: @ref_counted
 *
 **/
gpointer
cpg_ref_counted_ref (gpointer ref_counted)
{
	if (!ref_counted)
		return ref_counted;

	++(((CpgRefCounted *)ref_counted)->ref_count);
	return ref_counted;
}

/**
 * cpg_ref_counted_unref:
 * @ref_counted: a #CpgRefCounted
 *
 * Decrease the ref count on @ref_counted.
 *
 **/
void 
cpg_ref_counted_unref (gpointer ref_counted)
{
	CpgRefCounted *r = (CpgRefCounted *)ref_counted;
	
	if (!ref_counted || r->ref_count == 0)
		return;

	if (--r->ref_count == 0)
	{
		if (r->destroy_func)
			r->destroy_func (ref_counted);
	}
}

