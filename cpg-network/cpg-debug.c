/*
 * cpg-debug.c
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

#include "cpg-debug.h"

#ifndef RTLINUX
#include <stdarg.h>
#include <stdio.h>
#endif

char const *debug_types[] =
{
	"NONE",
	"ERROR",
	"EXPRESSION",
	"ERROR, EXPRESSION",
	"EVALUATION",
	"ERROR, EVALUATION",
	"EXPRESSION, EVALUATION",
	"ERROR, EXPRESSION, EVALUATION"
};

static int debug_type = CPG_DEBUG_TYPE_ERROR;

/**
 * cpg_debug_message_function:
 * @type: a #CpgDebugType
 *
 * Add a debug type
 *
 **/
void
cpg_debug_add (CpgDebugType type)
{
	debug_type |= type;
}

/**
 * cpg_debug_message_function:
 * @type: a #CpgDebugType
 * @function: the function
 * @format: the format
 *
 * Write a debug message
 *
 **/
void
cpg_debug_message_function (CpgDebugType  type, 
                            char const   *function, 
                            char const   *format, 
                            ...)
{
	if (!(type & debug_type))
		return;

	va_list ap;
	va_start (ap, format);

	fprintf (stderr, " ** DEBUG %s in %s: ", debug_types[type], function);
	vfprintf (stderr, format, ap);
	fprintf (stderr, "\n");
		
	va_end (ap);
}
