/*
 * cpg-compile-error.h
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

#ifndef __CPG_COMPILE_ERROR_H__
#define __CPG_COMPILE_ERROR_H__

#include <glib-object.h>
#include <cpg-network/cpg-ref-counted.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-link.h>

G_BEGIN_DECLS

#define CPG_TYPE_COMPILE_ERROR (cpg_compile_error_get_type ())
#define CPG_COMPILE_ERROR_TYPE (cpg_compile_error_type_quark ())

/**
 * CpgCompileErrorCode:
 * @CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND: property not found
 * @CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND: function not found
 * @CPG_COMPILE_ERROR_INVALID_TOKEN: invalid token
 * @CPG_COMPILE_ERROR_MAXARG: maximum number of arguments exceeded
 * @CPG_COMPILE_ERROR_INVALID_STACK: invalid stack produced
 * @CPG_COMPILE_ERROR_NUM_ERRORS: num errors
 *
 * Enum used to indicate the type of compile error
 *
 **/
typedef enum {
	CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND,
	CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND,
	CPG_COMPILE_ERROR_INVALID_TOKEN,
	CPG_COMPILE_ERROR_MAXARG,
	CPG_COMPILE_ERROR_INVALID_STACK,
	CPG_COMPILE_ERROR_NUM_ERRORS 
} CpgCompileErrorCode;

typedef struct _CpgCompileError CpgCompileError;

GQuark			  cpg_compile_error_type_quark			(void);

GType cpg_compile_error_get_type						(void);

CpgCompileError *cpg_compile_error_new					(void);

void cpg_compile_error_set								(CpgCompileError *error,
														 GError          *gerror,
														 CpgObject       *object,
														 CpgProperty     *property,
														 CpgLinkAction   *action);

GError 			 *cpg_compile_error_get_error			(CpgCompileError *error);
CpgObject 		 *cpg_compile_error_get_object			(CpgCompileError *error);
CpgProperty		 *cpg_compile_error_get_property		(CpgCompileError *error);
CpgLinkAction 	 *cpg_compile_error_get_link_action		(CpgCompileError *error);

const gchar 	 *cpg_compile_error_string				(CpgCompileError *error);
const gchar 	 *cpg_compile_error_code_string			(gint             code);
gint			  cpg_compile_error_get_code            (CpgCompileError *error);
const gchar      *cpg_compile_error_get_message         (CpgCompileError *error);

G_END_DECLS

#endif /* __CPG_COMPILE_ERROR_H__ */


