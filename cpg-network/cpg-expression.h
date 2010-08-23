/*
 * cpg-expression.h
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

#ifndef __CPG_EXPRESSION_H__
#define __CPG_EXPRESSION_H__

#include <stdio.h>
#include <glib-object.h>
#include <cpg-network/cpg-compile-context.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_EXPRESSION	(cpg_expression_get_type())

/* Forward declaration */
CPG_FORWARD_DECL (CpgProperty);
CPG_FORWARD_DECL (CpgFunction);

typedef struct _CpgInstruction 		CpgInstruction;

/**
 * CpgInstructionCode:
 * @CPG_INSTRUCTION_TYPE_NONE: none
 * @CPG_INSTRUCTION_TYPE_FUNCTION: function
 * @CPG_INSTRUCTION_TYPE_NUMBER: number
 * @CPG_INSTRUCTION_TYPE_OPERATOR: operator
 * @CPG_INSTRUCTION_TYPE_PROPERTY: property 
 * @CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION: custom function 
 *
 * Enum used to indicate instruction type
 *
 **/
typedef enum {
	CPG_INSTRUCTION_TYPE_NONE,
	CPG_INSTRUCTION_TYPE_FUNCTION,
	CPG_INSTRUCTION_TYPE_NUMBER,
	CPG_INSTRUCTION_TYPE_OPERATOR,
	CPG_INSTRUCTION_TYPE_PROPERTY,
	CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION
} CpgInstructionCode;

struct _CpgInstruction
{
	CpgInstructionCode type;
};

typedef struct
{
	CpgInstruction parent;
	
	guint id;
	gchar *name;
	gint arguments;
	gboolean variable;
} CpgInstructionFunction;

typedef struct
{
	CpgInstruction parent;
	
	CPG_FORWARD_DECL (CpgFunction) *function;

	gint arguments;
} CpgInstructionCustomFunction;

typedef struct
{
	CpgInstruction parent;
	
	gdouble value;
} CpgInstructionNumber;

typedef enum
{
	CPG_INSTRUCTION_BINDING_NONE = 0,
	CPG_INSTRUCTION_BINDING_FROM,
	CPG_INSTRUCTION_BINDING_TO,
} CpgInstructionBinding;

typedef struct _CpgInstructionProperty CpgInstructionProperty;

struct _CpgInstructionProperty
{
	CpgInstruction parent;

	CPG_FORWARD_DECL (CpgProperty) *property;
	CpgInstructionBinding binding;
};

typedef struct _CpgExpression 		CpgExpression;

GType			  cpg_expression_get_type			(void);
CpgExpression 	 *cpg_expression_new				(gchar const    *expression);

GSList		 	 *cpg_expression_get_dependencies	(CpgExpression  *expression);
const gchar      *cpg_expression_get_as_string		(CpgExpression  *expression);
gint			  cpg_expression_compile			(CpgExpression  *expression, 
													 CpgCompileContext *context,
													 GError        **error);

gdouble 		  cpg_expression_evaluate			(CpgExpression  *expression);
void			  cpg_expression_set_value			(CpgExpression  *expression, 
													 gdouble         value);
void			  cpg_expression_reset				(CpgExpression  *expression);

gboolean		  cpg_expression_equal				(CpgExpression  *expression,
													 CpgExpression  *other);

void			  cpg_expression_set_from_string	(CpgExpression  *expression, 
													 gchar const    *value);
void 			  cpg_expression_reset_cache		(CpgExpression  *expression);

/* Instructions */
GSList 			 *cpg_expression_get_instructions	(CpgExpression  *expression);
gboolean          cpg_expression_set_instructions   (CpgExpression  *expression,
                                                     GSList         *instructions);

CpgInstruction   *cpg_instruction_function_new 		(guint         id,
													 gchar const  *name,
													 gint          arguments,
													 gboolean      variable);

CpgInstruction   *cpg_instruction_custom_function_new (CPG_FORWARD_DECL (CpgFunction) *function,
													   gint                 arguments);

CpgInstruction   *cpg_instruction_number_new 		(gdouble value);
CpgInstruction   *cpg_instruction_operator_new 		(guint         id,
													 gchar const  *name,
													 gint          arguments);
CpgInstruction   *cpg_instruction_property_new 		(CPG_FORWARD_DECL (CpgProperty) *property,
                                                     CpgInstructionBinding binding);
CpgInstruction   *cpg_instruction_copy 				(CpgInstruction *instruction);
void              cpg_instruction_free 				(CpgInstruction *instruction);

gchar			 *cpg_instruction_to_string			(CpgInstruction *instruction);

G_END_DECLS

#endif /* __CPG_EXPRESSION_H__ */
