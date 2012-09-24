/*
 * cdn-instruction_boxed-boxed.h
 * This file is part of codyn
 *
 * Copyright (C) 2012 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with codyn. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __CDN_INSTRUCTION_BOXED_H__
#define __CDN_INSTRUCTION_BOXED_H__

#include <glib-object.h>
#include <codyn/cdn-stack.h>
#include <codyn/cdn-variable.h>
#include <codyn/cdn-function.h>
#include <codyn/operators/cdn-operator.h>

typedef struct _CdnInstruction CdnInstructionBoxed;
typedef struct _CdnInstructionVariable CdnInstructionVariableBoxed;

typedef struct _CdnInstructionCustomFunction CdnInstructionCustomFunctionBoxed;
typedef struct _CdnInstructionCustomFunctionRef CdnInstructionCustomFunctionRefBoxed;

typedef struct _CdnInstructionCustomOperator CdnInstructionCustomOperatorBoxed;
typedef struct _CdnInstructionCustomOperatorRef CdnInstructionCustomOperatorRefBoxed;

GType     cdn_instruction_boxed_get_type             (void) G_GNUC_CONST;

gchar    *cdn_instruction_boxed_to_string            (CdnInstructionBoxed *instruction);

GSList   *cdn_instruction_boxed_get_dependencies     (CdnInstructionBoxed *instruction);

void      cdn_instruction_boxed_get_location         (CdnInstructionBoxed *instruction,
                                                      gint                *start,
                                                      gint                *end);

CdnStackManipulation const *
          cdn_instruction_boxed_get_stack_manipulation (CdnInstructionBoxed  *instruction,
                                                        GError              **error);


GType     cdn_instruction_variable_boxed_get_type      (void);

CdnInstructionVariableBoxed *cdn_instruction_boxed_as_variable (CdnInstructionBoxed *instruction);
CdnVariable *cdn_instruction_variable_boxed_get_variable (CdnInstructionVariableBoxed *instruction);
guint const *cdn_instruction_variable_boxed_get_slice (CdnInstructionVariableBoxed *instruction,
                                                       guint                       *length,
                                                       CdnDimension                *dim);

GType     cdn_instruction_custom_function_boxed_get_type      (void);

CdnInstructionCustomFunctionBoxed *cdn_instruction_boxed_as_custom_function (CdnInstructionBoxed *instruction);
CdnFunction *cdn_instruction_custom_function_boxed_get_function (CdnInstructionCustomFunctionBoxed *instruction);

GType     cdn_instruction_custom_function_ref_boxed_get_type      (void);

CdnInstructionCustomFunctionRefBoxed *cdn_instruction_boxed_as_custom_function_ref (CdnInstructionBoxed *instruction);
CdnFunction *cdn_instruction_custom_function_ref_boxed_get_function (CdnInstructionCustomFunctionRefBoxed *instruction);

GType     cdn_instruction_custom_operator_boxed_get_type      (void);

CdnInstructionCustomOperatorBoxed *cdn_instruction_boxed_as_custom_operator (CdnInstructionBoxed *instruction);
CdnOperator *cdn_instruction_custom_operator_boxed_get_operator (CdnInstructionCustomOperatorBoxed *instruction);

GType     cdn_instruction_custom_operator_ref_boxed_get_type      (void);

CdnInstructionCustomOperatorRefBoxed *cdn_instruction_boxed_as_custom_operator_ref (CdnInstructionBoxed *instruction);
CdnOperator *cdn_instruction_custom_operator_ref_boxed_get_operator_ref (CdnInstructionCustomOperatorRefBoxed *instruction);

#endif /* __CDN_INSTRUCTION_BOXED_H__ */

