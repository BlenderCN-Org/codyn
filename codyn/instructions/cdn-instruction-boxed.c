#include "cdn-instruction-boxed.h"
#include "cdn-instruction.h"
#include "cdn-instruction-variable.h"
#include "cdn-instruction-custom-function.h"
#include "cdn-instruction-custom-function-ref.h"
#include "cdn-instruction-custom-operator.h"
#include "cdn-instruction-custom-operator-ref.h"

/**
 * CdnInstructionBoxed:
 *
 * Boxed instruction.
 *
 * #CdnInstructionBoxed provides boxed versions of the instructions. Instructions
 * are not full GObject types, which provides difficulties for language bindings.
 * The boxed values can be easily bound for other languages, but are of little
 * use otherwise.
 *
 */

/**
 * CdnInstructionCustomFunctionBoxed:
 *
 * Boxed custom function instruction.
 *
 * Boxed version of #CdnInstructionCustomFunction, only used for language bindings.
 *
 */

/**
 * CdnInstructionCustomFunctionRefBoxed:
 *
 * Boxed custom function ref instruction.
 *
 * Boxed version of #CdnInstructionCustomFunctionRef, only used for language bindings.
 *
 */

/**
 * CdnInstructionCustomOperatorBoxed:
 *
 * Boxed custom operator instruction.
 *
 * Boxed version of #CdnInstructionCustomOperator, only used for language bindings.
 *
 */

/**
 * CdnInstructionCustomOperatorRefBoxed:
 *
 * Boxed custom operator ref instruction.
 *
 * Boxed version of #CdnInstructionCustomOperatorRef, only used for language bindings.
 *
 */

/**
 * CdnInstructionVariableBoxed:
 *
 * Boxed variable instruction.
 *
 * Boxed version of #CdnInstructionVariable, only used for language bindings.
 *
 */

static gpointer
copy_func (gpointer instr)
{
	return cdn_mini_object_ref (instr);
}

static void
free_func (gpointer instr)
{
	cdn_mini_object_unref (instr);
}

#ifndef G_DEFINE_BOXED_TYPE
#define G_DEFINE_BOXED_TYPE(TypeName, type_name, copy_func, free_func)			\
GType											\
type_name##_get_type ()									\
{											\
	static GType gtype = 0;								\
											\
	if (G_UNLIKELY (gtype == 0))							\
	{										\
		gtype = g_boxed_type_register_static (#TypeName,			\
		                                      (GBoxedCopyFunc)copy_func,	\
		                                      (GBoxedFreeFunc)free_func);	\
	}										\
											\
	return gtype;									\
}
#endif

G_DEFINE_BOXED_TYPE (CdnInstructionBoxed, cdn_instruction_boxed, copy_func, free_func)
G_DEFINE_BOXED_TYPE (CdnInstructionVariableBoxed, cdn_instruction_variable_boxed, copy_func, free_func)
G_DEFINE_BOXED_TYPE (CdnInstructionCustomFunctionBoxed, cdn_instruction_custom_function_boxed, copy_func, free_func)
G_DEFINE_BOXED_TYPE (CdnInstructionCustomFunctionRefBoxed, cdn_instruction_custom_function_ref_boxed, copy_func, free_func)
G_DEFINE_BOXED_TYPE (CdnInstructionCustomOperatorBoxed, cdn_instruction_custom_operator_boxed, copy_func, free_func)
G_DEFINE_BOXED_TYPE (CdnInstructionCustomOperatorRefBoxed, cdn_instruction_custom_operator_ref_boxed, copy_func, free_func)

/**
 * cdn_instruction_boxed_to_string:
 * @instruction: A #CdnInstruction
 *
 * Get some sort of string representation of the instruction. Note that this
 * is only useful for debugging since the string representation only shows
 * what type of instruction this is.
 *
 * Returns: a string representation of the instruction
 *
 **/
gchar *
cdn_instruction_boxed_to_string (CdnInstructionBoxed *instruction)
{
	return cdn_instruction_to_string (instruction);
}

/**
 * cdn_instruction_boxed_get_dependencies:
 * @instruction: A #CdnInstruction
 *
 * Get the properties on which the instruction depends.
 *
 * Returns: (element-type CdnVariable) (transfer container): A #GSList of #CdnVariable. The list
 *          should be freed with g_slist_free when no longer used.
 *
 **/
GSList *
cdn_instruction_boxed_get_dependencies (CdnInstructionBoxed *instruction)
{
	return cdn_instruction_get_dependencies (instruction);
}

/**
 * cdn_instruction_boxed_get_location:
 * @instruction: a #CdnInstruction.
 * @start: (out): return value for the start.
 * @end: (out): return value for the end.
 *
 * Get the location of this instruction in the corresponding string representation
 * of the expression the instruction belongs to.
 *
 **/
void
cdn_instruction_boxed_get_location (CdnInstructionBoxed *instruction,
                                    gint                *start,
                                    gint                *end)
{
	cdn_instruction_get_location (instruction, start, end);
}

/**
 * cdn_instruction_boxed_get_stack_manipulation:
 * @instruction: A #CdnInstruction
 *
 * Get by how many values the instruction will modify the stack when executed.
 * Note that this can be a negative value if the instruction consumes more
 * values than it pushes. It can generally be assumed that every instruction
 * always pushes one value on the stack (i.e. the number of values it pops
 * is -stack_count + 1)
 *
 * Returns: the number of values that the instruction will pop and push on the
 *          stack
 *
 **/
CdnStackManipulation const *
cdn_instruction_boxed_get_stack_manipulation (CdnInstructionBoxed  *instruction,
                                              GError              **error)
{
	return cdn_instruction_get_stack_manipulation (instruction, error);
}

/**
 * cdn_instruction_boxed_as_variable:
 * @instruction: a #CdnInstructionBoxed.
 *
 * Get the instruction as a variable.
 *
 * Returns: (transfer none) (allow-none): a #CdnInstructionVariableBoxed.
 *
 **/
CdnInstructionVariableBoxed *
cdn_instruction_boxed_as_variable (CdnInstructionBoxed *instruction)
{
	if (CDN_IS_INSTRUCTION_VARIABLE (instruction))
	{
		return CDN_INSTRUCTION_VARIABLE (instruction);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_variable_boxed_get_variable:
 * @instruction: a #CdnInstructionVariableBoxed.
 *
 * Description.
 *
 * Returns: (transfer none): a #CdnVariable.
 *
 **/
CdnVariable *
cdn_instruction_variable_boxed_get_variable (CdnInstructionVariableBoxed *instruction)
{
	return cdn_instruction_variable_get_variable (instruction);
}

/**
 * cdn_instruction_variable_boxed_get_slice:
 * @instruction: a #CdnInstructionVariable.
 * @length: (out): the length of the returned slice array.
 * @dim: (out): the dimension of the slice.
 *
 * Get the slice, if any, of this variable.
 *
 * Returns: (array length=length): the slice indices.
 *
 **/
guint const *
cdn_instruction_variable_boxed_get_slice (CdnInstructionVariableBoxed *instruction,
                                          guint                       *length,
                                          CdnDimension                *dim)
{
	return cdn_instruction_variable_get_slice (instruction, length, dim);
}

/**
 * cdn_instruction_boxed_as_custom_function:
 * @instruction: a #CdnInstructionBoxed.
 *
 * Get the instruction as a custom function.
 *
 * Returns: (transfer none) (allow-none): a #CdnInstructionCustomFunctionBoxed.
 *
 **/
CdnInstructionCustomFunctionBoxed *
cdn_instruction_boxed_as_custom_function (CdnInstructionBoxed *instruction)
{
	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (instruction))
	{
		return CDN_INSTRUCTION_CUSTOM_FUNCTION (instruction);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_custom_function_boxed_get_function:
 * @instruction: a #CdnInstructionCustomFunctionBoxed.
 *
 * Get the function.
 *
 * Returns: (transfer none): a #CdnFunction.
 *
 **/
CdnFunction *
cdn_instruction_custom_function_boxed_get_function (CdnInstructionCustomFunctionBoxed *instruction)
{
	return cdn_instruction_custom_function_get_function (instruction);
}

/**
 * cdn_instruction_boxed_as_custom_function_ref:
 * @instruction: a #CdnInstructionBoxed.
 *
 * Get the instruction as a custom function.
 *
 * Returns: (transfer none) (allow-none): a #CdnInstructionCustomFunctionRefBoxed.
 *
 **/
CdnInstructionCustomFunctionRefBoxed *
cdn_instruction_boxed_as_custom_function_ref (CdnInstructionBoxed *instruction)
{
	if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction))
	{
		return CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_custom_function_ref_boxed_get_function:
 * @instruction: a #CdnInstructionCustomFunctionRefBoxed.
 *
 * Get the function.
 *
 * Returns: (transfer none): a #CdnFunction.
 *
 **/
CdnFunction *
cdn_instruction_custom_function_ref_boxed_get_function (CdnInstructionCustomFunctionRefBoxed *instruction)
{
	return cdn_instruction_custom_function_ref_get_function (instruction);
}

/**
 * cdn_instruction_boxed_as_custom_operator:
 * @instruction: a #CdnInstructionBoxed.
 *
 * Get the instruction as a custom function.
 *
 * Returns: (transfer none) (allow-none): a #CdnInstructionCustomOperatorBoxed.
 *
 **/
CdnInstructionCustomOperatorBoxed *
cdn_instruction_boxed_as_custom_operator (CdnInstructionBoxed *instruction)
{
	if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (instruction))
	{
		return CDN_INSTRUCTION_CUSTOM_OPERATOR (instruction);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_custom_operator_boxed_get_operator:
 * @instruction: a #CdnInstructionCustomOperatorBoxed.
 *
 * Get the function.
 *
 * Returns: (transfer none): a #CdnOperator.
 *
 **/
CdnOperator *
cdn_instruction_custom_operator_boxed_get_operator (CdnInstructionCustomOperatorBoxed *instruction)
{
	return cdn_instruction_custom_operator_get_operator (instruction);
}

/**
 * cdn_instruction_boxed_as_custom_operator_ref:
 * @instruction: a #CdnInstructionBoxed.
 *
 * Get the instruction as a custom function.
 *
 * Returns: (transfer none) (allow-none): a #CdnInstructionCustomOperatorRefBoxed.
 *
 **/
CdnInstructionCustomOperatorRefBoxed *
cdn_instruction_boxed_as_custom_operator_ref (CdnInstructionBoxed *instruction)
{
	if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction))
	{
		return CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_custom_operator_ref_boxed_get_operator_ref:
 * @instruction: a #CdnInstructionCustomOperatorRefBoxed.
 *
 * Get the function.
 *
 * Returns: (transfer none): a #CdnOperator.
 *
 **/
CdnOperator *
cdn_instruction_custom_operator_ref_boxed_get_operator_ref (CdnInstructionCustomOperatorRefBoxed *instruction)
{
	return cdn_instruction_custom_operator_ref_get_operator (instruction);
}
