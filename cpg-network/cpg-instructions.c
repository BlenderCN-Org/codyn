#include "cpg-instructions.h"
#include "cpg-function.h"

/**
 * SECTION:cpg-instructions
 * @short_description: Instruction set for math expressions
 *
 * The following documentation describes the instructions that are used
 * when compiling an expression. You can use @cpg_expression_get_instructions
 * to get a list of instructions once an expression is compiled.
 *
 */

static CpgInstruction *
cpg_instruction_initialize (CpgInstruction *instruction)
{
	return instruction;
}

#define instruction_new(Type) ((Type *)cpg_instruction_initialize ((CpgInstruction *)g_slice_new0 (Type)))

/**
 * cpg_instruction_copy:
 * @instruction: a #CpgInstruction
 *
 * Create a copy of a #CpgInstruction
 *
 * Return value: a new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_copy (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *property =
				CPG_INSTRUCTION_PROPERTY (instruction);

			return cpg_instruction_property_new (property->property,
			                                     property->binding);
		}
		break;
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		{
			CpgInstructionFunction *function =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return cpg_instruction_function_new (function->id,
			                                     function->name,
			                                     function->arguments,
			                                     function->variable);
		}
		break;
		case CPG_INSTRUCTION_TYPE_VARIADIC_FUNCTION:
		{
			CpgInstructionFunction *function =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return cpg_instruction_variadic_function_new (function->id,
			                                              function->name,
			                                              function->arguments,
			                                              function->variable);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *function =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return cpg_instruction_operator_new (function->id,
			                                     function->name,
			                                     function->arguments);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
		{
			CpgInstructionNumber *number =
				CPG_INSTRUCTION_NUMBER (instruction);

			return cpg_instruction_number_new (number->value);
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *function =
				CPG_INSTRUCTION_CUSTOM_FUNCTION (instruction);

			return cpg_instruction_custom_function_new (function->function,
			                                            function->arguments);
		}
		break;
		default:
			return NULL;
		break;
	}
}

static void
init_function (CpgInstructionFunction *func,
               guint                   id,
               gchar const            *name,
               gint                    arguments,
               gboolean                variable)
{
	func->id = id;
	func->name = g_strdup (name);
	func->arguments = arguments;
	func->variable = variable;
}

/**
 * cpg_instruction_function_new:
 * @id: function id
 * @name: function name
 * @arguments: the number of arguments this function takes
 * @variable: if the number of arguments is variable for this function
 *
 * Creates a new function call instruction. The id @id of the instruction
 * specifies which function the instruction represents (see
 * #CpgMathFunctionType). If @variable is %TRUE, the instruction expects the
 * first value on the stack, when the instruction is executed, to be the number
 * of arguments to process.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_function_new (guint         id,
                              gchar const  *name,
                              gint          arguments,
                              gboolean      variable)
{
	CpgInstructionFunction *res = instruction_new (CpgInstructionFunction);

	res->parent.type = CPG_INSTRUCTION_TYPE_FUNCTION;

	init_function (res, id, name, arguments, variable);

	return CPG_INSTRUCTION (res);
}

CpgInstruction *
cpg_instruction_variadic_function_new (guint         id,
                                       gchar const  *name,
                                       gint          arguments,
                                       gboolean      variable)
{
	CpgInstructionVariadicFunction *res = instruction_new (CpgInstructionVariadicFunction);
	CpgInstruction *parent = CPG_INSTRUCTION (res);

	parent->type = CPG_INSTRUCTION_TYPE_VARIADIC_FUNCTION;

	init_function (CPG_INSTRUCTION_FUNCTION (res), id, name, arguments, variable);

	res->cached = FALSE;
	res->cached_result = 0;

	return CPG_INSTRUCTION (res);
}

/**
 * cpg_instruction_number_new:
 * @value: the numeric value
 *
 * Creates a new number instruction. This is the most basic instruction, which
 * simply pushes a number onto the stack when executed.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_number_new (gdouble value)
{
	CpgInstructionNumber *res = instruction_new (CpgInstructionNumber);
	res->parent.type = CPG_INSTRUCTION_TYPE_NUMBER;
	res->value = value;

	return CPG_INSTRUCTION (res);
}

/**
 * cpg_instruction_operator_new:
 * @id: operator id
 * @name: operator name
 * @arguments: the number of arguments this operator takes
 *
 * Creates a new operator instruction. The id @id of the instruction
 * specifies which operator the instruction represents (see
 * #CpgMathOperatorType).
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_operator_new (guint         id,
                              gchar const  *name,
                              gint          arguments)
{
	CpgInstruction *res = cpg_instruction_function_new (id,
	                                                    name,
	                                                    arguments,
	                                                    cpg_math_operator_is_variable (id));

	res->type = CPG_INSTRUCTION_TYPE_OPERATOR;

	return res;
}

/**
 * cpg_instruction_property_new:
 * @property: a #CpgProperty
 * @binding: the property binding
 *
 * Creates a new property call instruction. When the instruction is executed,
 * the property value expression is evaluated and its return value is pushed
 * on the stack.
 *
 * Return value: the new #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_property_new (CpgProperty           *property,
                              CpgInstructionBinding  binding)
{
	CpgInstructionProperty *res = instruction_new (CpgInstructionProperty);
	res->parent.type = CPG_INSTRUCTION_TYPE_PROPERTY;

	res->property = property;
	res->binding = binding;

	_cpg_property_use (property);
	g_object_ref (property);

	return CPG_INSTRUCTION (res);
}

/**
 * cpg_instruction_custom_function_new:
 * @function: A #CpgFunction
 * @arguments: The number of arguments this function takes
 *
 * Creates a new custom function call instruction. If the number of arguments
 * (@arguments) is not equal to the number of arguments @function takes, the
 * instruction expects the first value on the stack, when the instruction is
 * executed, to be the number of arguments to process.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_custom_function_new (CpgFunction *function,
                                     gint         arguments)
{
	CpgInstructionCustomFunction *res = instruction_new (CpgInstructionCustomFunction);

	res->parent.type = CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION;
	res->function = g_object_ref (function);
	res->arguments = arguments;

	return CPG_INSTRUCTION (res);
}

/**
 * cpg_instruction_free:
 * @instruction: a #CpgInstruction
 *
 * Free previously created #CpgInstruction object
 *
 **/
void
cpg_instruction_free (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *prop =
				CPG_INSTRUCTION_PROPERTY (instruction);

			_cpg_property_unuse (prop->property);
			g_object_unref (prop->property);

			g_slice_free (CpgInstructionProperty, prop);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
			g_slice_free (CpgInstructionNumber,
			              CPG_INSTRUCTION_NUMBER (instruction));
		break;
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *func =
				CPG_INSTRUCTION_FUNCTION (instruction);

			g_free (func->name);
			g_slice_free (CpgInstructionFunction, func);
		}
		break;
		case CPG_INSTRUCTION_TYPE_VARIADIC_FUNCTION:
		{
			CpgInstructionFunction *func =
				CPG_INSTRUCTION_FUNCTION (instruction);

			g_free (func->name);

			g_slice_free (CpgInstructionVariadicFunction,
			              CPG_INSTRUCTION_VARIADIC_FUNCTION (func));
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *func =
				CPG_INSTRUCTION_CUSTOM_FUNCTION (instruction);

			g_object_unref (func->function);
			g_slice_free (CpgInstructionCustomFunction, func);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NONE:
		break;
	}
}

/**
 * cpg_instruction_to_string:
 * @instruction: A #CpgInstruction
 *
 * Get the string representation of an instruction.
 *
 * Returns: The string representation of the instruction.
 *
 **/
gchar *
cpg_instruction_to_string (CpgInstruction *instruction)
{
	switch (instruction->type)
	{
		case CPG_INSTRUCTION_TYPE_FUNCTION:
		{
			CpgInstructionFunction *inst =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return g_strdup_printf ("FUN (%s)", inst->name);
		}
		break;
		case CPG_INSTRUCTION_TYPE_VARIADIC_FUNCTION:
		{
			CpgInstructionFunction *inst =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return g_strdup_printf ("VAR (%s)", inst->name);
		}
		break;
		case CPG_INSTRUCTION_TYPE_NUMBER:
		{
			CpgInstructionNumber *inst =
				CPG_INSTRUCTION_NUMBER (instruction);

			return g_strdup_printf ("NUM (%f)", inst->value);
		}
		break;
		case CPG_INSTRUCTION_TYPE_OPERATOR:
		{
			CpgInstructionFunction *inst =
				CPG_INSTRUCTION_FUNCTION (instruction);

			return g_strdup_printf ("OP  (%s)", inst->name);
		}
		break;
		case CPG_INSTRUCTION_TYPE_PROPERTY:
		{
			CpgInstructionProperty *inst =
				CPG_INSTRUCTION_PROPERTY (instruction);

			return g_strdup_printf ("PRP (%s.%s)",
			                        cpg_object_get_id (cpg_property_get_object (inst->property)),
			                        cpg_property_get_name (inst->property));
		}
		break;
		case CPG_INSTRUCTION_TYPE_CUSTOM_FUNCTION:
		{
			CpgInstructionCustomFunction *inst =
				CPG_INSTRUCTION_CUSTOM_FUNCTION (instruction);

			return g_strdup_printf ("FNC (%s)", cpg_object_get_id (CPG_OBJECT (inst->function)));
		}
		break;
		default:
			return g_strdup ("NON");
		break;
	}
}
