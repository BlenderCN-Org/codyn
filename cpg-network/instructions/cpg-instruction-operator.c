#include "cpg-instruction-operator.h"

#include <cpg-network/cpg-math.h>

G_DEFINE_TYPE (CpgInstructionOperator, cpg_instruction_operator, CPG_TYPE_INSTRUCTION_FUNCTION)

static gchar *
cpg_instruction_operator_to_string (CpgInstruction *instruction)
{
	CpgInstructionOperator *self;

	self = CPG_INSTRUCTION_OPERATOR (instruction);

	return g_strdup_printf ("OP  (%s)",
	                        cpg_instruction_function_get_name (CPG_INSTRUCTION_FUNCTION (self)));
}

static void
cpg_instruction_operator_execute (CpgInstruction *instruction,
                                  CpgStack       *stack)
{
	CpgInstructionFunction *func;

	/* Direct cast to reduce overhead of GType cast */
	func = (CpgInstructionFunction *)instruction;

	cpg_math_operator_execute (cpg_instruction_function_get_id (func),
	                           cpg_instruction_function_get_arguments (func),
	                           stack);
}

static gboolean
cpg_instruction_operator_get_is_commutative (CpgInstruction *instruction)
{
	CpgInstructionFunction *func;
	CpgMathOperatorType type;

	/* Direct cast to reduce overhead of GType cast */
	func = (CpgInstructionFunction *)instruction;

	type = (CpgMathOperatorType)cpg_instruction_function_get_id (func);

	switch (type)
	{
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
		case CPG_MATH_OPERATOR_TYPE_PLUS:
		case CPG_MATH_OPERATOR_TYPE_EQUAL:
		case CPG_MATH_OPERATOR_TYPE_OR:
		case CPG_MATH_OPERATOR_TYPE_AND:
			return TRUE;
		default:
			return FALSE;
	}
}

static void
cpg_instruction_operator_class_init (CpgInstructionOperatorClass *klass)
{
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	inst_class->to_string = cpg_instruction_operator_to_string;
	inst_class->execute = cpg_instruction_operator_execute;
	inst_class->get_is_commutative = cpg_instruction_operator_get_is_commutative;
}

static void
cpg_instruction_operator_init (CpgInstructionOperator *self)
{
}

CpgInstruction *
cpg_instruction_operator_new (guint        id,
                              const gchar *name,
                              gint         arguments)
{
	CpgMiniObject *ret;
	CpgInstructionFunction *func;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_OPERATOR);
	func = CPG_INSTRUCTION_FUNCTION (ret);

	cpg_instruction_function_set_id (func, id);
	cpg_instruction_function_set_name (func, name);
	cpg_instruction_function_set_arguments (func, arguments);

	return CPG_INSTRUCTION (ret);
}
