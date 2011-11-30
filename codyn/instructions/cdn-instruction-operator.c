#include "cdn-instruction-operator.h"

#include <codyn/cdn-math.h>

G_DEFINE_TYPE (CdnInstructionOperator, cdn_instruction_operator, CDN_TYPE_INSTRUCTION_FUNCTION)

static gchar *
cdn_instruction_operator_to_string (CdnInstruction *instruction)
{
	CdnInstructionOperator *self;

	self = CDN_INSTRUCTION_OPERATOR (instruction);

	return g_strdup_printf ("OP  (%s)",
	                        cdn_instruction_function_get_name (CDN_INSTRUCTION_FUNCTION (self)));
}

static void
cdn_instruction_operator_execute (CdnInstruction *instruction,
                                  CdnStack       *stack)
{
	CdnInstructionFunction *func;

	/* Direct cast to reduce overhead of GType cast */
	func = (CdnInstructionFunction *)instruction;

	cdn_math_operator_execute (cdn_instruction_function_get_id (func),
	                           cdn_instruction_function_get_arguments (func),
	                           stack);
}

static gboolean
cdn_instruction_operator_get_is_commutative (CdnInstruction *instruction)
{
	CdnInstructionFunction *func;
	CdnMathOperatorType type;

	/* Direct cast to reduce overhead of GType cast */
	func = (CdnInstructionFunction *)instruction;

	type = (CdnMathOperatorType)cdn_instruction_function_get_id (func);

	return cdn_math_operator_is_commutative (type);
}

static void
cdn_instruction_operator_class_init (CdnInstructionOperatorClass *klass)
{
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	inst_class->to_string = cdn_instruction_operator_to_string;
	inst_class->execute = cdn_instruction_operator_execute;
	inst_class->get_is_commutative = cdn_instruction_operator_get_is_commutative;
}

static void
cdn_instruction_operator_init (CdnInstructionOperator *self)
{
}

CdnInstruction *
cdn_instruction_operator_new (guint        id,
                              const gchar *name,
                              gint         arguments)
{
	CdnMiniObject *ret;
	CdnInstructionFunction *func;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_OPERATOR);
	func = CDN_INSTRUCTION_FUNCTION (ret);

	cdn_instruction_function_set_id (func, id);
	cdn_instruction_function_set_name (func, name);
	cdn_instruction_function_set_arguments (func, arguments);

	return CDN_INSTRUCTION (ret);
}
