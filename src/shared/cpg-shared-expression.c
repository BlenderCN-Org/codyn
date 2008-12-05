#include "shared/cpg-shared-expression.h"
#include "shared/cpg-shared-property.h"
#include "cpg-debug.h"
#include "cpg-math.h"

static inline CpgSharedInstruction *
get_instruction(CpgSharedExpression *expression, unsigned idx, void *base)
{
	return cpg_shared_array_base_type(base, expression->instructions, idx, CpgSharedInstruction);
}

void
cpg_shared_expression_set_value(CpgSharedExpression *expression, double value, void *base)
{
	if (expression->num_instructions != 1)
		return;
	
	CpgSharedInstruction *inst = get_instruction(expression, 0, base);

	if (inst->type != CPG_INSTRUCTION_TYPE_NUMBER)
		return;
	
	inst->value = value;
}

double
cpg_shared_expression_evaluate(CpgSharedExpression *expression, void *base)
{
	unsigned i;
	expression->output.output_ptr = expression->output.output;
	
	for (i = 0; i < expression->num_instructions; ++i)
	{
		CpgSharedInstruction *instruction = get_instruction(expression, i, base);

		switch (instruction->type)
		{
			case CPG_INSTRUCTION_TYPE_NUMBER:
				cpg_stack_push(&(expression->output), instruction->value, base);
			break;
			case CPG_INSTRUCTION_TYPE_PROPERTY:
			{
				CpgSharedProperty *property = cpg_shared_pointer_base_type(base, instruction->property, CpgSharedProperty);
				cpg_stack_push(&(expression->output), cpg_shared_property_value(property, base), base);
			}
			break;
			case CPG_INSTRUCTION_TYPE_FUNCTION:
				cpg_math_function_execute(instruction->id, &(expression->output), base);
			break;
			case CPG_INSTRUCTION_TYPE_OPERATOR:
				cpg_math_operator_execute(instruction->id, &(expression->output), base);
			break;
			default:
			break;
		}
	}
	
	if (cpg_stack_count(&(expression->output)) != 1)
	{
		cpg_debug_error("Invalid expression stack size after evaluating");
		return 0.0;
	}
	
	return cpg_stack_pop(&(expression->output), base);	
}
