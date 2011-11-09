#include "cpg-instruction-custom-operator.h"
#include <cpg-network/cpg-expression.h>

#define CPG_INSTRUCTION_CUSTOM_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CpgInstructionCustomOperatorPrivate))

struct _CpgInstructionCustomOperatorPrivate
{
	CpgOperator *op;
};

G_DEFINE_TYPE (CpgInstructionCustomOperator, cpg_instruction_custom_operator, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_custom_operator_finalize (CpgMiniObject *object)
{
	CpgInstructionCustomOperator *op;

	op = CPG_INSTRUCTION_CUSTOM_OPERATOR (object);

	g_object_unref (op->priv->op);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_operator_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_custom_operator_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionCustomOperator *op;
	CpgInstructionCustomOperator const *src;

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_operator_parent_class)->copy (object);

	src = CPG_INSTRUCTION_CUSTOM_OPERATOR_CONST (object);
	op = CPG_INSTRUCTION_CUSTOM_OPERATOR (ret);

	op->priv->op = cpg_operator_copy (src->priv->op);

	return ret;
}

static gchar *
cpg_instruction_custom_operator_to_string (CpgInstruction *instruction)
{
	CpgInstructionCustomOperator *self;

	self = CPG_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	gchar const *name = cpg_operator_get_name (self->priv->op);
	gchar *ret = g_strdup_printf ("OPC (%s)", name);

	return ret;
}

static void
cpg_instruction_custom_operator_execute (CpgInstruction *instruction,
                                         CpgStack       *stack)
{
	CpgInstructionCustomOperator *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionCustomOperator *)instruction;
	cpg_operator_execute (self->priv->op, stack);
}

static gint
cpg_instruction_custom_operator_get_stack_count (CpgInstruction *instruction)
{
	CpgInstructionCustomOperator *self;

	self = CPG_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	return -cpg_operator_get_num_arguments (self->priv->op) + 1;
}

static GSList *
cpg_instruction_custom_operator_get_dependencies (CpgInstruction *instruction)
{
	CpgInstructionCustomOperator *self;

	self = CPG_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	GSList const *expressions;
	expressions = cpg_operator_get_expressions (self->priv->op);

	GSList *dependencies = NULL;

	while (expressions)
	{
		CpgExpression *expr = expressions->data;
		GSList *ret;

		ret = g_slist_copy ((GSList *)cpg_expression_get_dependencies (expr));
		dependencies = g_slist_concat (dependencies, ret);

		expressions = g_slist_next (expressions);
	}

	return dependencies;
}

static gboolean
cpg_instruction_custom_operator_equal (CpgInstruction *a,
                                       CpgInstruction *b)
{
	CpgInstructionCustomOperator *ac;
	CpgInstructionCustomOperator *bc;

	ac = CPG_INSTRUCTION_CUSTOM_OPERATOR (a);
	bc = CPG_INSTRUCTION_CUSTOM_OPERATOR (b);

	return cpg_operator_equal (ac->priv->op, bc->priv->op);
}

static void
cpg_instruction_custom_operator_class_init (CpgInstructionCustomOperatorClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_custom_operator_finalize;
	object_class->copy = cpg_instruction_custom_operator_copy;

	inst_class->to_string = cpg_instruction_custom_operator_to_string;
	inst_class->execute = cpg_instruction_custom_operator_execute;
	inst_class->get_stack_count = cpg_instruction_custom_operator_get_stack_count;
	inst_class->get_dependencies = cpg_instruction_custom_operator_get_dependencies;
	inst_class->equal = cpg_instruction_custom_operator_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionCustomOperatorPrivate));
}

static void
cpg_instruction_custom_operator_init (CpgInstructionCustomOperator *self)
{
	self->priv = CPG_INSTRUCTION_CUSTOM_OPERATOR_GET_PRIVATE (self);
}

/**
 * cpg_instruction_custom_operator_new:
 * @operator: A #CpgOperator
 * @expressions: (element-type CpgExpression) (transfer none): A #GSList of #CpgExpression
 *
 * Create a new #CpgInstructionCustomOperator.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_custom_operator_new (CpgOperator *operator)
{
	CpgMiniObject *ret;
	CpgInstructionCustomOperator *op;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR);
	op = CPG_INSTRUCTION_CUSTOM_OPERATOR (ret);

	op->priv->op = g_object_ref (operator);

	return CPG_INSTRUCTION (ret);
}

/**
 * cpg_instruction_custom_operator_get_operator:
 * @op: A #CpgInstructionCustomOperator
 *
 * Get the operator executed by the instruction.
 *
 * Returns: (transfer none): A #CpgOperator
 *
 **/
CpgOperator *
cpg_instruction_custom_operator_get_operator (CpgInstructionCustomOperator *op)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (op), NULL);

	return op->priv->op;
}
