#include "cpg-instruction-custom-operator-ref.h"
#include <cpg-network/cpg-expression.h>

#define CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CpgInstructionCustomOperatorRefPrivate))

struct _CpgInstructionCustomOperatorRefPrivate
{
	CpgOperator *op;
};

G_DEFINE_TYPE (CpgInstructionCustomOperatorRef, cpg_instruction_custom_operator_ref, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_custom_operator_ref_finalize (CpgMiniObject *object)
{
	CpgInstructionCustomOperatorRef *op;

	op = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (object);

	g_object_unref (op->priv->op);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_operator_ref_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_custom_operator_ref_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionCustomOperatorRef *op;
	CpgInstructionCustomOperatorRef const *src;

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_operator_ref_parent_class)->copy (object);

	src = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_CONST (object);
	op = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (ret);

	op->priv->op = cpg_operator_copy (src->priv->op);

	return ret;
}

static gchar *
cpg_instruction_custom_operator_ref_to_string (CpgInstruction *instruction)
{
	CpgInstructionCustomOperatorRef *self;

	self = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction);

	gchar const *name = cpg_operator_get_name (self->priv->op);
	gchar *ret = g_strdup_printf ("&OPC (%s)", name);

	return ret;
}

static void
cpg_instruction_custom_operator_ref_execute (CpgInstruction *instruction,
                                             CpgStack       *stack)
{
	// Make sure to not invalidate the stack
	cpg_stack_push (stack, 0);
}

static gint
cpg_instruction_custom_operator_ref_get_stack_count (CpgInstruction *instruction)
{
	return 1;
}

static gboolean
cpg_instruction_custom_operator_ref_equal (CpgInstruction *a,
                                       CpgInstruction *b)
{
	CpgInstructionCustomOperatorRef *ac;
	CpgInstructionCustomOperatorRef *bc;

	ac = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (a);
	bc = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (b);

	return cpg_operator_equal (ac->priv->op, bc->priv->op);
}

static void
cpg_instruction_custom_operator_ref_class_init (CpgInstructionCustomOperatorRefClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_custom_operator_ref_finalize;
	object_class->copy = cpg_instruction_custom_operator_ref_copy;

	inst_class->to_string = cpg_instruction_custom_operator_ref_to_string;
	inst_class->execute = cpg_instruction_custom_operator_ref_execute;
	inst_class->get_stack_count = cpg_instruction_custom_operator_ref_get_stack_count;
	inst_class->equal = cpg_instruction_custom_operator_ref_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionCustomOperatorRefPrivate));
}

static void
cpg_instruction_custom_operator_ref_init (CpgInstructionCustomOperatorRef *self)
{
	self->priv = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_PRIVATE (self);
}

/**
 * cpg_instruction_custom_operator_ref_new:
 * @operator: A #CpgOperator
 * @expressions: (element-type CpgExpression) (transfer none): A #GSList of #CpgExpression
 *
 * Create a new #CpgInstructionCustomOperatorRef.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_custom_operator_ref_new (CpgOperator *operator)
{
	CpgMiniObject *ret;
	CpgInstructionCustomOperatorRef *op;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF);
	op = CPG_INSTRUCTION_CUSTOM_OPERATOR_REF (ret);

	op->priv->op = g_object_ref (operator);

	return CPG_INSTRUCTION (ret);
}

/**
 * cpg_instruction_custom_operator_ref_get_operator:
 * @op: A #CpgInstructionCustomOperatorRef
 *
 * Get the operator executed by the instruction.
 *
 * Returns: (transfer none): A #CpgOperator
 *
 **/
CpgOperator *
cpg_instruction_custom_operator_ref_get_operator (CpgInstructionCustomOperatorRef *op)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (op), NULL);

	return op->priv->op;
}
