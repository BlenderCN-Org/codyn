#include "cdn-instruction-custom-operator-ref.h"
#include <codyn/cdn-expression.h>

#define CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF, CdnInstructionCustomOperatorRefPrivate))

struct _CdnInstructionCustomOperatorRefPrivate
{
	CdnOperator *op;
	CdnStackManipulation smanip;
};

G_DEFINE_TYPE (CdnInstructionCustomOperatorRef, cdn_instruction_custom_operator_ref, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_custom_operator_ref_finalize (CdnMiniObject *object)
{
	CdnInstructionCustomOperatorRef *op;

	op = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (object);

	g_object_unref (op->priv->op);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_operator_ref_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_custom_operator_ref_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionCustomOperatorRef *op;
	CdnInstructionCustomOperatorRef const *src;

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_operator_ref_parent_class)->copy (object);

	src = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_CONST (object);
	op = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (ret);

	op->priv->op = cdn_operator_copy (src->priv->op);

	return ret;
}

static gchar *
cdn_instruction_custom_operator_ref_to_string (CdnInstruction *instruction)
{
	CdnInstructionCustomOperatorRef *self;

	self = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (instruction);

	gchar const *name = cdn_operator_get_name (self->priv->op);
	gchar *ret = g_strdup_printf ("&OPC (%s)", name);

	return ret;
}

static void
cdn_instruction_custom_operator_ref_execute (CdnInstruction *instruction,
                                             CdnStack       *stack)
{
}

static CdnStackManipulation const *
cdn_instruction_custom_operator_ref_get_stack_manipulation (CdnInstruction  *instruction,
                                                            GError         **error)
{
	CdnInstructionCustomOperatorRef *self;

	self = (CdnInstructionCustomOperatorRef *)instruction;

	return &self->priv->smanip;
}

static gboolean
cdn_instruction_custom_operator_ref_equal (CdnInstruction *a,
                                           CdnInstruction *b)
{
	CdnInstructionCustomOperatorRef *ac;
	CdnInstructionCustomOperatorRef *bc;

	ac = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (a);
	bc = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (b);

	return cdn_operator_equal (ac->priv->op, bc->priv->op);
}

static void
cdn_instruction_custom_operator_ref_class_init (CdnInstructionCustomOperatorRefClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_custom_operator_ref_finalize;
	object_class->copy = cdn_instruction_custom_operator_ref_copy;

	inst_class->to_string = cdn_instruction_custom_operator_ref_to_string;
	inst_class->execute = cdn_instruction_custom_operator_ref_execute;
	inst_class->get_stack_manipulation = cdn_instruction_custom_operator_ref_get_stack_manipulation;
	inst_class->equal = cdn_instruction_custom_operator_ref_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionCustomOperatorRefPrivate));
}

static void
cdn_instruction_custom_operator_ref_init (CdnInstructionCustomOperatorRef *self)
{
	self->priv = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF_GET_PRIVATE (self);
}

/**
 * cdn_instruction_custom_operator_ref_new:
 * @operator: A #CdnOperator
 * @expressions: (element-type CdnExpression) (transfer none): A #GSList of #CdnExpression
 *
 * Create a new #CdnInstructionCustomOperatorRef.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_custom_operator_ref_new (CdnOperator *operator)
{
	CdnMiniObject *ret;
	CdnInstructionCustomOperatorRef *op;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR_REF);
	op = CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (ret);

	op->priv->op = g_object_ref (operator);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_custom_operator_ref_get_operator:
 * @op: A #CdnInstructionCustomOperatorRef
 *
 * Get the operator executed by the instruction.
 *
 * Returns: (transfer none): A #CdnOperator
 *
 **/
CdnOperator *
cdn_instruction_custom_operator_ref_get_operator (CdnInstructionCustomOperatorRef *op)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (op), NULL);

	return op->priv->op;
}
