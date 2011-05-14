#include "cpg-instruction-number.h"

#define CPG_INSTRUCTION_NUMBER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_NUMBER, CpgInstructionNumberPrivate))

struct _CpgInstructionNumberPrivate
{
	gdouble value;
};

G_DEFINE_TYPE (CpgInstructionNumber, cpg_instruction_number, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_number_finalize (CpgMiniObject *object)
{
	CPG_MINI_OBJECT_CLASS (cpg_instruction_number_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_number_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionNumber *self;
	CpgInstructionNumber const *src;

	src = CPG_INSTRUCTION_NUMBER_CONST (object);

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_number_parent_class)->copy (object);

	self = CPG_INSTRUCTION_NUMBER (ret);
	self->priv->value = src->priv->value;

	return ret;
}

static gchar *
cpg_instruction_number_to_string (CpgInstruction *instruction)
{
	CpgInstructionNumber *self;

	self = CPG_INSTRUCTION_NUMBER (instruction);

	return g_strdup_printf ("NUM (%.3f)", self->priv->value);
}

static void
cpg_instruction_number_execute (CpgInstruction *instruction,
                                CpgStack       *stack)
{
	CpgInstructionNumber *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionNumber *)instruction;
	cpg_stack_push (stack, self->priv->value);
}

static gint
cpg_instruction_number_get_stack_count (CpgInstruction *instruction)
{
	return 1;
}

static gboolean
cpg_instruction_number_equal (CpgInstruction *i1,
                              CpgInstruction *i2)
{
	CpgInstructionNumber *n1 = CPG_INSTRUCTION_NUMBER (i1);
	CpgInstructionNumber *n2 = CPG_INSTRUCTION_NUMBER (i2);

	return n1->priv->value == n2->priv->value;
}

static void
cpg_instruction_number_class_init (CpgInstructionNumberClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_number_finalize;
	object_class->copy = cpg_instruction_number_copy;

	inst_class->to_string = cpg_instruction_number_to_string;
	inst_class->execute = cpg_instruction_number_execute;
	inst_class->get_stack_count = cpg_instruction_number_get_stack_count;
	inst_class->equal = cpg_instruction_number_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionNumberPrivate));
}

static void
cpg_instruction_number_init (CpgInstructionNumber *self)
{
	self->priv = CPG_INSTRUCTION_NUMBER_GET_PRIVATE (self);
}

CpgInstruction *
cpg_instruction_number_new (gdouble value)
{
	CpgMiniObject *ret;
	CpgInstructionNumber *self;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_NUMBER);
	self = CPG_INSTRUCTION_NUMBER (ret);

	self->priv->value = value;

	return CPG_INSTRUCTION (ret);
}

gdouble
cpg_instruction_number_get_value (CpgInstructionNumber *number)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_NUMBER (number), 0.0);

	return number->priv->value;
}

void
cpg_instruction_number_set_value (CpgInstructionNumber *number,
                                  gdouble               value)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_NUMBER (number));

	number->priv->value = value;
}
