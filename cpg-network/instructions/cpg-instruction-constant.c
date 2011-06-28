#include "cpg-instruction-constant.h"
#include <cpg-network/cpg-math.h>

#define CPG_INSTRUCTION_CONSTANT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_CONSTANT, CpgInstructionConstantPrivate))

struct _CpgInstructionConstantPrivate
{
	gchar *symbol;
};

G_DEFINE_TYPE (CpgInstructionConstant, cpg_instruction_constant, CPG_TYPE_INSTRUCTION_NUMBER)

static void
cpg_instruction_constant_finalize (CpgMiniObject *object)
{
	CpgInstructionConstant *self;

	self = CPG_INSTRUCTION_CONSTANT (object);

	g_free (self->priv->symbol);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_constant_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_constant_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionConstant *self;
	CpgInstructionConstant const *src;

	src = CPG_INSTRUCTION_CONSTANT_CONST (object);

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_constant_parent_class)->copy (object);

	self = CPG_INSTRUCTION_CONSTANT (ret);
	self->priv->symbol = g_strdup (src->priv->symbol);

	return ret;
}

static gchar *
cpg_instruction_constant_to_string (CpgInstruction *instruction)
{
	CpgInstructionConstant *self;

	self = CPG_INSTRUCTION_CONSTANT (instruction);

	return g_strdup_printf ("CON (%s = %.3f)",
	                        self->priv->symbol,
	                        cpg_instruction_number_get_value (CPG_INSTRUCTION_NUMBER (instruction)));
}

static gboolean
cpg_instruction_constant_equal (CpgInstruction *i1,
                                CpgInstruction *i2)
{
	CpgInstructionConstant *n1 = CPG_INSTRUCTION_CONSTANT (i1);
	CpgInstructionConstant *n2 = CPG_INSTRUCTION_CONSTANT (i2);

	return g_strcmp0 (n1->priv->symbol, n2->priv->symbol) == 0;
}

static void
cpg_instruction_constant_class_init (CpgInstructionConstantClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_constant_finalize;
	object_class->copy = cpg_instruction_constant_copy;

	inst_class->to_string = cpg_instruction_constant_to_string;
	inst_class->equal = cpg_instruction_constant_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionConstantPrivate));
}

static void
cpg_instruction_constant_init (CpgInstructionConstant *self)
{
	self->priv = CPG_INSTRUCTION_CONSTANT_GET_PRIVATE (self);
	self->priv->symbol = NULL;
}

CpgInstruction *
cpg_instruction_constant_new (gchar const *symbol)
{
	CpgMiniObject *ret;
	CpgInstructionConstant *self;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_CONSTANT);
	self = CPG_INSTRUCTION_CONSTANT (ret);

	cpg_instruction_constant_set_symbol (self, symbol);

	return CPG_INSTRUCTION (ret);
}

gchar const *
cpg_instruction_constant_get_symbol (CpgInstructionConstant *constant)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CONSTANT (constant), NULL);

	return constant->priv->symbol;
}

void
cpg_instruction_constant_set_symbol (CpgInstructionConstant *constant,
                                     gchar const            *symbol)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_CONSTANT (constant));
	g_return_if_fail (symbol != NULL);

	g_free (constant->priv->symbol);

	constant->priv->symbol = g_strdup (symbol);

	cpg_instruction_number_set_value (CPG_INSTRUCTION_NUMBER (constant),
	                                  cpg_math_constant_lookup (symbol, NULL));
}
