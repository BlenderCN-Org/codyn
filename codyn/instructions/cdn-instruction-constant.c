#include "cdn-instruction-constant.h"
#include <codyn/cdn-math.h>

#define CDN_INSTRUCTION_CONSTANT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_CONSTANT, CdnInstructionConstantPrivate))

struct _CdnInstructionConstantPrivate
{
	gchar *symbol;
};

G_DEFINE_TYPE (CdnInstructionConstant, cdn_instruction_constant, CDN_TYPE_INSTRUCTION_NUMBER)

static void
cdn_instruction_constant_finalize (CdnMiniObject *object)
{
	CdnInstructionConstant *self;

	self = CDN_INSTRUCTION_CONSTANT (object);

	g_free (self->priv->symbol);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_constant_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_constant_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionConstant *self;
	CdnInstructionConstant const *src;

	src = CDN_INSTRUCTION_CONSTANT_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_constant_parent_class)->copy (object);

	self = CDN_INSTRUCTION_CONSTANT (ret);
	self->priv->symbol = g_strdup (src->priv->symbol);

	return ret;
}

static gchar *
cdn_instruction_constant_to_string (CdnInstruction *instruction)
{
	CdnInstructionConstant *self;

	self = CDN_INSTRUCTION_CONSTANT (instruction);

	return g_strdup_printf ("CON (%s = %.3f)",
	                        self->priv->symbol,
	                        cdn_instruction_number_get_value (CDN_INSTRUCTION_NUMBER (instruction)));
}

static gboolean
cdn_instruction_constant_equal (CdnInstruction *i1,
                                CdnInstruction *i2)
{
	CdnInstructionConstant *n1 = CDN_INSTRUCTION_CONSTANT (i1);
	CdnInstructionConstant *n2 = CDN_INSTRUCTION_CONSTANT (i2);

	return g_strcmp0 (n1->priv->symbol, n2->priv->symbol) == 0;
}

static void
cdn_instruction_constant_class_init (CdnInstructionConstantClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_constant_finalize;
	object_class->copy = cdn_instruction_constant_copy;

	inst_class->to_string = cdn_instruction_constant_to_string;
	inst_class->equal = cdn_instruction_constant_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionConstantPrivate));
}

static void
cdn_instruction_constant_init (CdnInstructionConstant *self)
{
	self->priv = CDN_INSTRUCTION_CONSTANT_GET_PRIVATE (self);
	self->priv->symbol = NULL;
}

CdnInstruction *
cdn_instruction_constant_new (gchar const *symbol)
{
	CdnMiniObject *ret;
	CdnInstructionConstant *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_CONSTANT);
	self = CDN_INSTRUCTION_CONSTANT (ret);

	cdn_instruction_constant_set_symbol (self, symbol);

	return CDN_INSTRUCTION (ret);
}

gchar const *
cdn_instruction_constant_get_symbol (CdnInstructionConstant *constant)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_CONSTANT (constant), NULL);

	return constant->priv->symbol;
}

void
cdn_instruction_constant_set_symbol (CdnInstructionConstant *constant,
                                     gchar const            *symbol)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_CONSTANT (constant));
	g_return_if_fail (symbol != NULL);

	g_free (constant->priv->symbol);

	constant->priv->symbol = g_strdup (symbol);

	cdn_instruction_number_set_value (CDN_INSTRUCTION_NUMBER (constant),
	                                  cdn_math_constant_lookup (symbol, NULL));
}
