#include "cdn-instruction-custom-function-ref.h"

G_DEFINE_TYPE (CdnInstructionCustomFunctionRef, cdn_instruction_custom_function_ref, CDN_TYPE_INSTRUCTION)

#define CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CdnInstructionCustomFunctionRefPrivate))

struct _CdnInstructionCustomFunctionRefPrivate
{
	CdnFunction *function;
	CdnStackManipulation smanip;
};

static void
cdn_instruction_custom_function_ref_finalize (CdnMiniObject *object)
{
	g_object_unref (CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (object)->priv->function);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_function_ref_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_custom_function_ref_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionCustomFunctionRef *func;
	CdnInstructionCustomFunctionRef const *src;

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_function_ref_parent_class)->copy (object);

	src = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_CONST (object);
	func = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (ret);

	func->priv->function = g_object_ref (src->priv->function);

	return ret;
}

static gchar *
cdn_instruction_custom_function_ref_to_string (CdnInstruction *instruction)
{
	CdnInstructionCustomFunctionRef *self;

	self = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction);

	return g_strdup_printf ("&FNC (%s)", cdn_object_get_id (CDN_OBJECT (self->priv->function)));
}

static void
cdn_instruction_custom_function_ref_execute (CdnInstruction *instruction,
                                             CdnStack       *stack)
{
}

static CdnStackManipulation const *
cdn_instruction_custom_function_ref_get_stack_manipulation (CdnInstruction *instruction)
{
	CdnInstructionCustomFunctionRef *self;

	self = (CdnInstructionCustomFunctionRef *)instruction;

	return &self->priv->smanip;
}

static gboolean
cdn_instruction_custom_function_ref_equal (CdnInstruction *i1,
                                       CdnInstruction *i2)
{
	CdnInstructionCustomFunctionRef *f1 = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (i1);
	CdnInstructionCustomFunctionRef *f2 = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (i2);

	return f1->priv->function == f2->priv->function;
}

static void
cdn_instruction_custom_function_ref_class_init (CdnInstructionCustomFunctionRefClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->copy = cdn_instruction_custom_function_ref_copy;
	object_class->finalize = cdn_instruction_custom_function_ref_finalize;

	inst_class->to_string = cdn_instruction_custom_function_ref_to_string;
	inst_class->execute = cdn_instruction_custom_function_ref_execute;
	inst_class->get_stack_manipulation = cdn_instruction_custom_function_ref_get_stack_manipulation;
	inst_class->equal = cdn_instruction_custom_function_ref_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionCustomFunctionRefPrivate));
}

static void
cdn_instruction_custom_function_ref_init (CdnInstructionCustomFunctionRef *self)
{
	self->priv = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_PRIVATE (self);

	self->priv->smanip.num_pop = 0;
	self->priv->smanip.num_push = 0;
}

CdnInstruction *
cdn_instruction_custom_function_ref_new (CdnFunction *function)
{
	CdnInstructionCustomFunctionRef *custom;

	custom = CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (
		cdn_mini_object_new (CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF));

	cdn_instruction_custom_function_ref_set_function (custom, function);

	return CDN_INSTRUCTION (custom);
}

/**
 * cdn_instruction_custom_function_ref_set_function:
 * @function: A #CdnInstructionCustomFunctionRef
 * @func: (transfer full): A #CdnFunction
 *
 * Set the function executed by the instruction.
 *
 **/
void
cdn_instruction_custom_function_ref_set_function (CdnInstructionCustomFunctionRef *function,
                                                  CdnFunction                  *func)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (function));
	g_return_if_fail (function == NULL || CDN_IS_FUNCTION (func));

	if (function->priv->function)
	{
		g_object_unref (function->priv->function);
		function->priv->function = NULL;
	}

	if (func)
	{
		function->priv->function = g_object_ref (func);
	}
}

/**
 * cdn_instruction_custom_function_ref_get_function:
 * @function: A #CdnInstructionCustomFunctionRef
 *
 * Get the function executed by the instruction.
 *
 * Returns: (transfer none): A #CdnFunction
 *
 **/
CdnFunction *
cdn_instruction_custom_function_ref_get_function (CdnInstructionCustomFunctionRef *function)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (function), NULL);

	return function->priv->function;
}
