#include "cpg-instruction-custom-function-ref.h"

G_DEFINE_TYPE (CpgInstructionCustomFunctionRef, cpg_instruction_custom_function_ref, CPG_TYPE_INSTRUCTION)

#define CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF, CpgInstructionCustomFunctionRefPrivate))

struct _CpgInstructionCustomFunctionRefPrivate
{
	CpgFunction *function;
};

static void
cpg_instruction_custom_function_ref_finalize (CpgMiniObject *object)
{
	g_object_unref (CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (object)->priv->function);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_function_ref_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_custom_function_ref_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionCustomFunctionRef *func;
	CpgInstructionCustomFunctionRef const *src;

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_function_ref_parent_class)->copy (object);

	src = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_CONST (object);
	func = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (ret);

	func->priv->function = g_object_ref (src->priv->function);

	return ret;
}

static gchar *
cpg_instruction_custom_function_ref_to_string (CpgInstruction *instruction)
{
	CpgInstructionCustomFunctionRef *self;

	self = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (instruction);

	return g_strdup_printf ("&FNC (%s)", cpg_object_get_id (CPG_OBJECT (self->priv->function)));
}

static void
cpg_instruction_custom_function_ref_execute (CpgInstruction *instruction,
                                             CpgStack       *stack)
{
	cpg_stack_push (stack, 0);
}

static gint
cpg_instruction_custom_function_ref_get_stack_count (CpgInstruction *instruction)
{
	return 1;
}

static gboolean
cpg_instruction_custom_function_ref_equal (CpgInstruction *i1,
                                       CpgInstruction *i2)
{
	CpgInstructionCustomFunctionRef *f1 = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (i1);
	CpgInstructionCustomFunctionRef *f2 = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (i2);

	return f1->priv->function == f2->priv->function;
}

static void
cpg_instruction_custom_function_ref_class_init (CpgInstructionCustomFunctionRefClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->copy = cpg_instruction_custom_function_ref_copy;
	object_class->finalize = cpg_instruction_custom_function_ref_finalize;

	inst_class->to_string = cpg_instruction_custom_function_ref_to_string;
	inst_class->execute = cpg_instruction_custom_function_ref_execute;
	inst_class->get_stack_count = cpg_instruction_custom_function_ref_get_stack_count;
	inst_class->equal = cpg_instruction_custom_function_ref_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionCustomFunctionRefPrivate));
}

static void
cpg_instruction_custom_function_ref_init (CpgInstructionCustomFunctionRef *self)
{
	self->priv = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF_GET_PRIVATE (self);
}

CpgInstruction *
cpg_instruction_custom_function_ref_new (CpgFunction *function)
{
	CpgInstructionCustomFunctionRef *custom;

	custom = CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (
		cpg_mini_object_new (CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION_REF));

	cpg_instruction_custom_function_ref_set_function (custom, function);

	return CPG_INSTRUCTION (custom);
}

/**
 * cpg_instruction_custom_function_ref_set_function:
 * @function: A #CpgInstructionCustomFunctionRef
 * @func: (transfer full): A #CpgFunction
 *
 * Set the function executed by the instruction.
 *
 **/
void
cpg_instruction_custom_function_ref_set_function (CpgInstructionCustomFunctionRef *function,
                                                  CpgFunction                  *func)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (function));
	g_return_if_fail (function == NULL || CPG_IS_FUNCTION (func));

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
 * cpg_instruction_custom_function_ref_get_function:
 * @function: A #CpgInstructionCustomFunctionRef
 *
 * Get the function executed by the instruction.
 *
 * Returns: (transfer none): A #CpgFunction
 *
 **/
CpgFunction *
cpg_instruction_custom_function_ref_get_function (CpgInstructionCustomFunctionRef *function)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (function), NULL);

	return function->priv->function;
}
