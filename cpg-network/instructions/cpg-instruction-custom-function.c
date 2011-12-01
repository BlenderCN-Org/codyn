#include "cpg-instruction-custom-function.h"

G_DEFINE_TYPE (CpgInstructionCustomFunction, cpg_instruction_custom_function, CPG_TYPE_INSTRUCTION)

#define CPG_INSTRUCTION_CUSTOM_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CpgInstructionCustomFunctionPrivate))

struct _CpgInstructionCustomFunctionPrivate
{
	CpgFunction *function;
	gint arguments;
};

static void
cpg_instruction_custom_function_finalize (CpgMiniObject *object)
{
	g_object_unref (CPG_INSTRUCTION_CUSTOM_FUNCTION (object)->priv->function);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_function_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_custom_function_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionCustomFunction *func;
	CpgInstructionCustomFunction const *src;

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_custom_function_parent_class)->copy (object);

	src = CPG_INSTRUCTION_CUSTOM_FUNCTION_CONST (object);
	func = CPG_INSTRUCTION_CUSTOM_FUNCTION (ret);

	func->priv->function = g_object_ref (src->priv->function);
	func->priv->arguments = src->priv->arguments;

	return ret;
}

static gchar *
cpg_instruction_custom_function_to_string (CpgInstruction *instruction)
{
	CpgInstructionCustomFunction *self;

	self = CPG_INSTRUCTION_CUSTOM_FUNCTION (instruction);

	return g_strdup_printf ("FNC (%s)", cpg_object_get_id (CPG_OBJECT (self->priv->function)));
}

static void
cpg_instruction_custom_function_execute (CpgInstruction *instruction,
                                         CpgStack       *stack)
{
	CpgInstructionCustomFunction *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionCustomFunction *)instruction;
	cpg_function_execute (self->priv->function, self->priv->arguments, stack);
}

static gint
cpg_instruction_custom_function_get_stack_count (CpgInstruction *instruction)
{
	CpgInstructionCustomFunction *self;

	self = CPG_INSTRUCTION_CUSTOM_FUNCTION (instruction);

	return -self->priv->arguments + 1;
}

static gboolean
cpg_instruction_custom_function_equal (CpgInstruction *i1,
                                       CpgInstruction *i2)
{
	CpgInstructionCustomFunction *f1 = CPG_INSTRUCTION_CUSTOM_FUNCTION (i1);
	CpgInstructionCustomFunction *f2 = CPG_INSTRUCTION_CUSTOM_FUNCTION (i2);

	return f1->priv->function == f2->priv->function;
}

static GSList *
cpg_instruction_custom_function_get_dependencies (CpgInstruction *inst)
{
	CpgInstructionCustomFunction *f = CPG_INSTRUCTION_CUSTOM_FUNCTION (inst);

	if (!f->priv->function)
	{
		return NULL;
	}
	else
	{
		return g_slist_prepend (NULL,
		                        cpg_function_get_expression (f->priv->function));
	}
}

static void
cpg_instruction_custom_function_class_init (CpgInstructionCustomFunctionClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->copy = cpg_instruction_custom_function_copy;
	object_class->finalize = cpg_instruction_custom_function_finalize;

	inst_class->to_string = cpg_instruction_custom_function_to_string;
	inst_class->execute = cpg_instruction_custom_function_execute;
	inst_class->get_stack_count = cpg_instruction_custom_function_get_stack_count;
	inst_class->equal = cpg_instruction_custom_function_equal;
	inst_class->get_dependencies = cpg_instruction_custom_function_get_dependencies;

	g_type_class_add_private (object_class, sizeof(CpgInstructionCustomFunctionPrivate));
}

static void
cpg_instruction_custom_function_init (CpgInstructionCustomFunction *self)
{
	self->priv = CPG_INSTRUCTION_CUSTOM_FUNCTION_GET_PRIVATE (self);
}

CpgInstruction *
cpg_instruction_custom_function_new (CpgFunction *function,
                                     gint         arguments)
{
	CpgInstructionCustomFunction *custom;

	custom = CPG_INSTRUCTION_CUSTOM_FUNCTION (
		cpg_mini_object_new (CPG_TYPE_INSTRUCTION_CUSTOM_FUNCTION));

	cpg_instruction_custom_function_set_function (custom, function);
	cpg_instruction_custom_function_set_arguments (custom, arguments);

	return CPG_INSTRUCTION (custom);
}

/**
 * cpg_instruction_custom_function_set_function:
 * @function: A #CpgInstructionCustomFunction
 * @func: (transfer full): A #CpgFunction
 *
 * Set the function executed by the instruction.
 *
 **/
void
cpg_instruction_custom_function_set_function (CpgInstructionCustomFunction *function,
                                              CpgFunction                  *func)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (function));
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
 * cpg_instruction_custom_function_get_function:
 * @function: A #CpgInstructionCustomFunction
 *
 * Get the function executed by the instruction.
 *
 * Returns: (transfer none): A #CpgFunction
 *
 **/
CpgFunction *
cpg_instruction_custom_function_get_function (CpgInstructionCustomFunction *function)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (function), NULL);

	return function->priv->function;
}

void
cpg_instruction_custom_function_set_arguments (CpgInstructionCustomFunction *function,
                                               gint                          arguments)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (function));

	function->priv->arguments = arguments;
}

gint
cpg_instruction_custom_function_get_arguments (CpgInstructionCustomFunction *function)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (function), 0);

	return function->priv->arguments;
}
