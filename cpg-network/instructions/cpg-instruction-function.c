#include "cpg-instruction-function.h"
#include <cpg-network/cpg-math.h>

#define CPG_INSTRUCTION_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_FUNCTION, CpgInstructionFunctionPrivate))

struct _CpgInstructionFunctionPrivate
{
	guint id;
	gchar *name;
	gint arguments;
};

G_DEFINE_TYPE (CpgInstructionFunction, cpg_instruction_function, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_function_finalize (CpgMiniObject *object)
{
	CpgInstructionFunction *function;

	function = CPG_INSTRUCTION_FUNCTION (object);
	g_free (function->priv->name);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_function_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_function_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionFunction *self;
	CpgInstructionFunction const *src;

	src = CPG_INSTRUCTION_FUNCTION_CONST (object);

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_function_parent_class)->copy (object);

	self = CPG_INSTRUCTION_FUNCTION (ret);

	self->priv->id = src->priv->id;
	self->priv->name = g_strdup (src->priv->name);
	self->priv->arguments = src->priv->arguments;

	return ret;
}

static gchar *
cpg_instruction_function_to_string (CpgInstruction *instruction)
{
	CpgInstructionFunction *self;

	self = CPG_INSTRUCTION_FUNCTION (instruction);

	return g_strdup_printf ("FUN (%s)", self->priv->name);
}

static void
cpg_instruction_function_execute (CpgInstruction *instruction,
                                  CpgStack       *stack)
{
	CpgInstructionFunction *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionFunction *)instruction;
	cpg_math_function_execute (self->priv->id, self->priv->arguments, stack);
}

static gint
cpg_instruction_function_get_stack_count (CpgInstruction *instruction)
{
	CpgInstructionFunction *self;
	gint fromstack;

	self = CPG_INSTRUCTION_FUNCTION (instruction);
	fromstack = self->priv->arguments;

	return -fromstack + 1;
}

static gboolean
cpg_instruction_function_equal (CpgInstruction *i1,
                                CpgInstruction *i2)
{
	CpgInstructionFunction *f1 = CPG_INSTRUCTION_FUNCTION (i1);
	CpgInstructionFunction *f2 = CPG_INSTRUCTION_FUNCTION (i2);

	return f1->priv->id == f2->priv->id;
}

static void
cpg_instruction_function_class_init (CpgInstructionFunctionClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_function_finalize;
	object_class->copy = cpg_instruction_function_copy;

	inst_class->to_string = cpg_instruction_function_to_string;
	inst_class->execute = cpg_instruction_function_execute;
	inst_class->get_stack_count = cpg_instruction_function_get_stack_count;
	inst_class->equal = cpg_instruction_function_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionFunctionPrivate));
}

static void
cpg_instruction_function_init (CpgInstructionFunction *self)
{
	self->priv = CPG_INSTRUCTION_FUNCTION_GET_PRIVATE (self);
}

CpgInstruction *
cpg_instruction_function_new (guint        id,
                              const gchar *name,
                              gint         arguments)
{
	CpgMiniObject *ret;
	CpgInstructionFunction *func;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_FUNCTION);
	func = CPG_INSTRUCTION_FUNCTION (ret);

	func->priv->id = id;
	func->priv->name = g_strdup (name);
	func->priv->arguments = arguments;

	return CPG_INSTRUCTION (ret);
}

void
cpg_instruction_function_set_id (CpgInstructionFunction *func,
                                 guint                   id)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func));

	func->priv->id = id;
}

/**
 * cpg_instruction_function_set_name:
 * @func: A #CpgInstructionFunction
 * @name: (transfer none): The function name
 *
 * Set the function name.
 *
 **/

void
cpg_instruction_function_set_name (CpgInstructionFunction *func,
                                   gchar const            *name)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func));

	g_free (func->priv->name);
	func->priv->name = g_strdup (name);
}

void
cpg_instruction_function_set_arguments (CpgInstructionFunction *func,
                                        gint                    arguments)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func));

	func->priv->arguments = arguments;
}

guint
cpg_instruction_function_get_id (CpgInstructionFunction *func)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func), 0);

	return func->priv->id;
}

/**
 * cpg_instruction_function_get_name:
 * @func: A #CpgInstructionFunction
 *
 * Get the function name.
 *
 * Returns: (transfer none): the function name
 *
 **/
gchar const *
cpg_instruction_function_get_name (CpgInstructionFunction *func)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func), NULL);

	return func->priv->name;
}

gint
cpg_instruction_function_get_arguments (CpgInstructionFunction *func)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_FUNCTION (func), 0);

	return func->priv->arguments;
}
