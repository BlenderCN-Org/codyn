#include "cpg-instruction-variadic-function.h"

#include <cpg-network/cpg-math.h>

#define CPG_INSTRUCTION_VARIADIC_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION, CpgInstructionVariadicFunctionPrivate))

struct _CpgInstructionVariadicFunctionPrivate
{
	gboolean cached;
	gdouble cached_result;
};

G_DEFINE_TYPE (CpgInstructionVariadicFunction, cpg_instruction_variadic_function, CPG_TYPE_INSTRUCTION_FUNCTION)

static void
cpg_instruction_variadic_function_finalize (CpgMiniObject *object)
{
	CpgInstructionVariadicFunction *self;

	self = CPG_INSTRUCTION_VARIADIC_FUNCTION (object);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_variadic_function_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_variadic_function_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionVariadicFunction *self;
	CpgInstructionVariadicFunction const *src;

	src = CPG_INSTRUCTION_VARIADIC_FUNCTION_CONST (object);

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_variadic_function_parent_class)->copy (object);

	self = CPG_INSTRUCTION_VARIADIC_FUNCTION (ret);

	return ret;
}

static gchar *
cpg_instruction_variadic_function_to_string (CpgInstruction *instruction)
{
	CpgInstructionVariadicFunction *self;

	self = CPG_INSTRUCTION_VARIADIC_FUNCTION (instruction);

	return g_strdup_printf ("VAR (%s)",
	                        cpg_instruction_function_get_name (CPG_INSTRUCTION_FUNCTION (instruction)));
}

static void
cpg_instruction_variadic_function_execute (CpgInstruction *instruction,
                                           CpgStack       *stack)
{
	CpgInstructionVariadicFunction *self;
	CpgInstructionFunction *func;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionVariadicFunction *)instruction;
	func = (CpgInstructionFunction *)instruction;

	if (self->priv->cached)
	{
		gint i;
		gint num = cpg_instruction_function_get_arguments (func) +
		           cpg_instruction_function_get_variable (func);

		for (i = 0; i < num; ++i)
		{
			cpg_stack_pop (stack);
		}

		cpg_stack_push (stack, self->priv->cached_result);
	}
	else
	{
		cpg_math_function_execute (cpg_instruction_function_get_id (func),
		                           stack);

		/* Cache the result */
		self->priv->cached_result = cpg_stack_at (stack,
		                                          cpg_stack_count (stack) - 1);

		self->priv->cached = TRUE;
	}
}

static void
cpg_instruction_variadic_function_class_init (CpgInstructionVariadicFunctionClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_variadic_function_finalize;
	object_class->copy = cpg_instruction_variadic_function_copy;

	inst_class->to_string = cpg_instruction_variadic_function_to_string;
	inst_class->execute = cpg_instruction_variadic_function_execute;

	g_type_class_add_private (object_class, sizeof(CpgInstructionVariadicFunctionPrivate));
}

static void
cpg_instruction_variadic_function_init (CpgInstructionVariadicFunction *self)
{
	self->priv = CPG_INSTRUCTION_VARIADIC_FUNCTION_GET_PRIVATE (self);
}

CpgInstruction *
cpg_instruction_variadic_function_new (guint        id,
                                       gchar const *name,
                                       gint         arguments,
                                       gboolean     variable)
{
	CpgMiniObject *ret;
	CpgInstructionFunction *func;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_VARIADIC_FUNCTION);
	func = CPG_INSTRUCTION_FUNCTION (ret);

	cpg_instruction_function_set_id (func, id);
	cpg_instruction_function_set_name (func, name);
	cpg_instruction_function_set_arguments (func, arguments);
	cpg_instruction_function_set_variable (func, variable);

	return CPG_INSTRUCTION (ret);
}

void
cpg_instruction_variadic_function_reset_cache (CpgInstructionVariadicFunction *self)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_VARIADIC_FUNCTION (self));

	self->priv->cached = FALSE;
}
