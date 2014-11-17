#include "cdn-instruction-function.h"
#include <codyn/cdn-math.h>
#include <string.h>

#define CDN_INSTRUCTION_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunctionPrivate))

struct _CdnInstructionFunctionPrivate
{
	guint id;
	gchar *name;

	CdnStackManipulation smanip;
	GError *error;
};

/**
 * CdnInstructionFunction:
 *
 * Function call instruction.
 *
 * The #CdnInstructionFunction type is a special #CdnInstruction subtype
 * which represents a builtin function call. This instruction type is used
 * both for builtin functions and builtin operators (which can be seen as a
 * special type of function). This instruction is only used to call builtin
 * functions and operators. To call a custom defined user function, use
 * #CdnInstructionCustomFunction instead.
 *
 */

G_DEFINE_TYPE (CdnInstructionFunction, cdn_instruction_function, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_function_finalize (CdnMiniObject *object)
{
	CdnInstructionFunction *function;

	function = CDN_INSTRUCTION_FUNCTION (object);
	g_free (function->priv->name);

	cdn_stack_manipulation_destroy (&function->priv->smanip);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_function_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_function_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionFunction *self;
	CdnInstructionFunction const *src;

	src = CDN_INSTRUCTION_FUNCTION_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_function_parent_class)->copy (object);

	self = CDN_INSTRUCTION_FUNCTION (ret);

	self->priv->id = src->priv->id;
	self->priv->name = g_strdup (src->priv->name);

	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	return ret;
}

static gchar *
cdn_instruction_function_to_string (CdnInstruction *instruction)
{
	CdnInstructionFunction *self;

	self = CDN_INSTRUCTION_FUNCTION (instruction);

	return g_strdup_printf ("FUN (%s)", self->priv->name);
}

static void
cdn_instruction_function_execute (CdnInstruction *instruction,
                                  CdnStack       *stack)
{
	CdnInstructionFunction *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionFunction *)instruction;

	cdn_math_function_execute (self->priv->id,
	                           &self->priv->smanip.pop,
	                           stack);
}

static CdnStackManipulation const *
cdn_instruction_function_get_stack_manipulation (CdnInstruction  *instruction,
                                                 GError         **error)
{
	CdnInstructionFunction *self;

	self = CDN_INSTRUCTION_FUNCTION (instruction);

	if (self->priv->error && error)
	{
		*error = g_error_copy (self->priv->error);
	}

	return self->priv->error ? NULL : &self->priv->smanip;
}

static gboolean
cdn_instruction_function_equal (CdnInstruction *i1,
                                CdnInstruction *i2,
                                gboolean        asstring)
{
	CdnInstructionFunction *f1 = CDN_INSTRUCTION_FUNCTION (i1);
	CdnInstructionFunction *f2 = CDN_INSTRUCTION_FUNCTION (i2);

	return f1->priv->id == f2->priv->id;
}

static gboolean
cdn_instruction_function_get_is_commutative (CdnInstruction *instruction)
{
	CdnInstructionFunction *func;
	CdnMathFunctionType type;

	/* Direct cast to reduce overhead of GType cast */
	func = (CdnInstructionFunction *)instruction;
	type = (CdnMathFunctionType)cdn_instruction_function_get_id (func);

	return cdn_math_function_is_commutative (type,
	                                         &func->priv->smanip.pop);
}

static void
cdn_instruction_function_class_init (CdnInstructionFunctionClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_function_finalize;
	object_class->copy = cdn_instruction_function_copy;

	inst_class->to_string = cdn_instruction_function_to_string;
	inst_class->execute = cdn_instruction_function_execute;
	inst_class->get_stack_manipulation = cdn_instruction_function_get_stack_manipulation;
	inst_class->equal = cdn_instruction_function_equal;
	inst_class->get_is_commutative = cdn_instruction_function_get_is_commutative;

	g_type_class_add_private (object_class, sizeof(CdnInstructionFunctionPrivate));
}

static void
cdn_instruction_function_init (CdnInstructionFunction *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_instruction_function_get_instance_private (self);
	self->priv = CDN_INSTRUCTION_FUNCTION_GET_PRIVATE (self);
}

/**
 * cdn_instruction_function_new:
 * @id: the function id
 * @name: (allow-none): the function name
 * @args: the function arguments
 *
 * Create a new function call instruction. The @id refers to
 * #CdnMathFunctionType for builtin functions in libcodyn, but is of
 * type guint because it may also refer to external, dynamically loaded
 * functions which get their own identifier > #CDN_MATH_FUNCTION_TYPE_NUM.
 *
 * @name may be specified for the purpose of the distinction of multiple
 * aliases for the same underlying function. This is useful when expressions
 * are reconstructed from instruction streams, but is usually only used
 * internally. @args defines the arguments that this instruction takes from the
 * stack when executed.
 *
 * Returns: (transfer full) (type CdnInstructionFunction): a new #CdnInstructionFunction
 *
 */
CdnInstruction *
cdn_instruction_function_new (guint               id,
                              const gchar        *name,
                              CdnStackArgs const *args)
{
	CdnMiniObject *ret;
	CdnInstructionFunction *func;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_FUNCTION);
	func = CDN_INSTRUCTION_FUNCTION (ret);

	func->priv->id = id;
	func->priv->name = g_strdup (name);

	if (!func->priv->name)
	{
		func->priv->name = g_strdup (cdn_math_function_lookup_by_id (id,
		                                                             NULL));
	}

	cdn_stack_args_copy (&func->priv->smanip.pop, args);

	cdn_math_function_get_stack_manipulation (id,
	                                          &func->priv->smanip.pop,
	                                          &func->priv->smanip.push,
	                                          &func->priv->smanip.extra_space,
	                                          &func->priv->error);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_function_set_name:
 * @func: A #CdnInstructionFunction
 * @name: (allow-none): The function name
 *
 * Set the function name.
 *
 **/
void
cdn_instruction_function_set_name (CdnInstructionFunction *func,
                                   gchar const            *name)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_FUNCTION (func));

	g_free (func->priv->name);
	func->priv->name = g_strdup (name);
}

/**
 * cdn_instruction_function_get_id:
 * @func: the #CdnInstructionFunction
 *
 * Get the id of the instruction call. This usually refers to a
 * #CdnMathFunctionType, but may also refer to external, dynamically
 * loaded functions (which have ids > #CDN_MATH_FUNCTION_TYPE_NUM).
 *
 * Returns: the function id
 *
 */
guint
cdn_instruction_function_get_id (CdnInstructionFunction *func)
{
	/* Omit type check to increase speed */
	return func->priv->id;
}

/**
 * cdn_instruction_function_get_name:
 * @func: A #CdnInstructionFunction
 *
 * Get the function name.
 *
 * Returns: (transfer none): the function name
 *
 **/
gchar const *
cdn_instruction_function_get_name (CdnInstructionFunction *func)
{
	/* Omit type check to increase speed */
	return func->priv->name;
}
