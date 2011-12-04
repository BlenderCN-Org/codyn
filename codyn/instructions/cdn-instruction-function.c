#include "cdn-instruction-function.h"
#include <codyn/cdn-math.h>

#define CDN_INSTRUCTION_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunctionPrivate))

struct _CdnInstructionFunctionPrivate
{
	guint id;
	gchar *name;
	gint arguments;
};

G_DEFINE_TYPE (CdnInstructionFunction, cdn_instruction_function, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_function_finalize (CdnMiniObject *object)
{
	CdnInstructionFunction *function;

	function = CDN_INSTRUCTION_FUNCTION (object);
	g_free (function->priv->name);

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
	self->priv->arguments = src->priv->arguments;

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
	cdn_math_function_execute (self->priv->id, self->priv->arguments, stack);
}

static gint
cdn_instruction_function_get_stack_count (CdnInstruction *instruction)
{
	CdnInstructionFunction *self;
	gint fromstack;

	self = CDN_INSTRUCTION_FUNCTION (instruction);
	fromstack = self->priv->arguments;

	return -fromstack + 1;
}

static gboolean
cdn_instruction_function_equal (CdnInstruction *i1,
                                CdnInstruction *i2)
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

	return cdn_math_function_is_commutative (type);
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
	inst_class->get_stack_count = cdn_instruction_function_get_stack_count;
	inst_class->equal = cdn_instruction_function_equal;
	inst_class->get_is_commutative = cdn_instruction_function_get_is_commutative;

	g_type_class_add_private (object_class, sizeof(CdnInstructionFunctionPrivate));
}

static void
cdn_instruction_function_init (CdnInstructionFunction *self)
{
	self->priv = CDN_INSTRUCTION_FUNCTION_GET_PRIVATE (self);
}

CdnInstruction *
cdn_instruction_function_new (guint        id,
                              const gchar *name,
                              gint         arguments)
{
	CdnMiniObject *ret;
	CdnInstructionFunction *func;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_FUNCTION);
	func = CDN_INSTRUCTION_FUNCTION (ret);

	func->priv->id = id;
	func->priv->name = g_strdup (name);
	func->priv->arguments = arguments;

	return CDN_INSTRUCTION (ret);
}

void
cdn_instruction_function_set_id (CdnInstructionFunction *func,
                                 guint                   id)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_FUNCTION (func));

	func->priv->id = id;
}

/**
 * cdn_instruction_function_set_name:
 * @func: A #CdnInstructionFunction
 * @name: (transfer none): The function name
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

void
cdn_instruction_function_set_arguments (CdnInstructionFunction *func,
                                        gint                    arguments)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_FUNCTION (func));

	func->priv->arguments = arguments;
}

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

gint
cdn_instruction_function_get_arguments (CdnInstructionFunction *func)
{
	/* Omit type check to increase speed */
	return func->priv->arguments;
}
