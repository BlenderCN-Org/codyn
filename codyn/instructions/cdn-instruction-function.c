#include "cdn-instruction-function.h"
#include <codyn/cdn-math.h>

#define CDN_INSTRUCTION_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_FUNCTION, CdnInstructionFunctionPrivate))

struct _CdnInstructionFunctionPrivate
{
	guint id;
	gchar *name;

	CdnStackManipulation smanip;
	gint push_dims[2];
	GError *error;
};

G_DEFINE_TYPE (CdnInstructionFunction, cdn_instruction_function, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_function_finalize (CdnMiniObject *object)
{
	CdnInstructionFunction *function;

	function = CDN_INSTRUCTION_FUNCTION (object);
	g_free (function->priv->name);

	g_free (function->priv->smanip.pop_dims);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_function_parent_class)->finalize (object);
}

static void
copy_smanip (CdnStackManipulation const *src,
             CdnStackManipulation       *dest)
{
	gint i;

	dest->num_pop = src->num_pop;
	dest->num_push = src->num_push;

	dest->pop_dims = g_new (gint, dest->num_pop * 2);
	dest->push_dims = g_new (gint, dest->num_push * 2);

	for (i = 0; i < dest->num_pop * 2; ++i)
	{
		dest->pop_dims[i] = src->pop_dims[i];
	}

	for (i = 0; i < dest->num_push * 2; ++i)
	{
		dest->push_dims[i] = src->push_dims[i];
	}
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

	copy_smanip (&src->priv->smanip, &self->priv->smanip);

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
	                           self->priv->smanip.num_pop,
	                           self->priv->smanip.pop_dims,
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
	inst_class->get_stack_manipulation = cdn_instruction_function_get_stack_manipulation;
	inst_class->equal = cdn_instruction_function_equal;
	inst_class->get_is_commutative = cdn_instruction_function_get_is_commutative;

	g_type_class_add_private (object_class, sizeof(CdnInstructionFunctionPrivate));
}

static void
cdn_instruction_function_init (CdnInstructionFunction *self)
{
	self->priv = CDN_INSTRUCTION_FUNCTION_GET_PRIVATE (self);

	self->priv->smanip.push_dims = self->priv->push_dims;
}

CdnInstruction *
cdn_instruction_function_new (guint        id,
                              const gchar *name,
                              gint         arguments,
                              gint        *argdim)
{
	CdnMiniObject *ret;
	CdnInstructionFunction *func;
	gint i;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_FUNCTION);
	func = CDN_INSTRUCTION_FUNCTION (ret);

	func->priv->id = id;
	func->priv->name = g_strdup (name);

	func->priv->smanip.num_pop = arguments;
	func->priv->smanip.pop_dims = g_new (gint, arguments * 2);

	for (i = 0; i < arguments * 2; ++i)
	{
		func->priv->smanip.pop_dims[i] = argdim ? argdim[i] : 1;
	}

	func->priv->smanip.num_push = 1;

	cdn_math_function_get_stack_manipulation (id,
	                                          arguments,
	                                          argdim,
	                                          func->priv->push_dims,
	                                          &func->priv->smanip.extra_space,
	                                          &func->priv->error);

	return CDN_INSTRUCTION (ret);
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

gint *
cdn_instruction_function_get_arguments_dimension (CdnInstructionFunction *func)
{
	return func->priv->smanip.pop_dims;
}
