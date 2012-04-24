#include "cdn-instruction-custom-function.h"

G_DEFINE_TYPE (CdnInstructionCustomFunction, cdn_instruction_custom_function, CDN_TYPE_INSTRUCTION)

#define CDN_INSTRUCTION_CUSTOM_FUNCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION, CdnInstructionCustomFunctionPrivate))

struct _CdnInstructionCustomFunctionPrivate
{
	CdnFunction *function;

	CdnStackManipulation smanip;
	gint push_manip[2];
};

static void
cdn_instruction_custom_function_finalize (CdnMiniObject *object)
{
	CdnInstructionCustomFunction *self;

	self = CDN_INSTRUCTION_CUSTOM_FUNCTION (object);

	g_free (self->priv->smanip.pop_dims);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_function_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_custom_function_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionCustomFunction *func;
	CdnInstructionCustomFunction const *src;
	gint i;

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_function_parent_class)->copy (object);

	src = CDN_INSTRUCTION_CUSTOM_FUNCTION_CONST (object);
	func = CDN_INSTRUCTION_CUSTOM_FUNCTION (ret);

	func->priv->function = g_object_ref (src->priv->function);

	func->priv->push_manip[0] = src->priv->push_manip[0];
	func->priv->push_manip[1] = src->priv->push_manip[1];

	func->priv->smanip.num_pop = src->priv->smanip.num_pop;
	func->priv->smanip.pop_dims = g_new (gint, func->priv->smanip.num_pop * 2);

	for (i = 0; i < func->priv->smanip.num_pop * 2; ++i)
	{
		func->priv->smanip.pop_dims[i] = src->priv->smanip.pop_dims[i];
	}

	return ret;
}

static gchar *
cdn_instruction_custom_function_to_string (CdnInstruction *instruction)
{
	CdnInstructionCustomFunction *self;

	self = CDN_INSTRUCTION_CUSTOM_FUNCTION (instruction);

	return g_strdup_printf ("FNC (%s)", cdn_object_get_id (CDN_OBJECT (self->priv->function)));
}

static void
cdn_instruction_custom_function_execute (CdnInstruction *instruction,
                                         CdnStack       *stack)
{
	CdnInstructionCustomFunction *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionCustomFunction *)instruction;

	cdn_function_execute (self->priv->function,
	                      self->priv->smanip.num_pop,
	                      self->priv->smanip.pop_dims,
	                      stack);
}

static CdnStackManipulation const *
cdn_instruction_custom_function_get_stack_manipulation (CdnInstruction  *instruction,
                                                        GError         **error)
{
	CdnInstructionCustomFunction *self;

	self = CDN_INSTRUCTION_CUSTOM_FUNCTION (instruction);

	if (self->priv->function)
	{
		cdn_function_get_dimension (self->priv->function,
		                            &(self->priv->push_manip[0]),
		                            &(self->priv->push_manip[1]));
	}

	return &self->priv->smanip;
}

static gboolean
cdn_instruction_custom_function_equal (CdnInstruction *i1,
                                       CdnInstruction *i2,
                                       gboolean        asstring)
{
	CdnInstructionCustomFunction *f1 = CDN_INSTRUCTION_CUSTOM_FUNCTION (i1);
	CdnInstructionCustomFunction *f2 = CDN_INSTRUCTION_CUSTOM_FUNCTION (i2);

	if (asstring)
	{
		return g_strcmp0 (cdn_object_get_id (CDN_OBJECT (f1->priv->function)),
		                  cdn_object_get_id (CDN_OBJECT (f2->priv->function))) == 0;
	}
	else
	{
		return f1->priv->function == f2->priv->function;
	}
}

static void
cdn_instruction_custom_function_class_init (CdnInstructionCustomFunctionClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->copy = cdn_instruction_custom_function_copy;
	object_class->finalize = cdn_instruction_custom_function_finalize;

	inst_class->to_string = cdn_instruction_custom_function_to_string;
	inst_class->execute = cdn_instruction_custom_function_execute;
	inst_class->get_stack_manipulation = cdn_instruction_custom_function_get_stack_manipulation;
	inst_class->equal = cdn_instruction_custom_function_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionCustomFunctionPrivate));
}

static void
cdn_instruction_custom_function_init (CdnInstructionCustomFunction *self)
{
	self->priv = CDN_INSTRUCTION_CUSTOM_FUNCTION_GET_PRIVATE (self);

	self->priv->smanip.push_dims = self->priv->push_manip;
	self->priv->smanip.num_push = 1;
}

static void
set_arguments (CdnInstructionCustomFunction *function,
               gint                          arguments,
               gint                         *argdim)
{
	gint i;

	g_return_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (function));

	g_free (function->priv->smanip.pop_dims);
	function->priv->smanip.pop_dims = g_new (gint, arguments * 2);

	for (i = 0; i < arguments * 2; ++i)
	{
		function->priv->smanip.pop_dims[i] = argdim[i];
	}

	function->priv->smanip.num_pop = arguments;
}

static void
set_function (CdnInstructionCustomFunction *function,
              CdnFunction                  *func)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (function));
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

CdnInstruction *
cdn_instruction_custom_function_new (CdnFunction *function,
                                     gint         arguments,
                                     gint        *argdim)
{
	CdnInstructionCustomFunction *custom;

	custom = CDN_INSTRUCTION_CUSTOM_FUNCTION (
		cdn_mini_object_new (CDN_TYPE_INSTRUCTION_CUSTOM_FUNCTION));

	set_function (custom, function);
	set_arguments (custom, arguments, argdim);

	return CDN_INSTRUCTION (custom);
}

/**
 * cdn_instruction_custom_function_get_function:
 * @function: A #CdnInstructionCustomFunction
 *
 * Get the function executed by the instruction.
 *
 * Returns: (transfer none): A #CdnFunction
 *
 **/
CdnFunction *
cdn_instruction_custom_function_get_function (CdnInstructionCustomFunction *function)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (function), NULL);

	return function->priv->function;
}

void
cdn_instruction_custom_function_set_function (CdnInstructionCustomFunction *function,
                                              CdnFunction                  *f)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (function));

	set_function (function, f);
}
