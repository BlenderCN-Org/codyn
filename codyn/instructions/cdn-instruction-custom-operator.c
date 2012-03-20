#include "cdn-instruction-custom-operator.h"
#include <codyn/cdn-expression.h>

#define CDN_INSTRUCTION_CUSTOM_OPERATOR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR, CdnInstructionCustomOperatorPrivate))

struct _CdnInstructionCustomOperatorPrivate
{
	CdnOperator *op;
};

G_DEFINE_TYPE (CdnInstructionCustomOperator, cdn_instruction_custom_operator, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_custom_operator_finalize (CdnMiniObject *object)
{
	CdnInstructionCustomOperator *op;

	op = CDN_INSTRUCTION_CUSTOM_OPERATOR (object);

	g_object_unref (op->priv->op);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_operator_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_custom_operator_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionCustomOperator *op;
	CdnInstructionCustomOperator const *src;

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_custom_operator_parent_class)->copy (object);

	src = CDN_INSTRUCTION_CUSTOM_OPERATOR_CONST (object);
	op = CDN_INSTRUCTION_CUSTOM_OPERATOR (ret);

	op->priv->op = cdn_operator_copy (src->priv->op);

	return ret;
}

static gchar *
cdn_instruction_custom_operator_to_string (CdnInstruction *instruction)
{
	CdnInstructionCustomOperator *self;

	self = CDN_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	gchar const *name = cdn_operator_get_name (self->priv->op);
	gchar *ret = g_strdup_printf ("OPC (%s)", name);

	return ret;
}

static void
cdn_instruction_custom_operator_execute (CdnInstruction *instruction,
                                         CdnStack       *stack)
{
	CdnInstructionCustomOperator *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionCustomOperator *)instruction;
	cdn_operator_execute (self->priv->op, stack);
}

static CdnStackManipulation const *
cdn_instruction_custom_operator_get_stack_manipulation (CdnInstruction  *instruction,
                                                        GError         **error)
{
	CdnInstructionCustomOperator *self;

	self = CDN_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	return cdn_operator_get_stack_manipulation (self->priv->op);
}

static GSList *
extract_dependencies (GSList const *expressions,
                      GSList       *ret)
{
	while (expressions)
	{
		ret = g_slist_prepend (ret, expressions->data);
		expressions = g_slist_next (expressions);
	}

	return ret;
}

static void
extract_function_dependencies (CdnFunction  *f,
                               GSList      **dependencies)
{
	CdnExpression *fe;

	fe = cdn_function_get_expression (f);

	if (fe)
	{
		*dependencies = g_slist_prepend (*dependencies, fe);
	}
}

static GSList *
cdn_instruction_custom_operator_get_dependencies (CdnInstruction *instruction)
{
	CdnInstructionCustomOperator *self;
	gint i;
	gint num;
	GSList *dependencies = NULL;

	self = CDN_INSTRUCTION_CUSTOM_OPERATOR (instruction);

	num = cdn_operator_num_indices (self->priv->op);

	for (i = 0; i < num; ++i)
	{
		dependencies =
			extract_dependencies (cdn_operator_get_indices (self->priv->op, i),
			                      dependencies);
	}

	num = cdn_operator_num_expressions (self->priv->op);

	for (i = 0; i < num; ++i)
	{
		dependencies =
			extract_dependencies (cdn_operator_get_expressions (self->priv->op, i),
			                      dependencies);
	}

	cdn_operator_foreach_function (self->priv->op,
	                               (CdnForeachFunctionFunc)extract_function_dependencies,
	                               &dependencies);

	return dependencies;
}

static gboolean
cdn_instruction_custom_operator_equal (CdnInstruction *a,
                                       CdnInstruction *b,
                                       gboolean        asstring)
{
	CdnInstructionCustomOperator *ac;
	CdnInstructionCustomOperator *bc;

	ac = CDN_INSTRUCTION_CUSTOM_OPERATOR (a);
	bc = CDN_INSTRUCTION_CUSTOM_OPERATOR (b);

	return cdn_operator_equal (ac->priv->op,
	                           bc->priv->op,
	                           asstring);
}

static void
cdn_instruction_custom_operator_class_init (CdnInstructionCustomOperatorClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_custom_operator_finalize;
	object_class->copy = cdn_instruction_custom_operator_copy;

	inst_class->to_string = cdn_instruction_custom_operator_to_string;
	inst_class->execute = cdn_instruction_custom_operator_execute;
	inst_class->get_stack_manipulation = cdn_instruction_custom_operator_get_stack_manipulation;
	inst_class->get_dependencies = cdn_instruction_custom_operator_get_dependencies;
	inst_class->equal = cdn_instruction_custom_operator_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionCustomOperatorPrivate));
}

static void
cdn_instruction_custom_operator_init (CdnInstructionCustomOperator *self)
{
	self->priv = CDN_INSTRUCTION_CUSTOM_OPERATOR_GET_PRIVATE (self);
}

/**
 * cdn_instruction_custom_operator_new:
 * @op: A #CdnOperator
 *
 * Create a new #CdnInstructionCustomOperator.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_custom_operator_new (CdnOperator *op)
{
	CdnMiniObject *ret;
	CdnInstructionCustomOperator *oper;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_CUSTOM_OPERATOR);
	oper = CDN_INSTRUCTION_CUSTOM_OPERATOR (ret);

	oper->priv->op = g_object_ref (op);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_custom_operator_get_operator:
 * @op: A #CdnInstructionCustomOperator
 *
 * Get the operator executed by the instruction.
 *
 * Returns: (transfer none): A #CdnOperator
 *
 **/
CdnOperator *
cdn_instruction_custom_operator_get_operator (CdnInstructionCustomOperator *op)
{
	return op->priv->op;
}
