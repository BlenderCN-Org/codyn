#include "cdn-instruction-variable.h"

#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-edge-action.h>
#include <codyn/cdn-math.h>
#include <codyn/cdn-expression-tree-iter.h>
#include <codyn/cdn-compile-error.h>

#define CDN_INSTRUCTION_VARIABLE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_VARIABLE, CdnInstructionVariablePrivate))

struct _CdnInstructionVariablePrivate
{
	CdnVariable *property;
	CdnInstructionVariableBinding binding;
	CdnStackManipulation smanip;
};

G_DEFINE_TYPE (CdnInstructionVariable, cdn_instruction_variable, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_variable_finalize (CdnMiniObject *object)
{
	CdnInstructionVariable *self;

	self = CDN_INSTRUCTION_VARIABLE (object);

	cdn_instruction_variable_set_variable (self, NULL);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_variable_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_variable_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionVariable *self;
	CdnInstructionVariable const *src;

	src = CDN_INSTRUCTION_VARIABLE_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_variable_parent_class)->copy (object);

	self = CDN_INSTRUCTION_VARIABLE (ret);

	cdn_instruction_variable_set_variable (self, src->priv->property);
	self->priv->binding = src->priv->binding;

	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	return ret;
}

static gchar *
cdn_instruction_variable_to_string (CdnInstruction *instruction)
{
	CdnInstructionVariable *self;
	gchar *s;
	gchar *ret;

	self = CDN_INSTRUCTION_VARIABLE (instruction);
	s = cdn_variable_get_full_name_for_display (self->priv->property);

	ret = g_strdup_printf ("PRP (%s)", s);
	g_free (s);

	return ret;
}

static void
cdn_instruction_variable_execute (CdnInstruction *instruction,
                                  CdnStack       *stack)
{
	gdouble const *values;
	CdnDimension dim;

	CdnInstructionVariable *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionVariable *)instruction;

	values = cdn_variable_get_values (self->priv->property, &dim);
	cdn_stack_pushn (stack, values, cdn_dimension_size (&dim));
}

static CdnStackManipulation const *
cdn_instruction_variable_get_stack_manipulation (CdnInstruction  *instruction,
                                                 GError         **error)
{
	CdnInstructionVariable *self;

	self = (CdnInstructionVariable *)instruction;

	if (self->priv->property)
	{
		CdnExpression *expr;

		expr = cdn_variable_get_expression (self->priv->property);

		if (!cdn_expression_get_dimension (expr,
		                                   &self->priv->smanip.push.dimension))
		{
			gchar *name;

			name = cdn_variable_get_full_name_for_display (self->priv->property);

			g_set_error (error,
			             CDN_COMPILE_ERROR_TYPE,
			             CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
			             "Could not statically determine dimensionality of `%s'",
			             name);

			g_free (name);
			return NULL;
		}
	}

	return &self->priv->smanip;
}

static GSList *
cdn_instruction_variable_get_dependencies (CdnInstruction *instruction)
{
	return g_slist_prepend (NULL, cdn_variable_get_expression (CDN_INSTRUCTION_VARIABLE (instruction)->priv->property));
}

static gboolean
cdn_instruction_variable_equal (CdnInstruction *i1,
                                CdnInstruction *i2,
                                gboolean        asstring)
{
	CdnInstructionVariable *p1 = CDN_INSTRUCTION_VARIABLE (i1);
	CdnInstructionVariable *p2 = CDN_INSTRUCTION_VARIABLE (i2);

	if (asstring)
	{
		return g_strcmp0 (cdn_variable_get_name (p1->priv->property),
		                  cdn_variable_get_name (p2->priv->property)) == 0 &&
		       p1->priv->binding == p2->priv->binding;
	}
	else
	{
		return p1->priv->property == p2->priv->property;
	}
}

static void
cdn_instruction_variable_class_init (CdnInstructionVariableClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_variable_finalize;
	object_class->copy = cdn_instruction_variable_copy;

	inst_class->to_string = cdn_instruction_variable_to_string;
	inst_class->execute = cdn_instruction_variable_execute;
	inst_class->get_stack_manipulation = cdn_instruction_variable_get_stack_manipulation;
	inst_class->get_dependencies = cdn_instruction_variable_get_dependencies;
	inst_class->equal = cdn_instruction_variable_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionVariablePrivate));
}

static void
cdn_instruction_variable_init (CdnInstructionVariable *self)
{
	self->priv = CDN_INSTRUCTION_VARIABLE_GET_PRIVATE (self);
}

/**
 * cdn_instruction_variable_new_with_binding:
 * @property: (transfer none): A #CdnVariable
 * @binding: A #CdnInstructionVariableBinding
 *
 * Create a new #CdnInstructionVariable.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_variable_new_with_binding (CdnVariable                   *property,
                                           CdnInstructionVariableBinding  binding)
{
	CdnMiniObject *ret;
	CdnInstructionVariable *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_VARIABLE);
	self = CDN_INSTRUCTION_VARIABLE (ret);

	cdn_instruction_variable_set_variable (self, property);
	self->priv->binding = binding;

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_variable_new:
 * @property: (transfer none): A #CdnVariable
 *
 * Create a new #CdnInstructionVariable.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_variable_new (CdnVariable *property)
{
	return cdn_instruction_variable_new_with_binding (property,
	                                                  CDN_INSTRUCTION_VARIABLE_BINDING_NONE);
}

/**
 * cdn_instruction_variable_set_variable:
 * @instruction: A #CdnInstructionVariable
 * @property: (transfer none): A #CdnVariable
 *
 * Set the property executed by the instruction.
 *
 **/
void
cdn_instruction_variable_set_variable (CdnInstructionVariable *instruction,
                                       CdnVariable            *property)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));

	if (instruction->priv->property)
	{
		cdn_usable_unuse (CDN_USABLE (instruction->priv->property));
		g_object_unref (instruction->priv->property);

		instruction->priv->property = NULL;
	}

	if (property)
	{
		instruction->priv->property = g_object_ref_sink (property);
		cdn_usable_use (CDN_USABLE (instruction->priv->property));
	}
}

/**
 * cdn_instruction_variable_get_variable:
 * @instruction: A #CdnInstructionVariable
 *
 * Get the property executed by the instruction.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_instruction_variable_get_variable (CdnInstructionVariable *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction), NULL);
	return instruction->priv->property;
}

void
cdn_instruction_variable_set_binding (CdnInstructionVariable        *instruction,
                                      CdnInstructionVariableBinding  binding)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction));

	instruction->priv->binding = binding;
}

CdnInstructionVariableBinding
cdn_instruction_variable_get_binding (CdnInstructionVariable *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_VARIABLE (instruction),
	                      CDN_INSTRUCTION_VARIABLE_BINDING_NONE);

	return instruction->priv->binding;
}
