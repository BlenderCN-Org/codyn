#include "cpg-instruction-property.h"
#include "cpg-instruction-operator.h"

#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-link-action.h>
#include <cpg-network/cpg-math.h>
#include <cpg-network/cpg-expression-tree-iter.h>

#define CPG_INSTRUCTION_PROPERTY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_PROPERTY, CpgInstructionPropertyPrivate))

struct _CpgInstructionPropertyPrivate
{
	CpgProperty *property;
	CpgInstructionPropertyBinding binding;
};

G_DEFINE_TYPE (CpgInstructionProperty, cpg_instruction_property, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_property_finalize (CpgMiniObject *object)
{
	CpgInstructionProperty *self;

	self = CPG_INSTRUCTION_PROPERTY (object);

	cpg_instruction_property_set_property (self, NULL);

	CPG_MINI_OBJECT_CLASS (cpg_instruction_property_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_property_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionProperty *self;
	CpgInstructionProperty const *src;

	src = CPG_INSTRUCTION_PROPERTY_CONST (object);

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_property_parent_class)->copy (object);

	self = CPG_INSTRUCTION_PROPERTY (ret);

	cpg_instruction_property_set_property (self, src->priv->property);
	self->priv->binding = src->priv->binding;

	return ret;
}

static gchar *
cpg_instruction_property_to_string (CpgInstruction *instruction)
{
	CpgInstructionProperty *self;
	gchar *s;
	gchar *ret;

	self = CPG_INSTRUCTION_PROPERTY (instruction);
	s = cpg_property_get_full_name_for_display (self->priv->property);

	ret = g_strdup_printf ("PRP (%s)", s);
	g_free (s);

	return ret;
}

static void
cpg_instruction_property_execute (CpgInstruction *instruction,
                                  CpgStack       *stack)
{
	CpgInstructionProperty *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionProperty *)instruction;

	cpg_stack_push (stack,
	                cpg_property_get_value (self->priv->property));
}

static gint
cpg_instruction_property_get_stack_count (CpgInstruction *instruction)
{
	return 1;
}

static GSList *
cpg_instruction_property_get_dependencies (CpgInstruction *instruction)
{
	return g_slist_prepend (NULL, cpg_property_get_expression (CPG_INSTRUCTION_PROPERTY (instruction)->priv->property));
}

static gboolean
cpg_instruction_property_equal (CpgInstruction *i1,
                                CpgInstruction *i2)
{
	CpgInstructionProperty *p1 = CPG_INSTRUCTION_PROPERTY (i1);
	CpgInstructionProperty *p2 = CPG_INSTRUCTION_PROPERTY (i2);

	return g_strcmp0 (cpg_property_get_name (p1->priv->property),
	                  cpg_property_get_name (p2->priv->property)) == 0 &&
	       p1->priv->binding == p2->priv->binding;
}

static void
cpg_instruction_property_class_init (CpgInstructionPropertyClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_property_finalize;
	object_class->copy = cpg_instruction_property_copy;

	inst_class->to_string = cpg_instruction_property_to_string;
	inst_class->execute = cpg_instruction_property_execute;
	inst_class->get_stack_count = cpg_instruction_property_get_stack_count;
	inst_class->get_dependencies = cpg_instruction_property_get_dependencies;
	inst_class->equal = cpg_instruction_property_equal;

	g_type_class_add_private (object_class, sizeof(CpgInstructionPropertyPrivate));
}

static void
cpg_instruction_property_init (CpgInstructionProperty *self)
{
	self->priv = CPG_INSTRUCTION_PROPERTY_GET_PRIVATE (self);
}

/**
 * cpg_instruction_property_new_with_binding:
 * @property: (transfer none): A #CpgProperty
 * @binding: A #CpgInstructionPropertyBinding
 *
 * Create a new #CpgInstructionProperty.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_property_new_with_binding (CpgProperty                   *property,
                                           CpgInstructionPropertyBinding  binding)
{
	CpgMiniObject *ret;
	CpgInstructionProperty *self;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_PROPERTY);
	self = CPG_INSTRUCTION_PROPERTY (ret);

	cpg_instruction_property_set_property (self, property);
	self->priv->binding = binding;

	return CPG_INSTRUCTION (ret);
}

/**
 * cpg_instruction_property_new:
 * @property: (transfer none): A #CpgProperty
 *
 * Create a new #CpgInstructionProperty.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_property_new (CpgProperty *property)
{
	return cpg_instruction_property_new_with_binding (property,
	                                                  CPG_INSTRUCTION_PROPERTY_BINDING_NONE);
}

/**
 * cpg_instruction_property_set_property:
 * @instruction: A #CpgInstructionProperty
 * @property: (transfer none): A #CpgProperty
 *
 * Set the property executed by the instruction.
 *
 **/
void
cpg_instruction_property_set_property (CpgInstructionProperty *instruction,
                                       CpgProperty            *property)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_PROPERTY (instruction));

	if (instruction->priv->property)
	{
		cpg_usable_unuse (CPG_USABLE (instruction->priv->property));
		g_object_unref (instruction->priv->property);

		instruction->priv->property = NULL;
	}

	if (property)
	{
		instruction->priv->property = g_object_ref_sink (property);
		cpg_usable_use (CPG_USABLE (instruction->priv->property));
	}
}

/**
 * cpg_instruction_property_get_property:
 * @instruction: A #CpgInstructionProperty
 *
 * Get the property executed by the instruction.
 *
 * Returns: (transfer none): A #CpgProperty
 *
 **/
CpgProperty *
cpg_instruction_property_get_property (CpgInstructionProperty *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_PROPERTY (instruction), NULL);
	return instruction->priv->property;
}

void
cpg_instruction_property_set_binding (CpgInstructionProperty        *instruction,
                                      CpgInstructionPropertyBinding  binding)
{
	g_return_if_fail (CPG_IS_INSTRUCTION_PROPERTY (instruction));

	instruction->priv->binding = binding;
}

CpgInstructionPropertyBinding
cpg_instruction_property_get_binding (CpgInstructionProperty *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_PROPERTY (instruction),
	                      CPG_INSTRUCTION_PROPERTY_BINDING_NONE);

	return instruction->priv->binding;
}
