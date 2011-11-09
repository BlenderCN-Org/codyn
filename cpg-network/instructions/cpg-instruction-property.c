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
	CpgExpression *diffeq;
};

G_DEFINE_TYPE (CpgInstructionProperty, cpg_instruction_property, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_property_finalize (CpgMiniObject *object)
{
	CpgInstructionProperty *self;

	self = CPG_INSTRUCTION_PROPERTY (object);

	cpg_instruction_property_set_property (self, NULL);

	if (self->priv->diffeq)
	{
		g_object_unref (self->priv->diffeq);
	}

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

	if (src->priv->diffeq)
	{
		self->priv->diffeq = cpg_expression_copy (src->priv->diffeq);
	}

	return ret;
}

static gchar *
cpg_instruction_property_to_string (CpgInstruction *instruction)
{
	CpgInstructionProperty *self;

	self = CPG_INSTRUCTION_PROPERTY (instruction);

	return g_strdup_printf ("PRP (%s.%s)",
	                        cpg_object_get_id (cpg_property_get_object (self->priv->property)),
	                        cpg_property_get_name (self->priv->property));
}

static void
cpg_instruction_property_execute (CpgInstruction *instruction,
                                  CpgStack       *stack)
{
	CpgInstructionProperty *self;
	CpgExpression *expr;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionProperty *)instruction;

	expr = cpg_instruction_property_get_diff (self);

	if (expr)
	{
		cpg_stack_push (stack,
		                cpg_expression_evaluate (expr));
	}
	else
	{
		cpg_stack_push (stack,
		                cpg_property_get_value (self->priv->property));
	}
}

static gint
cpg_instruction_property_get_stack_count (CpgInstruction *instruction)
{
	return 1;
}

static GSList *
cpg_instruction_property_get_dependencies (CpgInstruction *instruction)
{
	return g_slist_prepend (NULL, CPG_INSTRUCTION_PROPERTY (instruction)->priv->property);
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
 * cpg_instruction_property_new:
 * @property: (transfer none): A #CpgProperty
 * @binding: A #CpgInstructionPropertyBinding
 *
 * Create a new #CpgInstructionProperty.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_property_new (CpgProperty                   *property,
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
		instruction->priv->property = g_object_ref (property);
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

static CpgExpression *
collect_diffeq (CpgProperty *prop)
{
	GSList *actors;
	GSList *instructions = NULL;
	GSList *item;
	GSList *last = NULL;
	gchar *s;
	CpgExpression *ret;
	CpgExpressionTreeIter *iter;

	actors = cpg_property_get_actions (prop);

	for (item = actors; item; item = g_slist_next (item))
	{
		CpgExpression *e;
		GSList const *inst;
		GSList *cp = NULL;

		e = cpg_link_action_get_equation (item->data);

		inst = cpg_expression_get_instructions (e);

		while (inst)
		{
			cp = g_slist_prepend (cp,
			                      cpg_mini_object_copy (CPG_MINI_OBJECT (inst->data)));

			inst = g_slist_next (inst);
		}

		cp = g_slist_reverse (cp);

		instructions = g_slist_concat (cp,
		                               instructions);

		if (item != actors)
		{
			last = g_slist_append (last,
			                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
			                                                     "+",
			                                                     2));

			last = last->next;
		}
		else
		{
			last = g_slist_last (instructions);
		}
	}

	g_slist_free (actors);

	if (instructions == NULL)
	{
		return NULL;
	}

	iter = cpg_expression_tree_iter_new_from_instructions (instructions);

	s = cpg_expression_tree_iter_to_string (iter);
	ret = cpg_expression_new (s);
	_cpg_expression_set_instructions_take (ret,
	                                       cpg_expression_tree_iter_to_instructions (iter));

	g_slist_foreach (instructions, (GFunc)cpg_mini_object_free, NULL);
	g_slist_free (instructions);
	g_free (s);

	return ret;
}

CpgExpression *
cpg_instruction_property_get_diff (CpgInstructionProperty *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION_PROPERTY (instruction), NULL);

	if (!(instruction->priv->binding & CPG_INSTRUCTION_PROPERTY_BINDING_DIFF))
	{
		return NULL;
	}

	if (!instruction->priv->diffeq)
	{
		instruction->priv->diffeq = collect_diffeq (instruction->priv->property);
	}

	return instruction->priv->diffeq;
}

