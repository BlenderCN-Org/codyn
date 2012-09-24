#include "cdn-instruction.h"

/**
 * SECTION:cdn-instruction
 * @short_description: Mathematical instruction base class
 *
 * The #CdnInstruction class is an abstract base class for all mathematical
 * instructions in Codyn.
 *
 **/

#define CDN_INSTRUCTION_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION, CdnInstructionPrivate))

struct _CdnInstructionPrivate
{
	gint start;
	gint end;
};

G_DEFINE_TYPE (CdnInstruction, cdn_instruction, CDN_TYPE_MINI_OBJECT)

static gchar *
cdn_instruction_to_string_impl (CdnInstruction *instruction)
{
	return g_strdup_printf ("[%s]",
	                        g_type_name (G_TYPE_FROM_INSTANCE (instruction)));
}

static CdnStackManipulation const *
cdn_instruction_get_stack_manipulation_impl (CdnInstruction  *instruction,
                                             GError         **error)
{
	return NULL;
}

static GSList *
cdn_instruction_get_dependencies_impl (CdnInstruction *instruction)
{
	return NULL;
}

static gboolean
cdn_instruction_get_is_commutative_impl (CdnInstruction *instruction)
{
	return FALSE;
}

static CdnMiniObject *
cdn_instruction_copy_impl (CdnMiniObject *object)
{
	CdnInstruction *inst;
	CdnInstruction *cp;

	cp = (CdnInstruction *)CDN_MINI_OBJECT_CLASS (cdn_instruction_parent_class)->copy (object);
	inst = (CdnInstruction *)cp;

	cp->priv->start = inst->priv->start;
	cp->priv->end = inst->priv->end;

	return (CdnMiniObject *)cp;
}

static void
cdn_instruction_recalculate_sparsity_impl (CdnInstruction  *instruction)
{
}

static void
cdn_instruction_class_init (CdnInstructionClass *klass)
{
	CdnMiniObjectClass *mini_object_class;

	mini_object_class = CDN_MINI_OBJECT_CLASS (klass);

	klass->to_string = cdn_instruction_to_string_impl;
	klass->get_stack_manipulation = cdn_instruction_get_stack_manipulation_impl;
	klass->get_dependencies = cdn_instruction_get_dependencies_impl;
	klass->get_is_commutative = cdn_instruction_get_is_commutative_impl;
	klass->recalculate_sparsity = cdn_instruction_recalculate_sparsity_impl;

	mini_object_class->copy = cdn_instruction_copy_impl;

	g_type_class_add_private (mini_object_class, sizeof (CdnInstructionPrivate));
}

static void
cdn_instruction_init (CdnInstruction *instruction)
{
	instruction->priv = CDN_INSTRUCTION_GET_PRIVATE (instruction);
}

/**
 * cdn_instruction_to_string:
 * @instruction: A #CdnInstruction
 *
 * Get some sort of string representation of the instruction. Note that this
 * is only useful for debugging since the string representation only shows
 * what type of instruction this is.
 *
 * Returns: a string representation of the instruction
 *
 **/
gchar *
cdn_instruction_to_string (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->to_string (instruction);
}

/**
 * cdn_instruction_execute:
 * @instruction: A #CdnInstruction
 * @stack: A #CdnStack
 *
 * Execute the instruction on the given stack.
 *
 **/
void
cdn_instruction_execute (CdnInstruction *instruction,
                         CdnStack       *stack)
{
	/* Omit type check to increase speed */
	CDN_INSTRUCTION_GET_CLASS (instruction)->execute (instruction, stack);
}

/**
 * cdn_instruction_get_stack_manipulation:
 * @instruction: A #CdnInstruction
 *
 * Get by how many values the instruction will modify the stack when executed.
 * Note that this can be a negative value if the instruction consumes more
 * values than it pushes. It can generally be assumed that every instruction
 * always pushes one value on the stack (i.e. the number of values it pops
 * is -stack_count + 1)
 *
 * Returns: the number of values that the instruction will pop and push on the
 *          stack
 *
 **/
CdnStackManipulation const *
cdn_instruction_get_stack_manipulation (CdnInstruction  *instruction,
                                        GError         **error)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->get_stack_manipulation (instruction,
	                                                                        error);
}

/**
 * cdn_instruction_get_dependencies:
 * @instruction: A #CdnInstruction
 *
 * Get the properties on which the instruction depends.
 *
 * Returns: (element-type CdnVariable) (transfer container): A #GSList of #CdnVariable. The list
 *          should be freed with g_slist_free when no longer used.
 *
 **/
GSList *
cdn_instruction_get_dependencies (CdnInstruction *instruction)
{
	/* Omit type check to increase speed */
	return CDN_INSTRUCTION_GET_CLASS (instruction)->get_dependencies (instruction);
}

/**
 * cdn_instruction_equal:
 * @i1: A #CdnInstruction
 * @i2: A #CdnInstruction
 * @asstring: as string
 *
 * Compares two instructions for equality.
 *
 * Returns: %TRUE if both instructions are equal, %FALSE otherwise
 *
 **/
gboolean
cdn_instruction_equal (CdnInstruction *i1,
                       CdnInstruction *i2,
                       gboolean        asstring)
{
	if (i1 == NULL && i2 == NULL)
	{
		return TRUE;
	}

	if (i1 == NULL || i2 == NULL)
	{
		return FALSE;
	}

	if (G_TYPE_FROM_INSTANCE (i1) != G_TYPE_FROM_INSTANCE (i2))
	{
		return FALSE;
	}

	if (CDN_INSTRUCTION_GET_CLASS (i1)->equal)
	{
		return CDN_INSTRUCTION_GET_CLASS (i1)->equal (i1, i2, asstring);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_instruction_get_is_commutative:
 * @instruction: A #CdnInstruction
 *
 * Get whether an instruction has commutative arguments or not.
 *
 * Returns: %TRUE if the instruction as commutative arguments, %FALSE otherwise
 *
 **/
gboolean
cdn_instruction_get_is_commutative (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), FALSE);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->get_is_commutative (instruction);
}

/**
 * cdn_instruction_set_location:
 * @instruction: a #CdnInstruction.
 * @start: the start.
 * @end: the end.
 *
 * Set the location of this instruction in the corresponding string representation
 * of the expression the instruction belongs to.
 *
 **/
void
cdn_instruction_set_location (CdnInstruction *instruction,
                              gint            start,
                              gint            end)
{
	g_return_if_fail (CDN_IS_INSTRUCTION (instruction));

	instruction->priv->start = start;
	instruction->priv->end = end;
}

/**
 * cdn_instruction_get_location:
 * @instruction: a #CdnInstruction.
 * @start: (out): return value for the start.
 * @end: (out): return value for the end.
 *
 * Get the location of this instruction in the corresponding string representation
 * of the expression the instruction belongs to.
 *
 **/
void
cdn_instruction_get_location (CdnInstruction *instruction,
                              gint           *start,
                              gint           *end)
{
	g_return_if_fail (CDN_IS_INSTRUCTION (instruction));

	if (start)
	{
		*start = instruction->priv->start;
	}

	if (end)
	{
		*end = instruction->priv->end;
	}
}

void
cdn_instruction_recalculate_sparsity (CdnInstruction  *instruction)
{
	g_return_if_fail (CDN_IS_INSTRUCTION (instruction));

	return CDN_INSTRUCTION_GET_CLASS (instruction)->recalculate_sparsity (instruction);
}
