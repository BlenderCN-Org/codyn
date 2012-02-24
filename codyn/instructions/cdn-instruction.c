#include "cdn-instruction.h"

/**
 * SECTION:cdn-instruction
 * @short_description: Mathematical instruction base class
 *
 * The #CdnInstruction class is an abstract base class for all mathematical
 * instructions in Codyn.
 *
 **/

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

static void
cdn_instruction_class_init (CdnInstructionClass *klass)
{
	klass->to_string = cdn_instruction_to_string_impl;
	klass->get_stack_manipulation = cdn_instruction_get_stack_manipulation_impl;
	klass->get_dependencies = cdn_instruction_get_dependencies_impl;
	klass->get_is_commutative = cdn_instruction_get_is_commutative_impl;
}

static void
cdn_instruction_init (CdnInstruction *instruction)
{
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
 * cdn_instruction_get_stack_count:
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
