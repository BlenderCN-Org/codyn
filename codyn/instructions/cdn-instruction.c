#include "cdn-instruction.h"

G_DEFINE_TYPE (CdnInstruction, cdn_instruction, CDN_TYPE_MINI_OBJECT)

static gchar *
cdn_instruction_to_string_impl (CdnInstruction *instruction)
{
	return g_strdup_printf ("[%s]", g_type_name (G_TYPE_FROM_INSTANCE (instruction)));
}

static gint
cdn_instruction_get_stack_count_impl (CdnInstruction *instruction)
{
	g_warning ("%s should implement get_stack_count!", g_type_name (G_TYPE_FROM_INSTANCE (instruction)));
	return 0;
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
	klass->get_stack_count = cdn_instruction_get_stack_count_impl;
	klass->get_dependencies = cdn_instruction_get_dependencies_impl;
	klass->get_is_commutative = cdn_instruction_get_is_commutative_impl;
}

static void
cdn_instruction_init (CdnInstruction *instruction)
{
}

gchar *
cdn_instruction_to_string (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), NULL);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->to_string (instruction);
}

void
cdn_instruction_execute (CdnInstruction *instruction,
                         CdnStack       *stack)
{
	/* Omit type check to increase speed */
	CDN_INSTRUCTION_GET_CLASS (instruction)->execute (instruction, stack);
}

gint
cdn_instruction_get_stack_count (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), 0);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->get_stack_count (instruction);
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

gboolean
cdn_instruction_equal (CdnInstruction *i1,
                       CdnInstruction *i2)
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
		return CDN_INSTRUCTION_GET_CLASS (i1)->equal (i1, i2);
	}
	else
	{
		return FALSE;
	}
}

gboolean
cdn_instruction_get_is_commutative (CdnInstruction *instruction)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION (instruction), FALSE);

	return CDN_INSTRUCTION_GET_CLASS (instruction)->get_is_commutative (instruction);
}
