#include "cpg-instruction.h"

G_DEFINE_ABSTRACT_TYPE (CpgInstruction, cpg_instruction, CPG_TYPE_MINI_OBJECT)

static gchar *
cpg_instruction_to_string_impl (CpgInstruction *instruction)
{
	return g_strdup_printf ("[%s]", g_type_name (G_TYPE_FROM_INSTANCE (instruction)));
}

static gint
cpg_instruction_get_stack_count_impl (CpgInstruction *instruction)
{
	g_warning ("%s should implement get_stack_count!", g_type_name (G_TYPE_FROM_INSTANCE (instruction)));
	return 0;
}

static GSList *
cpg_instruction_get_dependencies_impl (CpgInstruction *instruction)
{
	return NULL;
}

static void
cpg_instruction_class_init (CpgInstructionClass *klass)
{
	klass->to_string = cpg_instruction_to_string_impl;
	klass->get_stack_count = cpg_instruction_get_stack_count_impl;
	klass->get_dependencies = cpg_instruction_get_dependencies_impl;
}

static void
cpg_instruction_init (CpgInstruction *instruction)
{
}

gchar *
cpg_instruction_to_string (CpgInstruction *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION (instruction), NULL);

	return CPG_INSTRUCTION_GET_CLASS (instruction)->to_string (instruction);
}

void
cpg_instruction_execute (CpgInstruction *instruction,
                         CpgStack       *stack)
{
	/* Omit type check to increase speed */
	CPG_INSTRUCTION_GET_CLASS (instruction)->execute (instruction, stack);
}

gint
cpg_instruction_get_stack_count (CpgInstruction *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION (instruction), 0);

	return CPG_INSTRUCTION_GET_CLASS (instruction)->get_stack_count (instruction);
}

/**
 * cpg_instruction_get_dependencies:
 * @instruction: A #CpgInstruction
 *
 * Get the properties on which the instruction depends.
 *
 * Returns: (element-type CpgProperty) (transfer container): A #GSList of #CpgProperty. The list
 *          should be freed with g_slist_free when no longer used.
 *
 **/

GSList *
cpg_instruction_get_dependencies (CpgInstruction *instruction)
{
	g_return_val_if_fail (CPG_IS_INSTRUCTION (instruction), NULL);

	return CPG_INSTRUCTION_GET_CLASS (instruction)->get_dependencies (instruction);
}

gboolean
cpg_instruction_equal (CpgInstruction *i1,
                       CpgInstruction *i2)
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

	if (CPG_INSTRUCTION_GET_CLASS (i1)->equal)
	{
		return CPG_INSTRUCTION_GET_CLASS (i1)->equal (i1, i2);
	}
	else
	{
		return FALSE;
	}
}
