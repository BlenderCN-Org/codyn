#include "cpg-instruction-rand.h"

#include <cpg-network/cpg-math.h>
#include <stdlib.h>

#define CPG_INSTRUCTION_RAND_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_INSTRUCTION_RAND, CpgInstructionRandPrivate))

struct _CpgInstructionRandPrivate
{
	glong random_value;
	gint num_arguments;
};

G_DEFINE_TYPE (CpgInstructionRand, cpg_instruction_rand, CPG_TYPE_INSTRUCTION)

static void
cpg_instruction_rand_finalize (CpgMiniObject *object)
{
	CPG_MINI_OBJECT_CLASS (cpg_instruction_rand_parent_class)->finalize (object);
}

static CpgMiniObject *
cpg_instruction_rand_copy (CpgMiniObject const *object)
{
	CpgMiniObject *ret;
	CpgInstructionRand *r;
	CpgInstructionRand *rret;

	ret = CPG_MINI_OBJECT_CLASS (cpg_instruction_rand_parent_class)->copy (object);

	r = CPG_INSTRUCTION_RAND (object);
	rret = CPG_INSTRUCTION_RAND (ret);

	rret->priv->num_arguments = r->priv->num_arguments;
	rret->priv->random_value = r->priv->random_value;

	return ret;
}

static gchar *
cpg_instruction_rand_to_string (CpgInstruction *instruction)
{
	return g_strdup ("RAND ()");
}

static void
cpg_instruction_rand_execute (CpgInstruction *instruction,
                              CpgStack       *stack)
{
	CpgInstructionRand *self;
	gdouble from = 0;
	gdouble to = 1;

	/* Direct cast to reduce overhead of GType cast */
	self = (CpgInstructionRand *)instruction;

	if (self->priv->num_arguments == 1)
	{
		to = cpg_stack_pop (stack);
	}
	else if (self->priv->num_arguments == 2)
	{
		to = cpg_stack_pop (stack);
		from = cpg_stack_pop (stack);
	}

	cpg_stack_push (stack, from + self->priv->random_value * (to - from) / RAND_MAX);
}

static gint
cpg_instruction_rand_get_stack_count (CpgInstruction *instruction)
{
	return 1 - CPG_INSTRUCTION_RAND (instruction)->priv->num_arguments;
}

static void
cpg_instruction_rand_class_init (CpgInstructionRandClass *klass)
{
	CpgMiniObjectClass *object_class = CPG_MINI_OBJECT_CLASS (klass);
	CpgInstructionClass *inst_class = CPG_INSTRUCTION_CLASS (klass);

	object_class->finalize = cpg_instruction_rand_finalize;
	object_class->copy = cpg_instruction_rand_copy;

	inst_class->to_string = cpg_instruction_rand_to_string;
	inst_class->execute = cpg_instruction_rand_execute;
	inst_class->get_stack_count = cpg_instruction_rand_get_stack_count;

	g_type_class_add_private (object_class, sizeof(CpgInstructionRandPrivate));
}

static void
cpg_instruction_rand_init (CpgInstructionRand *self)
{
	self->priv = CPG_INSTRUCTION_RAND_GET_PRIVATE (self);
}

/**
 * cpg_instruction_rand_new:
 * @id: The function id
 * @name: (transfer none): The function name
 * @arguments: The number of arguments
 * @variable: Whether the function accepts a variable number of arguments
 *
 * Create a new #CpgInstructionRand.
 *
 * Returns: A #CpgInstruction
 *
 **/
CpgInstruction *
cpg_instruction_rand_new (gint arguments)
{
	CpgMiniObject *ret;
	CpgInstructionRand *rnd;

	ret = cpg_mini_object_new (CPG_TYPE_INSTRUCTION_RAND);
	rnd = CPG_INSTRUCTION_RAND (ret);

	rnd->priv->num_arguments = arguments;

	return CPG_INSTRUCTION (ret);
}

void
cpg_instruction_rand_next (CpgInstructionRand *self)
{
	/* Omit type check to increase speed */
	self->priv->random_value = random();
}
