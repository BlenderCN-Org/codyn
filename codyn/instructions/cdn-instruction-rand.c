#include "cdn-instruction-rand.h"

#include <codyn/cdn-math.h>
#include <stdlib.h>

#define CDN_INSTRUCTION_RAND_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRandPrivate))

struct _CdnInstructionRandPrivate
{
	glong random_value;
	gint num_arguments;
};

G_DEFINE_TYPE (CdnInstructionRand, cdn_instruction_rand, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_rand_finalize (CdnMiniObject *object)
{
	CDN_MINI_OBJECT_CLASS (cdn_instruction_rand_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_rand_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionRand *r;
	CdnInstructionRand *rret;

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_rand_parent_class)->copy (object);

	r = CDN_INSTRUCTION_RAND (object);
	rret = CDN_INSTRUCTION_RAND (ret);

	rret->priv->num_arguments = r->priv->num_arguments;
	rret->priv->random_value = r->priv->random_value;

	return ret;
}

static gchar *
cdn_instruction_rand_to_string (CdnInstruction *instruction)
{
	return g_strdup ("RAND ()");
}

static void
cdn_instruction_rand_execute (CdnInstruction *instruction,
                              CdnStack       *stack)
{
	CdnInstructionRand *self;
	gdouble from = 0;
	gdouble to = 1;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionRand *)instruction;

	if (self->priv->num_arguments == 1)
	{
		to = cdn_stack_pop (stack);
	}
	else if (self->priv->num_arguments == 2)
	{
		to = cdn_stack_pop (stack);
		from = cdn_stack_pop (stack);
	}

	cdn_stack_push (stack, from + self->priv->random_value * (to - from) / RAND_MAX);
}

static gint
cdn_instruction_rand_get_stack_count (CdnInstruction *instruction)
{
	return 1 - CDN_INSTRUCTION_RAND (instruction)->priv->num_arguments;
}

static void
cdn_instruction_rand_class_init (CdnInstructionRandClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_rand_finalize;
	object_class->copy = cdn_instruction_rand_copy;

	inst_class->to_string = cdn_instruction_rand_to_string;
	inst_class->execute = cdn_instruction_rand_execute;
	inst_class->get_stack_count = cdn_instruction_rand_get_stack_count;

	g_type_class_add_private (object_class, sizeof(CdnInstructionRandPrivate));
}

static void
cdn_instruction_rand_init (CdnInstructionRand *self)
{
	self->priv = CDN_INSTRUCTION_RAND_GET_PRIVATE (self);
}

/**
 * cdn_instruction_rand_new:
 * @id: The function id
 * @name: (transfer none): The function name
 * @arguments: The number of arguments
 * @variable: Whether the function accepts a variable number of arguments
 *
 * Create a new #CdnInstructionRand.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_rand_new (gint arguments)
{
	CdnMiniObject *ret;
	CdnInstructionRand *rnd;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_RAND);
	rnd = CDN_INSTRUCTION_RAND (ret);

	rnd->priv->num_arguments = arguments;

	return CDN_INSTRUCTION (ret);
}

void
cdn_instruction_rand_next (CdnInstructionRand *self)
{
	/* Omit type check to increase speed */
	self->priv->random_value = random();
}
