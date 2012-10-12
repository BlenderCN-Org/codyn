#include "cdn-instruction-rand.h"

#include <codyn/cdn-math.h>
#include <stdlib.h>
#include <string.h>
#include <codyn/cdn-compile-error.h>

#define CDN_INSTRUCTION_RAND_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_RAND, CdnInstructionRandPrivate))

static gboolean use_streams = FALSE;

struct _CdnInstructionRandPrivate
{
	glong *random_value;
	guint num_random_value;

	CdnStackManipulation smanip;
};

typedef struct
{
	CdnInstructionRandPrivate priv;

	char state[8];
	guint seed;
} CdnInstructionRandStatePrivate;

G_DEFINE_TYPE (CdnInstructionRand, cdn_instruction_rand, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_rand_finalize (CdnMiniObject *object)
{
	CdnInstructionRand *self;

	self = CDN_INSTRUCTION_RAND (object);

	g_free (self->priv->random_value);

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

	rret->priv->num_random_value = r->priv->num_random_value;
	rret->priv->random_value = g_new (glong, r->priv->num_random_value);

	memcpy (rret->priv->random_value,
	        r->priv->random_value,
	        sizeof (glong) * r->priv->num_random_value);

	cdn_stack_manipulation_copy (&rret->priv->smanip, &r->priv->smanip);

	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)r->priv;

		cdn_instruction_rand_set_seed (rret, spriv->seed);
	}

	return ret;
}

static gchar *
cdn_instruction_rand_to_string (CdnInstruction *instruction)
{
	return g_strdup ("RAND ()");
}

#define RAND2(i, from, to) (from + self->priv->random_value[i] * (to - from) / RAND_MAX)

static void
cdn_instruction_rand_execute (CdnInstruction *instruction,
                              CdnStack       *stack)
{
	CdnInstructionRand *self;
	gint i;
	gdouble *ptr;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionRand *)instruction;

	ptr = cdn_stack_output_ptr (stack) - self->priv->num_random_value;

	if (self->priv->smanip.pop.num == 0)
	{
		cdn_stack_push (stack, (gdouble)self->priv->random_value[0] / RAND_MAX);
	}
	else if (self->priv->smanip.pop.num == 1)
	{
		for (i = 0; i < self->priv->num_random_value; ++i)
		{
			*ptr = self->priv->random_value[i] * *ptr / RAND_MAX;
			++ptr;
		}
	}
	else if (self->priv->smanip.pop.num == 2)
	{
		gint nd1 = cdn_stack_arg_size (&self->priv->smanip.pop.args[1]);
		gint nd2 = cdn_stack_arg_size (&self->priv->smanip.pop.args[0]);

		if (nd2 != 1 && nd1 == 1)
		{
			// Only one dimension for the "from"
			--ptr;

			gdouble from = *ptr;

			for (i = 0; i < nd2; ++i)
			{
				*ptr = RAND2 (i, from, *(ptr + 1));
				++ptr;
			}

			cdn_stack_pop (stack);
		}
		else if (nd1 != 1 && nd2 == 1)
		{
			gdouble to = cdn_stack_pop (stack);

			// Only one dimension for the "to"
			for (i = 0; i < nd1; ++i)
			{
				*ptr = RAND2 (i, *ptr, to);
			}
		}
		else
		{
			// Both dimensions equal
			for (i = 0; i < nd1; ++i)
			{
				--ptr;

				*ptr = RAND2 (i, *ptr, cdn_stack_pop (stack));
			}
		}
	}
}

static CdnStackManipulation const *
cdn_instruction_rand_get_stack_manipulation (CdnInstruction  *instruction,
                                             GError         **error)
{
	CdnInstructionRand *self;

	self = (CdnInstructionRand *)instruction;

	gint nd1 = 1;
	gint nd2 = 1;

	if (self->priv->smanip.pop.num > 2)
	{
		g_set_error (error,
		             CDN_COMPILE_ERROR_TYPE,
		             CDN_COMPILE_ERROR_INVALID_ARGUMENTS,
		             "The number of arguments can only be 0, 1 or 2");

		return NULL;
	}

	if (self->priv->smanip.pop.num > 0)
	{
		nd1 = cdn_stack_arg_size (&self->priv->smanip.pop.args[0]);
	}

	if (self->priv->smanip.pop.num > 1)
	{
		nd2 = cdn_stack_arg_size (&self->priv->smanip.pop.args[1]);
	}

	if (nd1 != 1 && nd2 != 1 && nd1 != nd2)
	{
		g_set_error (error,
		             CDN_COMPILE_ERROR_TYPE,
		             CDN_COMPILE_ERROR_INVALID_DIMENSION,
		             "The two arguments to rand must have the same dimension");

		return NULL;
	}

	return &self->priv->smanip;
}

static gboolean
cdn_instruction_rand_equal (CdnInstruction *i1,
                            CdnInstruction *i2,
                            gboolean        asstring)
{
	if (asstring)
	{
		return TRUE;
	}
	else
	{
		return i1 == i2;
	}
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
	inst_class->get_stack_manipulation = cdn_instruction_rand_get_stack_manipulation;
	inst_class->equal = cdn_instruction_rand_equal;

	if (use_streams)
	{
		g_type_class_add_private (object_class, sizeof(CdnInstructionRandStatePrivate));
	}
	else
	{
		g_type_class_add_private (object_class, sizeof(CdnInstructionRandPrivate));
	}
}

static void
cdn_instruction_rand_init (CdnInstructionRand *self)
{
	self->priv = CDN_INSTRUCTION_RAND_GET_PRIVATE (self);

	self->priv->smanip.push.rows = 1;
	self->priv->smanip.push.columns = 1;
}

/**
 * cdn_instruction_rand_new:
 * @argdim: the argument dimensions
 *
 * Create a new #CdnInstructionRand.
 *
 * Returns: A #CdnInstruction
 *
 **/
CdnInstruction *
cdn_instruction_rand_new (CdnStackArgs const *argdim)
{
	CdnMiniObject *ret;
	CdnInstructionRand *rnd;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_RAND);
	rnd = CDN_INSTRUCTION_RAND (ret);

	cdn_stack_args_copy (&rnd->priv->smanip.pop, argdim);

	if (argdim->num > 0)
	{
		gint nd1 = 1;
		gint nd2 = 1;

		nd1 = cdn_stack_arg_size (&rnd->priv->smanip.pop.args[0]);

		if (argdim->num > 1)
		{
			nd2 = cdn_stack_arg_size (&rnd->priv->smanip.pop.args[1]);
		}

		rnd->priv->num_random_value = nd1 == 1 ? nd2 : nd1;

		if (nd1 != 1)
		{
			rnd->priv->smanip.push.rows = rnd->priv->smanip.pop.args[0].rows;
			rnd->priv->smanip.push.columns = rnd->priv->smanip.pop.args[0].columns;
		}
		else if (nd2 != 1)
		{
			rnd->priv->smanip.push.rows = rnd->priv->smanip.pop.args[1].rows;
			rnd->priv->smanip.push.columns = rnd->priv->smanip.pop.args[1].columns;
		}
	}
	else
	{
		rnd->priv->num_random_value = 1;
	}

	rnd->priv->random_value = g_new0 (glong, rnd->priv->num_random_value);

	cdn_instruction_rand_next (rnd);
	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_rand_next:
 * @self: A #CdnInstructionRand
 *
 * Generate the next random value in the instruction.
 *
 **/
void
cdn_instruction_rand_next (CdnInstructionRand *self)
{
	gint i;

	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		setstate (spriv->state);
	}

	/* Omit type check to increase speed */
	for (i = 0; i < self->priv->num_random_value; ++i)
	{
		self->priv->random_value[i] = rand ();
	}
}

void
cdn_instruction_rand_set_use_streams (gboolean use)
{
	use_streams = use;
}

void
cdn_instruction_rand_set_seed (CdnInstructionRand *self,
                               guint               seed)
{
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		spriv->seed = seed;

		initstate (spriv->seed, spriv->state, sizeof(spriv->state));
		cdn_instruction_rand_next (self);
	}
}

guint
cdn_instruction_rand_get_seed (CdnInstructionRand *self)
{
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		return spriv->seed;
	}
	else
	{
		return 0;
	}
}
