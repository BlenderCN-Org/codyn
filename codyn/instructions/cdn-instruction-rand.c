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

#ifndef MINGW
	char state[8];
	guint seed;
#endif
} CdnInstructionRandStatePrivate;

/**
 * CdnInstructionRand:
 *
 * Random number instruction.
 *
 * #CdnInstructionRand is a special subtype of #CdnInstruction which represents
 * a random number. Random number instructions are handled a bit differently from
 * other instructions. Each random number instruction contains a cache and new
 * random numbers have to be explicitly requested. This is done such that evaluation
 * order does not have an effect on observation of random values. Also, this keeps
 * the evaluation of a single integration step consistent.
 *
 */

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

#ifndef MINGW
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)r->priv;

		cdn_instruction_rand_set_seed (rret, spriv->seed);
	}
#endif

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

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionRand *)instruction;

	if (self->priv->num_random_value == 0)
	{
		cdn_stack_push (stack, (gdouble)self->priv->random_value[0] / RAND_MAX);
	}
	else
	{
		for (i = 0; i < self->priv->num_random_value; ++i)
		{
			cdn_stack_push (stack, (gdouble)self->priv->random_value[i] / RAND_MAX);
		}
	}
}

static CdnStackManipulation const *
cdn_instruction_rand_get_stack_manipulation (CdnInstruction  *instruction,
                                             GError         **error)
{
	CdnInstructionRand *self;

	self = (CdnInstructionRand *)instruction;

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
 * Returns: (transfer full) (type CdnInstructionRand): a new #CdnInstructionRand
 *
 **/
CdnInstruction *
cdn_instruction_rand_new (CdnStackArgs const *argdim)
{
	CdnMiniObject *ret;
	CdnInstructionRand *rnd;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_RAND);
	rnd = CDN_INSTRUCTION_RAND (ret);

	if (argdim->num > 0)
	{
		gint nd1 = 1;
		gint nd2 = 1;

		nd1 = cdn_stack_arg_size (&argdim->args[0]);

		if (argdim->num > 1)
		{
			nd2 = cdn_stack_arg_size (&argdim->args[1]);
		}

		rnd->priv->num_random_value = nd1 == 1 ? nd2 : nd1;

		if (nd1 != 1)
		{
			rnd->priv->smanip.push.rows = argdim->args[0].rows;
			rnd->priv->smanip.push.columns = argdim->args[0].columns;
		}
		else if (nd2 != 1)
		{
			rnd->priv->smanip.push.rows = argdim->args[1].rows;
			rnd->priv->smanip.push.columns = argdim->args[1].columns;
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

#ifndef MINGW
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		setstate (spriv->state);
	}
#endif

	/* Omit type check to increase speed */
	for (i = 0; i < self->priv->num_random_value; ++i)
	{
		self->priv->random_value[i] = rand ();
	}
}

/**
 * cdn_instruction_rand_set_use_streams:
 * @use: whether or not to use streams
 *
 * Sets whether or not random instructions should work in a streaming
 * manner. If needed, this should be always called at program initialization,
 * before any instructions are being created. Random instructions in streaming
 * mode each carry their own random seed. This allows for external control of
 * random number generation and is used by rawc to ensure deterministic validation
 * of networks across code generation.
 *
 */
void
cdn_instruction_rand_set_use_streams (gboolean use)
{
	use_streams = use;
}

/**
 * cdn_instruction_rand_set_seed:
 * @self: the #CdnInstructionRand
 * @seed: the new seed
 *
 * Set the random instruction seed when using rand streams. Note that
 * this function does nothing if #cdn_instruction_rand_set_use_streams
 * is not set.
 */
void
cdn_instruction_rand_set_seed (CdnInstructionRand *self,
                               guint               seed)
{
#ifndef MINGW
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		spriv->seed = seed;

		cdn_instruction_rand_reset (self);
	}
#endif
}

/**
 * cdn_instruction_rand_get_seed:
 * @self: the #CdnInstructionRand
 *
 * Get the random instruction seed when using rand streams. Note that
 * this function always returns 0 if #cdn_instruction_rand_set_use_streams
 * is not set.
 *
 * Returns: the seed.
 *
 */
guint
cdn_instruction_rand_get_seed (CdnInstructionRand *self)
{
#ifndef MINGW
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		return spriv->seed;
	}
#endif

	return 0;
}

/**
 * cdn_instruction_rand_reset:
 * @self: the #CdnInstructionRand
 *
 * Reset the random number generator. When using streams, this initializes
 * the random number generator with the specified seed. Otherwise, this
 * will simply reduce to calling #cdn_instruction_rand_next.
 */
void
cdn_instruction_rand_reset (CdnInstructionRand *self)
{
#ifndef MINGW
	if (use_streams)
	{
		CdnInstructionRandStatePrivate *spriv =
			(CdnInstructionRandStatePrivate *)self->priv;

		initstate (spriv->seed, spriv->state, sizeof(spriv->state));
	}
#endif

	cdn_instruction_rand_next (self);
}
