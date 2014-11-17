#include "cdn-instruction-number.h"
#include <math.h>

#define CDN_INSTRUCTION_NUMBER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_NUMBER, CdnInstructionNumberPrivate))

struct _CdnInstructionNumberPrivate
{
	gdouble value;
	gchar *repr;

	CdnStackManipulation smanip;
};

/**
 * CdnInstructionNumber:
 *
 * Number instruction.
 *
 * #CdnInstructionNumber is a special #CdnInstruction subtype which represents
 * a single number. When executed, it pushes exactly one number onto the stack.
 *
 */
G_DEFINE_TYPE (CdnInstructionNumber, cdn_instruction_number, CDN_TYPE_INSTRUCTION)

static void
cdn_instruction_number_finalize (CdnMiniObject *object)
{
	CdnInstructionNumber *self;

	self = CDN_INSTRUCTION_NUMBER (object);

	g_free (self->priv->repr);
	cdn_stack_manipulation_destroy (&self->priv->smanip);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_number_parent_class)->finalize (object);
}

static CdnMiniObject *
cdn_instruction_number_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionNumber *self;
	CdnInstructionNumber const *src;

	src = CDN_INSTRUCTION_NUMBER_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_number_parent_class)->copy (object);

	self = CDN_INSTRUCTION_NUMBER (ret);

	self->priv->value = src->priv->value;
	self->priv->repr = g_strdup (src->priv->repr);

	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	return ret;
}

static gchar *
cdn_instruction_number_to_string (CdnInstruction *instruction)
{
	CdnInstructionNumber *self;

	self = CDN_INSTRUCTION_NUMBER (instruction);

	if (self->priv->repr)
	{
		return g_strdup (self->priv->repr);
	}

	return g_strdup_printf ("NUM (%.3f)", self->priv->value);
}

static void
cdn_instruction_number_execute (CdnInstruction *instruction,
                                CdnStack       *stack)
{
	CdnInstructionNumber *self;

	/* Direct cast to reduce overhead of GType cast */
	self = (CdnInstructionNumber *)instruction;

	cdn_stack_push (stack, self->priv->value);
}

static CdnStackManipulation const *
cdn_instruction_number_get_stack_manipulation (CdnInstruction  *instruction,
                                               GError         **error)
{
	CdnInstructionNumber *self;

	self = CDN_INSTRUCTION_NUMBER (instruction);
	return &self->priv->smanip;
}

static gboolean
cdn_instruction_number_equal (CdnInstruction *i1,
                              CdnInstruction *i2,
                              gboolean        asstring)
{
	CdnInstructionNumber *n1 = CDN_INSTRUCTION_NUMBER (i1);
	CdnInstructionNumber *n2 = CDN_INSTRUCTION_NUMBER (i2);

	return n1->priv->value == n2->priv->value;
}

static void
cdn_instruction_number_class_init (CdnInstructionNumberClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_number_finalize;
	object_class->copy = cdn_instruction_number_copy;

	inst_class->to_string = cdn_instruction_number_to_string;
	inst_class->execute = cdn_instruction_number_execute;
	inst_class->get_stack_manipulation = cdn_instruction_number_get_stack_manipulation;
	inst_class->equal = cdn_instruction_number_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionNumberPrivate));
}

static void
cdn_instruction_number_init (CdnInstructionNumber *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_instruction_number_get_instance_private (self);
	self->priv = CDN_INSTRUCTION_NUMBER_GET_PRIVATE (self);

	self->priv->smanip.push.rows = 1;
	self->priv->smanip.push.columns = 1;
}

/**
 * cdn_instruction_number_new:
 * @value: the value
 *
 * Create a new number instruction representing the given value.
 *
 * Returns: (transfer full) (type CdnInstructionNumber): a new #CdnInstructionNumber.
 *
 */
CdnInstruction *
cdn_instruction_number_new (gdouble value)
{
	CdnMiniObject *ret;
	CdnInstructionNumber *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_NUMBER);
	self = CDN_INSTRUCTION_NUMBER (ret);

	self->priv->value = value;
	self->priv->repr = NULL;

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_number_new_from_string:
 * @repr: the value
 *
 * Create a new number instruction representing the given value.
 *
 * Returns: (transfer full) (type CdnInstructionNumber): a new #CdnInstructionNumber.
 *
 */
CdnInstruction *
cdn_instruction_number_new_from_string (gchar const *repr)
{
	CdnInstructionNumber *self;

	self = CDN_INSTRUCTION_NUMBER (cdn_instruction_number_new (g_ascii_strtod (repr, NULL)));
	self->priv->repr = g_strdup (repr);

	return CDN_INSTRUCTION (self);
}

/**
 * cdn_instruction_number_get_value:
 * @number: the #CdnInstructioNumber
 *
 * Get the value of the number instruction.
 *
 * Returns: the number instruction value.
 *
 */
gdouble
cdn_instruction_number_get_value (CdnInstructionNumber *number)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_NUMBER (number), 0.0);

	return number->priv->value;
}

/**
 * cdn_instruction_number_set_value:
 * @number: the #CdnInstructioNumber
 * @value: the value
 *
 * Set the value of the number instruction.
 *
 */
void
cdn_instruction_number_set_value (CdnInstructionNumber *number,
                                  gdouble               value)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_NUMBER (number));

	number->priv->value = value;

	if (number->priv->repr)
	{
		g_free (number->priv->repr);
		number->priv->repr = NULL;
	}
}

/**
 * cdn_instruction_number_get_representation:
 * @number: the #CdnInstructioNumber
 *
 * Get the textual representation of the number instruction. If the
 * instruction was created using #cdn_instruction_number_new_from_string,
 * then the string provided to construct it will be returned. Otherwise,
 * the textual representation of the actual numerical value will be
 * returned.
 *
 * Returns: (transfer full): the textual representation of the value.
 *
 */
gchar *
cdn_instruction_number_get_representation (CdnInstructionNumber *number)
{
	gchar buf[G_ASCII_DTOSTR_BUF_SIZE];

	g_return_val_if_fail (CDN_IS_INSTRUCTION_NUMBER (number), NULL);

	if (number->priv->repr)
	{
		return g_strdup (number->priv->repr);
	}

	g_ascii_dtostr (buf, sizeof (buf), number->priv->value);

	return g_strdup (buf);
}

/**
 * cdn_instruction_number_set_representation:
 * @number: the #CdnInstructioNumber
 * @repr: the textual representation
 *
 * Set the textual representation of the number instruction. This does not
 * change the numerical value that the instruction encodes, only its textual
 * representation as returned by #cdn_instruction_number_get_representation.
 *
 */
void
cdn_instruction_number_set_representation (CdnInstructionNumber *number,
                                           gchar const          *repr)
{
	g_return_if_fail (CDN_IS_INSTRUCTION_NUMBER (number));

	g_free (number->priv->repr);
	number->priv->repr = g_strdup (repr);
}

