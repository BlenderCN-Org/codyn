#include "cdn-instruction-matrix.h"
#include <string.h>

#define CDN_INSTRUCTION_MATRIX_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrixPrivate))

struct _CdnInstructionMatrixPrivate
{
	CdnStackManipulation smanip;
};

/**
 * CdnInstructionMatrix:
 *
 * Matrix instruction.
 *
 * #CdnInstructionMatrix is a special #CdnInstruction subtype representing a
 * matrixing instruction. The purpose of this instruction is solely to provide
 * structure in the instruction stream. As all expressions in codyn are statically
 * sized, matrices can be simply encoded in the dimensionality of the arguments
 * on the stack. However, it can be useful to explicitly encode the matrix construction
 * (i.e. [1, 2, 3; 4, 5 6]) in the instruction stream for later reconstruction of the
 * expression.
 *
 * When executed, this instruction does nothing since it only encodes dimensionality.
 *
 */

G_DEFINE_TYPE (CdnInstructionMatrix, cdn_instruction_matrix, CDN_TYPE_INSTRUCTION)

static CdnMiniObject *
cdn_instruction_matrix_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionMatrix *self;
	CdnInstructionMatrix const *src;

	src = CDN_INSTRUCTION_MATRIX_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_matrix_parent_class)->copy (object);

	self = CDN_INSTRUCTION_MATRIX (ret);

	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	return ret;
}

static gchar *
cdn_instruction_matrix_to_string (CdnInstruction *instruction)
{
	CdnInstructionMatrix *self;

	self = CDN_INSTRUCTION_MATRIX (instruction);

	return g_strdup_printf ("MAT (%d, %d)",
	                        self->priv->smanip.push.rows,
	                        self->priv->smanip.push.columns);
}

static void
cdn_instruction_matrix_execute (CdnInstruction *instruction,
                                CdnStack       *stack)
{
	// Matrix is a dummy operation
}

static CdnStackManipulation const *
cdn_instruction_matrix_get_stack_manipulation (CdnInstruction  *instruction,
                                               GError         **error)
{
	CdnInstructionMatrix *self;

	self = CDN_INSTRUCTION_MATRIX (instruction);
	return &self->priv->smanip;
}

static gboolean
check_pop_dims (CdnInstructionMatrix *m1,
                CdnInstructionMatrix *m2)
{
	gint i;

	if (m1->priv->smanip.pop.num != m2->priv->smanip.pop.num)
	{
		return FALSE;
	}

	for (i = 0; i < m1->priv->smanip.pop.num; ++i)
	{
		if (m1->priv->smanip.pop.args[i].rows != m2->priv->smanip.pop.args[i].rows ||
		    m1->priv->smanip.pop.args[i].columns != m2->priv->smanip.pop.args[i].columns)
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
cdn_instruction_matrix_equal (CdnInstruction *i1,
                              CdnInstruction *i2,
                              gboolean        asstring)
{
	CdnInstructionMatrix *n1 = CDN_INSTRUCTION_MATRIX (i1);
	CdnInstructionMatrix *n2 = CDN_INSTRUCTION_MATRIX (i2);

	return n1->priv->smanip.push.rows == n2->priv->smanip.push.rows &&
	       n1->priv->smanip.push.columns == n2->priv->smanip.push.columns &&
	       check_pop_dims (n1, n2);
}

static void
cdn_instruction_matrix_finalize (CdnMiniObject *object)
{
	CdnInstructionMatrix *self;

	self = CDN_INSTRUCTION_MATRIX (object);

	cdn_stack_manipulation_destroy (&self->priv->smanip);

	CDN_MINI_OBJECT_CLASS (cdn_instruction_matrix_parent_class)->finalize (object);
}

static void
cdn_instruction_matrix_class_init (CdnInstructionMatrixClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_matrix_finalize;

	object_class->copy = cdn_instruction_matrix_copy;

	inst_class->to_string = cdn_instruction_matrix_to_string;
	inst_class->execute = cdn_instruction_matrix_execute;
	inst_class->get_stack_manipulation = cdn_instruction_matrix_get_stack_manipulation;
	inst_class->equal = cdn_instruction_matrix_equal;

	g_type_class_add_private (object_class, sizeof(CdnInstructionMatrixPrivate));
}

static void
cdn_instruction_matrix_init (CdnInstructionMatrix *self)
{
	/* noop call to suppress clang warning about unused function */
	cdn_instruction_matrix_get_instance_private (self);
	self->priv = CDN_INSTRUCTION_MATRIX_GET_PRIVATE (self);
}

/**
 * cdn_instruction_matrix_new:
 * @args: the #CdnStackArgs
 * @dim: the result dimension
 *
 * Create a new #CdnInstructionMatrix for the given @args. The dimension of the matrix
 * is given by @dim.
 *
 * Returns: (transfer full) (type CdnInstructionMatrix): a new #CdnInstructionMatrix.
 *
 */
CdnInstruction *
cdn_instruction_matrix_new (CdnStackArgs const *args,
                            CdnDimension const *dim)
{
	CdnMiniObject *ret;
	CdnInstructionMatrix *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_MATRIX);
	self = CDN_INSTRUCTION_MATRIX (ret);

	cdn_stack_args_copy (&self->priv->smanip.pop, args);
	self->priv->smanip.push.dimension = *dim;

	return CDN_INSTRUCTION (ret);
}
