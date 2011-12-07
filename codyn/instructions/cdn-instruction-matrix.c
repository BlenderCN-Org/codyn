#include "cdn-instruction-matrix.h"

#define CDN_INSTRUCTION_MATRIX_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrixPrivate))

struct _CdnInstructionMatrixPrivate
{
	CdnStackManipulation smanip;

	gint push_dims[2];
};

G_DEFINE_TYPE (CdnInstructionMatrix, cdn_instruction_matrix, CDN_TYPE_INSTRUCTION)

static CdnMiniObject *
cdn_instruction_matrix_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionMatrix *self;
	CdnInstructionMatrix const *src;
	gint n;
	gint i;

	src = CDN_INSTRUCTION_MATRIX_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_matrix_parent_class)->copy (object);

	self = CDN_INSTRUCTION_MATRIX (ret);

	self->priv->push_dims[0] = src->priv->push_dims[0];
	self->priv->push_dims[1] = src->priv->push_dims[1];

	self->priv->smanip.num_pop = src->priv->smanip.num_pop;

	n = src->priv->smanip.num_pop * 2;

	self->priv->smanip.pop_dims = g_new (gint, n);

	for (i = 0; i < n; ++i)
	{
		self->priv->smanip.pop_dims[i] = src->priv->smanip.pop_dims[i];
	}

	return ret;
}

static gchar *
cdn_instruction_matrix_to_string (CdnInstruction *instruction)
{
	CdnInstructionMatrix *self;

	self = CDN_INSTRUCTION_MATRIX (instruction);

	return g_strdup_printf ("MAT (%d, %d)",
	                        self->priv->push_dims[0],
	                        self->priv->push_dims[1]);
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

	for (i = 0; i < m1->priv->smanip.num_pop * 2; ++i)
	{
		if (m1->priv->smanip.pop_dims[i] != m2->priv->smanip.pop_dims[i])
		{
			return FALSE;
		}
	}

	return TRUE;
}

static gboolean
cdn_instruction_matrix_equal (CdnInstruction *i1,
                              CdnInstruction *i2)
{
	CdnInstructionMatrix *n1 = CDN_INSTRUCTION_MATRIX (i1);
	CdnInstructionMatrix *n2 = CDN_INSTRUCTION_MATRIX (i2);

	return n1->priv->push_dims[0] == n2->priv->push_dims[0] &&
	       n1->priv->push_dims[1] == n2->priv->push_dims[1] &&
	       n1->priv->smanip.num_pop == n2->priv->smanip.num_pop &&
	       check_pop_dims (n1, n2);
}

static void
cdn_instruction_matrix_finalize (CdnMiniObject *object)
{
	CdnInstructionMatrix *self;

	self = CDN_INSTRUCTION_MATRIX (object);

	g_free (self->priv->smanip.pop_dims);

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
	self->priv = CDN_INSTRUCTION_MATRIX_GET_PRIVATE (self);

	self->priv->smanip.push_dims = self->priv->push_dims;
	self->priv->smanip.num_push = 1;
}

CdnInstruction *
cdn_instruction_matrix_new (gint  numpop,
                            gint *popdims,
                            gint  numr,
                            gint  numc)
{
	CdnMiniObject *ret;
	CdnInstructionMatrix *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_MATRIX);
	self = CDN_INSTRUCTION_MATRIX (ret);

	self->priv->smanip.num_pop = numpop;
	self->priv->smanip.pop_dims = popdims;

	self->priv->push_dims[0] = numr;
	self->priv->push_dims[1] = numc;

	return CDN_INSTRUCTION (ret);
}
