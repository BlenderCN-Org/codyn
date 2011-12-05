#include "cdn-instruction-matrix.h"

#define CDN_INSTRUCTION_MATRIX_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_MATRIX, CdnInstructionMatrixPrivate))

struct _CdnInstructionMatrixPrivate
{
	CdnStackManipulation smanip;

	gint pop_manip[2];
	gint push_manip[2];
};

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

	self->priv->push_manip[0] = src->priv->push_manip[0];
	self->priv->push_manip[1] = src->priv->push_manip[1];

	self->priv->pop_manip[0] = src->priv->pop_manip[0];
	self->priv->pop_manip[1] = src->priv->pop_manip[1];

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
cdn_instruction_matrix_get_stack_manipulation (CdnInstruction *instruction)
{
	return &instruction->priv->smanip;
}

static gboolean
cdn_instruction_matrix_equal (CdnInstruction *i1,
                              CdnInstruction *i2)
{
	CdnInstructionMatrix *n1 = CDN_INSTRUCTION_MATRIX (i1);
	CdnInstructionMatrix *n2 = CDN_INSTRUCTION_MATRIX (i2);

	return n1->priv->push_dims[0] == n2->priv->push_dims[0] &&
	       n1->priv->push_dims[1] == n2->priv->push_dims[1] &&
	       n1->priv->pop_dims[0] == n2->priv->pop_dims[0] &&
	       n1->priv->pop_dims[1] == n2->priv->pop_dims[1];
}

static void
cdn_instruction_matrix_class_init (CdnInstructionMatrixClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

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

	self->priv->smanip.push_dims = &self->priv->push_manip;
	self->priv->smaniip.pop_dims = &self->priv->pop_manip;
}

CdnInstruction *
cdn_instruction_matrix_new (gint numr,
                            gint numc)
{
	CdnMiniObject *ret;
	CdnInstructionMatrix *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_MATRIX);
	self = CDN_INSTRUCTION_MATRIX (ret);

	self->priv->smanip.num_pop = 1;
	self->priv->smanip.num_push = 1;

	self->priv->pop_manip[0] = numr;
	self->priv->pop_manip[1] = numc;

	self->priv->push_manip[0] = numr;
	self->priv->push_manip[1] = numc;

	return CDN_INSTRUCTION (ret);
}
