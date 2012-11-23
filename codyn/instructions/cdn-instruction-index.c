#include "cdn-instruction-index.h"
#include <string.h>

#define CDN_INSTRUCTION_INDEX_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INSTRUCTION_INDEX, CdnInstructionIndexPrivate))

struct _CdnInstructionIndexPrivate
{
	CdnStackManipulation smanip;

	union
	{
		gint *indices;
		gint offset;
	};

	gint diff_size;
	guint is_offset : 1;
};

G_DEFINE_TYPE (CdnInstructionIndex, cdn_instruction_index, CDN_TYPE_INSTRUCTION)

static CdnMiniObject *
cdn_instruction_index_copy (CdnMiniObject *object)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;
	CdnInstructionIndex const *src;

	src = CDN_INSTRUCTION_INDEX_CONST (object);

	ret = CDN_MINI_OBJECT_CLASS (cdn_instruction_index_parent_class)->copy (object);

	self = CDN_INSTRUCTION_INDEX (ret);
	cdn_stack_manipulation_copy (&self->priv->smanip, &src->priv->smanip);

	self->priv->is_offset = src->priv->is_offset;

	if (self->priv->is_offset)
	{
		self->priv->offset = src->priv->offset;
	}
	else
	{
		self->priv->indices = g_memdup (src->priv->indices,
		                                sizeof (gint) * cdn_stack_arg_size (&src->priv->smanip.push));
	}

	return ret;
}

static gchar *
cdn_instruction_index_to_string (CdnInstruction *instruction)
{
	CdnInstructionIndex *self;

	self = CDN_INSTRUCTION_INDEX (instruction);

	return g_strdup_printf ("IDX (%d, %d)",
	                        self->priv->smanip.push.rows,
	                        self->priv->smanip.push.columns);
}

static void
cdn_instruction_index_execute (CdnInstruction *instruction,
                               CdnStack       *stack)
{
	CdnInstructionIndex *self = (CdnInstructionIndex *)instruction;

	if (self->priv->is_offset)
	{
		if (self->priv->offset == 0)
		{
			cdn_stack_popn (stack, self->priv->diff_size);
		}
		else
		{
			// Need to copy
			gdouble *retptr;

			retptr = cdn_stack_output_ptr (stack) -
			         cdn_stack_arg_size (self->priv->smanip.pop.args);

			memmove (retptr,
			         retptr + self->priv->offset,
			         sizeof (gdouble) * cdn_stack_arg_size (&self->priv->smanip.push));

			cdn_stack_popn (stack, self->priv->diff_size);
		}
	}
	else
	{
		gdouble *retptr;
		gdouble *endptr;
		gint n;
		gint nret;
		gint i;

		n = cdn_stack_arg_size (self->priv->smanip.pop.args);
		nret = cdn_stack_arg_size (&self->priv->smanip.push);

		endptr = cdn_stack_output_ptr (stack) - self->priv->diff_size;
		retptr = cdn_stack_output_ptr (stack) - n;

		// First copy in the back
		if (self->priv->diff_size <= 0)
		{
			memcpy (endptr, retptr, sizeof (gdouble) * n);
		}
		else
		{
			memmove (endptr, retptr, sizeof (gdouble) * n);
		}

		// Then calculate indices
		for (i = 0; i < nret; ++i)
		{
			*retptr++ = endptr[self->priv->indices[i]];
		}

		cdn_stack_set_output_ptr (stack, retptr);
	}
}

static CdnStackManipulation const *
cdn_instruction_index_get_stack_manipulation (CdnInstruction  *instruction,
                                               GError         **error)
{
	CdnInstructionIndex *self;

	self = CDN_INSTRUCTION_INDEX (instruction);
	return &self->priv->smanip;
}

static gboolean
cdn_instruction_index_equal (CdnInstruction *i1,
                             CdnInstruction *i2,
                             gboolean        asstring)
{
	CdnInstructionIndex *n1 = CDN_INSTRUCTION_INDEX (i1);
	CdnInstructionIndex *n2 = CDN_INSTRUCTION_INDEX (i2);
	gint n;
	gint i;

	if (!asstring && !cdn_dimension_equal (&n1->priv->smanip.push.dimension,
	                                       &n2->priv->smanip.push.dimension))
	{
		return FALSE;
	}

	if (n1->priv->is_offset != n2->priv->is_offset)
	{
		return FALSE;
	}

	if (n1->priv->is_offset)
	{
		return n1->priv->offset == n2->priv->offset;
	}

	n = cdn_dimension_size (&n1->priv->smanip.push.dimension);

	if (n != cdn_dimension_size (&n2->priv->smanip.push.dimension))
	{
		return FALSE;
	}

	for (i = 0; i < n; ++i)
	{
		if (n1->priv->indices[i] != n2->priv->indices[i])
		{
			return FALSE;
		}
	}

	return TRUE;
}

static void
cdn_instruction_index_finalize (CdnMiniObject *object)
{
	CdnInstructionIndex *self;

	self = CDN_INSTRUCTION_INDEX (object);

	cdn_stack_manipulation_destroy (&self->priv->smanip);

	if (!self->priv->is_offset)
	{
		g_free (self->priv->indices);
	}

	CDN_MINI_OBJECT_CLASS (cdn_instruction_index_parent_class)->finalize (object);
}

static void
cdn_instruction_index_recalculate_sparsity (CdnInstruction *instruction)
{
	// TODO
}

static void
cdn_instruction_index_class_init (CdnInstructionIndexClass *klass)
{
	CdnMiniObjectClass *object_class = CDN_MINI_OBJECT_CLASS (klass);
	CdnInstructionClass *inst_class = CDN_INSTRUCTION_CLASS (klass);

	object_class->finalize = cdn_instruction_index_finalize;

	object_class->copy = cdn_instruction_index_copy;

	inst_class->to_string = cdn_instruction_index_to_string;
	inst_class->execute = cdn_instruction_index_execute;
	inst_class->get_stack_manipulation = cdn_instruction_index_get_stack_manipulation;
	inst_class->equal = cdn_instruction_index_equal;
	inst_class->recalculate_sparsity = cdn_instruction_index_recalculate_sparsity;

	g_type_class_add_private (object_class, sizeof(CdnInstructionIndexPrivate));
}

static void
cdn_instruction_index_init (CdnInstructionIndex *self)
{
	self->priv = CDN_INSTRUCTION_INDEX_GET_PRIVATE (self);
}

CdnInstruction *
cdn_instruction_index_new (gint               *indices,
                           CdnDimension const *retdim,
                           CdnStackArg const  *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->is_offset = FALSE;
	self->priv->indices = indices;

	self->priv->smanip.push.dimension = *retdim;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));
	self->priv->smanip.extra_space = cdn_dimension_size (retdim);

	self->priv->diff_size = cdn_dimension_size (&arg->dimension) - cdn_dimension_size (retdim);

	return CDN_INSTRUCTION (ret);
}

CdnInstruction *
cdn_instruction_index_new_offset (gint                offset,
                                  CdnDimension const *retdim,
                                  CdnStackArg const  *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->is_offset = TRUE;
	self->priv->offset = offset;

	self->priv->smanip.push.dimension = *retdim;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));

	self->priv->diff_size = cdn_dimension_size (&arg->dimension) - cdn_dimension_size (retdim);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_index_is_offset:
 * @instr: a #CdnInstructionIndex.
 *
 * Get whether the index instruction is an index offset instruction.
 *
 * Returns: %TRUE if the instruction is an index offset instruction, %FALSE otherwise.
 *
 **/
gboolean
cdn_instruction_index_is_offset (CdnInstructionIndex *instr)
{
	return instr->priv->is_offset;
}

/**
 * cdn_instruction_index_get_offset:
 * @instr: a #CdnInstructionIndex.
 *
 * Get the offset of the index instruction. Note that this is only valid
 * for offset index instructions (see #cdn_instruction_index_is_offset).
 *
 * Returns: the index offset.
 *
 **/
gint
cdn_instruction_index_get_offset  (CdnInstructionIndex *instr)
{
	if (instr->priv->is_offset)
	{
		return instr->priv->offset;
	}
	else
	{
		return -1;
	}
}

/**
 * cdn_instruction_index_get_indices:
 * @instr: a #CdnInstructionIndex.
 * @length: (out): return value for the length of the indices array.
 *
 * Get the indices array of the index instruction. This is only valid if
 * the index instruction is not an offset instruction
 * (see #cdn_instruction_index_is_offset).
 *
 * Returns: (array-length length): an array of #gint. Note that the memory belongs to the instruction
 *                                 and should not be freed.
 *
 **/
gint const *
cdn_instruction_index_get_indices (CdnInstructionIndex *instr,
                                   gint                *length)
{
	if (instr->priv->is_offset)
	{
		if (length)
		{
			*length = 0;
		}

		return NULL;
	}
	else
	{
		if (length)
		{
			*length = cdn_stack_arg_size (&instr->priv->smanip.push);
		}

		return instr->priv->indices;
	}
}

