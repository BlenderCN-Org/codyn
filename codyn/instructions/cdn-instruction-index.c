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

		struct
		{
			CdnIndexRange range;
			gint n;
		} range;

		struct
		{
			CdnIndexRange rows;
			gint          nrows;

			CdnIndexRange columns;
			gint          ncolumns;
		} block;

		struct
		{
			gint *rows;
			gint *columns;
		} rows_x_columns;
	};

	gint diff_size;

	CdnInstructionIndexType type;
};

G_DEFINE_TYPE (CdnInstructionIndex, cdn_instruction_index, CDN_TYPE_INSTRUCTION)

gint
cdn_index_range_n (CdnIndexRange const *range)
{
	if (range->end < 0)
	{
		return 0;
	}

	return (range->end - range->start) / range->step;
}

gboolean
cdn_index_range_equal (CdnIndexRange const *a,
                       CdnIndexRange const *b)
{
	return a->start == b->start &&
	       a->step == b->step &&
	       a->end == b->end;
}

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

	self->priv->type = src->priv->type;

	switch (self->priv->type)
	{
	case CDN_INSTRUCTION_INDEX_TYPE_OFFSET:
		self->priv->offset = src->priv->offset;
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE:
		self->priv->range = src->priv->range;
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK:
		self->priv->block = src->priv->block;
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_INDEX:
		self->priv->indices = g_memdup (src->priv->indices,
		                                sizeof (gint) * cdn_stack_arg_size (&src->priv->smanip.push));
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS:
		self->priv->rows_x_columns.rows = g_memdup (src->priv->rows_x_columns.rows,
		                                            sizeof (gint) * src->priv->smanip.push.rows);

		self->priv->rows_x_columns.columns = g_memdup (src->priv->rows_x_columns.columns,
		                                               sizeof (gint) * src->priv->smanip.push.columns);

		break;
	}

	self->priv->diff_size = src->priv->diff_size;

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
execute_index (CdnInstructionIndex *self,
               CdnStack            *stack)
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

static void
execute_offset (CdnInstructionIndex *self,
                CdnStack            *stack)
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

static void
execute_range (CdnInstructionIndex *self,
               CdnStack            *stack)
{
	gint i;
	gint idx = self->priv->range.range.start;

	if (self->priv->smanip.pop.num == 1)
	{
		gdouble *readptr;
		gdouble *writeptr;

		readptr = cdn_stack_output_ptr (stack) -
		          cdn_stack_arg_size (self->priv->smanip.pop.args);

		if (self->priv->range.range.step > 0)
		{
			writeptr = readptr;
		}
		else
		{
			writeptr = cdn_stack_output_ptr (stack);
		}

		for (i = 0; i < self->priv->range.n; ++i)
		{
			*writeptr++ = readptr[idx];
			idx += self->priv->range.range.step;
		}

		if (self->priv->range.range.step < 0)
		{
			memcpy (readptr,
				cdn_stack_output_ptr (stack),
				sizeof (gdouble) * self->priv->range.n);
		}

		cdn_stack_popn (stack, self->priv->diff_size);
	}
	else
	{
		for (i = 0; i < self->priv->range.n; ++i)
		{
			cdn_stack_push (stack, (gdouble)idx);
			idx += self->priv->range.range.step;
		}
	}
}

static void
execute_range_block (CdnInstructionIndex *self,
                     CdnStack            *stack)
{
	gboolean needcpy;
	gdouble *readptr;
	gdouble *writeptr;
	gint cidx;
	gint nr;
	gint c;

	needcpy = self->priv->block.rows.step < 0 ||
	          self->priv->block.columns.step < 0;

	readptr = cdn_stack_output_ptr (stack) -
	          cdn_stack_arg_size (self->priv->smanip.pop.args);

	nr = self->priv->smanip.pop.args[0].rows;

	if (needcpy)
	{
		writeptr = cdn_stack_output_ptr (stack);
	}
	else
	{
		writeptr = readptr;
	}

	cidx = self->priv->block.columns.start * nr;

	for (c = 0; c < self->priv->block.ncolumns; ++c)
	{
		gint r;
		gint ridx = self->priv->block.rows.start;

		for (r = 0; r < self->priv->block.nrows; ++r)
		{
			*writeptr++ = readptr[ridx + cidx];
			ridx += self->priv->block.rows.step;
		}

		cidx += self->priv->block.columns.step * nr;
	}

	if (needcpy)
	{
		memcpy (readptr,
		        cdn_stack_output_ptr (stack),
		        sizeof (gdouble) * self->priv->block.nrows * self->priv->block.ncolumns);
	}

	cdn_stack_popn (stack, self->priv->diff_size);
}

static void
execute_rows_x_columns (CdnInstructionIndex *self,
                        CdnStack            *stack)
{
	gdouble *retptr;
	gdouble *endptr;
	gint n;
	gint nret;
	gint c;
	CdnDimension d;
	CdnDimension dout;

	d = self->priv->smanip.pop.args[0].dimension;
	n = cdn_dimension_size (&d);

	dout = self->priv->smanip.push.dimension;
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

	// Construct resulting matrix by indexing rows-cross-columns
	for (c = 0; c < dout.columns; ++c)
	{
		gint r;
		gint istart;

		istart = self->priv->rows_x_columns.columns[c] * d.rows;

		for (r = 0; r < dout.rows; ++r)
		{
			gint i;

			i = istart + self->priv->rows_x_columns.rows[r];

			*retptr++ = endptr[i];
		}
	}

	cdn_stack_set_output_ptr (stack, retptr);
}

static void
cdn_instruction_index_execute (CdnInstruction *instruction,
                               CdnStack       *stack)
{
	CdnInstructionIndex *self = (CdnInstructionIndex *)instruction;

	switch (self->priv->type)
	{
	case CDN_INSTRUCTION_INDEX_TYPE_INDEX:
		execute_index (self, stack);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_OFFSET:
		execute_offset (self, stack);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE:
		execute_range (self, stack);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK:
		execute_range_block (self, stack);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS:
		execute_rows_x_columns (self, stack);
		break;
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
indices_equal (gint const *i1,
               gint        ni1,
               gint const *i2,
               gint        ni2)
{
	if (ni1 != ni2)
	{
		return FALSE;
	}

	return memcmp (i1, i2, sizeof (gint) * ni1) == 0;
}

static gboolean
cdn_instruction_index_equal (CdnInstruction *i1,
                             CdnInstruction *i2,
                             gboolean        asstring)
{
	CdnInstructionIndex *n1 = CDN_INSTRUCTION_INDEX (i1);
	CdnInstructionIndex *n2 = CDN_INSTRUCTION_INDEX (i2);
	gboolean dimeq;

	dimeq = cdn_dimension_equal (&n1->priv->smanip.push.dimension,
	                             &n2->priv->smanip.push.dimension);

	if (!dimeq)
	{
		return FALSE;
	}

	if (n1->priv->type != n2->priv->type)
	{
		return FALSE;
	}

	switch (n1->priv->type)
	{
	case CDN_INSTRUCTION_INDEX_TYPE_INDEX:
	{
		return indices_equal (n1->priv->indices,
		                      cdn_dimension_size (&n1->priv->smanip.push.dimension),
		                      n2->priv->indices,
		                      cdn_dimension_size (&n2->priv->smanip.push.dimension));
	}
	case CDN_INSTRUCTION_INDEX_TYPE_OFFSET:
		return n1->priv->offset == n2->priv->offset;
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE:
		return cdn_index_range_equal (&n1->priv->range.range,
		                              &n2->priv->range.range);
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK:
		return cdn_index_range_equal (&n1->priv->block.rows,
		                              &n2->priv->block.rows) &&
		       cdn_index_range_equal (&n1->priv->block.columns,
		                              &n2->priv->block.columns);
	case CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS:
		return indices_equal (n1->priv->rows_x_columns.rows,
		                      n1->priv->smanip.push.dimension.rows,
		                      n2->priv->rows_x_columns.rows,
		                      n1->priv->smanip.push.dimension.rows) &&
		       indices_equal (n1->priv->rows_x_columns.columns,
		                      n1->priv->smanip.push.dimension.columns,
		                      n2->priv->rows_x_columns.columns,
		                      n2->priv->smanip.push.dimension.columns);
	}

	return FALSE;
}

static void
cdn_instruction_index_finalize (CdnMiniObject *object)
{
	CdnInstructionIndex *self;

	self = CDN_INSTRUCTION_INDEX (object);

	cdn_stack_manipulation_destroy (&self->priv->smanip);

	switch (self->priv->type)
	{
	case CDN_INSTRUCTION_INDEX_TYPE_INDEX:
		g_free (self->priv->indices);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS:
		g_free (self->priv->rows_x_columns.rows);
		g_free (self->priv->rows_x_columns.columns);
	default:
		break;
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

/**
 * cdn_instruction_index_new:
 * @indices: Description.
 * @retdim: a #CdnDimension.
 * @arg: a #CdnStackArg.
 *
 * Create a new linear index instruction.
 *
 * Returns: (transfer full): a #CdnInstruction.
 *
 **/
CdnInstruction *
cdn_instruction_index_new (gint               *indices,
                           CdnDimension const *retdim,
                           CdnStackArg const  *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->type = CDN_INSTRUCTION_INDEX_TYPE_INDEX;
	self->priv->indices = indices;

	self->priv->smanip.push.dimension = *retdim;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));
	self->priv->smanip.extra_space = cdn_dimension_size (retdim);

	self->priv->diff_size = cdn_dimension_size (&arg->dimension) - cdn_dimension_size (retdim);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_index_new_rows_x_columns:
 * @rows: (array length=n_rows) (transfer full): row indices.
 * @n_rows: number of row indices in @rows.
 * @columns: (array length=n_columns) (transfer full): column indices.
 * @n_columns: number of column indices in @columns.
 * @arg: a #CdnStackArg.
 *
 * Create a new range index instruction. @arg can be %NULL in which case the
 * range always represents a row. If @arg is not %NULL however, the range will
 * be a column or row depending on the dimension of @arg.
 *
 * Returns: (transfer full): a #CdnInstruction.
 *
 **/
CdnInstruction *
cdn_instruction_index_new_rows_x_columns (gint              *rows,
                                          gint               n_rows,
                                          gint              *columns,
                                          gint               n_columns,
                                          CdnStackArg const *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->type = CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS;

	self->priv->smanip.push.dimension.rows = n_rows;
	self->priv->smanip.push.dimension.columns = n_columns;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	self->priv->rows_x_columns.rows = rows;
	self->priv->rows_x_columns.columns = columns;

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));
	self->priv->smanip.extra_space = n_rows * n_columns;

	self->priv->diff_size = cdn_dimension_size (&arg->dimension) - n_rows * n_columns;

	return CDN_INSTRUCTION (self);
}

/**
 * cdn_instruction_index_new_range:
 * @range: a #CdnIndexRange.
 * @arg: (allow-none): a #CdnStackArg.
 *
 * Create a new range index instruction. @arg can be %NULL in which case the
 * range always represents a row. If @arg is not %NULL however, the range will
 * be a column or row depending on the dimension of @arg.
 *
 * Returns: (transfer full): a #CdnInstruction.
 *
 **/
CdnInstruction *
cdn_instruction_index_new_range (CdnIndexRange const *range,
                                 CdnStackArg const   *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;
	CdnDimension retdim;
	gint n;

	n = cdn_index_range_n (range);

	if (arg && arg->dimension.columns == 1)
	{
		retdim.rows = n;
		retdim.columns = 1;
	}
	else
	{
		retdim.rows = 1;
		retdim.columns = n;
	}

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->type = CDN_INSTRUCTION_INDEX_TYPE_RANGE;

	self->priv->range.range = *range;
	self->priv->range.n = n;

	self->priv->smanip.push.dimension = retdim;

	if (arg)
	{
		cdn_stack_args_init (&self->priv->smanip.pop, 1);
		cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

		if (range->step < 0)
		{
			self->priv->smanip.extra_space = n;
		}

		self->priv->diff_size = cdn_stack_arg_size (arg) -
		                        cdn_dimension_size (&retdim);
	}

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_index_new_range_block:
 * @rows: a #CdnIndexRange.
 * @columns: a #CdnIndexRange.
 * @arg: a #CdnStackArg.
 *
 * Create a new range block index instruction. The range block indexes its
 * argument using a range of rows and a range of columns selecting the block
 * represented by the intersection (in terms of rows-x-columns) of the ranges.
 *
 * Returns: (transfer full): a #CdnInstruction.
 *
 **/
CdnInstruction *
cdn_instruction_index_new_range_block (CdnIndexRange const *rows,
                                       CdnIndexRange const *columns,
                                       CdnStackArg const   *arg)
{
	CdnMiniObject *ret;
	CdnInstructionIndex *self;
	CdnDimension retdim;

	retdim.rows = cdn_index_range_n (rows);
	retdim.columns = cdn_index_range_n (columns);

	ret = cdn_mini_object_new (CDN_TYPE_INSTRUCTION_INDEX);
	self = CDN_INSTRUCTION_INDEX (ret);

	self->priv->type = CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK;

	self->priv->block.rows = *rows;
	self->priv->block.nrows = retdim.rows;
	self->priv->block.columns = *columns;
	self->priv->block.ncolumns = retdim.columns;

	self->priv->smanip.push.dimension = retdim;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	if (rows->step < 0 || columns->step < 0)
	{
		self->priv->smanip.extra_space = cdn_dimension_size (&retdim);
	}

	self->priv->diff_size = cdn_stack_arg_size (arg) -
	                        cdn_dimension_size (&retdim);

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));

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

	self->priv->type = CDN_INSTRUCTION_INDEX_TYPE_OFFSET;
	self->priv->offset = offset;

	self->priv->smanip.push.dimension = *retdim;

	cdn_stack_args_init (&self->priv->smanip.pop, 1);
	cdn_stack_arg_copy (self->priv->smanip.pop.args, arg);

	cdn_instruction_index_recalculate_sparsity (CDN_INSTRUCTION (self));

	self->priv->diff_size = cdn_dimension_size (&arg->dimension) - cdn_dimension_size (retdim);

	return CDN_INSTRUCTION (ret);
}

/**
 * cdn_instruction_index_get_index_type:
 * @instr: a #CdnInstructionIndex.
 *
 * Get the index type.
 *
 * Returns: the index type.
 *
 **/
CdnInstructionIndexType
cdn_instruction_index_get_index_type (CdnInstructionIndex *instr)
{
	g_return_val_if_fail (CDN_IS_INSTRUCTION_INDEX (instr),
	                      CDN_INSTRUCTION_INDEX_TYPE_INDEX);

	return instr->priv->type;
}

/**
 * cdn_instruction_index_get_range:
 * @instr: a #CdnInstructionIndex.
 *
 * Get the range of the index range instruction. This is only valid with the
 * type of the index instruction is #CDN_INSTRUCTION_INDEX_TYPE_RANGE.
 *
 * Returns: Description.
 *
 **/
CdnIndexRange const *
cdn_instruction_index_get_range (CdnInstructionIndex *instr)
{
	if (instr->priv->type == CDN_INSTRUCTION_INDEX_TYPE_RANGE)
	{
		return &instr->priv->range.range;
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_instruction_index_get_range_block:
 * @instr: a #CdnInstructionIndex.
 * @rows: (out): rows range.
 * @columns: (out): columns range.
 *
 * Get the range blocks of the index range block instruction. This is only
 * valid with the type of the index instruction is
 * #CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK.
 *
 **/
void
cdn_instruction_index_get_range_block (CdnInstructionIndex *instr,
                                       CdnIndexRange       *rows,
                                       CdnIndexRange       *columns)
{
	if (instr->priv->type == CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK)
	{
		if (rows)
		{
			*rows = instr->priv->block.rows;
		}

		if (columns)
		{
			*columns = instr->priv->block.columns;
		}
	}
}

/**
 * cdn_instruction_index_get_offset:
 * @instr: a #CdnInstructionIndex.
 *
 * Get the offset of the offset index instruction. This is only valid with the
 * type of the index instruction is #CDN_INSTRUCTION_INDEX_TYPE_OFFSET.
 *
 * Returns: the index offset.
 *
 **/
gint
cdn_instruction_index_get_offset (CdnInstructionIndex *instr)
{
	if (instr->priv->type == CDN_INSTRUCTION_INDEX_TYPE_OFFSET)
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
 * Get the indices array of the index instruction. This is only valid with the
 * type of the index instruction is #CDN_INSTRUCTION_INDEX_TYPE_INDEX.
 *
 * Returns: (array-length length): an array of #gint. Note that the memory belongs to the instruction
 *                                 and should not be freed.
 *
 **/
gint const *
cdn_instruction_index_get_indices (CdnInstructionIndex *instr,
                                   gint                *length)
{
	if (length)
	{
		*length = 0;
	}

	g_return_val_if_fail (CDN_IS_INSTRUCTION_INDEX (instr), NULL);

	if (instr->priv->type == CDN_INSTRUCTION_INDEX_TYPE_INDEX)
	{
		if (length)
		{
			*length = cdn_stack_arg_size (&instr->priv->smanip.push);
		}

		return instr->priv->indices;
	}

	return NULL;
}

gint
cdn_instruction_index_num_indices (CdnInstructionIndex *instr)
{
	CdnDimension const *retdim;

	g_return_val_if_fail (CDN_IS_INSTRUCTION_INDEX (instr), 0);

	retdim = &instr->priv->smanip.push.dimension;
	return cdn_dimension_size (retdim);
}

gboolean
cdn_instruction_index_write_indices (CdnInstructionIndex *instr,
                                     gint                *indices,
                                     gint                 l)
{
	CdnDimension const *dim = NULL;
	CdnDimension const *retdim = NULL;
	gint n;

	g_return_val_if_fail (CDN_IS_INSTRUCTION_INDEX (instr), FALSE);
	g_return_val_if_fail (indices != NULL, FALSE);

	retdim = &instr->priv->smanip.push.dimension;
	n = cdn_dimension_size (retdim);

	if (l != -1 && n > l)
	{
		return FALSE;
	}

	if (instr->priv->smanip.pop.num == 1)
	{
		dim = &instr->priv->smanip.pop.args[0].dimension;
	}

	switch (instr->priv->type)
	{
	case CDN_INSTRUCTION_INDEX_TYPE_INDEX:
		memcpy (indices,
		        instr->priv->indices,
		        sizeof (gint) * n);
		break;
	case CDN_INSTRUCTION_INDEX_TYPE_OFFSET:
	{
		gint i;

		for (i = instr->priv->offset; i < instr->priv->offset + n; ++i)
		{
			*indices++ = i;
		}
		break;
	}
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE:
	{
		gint i;
		gint idx = instr->priv->range.range.start;

		for (i = 0; i < n; ++i)
		{
			*indices++ = idx;
			idx += instr->priv->range.range.step;
		}

		break;
	}
	case CDN_INSTRUCTION_INDEX_TYPE_RANGE_BLOCK:
	{
		gint cidx = instr->priv->block.columns.start;
		gint c;

		for (c = 0; c < instr->priv->block.ncolumns; ++c)
		{
			gint ridx = instr->priv->block.rows.start;
			gint r;
			gint lridx;

			lridx = cidx * dim->rows;

			for (r = 0; r < instr->priv->block.nrows; ++r)
			{
				*indices++ = lridx + ridx;
				ridx += instr->priv->block.rows.step;
			}

			cidx += instr->priv->block.columns.step;
		}

		break;
	}
	case CDN_INSTRUCTION_INDEX_TYPE_ROWS_X_COLUMNS:
	{
		gint c;
		CdnDimension d;
		CdnDimension dout;

		d = instr->priv->smanip.pop.args[0].dimension;
		dout = instr->priv->smanip.push.dimension;

		for (c = 0; c < dout.columns; ++c)
		{
			gint r;
			gint istart;

			istart = d.rows * instr->priv->rows_x_columns.columns[c];

			for (r = 0; r < dout.rows; ++r)
			{
				*indices++ = istart + instr->priv->rows_x_columns.rows[r];
			}
		}
		break;
	}
	}

	return TRUE;
}

