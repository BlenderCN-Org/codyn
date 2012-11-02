#include "cdn-matrix.h"

#include <string.h>

static CdnMatrix *
cdn_matrix_boxed_copy (CdnMatrix *matrix)
{
	return cdn_matrix_new (cdn_matrix_get (matrix),
	                       &matrix->dimension);
}

// Do not use G_DEFINE_BOXED_TYPE here because the C# API parser doesn't
// understand
GType
cdn_matrix_get_type (void)
{
	static GType gtype = 0;

	if (G_UNLIKELY (gtype == 0))
	{
		gtype = g_boxed_type_register_static ("CdnMatrix",
		                                      (GBoxedCopyFunc)cdn_matrix_boxed_copy,
		                                      (GBoxedFreeFunc)cdn_matrix_free);
	}

	return gtype;

}

/**
 * cdn_matrix_new:
 * @values: matrix values (or %NULL).
 * @dimension: the matrix dimensions.
 *
 * Create a new matrix value of a given dimension.
 *
 * Returns: (transfer full): a #CdnMatrix.
 *
 **/
CdnMatrix *
cdn_matrix_new (gdouble const      *values,
                CdnDimension const *dimension)
{
	CdnMatrix *matrix;

	matrix = g_slice_new0 (CdnMatrix);

	matrix->dimension.rows = 1;
	matrix->dimension.columns = 1;

	cdn_matrix_set (matrix, values, dimension);
	return matrix;
}

/**
 * cdn_matrix_new_one:
 * @value: the matrix value.
 *
 * Create a new 1x1 matrix.
 *
 * Returns: (transfer full): a #CdnMatrix.
 *
 **/
CdnMatrix *
cdn_matrix_new_one (gdouble value)
{
	return cdn_matrix_new (&value, cdn_dimension_onep);
}

/**
 * cdn_matrix_free:
 * @matrix: a #CdnMatrix.
 *
 * Free a matrix value previously created with cdn_matrix_new.
 *
 **/
void
cdn_matrix_free (CdnMatrix *matrix)
{
	cdn_matrix_destroy (matrix);
	g_slice_free (CdnMatrix, matrix);
}

/**
 * cdn_matrix_destroy:
 * @matrix: a #CdnMatrix.
 *
 * Destroy a matrix value allocated on the stack.
 *
 **/
void
cdn_matrix_destroy (CdnMatrix *matrix)
{
	if (!cdn_dimension_is_one (&matrix->dimension))
	{
		g_free (matrix->values);
		matrix->values = NULL;
	}

	matrix->value = 0;

	matrix->dimension.rows = 1;
	matrix->dimension.columns = 1;
}

/**
 * cdn_matrix_set_one:
 * @matrix: a #CdnMatrix.
 * @value: matrix value.
 *
 * Set the matrix value from a single matrix (matrix will become 1x1).
 *
 **/
void
cdn_matrix_set_one (CdnMatrix *matrix,
                    gdouble    value)
{
	cdn_matrix_set (matrix, &value, cdn_dimension_onep);
}

/**
 * cdn_matrix_set: (skip):
 * @matrix: a #CdnMatrix.
 * @values: the matrix values.
 * @dimension: the values dimension.
 *
 * Set the values of a matrix value to the given values. The matrix will
 * automatically resize if needed.
 *
 **/
void
cdn_matrix_set (CdnMatrix          *matrix,
                gdouble const      *values,
                CdnDimension const *dimension)
{
	gint dimsize;
	gint mydimsize;
	gint bsize;

	dimsize = cdn_dimension_size (dimension);
	mydimsize = cdn_dimension_size (&matrix->dimension);

	bsize = dimsize * sizeof (gdouble);

	if (dimsize != mydimsize && mydimsize > 1)
	{
		g_free (matrix->values);
		matrix->value = 0;
	}

	if (dimsize <= 1)
	{
		if (values)
		{
			matrix->value = *values;
		}
	}
	else if (dimsize == mydimsize)
	{
		if (values)
		{
			memcpy (matrix->values, values, bsize);
		}
		else
		{
			memset (matrix->values, 0, bsize);
		}
	}
	else
	{
		if (values)
		{
			matrix->values = g_memdup (values, bsize);
		}
		else
		{
			matrix->values = g_new0 (gdouble, dimsize);
		}
	}

	matrix->dimension = *dimension;
}

/**
 * cdn_matrix_get: (skip):
 * @matrix: a #CdnMatrix.
 *
 * Get the values memory of a matrix for reading values. For writing values,
 * use #cdn_matrix_get_memory. Note that the returned value is pointing to
 * the matrix internal memory.
 *
 * Returns: the matrix values.
 *
 **/
gdouble const *
cdn_matrix_get (CdnMatrix const *matrix)
{
	if (cdn_dimension_is_one (&matrix->dimension))
	{
		return &matrix->value;
	}
	else
	{
		return matrix->values;
	}
}

/**
 * cdn_matrix_get: (skip):
 * @matrix: a #CdnMatrix.
 *
 * Get the values memory of a matrix for writing values. For reading values,
 * use #cdn_matrix_get. Note that the returned value is pointing to
 * the matrix internal memory and does not need to be freed.
 *
 * Returns: (transfer none): the matrix values.
 *
 **/
gdouble *
cdn_matrix_get_memory (CdnMatrix    *matrix)
{
	return (gdouble *)cdn_matrix_get (matrix);
}

/**
 * cdn_matrix_get_flat:
 * @matrix: a #CdnMatrix.
 * @length: (out): return value for the length.
 *
 * Get the values memory of a matrix. This method is mostly for writing bindings
 * and should not be used.
 *
 * Returns: the matrix values.
 *
 **/
gdouble const *
cdn_matrix_get_flat (CdnMatrix const *matrix,
                     gint            *length)
{
	if (length)
	{
		*length = cdn_dimension_size (&matrix->dimension);
	}

	return cdn_matrix_get (matrix);
}

/**
 * cdn_matrix_size:
 * @matrix: a #CdnMatrix.
 *
 * Get the total size (in number of values) of the matrix (i.e. rows-x-columns).
 *
 * Returns: the size of the matrix.
 *
 **/
gint
cdn_matrix_size (CdnMatrix const *matrix)
{
	return cdn_dimension_size (&matrix->dimension);
}

/**
 * cdn_matrix_copy:
 * @dest: a #CdnMatrix.
 * @src: a #CdnMatrix.
 *
 * Copy the matrix values from @src to @dest.
 *
 **/
void
cdn_matrix_copy (CdnMatrix       *dest,
                 CdnMatrix const *src)
{
	cdn_matrix_set (dest, cdn_matrix_get (src), &src->dimension);
}

/**
 * cdn_matrix_copy_to: (skip):
 * @matrix: a #CdnMatrix.
 * @values: matrix values.
 *
 * Copy the values of @matrix into the memory pointed to by @values. You must
 * make sure that @values is large enough to hold the values from @matrix.
 *
 **/
void
cdn_matrix_copy_to (CdnMatrix const *matrix,
                    gdouble         *values)
{
	memcpy (values, cdn_matrix_get (matrix), cdn_matrix_size (matrix) * sizeof (gdouble));
}

/**
 * cdn_matrix_clear:
 * @matrix: a #CdnMatrix.
 *
 * Clear all matrix values to 0.
 *
 **/
void
cdn_matrix_clear (CdnMatrix *matrix)
{
	if (cdn_dimension_is_one (&matrix->dimension))
	{
		matrix->value = 0;
	}
	else
	{
		memset (matrix->values, 0, sizeof (gdouble) * cdn_matrix_size (matrix));
	}
}
