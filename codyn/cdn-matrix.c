#include "cdn-matrix.h"

#include <string.h>

CdnMatrix *
cdn_matrix_new (gdouble const *values,
                CdnDimension  *dimension)
{
	CdnMatrix *matrix;

	matrix = g_slice_new0 (CdnMatrix);

	if (values == NULL)
	{
		return matrix;
	}

	cdn_matrix_set (matrix, values, dimension);
	return matrix;
}

CdnMatrix *
cdn_matrix_new_one (gdouble value)
{
	CdnDimension dimension = CDN_DIMENSION(1, 1);

	return cdn_matrix_new (&value, &dimension);
}

void
cdn_matrix_free (CdnMatrix *matrix)
{
	cdn_matrix_destroy (matrix);
	g_slice_free (CdnMatrix, matrix);
}

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

void
cdn_matrix_set (CdnMatrix     *matrix,
                gdouble const *values,
                CdnDimension  *dimension)
{
	gint dimsize;
	gint mydimsize;

	dimsize = cdn_dimension_size (dimension);
	mydimsize = cdn_dimension_size (&matrix->dimension);

	if (dimsize != 1 && mydimsize == 1)
	{
		matrix->values = g_memdup (values, dimsize * sizeof (gdouble));
	}
	else if (dimsize != 1)
	{
		g_free (matrix->values);
		matrix->values = g_memdup (values, dimsize * sizeof (gdouble));
	}
	else if (dimsize != mydimsize)
	{
		g_free (matrix->values);
		matrix->value = *values;
	}
	else
	{
		memcpy (matrix->values, values, dimsize * sizeof (gdouble));
	}

	matrix->dimension = *dimension;
}

gdouble const *
cdn_matrix_get (CdnMatrix    *matrix,
                CdnDimension *dimension)
{
	if (dimension)
	{
		*dimension = matrix->dimension;
	}

	if (cdn_dimension_is_one (&matrix->dimension))
	{
		return &matrix->value;
	}
	else
	{
		return matrix->values;
	}
}

gdouble const *
cdn_matrix_get_flat (CdnMatrix *matrix,
                     gint      *length)
{
	if (length)
	{
		*length = cdn_dimension_size (&matrix->dimension);
	}

	return cdn_matrix_get (matrix, NULL);
}

