#include "cpg-function-polynomial-piece.h"
#include <string.h>

/**
 * SECTION:cpg-function-polynomial-piece
 * @short_description: Piecewise polynomial piece
 *
 * This class provides a single piece of a piecewise polynomial function. See
 * #CpgFunctionPolynomial for more information on the support for piecewise
 * polynomial functions.
 *
 */

#define CPG_FUNCTION_POLYNOMIAL_PIECE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE, CpgFunctionPolynomialPiecePrivate))

struct _CpgFunctionPolynomialPiecePrivate
{
	gdouble begin;
	gdouble end;

	gdouble *coefficients;
	guint num_coefficients;

	gdouble *normalization;
};

G_DEFINE_TYPE (CpgFunctionPolynomialPiece, cpg_function_polynomial_piece, G_TYPE_INITIALLY_UNOWNED)

enum
{
	PROP_0,
	PROP_BEGIN,
	PROP_END,
	PROP_COEFFICIENTS,
	PROP_NORMALIZATION
};

static GValueArray *
create_coefficients_value (gdouble *coefficients,
                           guint    num_coefficients)
{
	GValueArray *ret;
	guint i;

	ret = g_value_array_new (num_coefficients);

	for (i = 0; i < num_coefficients; ++i)
	{
		GValue val = {0,};
		g_value_init (&val, G_TYPE_DOUBLE);

		g_value_set_double (&val, coefficients[i]);

		g_value_array_insert (ret, i, &val);
		g_value_unset (&val);
	}

	return ret;
}

static void
renormalize_piece (CpgFunctionPolynomialPiece *piece)
{
	guint i;
	gdouble norm = piece->priv->end - piece->priv->begin;
	gdouble nsum = 1;

	norm = norm == 0 ? 1 : 1 / norm;

	g_free (piece->priv->normalization);
	piece->priv->normalization = g_new (gdouble, piece->priv->num_coefficients);

	for (i = 0; i < piece->priv->num_coefficients; ++i)
	{
		piece->priv->normalization[i] = nsum;
		nsum *= norm;
	}

	g_object_notify (G_OBJECT (piece), "normalization");
}

static void
set_coefficients (CpgFunctionPolynomialPiece *piece,
                  gdouble                    *coefficients,
                  guint                       num)
{
	g_free (piece->priv->coefficients);

	piece->priv->coefficients = g_new (gdouble, num);
	memcpy (piece->priv->coefficients, coefficients, sizeof (gdouble) * num);

	piece->priv->num_coefficients = num;
	renormalize_piece (piece);

	g_object_notify (G_OBJECT (piece), "coefficients");
}

static void
set_coefficients_array (CpgFunctionPolynomialPiece *piece,
                        GValueArray                *array)
{
	guint num_coefficients;
	guint i;
	gdouble *coefficients;

	num_coefficients = array->n_values;
	coefficients = g_new (gdouble, num_coefficients);

	for (i = 0; i < num_coefficients; ++i)
	{
		coefficients[i] = g_value_get_double (&(array->values[i]));
	}

	set_coefficients (piece, coefficients, num_coefficients);
	g_free (coefficients);
}

static void
set_range (CpgFunctionPolynomialPiece *piece,
           gdouble                     begin,
           gdouble                     end)
{
	gboolean begin_changed = piece->priv->begin != begin;
	gboolean end_changed = piece->priv->end != end;

	if (!begin_changed && !end_changed)
	{
		return;
	}

	piece->priv->begin = begin;
	piece->priv->end = end;

	renormalize_piece (piece);

	if (begin_changed)
	{
		g_object_notify (G_OBJECT (piece), "begin");
	}

	if (end_changed)
	{
		g_object_notify (G_OBJECT (piece), "end");
	}
}

static void
cpg_function_polynomial_piece_finalize (GObject *object)
{
	CpgFunctionPolynomialPiece *piece = CPG_FUNCTION_POLYNOMIAL_PIECE (object);

	g_free (piece->priv->coefficients);
	g_free (piece->priv->normalization);

	G_OBJECT_CLASS (cpg_function_polynomial_piece_parent_class)->finalize (object);
}

static void
cpg_function_polynomial_piece_set_property (GObject      *object,
                                            guint         prop_id,
                                            const GValue *value,
                                            GParamSpec   *pspec)
{
	CpgFunctionPolynomialPiece *self = CPG_FUNCTION_POLYNOMIAL_PIECE (object);

	switch (prop_id)
	{
		case PROP_BEGIN:
			set_range (self, g_value_get_double (value), self->priv->end);
		break;
		case PROP_END:
			set_range (self, self->priv->begin, g_value_get_double (value));
		break;
		case PROP_COEFFICIENTS:
			set_coefficients_array (self, g_value_get_boxed (value));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_function_polynomial_piece_get_property (GObject    *object,
                                            guint       prop_id,
                                            GValue     *value,
                                            GParamSpec *pspec)
{
	CpgFunctionPolynomialPiece *self = CPG_FUNCTION_POLYNOMIAL_PIECE (object);

	switch (prop_id)
	{
		case PROP_BEGIN:
			g_value_set_double (value, self->priv->begin);
		break;
		case PROP_END:
			g_value_set_double (value, self->priv->end);
		break;
		case PROP_COEFFICIENTS:
			g_value_take_boxed (value,
			                    create_coefficients_value (self->priv->coefficients,
			                                               self->priv->num_coefficients));
		break;
		case PROP_NORMALIZATION:
			g_value_take_boxed (value,
			                    create_coefficients_value (self->priv->normalization,
			                                               self->priv->num_coefficients));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_function_polynomial_piece_class_init (CpgFunctionPolynomialPieceClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_function_polynomial_piece_finalize;

	object_class->get_property = cpg_function_polynomial_piece_get_property;
	object_class->set_property = cpg_function_polynomial_piece_set_property;


	g_type_class_add_private (object_class, sizeof(CpgFunctionPolynomialPiecePrivate));

	/**
	 * CpgFunctionPolynomialPiece:begin:
	 *
	 * The begin point of the piece
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_BEGIN,
	                                 g_param_spec_double ("begin",
	                                                      "Begin",
	                                                      "Begin",
	                                                      -G_MAXDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgFunctionPolynomialPiece:end:
	 *
	 * The end point of the piece
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_END,
	                                 g_param_spec_double ("end",
	                                                      "End",
	                                                      "End",
	                                                      -G_MAXDOUBLE,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgFunctionPolynomialPiece:coefficients:
	 *
	 * The coefficients of the piece
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_COEFFICIENTS,
	                                 g_param_spec_value_array ("coefficients",
	                                                           "Coefficients",
	                                                           "Coefficients",
	                                                           g_param_spec_double ("coefficient",
	                                                                                "Coefficient",
	                                                                                "Coefficient",
	                                                                                -G_MAXDOUBLE,
	                                                                                G_MAXDOUBLE,
	                                                                                0,
	                                                                                0),
	                                                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgFunctionPolynomialPiece:begin:
	 *
	 * The normalization of the piece
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_NORMALIZATION,
	                                 g_param_spec_value_array ("normalization",
	                                                           "Normalization",
	                                                           "Normalization",
	                                                           g_param_spec_double ("coefficient",
	                                                                                "Coefficient",
	                                                                                "Coefficient",
	                                                                                -G_MAXDOUBLE,
	                                                                                G_MAXDOUBLE,
	                                                                                0,
	                                                                                0),
	                                                           G_PARAM_READABLE));
}

static void
cpg_function_polynomial_piece_init (CpgFunctionPolynomialPiece *self)
{
	self->priv = CPG_FUNCTION_POLYNOMIAL_PIECE_GET_PRIVATE (self);
}

/**
 * cpg_function_polynomial_piece_new:
 * @begin: The polynomial interval begin
 * @end: The polynomial interval end
 * @coefficients: The coefficients
 * @num_coefficients: The number of coefficients provided in @coefficients
 *
 * Create a new polynomial to be used in a piecewise polynomial function. The
 * coefficients are specified from high to low order.
 *
 * Returns: A #CpgFunctionPolynomialPiece
 *
 **/
CpgFunctionPolynomialPiece *
cpg_function_polynomial_piece_new (gdouble  begin,
                                   gdouble  end,
                                   gdouble *coefficients,
                                   guint    num_coefficients)
{
	GValueArray *coefs;
	CpgFunctionPolynomialPiece *ret;

	coefs = create_coefficients_value (coefficients, num_coefficients);

	ret = g_object_new (CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE,
	                    "begin", begin,
	                    "end", end,
	                    "coefficients", coefs,
	                    NULL);

	g_value_array_free (coefs);
	return ret;
}

/**
 * cpg_function_polynomial_piece_copy:
 * @piece: A #CpgFunctionPolynomialPiece
 *
 * Create a copy of a #CpgFunctionPolynomialPiece.
 *
 * Returns: A #CpgFunctionPolynomialPiece
 *
 **/
CpgFunctionPolynomialPiece *
cpg_function_polynomial_piece_copy (CpgFunctionPolynomialPiece *piece)
{
	return cpg_function_polynomial_piece_new (piece->priv->begin,
	                                          piece->priv->end,
	                                          piece->priv->coefficients,
	                                          piece->priv->num_coefficients);
}

/**
 * cpg_function_polynomial_piece_get_begin:
 * @piece: A #CpgFunctionPolynomialPiece
 *
 * Get the interval begin of the polynomial.
 *
 * Returns: the interval begin of the polynomial
 *
 **/
gdouble
cpg_function_polynomial_piece_get_begin (CpgFunctionPolynomialPiece *piece)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL_PIECE (piece), 0);

	return piece->priv->begin;
}

/**
 * cpg_function_polynomial_piece_set_begin:
 * @piece: A #CpgFunctionPolynomialPiece
 * @begin: the interval begin of the polynomial
 *
 * Set the interval begin of the polynomial.
 *
 **/
void
cpg_function_polynomial_piece_set_begin (CpgFunctionPolynomialPiece *piece,
                                         gdouble                     begin)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL_PIECE (piece));

	set_range (piece, begin, piece->priv->end);
}

/**
 * cpg_function_polynomial_piece_get_end:
 * @piece: A #CpgFunctionPolynomialPiece
 *
 * Get the interval end of the polynomial.
 *
 * Returns: the interval end of the polynomial
 *
 **/
gdouble
cpg_function_polynomial_piece_get_end (CpgFunctionPolynomialPiece *piece)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL_PIECE (piece), 0);

	return piece->priv->end;
}

/**
 * cpg_function_polynomial_piece_set_end:
 * @piece: A #CpgFunctionPolynomialPiece
 * @end: the interval end of the polynomial
 *
 * Set the interval end of the polynomial.
 *
 **/
void
cpg_function_polynomial_piece_set_end (CpgFunctionPolynomialPiece *piece,
                                       gdouble                     end)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL_PIECE (piece));

	set_range (piece, piece->priv->begin, end);
}

/**
 * cpg_function_polynomial_piece_get_coefficients:
 * @piece: A #CpgFunctionPolynomialPiece
 * @num: (out caller-allocates): Return value for the number of coefficients
 *
 * Get the polynomial coefficients. The order of the coefficients is from high
 * to low
 *
* Returns: (array type=double length=num): the polynomial coefficients
 *
 **/
gdouble	const *
cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
                                                guint                      *num)
{
	if (num)
	{
		*num = piece->priv->num_coefficients;
	}

	return piece->priv->coefficients;
}

/**
 * cpg_function_polynomial_piece_set_coefficients:
 * @piece: A #CpgFunctionPolynomialPiece
 * @coefficients: (array type=double length=num): The polynomial coefficients
 * @num: The number of coefficients provided in @coefficients
 *
 * Set the coefficients of the polynomial. The order of the coefficients is
 * from high to low.
 *
 **/
void
cpg_function_polynomial_piece_set_coefficients (CpgFunctionPolynomialPiece *piece,
                                                gdouble                    *coefficients,
                                                guint                       num)
{
	g_return_if_fail (coefficients != NULL);
	g_return_if_fail (num > 0);

	set_coefficients (piece, coefficients, num);
}

/**
 * cpg_function_polynomial_piece_get_normalization:
 * @piece: A #CpgFunctionPolynomialPiece
 * @num: (out caller-allocates): Return value for the number of coefficients
 *
 * Get the polynomial normalization. The order of the normalization is from high
 * to low
 *
 * Returns: (array type=double length=num): the polynomial coefficients
 *
 **/
gdouble	const *
cpg_function_polynomial_piece_get_normalization (CpgFunctionPolynomialPiece *piece,
                                                 guint                      *num)
{
	if (num)
	{
		*num = piece->priv->num_coefficients;
	}

	return piece->priv->normalization;
}
