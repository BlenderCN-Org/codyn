#include "cpg-function-polynomial.h"

/**
 * SECTION:cpg-function-polynomial
 * @short_description: Custom user defined piecewise polynomial
 *
 * This class provides a specialized custom user function which defines
 * and evaluates piecewise polynomials. The piece polynomials can be
 * specified in terms of the interval in which they are evaluated and
 * the polynomial coefficients. Note that each polynomial will be evaluated
 * on the normalized interval 0 to 1.
 *
 * In addition, you can automatically evaluate the Nth derivative of the
 * polynomial by use of the optional second argument of the function.
 *
 */
 
#define CPG_FUNCTION_POLYNOMIAL_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomialPrivate))

struct _CpgFunctionPolynomialPrivate
{
	GSList *polynomials;

	CpgProperty *t;
	CpgProperty *order;
};

G_DEFINE_TYPE (CpgFunctionPolynomial, cpg_function_polynomial, CPG_TYPE_FUNCTION)


static void
cpg_function_polynomial_finalize (GObject *object)
{
	CpgFunctionPolynomial *function = CPG_FUNCTION_POLYNOMIAL (object);

	g_slist_foreach (function->priv->polynomials, (GFunc)g_object_unref, NULL);
	g_slist_free (function->priv->polynomials);

	G_OBJECT_CLASS (cpg_function_polynomial_parent_class)->finalize (object);
}

static gint
order_faculty (gint n, gint order)
{
	if (order == 0)
	{
		return 1;
	}

	if (n == 0)
	{
		return 0;
	}

	return n * order_faculty (n - 1, order - 1);
}

static gdouble
evaluate_polynomial (CpgFunctionPolynomialPiece *polynomial,
                     gdouble                     t,
                     guint                       order)
{
	gdouble ret = 0;
	gdouble power = 1;
	gint i;
	gdouble const *coefs;
	gdouble const *norm;
	guint num_coefs;

	coefs = cpg_function_polynomial_piece_get_coefficients (polynomial,
	                                                        &num_coefs);

	if (order >= num_coefs)
	{
		return 0;
	}

	for (i = (gint)num_coefs - 1 - order; i >= 0; --i)
	{
		gint revt = (gint)num_coefs - i - 1;

		ret += order_faculty (revt, order) * coefs[i] * power;
		power *= t;
	}

	norm = cpg_function_polynomial_piece_get_normalization (polynomial,
	                                                        NULL);

	return ret * norm[order];
}

static gdouble
cpg_function_polynomial_evaluate_impl (CpgFunction *function)
{
	CpgFunctionPolynomial *pol = CPG_FUNCTION_POLYNOMIAL (function);
	GSList *item;

	/* Evaluate the polynomial at 't' */
	gdouble val = cpg_property_get_value (pol->priv->t);
	gboolean found = FALSE;
	guint num = 0;
	gdouble ret = 0;
	guint order = (guint)cpg_property_get_value (pol->priv->order);

	for (item = pol->priv->polynomials; item; item = g_slist_next (item))
	{
		CpgFunctionPolynomialPiece *piece = item->data;
		gdouble begin = cpg_function_polynomial_piece_get_begin (piece);
		gdouble end = cpg_function_polynomial_piece_get_end (piece);

		if (val >= begin && val < end)
		{
			gdouble de = (end - begin);
			gdouble norm = de != 0 ? (val - begin) / de : 0;

			ret += evaluate_polynomial (piece, norm, order);
			++num;

			found = TRUE;
		}
		else if (found)
		{
			break;
		}
	}

	return num != 0 ? ret / num : 0;
}

static void
cpg_function_polynomial_copy_impl (CpgObject *object,
                                   CpgObject *source)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_polynomial_parent_class)->copy != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_polynomial_parent_class)->copy (object, source);
	}

	/* Copy polynomial definitions */
	GSList *item;
	CpgFunctionPolynomial *source_polynomial = CPG_FUNCTION_POLYNOMIAL (source);
	CpgFunctionPolynomial *target = CPG_FUNCTION_POLYNOMIAL (object);

	for (item = source_polynomial->priv->polynomials; item; item = g_slist_next (item))
	{
		CpgFunctionPolynomialPiece *piece;

		piece = cpg_function_polynomial_piece_copy (item->data);

		target->priv->polynomials =
			g_slist_prepend (target->priv->polynomials,
			                 piece);
	}

	target->priv->polynomials = g_slist_reverse (target->priv->polynomials);
}

static void
cpg_function_polynomial_class_init (CpgFunctionPolynomialClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgFunctionClass *function_class = CPG_FUNCTION_CLASS (klass);
	CpgObjectClass *cpg_object_class = CPG_OBJECT_CLASS (klass);

	function_class->evaluate = cpg_function_polynomial_evaluate_impl;
	cpg_object_class->copy = cpg_function_polynomial_copy_impl;

	object_class->finalize = cpg_function_polynomial_finalize;

	g_type_class_add_private (object_class, sizeof(CpgFunctionPolynomialPrivate));
}

static void
cpg_function_polynomial_init (CpgFunctionPolynomial *self)
{
	self->priv = CPG_FUNCTION_POLYNOMIAL_GET_PRIVATE (self);

	/* Add 't' argument */
	cpg_function_add_argument (CPG_FUNCTION (self),
	                            cpg_function_argument_new ("__t", FALSE, 0));

	/* Add optional 'order' argument */
	cpg_function_add_argument (CPG_FUNCTION (self),
	                           cpg_function_argument_new ("__order", TRUE, 0));

	self->priv->t = cpg_object_get_property (CPG_OBJECT (self), "__t");
	self->priv->order = cpg_object_get_property (CPG_OBJECT (self), "__order");
}

/**
 * cpg_function_polynomial_new:
 * @name: The function name
 * 
 * Create a new polynomial function. This is a special kind of user function
 * which calculates a piecewise polynomial. The function can be called with one
 * mandatory argument, which is the point at which to evaluate the piecewise
 * polynomial (t: [0, 1]). The second argument is optional and determines the
 * order of derivation of the polynomial (default being 0).
 *
 * Returns: A #CpgFunctionPolynomial
 *
 **/
CpgFunctionPolynomial *
cpg_function_polynomial_new (gchar const *name)
{
	return g_object_new (CPG_TYPE_FUNCTION_POLYNOMIAL, "id", name, NULL);
}

static gint
compare_polynomials (CpgFunctionPolynomialPiece *p1,
                     CpgFunctionPolynomialPiece *p2)
{
	gdouble b1;
	gdouble b2;

	b1 = cpg_function_polynomial_piece_get_begin (p1);
	b2 = cpg_function_polynomial_piece_get_begin (p2);

	return b1 < b2 ? -1 : (b2 < b1 ? 1 : 0);
}

/**
 * cpg_function_polynomial_add:
 * @function: A #CpgFunctionPolynomial
 * @piece: A #CpgFunctionPolynomialPiece
 * 
 * Add a polynomial piece.
 *
 **/
void
cpg_function_polynomial_add (CpgFunctionPolynomial      *function,
                             CpgFunctionPolynomialPiece *piece)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function));
	g_return_if_fail (piece != NULL);

	function->priv->polynomials = g_slist_insert_sorted (function->priv->polynomials,
	                                                     g_object_ref_sink (piece),
	                                                     (GCompareFunc)compare_polynomials);
}

/**
 * cpg_function_polynomial_remove:
 * @function: A #CpgFunctionPolynomial
 * @piece: A #CpgFunctionPolynomialPiece
 * 
 * Remove a polynomial piece.
 *
 **/
void
cpg_function_polynomial_remove (CpgFunctionPolynomial      *function,
                                CpgFunctionPolynomialPiece *piece)
{
	GSList *item;

	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function));
	g_return_if_fail (piece != NULL);

	item = g_slist_find (function->priv->polynomials, piece);

	if (item)
	{
		function->priv->polynomials = g_slist_delete_link (function->priv->polynomials,
		                                                   item);
		g_object_unref (piece);
	}
}

/**
 * cpg_function_polynomial_clear:
 * @function: A #CpgFunctionPolynomial
 * 
 * Remove all the polynomial pieces.
 *
 **/
void
cpg_function_polynomial_clear (CpgFunctionPolynomial *function)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function));

	g_slist_foreach (function->priv->polynomials, (GFunc)g_object_unref, NULL);
	g_slist_free (function->priv->polynomials);

	function->priv->polynomials = NULL;
}

/**
 * cpg_function_polynomial_get_pieces:
 * @function: A #CpgFunctionPolynomial
 * 
 * Get a list of the polynomials which make up the function. This returns 
 * the internally used list which should not be modified or freed.
 *
 * Returns: (element-type CpgFunctionPolynomialPiece):
 *          A #GSList of #CpgFunctionPolynomialPiece
 *
 **/
GSList const *
cpg_function_polynomial_get_pieces (CpgFunctionPolynomial *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), NULL);

	return function->priv->polynomials;
}

