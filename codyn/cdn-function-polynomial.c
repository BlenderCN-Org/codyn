/*
 * cdn-function-polynomial.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-function-polynomial.h"

/**
 * SECTION:cdn-function-polynomial
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

#define CDN_FUNCTION_POLYNOMIAL_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_FUNCTION_POLYNOMIAL, CdnFunctionPolynomialPrivate))

/* signals */
enum
{
	PIECE_ADDED,
	PIECE_REMOVED,
	NUM_SIGNALS
};

struct _CdnFunctionPolynomialPrivate
{
	GSList *polynomials;

	CdnVariable *t;
	CdnVariable *order;
};

G_DEFINE_TYPE (CdnFunctionPolynomial, cdn_function_polynomial, CDN_TYPE_FUNCTION)

static guint signals[NUM_SIGNALS] = {0,};

static gint
compare_polynomials (CdnFunctionPolynomialPiece *p1,
                     CdnFunctionPolynomialPiece *p2)
{
	gdouble b1;
	gdouble b2;

	b1 = cdn_function_polynomial_piece_get_begin (p1);
	b2 = cdn_function_polynomial_piece_get_begin (p2);

	return b1 < b2 ? -1 : (b2 < b1 ? 1 : 0);
}

static void
on_piece_range_changed (CdnFunctionPolynomialPiece *piece,
                        GParamSpec                 *spec,
                        CdnFunctionPolynomial      *function)
{
	function->priv->polynomials =
		g_slist_sort (function->priv->polynomials,
		              (GCompareFunc)compare_polynomials);
}

static void
remove_piece (CdnFunctionPolynomial      *function,
              CdnFunctionPolynomialPiece *piece)
{
	g_signal_handlers_disconnect_by_func (piece,
	                                      on_piece_range_changed,
	                                      function);

	g_signal_emit (function, signals[PIECE_REMOVED], 0, piece);
	g_object_unref (piece);
}

static void
cdn_function_polynomial_finalize (GObject *object)
{
	CdnFunctionPolynomial *function = CDN_FUNCTION_POLYNOMIAL (object);

	cdn_function_polynomial_clear_pieces (function);

	G_OBJECT_CLASS (cdn_function_polynomial_parent_class)->finalize (object);
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
evaluate_polynomial (CdnFunctionPolynomialPiece *polynomial,
                     gdouble                     t,
                     guint                       order)
{
	gdouble ret = 0;
	gdouble power = 1;
	gint i;
	gdouble const *coefs;
	gdouble const *norm;
	guint num_coefs;

	coefs = cdn_function_polynomial_piece_get_coefficients (polynomial,
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

	norm = cdn_function_polynomial_piece_get_normalization (polynomial,
	                                                        NULL);

	return ret * norm[order];
}

static void
cdn_function_polynomial_evaluate_impl (CdnFunction *function,
                                       CdnStack    *stack)
{
	CdnFunctionPolynomial *pol = CDN_FUNCTION_POLYNOMIAL (function);
	GSList *item;

	/* Evaluate the polynomial at 't' */
	gdouble val = cdn_variable_get_value (pol->priv->t);
	gboolean found = FALSE;
	guint num = 0;
	gdouble ret = 0;
	guint order = (guint)cdn_variable_get_value (pol->priv->order);

	for (item = pol->priv->polynomials; item; item = g_slist_next (item))
	{
		CdnFunctionPolynomialPiece *piece = item->data;
		gdouble begin = cdn_function_polynomial_piece_get_begin (piece);
		gdouble end = cdn_function_polynomial_piece_get_end (piece);

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

	cdn_stack_push (stack, num != 0 ? ret / num : 0);
}

static void
cdn_function_polynomial_copy_impl (CdnObject *object,
                                   CdnObject *source)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_function_polynomial_parent_class)->copy != NULL)
	{
		CDN_OBJECT_CLASS (cdn_function_polynomial_parent_class)->copy (object, source);
	}

	/* Copy polynomial definitions */
	GSList *item;
	CdnFunctionPolynomial *source_polynomial = CDN_FUNCTION_POLYNOMIAL (source);
	CdnFunctionPolynomial *target = CDN_FUNCTION_POLYNOMIAL (object);

	for (item = source_polynomial->priv->polynomials; item; item = g_slist_next (item))
	{
		CdnFunctionPolynomialPiece *piece;

		piece = cdn_function_polynomial_piece_copy (item->data);

		target->priv->polynomials =
			g_slist_prepend (target->priv->polynomials,
			                 piece);
	}

	target->priv->polynomials = g_slist_reverse (target->priv->polynomials);
}

static void
cdn_function_polynomial_clear_impl (CdnObject *object)
{
	/* Chain up */
	if (CDN_OBJECT_CLASS (cdn_function_polynomial_parent_class)->clear != NULL)
	{
		CDN_OBJECT_CLASS (cdn_function_polynomial_parent_class)->clear (object);
	}

	cdn_function_polynomial_clear_pieces (CDN_FUNCTION_POLYNOMIAL (object));
}

static void
cdn_function_polynomial_get_dimension_impl (CdnFunction *function,
                                            gint        *numr,
                                            gint        *numc)
{
	*numr = 1;
	*numc = 1;
}

static void
cdn_function_polynomial_class_init (CdnFunctionPolynomialClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnFunctionClass *function_class = CDN_FUNCTION_CLASS (klass);
	CdnObjectClass *cdn_object_class = CDN_OBJECT_CLASS (klass);

	function_class->evaluate = cdn_function_polynomial_evaluate_impl;
	function_class->get_dimension = cdn_function_polynomial_get_dimension_impl;

	cdn_object_class->copy = cdn_function_polynomial_copy_impl;
	cdn_object_class->clear = cdn_function_polynomial_clear_impl;

	object_class->finalize = cdn_function_polynomial_finalize;

	/**
	 * CdnFunctionPolynomial::piece-added:
	 * @object: a #CdnFunctionPolynomial
	 * @piece: a #CdnFunctionPolynomialPiece
	 *
	 * Emitted when a new #CdnFunctionPolynomialPiece is added to the
	 * piecewise polynomial function.
	 *
	 **/
	signals[PIECE_ADDED] =
		g_signal_new ("piece-added",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionPolynomialClass, piece_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE);

	/**
	 * CdnFunctionPolynomial::piece-removed:
	 * @object: a #CdnFunctionPolynomial
	 * @piece: a #CdnFunctionPolynomialPiece
	 *
	 * Emitted when a #CdnFunctionPolynomialPiece is removed from the
	 * piecewise polynomial function.
	 *
	 **/
	signals[PIECE_REMOVED] =
		g_signal_new ("piece-removed",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionPolynomialClass, piece_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_FUNCTION_POLYNOMIAL_PIECE);

	g_type_class_add_private (object_class, sizeof(CdnFunctionPolynomialPrivate));
}

static void
cdn_function_polynomial_init (CdnFunctionPolynomial *self)
{
	self->priv = CDN_FUNCTION_POLYNOMIAL_GET_PRIVATE (self);

	/* Add 't' argument */
	cdn_function_add_argument (CDN_FUNCTION (self),
	                           cdn_function_argument_new ("x", TRUE, NULL));

	self->priv->t = cdn_object_get_variable (CDN_OBJECT (self), "x");
}

/**
 * cdn_function_polynomial_new:
 * @name: The function name
 *
 * Create a new polynomial function. This is a special kind of user function
 * which calculates a piecewise polynomial. The function can be called with one
 * mandatory argument, which is the point at which to evaluate the piecewise
 * polynomial (t: [0, 1]). The second argument is optional and determines the
 * order of derivation of the polynomial (default being 0).
 *
 * Returns: A #CdnFunctionPolynomial
 *
 **/
CdnFunctionPolynomial *
cdn_function_polynomial_new (const gchar *name)
{
	return g_object_new (CDN_TYPE_FUNCTION_POLYNOMIAL, "id", name, NULL);
}

/**
 * cdn_function_polynomial_add:
 * @function: A #CdnFunctionPolynomial
 * @piece: A #CdnFunctionPolynomialPiece
 *
 * Add a polynomial piece.
 *
 * Returns: %TRUE if the piece could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cdn_function_polynomial_add (CdnFunctionPolynomial      *function,
                             CdnFunctionPolynomialPiece *piece)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_POLYNOMIAL (function), FALSE);
	g_return_val_if_fail (piece != NULL, FALSE);

	if (g_slist_find (function->priv->polynomials, piece))
	{
		return FALSE;
	}

	function->priv->polynomials = g_slist_insert_sorted (function->priv->polynomials,
	                                                     g_object_ref_sink (piece),
	                                                     (GCompareFunc)compare_polynomials);

	g_signal_connect (piece,
	                  "notify::begin",
	                  G_CALLBACK (on_piece_range_changed),
	                  function);

	g_signal_emit (function, signals[PIECE_ADDED], 0, piece);
	return TRUE;
}

/**
 * cdn_function_polynomial_remove:
 * @function: A #CdnFunctionPolynomial
 * @piece: A #CdnFunctionPolynomialPiece
 *
 * Remove a polynomial piece.
 *
 * Returns: %TRUE if the piece could be successfully removed, %FALSE otherwise
 *
 **/
gboolean
cdn_function_polynomial_remove (CdnFunctionPolynomial      *function,
                                CdnFunctionPolynomialPiece *piece)
{
	GSList *item;

	g_return_val_if_fail (CDN_IS_FUNCTION_POLYNOMIAL (function), FALSE);
	g_return_val_if_fail (piece != NULL, FALSE);

	item = g_slist_find (function->priv->polynomials, piece);

	if (item)
	{
		function->priv->polynomials = g_slist_delete_link (function->priv->polynomials,
		                                                   item);

		remove_piece (function, piece);
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_function_polynomial_clear_pieces:
 * @function: A #CdnFunctionPolynomial
 *
 * Remove all the polynomial pieces.
 *
 **/
void
cdn_function_polynomial_clear_pieces (CdnFunctionPolynomial *function)
{
	g_return_if_fail (CDN_IS_FUNCTION_POLYNOMIAL (function));

	GSList *item;

	for (item = function->priv->polynomials; item; item = g_slist_next (item))
	{
		remove_piece (function, item->data);
	}

	g_slist_free (function->priv->polynomials);
	function->priv->polynomials = NULL;
}

/**
 * cdn_function_polynomial_get_pieces:
 * @function: A #CdnFunctionPolynomial
 *
 * Get a list of the polynomials which make up the function. This returns
 * the internally used list which should not be modified or freed.
 *
 * Returns: (element-type CdnFunctionPolynomialPiece) (transfer none):
 *          A #GSList of #CdnFunctionPolynomialPiece
 *
 **/
const GSList *
cdn_function_polynomial_get_pieces (CdnFunctionPolynomial *function)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_POLYNOMIAL (function), NULL);

	return function->priv->polynomials;
}

