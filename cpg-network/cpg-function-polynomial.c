/*
 * cpg-function-polynomial.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

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

/* signals */
enum
{
	PIECE_ADDED,
	PIECE_REMOVED,
	NUM_SIGNALS
};

struct _CpgFunctionPolynomialPrivate
{
	GSList *polynomials;

	CpgProperty *t;
	CpgProperty *order;
};

G_DEFINE_TYPE (CpgFunctionPolynomial, cpg_function_polynomial, CPG_TYPE_FUNCTION)

static guint signals[NUM_SIGNALS] = {0,};

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

static void
on_piece_range_changed (CpgFunctionPolynomialPiece *piece,
                        GParamSpec                 *spec,
                        CpgFunctionPolynomial      *function)
{
	function->priv->polynomials =
		g_slist_sort (function->priv->polynomials,
		              (GCompareFunc)compare_polynomials);
}

static void
remove_piece (CpgFunctionPolynomial      *function,
              CpgFunctionPolynomialPiece *piece)
{
	g_signal_handlers_disconnect_by_func (piece,
	                                      on_piece_range_changed,
	                                      function);

	g_signal_emit (function, signals[PIECE_REMOVED], 0, piece);
	g_object_unref (piece);
}

static void
cpg_function_polynomial_finalize (GObject *object)
{
	CpgFunctionPolynomial *function = CPG_FUNCTION_POLYNOMIAL (object);

	cpg_function_polynomial_clear_pieces (function);

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
cpg_function_polynomial_clear_impl (CpgObject *object)
{
	/* Chain up */
	if (CPG_OBJECT_CLASS (cpg_function_polynomial_parent_class)->clear != NULL)
	{
		CPG_OBJECT_CLASS (cpg_function_polynomial_parent_class)->clear (object);
	}

	cpg_function_polynomial_clear_pieces (CPG_FUNCTION_POLYNOMIAL (object));
}

static void
cpg_function_polynomial_class_init (CpgFunctionPolynomialClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgFunctionClass *function_class = CPG_FUNCTION_CLASS (klass);
	CpgObjectClass *cpg_object_class = CPG_OBJECT_CLASS (klass);

	function_class->evaluate = cpg_function_polynomial_evaluate_impl;

	cpg_object_class->copy = cpg_function_polynomial_copy_impl;
	cpg_object_class->clear = cpg_function_polynomial_clear_impl;

	object_class->finalize = cpg_function_polynomial_finalize;

	/**
	 * CpgFunctionPolynomial::piece-added:
	 * @object: a #CpgFunctionPolynomial
	 * @piece: a #CpgFunctionPolynomialPiece
	 *
	 * Emitted when a new #CpgFunctionPolynomialPiece is added to the
	 * piecewise polynomial function.
	 *
	 **/
	signals[PIECE_ADDED] =
		g_signal_new ("piece-added",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgFunctionPolynomialClass, piece_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE);

	/**
	 * CpgFunctionPolynomial::piece-removed:
	 * @object: a #CpgFunctionPolynomial
	 * @piece: a #CpgFunctionPolynomialPiece
	 *
	 * Emitted when a #CpgFunctionPolynomialPiece is removed from the
	 * piecewise polynomial function.
	 *
	 **/
	signals[PIECE_REMOVED] =
		g_signal_new ("piece-removed",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgFunctionPolynomialClass, piece_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_FUNCTION_POLYNOMIAL_PIECE);

	g_type_class_add_private (object_class, sizeof(CpgFunctionPolynomialPrivate));
}

static void
cpg_function_polynomial_init (CpgFunctionPolynomial *self)
{
	self->priv = CPG_FUNCTION_POLYNOMIAL_GET_PRIVATE (self);

	/* Add 't' argument */
	cpg_function_add_argument (CPG_FUNCTION (self),
	                           cpg_function_argument_new ("__t", NULL, TRUE));

	/* Add optional 'order' argument */
	cpg_function_add_argument (CPG_FUNCTION (self),
	                           cpg_function_argument_new ("__order", NULL, TRUE));

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
cpg_function_polynomial_new (const gchar *name)
{
	return g_object_new (CPG_TYPE_FUNCTION_POLYNOMIAL, "id", name, NULL);
}

/**
 * cpg_function_polynomial_add:
 * @function: A #CpgFunctionPolynomial
 * @piece: A #CpgFunctionPolynomialPiece
 *
 * Add a polynomial piece.
 *
 * Returns: %TRUE if the piece could be successfully added, %FALSE otherwise
 *
 **/
gboolean
cpg_function_polynomial_add (CpgFunctionPolynomial      *function,
                             CpgFunctionPolynomialPiece *piece)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), FALSE);
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
 * cpg_function_polynomial_remove:
 * @function: A #CpgFunctionPolynomial
 * @piece: A #CpgFunctionPolynomialPiece
 *
 * Remove a polynomial piece.
 *
 * Returns: %TRUE if the piece could be successfully removed, %FALSE otherwise
 *
 **/
gboolean
cpg_function_polynomial_remove (CpgFunctionPolynomial      *function,
                                CpgFunctionPolynomialPiece *piece)
{
	GSList *item;

	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), FALSE);
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
 * cpg_function_polynomial_clear_pieces:
 * @function: A #CpgFunctionPolynomial
 *
 * Remove all the polynomial pieces.
 *
 **/
void
cpg_function_polynomial_clear_pieces (CpgFunctionPolynomial *function)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function));

	GSList *item;

	for (item = function->priv->polynomials; item; item = g_slist_next (item))
	{
		remove_piece (function, item->data);
	}

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
 * Returns: (element-type CpgFunctionPolynomialPiece) (transfer none):
 *          A #GSList of #CpgFunctionPolynomialPiece
 *
 **/
const GSList *
cpg_function_polynomial_get_pieces (CpgFunctionPolynomial *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), NULL);

	return function->priv->polynomials;
}

