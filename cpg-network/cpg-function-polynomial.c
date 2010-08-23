/*
 * cpg-function-polynomial.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-function-polynomial.h"
#include "cpg-ref-counted-private.h"

/**
 * SECTION:function-polynomial
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

struct _CpgFunctionPolynomialPiece
{
	CpgRefCounted parent;

	gdouble begin;
	gdouble end;

	gdouble *coefficients;
	guint num_coefficients;

	gdouble *normalization;
};

struct _CpgFunctionPolynomialPrivate
{
	GSList *polynomials;

	CpgProperty *t;
	CpgProperty *order;
};

G_DEFINE_TYPE (CpgFunctionPolynomial, cpg_function_polynomial, CPG_TYPE_FUNCTION)

static void
cpg_function_polynomial_piece_free (CpgFunctionPolynomialPiece *piece)
{
	g_free (piece->coefficients);
	g_free (piece->normalization);

	g_slice_free (CpgFunctionPolynomialPiece, piece);
}

GType
cpg_function_polynomial_piece_get_type (void)
{
	static GType type_id = 0;
	
	if (G_UNLIKELY (type_id == 0))
	{
		type_id = g_boxed_type_register_static ("CpgFunctionPolynomialPiece",
		                                        cpg_ref_counted_ref,
		                                        cpg_ref_counted_unref);
	}
	
	return type_id;
}

static void
renormalize_piece (CpgFunctionPolynomialPiece *piece)
{
	guint i;
	gdouble norm = piece->end - piece->begin;
	gdouble nsum = 1;

	norm = norm == 0 ? 1 : 1 / norm;

	g_free (piece->normalization);
	piece->normalization = g_new (gdouble, piece->num_coefficients);

	for (i = 0; i < piece->num_coefficients; ++i)
	{
		piece->normalization[i] = nsum;
		nsum *= norm;
	}
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
	CpgFunctionPolynomialPiece *piece = g_slice_new0 (CpgFunctionPolynomialPiece);

	cpg_ref_counted_init (&(piece->parent), (GDestroyNotify)cpg_function_polynomial_piece_free);

	piece->begin = begin;
	piece->end = end;

	cpg_function_polynomial_piece_set_coefficients (piece, coefficients, num_coefficients);
	return piece;
}

static CpgFunctionPolynomialPiece *
cpg_function_polynomial_piece_copy (CpgFunctionPolynomialPiece *piece)
{
	return cpg_function_polynomial_piece_new (piece->begin,
	                                          piece->end,
	                                          piece->coefficients,
	                                          piece->num_coefficients);
}

static void
cpg_function_polynomial_finalize (GObject *object)
{
	CpgFunctionPolynomial *function = CPG_FUNCTION_POLYNOMIAL (object);

	g_slist_foreach (function->priv->polynomials, (GFunc)cpg_ref_counted_unref, NULL);
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

	if (order >= polynomial->num_coefficients)
	{
		return 0;
	}

	for (i = (gint)polynomial->num_coefficients - 1 - order; i >= 0; --i)
	{
		gint revt = (gint)polynomial->num_coefficients - i - 1;

		ret += order_faculty (revt, order) * polynomial->coefficients[i] * power;
		power *= t;
	}

	return ret * polynomial->normalization[order];
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
		CpgFunctionPolynomialPiece *piece = (CpgFunctionPolynomialPiece *)item->data;

		if (val >= piece->begin && val < piece->end)
		{
			double de = (piece->end - piece->begin);
			double norm = de != 0 ? (val - piece->begin) / de : 0;
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
		target->priv->polynomials = g_slist_prepend (target->priv->polynomials,
		                                             cpg_function_polynomial_piece_copy ((CpgFunctionPolynomialPiece *)item->data));
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
	CpgFunctionArgument *argument = cpg_function_argument_new ("__t", FALSE, 0);
	cpg_function_add_argument (CPG_FUNCTION (self), argument);
	cpg_ref_counted_unref (argument);

	/* Add optional 'order' argument */
	argument = cpg_function_argument_new ("__order", TRUE, 0);
	cpg_function_add_argument (CPG_FUNCTION (self), argument);
	cpg_ref_counted_unref (argument);

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
	return p1->begin < p2->begin ? -1 : (p2->begin < p1->begin ? 1 : 0);
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
	                                                     cpg_ref_counted_ref (piece),
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
		cpg_ref_counted_unref (piece);
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

	g_slist_foreach (function->priv->polynomials, (GFunc)cpg_ref_counted_unref, NULL);
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
 * Returns: A #GSList of #CpgFunctionPolynomialPiece
 *
 **/
GSList *
cpg_function_polynomial_get_pieces (CpgFunctionPolynomial *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), NULL);

	return function->priv->polynomials;
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
	return piece->begin;
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
	piece->begin = begin;
	renormalize_piece (piece);
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
	return piece->end;
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
	piece->end = end;
	renormalize_piece (piece);
}

/**
 * cpg_function_polynomial_piece_get_coefficients:
 * @piece: A #CpgFunctionPolynomialPiece
 * @num: Return value for the number of coefficients
 * 
 * Get the polynomial coefficients. The order of the coefficients is from high
 * to low
 *
 * Returns: the polynomial coefficients
 *
 **/
gdouble	*
cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
                                                guint                      *num)
{
	*num = piece->num_coefficients;
	return piece->coefficients;
}

/**
 * cpg_function_polynomial_piece_set_coefficients:
 * @piece: A #CpgFunctionPolynomialPiece
 * @coefficients: The polynomial coefficients
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
	guint i;

	g_return_if_fail (coefficients != NULL);
	g_return_if_fail (num > 0);

	g_free (piece->coefficients);

	piece->coefficients = g_new (gdouble, num);

	for (i = 0; i < num; ++i)
	{
		piece->coefficients[i] = coefficients[i];
	}

	piece->num_coefficients = num;
	renormalize_piece (piece);
}
