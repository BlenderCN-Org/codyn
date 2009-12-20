#include "cpg-function-polynomial.h"
#include "cpg-ref-counted-private.h"

#define CPG_FUNCTION_POLYNOMIAL_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION_POLYNOMIAL, CpgFunctionPolynomialPrivate))

struct _CpgFunctionPolynomialPiece
{
	CpgRefCounted parent;

	gdouble begin;
	gdouble end;

	gdouble *coefficients;
	guint num_coefficients;
};

struct _CpgFunctionPolynomialPrivate
{
	GSList *polynomials;

	CpgProperty *t;
};

G_DEFINE_TYPE (CpgFunctionPolynomial, cpg_function_polynomial, CPG_TYPE_FUNCTION)

static void
cpg_function_polynomial_piece_free (CpgFunctionPolynomialPiece *piece)
{
	g_free (piece->coefficients);
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

static gdouble
evaluate_polynomial (CpgFunctionPolynomialPiece *polynomial,
                     gdouble     t)
{
	gdouble ret = 0;
	gdouble power = 1;
	gint i;

	for (i = (gint)polynomial->num_coefficients - 1; i >= 0; --i)
	{
		ret += polynomial->coefficients[i] * power;
		power *= t;
	}

	return ret;
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

	for (item = pol->priv->polynomials; item; item = g_slist_next (item))
	{
		CpgFunctionPolynomialPiece *piece = (CpgFunctionPolynomialPiece *)item->data;

		if (val >= piece->begin && val < piece->end)
		{
			double norm = (val - piece->begin) / (piece->end - piece->begin);
			ret += evaluate_polynomial (piece, norm);
			++num;

			found = TRUE;
		}
		else if (found)
		{
			break;
		}
	}

	return ret / num;
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

	/* Add argument */
	cpg_function_add_argument (CPG_FUNCTION (self), "t");

	self->priv->t = cpg_object_get_property (CPG_OBJECT (self), "t");
}

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

void
cpg_function_polynomial_clear (CpgFunctionPolynomial *function)
{
	g_return_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function));

	g_slist_foreach (function->priv->polynomials, (GFunc)cpg_ref_counted_unref, NULL);
	g_slist_free (function->priv->polynomials);

	function->priv->polynomials = NULL;
}

GSList *
cpg_function_polynomial_get_pieces (CpgFunctionPolynomial *function)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_POLYNOMIAL (function), NULL);

	return function->priv->polynomials;
}

gdouble
cpg_function_polynomial_piece_get_begin (CpgFunctionPolynomialPiece *piece)
{
	return piece->begin;
}

void
cpg_function_polynomial_piece_set_begin (CpgFunctionPolynomialPiece *piece,
                                         gdouble                     begin)
{
	piece->begin = begin;
}

gdouble
cpg_function_polynomial_piece_get_end (CpgFunctionPolynomialPiece *piece)
{
	return piece->end;
}

void
cpg_function_polynomial_piece_set_end (CpgFunctionPolynomialPiece *piece,
                                       gdouble                     end)
{
	piece->end = end;
}

gdouble	*
cpg_function_polynomial_piece_get_coefficients (CpgFunctionPolynomialPiece *piece,
                                                guint                      *num)
{
	*num = piece->num_coefficients;
	return piece->coefficients;
}

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
}
