#include <codyn/cdn-expression-tree-iter.h>
#include <codyn/cdn-math.h>
#include <codyn/instructions/cdn-instructions.h>
#include <codyn/cdn-expression-tree-iter.h>
#include <codyn/cdn-operators.h>
#include <codyn/cdn-edge-action.h>
#include <codyn/cdn-debug.h>
#include <codyn/cdn-compile-error.h>

#include "cdn-tree-algorithms-private.h"

#include <math.h>
#include <glib/gprintf.h>

typedef struct
{
	CdnExpressionTreeIterDeriveFlags flags;
	GError **error;

	GHashTable *symbols;
	GHashTable *towards;
} DeriveContext;

static CdnExpressionTreeIter *derive_iter (CdnExpressionTreeIter *iter,
                                           DeriveContext         *ctx);

/**
 * cdn_expression_tree_iter_derive_error_quark:
 *
 * Get the derive error quark.
 *
 * Returns: a #GQuark for the derive error type.
 *
 */
GQuark
cdn_expression_tree_iter_derive_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("CdnExpressionTreeIterDeriveError");
	}

	return quark;
}

typedef CdnExpressionTreeIter *(*OptimizedOperatorFunc) (CdnExpressionTreeIter *a,
                                                         CdnExpressionTreeIter *b);

static gboolean
cmp_double (gdouble a,
            gdouble b)
{
	return fabs (a - b) <= 10e-13;
}

static void
free_iter (CdnExpressionTreeIter *iter,
           gboolean               reallyfree)
{
	if (reallyfree)
	{
		cdn_expression_tree_iter_free (iter);
	}
}

static CdnExpressionTreeIter *
copy_iter (CdnExpressionTreeIter *iter,
           gboolean               reallyfree)
{
	if (reallyfree)
	{
		return iter;
	}

	return iter_copy (iter);
}

static CdnExpressionTreeIter *
multiply_optimized (CdnExpressionTreeIter *a,
                    CdnExpressionTreeIter *b,
                    gboolean               take_a,
                    gboolean               take_b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = iter_is_number (a, &numa);
	isnumb = iter_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_num (numa * numb);
	}
	else if (isnuma || isnumb)
	{
		if (cmp_double (isnuma ? numa : numb, 0))
		{
			free_iter (a, take_a);
			free_iter (b, take_b);

			// 0 multiplied by a or b is just zero
			return iter_new_numstr ("0");
		}
		else if (cmp_double (isnuma ? numa : numb, 1))
		{
			if (isnuma)
			{
				free_iter (a, take_a);
				return copy_iter (b, take_b);
			}
			else
			{
				free_iter (b, take_b);
				return copy_iter (a, take_a);
			}
		}
	}

	return iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                       a,
	                       b,
	                       take_a,
	                       take_b);
}

static CdnExpressionTreeIter *
divide_optimized (CdnExpressionTreeIter *a,
                  CdnExpressionTreeIter *b,
                  gboolean               take_a,
                  gboolean               take_b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = iter_is_number (a, &numa);
	isnumb = iter_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_num (numa / numb);
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_numstr ("0");
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		free_iter (b, take_b);
		return copy_iter (a, take_a);
	}

	return iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_DIVIDE,
	                       a,
	                       b,
	                       take_a,
	                       take_b);
}

static CdnExpressionTreeIter *
add_optimized (CdnExpressionTreeIter *a,
               CdnExpressionTreeIter *b,
               gboolean               take_a,
               gboolean               take_b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = iter_is_number (a, &numa);
	isnumb = iter_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_num (numa + numb);
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		free_iter (a, take_a);

		return copy_iter (b, take_b);
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		free_iter (b, take_b);
		return copy_iter (a, take_a);
	}

	return iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_PLUS,
	                       a,
	                       b,
	                       take_a,
	                       take_b);
}

static CdnExpressionTreeIter *
subtract_optimized (CdnExpressionTreeIter *a,
                    CdnExpressionTreeIter *b,
                    gboolean               take_a,
                    gboolean               take_b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = iter_is_number (a, &numa);
	isnumb = iter_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_num (numa - numb);
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		free_iter (a, take_a);

		return iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
		                       b,
		                       take_b);
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		free_iter (b, take_b);

		return copy_iter (a, take_a);
	}

	return iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_MINUS,
	                       a,
	                       b,
	                       take_a,
	                       take_b);
}

static CdnExpressionTreeIter *
power_optimized (CdnExpressionTreeIter *a,
                 CdnExpressionTreeIter *b,
                 gboolean               take_a,
                 gboolean               take_b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = iter_is_number (a, &numa);
	isnumb = iter_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_num (pow (numa, numb));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_numstr ("0");
	}
	else if (isnuma && cmp_double (numa, 1))
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_numstr ("1");
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		free_iter (a, take_a);
		free_iter (b, take_b);

		return iter_new_numstr ("1");
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		free_iter (b, take_b);
		return copy_iter (a, take_a);
	}

	return iter_new_bfunc (CDN_MATH_FUNCTION_TYPE_POWER,
	                       a,
	                       b,
	                       take_a,
	                       take_b);
}

static CdnExpressionTreeIter *
derive_power_real (CdnExpressionTreeIter *fi,
                   CdnExpressionTreeIter *gi,
                   DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fd;
	CdnExpressionTreeIter *gd;
	gint numg;

	// Power rule: (f^g)' = f^g * (f' * (g / f) + g' ln (f))
	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		return NULL;
	}

	if (iter_is_natural_number (gi, &numg))
	{
		return multiply_optimized (multiply_optimized (gi,
		                                               power_optimized (fi,
		                                                                iter_new_num (numg - 1),
		                                                                FALSE,
		                                                                TRUE),
		                                               FALSE,
		                                               TRUE),
		                            fd,
		                            TRUE,
		                            TRUE);
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		cdn_expression_tree_iter_free (fd);
		return NULL;
	}

	return multiply_optimized (power_optimized (fi,
	                                            gi,
	                                            FALSE,
	                                            FALSE),
	                           add_optimized (multiply_optimized (fd,
	                                                              divide_optimized (gi,
	                                                                                fi,
	                                                                                FALSE,
	                                                                                FALSE),
	                                                              TRUE,
	                                                              TRUE),
	                                          multiply_optimized (gd,
	                                                              iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_LN,
	                                                                              fi,
	                                                                              FALSE),
	                                                              TRUE,
	                                                              TRUE),
	                                          TRUE,
	                                          TRUE),
	                            TRUE,
	                            TRUE);
}

static CdnExpressionTreeIter *
derive_power (CdnExpressionTreeIter *iter,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	return derive_power_real (fi, gi, ctx);
}

static CdnExpressionTreeIter *
derive_division (CdnExpressionTreeIter *iter,
                 DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;
	CdnExpressionTreeIter *fd;
	CdnExpressionTreeIter *gd;

	// (f / g)' = (f'g - fg') / g^2

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		return NULL;
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		cdn_expression_tree_iter_free (fd);
		return NULL;
	}

	return divide_optimized (subtract_optimized (multiply_optimized (fd,
	                                                                 gi,
	                                                                 TRUE,
	                                                                 FALSE),
	                                             multiply_optimized (fi,
	                                                                 gd,
	                                                                 FALSE,
	                                                                 TRUE),
	                                             TRUE,
	                                             TRUE),
	                         power_optimized (gi,
	                                          iter_new_numstr ("2"),
	                                          FALSE,
	                                          TRUE),
	                         TRUE,
	                         TRUE);
}

static CdnExpressionTreeIter *
derive_product_real (CdnExpressionTreeIter *fi,
                     CdnExpressionTreeIter *gi,
                     DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fd;
	CdnExpressionTreeIter *gd;

	// (fg)' = f'g + fg'

	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		return NULL;
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		cdn_expression_tree_iter_free (fd);
		return NULL;
	}

	return add_optimized (multiply_optimized (fd, gi, TRUE, FALSE),
	                      multiply_optimized (fi, gd, FALSE, TRUE),
	                      TRUE,
	                      TRUE);
}

static CdnExpressionTreeIter *
derive_product (CdnExpressionTreeIter *iter,
                DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	return derive_product_real (fi, gi, ctx);
}

static CdnExpressionTreeIter *
derive_unary_minus (CdnExpressionTreeIter *iter,
                    DeriveContext         *ctx)
{
	CdnExpressionTreeIter *a;
	gdouble num;

	a = derive_iter (cdn_expression_tree_iter_get_child (iter, 0),
	                 ctx);

	if (!a)
	{
		return NULL;
	}

	if (iter_is_number (a, &num))
	{
		return iter_new_num (-num);
	}
	else
	{
		return iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
		                       a,
		                       TRUE);
	}
}

static CdnExpressionTreeIter *
derive_operator (CdnExpressionTreeIter  *iter,
                 CdnInstructionFunction *instr,
                 DeriveContext          *ctx)
{
	CdnExpressionTreeIter *ret = NULL;
	CdnMathFunctionType id;

	id = cdn_instruction_function_get_id (instr);

	switch (id)
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
			ret = derive_unary_minus (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_MINUS:
		case CDN_MATH_FUNCTION_TYPE_PLUS:
		{
			CdnExpressionTreeIter *a = NULL;
			CdnExpressionTreeIter *b = NULL;

			// Linear rule: (f - g)' = f' - g'
			//         and: (f + g)' = f' + g'

			a = derive_iter (cdn_expression_tree_iter_get_child (iter, 0),
			                 ctx);

			if (a)
			{
				b = derive_iter (cdn_expression_tree_iter_get_child (iter, 1),
				                 ctx);

				if (!b)
				{
					cdn_expression_tree_iter_free (a);
					a = NULL;
				}
			}

			if (a && b)
			{
				if (id == CDN_MATH_FUNCTION_TYPE_PLUS)
				{
					ret = add_optimized (a, b, TRUE, TRUE);
				}
				else {
					ret = subtract_optimized (a, b, TRUE, TRUE);
				}
			}
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_MULTIPLY:
			// Product rule: (fg)' = f'g + fg'
			ret = derive_product (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_DIVIDE:
			// Quotient rule: (f/g)' = (f'g - fg') / g^2
			ret = derive_division (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_POWER:
			// Power rule: (f^g)' = f^g * (f' (g / f) + g' ln (f))
			ret = derive_power (iter, ctx);
		break;
		default:
			g_set_error (ctx->error,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
			             "Derivation of operator `%s' is not supported",
			             cdn_instruction_function_get_name (instr));
		break;
	}

	return ret;
}

static CdnExpressionTreeIter *
derive_cos (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// Chain rule: -sin(x) * x'
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	                                           iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_SIN,
	                                                           g,
	                                                           FALSE),
	                                           TRUE),
	                           gd,
	                           TRUE,
	                           TRUE);
}

static CdnExpressionTreeIter *
derive_sin (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// Chain rule: cos(x) * x'
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_COS,
	                                           g,
	                                           FALSE),
	                           gd,
	                           TRUE,
	                           TRUE);
}

static CdnExpressionTreeIter *
derive_tan (CdnExpressionTreeIter *f,
            DeriveContext      *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// Chain rule: (1 + tan^2(x)) * x'
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (add_optimized (iter_new_numstr ("1"),
	                                          power_optimized (iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_TAN,
	                                                                           g,
	                                                                           FALSE),
	                                                           iter_new_numstr ("2"),
	                                                           TRUE,
	                                                           TRUE),
	                                           TRUE,
	                                           TRUE),
	                            gd,
	                            TRUE,
	                            TRUE);
}

static CdnExpressionTreeIter *
derive_ln (CdnExpressionTreeIter *f,
           DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// Chain rule: x' / x
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return divide_optimized (gd, g, TRUE, FALSE);
}

static CdnExpressionTreeIter *
derive_atan (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// x' / (1 + x^2)
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return divide_optimized (gd,
	                         add_optimized (iter_new_numstr ("1"),
	                                        power_optimized (g,
	                                                         iter_new_numstr ("2"),
	                                                         FALSE,
	                                                         TRUE),
	                                         TRUE,
	                                         TRUE),
	                         TRUE,
	                         TRUE);
}

static CdnExpressionTreeIter *
derive_acos (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// x' / -sqrt(1 - x^2)
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return divide_optimized (gd,
	                         iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	                                         iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_SQRT,
	                                                         subtract_optimized (iter_new_numstr ("1"),
	                                                                             power_optimized (g,
	                                                                                              iter_new_numstr ("2"),
	                                                                                              FALSE,
	                                                                                              TRUE),
	                                                                             TRUE,
	                                                                             TRUE),
	                                                         TRUE),
	                                         TRUE),
	                         TRUE,
	                         TRUE);
}

static CdnExpressionTreeIter *
derive_asin (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// x' / sqrt(1 - x^2)
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return divide_optimized (gd,
	                         iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_SQRT,
	                                         subtract_optimized (iter_new_numstr ("1"),
	                                                             power_optimized (g,
	                                                                              iter_new_numstr ("2"),
	                                                                              FALSE,
	                                                                              TRUE),
	                                                             TRUE,
	                                                             TRUE),
	                                         TRUE),
	                         TRUE,
	                         TRUE);
}

static CdnExpressionTreeIter *
derive_sqrt_inside (CdnExpressionTreeIter *g,
                    DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;

	// 0.5 * x^(-0.5) * x'
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (multiply_optimized (iter_new_numstr ("0.5"),
	                                               power_optimized (g,
	                                                                iter_new_numstr ("-0.5"),
	                                                                FALSE,
	                                                                TRUE),
	                                               TRUE,
	                                               TRUE),
	                           gd,
	                           TRUE,
	                           TRUE);
}

static CdnExpressionTreeIter *
derive_sqrt (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	return derive_sqrt_inside (cdn_expression_tree_iter_get_child (f, 0),
	                           ctx);
}

static CdnExpressionTreeIter *
derive_exp (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (f, gd, FALSE, TRUE);
}

static CdnExpressionTreeIter *
derive_exp2 (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	CdnExpressionTreeIter *gd;
	CdnExpressionTreeIter *g;

	// Power rule: (2^g)' = 2^g * (g' * ln (2))
	g = cdn_expression_tree_iter_get_child (f, 0);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return multiply_optimized (power_optimized (iter_new_numstr ("2"),
	                                            g,
	                                            TRUE,
	                                            FALSE),
	                           multiply_optimized (gd,
	                                               iter_new_num (M_LN2),
	                                               TRUE,
	                                               TRUE),
	                           TRUE,
	                           TRUE);
}

static CdnExpressionTreeIter *
derive_sqsum (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *ret = NULL;
	gint i;
	CdnExpressionTreeIter *num2;

	num2 = iter_new_numstr ("2");

	for (i = 0; i < cdn_expression_tree_iter_get_num_children (f); ++i)
	{
		CdnExpressionTreeIter *iter;
		CdnExpressionTreeIter *pow;

		iter = cdn_expression_tree_iter_get_child (f, i);
		pow = derive_power_real (iter, num2, ctx);

		if (!pow)
		{
			if (ret)
			{
				cdn_expression_tree_iter_free (ret);
			}

			cdn_expression_tree_iter_free (num2);
			return NULL;
		}

		if (ret != NULL)
		{
			ret = add_optimized (ret,
			                     iter,
			                     TRUE,
			                     TRUE);
		}
		else
		{
			ret = iter;
		}
	}

	cdn_expression_tree_iter_free (num2);
	return ret;
}

static CdnExpressionTreeIter *
derive_hypot (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *sq;
	CdnExpressionTreeIter *ret;

	sq = derive_sqsum (f, ctx);

	if (!sq)
	{
		return NULL;
	}

	// sqrt of that
	ret = derive_sqrt_inside (sq, ctx);
	cdn_expression_tree_iter_free (sq);

	return ret;
}

static CdnExpressionTreeIter *
derive_log10 (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *g;
	CdnExpressionTreeIter *gd;

	g = cdn_expression_tree_iter_get_child (f, 0);

	// log10(x)' = log10(e) / x * x'
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		return NULL;
	}

	return divide_optimized (iter_new_num (M_LOG10E),
	                         multiply_optimized (g,
	                                             gd,
	                                             FALSE,
	                                             TRUE),
	                         TRUE,
	                         TRUE);
}

static CdnExpressionTreeIter *
derive_index (CdnExpressionTreeIter *iter,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *cp;
	CdnExpressionTreeIter *derived;
	gint idx = iter->num_children - 1;

	derived = derive_iter (iter->children[idx], ctx);

	if (!derived)
	{
		return NULL;
	}

	cp = iter_copy (iter);

	cdn_expression_tree_iter_free (cp->children[idx]);
	cp->children[idx] = derived;
	derived->parent = cp;

	return cp;
}

static CdnExpressionTreeIter *
derive_sum (CdnExpressionTreeIter *iter,
            DeriveContext         *context)
{
	CdnExpressionTreeIter *ret = NULL;
	guint i;

	ret = iter_new_sized (cdn_expression_tree_iter_get_instruction (iter),
	                      cdn_expression_tree_iter_get_num_children (iter));

	// Simply derive all the children
	for (i = 0; i < cdn_expression_tree_iter_get_num_children (iter); ++i)
	{
		CdnExpressionTreeIter *child;

		child = cdn_expression_tree_iter_get_child (iter, i);
		child = derive_iter (child, context);

		if (!child)
		{
			cdn_expression_tree_iter_free (ret);
			return NULL;
		}

		ret->children[i] = child;
	}

	return ret;
}

static CdnExpressionTreeIter *
derive_function (CdnExpressionTreeIter  *iter,
                 CdnInstructionFunction *instr,
                 DeriveContext          *ctx)
{
	switch (cdn_instruction_function_get_id (instr))
	{
		case CDN_MATH_FUNCTION_TYPE_COS:
			// -sin(x) * x'
			return derive_cos (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_SIN:
			// cos(x) * x'
			return derive_sin (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_TAN:
			// (1 + tan^2(x)) * x'
			return derive_tan (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_ATAN:
			// x' / (1 + x^2)
			return derive_atan (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_ACOS:
			// x' / -sqrt(1 - x^2)
			return derive_acos (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_ASIN:
			// x' / sqrt(1 - x^2)
			return derive_asin (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_LN:
			// (1 / x) * x'
			return derive_ln (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_SQRT:
			// 0.5 * x^(-0.5) * x'
			return derive_sqrt (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_POW:
			return derive_power (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_SQSUM:
			return derive_sqsum (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_HYPOT:
			return derive_hypot (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_EXP2:
			return derive_exp2 (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_EXP:
			return derive_exp (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_LOG10:
			return derive_log10 (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_INDEX:
			return derive_index (iter, ctx);
		break;
		case CDN_MATH_FUNCTION_TYPE_SUM:
			return derive_sum (iter, ctx);
		break;
	}

	g_set_error (ctx->error,
	             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
	             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
	             "Derivation of function `%s' is not supported",
	             cdn_instruction_function_get_name (instr));

	return NULL;
}

static CdnExpressionTreeIter *
derive_expression (CdnExpression *expression,
                   DeriveContext *ctx)
{
	CdnExpressionTreeIter *ret;
	CdnExpressionTreeIter *iter;

	iter = cdn_expression_tree_iter_new (expression);

	if (!iter)
	{
		// This means that the expression was not actually compiled yet
		// For now we are just going to assume that we are deriving that
		// to 0
		return iter_new_numstr ("0");
	}

	ret = derive_iter (iter, ctx);
	cdn_expression_tree_iter_free (iter);

	return ret;
}

static CdnExpressionTreeIter *
derive_integrated (CdnVariable   *prop,
                   DeriveContext *ctx)
{
	GSList *actors;
	CdnExpressionTreeIter *ret = NULL;
	GSList *item;
	CdnCompileError *err;
	gboolean isret = TRUE;

	err = cdn_compile_error_new ();
	actors = cdn_variable_get_actions (prop);

	for (item = actors; item; item = g_slist_next (item))
	{
		CdnExpression *e;
		CdnExpressionTreeIter *iter;

		if (!cdn_edge_action_compile (item->data, NULL, err))
		{
			if (ctx->error)
			{
				*ctx->error = g_error_copy (cdn_compile_error_get_error (err));
			}

			isret = FALSE;
			break;
		}

		e = cdn_edge_action_get_equation (item->data);
		iter = cdn_expression_tree_iter_new (e);

		if (ret != NULL)
		{
			ret = add_optimized (ret, iter, TRUE, TRUE);
		}
		else
		{
			ret = iter;
		}
	}

	g_object_unref (err);
	g_slist_free (actors);

	if (!isret)
	{
		cdn_expression_tree_iter_free (ret);
		return NULL;
	}

	return ret;
}

static CdnExpressionTreeIter *
derive_variable_real (CdnInstructionVariable *instr,
                      CdnVariable            *variable,
                      DeriveContext          *ctx)
{
	CdnExpressionTreeIter *derivative = NULL;

	if (!variable && instr)
	{
		variable = cdn_instruction_variable_get_variable (instr);
	}

	if (g_hash_table_lookup_extended (ctx->towards,
	                                  variable,
	                                  NULL,
	                                  (gpointer *)&derivative))
	{
		// We are deriving towards this variable
		// 1) Partial: the partial derivative of the variable being
		//             derived towards is 1
		// 2) Full: the derivative of the variable being derived towards
		//          is simply the variable representing its derivative
		if (ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL)
		{
			// Partial derivative towards this variable is 1
			return iter_new_numstr ("1");
		}
		else
		{
			return iter_copy (derivative);
		}
	}

	// Otherwise, if it's still a symbol, then its derivative is 0
	if (g_hash_table_lookup_extended (ctx->symbols, variable, NULL, NULL))
	{
		return iter_new_numstr ("0");
	}

	if (ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_TIME)
	{
		// Check if there is a time derivative for this variable...
		CdnVariable *dv;

		dv = cdn_variable_get_derivative (variable);

		if (dv)
		{
			return iter_new_take (cdn_instruction_variable_new (dv));
		}
		else if (cdn_variable_has_flag (variable, CDN_VARIABLE_FLAG_INTEGRATED))
		{
			return derive_integrated (variable, ctx);
		}
	}

	// Otherwise expand on the variable
	return derive_expression (cdn_variable_get_expression (variable),
	                          ctx);
}

static CdnExpressionTreeIter *
derive_variable (CdnExpressionTreeIter  *iter,
                 CdnInstructionVariable *instr,
                 DeriveContext          *ctx)
{
	return derive_variable_real (instr, NULL, ctx);
}

static gboolean
iter_contains_variable (CdnExpressionTreeIter *iter,
                        GHashTable            *symbols)
{
	gint i;

	if (CDN_IS_INSTRUCTION_VARIABLE (iter->instruction))
	{
		CdnInstructionVariable *vinstr;
		CdnVariable *v;

		vinstr = CDN_INSTRUCTION_VARIABLE (iter->instruction);
		v = cdn_instruction_variable_get_variable (vinstr);

		if (g_hash_table_lookup_extended (symbols, v, NULL, NULL))
		{
			return TRUE;
		}
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		if (iter_contains_variable (iter->children[i], symbols))
		{
			return TRUE;
		}
	}

	return FALSE;
}

static GSList *
collect_towards (CdnExpressionTreeIter  *iter,
                 CdnFunction            *func,
                 gboolean              **towardsmap,
                 gint                   *newgen,
                 DeriveContext          *ctx)
{
	GSList *ret = NULL;
	gint i;
	GList const *args;

	args = cdn_function_get_arguments (func);

	if (towardsmap)
	{
		*towardsmap = g_new0 (gboolean, iter->num_children);
	}

	if (newgen)
	{
		*newgen = 0;
	}

	for (i = 0; i < iter->num_children; ++i)
	{
		CdnFunctionArgument *arg;
		CdnVariable *v;

		arg = args->data;
		args = g_list_next (args);

		if ((ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_TIME) == 0 &&
		    !iter_contains_variable (iter->children[i], ctx->towards))
		{
			continue;
		}

		if (towardsmap)
		{
			(*towardsmap)[i] = TRUE;
		}

		v = cdn_function_argument_get_variable (arg);

		if (newgen && !cdn_variable_get_derivative (v))
		{
			++*newgen;
		}

		// Need to derive towards this
		ret = g_slist_prepend (ret, arg);
	}

	return g_slist_reverse (ret);
}

static gboolean
find_t_variable (CdnVariable            *variable,
                 CdnExpressionTreeIter  *replacement,
                 CdnVariable           **ret)
{
	CdnObject *p;

	p = cdn_variable_get_object (variable);

	if (CDN_IS_INTEGRATOR (p) && cdn_object_get_variable (p, "t") == variable)
	{
		*ret = variable;
		return TRUE;
	}

	return FALSE;
}

static CdnVariable *
get_t_variable (DeriveContext *ctx)
{
	CdnVariable *ret = NULL;
	g_hash_table_find (ctx->towards, (GHRFunc)find_t_variable, &ret);

	return ret;
}

static CdnExpressionTreeIter *
derive_custom_function_real (CdnExpressionTreeIter *iter,
                             CdnFunction           *func,
                             DeriveContext         *ctx,
                             gboolean               isref)
{
	gint i;
	gint num;
	CdnFunction *df;
	CdnExpressionTreeIter *ret = NULL;
	CdnStackArgs args;
	CdnExpressionTreeIter **children;
	GSList *towards;
	CdnExpressionTreeIterDeriveFlags flags;
	gint newidx;
	gboolean *towardsmap;
	gint newgen;
	gint idx;
	gint newstart;
	CdnFunction *nf;
	CdnFunctionArgument *ft = NULL;

	// Construct list of variables towards which to derive. We are going
	// to be relatively smart here and only derive towards those arguments
	// containing currently derived symbols
	towards = collect_towards (iter, func, &towardsmap, &newgen, ctx);

	if ((ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_TIME) != 0)
	{
		CdnDimension dim = CDN_DIMENSION(1, 1);
		CdnVariable *t;

		t = get_t_variable (ctx);

		if (t != NULL)
		{
			ft = cdn_function_argument_new ("t", TRUE, NULL);
			_cdn_function_argument_set_variable (ft, t);
			cdn_function_argument_set_dimension (ft, &dim);

			towards = g_slist_append (towards, ft);
			newgen++;
		}
	}

	if (!towards)
	{
		CdnDimension dim;

		cdn_expression_get_dimension (cdn_function_get_expression (func),
		                              &dim);

		g_free (towardsmap);
		return iter_new_zeros (&dim);
	}

	flags = ctx->flags;

	if (!isref)
	{
		flags &= ~CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL;
	}

	// Try to derive func towards all of its arguments.
	df = cdn_function_get_derivative (func,
	                                  towards,
	                                  1,
	                                  flags,
	                                  ctx->error);

	if (!df)
	{
		gchar *id;

		if (ft)
		{
			g_object_unref (ft);
		}

		g_slist_free (towards);
		g_free (towardsmap);

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (func));

		if (ctx->error && !*ctx->error)
		{
			g_set_error (ctx->error,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_FUNCTION,
			             "The derivative of the function `%s' (type `%s') is not implemented",
			             id,
			             g_type_name (G_TYPE_FROM_INSTANCE (func)));
		}

		g_free (id);
		return NULL;
	}

	num = cdn_expression_tree_iter_get_num_children (iter);
	cdn_stack_args_init (&args, num + newgen);

	children = g_new0 (CdnExpressionTreeIter *, num + newgen);

	idx = (num + newgen) - 1;
	newidx = newgen - 1;
	newstart = num;

	// Now, df will have new arguments for the derivatives which
	// we will compute here now
	for (i = 0; i < num; ++i)
	{
		CdnExpressionTreeIter *child;
		CdnExpressionTreeIter *derived;
		CdnInstruction *instr;
		CdnStackManipulation const *smanip;

		// First simply copy the child
		child = cdn_expression_tree_iter_get_child (iter, i);
		instr = cdn_expression_tree_iter_get_instruction (child);

		children[i] = iter_copy (child);

		smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

		cdn_stack_arg_copy (&args.args[idx--], &smanip->push);

		// Then see if we need to derive this
		if (towardsmap[i])
		{
			CdnVariable *tow;

			tow = cdn_function_argument_get_variable (towards->data);

			towards = g_slist_delete_link (towards, towards);

			if (!cdn_variable_get_derivative (tow))
			{
				// Derive it here
				derived = derive_iter (child, ctx);

				if (!derived)
				{
					// Oops
					cdn_stack_args_destroy (&args);
					g_free (towardsmap);
					g_free (children);
					g_slist_free (towards);

					g_object_unref (df);

					if (ft)
					{
						g_object_unref (ft);
					}

					return NULL;
				}

				instr = cdn_expression_tree_iter_get_instruction (derived);
				smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

				children[newstart++] = derived;

				cdn_stack_arg_copy (&args.args[newidx--], &smanip->push);
			}
		}
	}

	if (ft)
	{
		CdnDimension onedim = CDN_DIMENSION(1, 1);

		children[newstart++] = iter_new_num (1);
		args.args[newidx--].dimension = onedim;
	}

	nf = cdn_function_for_dimension (df, &args);
	g_object_unref (df);

	ret = iter_new_take (cdn_instruction_custom_function_new (nf,
	                                                          &args));

	g_object_unref (nf);

	ret->children = children;
	ret->num_children = num + newgen;

	cdn_stack_args_destroy (&args);
	g_free (towardsmap);
	g_slist_free (towards);

	if (ft)
	{
		g_object_unref (ft);
	}

	return ret;
}

static CdnExpressionTreeIter *
derive_custom_function (CdnExpressionTreeIter        *iter,
                        CdnInstructionCustomFunction *instr,
                        DeriveContext                *ctx)
{
	return derive_custom_function_real (iter,
	                                    cdn_instruction_custom_function_get_function (instr),
	                                    ctx,
	                                    FALSE);
}

static CdnExpressionTreeIter *
derive_custom_function_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomFunctionRef *instr,
                            DeriveContext                   *ctx)
{
	return derive_custom_function_real (iter,
	                                    cdn_instruction_custom_function_ref_get_function (instr),
	                                    ctx,
	                                    TRUE);
}

static CdnExpressionTreeIter *
derive_custom_operator_real (CdnExpressionTreeIter *iter,
                             CdnOperator           *op,
                             DeriveContext         *ctx,
                             gboolean               isref)
{
	CdnFunction *f;

	f = cdn_operator_get_primary_function (op);

	if (f)
	{
		return derive_custom_function_real (iter, f, ctx, isref);
	}
	else
	{
		g_set_error (ctx->error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Derivation of the operator `%s' is not supported",
		             cdn_operator_get_name (op));

		return NULL;
	}
}

static CdnExpressionTreeIter *
derive_custom_operator (CdnExpressionTreeIter        *iter,
                        CdnInstructionCustomOperator *instr,
                        DeriveContext                *ctx)
{
	CdnOperator *op;

	op = cdn_instruction_custom_operator_get_operator (instr);
	return derive_custom_operator_real (iter, op, ctx, FALSE);
}

static CdnExpressionTreeIter *
derive_custom_operator_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomOperatorRef *instr,
                            DeriveContext                   *ctx)
{
	CdnOperator *op;

	op = cdn_instruction_custom_operator_ref_get_operator (instr);
	return derive_custom_operator_real (iter, op, ctx, TRUE);
}

static CdnExpressionTreeIter *
derive_matrix (CdnExpressionTreeIter *iter,
               CdnInstructionMatrix  *matrix,
               DeriveContext         *context)
{
	CdnExpressionTreeIter *ret = NULL;
	guint i;

	ret = iter_new_sized (CDN_INSTRUCTION (matrix), iter->num_children);

	// Simply derive all the children
	for (i = 0; i < cdn_expression_tree_iter_get_num_children (iter); ++i)
	{
		CdnExpressionTreeIter *child;

		child = cdn_expression_tree_iter_get_child (iter, i);
		child = derive_iter (child, context);

		if (!child)
		{
			cdn_expression_tree_iter_free (ret);
			return NULL;
		}

		ret->children[i] = child;
	}

	return ret;
}

static CdnExpressionTreeIter *
derive_index_instr (CdnExpressionTreeIter *iter,
                    CdnInstructionIndex   *index,
                    DeriveContext         *context)
{
	CdnExpressionTreeIter *ret = NULL;
	CdnExpressionTreeIter *deriv;

	deriv = derive_iter (iter->children[0], context);

	if (!deriv)
	{
		return NULL;
	}

	// Index into the derived
	ret = iter_copy (iter);
	cdn_expression_tree_iter_free (ret->children[0]);
	ret->children[0] = deriv;

	return ret;
}

static CdnExpressionTreeIter *
derive_iter (CdnExpressionTreeIter *iter,
             DeriveContext         *ctx)
{
	CdnInstruction *instr;
	CdnExpressionTreeIter *ret = NULL;

	instr = cdn_expression_tree_iter_get_instruction (iter);

	if (CDN_IS_INSTRUCTION_NUMBER (instr))
	{
		// Derivation of a constant is 0
		ret = iter_new_numstr ("0");
	}
	else if (CDN_IS_INSTRUCTION_FUNCTION (instr) &&
	         cdn_instruction_function_get_id (CDN_INSTRUCTION_FUNCTION (instr)) < CDN_MATH_FUNCTION_TYPE_NUM_OPERATORS)
	{
		ret = derive_operator (iter,
		                       CDN_INSTRUCTION_FUNCTION (instr),
		                       ctx);
	}
	else if (CDN_IS_INSTRUCTION_FUNCTION (instr))
	{
		ret = derive_function (iter,
		                       CDN_INSTRUCTION_FUNCTION (instr),
		                       ctx);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		ret = derive_custom_function (iter,
		                              CDN_INSTRUCTION_CUSTOM_FUNCTION (instr),
		                              ctx);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr))
	{
		ret = derive_custom_function_ref (iter,
		                                  CDN_INSTRUCTION_CUSTOM_FUNCTION_REF (instr),
		                                  ctx);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR (instr))
	{
		ret = derive_custom_operator (iter,
		                              CDN_INSTRUCTION_CUSTOM_OPERATOR (instr),
		                              ctx);
	}
	else if (CDN_IS_INSTRUCTION_CUSTOM_OPERATOR_REF (instr))
	{
		ret = derive_custom_operator_ref (iter,
		                                  CDN_INSTRUCTION_CUSTOM_OPERATOR_REF (instr),
		                                  ctx);
	}
	else if (CDN_IS_INSTRUCTION_VARIABLE (instr))
	{
		ret = derive_variable (iter,
		                       CDN_INSTRUCTION_VARIABLE (instr),
		                       ctx);
	}
	else if (CDN_IS_INSTRUCTION_MATRIX (instr))
	{
		ret = derive_matrix (iter,
		                     CDN_INSTRUCTION_MATRIX (instr),
		                     ctx);
	}
	else if (CDN_IS_INSTRUCTION_INDEX (instr))
	{
		ret = derive_index_instr (iter,
		                          CDN_INSTRUCTION_INDEX (instr),
		                          ctx);
	}
	else
	{
		g_set_error (ctx->error,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
		             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
		             "Instructions of type `%s' cannot be derived",
		             g_type_name (G_TYPE_FROM_INSTANCE (instr)));
	}

	return ret;
}

static GHashTable *
variables_to_hash_set (GSList *variables)
{
	GHashTable *ret;

	ret = g_hash_table_new_full (g_direct_hash,
	                             g_direct_equal,
	                             (GDestroyNotify)g_object_unref,
	                             NULL);

	while (variables)
	{
		g_hash_table_insert (ret,
		                     g_object_ref (variables->data),
		                     GINT_TO_POINTER (1));

		variables = g_slist_next (variables);
	}

	return ret;
}

#define DEBUG_PRINTIT

#ifdef DEBUG_PRINTIT
static void
print_syms (GSList *syms)
{
	GString *s;

	s = g_string_new ("Symbols: ");

	while (syms)
	{
		gchar *nm;

		nm = cdn_variable_get_full_name (syms->data);
		g_string_append (s, nm);
		g_free (nm);

		if (syms->next)
		{
			g_string_append (s, ", ");
		}

		syms = g_slist_next (syms);
	}

	cdn_debug_message (DEBUG_DIFF,
	                   "%s",
	                   s->str);

	g_string_free (s, TRUE);
}

static void
foreach_towards (CdnVariable *key,
                 CdnExpressionTreeIter *value,
                 GString *s)
{
	gchar *nm;

	nm = cdn_variable_get_full_name (key);
	g_string_append (s, nm);
	g_string_append (s, ", ");
	g_free (nm);
}

static void
print_towards (GHashTable *towards)
{
	GString *s;

	if (!towards)
	{
		return;
	}

	s = g_string_new ("Towards: ");

	g_hash_table_foreach (towards, (GHFunc)foreach_towards, s);

	cdn_debug_message (DEBUG_DIFF,
	                   "%s",
	                   s->str);

	g_string_free (s, TRUE);
}
#endif

/**
 * cdn_expression_tree_iter_derive:
 * @iter: the expression to derive
 * @symbols: (element-type CdnVariable): a list of symbols considered to be
 *                                       derivable (e.g. all arguments to a
 *                                       function).
 * @towards: (element-type CdnVariable CdnExpressionTreeIter):
 *                                       a hash of symbols to derive (mapping
 *                                       to its derivative if any)
 * @order: the order of the derivation
 * @flags: a #CdnExpressionTreeIterDeriveFlags
 * @error: a #GError
 *
 * Derive an expression symbolically.
 *
 * Returns: (transfer full): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_derive (CdnExpressionTreeIter             *iter,
                                 GSList                            *symbols,
                                 GHashTable                        *towards,
                                 gint                               order,
                                 CdnExpressionTreeIterDeriveFlags   flags,
                                 GError                           **error)
{
	DeriveContext ctx;
	CdnExpressionTreeIter *derived = NULL;

	if (iter == NULL)
	{
		return NULL;
	}

	if (order == 0)
	{
		return iter_copy (iter);
	}


	if (cdn_debug_is_enabled (CDN_DEBUG_DIFF))
	{
		CdnStackManipulation const *smanip;

		smanip = cdn_instruction_get_stack_manipulation (iter->instruction, NULL);

		cdn_debug_message (DEBUG_DIFF,
		                   "Deriving: {%s} (%d-by-%d)",
		                   cdn_expression_tree_iter_to_string_dbg (iter),
		                   smanip ? smanip->push.rows : 0,
		                   smanip ? smanip->push.columns : 0);

#ifdef DEBUG_PRINTIT
		print_syms (symbols);
		print_towards (towards);
	}
#endif

	ctx.flags = flags;
	ctx.error = error;

	ctx.symbols = variables_to_hash_set (symbols);
	ctx.towards = towards;

	if (!towards)
	{
		ctx.towards = g_hash_table_new (g_direct_hash,
		                                g_direct_equal);
	}
	else
	{
		g_hash_table_ref (ctx.towards);
	}

	derived = iter;

	while (order > 0)
	{
		CdnExpressionTreeIter *tmp;

		tmp = derive_iter (derived, &ctx);

		if (derived != iter)
		{
			cdn_expression_tree_iter_free (derived);
		}

		derived = tmp;

		if (!derived)
		{
			break;
		}

		--order;
	}

	g_hash_table_destroy (ctx.symbols);
	g_hash_table_unref (ctx.towards);

	if (!derived)
	{
		if (error && !*error)
		{
			g_set_error (error,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
			             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
			             "Derivation of expression `%s' is not supported",
			             cdn_expression_tree_iter_to_string (iter));
		}

		return NULL;
	}

	if (flags & CDN_EXPRESSION_TREE_ITER_DERIVE_SIMPLIFY)
	{
		derived = cdn_expression_tree_iter_simplify (derived);
	}

	cdn_debug_message (DEBUG_DIFF,
	                   "Derived: {%s}",
	                   cdn_expression_tree_iter_to_string_dbg (derived));

	return derived;
}
