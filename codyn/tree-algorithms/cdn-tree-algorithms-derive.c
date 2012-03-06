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
	GHashTable *symbols;
	CdnExpressionTreeIterDeriveFlags flags;
	GError **error;
	GHashTable *property_map;
	GHashTable *diff_map;
} DeriveContext;

static CdnExpressionTreeIter *derive_iter (CdnExpressionTreeIter *iter,
                                           DeriveContext         *ctx);

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
derive_operator (CdnExpressionTreeIter  *iter,
                 CdnInstructionFunction *instr,
                 DeriveContext          *ctx)
{
	CdnExpressionTreeIter *ret = NULL;

	switch (cdn_instruction_function_get_id (instr))
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		{
			CdnExpressionTreeIter *a;

			a = derive_iter (cdn_expression_tree_iter_get_child (iter, 0),
			                 ctx);

			if (a)
			{
				ret = iter_new_ufunc (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
				                      a,
				                      TRUE);
			}
		}
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
				ret = add_optimized (a, b, TRUE, TRUE);
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
		case CDN_MATH_FUNCTION_TYPE_NEGATE:
		case CDN_MATH_FUNCTION_TYPE_MODULO:
		case CDN_MATH_FUNCTION_TYPE_GREATER:
		case CDN_MATH_FUNCTION_TYPE_LESS:
		case CDN_MATH_FUNCTION_TYPE_GREATER_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_LESS_OR_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_EQUAL:
		case CDN_MATH_FUNCTION_TYPE_OR:
		case CDN_MATH_FUNCTION_TYPE_AND:
		case CDN_MATH_FUNCTION_TYPE_TERNARY:
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
			return iter_copy (iter);
		break;
	}

	g_set_error (ctx->error,
	             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR,
	             CDN_EXPRESSION_TREE_ITER_DERIVE_ERROR_UNSUPPORTED,
	             "Derivation of function `%s' is not supported",
	             cdn_instruction_function_get_name (instr));

	return NULL;
}

#undef DEBUG_PRINTIT
//#define DEBUG_PRINTIT

#ifdef DEBUG_PRINTIT
static void
print_sym (gpointer key, gpointer value, gboolean *first)
{
	if (!*first)
	{
		g_printf (", ");
	}
	else
	{
		*first = FALSE;
	}

	g_printf ("`%s'", cdn_variable_get_full_name (key));
}

static void
print_syms (GHashTable *syms)
{
	gboolean first = TRUE;
	g_printf ("    ");

	g_hash_table_foreach (syms, (GHFunc)print_sym, &first);

	g_printf ("\n");
}

static void
printit (gpointer key, gpointer value)
{
	g_printf ("    `%s' -> `%s'\n",
	          cdn_variable_get_full_name (key),
	          value ? cdn_expression_tree_iter_to_string_dbg (value) : NULL);
}

static void
print_map (GHashTable *table)
{
	if (!table)
	{
		return;
	}

	g_hash_table_foreach (table, (GHFunc)printit, NULL);
}
#endif

static CdnExpressionTreeIter *
map_iter (CdnExpressionTreeIter *iter,
          DeriveContext         *ctx)
{
	gint n;
	gint i;
	CdnInstruction *instr;

	n = cdn_expression_tree_iter_get_num_children (iter);

	for (i = 0; i < n; ++i)
	{
		CdnExpressionTreeIter *mapped;
		CdnExpressionTreeIter *child;

		child = cdn_expression_tree_iter_get_child (iter, i);

		mapped = map_iter (child, ctx);

		if (mapped != child)
		{
			cdn_expression_tree_iter_take_child (iter, i, mapped);
		}
	}

	instr = cdn_expression_tree_iter_get_instruction (iter);

	if (CDN_IS_INSTRUCTION_VARIABLE (instr))
	{
		CdnVariable *prop;
		CdnExpressionTreeIter *mapped;

		prop = cdn_instruction_variable_get_variable (CDN_INSTRUCTION_VARIABLE (instr));
		mapped = g_hash_table_lookup (ctx->property_map, prop);

		if (mapped)
		{
			CdnExpressionTreeIter *cp;
			CdnExpressionTreeIter *ncp;

			cp = cdn_expression_tree_iter_copy (mapped);

			ncp = map_iter (cp, ctx);

			if (ncp != cp)
			{
				cdn_expression_tree_iter_free (cp);
			}

			return ncp;
		}
	}

	return iter;
}

static CdnExpressionTreeIter *
derive_expression (CdnExpression *expression,
                   DeriveContext *ctx,
                   gboolean       mapiter)
{
	CdnExpressionTreeIter *ret;
	CdnExpressionTreeIter *iter;
	CdnExpressionTreeIter *mapped;

	iter = cdn_expression_tree_iter_new (expression);

	if (!iter)
	{
#ifdef DEBUG_PRINTIT
		g_printf ("Deriving non compiled (to 0): %s", cdn_expression_get_as_string (expression));
#endif

		// This means that the expression was not actually compiled yet
		// For now we are just going to assume that we are deriving that
		// to 0
		return iter_new_numstr ("0");
	}

	mapped = iter;

	if (mapiter)
	{
		mapped = map_iter (iter, ctx);
	}

#ifdef DEBUG_PRINTIT
	g_printf ("Deriving: %s\n", cdn_expression_tree_iter_to_string_dbg (iter));

	g_printf ("  Properties:\n");
	print_map (ctx->property_map);

	g_printf ("  Diff:\n");
	print_map (ctx->diff_map);

	g_printf ("  Symbols:\n");
	print_syms (ctx->symbols);

	if (mapped != iter)
	{
		g_printf ("  Mapped: %s\n", cdn_expression_tree_iter_to_string_dbg (mapped));
	}
#endif

	ret = derive_iter (mapped, ctx);

	if (mapped != iter)
	{
		cdn_expression_tree_iter_free (mapped);
	}

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
derive_property_real (CdnInstructionVariable *instr,
                      CdnVariable            *prop,
                      DeriveContext          *ctx)
{
	CdnExpressionTreeIter *mapped = NULL;

	if (!prop && instr)
	{
		prop = cdn_instruction_variable_get_variable (instr);
	}

	/* Check if the property has a diff mapped symbol */
	if (ctx->diff_map && g_hash_table_lookup_extended (ctx->diff_map,
	                                                   prop,
	                                                   NULL,
	                                                   (gpointer *)&mapped))
	{
		if (ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL)
		{
			// Partial derivative towards this property, is 1
			return iter_new_numstr ("1");
		}
		else
		{
			return iter_copy (mapped);
		}
	}

	/* Check if the property is a derived symbol (we don't go deep, but we are not differentiating towards it) */
	if (g_hash_table_lookup (ctx->symbols, prop))
	{
		if (ctx->flags & CDN_EXPRESSION_TREE_ITER_DERIVE_PARTIAL)
		{
			return iter_new_numstr ("0");
		}
		else
		{
			return iter_new_numstr ("1");
		}
	}

	if (cdn_variable_get_derivative (prop))
	{
		return iter_new_take (cdn_instruction_variable_new (cdn_variable_get_derivative (prop)));
	}
	else if (cdn_variable_get_integrated (prop))
	{
		return derive_integrated (prop, ctx);
	}
	else
	{
		// Derive further into x (considering x a helper variable)
		return derive_expression (cdn_variable_get_expression (prop),
		                          ctx,
		                          FALSE);
	}
}

static CdnExpressionTreeIter *
derive_property (CdnExpressionTreeIter  *iter,
                 CdnInstructionVariable *instr,
                 DeriveContext          *ctx)
{
	return derive_property_real (instr, NULL, ctx);
}

static CdnExpressionTreeIter *
derive_custom_function_intern (CdnExpressionTreeIter *iter,
                               CdnFunction           *func,
                               DeriveContext         *ctx)
{
	gint i;
	gint num;
	CdnFunction *df;
	CdnExpressionTreeIter *ret = NULL;
	gint *argdim;
	CdnExpressionTreeIter **children;

	// Try to derive func towards all of its arguments
	df = cdn_function_get_derivative (func, 1, NULL);

	if (!df)
	{
		return NULL;
	}

	num = cdn_expression_tree_iter_get_num_children (iter);
	argdim = g_new0 (gint, num * 4);

	children = g_new0 (CdnExpressionTreeIter *, num * 2);

	// Now, df will have new arguments for the derivatives which
	// we will compute here now and add on the stack in addition
	// to the arguments already there
	for (i = 0; i < num; ++i)
	{
		CdnExpressionTreeIter *child;
		CdnExpressionTreeIter *derived;
		CdnInstruction *instr;
		CdnStackManipulation const *smanip;
		gint didx;

		child = cdn_expression_tree_iter_get_child (iter, i);
		instr = cdn_expression_tree_iter_get_instruction (child);

		children[i] = iter_copy (child);

		smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

		argdim[i * 2] = smanip->push_dims ? smanip->push_dims[0] : 1;
		argdim[i * 2 + 1] = smanip->push_dims ? smanip->push_dims[1] : 1;

		// Derive it here
		derived = derive_iter (child, ctx);
		instr = cdn_expression_tree_iter_get_instruction (derived);
		smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

		didx = (i + num) * 2;
		children[didx] = derived;

		argdim[didx] = smanip->push_dims ? smanip->push_dims[0] : 1;
		argdim[didx + 1] = smanip->push_dims ? smanip->push_dims[1] : 1;
	}

	ret = iter_new_take (cdn_instruction_custom_function_new (df,
	                                                          num * 2,
	                                                          argdim));

	ret->children = children;
	ret->num_children = num;

	g_free (argdim);

	return ret;
}

static CdnExpressionTreeIter *
derive_custom_function_real (CdnExpressionTreeIter *iter,
                             CdnFunction           *func,
                             DeriveContext         *ctx,
                             gboolean               mapargs)
{
	gint i = 0;
	CdnExpressionTreeIter *ret = NULL;

	ret = derive_custom_function_intern (iter, func, ctx);

	if (ret)
	{
		return ret;
	}

	// Map arguments of the function object to arguments in the iter
	if (mapargs)
	{
		GList const *args;
		GSList *mapped = NULL;
		args = cdn_function_get_arguments (func);
		GSList *item;

		while (args)
		{
			CdnFunctionArgument *arg = args->data;
			CdnExpressionTreeIter *cp;
			CdnVariable *prop;

			args = g_list_next (args);

			prop = _cdn_function_argument_get_variable (arg);

			cp = cdn_expression_tree_iter_copy (cdn_expression_tree_iter_get_child (iter, i));

			mapped = g_slist_prepend (mapped, prop);

			g_hash_table_insert (ctx->property_map, prop, cp);

			++i;
		}

		// Map also the iters now
		for (item = mapped; item; item = g_slist_next (item))
		{
			CdnVariable *prop = item->data;
			CdnExpressionTreeIter *cp;
			CdnExpressionTreeIter *ncp;

			cp = g_hash_table_lookup (ctx->property_map, prop);

			ncp = map_iter (cp, ctx);

			if (ncp != cp)
			{
				g_hash_table_insert (ctx->property_map, prop, ncp);
			}
		}

		g_slist_free (mapped);
	}

	// Then, evaluate the function expression
	ret = derive_expression (cdn_function_get_expression (func),
	                         ctx,
	                         TRUE);

	// Remove again from the map
	if (mapargs)
	{
		GList const *args;

		args = cdn_function_get_arguments (func);

		while (args)
		{
			CdnFunctionArgument *arg = args->data;

			g_hash_table_remove (ctx->property_map,
			                     _cdn_function_argument_get_variable (arg));

			args = g_list_next (args);
		}
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
	                                    TRUE);
}

static CdnExpressionTreeIter *
derive_custom_function_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomFunctionRef *instr,
                            DeriveContext                   *ctx)
{
	return derive_custom_function_real (iter,
	                                    cdn_instruction_custom_function_ref_get_function (instr),
	                                    ctx,
	                                    FALSE);
}

static CdnExpressionTreeIter *
derive_custom_operator_real (CdnExpressionTreeIter *iter,
                             CdnOperator           *op,
                             DeriveContext         *ctx,
                             gboolean               mapargs)
{
	CdnFunction *f;

	f = cdn_operator_get_primary_function (op);

	if (f)
	{
		return derive_custom_function_real (iter, f, ctx, mapargs);
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
	return derive_custom_operator_real (iter, op, ctx, TRUE);
}

static CdnExpressionTreeIter *
derive_custom_operator_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomOperatorRef *instr,
                            DeriveContext                   *ctx)
{
	CdnOperator *op;

	op = cdn_instruction_custom_operator_ref_get_operator (instr);
	return derive_custom_operator_real (iter, op, ctx, FALSE);
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
		ret = derive_property (iter,
		                       CDN_INSTRUCTION_VARIABLE (instr),
		                       ctx);
	}
	else if (CDN_IS_INSTRUCTION_MATRIX (instr))
	{
		ret = derive_matrix (iter,
		                     CDN_INSTRUCTION_MATRIX (instr),
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

/**
 * cdn_expression_tree_iter_derive:
 * @iter: The expression to derive
 * @symbols: (element-type CdnVariable): A list of symbols to derive towards
 * @property_map: A #GHashTable
 * @diff_map: A #GHashTable
 * @order: The order of the derivation
 * @flags: A #CdnExpressionTreeIterDeriveFlags
 * @error: A #GError
 *
 * Derive an expression symbolically.
 *
 * Returns: (transfer full): A #CdnExpressionTreeIter
 *
 **/
CdnExpressionTreeIter *
cdn_expression_tree_iter_derive (CdnExpressionTreeIter             *iter,
                                 GSList                            *symbols,
                                 GHashTable                        *property_map,
                                 GHashTable                        *diff_map,
                                 gint                               order,
                                 CdnExpressionTreeIterDeriveFlags   flags,
                                 GError                           **error)
{
	DeriveContext ctx;
	CdnExpressionTreeIter *mapped;

	if (iter == NULL)
	{
		return NULL;
	}

	if (order == 0)
	{
		return iter_copy (iter);
	}

	cdn_debug_message (DEBUG_DIFF,
	                   "Deriving: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	ctx.flags = flags;
	ctx.error = error;
	ctx.symbols = g_hash_table_new (g_direct_hash, g_direct_equal);
	ctx.property_map = property_map;

	if (!ctx.property_map)
	{
		ctx.property_map = g_hash_table_new_full (g_direct_hash,
		                                          g_direct_equal,
		                                          NULL,
		                                          (GDestroyNotify)cdn_expression_tree_iter_free);
	}
	else
	{
		g_hash_table_ref (ctx.property_map);
	}

	ctx.diff_map = diff_map;

	while (symbols)
	{
		g_hash_table_insert (ctx.symbols, symbols->data, GINT_TO_POINTER (1));
		symbols = g_slist_next (symbols);
	}

	mapped = map_iter (iter, &ctx);

	while (order > 0)
	{
		CdnExpressionTreeIter *derived;

		derived = derive_iter (mapped, &ctx);
		cdn_expression_tree_iter_free (mapped);
		mapped = NULL;

		if (!derived)
		{
			break;
		}

		--order;

		if (order == 0)
		{
			mapped = derived;
			break;
		}

		mapped = map_iter (derived, &ctx);
		cdn_expression_tree_iter_free (derived);
	}

	g_hash_table_destroy (ctx.symbols);
	g_hash_table_unref (ctx.property_map);

	if (!mapped)
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
		mapped = cdn_expression_tree_iter_simplify (mapped);
	}

	cdn_debug_message (DEBUG_DIFF,
	                   "Derived: {%s}",
	                   cdn_expression_tree_iter_to_string (mapped));

	return mapped;
}
