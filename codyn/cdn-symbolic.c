#include "cdn-symbolic.h"
#include "cdn-math.h"
#include "cdn-expression-tree-iter.h"
#include "instructions/cdn-instructions.h"
#include "cdn-expression-tree-iter.h"
#include "cdn-operators.h"
#include "cdn-edge-action.h"
#include "cdn-debug.h"
#include "cdn-compile-error.h"

#include <math.h>
#include <glib/gprintf.h>

typedef struct
{
	GHashTable *symbols;
	CdnSymbolicDeriveFlags flags;
	GError **error;
	GHashTable *property_map;
	GHashTable *diff_map;
} DeriveContext;

static GSList *derive_iter (CdnExpressionTreeIter *iter,
                            DeriveContext         *ctx);

GQuark
cdn_symbolic_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_symbolic_error");
	}

	return quark;
}

static gboolean
instructions_is_number (GSList  *instructions,
                        gdouble *num)
{
	gdouble multiplier = 1;

	if (CDN_IS_INSTRUCTION_NUMBER (instructions->data) &&
	    instructions->next && !instructions->next->next &&
	    CDN_IS_INSTRUCTION_FUNCTION (instructions->next->data) &&
	    cdn_instruction_function_get_id (CDN_INSTRUCTION_FUNCTION (instructions->next->data)) ==
	    CDN_MATH_FUNCTION_TYPE_UNARY_MINUS)
	{
		multiplier = -1;
	}
	else if (instructions->next || !CDN_IS_INSTRUCTION_NUMBER (instructions->data))
	{
		return FALSE;
	}

	if (num)
	{
		*num = multiplier * cdn_instruction_number_get_value (instructions->data);
	}

	return TRUE;
}

static gboolean
cmp_double (gdouble a, gdouble b)
{
	return fabs (a - b) < 10e-9;
}

static gboolean
instructions_is_natural_number (GSList  *instructions,
                                gint    *num)
{
	gdouble numf;

	if (!instructions_is_number (instructions, &numf) || numf < 0)
	{
		return FALSE;
	}

	if (cmp_double (rint (numf), numf))
	{
		if (num)
		{
			*num = rint (numf);
		}

		return TRUE;
	}

	return FALSE;
}

static void
free_instructions (GSList *l)
{
	g_slist_foreach (l, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (l);
}

static GSList *
instructions_copy (GSList const *l)
{
	GSList *ret = NULL;

	while (l)
	{
		ret = g_slist_prepend (ret,
		                       cdn_mini_object_copy (l->data));

		l = g_slist_next (l);
	}

	return g_slist_reverse (ret);
}

typedef GSList *(*OptimizedOperatorFunc) (GSList *a, GSList *b);

static GSList *
multiply_optimized (GSList *a,
                    GSList *b)
{
	GSList *rret;

	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = instructions_is_number (a, &numa);
	isnumb = instructions_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		gdouble ret = numa * numb;

		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new (ret));
	}
	else if (isnuma || isnumb)
	{
		if (cmp_double (isnuma ? numa : numb, 0))
		{
			// 0 multiplied by a or b is just zero
			return g_slist_prepend (NULL,
			                        cdn_instruction_number_new_from_string ("0"));
		}
		else if (cmp_double (isnuma ? numa : numb, 1))
		{
			// 1 multiplied by a/b is just a/b
			return instructions_copy (isnuma ? b : a);
		}
	}

	/* TODO: argdim */
	rret = g_slist_prepend (NULL,
	                        cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MULTIPLY,
	                                                      "*",
	                                                      2,
	                                                      NULL));

	rret = g_slist_concat (instructions_copy (b), rret);
	rret = g_slist_concat (instructions_copy (a), rret);

	return rret;
}

static GSList *
divide_optimized (GSList *a,
                  GSList *b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = instructions_is_number (a, &numa);
	isnumb = instructions_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		gdouble ret = numa / numb;

		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new_from_string ("0"));
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret;

		/* TODO: argdim */
		ret = g_slist_prepend (NULL,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_DIVIDE,
		                                                     "/",
		                                                     2,
		                                                     NULL));

		ret = g_slist_concat (instructions_copy (b), ret);
		ret = g_slist_concat (instructions_copy (a), ret);

		return ret;
	}
}

static GSList *
add_optimized (GSList *a,
               GSList *b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = instructions_is_number (a, &numa);
	isnumb = instructions_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		gdouble ret = numa + numb;

		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		return instructions_copy (b);
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret;

		ret = g_slist_prepend (NULL,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
		                                                     "+",
		                                                     2,
		                                                     NULL));

		ret = g_slist_concat (instructions_copy (b), ret);
		ret = g_slist_concat (instructions_copy (a), ret);

		return ret;
	}
}

static GSList *
subtract_optimized (GSList *a,
                    GSList *b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = instructions_is_number (a, &numa);
	isnumb = instructions_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		gdouble ret = numa - numb;

		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		GSList *ret;

		ret = g_slist_prepend (NULL,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
		                                                     "-",
		                                                     1,
		                                                     NULL));

		ret = g_slist_concat (instructions_copy (b), ret);
		return ret;
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret;

		ret = g_slist_prepend (NULL,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_MINUS,
		                                                     "-",
		                                                     2,
		                                                     NULL));

		ret = g_slist_concat (instructions_copy (b), ret);
		ret = g_slist_concat (instructions_copy (a), ret);

		return ret;
	}
}

static GSList *
power_optimized (GSList *a,
                 GSList *b)
{
	gdouble numa;
	gboolean isnuma;

	gdouble numb;
	gboolean isnumb;

	isnuma = instructions_is_number (a, &numa);
	isnumb = instructions_is_number (b, &numb);

	if (isnuma && isnumb)
	{
		gdouble ret = pow (numa, numb);

		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new_from_string ("0"));
	}
	else if (isnuma && cmp_double (numa, 1))
	{
		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new_from_string ("1"));
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new_from_string ("1"));
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret = NULL;

		ret = g_slist_prepend (NULL,
		                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
		                                                     "^",
		                                                     2,
		                                                     NULL));

		ret = g_slist_concat (instructions_copy (b), ret);
		ret = g_slist_concat (instructions_copy (a), ret);

		return ret;
	}
}

static GSList *
derive_power_real (CdnExpressionTreeIter *fi,
                   CdnExpressionTreeIter *gi,
                   DeriveContext         *ctx)
{
	GSList *fd;
	GSList *gd;
	GSList *f;
	GSList *g;
	GSList *goverf;
	GSList *fdtimesgoverf;
	GSList *lnf;
	GSList *gdtimeslnf;
	GSList *fpowg;
	GSList *sum;
	GSList *ret;
	gint numg;

	// Power rule: (f^g)' = f^g * (f' * (g / f) + g' ln (f))

	f = cdn_expression_tree_iter_to_instructions (fi);
	g = cdn_expression_tree_iter_to_instructions (gi);

	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		free_instructions (f);
		free_instructions (g);

		return NULL;
	}

	if (instructions_is_natural_number (g, &numg))
	{
		GSList *n1;
		GSList *fn1;
		GSList *nfn1;

		n1 = g_slist_prepend (NULL,
		                      cdn_instruction_number_new (numg - 1));

		fn1 = power_optimized (f, n1);
		nfn1 = multiply_optimized (g, fn1);

		ret = multiply_optimized (nfn1, fd);

		free_instructions (n1);
		free_instructions (nfn1);
		free_instructions (fn1);
		free_instructions (fd);

		return ret;
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		free_instructions (f);
		free_instructions (g);
		free_instructions (fd);

		return NULL;
	}

	lnf = g_slist_prepend (NULL,
	                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LN,
	                                                     "ln",
	                                                     1,
	                                                     NULL));

	lnf = g_slist_concat (instructions_copy (f), lnf);

	goverf = divide_optimized (g, f);
	fdtimesgoverf = multiply_optimized (fd, goverf);
	gdtimeslnf = multiply_optimized (gd, lnf);

	sum = add_optimized (fdtimesgoverf, gdtimeslnf);

	fpowg = power_optimized (f, g);

	ret = multiply_optimized (fpowg, sum);

	free_instructions (lnf);
	free_instructions (goverf);
	free_instructions (fdtimesgoverf);
	free_instructions (gdtimeslnf);
	free_instructions (sum);
	free_instructions (fpowg);

	free_instructions (f);
	free_instructions (g);

	free_instructions (fd);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_power (CdnExpressionTreeIter *iter,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	return derive_power_real (fi, gi, ctx);

}

static GSList *
derive_division (CdnExpressionTreeIter *iter,
                 DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;
	GSList *fd;
	GSList *gd;
	GSList *f;
	GSList *g;
	GSList *a;
	GSList *b;
	GSList *numerator;
	GSList *denominator;
	GSList *ret;

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	f = cdn_expression_tree_iter_to_instructions (fi);
	g = cdn_expression_tree_iter_to_instructions (gi);

	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		free_instructions (f);
		free_instructions (g);

		return NULL;
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		free_instructions (f);
		free_instructions (g);
		free_instructions (fd);

		return NULL;
	}

	a = multiply_optimized (fd, g);
	b = multiply_optimized (f, gd);

	numerator = subtract_optimized (a, b);
	denominator = multiply_optimized (g, g);

	ret = divide_optimized (numerator, denominator);

	free_instructions (a);
	free_instructions (b);

	free_instructions (f);
	free_instructions (g);

	free_instructions (fd);
	free_instructions (gd);

	free_instructions (numerator);
	free_instructions (denominator);

	return ret;
}

static GSList *
derive_product_real (CdnExpressionTreeIter *fi,
                     CdnExpressionTreeIter *gi,
                     DeriveContext         *ctx)
{
	GSList *fd;
	GSList *gd;
	GSList *f;
	GSList *g;
	GSList *fdg;
	GSList *fgd;
	GSList *ret;

	f = cdn_expression_tree_iter_to_instructions (fi);
	g = cdn_expression_tree_iter_to_instructions (gi);

	fd = derive_iter (fi, ctx);

	if (!fd)
	{
		free_instructions (f);
		free_instructions (g);

		return NULL;
	}

	gd = derive_iter (gi, ctx);

	if (!gd)
	{
		free_instructions (f);
		free_instructions (g);
		free_instructions (fd);

		return NULL;
	}

	fdg = multiply_optimized (fd, g);
	fgd = multiply_optimized (f, gd);

	ret = add_optimized (fdg, fgd);

	free_instructions (f);
	free_instructions (g);

	free_instructions (fd);
	free_instructions (gd);

	free_instructions (fdg);
	free_instructions (fgd);

	return ret;
}

static GSList *
derive_product (CdnExpressionTreeIter *iter,
                DeriveContext         *ctx)
{
	CdnExpressionTreeIter *fi;
	CdnExpressionTreeIter *gi;

	fi = cdn_expression_tree_iter_get_child (iter, 0);
	gi = cdn_expression_tree_iter_get_child (iter, 1);

	return derive_product_real (fi, gi, ctx);
}

static GSList *
derive_operator (CdnExpressionTreeIter  *iter,
                 CdnInstructionFunction *instr,
                 DeriveContext          *ctx)
{
	GSList *ret = NULL;

	switch (cdn_instruction_function_get_id (instr))
	{
		case CDN_MATH_FUNCTION_TYPE_UNARY_MINUS:
		{
			GSList *a;

			a = derive_iter (cdn_expression_tree_iter_get_child (iter, 0), ctx);

			if (a)
			{
				ret = g_slist_prepend (ret, cdn_mini_object_copy (CDN_MINI_OBJECT (instr)));
				ret = g_slist_concat (a, ret);
			}
		}
		break;
		case CDN_MATH_FUNCTION_TYPE_MINUS:
		case CDN_MATH_FUNCTION_TYPE_PLUS:
		{
			GSList *a = NULL;
			GSList *b = NULL;

			// Linear rule: (f - g)' = f' - g'
			//         and: (f + g)' = f' + g'

			a = derive_iter (cdn_expression_tree_iter_get_child (iter, 0), ctx);

			if (a)
			{
				b = derive_iter (cdn_expression_tree_iter_get_child (iter, 1), ctx);
			}

			if (a && b)
			{
				ret = add_optimized (a, b);
			}

			free_instructions (a);
			free_instructions (b);
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
			             CDN_SYMBOLIC_ERROR,
			             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Derivation of operator `%s' is not supported",
			             cdn_instruction_function_get_name (instr));
		break;
	}

	return ret;
}

static GSList *
derive_cos (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CdnExpressionTreeIter *g;

	// Chain rule: -sin(x) * x'

	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	a = g_slist_prepend (NULL,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	                                                   "-",
	                                                   1,
	                                                   NULL));

	a = g_slist_prepend (a,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_SIN,
	                                                   "sin",
	                                                   1,
	                                                   NULL));

	a = g_slist_concat (gi, a);

	ret = multiply_optimized (a, gd);

	free_instructions (a);
	free_instructions (gd);

	return ret;
}

/*static void*/
/*print_instructions (GSList const *inst)*/
/*{*/
/*	while (inst)*/
/*	{*/
/*		g_printf ("%s ", cdn_instruction_to_string (inst->data));*/
/*		inst = g_slist_next (inst);*/
/*	}*/

/*	g_printf ("\n");*/
/*}*/

static GSList *
derive_sin (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CdnExpressionTreeIter *g;

	// Chain rule: cos(x) * x'

	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	a = g_slist_prepend (NULL,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_COS,
	                                                   "cos",
	                                                   1,
	                                                   NULL));

	a = g_slist_concat (gi, a);

	ret = multiply_optimized (a, gd);

	free_instructions (a);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_tan (CdnExpressionTreeIter *f,
            DeriveContext      *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a = NULL;
	GSList *gi;
	CdnExpressionTreeIter *g;

	// Chain rule: (1 + tan^2(x)) * x'

	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);

		return NULL;
	}

	a = g_slist_prepend (a,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
	                                                   "+",
	                                                   2,
	                                                   NULL));

	a = g_slist_prepend (a,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                   "^",
	                                                   2,
	                                                   NULL));

	a = g_slist_prepend (a,
	                     cdn_instruction_number_new_from_string ("2"));

	a = g_slist_prepend (a,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_TAN,
	                                                   "tan",
	                                                   1,
	                                                   NULL));

	a = g_slist_concat (gi, a);

	a = g_slist_prepend (a,
	                     cdn_instruction_number_new_from_string ("1"));

	ret = multiply_optimized (a, gd);
	free_instructions (a);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_ln (CdnExpressionTreeIter *f,
           DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CdnExpressionTreeIter *g;

	// Chain rule: x' / x
	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	ret = divide_optimized (gd, gi);

	free_instructions (gi);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_atan (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CdnExpressionTreeIter *g;
	GSList *composed;
	GSList *one;
	GSList *g2;

	// x' / (1 + x^2)
	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cdn_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                    "^",
	                                                    2,
	                                                    NULL));

	g2 = g_slist_prepend (g2,
	                      cdn_instruction_number_new_from_string ("2"));

	g2 = g_slist_concat (gi, g2);

	composed = add_optimized(one, g2);

	ret = divide_optimized (gd, composed);

	free_instructions (gi);
	free_instructions (composed);
	free_instructions (g2);
	free_instructions (one);

	return ret;
}

static GSList *
derive_acos (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CdnExpressionTreeIter *g;
	GSList *one;
	GSList *g2;
	GSList *sq;

	// x' / -sqrt(1 - x^2)

	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cdn_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                    "**",
	                                                    2,
	                                                    NULL));

	g2 = g_slist_prepend (g2,
	                      cdn_instruction_number_new_from_string ("2"));

	g2 = g_slist_concat (gi, g2);

	sq = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	                                                    "-",
	                                                    1,
	                                                    NULL));

	sq = g_slist_prepend (sq,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_SQRT,
	                                                    "sqrt",
	                                                    1,
	                                                    NULL));

	sq = g_slist_concat (subtract_optimized(one, g2), sq);

	ret = divide_optimized (gd, sq);

	free_instructions (gi);
	free_instructions (g2);
	free_instructions (one);
	free_instructions (sq);

	return ret;
}

static GSList *
derive_asin (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CdnExpressionTreeIter *g;
	GSList *one;
	GSList *g2;
	GSList *sq;

	// x' / sqrt(1 - x^2)

	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cdn_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_POWER,
	                                                    "^",
	                                                    2,
	                                                    NULL));

	g2 = g_slist_prepend (g2,
	                      cdn_instruction_number_new_from_string ("2"));

	g2 = g_slist_concat (gi, g2);

	sq = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_SQRT,
	                                                    "sqrt",
	                                                    1,
	                                                    NULL));

	sq = g_slist_concat (subtract_optimized(one, g2), sq);

	ret = divide_optimized (gd, sq);

	free_instructions (gi);
	free_instructions (g2);
	free_instructions (one);
	free_instructions (sq);

	return ret;
}

static GSList *
derive_sqrt_inside (CdnExpressionTreeIter *g,
                    DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	GSList *composed;
	GSList *o5;
	GSList *pp;

	// 0.5 * x^(-0.5) * x'
	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	o5 = g_slist_prepend (NULL,
	                      cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_UNARY_MINUS,
	                                                    "-",
	                                                    1,
	                                                    NULL));

	o5 = g_slist_prepend (o5,
	                      cdn_instruction_number_new_from_string ("0.5"));

	pp = power_optimized (gi, o5);

	free_instructions (o5);
	free_instructions (gi);

	o5 = g_slist_prepend (NULL,
	                      cdn_instruction_number_new_from_string ("0.5"));

	composed = multiply_optimized (o5, pp);

	free_instructions (o5);
	free_instructions (pp);

	ret = multiply_optimized (composed, gd);

	free_instructions (composed);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_sqrt (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	return derive_sqrt_inside (cdn_expression_tree_iter_get_child (f, 0), ctx);
}

static GSList *
derive_exp (CdnExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *fi;
	CdnExpressionTreeIter *g;

	g = cdn_expression_tree_iter_get_child (f, 0);

	fi = cdn_expression_tree_iter_to_instructions (f);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (fi);
		return NULL;
	}

	ret = multiply_optimized (fi, gd);

	free_instructions (fi);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_exp2 (CdnExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	GSList *p2;
	GSList *po;
	GSList *ppd;
	CdnExpressionTreeIter *g;

	// Power rule: (2^g)' = 2^x * g' ln (2)
	g = cdn_expression_tree_iter_get_child (f, 0);

	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	p2 = g_slist_prepend (NULL,
	                      cdn_instruction_number_new_from_string ("2"));

	po = power_optimized (p2, gi);

	p2 = g_slist_append (p2,
	                     cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LN,
	                                                   "ln",
	                                                   1,
	                                                   NULL));

	ppd = multiply_optimized (gd, p2);

	ret = multiply_optimized (po, ppd);

	free_instructions (gi);
	free_instructions (gd);

	free_instructions (po);
	free_instructions (ppd);
	free_instructions (p2);

	return ret;
}

static GSList *
derive_sqsum (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	GSList *ret = NULL;
	gint i;

	for (i = 0; i < cdn_expression_tree_iter_get_num_children (f); ++i)
	{
		CdnExpressionTreeIter *iter;
		GSList *pow;

		iter = cdn_expression_tree_iter_get_child (f, i);
		pow = derive_product_real (iter, iter, ctx);

		if (!pow)
		{
			free_instructions (ret);
			return NULL;
		}

		if (i != 0)
		{
			GSList *add;

			add = add_optimized (ret, pow);
			free_instructions (ret);
			ret = add;
		}
		else
		{
			ret = pow;
		}
	}

	return ret;
}

static GSList *
derive_hypot (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	GSList *sq;
	GSList *ret;
	CdnExpressionTreeIter *iter;

	sq = derive_sqsum (f, ctx);

	if (!sq)
	{
		return NULL;
	}

	iter = cdn_expression_tree_iter_new_from_instructions (sq);
	free_instructions (sq);

	// sqrt of that
	ret = derive_sqrt_inside (iter, ctx);
	cdn_expression_tree_iter_free (iter);

	return ret;
}

static GSList *
derive_log10 (CdnExpressionTreeIter *f,
              DeriveContext         *ctx)
{
	CdnExpressionTreeIter *g;
	GSList *gi;
	GSList *gd;
	GSList *top;
	GSList *ret;
	GSList *div;

	g = cdn_expression_tree_iter_get_child (f, 0);

	// f'(g) * g'
	gi = cdn_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	// log10(x)' = log10(e) / x * x'
	top = g_slist_prepend (NULL,
	                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_LOG10,
	                                                     "log10",
	                                                     1,
	                                                     NULL));

	top = g_slist_prepend (top,
	                       cdn_instruction_number_new (M_E));

	div = divide_optimized (top, gi);
	ret = multiply_optimized (div, gd);

	free_instructions (top);
	free_instructions (div);
	free_instructions (gi);

	return ret;
}

static GSList *
derive_function (CdnExpressionTreeIter  *iter,
                 CdnInstructionFunction *instr,
                 DeriveContext       *ctx)
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
	}

	g_set_error (ctx->error,
	             CDN_SYMBOLIC_ERROR,
	             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
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

static GSList *
derive_expression (CdnExpression *expression,
                   DeriveContext *ctx,
                   gboolean       mapiter)
{
	GSList *ret;
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
		return g_slist_prepend (NULL,
		                        cdn_instruction_number_new_from_string ("0"));
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

static GSList *
derive_integrated (CdnVariable   *prop,
                   DeriveContext *ctx)
{
	GSList *actors;
	GSList *instructions = NULL;
	GSList *item;
	GSList *last = NULL;
	CdnCompileError *err;
	gboolean ret = TRUE;

	err = cdn_compile_error_new ();
	actors = cdn_variable_get_actions (prop);

	for (item = actors; item; item = g_slist_next (item))
	{
		CdnExpression *e;
		GSList const *inst;
		GSList *cp = NULL;

		if (!cdn_edge_action_compile (item->data, NULL, err))
		{
			if (ctx->error)
			{
				*ctx->error = g_error_copy (cdn_compile_error_get_error (err));
			}

			ret = FALSE;
			break;
		}

		e = cdn_edge_action_get_equation (item->data);
		inst = cdn_expression_get_instructions (e);

		if (!inst)
		{
			ret = FALSE;
			break;
		}

		while (inst)
		{
			cp = g_slist_prepend (cp,
			                      cdn_mini_object_copy (CDN_MINI_OBJECT (inst->data)));

			inst = g_slist_next (inst);
		}

		cp = g_slist_reverse (cp);

		instructions = g_slist_concat (cp,
		                               instructions);

		if (item != actors)
		{
			last = g_slist_append (last,
			                       cdn_instruction_function_new (CDN_MATH_FUNCTION_TYPE_PLUS,
			                                                     "+",
			                                                     2,
			                                                     NULL));

			last = last->next;
		}
		else
		{
			last = g_slist_last (instructions);
		}
	}

	g_object_unref (err);
	g_slist_free (actors);

	if (!ret)
	{
		g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
		g_slist_free (instructions);

		return NULL;
	}

	return instructions;
}

static GSList *
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
		if (ctx->flags & CDN_SYMBOLIC_DERIVE_PARTIAL)
		{
			// Partial derivative towards this property, is 1
			return g_slist_prepend (NULL,
			                        cdn_instruction_number_new_from_string ("1"));
		}
		else
		{
			return cdn_expression_tree_iter_to_instructions (mapped);
		}
	}

	/* Check if the property is a derived symbol (we don't go deep, but we are not differentiating towards it) */
	if (g_hash_table_lookup (ctx->symbols, prop))
	{
		if (ctx->flags & CDN_SYMBOLIC_DERIVE_PARTIAL)
		{
			return g_slist_prepend (NULL,
			                        cdn_instruction_number_new_from_string ("0"));
		}
		else
		{
			return g_slist_prepend (NULL,
			                        cdn_instruction_number_new_from_string ("1"));
		}
	}

	if (cdn_variable_get_derivative (prop))
	{
		return g_slist_prepend (NULL,
		                        cdn_instruction_variable_new (cdn_variable_get_derivative (prop)));
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

static GSList *
derive_property (CdnExpressionTreeIter  *iter,
                 CdnInstructionVariable *instr,
                 DeriveContext          *ctx)
{
	return derive_property_real (instr, NULL, ctx);
}

static GSList *
derive_custom_function_intern (CdnExpressionTreeIter *iter,
                               CdnFunction           *func,
                               DeriveContext         *ctx)
{
	gint i;
	gint num;
	CdnFunction *df;
	GSList *ret = NULL;
	GSList *derived = NULL;
	gint *argdim;

	// Try to derive func towards all of its arguments
	df = cdn_function_get_derivative (func, 1, NULL);

	if (!df)
	{
		return NULL;
	}

	num = cdn_expression_tree_iter_get_num_children (iter);

	argdim = g_new0 (gint, num * 4);

	// Now, df will have new arguments for the derivatives which
	// we will compute here now and add on the stack in addition
	// to the arguments already there
	for (i = 0; i < num; ++i)
	{
		CdnExpressionTreeIter *child;
		GSList *instrs;
		CdnInstruction *instr;
		CdnStackManipulation const *smanip;

		child = cdn_expression_tree_iter_get_child (iter, i);
		instr = cdn_expression_tree_iter_get_instruction (child);

		instrs = cdn_expression_tree_iter_to_instructions (child);
		smanip = cdn_instruction_get_stack_manipulation (instr, NULL);

		argdim[i * 2] = smanip->push_dims ? smanip->push_dims[0] : 1;
		argdim[i * 2 + 1] = smanip->push_dims ? smanip->push_dims[1] : 1;

		while (instrs)
		{
			ret = g_slist_prepend (ret,
			                       cdn_mini_object_copy (instrs->data));

			instrs = g_slist_delete_link (instrs, instrs);
		}

		// Derive it here
		derived = g_slist_concat (derived, derive_iter (child, ctx));

		// Compute argdim (note, potentially slow...)
		smanip = cdn_instruction_get_stack_manipulation (g_slist_last (derived)->data,
		                                                 NULL);

		argdim[(i + num) * 2] = smanip->push_dims ? smanip->push_dims[0] : 1;
		argdim[(i + num) * 2 + 1] = smanip->push_dims ? smanip->push_dims[1] : 1;
	}

	ret = g_slist_concat (g_slist_reverse (ret),
	                      derived);

	// Finally, add a new custom function instruction
	ret = g_slist_append (ret,
	                      cdn_instruction_custom_function_new (df,
	                                                           num * 2,
	                                                           argdim));

	return ret;
}

static GSList *
derive_custom_function_real (CdnExpressionTreeIter *iter,
                             CdnFunction           *func,
                             DeriveContext         *ctx,
                             gboolean               mapargs)
{
	gint i = 0;
	GSList *ret = NULL;

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

static GSList *
derive_custom_function (CdnExpressionTreeIter        *iter,
                        CdnInstructionCustomFunction *instr,
                        DeriveContext                *ctx)
{
	return derive_custom_function_real (iter,
	                                    cdn_instruction_custom_function_get_function (instr),
	                                    ctx,
	                                    TRUE);
}

static GSList *
derive_custom_function_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomFunctionRef *instr,
                            DeriveContext                   *ctx)
{
	return derive_custom_function_real (iter,
	                                    cdn_instruction_custom_function_ref_get_function (instr),
	                                    ctx,
	                                    FALSE);
}

static GSList *
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
		             CDN_SYMBOLIC_ERROR,
		             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
		             "Derivation of the operator `%s' is not supported",
		             cdn_operator_get_name (op));

		return NULL;
	}
}

static GSList *
derive_custom_operator (CdnExpressionTreeIter        *iter,
                        CdnInstructionCustomOperator *instr,
                        DeriveContext                *ctx)
{
	CdnOperator *op;

	op = cdn_instruction_custom_operator_get_operator (instr);
	return derive_custom_operator_real (iter, op, ctx, TRUE);
}

static GSList *
derive_custom_operator_ref (CdnExpressionTreeIter           *iter,
                            CdnInstructionCustomOperatorRef *instr,
                            DeriveContext                   *ctx)
{
	CdnOperator *op;

	op = cdn_instruction_custom_operator_ref_get_operator (instr);
	return derive_custom_operator_real (iter, op, ctx, FALSE);
}

static GSList *
derive_iter (CdnExpressionTreeIter *iter,
             DeriveContext         *ctx)
{
	CdnInstruction *instr;
	GSList *ret = NULL;

	instr = cdn_expression_tree_iter_get_instruction (iter);

	if (CDN_IS_INSTRUCTION_NUMBER (instr))
	{
		// Derivation of a constant is 0
		ret = g_slist_prepend (NULL,
		                       cdn_instruction_number_new_from_string ("0"));
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

	return ret;
}

/**
 * cdn_symbolic_derive:
 * @expression: The expression to derive
 * @symbols: (element-type CdnVariable): A list of symbols to derive towards
 * @property_map: A #GHashTable
 * @diff_map: A #GHashTable
 * @order: The order of the derivation
 * @flags: A #CdnSymbolicDeriveFlags
 * @error: A #GError
 *
 * Derive an expression symbolically.
 *
 * Returns: (transfer full): A #CdnExpression
 *
 **/
CdnExpression *
cdn_symbolic_derive (CdnExpression          *expression,
                     GSList                 *symbols,
                     GHashTable             *property_map,
                     GHashTable             *diff_map,
                     gint                    order,
                     CdnSymbolicDeriveFlags  flags,
                     GError                 **error)
{
	CdnExpressionTreeIter *iter;
	GSList *instructions = NULL;
	gchar const *es;
	CdnExpression *ret;
	DeriveContext ctx;
	CdnExpressionTreeIter *mapped;

	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	if (order == 0)
	{
		return cdn_expression_copy (expression);
	}

	iter = cdn_expression_tree_iter_new (expression);

	cdn_debug_message (DEBUG_DIFF,
	                   "Deriving: {%s}",
	                   cdn_expression_tree_iter_to_string (iter));

	ctx.flags = flags;
	ctx.error = error;

	ctx.symbols = g_hash_table_new (g_direct_hash,
	                                g_direct_equal);

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
		g_hash_table_insert (ctx.symbols,
		                     symbols->data,
		                     GINT_TO_POINTER (1));

		symbols = g_slist_next (symbols);
	}

	mapped = map_iter (iter, &ctx);

	while (order > 0)
	{
		free_instructions (instructions);

		instructions = derive_iter (mapped, &ctx);

		if (mapped == iter)
		{
			cdn_expression_tree_iter_free (iter);
		}

		if (!instructions)
		{
			break;
		}

		iter = cdn_expression_tree_iter_new_from_instructions (instructions);
		mapped = map_iter (iter, &ctx);

		--order;
	}

	g_hash_table_destroy (ctx.symbols);
	g_hash_table_unref (ctx.property_map);

	if (!instructions)
	{
		if (error && !*error)
		{
			g_set_error (error,
			             CDN_SYMBOLIC_ERROR,
			             CDN_SYMBOLIC_ERROR_UNSUPPORTED,
			             "Derivation of expression `%s' is not supported",
			             cdn_expression_get_as_string (expression));
		}

		return NULL;
	}

	if (flags & CDN_SYMBOLIC_DERIVE_SIMPLIFY)
	{
		mapped = cdn_expression_tree_iter_simplify (mapped);

		free_instructions (instructions);
		instructions = cdn_expression_tree_iter_to_instructions (mapped);
	}

	es = cdn_expression_tree_iter_to_string (iter);
	ret = cdn_expression_new (es);

	cdn_expression_set_instructions_take (ret, instructions);
	g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instructions);

	cdn_debug_message (DEBUG_DIFF,
	                   "Derived: {%s}",
	                   cdn_expression_tree_iter_to_string (mapped));

	if (mapped == iter)
	{
		cdn_expression_tree_iter_free (iter);
	}

	return ret;
}

/**
 * cdn_symbolic_simplify:
 * @expression: A #CdnExpression
 *
 *  Symbolically simplify an expression.
 *
 * Returns: (transfer full): A #CdnExpression
 *
 **/
CdnExpression *
cdn_symbolic_simplify (CdnExpression *expression)
{
	CdnExpressionTreeIter *iter;
	gchar const *es;
	GSList *instructions;
	CdnExpression *ret;

	g_return_val_if_fail (CDN_IS_EXPRESSION (expression), NULL);

	iter = cdn_expression_tree_iter_new (expression);

	// Simplify
	iter = cdn_expression_tree_iter_simplify (iter);
	instructions = cdn_expression_tree_iter_to_instructions (iter);

	es = cdn_expression_tree_iter_to_string (iter);
	ret = cdn_expression_new (es);

	cdn_expression_tree_iter_free (iter);

	cdn_expression_set_instructions_take (ret, instructions);

	g_slist_foreach (instructions, (GFunc)cdn_mini_object_unref, NULL);
	g_slist_free (instructions);

	return ret;
}
