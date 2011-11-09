#include "cpg-symbolic.h"
#include "cpg-math.h"
#include "cpg-expression-tree-iter.h"
#include "instructions/cpg-instructions.h"
#include "cpg-expression-tree-iter.h"
#include "cpg-integrator.h"
#include "cpg-operator-diff.h"
#include "cpg-operator-pdiff.h"

#include <math.h>
#include <glib/gprintf.h>

typedef struct
{
	CpgProperty *x;
	GHashTable *symbols;
	CpgSymbolicDeriveFlags flags;
	GError **error;
	GHashTable *property_map;
} DeriveContext;

static GSList *derive_iter (CpgExpressionTreeIter *iter,
                            DeriveContext         *ctx);

GQuark
cpg_symbolic_derive_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_symbolic_derive_error");
	}

	return quark;
}


static gboolean
instructions_is_number (GSList  *instructions,
                        gdouble *num)
{
	gdouble multiplier = 1;

	if (CPG_IS_INSTRUCTION_NUMBER (instructions->data) &&
	    instructions->next && !instructions->next->next &&
	    CPG_IS_INSTRUCTION_OPERATOR (instructions->next->data) &&
	    cpg_instruction_function_get_id (CPG_INSTRUCTION_FUNCTION (instructions->next->data)) ==
	    CPG_MATH_OPERATOR_TYPE_UNARY_MINUS)
	{
		multiplier = -1;
	}
	else if (instructions->next || !CPG_IS_INSTRUCTION_NUMBER (instructions->data))
	{
		return FALSE;
	}

	if (num)
	{
		*num = multiplier * cpg_instruction_number_get_value (instructions->data);
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
	g_slist_foreach (l, (GFunc)cpg_mini_object_free, NULL);
	g_slist_free (l);
}

static GSList *
instructions_copy (GSList const *l)
{
	GSList *ret = NULL;

	while (l)
	{
		ret = g_slist_prepend (ret,
		                       cpg_mini_object_copy (l->data));

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
		                        cpg_instruction_number_new (ret));
	}
	else if (isnuma || isnumb)
	{
		if (cmp_double (isnuma ? numa : numb, 0))
		{
			// 0 multiplied by a or b is just zero
			return g_slist_prepend (NULL,
			                        cpg_instruction_number_new_from_string ("0"));
		}
		else if (cmp_double (isnuma ? numa : numb, 1))
		{
			// 1 multiplied by a/b is just a/b
			return instructions_copy (isnuma ? b : a);
		}
	}

	rret = g_slist_prepend (NULL,
	                        cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MULTIPLY,
	                                                      "*",
	                                                      2));

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
		                        cpg_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("0"));
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret;

		ret = g_slist_prepend (NULL,
		                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_DIVIDE,
		                                                     "/",
		                                                     2));

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
		                        cpg_instruction_number_new (ret));
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
		                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
		                                                     "+",
		                                                     2));

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
		                        cpg_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		GSList *ret;

		ret = g_slist_prepend (NULL,
		                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
		                                                     "-",
		                                                     1));

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
		                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_MINUS,
		                                                     "-",
		                                                     2));

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
		                        cpg_instruction_number_new (ret));
	}
	else if (isnuma && cmp_double (numa, 0))
	{
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("0"));
	}
	else if (isnuma && cmp_double (numa, 1))
	{
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("1"));
	}
	else if (isnumb && cmp_double (numb, 0))
	{
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("1"));
	}
	else if (isnumb && cmp_double (numb, 1))
	{
		return instructions_copy (a);
	}
	else
	{
		GSList *ret = NULL;

		ret = g_slist_prepend (NULL,
		                       cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
		                                                     "**",
		                                                     2));

		ret = g_slist_concat (instructions_copy (b), ret);
		ret = g_slist_concat (instructions_copy (a), ret);

		return ret;
	}
}

static GSList *
derive_power (CpgExpressionTreeIter *iter,
              DeriveContext         *ctx)
{
	CpgExpressionTreeIter *fi;
	CpgExpressionTreeIter *gi;
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

	fi = cpg_expression_tree_iter_get_child (iter, 0);
	gi = cpg_expression_tree_iter_get_child (iter, 1);

	f = cpg_expression_tree_iter_to_instructions (fi);
	g = cpg_expression_tree_iter_to_instructions (gi);

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
		                      cpg_instruction_number_new (numg - 1));

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
	                       cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_LN,
	                                                     "ln",
	                                                     1));

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
derive_division (CpgExpressionTreeIter *iter,
                 DeriveContext         *ctx)
{
	CpgExpressionTreeIter *fi;
	CpgExpressionTreeIter *gi;
	GSList *fd;
	GSList *gd;
	GSList *f;
	GSList *g;
	GSList *a;
	GSList *b;
	GSList *numerator;
	GSList *denominator;
	GSList *ret;

	fi = cpg_expression_tree_iter_get_child (iter, 0);
	gi = cpg_expression_tree_iter_get_child (iter, 1);

	f = cpg_expression_tree_iter_to_instructions (fi);
	g = cpg_expression_tree_iter_to_instructions (gi);

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
derive_product (CpgExpressionTreeIter *iter,
                DeriveContext         *ctx)
{
	CpgExpressionTreeIter *fi;
	CpgExpressionTreeIter *gi;
	GSList *fd;
	GSList *gd;
	GSList *f;
	GSList *g;
	GSList *fdg;
	GSList *fgd;
	GSList *ret;

	fi = cpg_expression_tree_iter_get_child (iter, 0);
	gi = cpg_expression_tree_iter_get_child (iter, 1);

	f = cpg_expression_tree_iter_to_instructions (fi);
	g = cpg_expression_tree_iter_to_instructions (gi);

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
derive_operator (CpgExpressionTreeIter  *iter,
                 CpgInstructionFunction *instr,
                 DeriveContext          *ctx)
{
	GSList *ret = NULL;

	switch (cpg_instruction_function_get_id (instr))
	{
		case CPG_MATH_OPERATOR_TYPE_UNARY_MINUS:
		{
			GSList *a;

			a = derive_iter (cpg_expression_tree_iter_get_child (iter, 0), ctx);

			if (a)
			{
				ret = g_slist_prepend (ret, cpg_mini_object_copy (CPG_MINI_OBJECT (instr)));
				ret = g_slist_concat (a, ret);
			}
		}
		break;
		case CPG_MATH_OPERATOR_TYPE_MINUS:
		case CPG_MATH_OPERATOR_TYPE_PLUS:
		{
			GSList *a = NULL;
			GSList *b = NULL;

			// Linear rule: (f - g)' = f' - g'
			//         and: (f + g)' = f' + g'

			a = derive_iter (cpg_expression_tree_iter_get_child (iter, 0), ctx);

			if (a)
			{
				b = derive_iter (cpg_expression_tree_iter_get_child (iter, 1), ctx);
			}

			if (a && b)
			{
				ret = add_optimized (a, b);
			}

			free_instructions (a);
			free_instructions (b);
		}
		break;
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
			// Product rule: (fg)' = f'g + fg'
			ret = derive_product (iter, ctx);
		break;
		case CPG_MATH_OPERATOR_TYPE_DIVIDE:
			// Quotient rule: (f/g)' = (f'g - fg') / g^2
			ret = derive_division (iter, ctx);
		break;
		case CPG_MATH_OPERATOR_TYPE_POWER:
			// Power rule: (f^g)' = f^g * (f' (g / f) + g' ln (f))
			ret = derive_power (iter, ctx);
		break;
		case CPG_MATH_OPERATOR_TYPE_NEGATE:
		case CPG_MATH_OPERATOR_TYPE_MODULO:
		case CPG_MATH_OPERATOR_TYPE_GREATER:
		case CPG_MATH_OPERATOR_TYPE_LESS:
		case CPG_MATH_OPERATOR_TYPE_GREATER_OR_EQUAL:
		case CPG_MATH_OPERATOR_TYPE_LESS_OR_EQUAL:
		case CPG_MATH_OPERATOR_TYPE_EQUAL:
		case CPG_MATH_OPERATOR_TYPE_OR:
		case CPG_MATH_OPERATOR_TYPE_AND:
		case CPG_MATH_OPERATOR_TYPE_TERNARY:
			g_set_error (ctx->error,
			             CPG_SYMBOLIC_DERIVE_ERROR,
			             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
			             "Derivation of operator `%s' is not supported",
			             cpg_instruction_function_get_name (instr));
		break;
	}

	return ret;
}

static GSList *
derive_cos (CpgExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: -sin(x) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	a = g_slist_prepend (NULL,
	                     cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	                                                   "-",
	                                                   1));

	a = g_slist_prepend (a,
	                     cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_SIN,
	                                                   "sin",
	                                                   1));

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
/*		g_printf ("%s ", cpg_instruction_to_string (inst->data));*/
/*		inst = g_slist_next (inst);*/
/*	}*/

/*	g_printf ("\n");*/
/*}*/

static GSList *
derive_sin (CpgExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: cos(x) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	a = g_slist_prepend (NULL,
	                     cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_COS,
	                                                   "cos",
	                                                   1));

	a = g_slist_concat (gi, a);

	ret = multiply_optimized (a, gd);

	free_instructions (a);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_tan (CpgExpressionTreeIter *f,
            DeriveContext      *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *a = NULL;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: (1 + tan^2(x)) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);

		return NULL;
	}

	a = g_slist_prepend (a,
	                     cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_PLUS,
	                                                   "+",
	                                                   2));

	a = g_slist_prepend (a,
	                     cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
	                                                   "**",
	                                                   2));

	a = g_slist_prepend (a,
	                     cpg_instruction_number_new_from_string ("2"));

	a = g_slist_prepend (a,
	                     cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_TAN,
	                                                   "tan",
	                                                   1));

	a = g_slist_concat (gi, a);

	a = g_slist_prepend (a,
	                     cpg_instruction_number_new_from_string ("1"));

	ret = multiply_optimized (a, gd);
	free_instructions (a);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_ln (CpgExpressionTreeIter *f,
           DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: x' / x
	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
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
derive_atan (CpgExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;
	GSList *composed;
	GSList *one;
	GSList *g2;

	// x' / (1 + x^2)
	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cpg_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
	                                                    "**",
	                                                    2));

	g2 = g_slist_prepend (g2,
	                      cpg_instruction_number_new_from_string ("2"));

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
derive_acos (CpgExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;
	GSList *one;
	GSList *g2;
	GSList *sq;

	// x' / -sqrt(1 - x^2)

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cpg_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
	                                                    "**",
	                                                    2));

	g2 = g_slist_prepend (g2,
	                      cpg_instruction_number_new_from_string ("2"));

	g2 = g_slist_concat (gi, g2);

	sq = g_slist_prepend (NULL,
	                      cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	                                                    "-",
	                                                    1));

	sq = g_slist_prepend (sq,
	                      cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_SQRT,
	                                                    "sqrt",
	                                                    1));

	sq = g_slist_concat (subtract_optimized(one, g2), sq);

	ret = divide_optimized (gd, sq);

	free_instructions (gi);
	free_instructions (g2);
	free_instructions (one);
	free_instructions (sq);

	return ret;
}

static GSList *
derive_asin (CpgExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;
	GSList *one;
	GSList *g2;
	GSList *sq;

	// x' / sqrt(1 - x^2)

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	one = g_slist_prepend (NULL,
	                       cpg_instruction_number_new_from_string ("1"));

	g2 = g_slist_prepend (NULL,
	                      cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_POWER,
	                                                    "**",
	                                                    2));

	g2 = g_slist_prepend (g2,
	                      cpg_instruction_number_new_from_string ("2"));

	g2 = g_slist_concat (gi, g2);

	sq = g_slist_prepend (NULL,
	                      cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_SQRT,
	                                                    "sqrt",
	                                                    1));

	sq = g_slist_concat (subtract_optimized(one, g2), sq);

	ret = divide_optimized (gd, sq);

	free_instructions (gi);
	free_instructions (g2);
	free_instructions (one);
	free_instructions (sq);

	return ret;
}

static GSList *
derive_sqrt (CpgExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;
	GSList *composed;
	GSList *o5;
	GSList *pp;

	// 0.5 * x^(-0.5) * x'
	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	o5 = g_slist_prepend (NULL,
	                      cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	                                                    "-",
	                                                    1));

	o5 = g_slist_prepend (o5,
	                      cpg_instruction_number_new_from_string ("0.5"));

	pp = power_optimized (gi, o5);

	free_instructions (o5);
	free_instructions (gi);

	o5 = g_slist_prepend (NULL,
	                      cpg_instruction_number_new_from_string ("0.5"));

	composed = multiply_optimized (o5, pp);

	free_instructions (o5);
	free_instructions (pp);

	ret = multiply_optimized (composed, gd);

	free_instructions (composed);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_exp (CpgExpressionTreeIter *f,
            DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *fi;
	CpgExpressionTreeIter *g;

	g = cpg_expression_tree_iter_get_child (f, 0);

	fi = cpg_expression_tree_iter_to_instructions (f);
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
derive_exp2 (CpgExpressionTreeIter *f,
             DeriveContext         *ctx)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	GSList *p2;
	GSList *po;
	GSList *ppd;
	CpgExpressionTreeIter *g;

	// Power rule: (2^g)' = 2^x * g' ln (2)
	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, ctx);

	if (!gd)
	{
		free_instructions (gi);
		return NULL;
	}

	p2 = g_slist_prepend (NULL,
	                      cpg_instruction_number_new_from_string ("2"));

	po = power_optimized (p2, gi);

	p2 = g_slist_append (p2,
	                     cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_LN,
	                                                   "ln",
	                                                   1));

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
derive_function (CpgExpressionTreeIter  *iter,
                 CpgInstructionFunction *instr,
                 DeriveContext       *ctx)
{
	switch (cpg_instruction_function_get_id (instr))
	{
		case CPG_MATH_FUNCTION_TYPE_COS:
			// -sin(x) * x'
			return derive_cos (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_SIN:
			// cos(x) * x'
			return derive_sin (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_TAN:
			// (1 + tan^2(x)) * x'
			return derive_tan (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_ATAN:
			// x' / (1 + x^2)
			return derive_atan (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_ACOS:
			// x' / -sqrt(1 - x^2)
			return derive_acos (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_ASIN:
			// x' / sqrt(1 - x^2)
			return derive_asin (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_LN:
			// (1 / x) * x'
			return derive_ln (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_SQRT:
			// 0.5 * x^(-0.5) * x'
			return derive_sqrt (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_POW:
			return derive_power (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_SQSUM:
		break;
		case CPG_MATH_FUNCTION_TYPE_HYPOT:
		break;
		case CPG_MATH_FUNCTION_TYPE_EXP2:
			return derive_exp2 (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_EXP:
			return derive_exp (iter, ctx);
		break;
		case CPG_MATH_FUNCTION_TYPE_LOG10:
		break;
	}

	g_set_error (ctx->error,
	             CPG_SYMBOLIC_DERIVE_ERROR,
	             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
	             "Derivation of function `%s' is not supported",
	             cpg_instruction_function_get_name (instr));

	return NULL;
}

static gboolean
property_is_time (CpgProperty *prop)
{
	return prop == NULL ||
	       (g_strcmp0 (cpg_property_get_name (prop), "t") == 0 &&
	        CPG_IS_INTEGRATOR (cpg_property_get_object (prop)));
}

static GSList *
derive_expression (CpgExpression *expression,
                   DeriveContext *ctx)
{
	GSList *ret;
	CpgExpressionTreeIter *iter;

	iter = cpg_expression_tree_iter_new (expression);
	ret = derive_iter (iter, ctx);

	cpg_expression_tree_iter_free (iter);
	return ret;
}

static GSList *
derive_property (CpgExpressionTreeIter  *iter,
                 CpgInstructionProperty *instr,
                 DeriveContext          *ctx)
{
	CpgProperty *prop;
	CpgExpressionTreeIter *mapped;

	prop = cpg_instruction_property_get_property (instr);

	mapped = g_hash_table_lookup (ctx->property_map, prop);

	if (mapped)
	{
		return derive_iter (mapped, ctx);
	}

	if (cpg_instruction_property_get_binding (instr) &
	    CPG_INSTRUCTION_PROPERTY_BINDING_DIFF)
	{
		// Collect an expression that represents the differential
		// equation on 'prop' and go deep
		return derive_expression (cpg_instruction_property_get_diff (instr),
		                          ctx);
	}

	if (g_hash_table_lookup (ctx->symbols, prop))
	{
		// This is for partial derivatives
		if (prop == ctx->x)
		{
			// Partial derivative towards this property, is 1
			return g_slist_prepend (NULL,
			                        cpg_instruction_number_new_from_string ("1"));
		}
		else
		{
			// Regard it as a constant for partial derivative
			return g_slist_prepend (NULL,
			                        cpg_instruction_number_new_from_string ("0"));
		}
	}

	if (!(ctx->flags & CPG_SYMBOLIC_DERIVE_PARTIAL) &&
	    (ctx->x == prop || (!ctx->x && property_is_time (prop))))
	{
		// Fully deriving towards this
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("1"));
	}

	if (cpg_property_get_integrated (prop) &&
	    property_is_time (ctx->x))
	{
		// This is an integrated property derived towards t resulting
		// in x'
		return g_slist_prepend (NULL,
		                        cpg_instruction_property_new (prop,
		                                                      CPG_INSTRUCTION_PROPERTY_BINDING_DIFF));
	}
	else
	{
		// Derive further into x (considering x a helper variable)
		return derive_expression (cpg_property_get_expression (prop),
		                          ctx);
	}
}

static GSList *
derive_custom_function_real (CpgExpressionTreeIter *iter,
                             CpgFunction           *func,
                             DeriveContext         *ctx)
{
	GList const *args;
	gint i = 0;
	GSList *ret;

	// Map arguments of the function object to arguments in the iter
	args = cpg_function_get_arguments (func);

	while (args)
	{
		CpgFunctionArgument *arg = args->data;

		g_hash_table_insert (ctx->property_map,
		                     _cpg_function_argument_get_property (arg),
		                     cpg_expression_tree_iter_get_child (iter, i));

		args = g_list_next (args);
		++i;
	}

	// Then, evaluate the function expression
	ret = derive_expression (cpg_function_get_expression (func),
	                         ctx);

	// Remove again from the map
	args = cpg_function_get_arguments (func);

	while (args)
	{
		CpgFunctionArgument *arg = args->data;

		g_hash_table_remove (ctx->property_map,
		                     _cpg_function_argument_get_property (arg));

		args = g_list_next (args);
	}

	return ret;
}

static GSList *
derive_custom_function (CpgExpressionTreeIter        *iter,
                        CpgInstructionCustomFunction *instr,
                        DeriveContext                *ctx)
{
	return derive_custom_function_real (iter,
	                                    cpg_instruction_custom_function_get_function (instr),
	                                    ctx);
}

static GSList *
derive_custom_function_ref (CpgExpressionTreeIter           *iter,
                            CpgInstructionCustomFunctionRef *instr,
                            DeriveContext                   *ctx)
{
	return derive_custom_function_real (iter,
	                                    cpg_instruction_custom_function_ref_get_function (instr),
	                                    ctx);
}

static GSList *
derive_operator_diff (CpgExpressionTreeIter *iter,
                      CpgOperatorDiff       *diff,
                      DeriveContext         *ctx)
{
	// Recurse into diff expression
	return derive_expression (cpg_operator_diff_get_derived (diff),
	                          ctx);
}

static GSList *
derive_operator_pdiff (CpgExpressionTreeIter *iter,
                       CpgOperatorPDiff      *pdiff,
                       DeriveContext         *ctx)
{
	// Recurse into pdiff function
	return derive_custom_function_real (iter,
	                                    cpg_operator_pdiff_get_function (pdiff),
	                                    ctx);
}

static GSList *
derive_custom_operator (CpgExpressionTreeIter        *iter,
                        CpgInstructionCustomOperator *instr,
                        DeriveContext                *ctx)
{
	CpgOperator *op;

	op = cpg_instruction_custom_operator_get_operator (instr);

	if (CPG_IS_OPERATOR_DIFF (op))
	{
		return derive_operator_diff (iter,
		                             CPG_OPERATOR_DIFF (op),
		                             ctx);
	}
	else if (CPG_IS_OPERATOR_PDIFF (op))
	{
		return derive_operator_pdiff (iter,
		                              CPG_OPERATOR_PDIFF (op),
		                              ctx);
	}
	else
	{
		g_set_error (ctx->error,
		             CPG_SYMBOLIC_DERIVE_ERROR,
		             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
		             "Derivation of the operator `%s' is not supported",
		             cpg_operator_get_name (op));

		return NULL;
	}
}

static GSList *
derive_iter (CpgExpressionTreeIter *iter,
             DeriveContext      *ctx)
{
	CpgInstruction *instr;
	GSList *ret = NULL;

	instr = cpg_expression_tree_iter_get_instruction (iter);

	if (CPG_IS_INSTRUCTION_NUMBER (instr))
	{
		// Derivation of a constant is 0
		ret = g_slist_prepend (NULL,
		                       cpg_instruction_number_new_from_string ("0"));
	}
	else if (CPG_IS_INSTRUCTION_OPERATOR (instr))
	{
		ret = derive_operator (iter,
		                       CPG_INSTRUCTION_FUNCTION (instr),
		                       ctx);
	}
	else if (CPG_IS_INSTRUCTION_FUNCTION (instr))
	{
		ret = derive_function (iter,
		                       CPG_INSTRUCTION_FUNCTION (instr),
		                       ctx);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		ret = derive_custom_function (iter,
		                              CPG_INSTRUCTION_CUSTOM_FUNCTION (instr),
		                              ctx);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION_REF (instr))
	{
		ret = derive_custom_function_ref (iter,
		                                  CPG_INSTRUCTION_CUSTOM_FUNCTION_REF (instr),
		                                  ctx);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (instr))
	{
		ret = derive_custom_operator (iter,
		                              CPG_INSTRUCTION_CUSTOM_OPERATOR (instr),
		                              ctx);
	}
	else if (CPG_IS_INSTRUCTION_PROPERTY (instr))
	{
		ret = derive_property (iter,
		                       CPG_INSTRUCTION_PROPERTY (instr),
		                       ctx);
	}

	return ret;
}

CpgExpression *
cpg_symbolic_derive (CpgExpression          *expression,
                     GSList                 *symbols,
                     GHashTable             *property_map,
                     CpgProperty            *property,
                     gint                    order,
                     CpgSymbolicDeriveFlags  flags,
                     GError                 **error)
{
	CpgExpressionTreeIter *iter;
	GSList *instructions = NULL;
	gchar *es;
	CpgExpression *ret;
	DeriveContext ctx;

	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);
	g_return_val_if_fail (property == NULL || CPG_IS_PROPERTY (property), NULL);

	if (order == 0)
	{
		return cpg_expression_copy (expression);
	}

	iter = cpg_expression_tree_iter_new (expression);

	ctx.x = property;
	ctx.flags = flags;
	ctx.error = error;

	ctx.symbols = g_hash_table_new (g_direct_hash,
	                                g_direct_equal);

	ctx.property_map = property_map;

	if (!ctx.property_map)
	{
		ctx.property_map = g_hash_table_new (g_direct_hash,
		                                     g_direct_equal);
	}
	else
	{
		g_hash_table_ref (ctx.property_map);
	}

	while (symbols)
	{
		g_hash_table_insert (ctx.symbols,
		                     symbols->data,
		                     GINT_TO_POINTER (1));

		symbols = g_slist_next (symbols);
	}

	while (order > 0)
	{
		free_instructions (instructions);

		instructions = derive_iter (iter, &ctx);
		cpg_expression_tree_iter_free (iter);

		if (!instructions)
		{
			break;
		}

		iter = cpg_expression_tree_iter_new_from_instructions (instructions);

		--order;
	}

	g_hash_table_destroy (ctx.symbols);
	g_hash_table_unref (ctx.property_map);

	if (!instructions)
	{
		if (error && !*error)
		{
			g_set_error (error,
			             CPG_SYMBOLIC_DERIVE_ERROR,
			             CPG_SYMBOLIC_DERIVE_ERROR_UNSUPPORTED,
			             "Derivation of expression `%s' is not supported",
			             cpg_expression_get_as_string (expression));
		}

		return NULL;
	}

	es = cpg_expression_tree_iter_to_string (iter);
	ret = cpg_expression_new (es);
	g_free (es);

	_cpg_expression_set_instructions_take (ret, instructions);
	cpg_expression_tree_iter_free (iter);

	g_message ("Derived: {%s} to {%s}",
	           cpg_expression_get_as_string (expression),
	           cpg_expression_get_as_string (ret));

	return ret;
}
