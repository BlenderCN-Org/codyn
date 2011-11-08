#include "cpg-symbolic.h"
#include "cpg-math.h"
#include "cpg-expression-tree-iter.h"
#include "instructions/cpg-instructions.h"
#include "cpg-expression-tree-iter.h"

#include <math.h>
#include <glib/gprintf.h>

static GSList *derive_iter (CpgExpressionTreeIter *iter,
                            CpgProperty           *x);

static gboolean
instructions_is_number (GSList  *instructions,
                        gdouble *num)
{
	if (instructions->next || !CPG_IS_INSTRUCTION_NUMBER (instructions->data))
	{
		return FALSE;
	}

	if (num)
	{
		*num = cpg_instruction_number_get_value (instructions->data);
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
              CpgProperty           *x)
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

	fd = derive_iter (fi, x);

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

		return ret;
	}

	gd = derive_iter (gi, x);

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
                 CpgProperty           *x)
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

	fd = derive_iter (fi, x);
	gd = derive_iter (gi, x);

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
                CpgProperty           *x)
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

	fd = derive_iter (fi, x);
	gd = derive_iter (gi, x);

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
                 CpgProperty            *x)
{
	GSList *ret = NULL;

	switch (cpg_instruction_function_get_id (instr))
	{
		case CPG_MATH_OPERATOR_TYPE_UNARY_MINUS:
			ret = g_slist_prepend (ret, cpg_mini_object_copy (CPG_MINI_OBJECT (instr)));
			ret = g_slist_concat (derive_iter (cpg_expression_tree_iter_get_child (iter, 0), x),
			                      ret);
		break;
		case CPG_MATH_OPERATOR_TYPE_MINUS:
		case CPG_MATH_OPERATOR_TYPE_PLUS:
			// Linear rule: (f - g)' = f' - g'
			//         and: (f + g)' = f' + g'
			ret = g_slist_prepend (ret, cpg_mini_object_copy (CPG_MINI_OBJECT (instr)));

			ret = g_slist_concat (derive_iter (cpg_expression_tree_iter_get_child (iter, 1), x),
			                      ret);

			ret = g_slist_concat (derive_iter (cpg_expression_tree_iter_get_child (iter, 0), x),
			                      ret);
		break;
		case CPG_MATH_OPERATOR_TYPE_MULTIPLY:
			// Product rule: (fg)' = f'g + fg'
			ret = derive_product (iter, x);
		break;
		case CPG_MATH_OPERATOR_TYPE_DIVIDE:
			// Quotient rule: (f/g)' = (f'g - fg') / g^2
			ret = derive_division (iter, x);
		break;
		case CPG_MATH_OPERATOR_TYPE_POWER:
			// Power rule: (f^g)' = f^g * (f' (g / f) + g' ln (f))
			ret = derive_power (iter, x);
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
			// Can't derive this kind of stuff
		break;
	}

	return ret;
}

static GSList *
derive_cos (CpgExpressionTreeIter *f,
            CpgProperty           *x)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: -sin(x) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, x);

	a = g_slist_prepend (NULL,
	                     cpg_instruction_operator_new (CPG_MATH_OPERATOR_TYPE_UNARY_MINUS,
	                                                   "-",
	                                                   1));

	a = g_slist_prepend (a,
	                     cpg_instruction_function_new (CPG_MATH_FUNCTION_TYPE_SIN,
	                                                   "sin",
	                                                   1));

	a = g_slist_concat (gi, a);
	free_instructions (gi);

	ret = multiply_optimized (a, gd);
	free_instructions (a);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_sin (CpgExpressionTreeIter *f,
            CpgProperty           *x)
{
	GSList *gd;
	GSList *ret;
	GSList *a;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: cos(x) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, x);

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
            CpgProperty           *x)
{
	GSList *gd;
	GSList *ret;
	GSList *a = NULL;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: (1 + tan^2(x)) * x'

	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, x);

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
           CpgProperty           *x)
{
	GSList *gd;
	GSList *ret;
	GSList *gi;
	CpgExpressionTreeIter *g;

	// Chain rule: x' / x
	g = cpg_expression_tree_iter_get_child (f, 0);

	gi = cpg_expression_tree_iter_to_instructions (g);
	gd = derive_iter (g, x);

	ret = divide_optimized (gd, gi);

	free_instructions (gi);
	free_instructions (gd);

	return ret;
}

static GSList *
derive_atan (CpgExpressionTreeIter *f,
             CpgProperty           *x)
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
	gd = derive_iter (g, x);

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
             CpgProperty           *x)
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
	gd = derive_iter (g, x);

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
             CpgProperty           *x)
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
	gd = derive_iter (g, x);

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
             CpgProperty           *x)
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
	gd = derive_iter (g, x);

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
derive_function (CpgExpressionTreeIter  *iter,
                 CpgInstructionFunction *instr,
                 CpgProperty            *x)
{
	switch (cpg_instruction_function_get_id (instr))
	{
		case CPG_MATH_FUNCTION_TYPE_COS:
			// -sin(x) * x'
			return derive_cos (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_SIN:
			// cos(x) * x'
			return derive_sin (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_TAN:
			// (1 + tan^2(x)) * x'
			return derive_tan (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_ATAN:
			// x' / (1 + x^2)
			return derive_atan (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_ACOS:
			// x' / -sqrt(1 - x^2)
			return derive_acos (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_ASIN:
			// x' / sqrt(1 - x^2)
			return derive_asin (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_LN:
			// (1 / x) * x'
			return derive_ln (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_SQRT:
			// 0.5 * x^(-0.5) * x'
			return derive_sqrt (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_POW:
			return derive_power (iter, x);
		break;
		case CPG_MATH_FUNCTION_TYPE_SQSUM:
		break;
		case CPG_MATH_FUNCTION_TYPE_HYPOT:
		break;
		case CPG_MATH_FUNCTION_TYPE_EXP2:
		break;
		case CPG_MATH_FUNCTION_TYPE_EXP:
		break;
		case CPG_MATH_FUNCTION_TYPE_LOG10:
		break;
	}

	// TODO: error out
	return NULL;
}

static gboolean
property_is_acted (CpgObject  *o,
                   CpgProperty *prop)
{
	GSList const *actors;

	if (!o)
	{
		return FALSE;
	}

	actors = cpg_object_get_actors (o);

	while (actors)
	{
		if (actors->data == prop)
		{
			return TRUE;
		}

		actors = g_slist_next (actors);
	}

	return property_is_acted (cpg_object_get_parent (o), prop);
}

static GSList *
derive_property (CpgExpressionTreeIter  *iter,
                 CpgInstructionProperty *instr,
                 CpgProperty            *x)
{
	CpgProperty *prop;

	prop = cpg_instruction_property_get_property (instr);

	if (prop == x)
	{
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("1"));
	}
	else if (property_is_acted (cpg_property_get_object (prop), prop))
	{
		// Regard it as a constant
		return g_slist_prepend (NULL,
		                        cpg_instruction_number_new_from_string ("0"));
	}
	else
	{
		// Derive the property expression wrt x
		GSList *ret;
		CpgExpressionTreeIter *iter;

		iter = cpg_expression_tree_iter_new (cpg_property_get_expression (prop));
		ret = derive_iter (iter, x);
		cpg_expression_tree_iter_free (iter);

		return ret;
	}
}

static GSList *
derive_iter (CpgExpressionTreeIter *iter,
             CpgProperty           *x)
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
		                       x);
	}
	else if (CPG_IS_INSTRUCTION_FUNCTION (instr))
	{
		ret = derive_function (iter,
		                       CPG_INSTRUCTION_FUNCTION (instr),
		                       x);
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_FUNCTION (instr))
	{
		// TODO
	}
	else if (CPG_IS_INSTRUCTION_CUSTOM_OPERATOR (instr))
	{
		// TODO:
	}
	else if (CPG_IS_INSTRUCTION_PROPERTY (instr))
	{
		ret = derive_property (iter,
		                       CPG_INSTRUCTION_PROPERTY (instr),
		                       x);
	}

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

CpgExpression *
cpg_symbolic_derive (CpgExpression *expression,
                     CpgProperty   *property,
                     gint           order)
{
	CpgExpressionTreeIter *iter;
	GSList *instructions = NULL;
	gchar *es;
	CpgExpression *ret;

	g_return_val_if_fail (CPG_IS_EXPRESSION (expression), NULL);
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	if (order == 0)
	{
		return cpg_expression_copy (expression);
	}

	iter = cpg_expression_tree_iter_new (expression);

	while (order > 0)
	{
		free_instructions (instructions);

		instructions = derive_iter (iter, property);
		cpg_expression_tree_iter_free (iter);

		iter = cpg_expression_tree_iter_new_from_instructions (instructions);

		--order;
	}

	es = cpg_expression_tree_iter_to_string (iter);
	ret = cpg_expression_new (es);
	g_free (es);

	_cpg_expression_set_instructions_take (ret, instructions);
	cpg_expression_tree_iter_free (iter);

	return ret;
}
