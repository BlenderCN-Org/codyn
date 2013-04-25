#include <stdlib.h>
#include <math.h>

#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-debug.h>

#include "utils.h"

static CdnExpression *expression;
#define RAND(A, B)  ((A) + rand() * 1.0 / RAND_MAX * ((B) - (A)))

static void
expression_initialize_context (gchar const *exp, CdnObject *context)
{
	expression = cdn_expression_new (exp);

	CdnCompileContext *ctx = cdn_compile_context_new ();
	cdn_compile_context_prepend_object (ctx, context);

	CdnCompileError *err = cdn_compile_error_new ();
	gboolean ret = cdn_expression_compile (expression, ctx, err);
	g_object_unref (ctx);

	if (!ret)
	{
		fprintf (stderr,
		         "%s\n",
		         cdn_compile_error_get_formatted_string (err));

		exit (1);
	}
}

static void
expression_initialize (gchar const *exp)
{
	expression_initialize_context (exp, NULL);
}

static gdouble
expression_eval ()
{
	return cdn_expression_evaluate (expression);
}

static void
test_operator_multiply ()
{
	CdnMatrix const *vals;

	// Simple multiplication
	expression_initialize ("3 * 4");
	cdn_assert_tol (expression_eval (), (3 * 4));

	// Matrix multiplication
	expression_initialize ("[1, 0; 0, 1] * [2, 3; 4, 5]");
	vals = cdn_expression_evaluate_values (expression);

	g_assert_cmpint (vals->dimension.rows, ==, 2);
	g_assert_cmpint (vals->dimension.columns, ==, 2);

	cdn_assert_tol (vals->values[0], 2);
	cdn_assert_tol (vals->values[1], 4);
	cdn_assert_tol (vals->values[2], 3);
	cdn_assert_tol (vals->values[3], 5);

	// Element wise multiplication
	expression_initialize ("[1, 0; 0, 1] .* [2, 3; 4, 5]");
	vals = cdn_expression_evaluate_values (expression);

	g_assert_cmpint (vals->dimension.rows, ==, 2);
	g_assert_cmpint (vals->dimension.columns, ==, 2);

	cdn_assert_tol (vals->values[0], 2);
	cdn_assert_tol (vals->values[1], 0);
	cdn_assert_tol (vals->values[2], 0);
	cdn_assert_tol (vals->values[3], 5);

}

static void
test_operator_plus ()
{
	CdnMatrix const *vals;

	expression_initialize ("3 + 4");
	cdn_assert_tol (expression_eval (), (3 + 4));

	expression_initialize ("[1, 2] + [3, 4]");
	vals = cdn_expression_evaluate_values (expression);

	g_assert_cmpint (vals->dimension.rows, ==, 1);
	g_assert_cmpint (vals->dimension.columns, ==, 2);

	cdn_assert_tol (vals->values[0], 4);
	cdn_assert_tol (vals->values[1], 6);
}

static void
test_operator_minus ()
{
	CdnMatrix const *vals;

	expression_initialize ("3 - 4");
	cdn_assert_tol (expression_eval (), (3 - 4));

	expression_initialize ("[1, 2] - [3, 4]");
	vals = cdn_expression_evaluate_values (expression);

	g_assert_cmpint (vals->dimension.rows, ==, 1);
	g_assert_cmpint (vals->dimension.columns, ==, 2);

	cdn_assert_tol (vals->values[0], -2);
	cdn_assert_tol (vals->values[1], -2);
}

static void
test_operator_minus_unary ()
{
	expression_initialize ("3 + -4");
	cdn_assert_tol (expression_eval (), (3 + -4));
}

static void
test_priority ()
{
	expression_initialize ("3 * 4 + 3");
	cdn_assert_tol (expression_eval (), (3 * 4 + 3));

	expression_initialize ("3 + 4 * 2");
	cdn_assert_tol (expression_eval (), (3 + 4 * 2));
}

static void
test_function_sin ()
{
	expression_initialize ("sin (pi)");
	cdn_assert_tol (expression_eval (), sin (M_PI));
}

static void
test_function_varargs ()
{
	expression_initialize ("max (0, 2 * pi)");
	cdn_assert_tol (expression_eval (), 2 * M_PI);
}

static void
test_complex ()
{
	CdnObject *obj = CDN_OBJECT (cdn_node_new (NULL));
	CdnVariable *prop;

	prop = cdn_variable_new ("x", cdn_expression_new ("1"), 0);
	cdn_object_add_variable (obj, prop, NULL);

	cdn_expression_compile (cdn_variable_get_expression (prop), NULL, NULL);

	prop = cdn_variable_new ("phase", cdn_expression_new ("2"), 0);
	cdn_object_add_variable (obj, prop, NULL);
	cdn_expression_compile (cdn_variable_get_expression (prop), NULL, NULL);

	prop = cdn_variable_new ("y", cdn_expression_new ("3"), 0);
	cdn_object_add_variable (obj, prop, NULL);
	cdn_expression_compile (cdn_variable_get_expression (prop), NULL, NULL);

	cdn_object_reset (obj);

	expression_initialize_context ("x * sin (phase) + 2 * y * PI", obj);
	cdn_assert_tol (expression_eval (), 1 * sin (2) + 2 * 3 * M_PI);

	g_object_unref (obj);
}

static void
test_scientific_notation ()
{
	expression_initialize ("1e-2");
	cdn_assert_tol (expression_eval (), 1e-2);

	expression_initialize ("1e+20");
	cdn_assert_tol (expression_eval (), 1e+20);

	expression_initialize ("1.25e-5");
	cdn_assert_tol (expression_eval (), 1.25e-5);

	expression_initialize ("10.2523e+4");
	cdn_assert_tol (expression_eval (), 10.2523e+4);
}

static void
test_random ()
{
	double ret;
	double val;

	srand (1);
	expression_initialize ("rand ()");
	ret = expression_eval ();

	srand (1);
	val = RAND (0, 1);
	cdn_assert_tol (ret, val);

	srand (4);
	expression_initialize ("rand (-2.5, 4.3)");
	ret = expression_eval ();

	srand (4);
	val = RAND (-2.5, 4.3);
	cdn_assert_tol (ret, val);
}

static void
test_globals ()
{
	CdnNetwork *network = cdn_network_new ();

	CdnVariable *x = cdn_variable_new ("x", cdn_expression_new ("0"), FALSE);
	cdn_object_add_variable (CDN_OBJECT (network), x, NULL);

	cdn_network_step (network, 0.001);

	cdn_variable_set_value (x, 1);
	cdn_assert_tol (1, cdn_variable_get_value (x));

	cdn_network_step (network, 0.001);
	cdn_assert_tol (1, cdn_variable_get_value (x));
}

static void
test_lerp ()
{
	cdn_test_variables_with_annotated_output_from_path ("test_lerp.cdn");
}

static void
test_clip ()
{
	cdn_test_variables_with_annotated_output_from_path ("test_clip.cdn");
}

static void
test_cycle ()
{
	cdn_test_variables_with_annotated_output_from_path ("test_cycle.cdn");
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	cdn_debug_init ();

	g_test_add_func ("/expression/operator_multiply", test_operator_multiply);
	g_test_add_func ("/expression/operator_plus", test_operator_plus);
	g_test_add_func ("/expression/operator_minus", test_operator_minus);
	g_test_add_func ("/expression/operator_minus_unary", test_operator_minus_unary);
	g_test_add_func ("/expression/priority", test_priority);
	g_test_add_func ("/expression/function_sin", test_function_sin);
	g_test_add_func ("/expression/complex", test_complex);
	g_test_add_func ("/expression/function_varargs", test_function_varargs);
	g_test_add_func ("/expression/scientific_notation", test_scientific_notation);
	g_test_add_func ("/expression/random", test_random);
	g_test_add_func ("/expression/globals", test_globals);

	g_test_add_func ("/expression/lerp", test_lerp);
	g_test_add_func ("/expression/clip", test_clip);
	g_test_add_func ("/expression/cycle", test_cycle);

	g_test_run ();

	return 0;
}
