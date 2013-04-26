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
test_math ()
{
	cdn_test_variables_with_annotated_output_from_path ("test_math.cdn");
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

	g_test_add_func ("/expression/math", test_math);
	g_test_add_func ("/expression/complex", test_complex);
	g_test_add_func ("/expression/random", test_random);
	g_test_add_func ("/expression/globals", test_globals);

	g_test_run ();

	return 0;
}
