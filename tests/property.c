#include <stdlib.h>
#include <math.h>

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>
#include "utils.h"

static void
test_once ()
{
	CpgProperty *prop;

	prop = cpg_property_new ("x", "rand()", CPG_PROPERTY_FLAG_ONCE, NULL);
	cpg_expression_compile (cpg_property_get_expression (prop), NULL, NULL);

	gdouble v1 = cpg_property_get_value (prop);
	gdouble v2 = cpg_property_get_value (prop);

	cpg_assert_tol (v1, v2);
}

static void
test_once_reset_cache ()
{
	CpgProperty *prop;
	CpgExpression *expr;

	prop = cpg_property_new ("x", "rand()", CPG_PROPERTY_FLAG_ONCE, NULL);
	expr = cpg_property_get_expression (prop);

	cpg_expression_compile (expr, NULL, NULL);

	gdouble v1 = cpg_property_get_value (prop);
	cpg_expression_reset_cache (expr);

	gdouble v2 = cpg_property_get_value (prop);
	cpg_assert_tol (v1, v2);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/property/once", test_once);
	g_test_add_func ("/property/once_reset_cache", test_once_reset_cache);

	g_test_run ();

	return 0;
}
