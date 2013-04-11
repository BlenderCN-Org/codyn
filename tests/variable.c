#include <stdlib.h>
#include <math.h>

#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include "utils.h"

static void
test_once ()
{
	CdnVariable *prop;

	prop = cdn_variable_new ("x",
	                         cdn_expression_new ("rand()"),
	                         CDN_VARIABLE_FLAG_ONCE);

	cdn_expression_compile (cdn_variable_get_expression (prop), NULL, NULL);

	gdouble v1 = cdn_variable_get_value (prop);
	gdouble v2 = cdn_variable_get_value (prop);

	cdn_assert_tol (v1, v2);
}

static void
test_once_reset_cache ()
{
	CdnVariable *prop;
	CdnExpression *expr;

	prop = cdn_variable_new ("x",
	                         cdn_expression_new ("rand()"),
	                         CDN_VARIABLE_FLAG_ONCE);

	expr = cdn_variable_get_expression (prop);

	cdn_expression_compile (expr, NULL, NULL);

	gdouble v1 = cdn_variable_get_value (prop);
	cdn_expression_reset_cache (expr);

	gdouble v2 = cdn_variable_get_value (prop);
	cdn_assert_tol (v1, v2);
}

static void
invalid_variable_name (gchar const *name)
{
	CdnObject *obj;
	GError *error = NULL;
	gboolean ret;

	obj = CDN_OBJECT (cdn_node_new ("s"));

	ret = cdn_object_add_variable (obj,
	                               cdn_variable_new (name,
	                                                 cdn_expression_new ("0"),
	                                                 CDN_VARIABLE_FLAG_NONE),
	                               &error);

	g_assert (!ret);
	g_assert_error (error,
	                CDN_OBJECT_ERROR,
	                CDN_OBJECT_ERROR_INVALID_VARIABLE_NAME);
}

static void
test_invalid_name ()
{
	invalid_variable_name (" prop");
	invalid_variable_name ("1prop");
	invalid_variable_name ("prop-e");
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/variable/once", test_once);
	g_test_add_func ("/variable/once_reset_cache", test_once_reset_cache);
	g_test_add_func ("/variable/invalid_name", test_invalid_name);

	g_test_run ();

	return 0;
}
