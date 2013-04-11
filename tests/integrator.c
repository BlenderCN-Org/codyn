#include <stdlib.h>
#include <math.h>

#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-integrators.h>
#include "utils.h"

static void
test_switch ()
{
	CdnNetwork *network;

	network = cdn_network_new ();

	CdnObject *state = CDN_OBJECT (cdn_node_new ("state"));
	CdnVariable *variable = cdn_variable_new ("x",
	                                          cdn_expression_new ("sin(t)"),
	                                          0);

	cdn_object_add_variable (state, variable, NULL);
	cdn_node_add (CDN_NODE (network), state, NULL);

	cdn_network_run (network, 0, 0.01, 1, NULL);
	gdouble val = cdn_variable_get_value (variable);

	cdn_network_set_integrator (network,
	                            CDN_INTEGRATOR (cdn_integrator_euler_new ()));

	cdn_network_run (network, 0, 0.01, 1, NULL);

	cdn_assert_tol (cdn_variable_get_value (variable), val);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/integrator/switch", test_switch);

	g_test_run ();

	return 0;
}
