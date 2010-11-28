#include <stdlib.h>
#include <math.h>

#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-integrator-euler.h>
#include "utils.h"

static void
test_switch ()
{
	CpgNetwork *network;

	network = cpg_network_new ();

	CpgObject *state = cpg_object_new ("state");
	CpgProperty *property = cpg_property_new ("x", "sin(t)", 0);

	cpg_object_add_property (state, property);
	cpg_group_add (CPG_GROUP (network), state, NULL);

	cpg_network_run (network, 0, 0.01, 1);
	gdouble val = cpg_property_get_value (property);

	cpg_network_set_integrator (network,
	                            CPG_INTEGRATOR (cpg_integrator_euler_new ()));

	cpg_network_run (network, 0, 0.01, 1);

	cpg_assert_tol (cpg_property_get_value (property), val);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/integrator/switch", test_switch);

	g_test_run ();

	return 0;
}
