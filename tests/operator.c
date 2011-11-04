#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = "state \"state\" { x = \"t\" last_x = \"delayed[x, 0.1]\" ddt = \"delayed[x, dt]\" }";

static void
test_delayed ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_OBJECT, "state",
	                             CPG_PATH_PROPERTY, "state.x",
	                             CPG_PATH_PROPERTY, "state.last_x",
	                             NULL);

	CpgProperty *x = cpg_group_find_property (CPG_GROUP (network), "state.x");
	CpgProperty *last_x = cpg_group_find_property (CPG_GROUP (network), "state.last_x");

	g_assert (x);
	g_assert (last_x);

	gdouble ts[] = {0, 0.1, 0.2, 0.3, 0.4};
	gdouble pts[] = {0, 0, 0.1, 0.2, 0.3, 0.4};
	gint i;

	for (i = 0; i < 4; ++i)
	{
		cpg_assert_tol (cpg_property_get_value (x), ts[i]);
		cpg_assert_tol (cpg_property_get_value (last_x), pts[i]);

		cpg_network_step (network, 0.1);
	}

	g_object_unref (network);
}

static void
test_delayed_dt ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_OBJECT, "state",
	                             CPG_PATH_PROPERTY, "state.x",
	                             CPG_PATH_PROPERTY, "state.ddt",
	                             NULL);

	CpgProperty *x = cpg_group_find_property (CPG_GROUP (network), "state.x");
	CpgProperty *ddt = cpg_group_find_property (CPG_GROUP (network), "state.ddt");

	g_assert (x);
	g_assert (ddt);

	gdouble ts[] = {0, 0.1, 0.2, 0.3, 0.4};
	gdouble pts[] = {0, 0, 0.1, 0.2, 0.3, 0.4};
	gint i;

	for (i = 0; i < 4; ++i)
	{
		cpg_assert_tol (cpg_property_get_value (x), ts[i]);
		cpg_assert_tol (cpg_property_get_value (ddt), pts[i]);

		cpg_network_step (network, 0.1);
	}

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/operator/delayed", test_delayed);
	g_test_add_func ("/operator/delayed_dt", test_delayed_dt);

	g_test_run ();

	return 0;
}
