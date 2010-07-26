#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <state id=\"state\">\n"
"      <property name=\"x\">t</property>\n"
"      <property name=\"last_x\">lastof[x]</property>\n"
"    </state>\n"
"  </network>\n"
"</cpg>\n";

static void
test_lastof ()
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

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/operator/lastof", test_lastof);

	g_test_run ();

	return 0;
}
