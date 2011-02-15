#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <state id=\"state\">\n"
"      <interface>\n"
"        <property name=\"c1_x\">c1.x</property>\n"
"      </interface>\n"
"      <state id=\"c1\">\n"
"        <property name=\"x\">1</property>\n"
"      </state>\n"
"    </state>\n"
"  </network>\n"
"</cpg>\n";

static void
test_load ()
{
	CpgNetwork *network;
	CpgProperty *property;

	network = test_load_network (simple_xml,
	                             CPG_PATH_OBJECT, "state.c1",
	                             CPG_PATH_PROPERTY, "state.c1.x",
	                             CPG_PATH_PROPERTY, "state.c1_x",
	                             NULL);

	property = cpg_group_find_property (CPG_GROUP (network), "state.c1_x");

	cpg_assert_tol (cpg_property_get_value (property), 1);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/interface/load", test_load);

	g_test_run ();

	return 0;
}
