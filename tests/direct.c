#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <group id=\"s1\">\n"
"      <property name=\"a\">1</property>\n"
"    </group>\n"
"    <group id=\"s2\">\n"
"      <property name=\"b\">0</property>\n"
"    </group>\n"
"    <group id=\"s3\">\n"
"      <property name=\"c\">0</property>\n"
"    </group>\n"
"    <link id=\"l1\" from=\"s1\" to=\"s2\">\n"
"      <action target=\"b\">a</action>\n"
"    </link>\n"
"    <link id=\"l2\" from=\"s2\" to=\"s3\">\n"
"      <action target=\"c\">b</action>\n"
"    </link>\n"
"  </network>\n"
"</cpg>\n";

static void
test_direct ()
{
	CpgNetwork *network;

	network = cpg_network_new_from_string (simple_xml, NULL);

	CpgProperty *p1 = cpg_group_find_property (CPG_GROUP (network), "s2.b");

	cpg_network_step (network, 0.1);

	cpg_assert_tol (cpg_property_get_value (p1), 1);

	g_object_unref (network);
}

static void
test_dependencies ()
{
	CpgNetwork *network;

	network = cpg_network_new_from_string (simple_xml, NULL);

	CpgProperty *p1 = cpg_group_find_property (CPG_GROUP (network), "s3.c");

	cpg_network_step (network, 0.1);

	cpg_assert_tol (cpg_property_get_value (p1), 1);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/direct/direct", test_direct);
	g_test_add_func ("/direct/dependencies", test_dependencies);

	g_test_run ();

	return 0;
}
