#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <templates>\n"
"      <state id=\"template1\">\n"
"        <property name=\"x\">0</property>\n"
"\n"
"        <state id=\"nested1\">\n"
"          <property name=\"y\">0</property>\n"
"        </state>\n"
"      </state>\n"
"    </templates>\n"
"\n"
"    <state id=\"state1\" ref=\"template1\"/>\n"
"  </network>\n"
"</cpg>\n";

static void
test_load ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_TEMPLATE_OBJECT, "template1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "template1.x",
	                             CPG_PATH_TEMPLATE_OBJECT, "template1.nested1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "template1.nested1.y",
	                             CPG_PATH_OBJECT, "state1",
	                             CPG_PATH_PROPERTY, "state1.x",
	                             CPG_PATH_OBJECT, "state1.nested1",
	                             CPG_PATH_PROPERTY, "state1.nested1.y",
	                             NULL);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/template/load", test_load);

	g_test_run ();

	return 0;
}
