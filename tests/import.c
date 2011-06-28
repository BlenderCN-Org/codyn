#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <import id=\"imported\">import.cpg</import>"
"  </network>\n"
"</cpg>\n";

static gchar simple_template_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <templates>\n"
"      <import id=\"imported\">import.cpg</import>"
"    </templates>\n"
"  </network>\n"
"</cpg>\n";

static void
test_import ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_TEMPLATE_OBJECT, "imported.template1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "imported.template1.x",
	                             CPG_PATH_TEMPLATE_OBJECT, "imported.template1.nested1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "imported.template1.nested1.y",
	                             CPG_PATH_OBJECT, "imported",
	                             CPG_PATH_OBJECT, "imported.state1",
	                             CPG_PATH_PROPERTY, "imported.state1.x",
	                             CPG_PATH_OBJECT, "imported.state1.nested1",
	                             CPG_PATH_PROPERTY, "imported.state1.nested1.y",
	                             NULL);

	g_object_unref (network);
}

static void
test_import_templates ()
{
	CpgNetwork *network;

	network = test_load_network (simple_template_xml,
	                             CPG_PATH_TEMPLATE_OBJECT, "imported.template1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "imported.template1.x",
	                             CPG_PATH_TEMPLATE_OBJECT, "imported.template1.nested1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "imported.template1.nested1.y",
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

	g_test_add_func ("/import/import_templates", test_import_templates);
	g_test_add_func ("/import/import", test_import);

	g_test_run ();

	return 0;
}
