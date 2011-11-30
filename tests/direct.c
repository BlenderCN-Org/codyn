#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cdn>\n"
"  <network>\n"
"    <node id=\"s1\">\n"
"      <variable name=\"a\">1</variable>\n"
"    </node>\n"
"    <node id=\"s2\">\n"
"      <variable name=\"b\">0</variable>\n"
"    </node>\n"
"    <node id=\"s3\">\n"
"      <variable name=\"c\">0</variable>\n"
"    </node>\n"
"    <edge id=\"l1\" from=\"s1\" to=\"s2\">\n"
"      <action target=\"b\">a</action>\n"
"    </edge>\n"
"    <edge id=\"l2\" from=\"s2\" to=\"s3\">\n"
"      <action target=\"c\">b</action>\n"
"    </edge>\n"
"  </network>\n"
"</cdn>\n";

static void
test_direct ()
{
	CdnNetwork *network;

	network = cdn_network_new_from_string (simple_xml, NULL);

	CdnVariable *p1 = cdn_node_find_variable (CDN_NODE (network), "s2.b");

	cdn_network_step (network, 0.1);

	cdn_assert_tol (cdn_variable_get_value (p1), 1);

	g_object_unref (network);
}

static void
test_dependencies ()
{
	CdnNetwork *network;

	network = cdn_network_new_from_string (simple_xml, NULL);

	CdnVariable *p1 = cdn_node_find_variable (CDN_NODE (network), "s3.c");

	cdn_network_step (network, 0.1);

	cdn_assert_tol (cdn_variable_get_value (p1), 1);

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
