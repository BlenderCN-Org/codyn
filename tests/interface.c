#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"node \"state\"\n"
"{\n"
"  interface\n"
"  {\n"
"    c1_x = x in c1\n"
"  }\n"
"  node \"c1\"\n"
"  {\n"
"    x = 1\n"
"  }\n"
"}\n";

static void
test_load ()
{
	CdnNetwork *network;
	CdnVariable *variable;

	network = test_load_network (simple_xml,
	                             CDN_PATH_OBJECT, "state.c1",
	                             CDN_PATH_PROPERTY, "state.c1.x",
	                             CDN_PATH_PROPERTY, "state.c1_x",
	                             NULL);

	variable = cdn_node_find_variable (CDN_NODE (network), "state.c1_x");

	cdn_assert_tol (cdn_variable_get_value (variable), 1);

	g_object_unref (network);
}

static void
test_templates ()
{
	CdnNetwork *network;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state.c1",
	                                       CDN_PATH_OBJECT, "state.c2",
	                                       CDN_PATH_PROPERTY, "state.c1.p1",
	                                       CDN_PATH_PROPERTY, "state.c2.p2",
	                                       CDN_PATH_PROPERTY, "state.c1_p1",
	                                       CDN_PATH_PROPERTY, "state.c2_p2",
	                                       NULL);

	g_object_unref (network);
}

static void
test_templates_overrides ()
{
	CdnNetwork *network;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state_override_interface.c1",
	                                       CDN_PATH_OBJECT, "state_override_interface.c3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CdnNode *node;
	CdnVariable *prop;

	node = CDN_NODE (cdn_node_get_child (CDN_NODE (network), "state_override_interface"));

	g_assert (node);

	prop = cdn_object_get_variable (CDN_OBJECT (node), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_remove_uninherited ()
{
	CdnNetwork *network;
	GError *error = NULL;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state_override_interface.c1",
	                                       CDN_PATH_OBJECT, "state_override_interface.c3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CdnNode *node;
	CdnVariable *prop;

	node = CDN_NODE (cdn_node_get_child (CDN_NODE (network), "state_override_interface"));

	g_assert (node);

	cdn_object_unapply_template (CDN_OBJECT (node),
	                             cdn_node_get_child (cdn_network_get_template_node (network),
	                                                  "t1"),
	                             &error);

	g_assert_no_error (error);

	prop = cdn_object_get_variable (CDN_OBJECT (node), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_remove_inherited ()
{
	CdnNetwork *network;
	GError *error = NULL;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state_override_interface.c1",
	                                       CDN_PATH_OBJECT, "state_override_interface.c3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CdnNode *node;
	CdnVariable *prop;

	node = CDN_NODE (cdn_node_get_child (CDN_NODE (network), "state_override_interface"));

	g_assert (node);

	cdn_object_unapply_template (CDN_OBJECT (node),
	                             cdn_node_get_child (cdn_network_get_template_node (network),
	                                                  "t3"),
	                             &error);

	g_assert_no_error (error);

	prop = cdn_object_get_variable (CDN_OBJECT (node), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p1");

	g_object_unref (network);
}

static void
test_templates_overrides_add_inherited ()
{
	CdnNetwork *network;
	CdnVariable *prop;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state_override_interface.c1",
	                                       CDN_PATH_OBJECT, "state_override_interface.c3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.cc_p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.cc_p3",
	                                       NULL);

	CdnNode *node;
	CdnVariableInterface *iface;
	CdnNode *state;

	node = CDN_NODE (cdn_node_get_child (cdn_network_get_template_node (network), "t1"));
	state = CDN_NODE (cdn_node_get_child (CDN_NODE (network), "state_override_interface"));

	iface = cdn_node_get_variable_interface (node);

	prop = cdn_object_get_variable (CDN_OBJECT (state), "cc_p3");

	g_assert (prop);
	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p3");

	cdn_variable_interface_add (iface, "cc_p3", "c1", "p1", NULL);

	prop = cdn_object_get_variable (CDN_OBJECT (state), "cc_p3");

	g_assert (prop);
	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_add_uninherited ()
{
	CdnNetwork *network;
	CdnVariable *prop;

	network = test_load_network_from_path ("test_interface_templates.cdn",
	                                       CDN_PATH_OBJECT, "state_override_interface.c1",
	                                       CDN_PATH_OBJECT, "state_override_interface.c3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CDN_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.cc_p1",
	                                       CDN_PATH_PROPERTY, "state_override_interface.cc_p3",
	                                       NULL);

	CdnNode *node;
	CdnVariableInterface *iface;
	CdnNode *state;

	node = CDN_NODE (cdn_node_get_child (cdn_network_get_template_node (network), "t3"));
	state = CDN_NODE (cdn_node_get_child (CDN_NODE (network), "state_override_interface"));

	iface = cdn_node_get_variable_interface (node);

	prop = cdn_object_get_variable (CDN_OBJECT (state), "cc_p1");

	g_assert (prop);
	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p1");

	cdn_variable_interface_add (iface, "cc_p1", "c3", "p3", NULL);

	prop = cdn_object_get_variable (CDN_OBJECT (state), "cc_p1");

	g_assert (prop);
	g_assert_cmpstr (cdn_variable_get_name (prop), ==, "p3");

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

	g_test_add_func ("/interface/load", test_load);
	g_test_add_func ("/interface/templates", test_templates);
	g_test_add_func ("/interface/templates_overrides", test_templates_overrides);
	g_test_add_func ("/interface/templates_overrides_remove_uninherited", test_templates_overrides_remove_uninherited);
	g_test_add_func ("/interface/templates_overrides_remove_inherited", test_templates_overrides_remove_inherited);
	g_test_add_func ("/interface/templates_overrides_add_uninherited", test_templates_overrides_add_uninherited);
	g_test_add_func ("/interface/templates_overrides_add_inherited", test_templates_overrides_add_inherited);

	g_test_run ();

	return 0;
}
