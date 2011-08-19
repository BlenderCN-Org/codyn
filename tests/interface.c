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

static void
test_templates ()
{
	CpgNetwork *network;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state.c1",
	                                       CPG_PATH_OBJECT, "state.c2",
	                                       CPG_PATH_PROPERTY, "state.c1.p1",
	                                       CPG_PATH_PROPERTY, "state.c2.p2",
	                                       CPG_PATH_PROPERTY, "state.c1_p1",
	                                       CPG_PATH_PROPERTY, "state.c2_p2",
	                                       NULL);

	g_object_unref (network);
}

static void
test_templates_overrides ()
{
	CpgNetwork *network;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state_override_interface.c1",
	                                       CPG_PATH_OBJECT, "state_override_interface.c3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CpgGroup *group;
	CpgProperty *prop;

	group = CPG_GROUP (cpg_group_get_child (CPG_GROUP (network), "state_override_interface"));

	g_assert (group);

	prop = cpg_object_get_property (CPG_OBJECT (group), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_remove_uninherited ()
{
	CpgNetwork *network;
	GError *error = NULL;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state_override_interface.c1",
	                                       CPG_PATH_OBJECT, "state_override_interface.c3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CpgGroup *group;
	CpgProperty *prop;

	group = CPG_GROUP (cpg_group_get_child (CPG_GROUP (network), "state_override_interface"));

	g_assert (group);

	cpg_object_unapply_template (CPG_OBJECT (group),
	                             cpg_group_get_child (cpg_network_get_template_group (network),
	                                                  "t1"),
	                             &error);

	g_assert_no_error (error);

	prop = cpg_object_get_property (CPG_OBJECT (group), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_remove_inherited ()
{
	CpgNetwork *network;
	GError *error = NULL;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state_override_interface.c1",
	                                       CPG_PATH_OBJECT, "state_override_interface.c3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       NULL);

	CpgGroup *group;
	CpgProperty *prop;

	group = CPG_GROUP (cpg_group_get_child (CPG_GROUP (network), "state_override_interface"));

	g_assert (group);

	cpg_object_unapply_template (CPG_OBJECT (group),
	                             cpg_group_get_child (cpg_network_get_template_group (network),
	                                                  "t3"),
	                             &error);

	g_assert_no_error (error);

	prop = cpg_object_get_property (CPG_OBJECT (group), "c1_p1");

	g_assert (prop);

	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p1");

	g_object_unref (network);
}

static void
test_templates_overrides_add_inherited ()
{
	CpgNetwork *network;
	CpgProperty *prop;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state_override_interface.c1",
	                                       CPG_PATH_OBJECT, "state_override_interface.c3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.cc_p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.cc_p3",
	                                       NULL);

	CpgGroup *group;
	CpgPropertyInterface *iface;
	CpgGroup *state;

	group = CPG_GROUP (cpg_group_get_child (cpg_network_get_template_group (network), "t1"));
	state = CPG_GROUP (cpg_group_get_child (CPG_GROUP (network), "state_override_interface"));

	iface = cpg_group_get_property_interface (group);

	prop = cpg_object_get_property (CPG_OBJECT (state), "cc_p3");

	g_assert (prop);
	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p3");

	cpg_property_interface_add (iface, "cc_p3", "c1", "p1", NULL);

	prop = cpg_object_get_property (CPG_OBJECT (state), "cc_p3");

	g_assert (prop);
	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_add_uninherited ()
{
	CpgNetwork *network;
	CpgProperty *prop;

	network = test_load_network_from_path ("test_interface_templates.cpg",
	                                       CPG_PATH_OBJECT, "state_override_interface.c1",
	                                       CPG_PATH_OBJECT, "state_override_interface.c3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1.p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c3.p3",
	                                       CPG_PATH_PROPERTY, "state_override_interface.c1_p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.cc_p1",
	                                       CPG_PATH_PROPERTY, "state_override_interface.cc_p3",
	                                       NULL);

	CpgGroup *group;
	CpgPropertyInterface *iface;
	CpgGroup *state;

	group = CPG_GROUP (cpg_group_get_child (cpg_network_get_template_group (network), "t3"));
	state = CPG_GROUP (cpg_group_get_child (CPG_GROUP (network), "state_override_interface"));

	iface = cpg_group_get_property_interface (group);

	prop = cpg_object_get_property (CPG_OBJECT (state), "cc_p1");

	g_assert (prop);
	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p1");

	cpg_property_interface_add (iface, "cc_p1", "c3", "p3", NULL);

	prop = cpg_object_get_property (CPG_OBJECT (state), "cc_p1");

	g_assert (prop);
	g_assert_cmpstr (cpg_property_get_name (prop), ==, "p3");

	g_object_unref (network);
}

static void
test_templates_overrides_proxy ()
{
	CpgNetwork *network;
	CpgProperty *p1;
	CpgProperty *p2;

	network = test_load_network_from_path ("test_interface_templates_override_proxy.cpg",
	                                       CPG_PATH_OBJECT, "state.c1",
	                                       CPG_PATH_OBJECT, "state.c2",
	                                       CPG_PATH_PROPERTY, "state.p1",
	                                       CPG_PATH_PROPERTY, "state.c1.p1",
	                                       CPG_PATH_PROPERTY, "state.c2.p2",
	                                       NULL);

	p1 = cpg_group_find_property (CPG_GROUP (network), "state.p1");

	g_assert (p1);

	/* The prop should be from c1 because it comes from the proxy */
	p2 = cpg_group_find_property (CPG_GROUP (network), "state.c1.p1");

	g_assert (p2);
	g_assert (p1 == p2);

	g_object_unref (network);
}

static void
test_templates_overrides_proxy_unapply ()
{
	CpgNetwork *network;
	CpgGroup *templates;
	CpgObject *temp;
	GError *error = NULL;
	CpgObject *object;
	CpgProperty *property;

	network = test_load_network_from_path ("test_interface_templates_override_proxy.cpg",
	                                       CPG_PATH_OBJECT, "state.c1",
	                                       CPG_PATH_OBJECT, "state.c2",
	                                       CPG_PATH_PROPERTY, "state.p1",
	                                       CPG_PATH_PROPERTY, "state.c1.p1",
	                                       CPG_PATH_PROPERTY, "state.c2.p2",
	                                       NULL);

	templates = cpg_network_get_template_group (network);
	temp = cpg_group_find_object (templates, "t2");

	g_assert (temp);

	object = cpg_group_find_object (CPG_GROUP (network), "state");
	g_assert (object);

	property = cpg_group_find_property (CPG_GROUP (network), "state.p1");
	g_assert (property);
	g_assert_cmpstr (cpg_property_get_name (property), ==, "p1");

	cpg_object_unapply_template (object, temp, &error);
	g_assert_no_error (error);

	/* Interface from proxy should still be there */
	property = cpg_group_find_property (CPG_GROUP (network), "state.p1");
	g_assert (property);

	g_assert_cmpstr (cpg_property_get_name (property), ==, "p1");

	g_object_unref (network);
}

static void
test_templates_overrides_proxy_unapply_re_add ()
{
	CpgNetwork *network;
	CpgGroup *templates;
	CpgObject *temp;
	GError *error = NULL;
	CpgObject *object;
	CpgProperty *property;

	network = test_load_network_from_path ("test_interface_templates_override_proxy.cpg",
	                                       CPG_PATH_OBJECT, "state.c1",
	                                       CPG_PATH_OBJECT, "state.c2",
	                                       CPG_PATH_PROPERTY, "state.p1",
	                                       CPG_PATH_PROPERTY, "state.c1.p1",
	                                       CPG_PATH_PROPERTY, "state.c2.p2",
	                                       NULL);

	templates = cpg_network_get_template_group (network);
	temp = cpg_group_find_object (templates, "t1");

	g_assert (temp);

	object = cpg_group_find_object (CPG_GROUP (network), "state");
	g_assert (object);

	cpg_object_unapply_template (object, temp, &error);
	g_assert_no_error (error);

	/* Interface should now come from t2 */
	property = cpg_group_find_property (CPG_GROUP (network), "state.p1");
	g_assert (property);

	g_assert_cmpstr (cpg_property_get_name (property), ==, "p2");

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
	g_test_add_func ("/interface/templates", test_templates);
	g_test_add_func ("/interface/templates_overrides", test_templates_overrides);
	g_test_add_func ("/interface/templates_overrides_remove_uninherited", test_templates_overrides_remove_uninherited);
	g_test_add_func ("/interface/templates_overrides_remove_inherited", test_templates_overrides_remove_inherited);
	g_test_add_func ("/interface/templates_overrides_add_uninherited", test_templates_overrides_add_uninherited);
	g_test_add_func ("/interface/templates_overrides_add_inherited", test_templates_overrides_add_inherited);
	g_test_add_func ("/interface/templates_overrides_proxy", test_templates_overrides_proxy);
	g_test_add_func ("/interface/templates_overrides_proxy_unapply", test_templates_overrides_proxy_unapply);
	g_test_add_func ("/interface/templates_overrides_proxy_re_add", test_templates_overrides_proxy_unapply_re_add);

	g_test_run ();

	return 0;
}
