#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <state id=\"state\">\n"
"      <property name=\"x\" integrated=\"yes\" in=\"yes\" out=\"yes\" once=\"yes\">0</property>\n"
"      <property name=\"y\">0</property>\n"
"      <property name=\"z\" once=\"yes\">rand()</property>\n"
"    </state>\n"
"    <link id=\"link\" from=\"state\" to=\"state\">\n"
"      <action target=\"x\">1</action>\n"
"      <action target=\"y\">t</action>"
"    </link>\n"
"  </network>\n"
"</cpg>\n";

static gchar simple_group_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <state id=\"group\">\n"
"      <state id=\"state\">\n"
"        <property name=\"x\" flags=\"integrated | in | out | once\">0</property>\n"
"      </state>\n"
"      <link id=\"link\" from=\"state\" to=\"state\">\n"
"        <action target=\"x\">1</action>\n"
"      </link>\n"
"    </state>\n"
"  </network>\n"
"</cpg>\n";

static void
test_load ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_OBJECT, "state",
	                             CPG_PATH_PROPERTY, "state.x",
	                             CPG_PATH_OBJECT, "link",
	                             CPG_PATH_ACTION, "link.x",
	                             CPG_PATH_PROPERTY, "state.y",
	                             CPG_PATH_ACTION, "link.y",
	                             NULL);

	CpgProperty *property = cpg_group_find_property (CPG_GROUP (network),
	                                                 "state.x");

	g_assert_cmpint (cpg_property_get_flags (property),
	                 ==,
	                 CPG_PROPERTY_FLAG_IN |
	                 CPG_PROPERTY_FLAG_OUT |
	                 CPG_PROPERTY_FLAG_ONCE |
	                 CPG_PROPERTY_FLAG_INTEGRATED);

	cpg_assert_tol (cpg_property_get_value (property), 0);

	CpgLinkAction *action = find_action (CPG_GROUP (network), "link.x");
	g_assert (cpg_link_action_get_target_property (action) == property);

	g_object_unref (network);
}

static void
test_integrate_network (gchar const *xml,
                        gchar const *path,
                        gdouble      timestep,
                        gdouble     *values,
                        guint        size)
{
	CpgNetwork *network = cpg_network_new_from_xml (xml, NULL);
	g_assert (network != NULL);

	g_assert (cpg_object_compile (CPG_OBJECT (network), NULL, NULL));

	CpgProperty *prop = cpg_group_find_property (CPG_GROUP (network), path);

	g_assert (prop != NULL);

	guint i;

	for (i = 0; i < size; ++i)
	{
		cpg_network_step (network, timestep);

		cpg_assert_tol (cpg_property_get_value (prop),
		                values[i]);
	}

	g_object_unref (network);
}

static void
test_integrate ()
{
	gdouble values[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0};

	test_integrate_network (simple_xml,
	                        "state.x",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_variadic ()
{
	CpgNetwork *network;

	srand (time (0));

	network = cpg_network_new ();

	CpgProperty *prop = cpg_property_new ("x", "rand()", CPG_PROPERTY_FLAG_NONE);
	CpgProperty *other = cpg_property_new ("y", "0", CPG_PROPERTY_FLAG_NONE);

	cpg_object_add_property (CPG_OBJECT (network), prop);
	cpg_object_add_property (CPG_OBJECT (network), other);

	CpgLink *link = cpg_link_new ("link",
	                              CPG_OBJECT (network),
	                              CPG_OBJECT (network));

	CpgLinkAction *action = cpg_link_action_new ("y", cpg_expression_new ("x"));
	cpg_link_add_action (link, action);

	cpg_group_add (CPG_GROUP (network),
	               CPG_OBJECT (link),
	               NULL);

	g_assert (cpg_object_compile (CPG_OBJECT (network), NULL, NULL));

	gdouble r = cpg_property_get_value (prop);
	cpg_expression_reset_cache (cpg_property_get_expression (prop));

	g_assert_cmpfloat (r, ==, cpg_property_get_value (prop));

	cpg_network_step (network, 0.01);

	g_assert_cmpfloat (cpg_property_get_value (prop), ==, cpg_property_get_value (other));

	g_object_unref (network);
}

static void
test_direct ()
{
	gdouble values[] = {0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9};

	test_integrate_network (simple_xml,
	                        "state.y",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_reset ()
{
	CpgNetwork *network = cpg_network_new_from_xml (simple_xml, NULL);
	cpg_object_compile (CPG_OBJECT (network), NULL, NULL);

	CpgProperty *prop = cpg_group_find_property (CPG_GROUP (network),
	                                             "state.x");

	cpg_network_run (network, 0, 0.1, 1);
	cpg_assert_tol (cpg_property_get_value (prop), 1.1);

	cpg_object_reset (CPG_OBJECT (network));

	cpg_assert_tol (cpg_property_get_value (prop), 0);
}

static void
test_recompile ()
{
	CpgNetwork *network = cpg_network_new_from_xml (simple_xml, NULL);
	cpg_object_compile (CPG_OBJECT (network), NULL, NULL);

	CpgProperty *prop = cpg_group_find_property (CPG_GROUP (network),
	                                             "state.x");
	CpgExpression *expr = cpg_property_get_expression (prop);

	cpg_network_run (network, 0, 0.1, 1);
	cpg_expression_set_from_string (expr, "5");

	cpg_network_run (network, 0, 0.1, 1);

	cpg_assert_tol (cpg_property_get_value (prop), 6.1);
}

static void
test_once ()
{
	CpgNetwork *network = cpg_network_new_from_xml (simple_xml, NULL);
	cpg_object_compile (CPG_OBJECT (network), NULL, NULL);

	CpgMonitor *monitor;
	CpgProperty *prop = cpg_group_find_property (CPG_GROUP (network),
	                                             "state.z");

	monitor = cpg_monitor_new (network, prop);
	cpg_network_run (network, 0, 0.1, 0.3);

	guint size;
	const gdouble *data = cpg_monitor_get_data (monitor, &size);

	g_assert_cmpint ((gint)size, =, 3);

	cpg_assert_tol (data[0], data[1]);
	cpg_assert_tol (data[1], data[2]);
}

static void
test_group_load ()
{
	CpgNetwork *network;

	network = test_load_network (simple_group_xml,
	                             CPG_PATH_OBJECT, "group.state",
	                             CPG_PATH_PROPERTY, "group.state.x",
	                             CPG_PATH_OBJECT, "group.link",
	                             CPG_PATH_ACTION, "group.link.x",
	                             NULL);

	CpgProperty *property = cpg_group_find_property (CPG_GROUP (network),
	                                                 "group.state.x");

	g_assert_cmpint (cpg_property_get_flags (property),
	                 ==,
	                 CPG_PROPERTY_FLAG_IN |
	                 CPG_PROPERTY_FLAG_OUT |
	                 CPG_PROPERTY_FLAG_ONCE |
	                 CPG_PROPERTY_FLAG_INTEGRATED);

	cpg_assert_tol (cpg_property_get_value (property), 0);

	CpgLinkAction *action = find_action (CPG_GROUP (network), "group.link.x");
	g_assert (cpg_link_action_get_target_property (action) == property);

	g_object_unref (network);
}

static void
test_group_integrate ()
{
	gdouble values[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0};

	test_integrate_network (simple_group_xml,
	                        "group.state.x",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_group_reset ()
{
	CpgNetwork *network = cpg_network_new_from_xml (simple_group_xml, NULL);
	cpg_object_compile (CPG_OBJECT (network), NULL, NULL);

	CpgProperty *prop = cpg_group_find_property (CPG_GROUP (network),
	                                             "group.state.x");

	cpg_network_run (network, 0, 0.1, 1);
	cpg_object_reset (CPG_OBJECT (network));

	cpg_assert_tol (cpg_property_get_value (prop), 0);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/network/load", test_load);
	g_test_add_func ("/network/integrate", test_integrate);
	g_test_add_func ("/network/direct", test_direct);
	g_test_add_func ("/network/reset", test_reset);
	g_test_add_func ("/network/recompile", test_recompile);
	g_test_add_func ("/network/once", test_once);

	g_test_add_func ("/network/group/load", test_group_load);
	g_test_add_func ("/network/group/integrate", test_group_integrate);
	g_test_add_func ("/network/group/reset", test_group_reset);

	g_test_add_func ("/network/variadic", test_variadic);


	g_test_run ();

	return 0;
}
