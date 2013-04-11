#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"node \"s1\"\n"
"{\n"
"  x = 0 | integrated in out once\n"
"  y = 0\n"
"  z = \"rand()\" | once\n"
"}\n"
"\n"
"edge \"edge\" from \"s1\" to \"s1\"\n"
"{\n"
"  x <= 1\n"
"  y <= t\n"
"}\n";

static gchar simple_node_xml[] = ""
"node \"node\" {"
"  node \"s1\" { x = 0 | integrated in out once }"
"  edge \"edge\" from \"s1\" to \"s1\" { x <= 1 }"
"}";

static void
test_load ()
{
	CdnNetwork *network;

	network = test_load_network (simple_xml,
	                             CDN_PATH_OBJECT, "s1",
	                             CDN_PATH_PROPERTY, "s1.x",
	                             CDN_PATH_OBJECT, "edge",
	                             CDN_PATH_ACTION, "edge.x",
	                             CDN_PATH_PROPERTY, "s1.y",
	                             CDN_PATH_ACTION, "edge.y",
	                             NULL);

	CdnVariable *variable = cdn_node_find_variable (CDN_NODE (network),
	                                                 "s1.x");

	g_assert_cmpint (cdn_variable_get_flags (variable),
	                 ==,
	                 CDN_VARIABLE_FLAG_IN |
	                 CDN_VARIABLE_FLAG_OUT |
	                 CDN_VARIABLE_FLAG_ONCE |
	                 CDN_VARIABLE_FLAG_INTEGRATED);

	cdn_assert_tol (cdn_variable_get_value (variable), 0);

	CdnEdgeAction *action = find_action (CDN_NODE (network), "edge.x");
	g_assert (cdn_edge_action_get_target_variable (action) == variable);

	g_object_unref (network);
}

static void
test_integrate_network (gchar const *xml,
                        gchar const *path,
                        gdouble      timestep,
                        gdouble     *values,
                        guint        size)
{
	CdnNetwork *network = cdn_network_new_from_string (xml, NULL);
	g_assert (network != NULL);

	g_assert (cdn_object_compile (CDN_OBJECT (network), NULL, NULL));

	CdnVariable *prop = cdn_node_find_variable (CDN_NODE (network), path);

	g_assert (prop != NULL);

	guint i;

	for (i = 0; i < size; ++i)
	{
		cdn_network_step (network, timestep);

		cdn_assert_tol (cdn_variable_get_value (prop),
		                values[i]);
	}

	g_object_unref (network);
}

static void
test_integrate ()
{
	gdouble values[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0};

	test_integrate_network (simple_xml,
	                        "s1.x",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_variadic ()
{
	CdnNetwork *network;

	network = cdn_network_new ();

	CdnVariable *prop = cdn_variable_new ("x",
	                                      cdn_expression_new ("rand()"),
	                                      CDN_VARIABLE_FLAG_ONCE);

	CdnVariable *other = cdn_variable_new ("y",
	                                       cdn_expression_new ("0"),
	                                       CDN_VARIABLE_FLAG_NONE);

	cdn_object_add_variable (CDN_OBJECT (network), prop, NULL);
	cdn_object_add_variable (CDN_OBJECT (network), other, NULL);

	CdnEdge *edge = cdn_edge_new ("edge",
	                              CDN_NODE (network),
	                              CDN_NODE (network));

	CdnEdgeAction *action = cdn_edge_action_new ("y", cdn_expression_new ("x"));
	cdn_edge_add_action (edge, action);

	cdn_node_add (CDN_NODE (network),
	               CDN_OBJECT (edge),
	               NULL);

	g_assert (cdn_object_compile (CDN_OBJECT (network), NULL, NULL));

	gdouble r = cdn_variable_get_value (prop);
	cdn_expression_reset_cache (cdn_variable_get_expression (prop));

	g_assert_cmpfloat (r, ==, cdn_variable_get_value (prop));

	cdn_network_step (network, 0.01);

	g_assert_cmpfloat (r, ==, cdn_variable_get_value (other));

	g_object_unref (network);
}

static void
test_direct ()
{
	gdouble values[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9};

	test_integrate_network (simple_xml,
	                        "s1.y",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_reset ()
{
	CdnNetwork *network = cdn_network_new_from_string (simple_xml, NULL);
	cdn_object_compile (CDN_OBJECT (network), NULL, NULL);

	CdnVariable *prop = cdn_node_find_variable (CDN_NODE (network),
	                                             "s1.x");

	cdn_network_run (network, 0, 0.1, 1, NULL);
	cdn_assert_tol (cdn_variable_get_value (prop), 1.0);

	cdn_object_reset (CDN_OBJECT (network));

	cdn_assert_tol (cdn_variable_get_value (prop), 0);
}

static void
test_once ()
{
	CdnNetwork *network = cdn_network_new_from_string (simple_xml, NULL);
	cdn_object_compile (CDN_OBJECT (network), NULL, NULL);

	CdnMonitor *monitor;
	CdnVariable *prop = cdn_node_find_variable (CDN_NODE (network),
	                                             "s1.z");

	monitor = cdn_monitor_new (network, prop);
	cdn_network_run (network, 0, 0.1, 0.3, NULL);

	guint size;
	const gdouble *data = cdn_monitor_get_data (monitor, &size);

	g_assert_cmpint ((gint)size, ==, 4);

	cdn_assert_tol (data[0], data[1]);
	cdn_assert_tol (data[1], data[2]);
}

static void
test_node_load ()
{
	CdnNetwork *network;

	network = test_load_network (simple_node_xml,
	                             CDN_PATH_OBJECT, "node.s1",
	                             CDN_PATH_PROPERTY, "node.s1.x",
	                             CDN_PATH_OBJECT, "node.edge",
	                             CDN_PATH_ACTION, "node.edge.x",
	                             NULL);

	CdnVariable *variable = cdn_node_find_variable (CDN_NODE (network),
	                                                 "node.s1.x");

	g_assert_cmpint (cdn_variable_get_flags (variable),
	                 ==,
	                 CDN_VARIABLE_FLAG_IN |
	                 CDN_VARIABLE_FLAG_OUT |
	                 CDN_VARIABLE_FLAG_ONCE |
	                 CDN_VARIABLE_FLAG_INTEGRATED);

	cdn_assert_tol (cdn_variable_get_value (variable), 0);

	CdnEdgeAction *action = find_action (CDN_NODE (network), "node.edge.x");
	g_assert (cdn_edge_action_get_target_variable (action) == variable);

	g_object_unref (network);
}

static void
test_node_integrate ()
{
	gdouble values[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0};

	test_integrate_network (simple_node_xml,
	                        "node.s1.x",
	                        0.1,
	                        values,
	                        G_N_ELEMENTS (values));
}

static void
test_node_reset ()
{
	CdnNetwork *network = cdn_network_new_from_string (simple_node_xml, NULL);
	cdn_object_compile (CDN_OBJECT (network), NULL, NULL);

	CdnVariable *prop = cdn_node_find_variable (CDN_NODE (network),
	                                             "node.s1.x");

	cdn_network_run (network, 0, 0.1, 1, NULL);
	cdn_object_reset (CDN_OBJECT (network));

	cdn_assert_tol (cdn_variable_get_value (prop), 0);
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/network/load", test_load);
	g_test_add_func ("/network/integrate", test_integrate);
	g_test_add_func ("/network/direct", test_direct);
	g_test_add_func ("/network/reset", test_reset);
	g_test_add_func ("/network/once", test_once);

	g_test_add_func ("/network/node/load", test_node_load);
	g_test_add_func ("/network/node/integrate", test_node_integrate);
	g_test_add_func ("/network/node/reset", test_node_reset);

	g_test_add_func ("/network/variadic", test_variadic);


	g_test_run ();

	return 0;
}
