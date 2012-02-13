#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static gchar simple_xml[] = "node \"state\" { x = \"t\" last_x = \"delayed[x](0.1)\" ddt = \"delayed[x](dt)\" }";

static void
test_delayed ()
{
	CdnNetwork *network;

	network = test_load_network (simple_xml,
	                             CDN_PATH_OBJECT, "state",
	                             CDN_PATH_PROPERTY, "state.x",
	                             CDN_PATH_PROPERTY, "state.last_x",
	                             NULL);

	CdnVariable *x = cdn_node_find_variable (CDN_NODE (network), "state.x");
	CdnVariable *last_x = cdn_node_find_variable (CDN_NODE (network), "state.last_x");

	g_assert (x);
	g_assert (last_x);

	gdouble ts[] = {0, 0.1, 0.2, 0.3, 0.4};
	gdouble pts[] = {0, 0, 0.1, 0.2, 0.3, 0.4};
	gint i;

	cdn_network_begin (network, 0);

	for (i = 0; i < sizeof (ts) / sizeof (gdouble) - 1; ++i)
	{
		cdn_assert_tol (cdn_variable_get_value (x), ts[i]);
		cdn_assert_tol (cdn_variable_get_value (last_x), pts[i]);

		cdn_network_step (network, 0.1);
	}

	cdn_network_end (network);

	g_object_unref (network);
}

static void
test_delayed_dt ()
{
	CdnNetwork *network;

	network = test_load_network (simple_xml,
	                             CDN_PATH_OBJECT, "state",
	                             CDN_PATH_PROPERTY, "state.x",
	                             CDN_PATH_PROPERTY, "state.ddt",
	                             NULL);

	CdnVariable *x = cdn_node_find_variable (CDN_NODE (network), "state.x");
	CdnVariable *ddt = cdn_node_find_variable (CDN_NODE (network), "state.ddt");

	g_assert (x);
	g_assert (ddt);

	gdouble ts[] = {0, 0.1, 0.2, 0.3, 0.4};
	gdouble pts[] = {0, 0, 0.1, 0.2, 0.3, 0.4};
	gint i;

	for (i = 0; i < 4; ++i)
	{
		cdn_assert_tol (cdn_variable_get_value (x), ts[i]);
		cdn_assert_tol (cdn_variable_get_value (ddt), pts[i]);

		cdn_network_step (network, 0.1);
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

	g_test_add_func ("/operator/delayed", test_delayed);
	g_test_add_func ("/operator/delayed_dt", test_delayed_dt);

	g_test_run ();

	return 0;
}
