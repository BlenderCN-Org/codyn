#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static void
test_add_child ()
{
	CdnNode *node = cdn_node_new ("id");
	CdnObject *child = CDN_OBJECT (cdn_node_new ("child"));

	cdn_node_add (node, child, NULL);

	g_assert (cdn_node_get_child (node, "child"));
}

static void
test_remove_child ()
{
	CdnNode *node = cdn_node_new ("id");
	CdnObject *child = CDN_OBJECT (cdn_node_new ("child"));

	g_assert (cdn_node_add (node, child, NULL));
	g_assert (cdn_node_remove (node, child, NULL));

	g_assert (cdn_node_get_child (node, "child") == NULL);
}

static void
test_clear ()
{
	CdnNode *node = cdn_node_new ("id");
	CdnObject *child = CDN_OBJECT (cdn_node_new ("child"));
	CdnObject *child2 = CDN_OBJECT (cdn_node_new ("child2"));

	g_assert (cdn_node_add (node, child, NULL));
	g_assert (cdn_node_add (node, child2, NULL));

	cdn_object_clear (CDN_OBJECT (node));

	g_assert (cdn_node_get_child (node, "child") == NULL);
	g_assert (cdn_node_get_child (node, "child2") == NULL);
}

static void
test_unique_id ()
{
	CdnNode *node = cdn_node_new ("id");
	CdnObject *child = CDN_OBJECT (cdn_node_new ("child"));
	CdnObject *child2 = CDN_OBJECT (cdn_node_new ("child"));

	g_assert (cdn_node_add (node, child, NULL));
	g_assert (cdn_node_add (node, child2, NULL));

	g_assert (cdn_node_get_child (node, "child"));
	g_assert (cdn_node_get_child (node, "child_1"));
}

static void
test_add_same ()
{
	CdnNode *node = cdn_node_new ("id");
	CdnObject *child = CDN_OBJECT (cdn_node_new ("child"));

	g_assert (cdn_node_add (node, child, NULL));
	g_assert (!cdn_node_add (node, child, NULL));

	g_assert (g_slist_length ((GSList *)cdn_node_get_children (node)) == 1);
}

static void
test_integrate_multiple_euler ()
{
	CdnNetwork *network;
	CdnVariable *prop;

	network = test_load_network_from_path ("test_node_integrate_multiple_euler.cdn", NULL);

	cdn_object_compile (CDN_OBJECT (network), NULL, NULL);

	cdn_network_step (network, 0.1);

	prop = cdn_node_find_variable (CDN_NODE (network), "node.x");

	g_assert (prop);
	cdn_assert_tol (cdn_variable_get_value (prop), 0.1);

	g_assert (network);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/node/add_child", test_add_child);
	g_test_add_func ("/node/remove_child", test_remove_child);
	g_test_add_func ("/node/clear", test_clear);
	g_test_add_func ("/node/unique_id", test_unique_id);
	g_test_add_func ("/node/add_same", test_add_same);
	g_test_add_func ("/node/integrate_multiple_euler", test_integrate_multiple_euler);

	g_test_run ();

	return 0;
}
