#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static void
test_add_child ()
{
	CpgGroup *group = cpg_group_new ("id", NULL);
	CpgObject *child = cpg_object_new ("child");

	cpg_group_add (group, child, NULL);

	g_assert (cpg_group_get_child (group, "child"));
}

static void
test_remove_child ()
{
	CpgGroup *group = cpg_group_new ("id", NULL);
	CpgObject *child = cpg_object_new ("child");

	g_assert (cpg_group_add (group, child, NULL));
	g_assert (cpg_group_remove (group, child, NULL));

	g_assert (cpg_group_get_child (group, "child") == NULL);
}

static void
test_clear ()
{
	CpgGroup *group = cpg_group_new ("id", NULL);
	CpgObject *child = cpg_object_new ("child");
	CpgObject *child2 = cpg_object_new ("child2");

	g_assert (cpg_group_add (group, child, NULL));
	g_assert (cpg_group_add (group, child2, NULL));

	cpg_object_clear (CPG_OBJECT (group));

	g_assert (cpg_group_get_child (group, "child") == NULL);
	g_assert (cpg_group_get_child (group, "child2") == NULL);
}

static void
test_unique_id ()
{
	CpgGroup *group = cpg_group_new ("id", NULL);
	CpgObject *child = cpg_object_new ("child");
	CpgObject *child2 = cpg_object_new ("child");

	g_assert (cpg_group_add (group, child, NULL));
	g_assert (cpg_group_add (group, child2, NULL));

	g_assert (cpg_group_get_child (group, "child"));
	g_assert (cpg_group_get_child (group, "child_1"));
}

static void
test_add_same ()
{
	CpgGroup *group = cpg_group_new ("id", NULL);
	CpgObject *child = cpg_object_new ("child");

	g_assert (cpg_group_add (group, child, NULL));
	g_assert (!cpg_group_add (group, child, NULL));

	g_assert (g_slist_length ((GSList *)cpg_group_get_children (group)) == 1);
}

static void
test_proxy ()
{
	CpgObject *proxy = cpg_object_new ("proxy");

	cpg_object_add_property (proxy, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_NONE), NULL);

	CpgGroup *group = cpg_group_new ("id", proxy);

	g_assert (cpg_group_get_child (group, "proxy"));

	CpgProperty *p1 = cpg_object_get_property (CPG_OBJECT (group), "p1");

	g_assert (p1);

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p1)), ==, "0");
}

static void
test_copy ()
{
	CpgObject *proxy = cpg_object_new ("proxy");

	cpg_object_add_property (proxy, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_NONE), NULL);

	CpgGroup *group = cpg_group_new ("id", proxy);

	CpgObject *child = cpg_object_new ("child");
	cpg_group_add (group, child, NULL);

	CpgGroup *copy = CPG_GROUP (cpg_object_copy (CPG_OBJECT (group)));

	proxy = cpg_group_get_proxy (copy);

	g_assert (proxy);
	g_assert_cmpstr (cpg_object_get_id (proxy), ==, "proxy");

	g_assert (cpg_object_get_property (proxy, "p1"));

	g_assert (cpg_group_get_child (copy, "child"));
}

static void
test_integrate_multiple_euler ()
{
	CpgNetwork *network;
	CpgProperty *prop;

	network = cpg_network_new_from_path ("test_group_integrate_multiple_euler.cpg", NULL);

	cpg_object_compile (CPG_OBJECT (network), NULL, NULL);

	cpg_network_step (network, 0.1);

	prop = cpg_group_find_property (CPG_GROUP (network), "group.x");

	cpg_assert_tol (cpg_property_get_value (prop), 0.1);

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

	g_test_add_func ("/group/add_child", test_add_child);
	g_test_add_func ("/group/remove_child", test_remove_child);
	g_test_add_func ("/group/clear", test_clear);
	g_test_add_func ("/group/unique_id", test_unique_id);
	g_test_add_func ("/group/add_same", test_add_same);
	g_test_add_func ("/group/proxy", test_proxy);
	g_test_add_func ("/group/copy", test_copy);
	g_test_add_func ("/group/integrate_multiple_euler", test_integrate_multiple_euler);

	g_test_run ();

	return 0;
}