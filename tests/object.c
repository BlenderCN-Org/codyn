#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static void
test_create ()
{
	CpgObject *obj = cpg_object_new ("id");

	g_assert_cmpstr (cpg_object_get_id (obj), ==, "id");
	g_assert (cpg_object_get_properties (obj) == NULL);
	g_assert (cpg_object_get_actors (obj) == NULL);
}

static void
test_add_property ()
{
	CpgObject *obj = cpg_object_new ("id");
	CpgProperty *prop;

	prop = cpg_property_new ("prop", "0", CPG_PROPERTY_FLAG_NONE);
	cpg_object_add_property (obj, prop);

	g_assert (prop != NULL);
	g_assert (cpg_object_get_property (obj, "prop") != NULL);
}

static void
test_remove_property ()
{
	CpgObject *obj = cpg_object_new ("id");
	CpgProperty *prop;

	prop = cpg_property_new ("prop", "0", CPG_PROPERTY_FLAG_NONE);
	cpg_object_add_property (obj, prop);
	g_assert (cpg_object_remove_property (obj, "prop", NULL));

	prop = cpg_object_get_property (obj, "prop");
	g_assert (prop == NULL);
}

static void
test_clear ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_NONE));
	cpg_object_add_property (obj, cpg_property_new ("p2", "0", CPG_PROPERTY_FLAG_NONE));

	cpg_object_clear (obj);

	g_assert (cpg_object_get_property (obj, "p1") == NULL);
	g_assert (cpg_object_get_property (obj, "p2") == NULL);
}

static void
test_copy ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_INTEGRATED));
	cpg_object_add_property (obj, cpg_property_new ("p2", "1", CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT));

	CpgObject *cp = cpg_object_copy (obj);
	CpgProperty *p1 = cpg_object_get_property (cp, "p1");

	g_assert (p1);

	CpgProperty *p2 = cpg_object_get_property (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cpg_property_get_flags (p1), ==, CPG_PROPERTY_FLAG_INTEGRATED);
	g_assert_cmpuint (cpg_property_get_flags (p2), ==, CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT);
}

static void
test_apply_template ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_INTEGRATED));
	cpg_object_add_property (obj, cpg_property_new ("p2", "1", CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT));

	CpgObject *cp = cpg_object_new ("id2");
	cpg_object_apply_template (cp, obj);

	CpgProperty *p1 = cpg_object_get_property (cp, "p1");

	g_assert (p1);

	CpgProperty *p2 = cpg_object_get_property (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cpg_property_get_flags (p1), ==, CPG_PROPERTY_FLAG_INTEGRATED);
	g_assert_cmpuint (cpg_property_get_flags (p2), ==, CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT);
}

static void
test_new_from_template ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj, cpg_property_new ("p1", "0", CPG_PROPERTY_FLAG_INTEGRATED));
	cpg_object_add_property (obj, cpg_property_new ("p2", "1", CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT));

	CpgObject *cp = cpg_object_new_from_template (obj);
	cpg_object_apply_template (cp, obj);

	CpgProperty *p1 = cpg_object_get_property (cp, "p1");

	g_assert (p1);

	CpgProperty *p2 = cpg_object_get_property (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cpg_property_get_flags (p1), ==, CPG_PROPERTY_FLAG_INTEGRATED);
	g_assert_cmpuint (cpg_property_get_flags (p2), ==, CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT);
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/object/create", test_create);
	g_test_add_func ("/object/add_property", test_add_property);
	g_test_add_func ("/object/remove_property", test_remove_property);
	g_test_add_func ("/object/clear", test_clear);
	g_test_add_func ("/object/copy", test_copy);
	g_test_add_func ("/object/apply_template", test_apply_template);
	g_test_add_func ("/object/new_from_template", test_new_from_template);

	g_test_run ();

	return 0;
}
