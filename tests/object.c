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

	prop = cpg_property_new ("prop",
	                         cpg_expression_new ("0"),
	                         CPG_PROPERTY_FLAG_NONE);
	cpg_object_add_property (obj, prop, NULL);

	g_assert (prop != NULL);
	g_assert (cpg_object_get_property (obj, "prop") != NULL);
}

static void
test_remove_property ()
{
	CpgObject *obj = cpg_object_new ("id");
	CpgProperty *prop;

	prop = cpg_property_new ("prop",
	                         cpg_expression_new ("0"),
	                         CPG_PROPERTY_FLAG_NONE);
	cpg_object_add_property (obj, prop, NULL);
	g_assert (cpg_object_remove_property (obj, "prop", NULL));

	prop = cpg_object_get_property (obj, "prop");
	g_assert (prop == NULL);
}

static void
test_clear ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj,
	                         cpg_property_new ("p1",
	                                           cpg_expression_new ("0"),
	                                           CPG_PROPERTY_FLAG_NONE),
	                         NULL);

	cpg_object_add_property (obj,
	                         cpg_property_new ("p2",
	                                           cpg_expression_new ("0"),
	                                           CPG_PROPERTY_FLAG_NONE),
	                         NULL);

	cpg_object_clear (obj);

	g_assert (cpg_object_get_property (obj, "p1") == NULL);
	g_assert (cpg_object_get_property (obj, "p2") == NULL);
}

static void
test_copy ()
{
	CpgObject *obj = cpg_object_new ("id");

	cpg_object_add_property (obj,
	                         cpg_property_new ("p1",
	                                           cpg_expression_new ("0"),
	                                           CPG_PROPERTY_FLAG_INTEGRATED),
	                         NULL);

	cpg_object_add_property (obj,
	                         cpg_property_new ("p2",
	                                           cpg_expression_new ("1"),
	                                           CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT),
	                         NULL);

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
	GError *error = NULL;

	cpg_object_add_property (obj,
	                         cpg_property_new ("p1",
	                                           cpg_expression_new ("0"),
	                                           CPG_PROPERTY_FLAG_INTEGRATED),
	                         NULL);

	cpg_object_add_property (obj,
	                         cpg_property_new ("p2",
	                                           cpg_expression_new ("1"),
	                                           CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT),
	                         NULL);

	CpgObject *cp = cpg_object_new ("id2");
	cpg_object_apply_template (cp, obj, &error);
	g_assert_no_error (error);

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
	GError *error = NULL;

	cpg_object_add_property (obj,
	                         cpg_property_new ("p1",
	                                           cpg_expression_new ("0"),
	                                           CPG_PROPERTY_FLAG_INTEGRATED),
	                         NULL);

	cpg_object_add_property (obj,
	                         cpg_property_new ("p2",
	                                           cpg_expression_new ("1"),
	                                           CPG_PROPERTY_FLAG_IN | CPG_PROPERTY_FLAG_OUT),
	                         NULL);

	CpgObject *cp = cpg_object_new_from_template (obj, &error);
	g_assert_no_error (error);

	cpg_object_apply_template (cp, obj, &error);
	g_assert_no_error (error);

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
test_relative_id ()
{
	CpgGroup *g1;
	CpgGroup *g2;
	CpgObject *obj;

	g1 = cpg_group_new ("g1", NULL);
	g2 = cpg_group_new ("g2", NULL);

	obj = cpg_object_new ("o1");

	cpg_group_add (g1, CPG_OBJECT (g2), NULL);
	cpg_group_add (g2, obj, NULL);

	g_assert (cpg_group_find_object (g1, "g2"));
	g_assert (cpg_group_find_object (g2, "o1"));
	g_assert (cpg_group_find_object (g1, "g2.o1"));

	g_assert_cmpstr (cpg_object_get_relative_id (CPG_OBJECT (g2),
	                                             CPG_OBJECT (g1)), ==, "g2");

	g_assert_cmpstr (cpg_object_get_relative_id (obj,
	                                             CPG_OBJECT (g1)), ==, "g2.o1");

	g_assert_cmpstr (cpg_object_get_full_id (CPG_OBJECT (g2)), ==, "g2");
	g_assert_cmpstr (cpg_object_get_full_id (obj), ==, "g2.o1");

	g_object_unref (g1);
	g_object_unref (g2);
	g_object_unref (obj);
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
	g_test_add_func ("/object/relative_id", test_relative_id);

	g_test_run ();

	return 0;
}
