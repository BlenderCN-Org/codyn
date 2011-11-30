#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static void
test_create ()
{
	CdnObject *obj = cdn_object_new ("id");

	g_assert_cmpstr (cdn_object_get_id (obj), ==, "id");
	g_assert (cdn_object_get_variables (obj) == NULL);
}

static void
test_add_variable ()
{
	CdnObject *obj = cdn_object_new ("id");
	CdnVariable *prop;

	prop = cdn_variable_new ("prop",
	                         cdn_expression_new ("0"),
	                         CDN_VARIABLE_FLAG_NONE);
	cdn_object_add_variable (obj, prop, NULL);

	g_assert (prop != NULL);
	g_assert (cdn_object_get_variable (obj, "prop") != NULL);
}

static void
test_remove_variable ()
{
	CdnObject *obj = cdn_object_new ("id");
	CdnVariable *prop;

	prop = cdn_variable_new ("prop",
	                         cdn_expression_new ("0"),
	                         CDN_VARIABLE_FLAG_NONE);
	cdn_object_add_variable (obj, prop, NULL);
	g_assert (cdn_object_remove_variable (obj, "prop", NULL));

	prop = cdn_object_get_variable (obj, "prop");
	g_assert (prop == NULL);
}

static void
test_clear ()
{
	CdnObject *obj = cdn_object_new ("id");

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p1",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_NONE),
	                         NULL);

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p2",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_NONE),
	                         NULL);

	cdn_object_clear (obj);

	g_assert (cdn_object_get_variable (obj, "p1") == NULL);
	g_assert (cdn_object_get_variable (obj, "p2") == NULL);
}

static void
test_copy ()
{
	CdnObject *obj = cdn_object_new ("id");

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p1",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p2",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT),
	                         NULL);

	CdnObject *cp = cdn_object_copy (obj);
	CdnVariable *p1 = cdn_object_get_variable (cp, "p1");

	g_assert (p1);

	CdnVariable *p2 = cdn_object_get_variable (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cdn_variable_get_flags (p1), ==, CDN_VARIABLE_FLAG_INTEGRATED);
	g_assert_cmpuint (cdn_variable_get_flags (p2), ==, CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT);
}

static void
test_apply_template ()
{
	CdnObject *obj = cdn_object_new ("id");
	GError *error = NULL;

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p1",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p2",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT),
	                         NULL);

	CdnObject *cp = cdn_object_new ("id2");
	cdn_object_apply_template (cp, obj, &error);
	g_assert_no_error (error);

	CdnVariable *p1 = cdn_object_get_variable (cp, "p1");

	g_assert (p1);

	CdnVariable *p2 = cdn_object_get_variable (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cdn_variable_get_flags (p1), ==, CDN_VARIABLE_FLAG_INTEGRATED);
	g_assert_cmpuint (cdn_variable_get_flags (p2), ==, CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT);
}

static void
test_new_from_template ()
{
	CdnObject *obj = cdn_object_new ("id");
	GError *error = NULL;

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p1",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	cdn_object_add_variable (obj,
	                         cdn_variable_new ("p2",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT),
	                         NULL);

	CdnObject *cp = cdn_object_new_from_template (obj, &error);
	g_assert_no_error (error);

	cdn_object_apply_template (cp, obj, &error);
	g_assert_no_error (error);

	CdnVariable *p1 = cdn_object_get_variable (cp, "p1");

	g_assert (p1);

	CdnVariable *p2 = cdn_object_get_variable (cp, "p2");

	g_assert (p2);

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p1)), ==, "0");
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (p2)), ==, "1");

	g_assert_cmpuint (cdn_variable_get_flags (p1), ==, CDN_VARIABLE_FLAG_INTEGRATED);
	g_assert_cmpuint (cdn_variable_get_flags (p2), ==, CDN_VARIABLE_FLAG_IN | CDN_VARIABLE_FLAG_OUT);
}

static void
test_relative_id ()
{
	CdnNode *g1;
	CdnNode *g2;
	CdnObject *obj;

	g1 = cdn_node_new ("g1", NULL);
	g2 = cdn_node_new ("g2", NULL);

	obj = cdn_object_new ("o1");

	cdn_node_add (g1, CDN_OBJECT (g2), NULL);
	cdn_node_add (g2, obj, NULL);

	g_assert (cdn_node_find_object (g1, "g2"));
	g_assert (cdn_node_find_object (g2, "o1"));
	g_assert (cdn_node_find_object (g1, "g2.o1"));

	g_assert_cmpstr (cdn_object_get_relative_id (CDN_OBJECT (g2),
	                                             g1), ==, "g2");

	g_assert_cmpstr (cdn_object_get_relative_id (obj,
	                                             g1), ==, "g2.o1");

	g_assert_cmpstr (cdn_object_get_full_id (CDN_OBJECT (g2)), ==, "g2");
	g_assert_cmpstr (cdn_object_get_full_id (obj), ==, "g2.o1");

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
	g_test_add_func ("/object/add_variable", test_add_variable);
	g_test_add_func ("/object/remove_variable", test_remove_variable);
	g_test_add_func ("/object/clear", test_clear);
	g_test_add_func ("/object/copy", test_copy);
	g_test_add_func ("/object/apply_template", test_apply_template);
	g_test_add_func ("/object/new_from_template", test_new_from_template);
	g_test_add_func ("/object/relative_id", test_relative_id);

	g_test_run ();

	return 0;
}
