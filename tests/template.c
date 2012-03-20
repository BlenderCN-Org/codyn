#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"templates\n"
"{\n"
"  node \"template1\"\n"
"  {\n"
"    x = 0\n"
"    node \"nested1\"\n"
"    {\n"
"      y = 0\n"
"    }\n"
"  }\n"
"}\n"
"node \"state1\" : \"template1\"\n"
"{\n"
"}";

static void
test_load ()
{
	CdnNetwork *network;

	network = test_load_network (simple_xml,
	                             CDN_PATH_TEMPLATE_OBJECT, "template1",
	                             CDN_PATH_TEMPLATE_PROPERTY, "template1.x",
	                             CDN_PATH_TEMPLATE_OBJECT, "template1.nested1",
	                             CDN_PATH_TEMPLATE_PROPERTY, "template1.nested1.y",
	                             CDN_PATH_OBJECT, "state1",
	                             CDN_PATH_PROPERTY, "state1.x",
	                             CDN_PATH_OBJECT, "state1.nested1",
	                             CDN_PATH_PROPERTY, "state1.nested1.y",
	                             NULL);

	g_object_unref (network);
}

static void
test_apply_state ()
{
	CdnObject *state = CDN_OBJECT (cdn_node_new ("state", NULL));
	GError *error = NULL;

	cdn_object_add_variable (state,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	CdnObject *instance = cdn_object_new_from_template (state, &error);
	g_assert_no_error (error);

	CdnVariable *prop = cdn_object_get_variable (instance, "x");

	g_assert (prop);
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)), ==,
	                 "1");

	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_INTEGRATED);
}

static void
test_apply_edge ()
{
	CdnObject *edge = CDN_OBJECT (cdn_edge_new ("edge", NULL, NULL));
	GError *error = NULL;

	cdn_object_add_variable (edge,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	cdn_edge_add_action (CDN_EDGE (edge),
	                     cdn_edge_action_new ("x", cdn_expression_new ("2")));

	CdnObject *instance = cdn_object_new_from_template (edge, &error);
	g_assert_no_error (error);

	g_assert (CDN_IS_EDGE (instance));

	CdnVariable *prop = cdn_object_get_variable (instance, "x");

	g_assert (prop);
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)), ==,
	                 "1");

	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_INTEGRATED);

	CdnEdgeAction *action = cdn_edge_get_action (CDN_EDGE (edge), "x");

	g_assert (action);
	g_assert_cmpstr (cdn_edge_action_get_target (action), ==, "x");
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_edge_action_get_equation (action)), ==, "2");
}

static void
test_apply_node ()
{
	CdnNode *grp = cdn_node_new ("node", NULL);
	CdnObject *state = CDN_OBJECT (cdn_node_new ("state", NULL));

	cdn_object_add_variable (state,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("0"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	CdnObject *edge = CDN_OBJECT (cdn_edge_new ("edge",
	                                            CDN_NODE (state),
	                                            CDN_NODE (state)));
	cdn_edge_add_action (CDN_EDGE (edge),
	                     cdn_edge_action_new ("x",
	                                          cdn_expression_new ("1")));

	cdn_node_add (grp, state, NULL);
	cdn_node_add (grp, edge, NULL);

	cdn_node_set_proxy (grp, state);

	CdnObject *instance = cdn_object_copy (CDN_OBJECT (grp));

	g_assert (CDN_IS_NODE (instance));

	grp = CDN_NODE (instance);

	state = cdn_node_get_child (grp, "state");

	g_assert (state);
	g_assert (state == cdn_node_get_proxy (grp));

	g_assert (cdn_object_get_variable (state, "x"));

	edge = cdn_node_get_child (grp, "edge");

	g_assert (edge);

	g_assert (cdn_edge_get_action (CDN_EDGE (edge), "x"));
	g_assert (cdn_edge_get_input (CDN_EDGE (edge)) == CDN_NODE (state));
	g_assert (cdn_edge_get_output (CDN_EDGE (edge)) == CDN_NODE (state));
}

static void
test_apply_overload_state ()
{
	static gchar xml[] = ""
	"templates\n"
	"{\n"
	"  node \"state\"\n"
	"  {\n"
	"    x = 0 | integrated"
	"  }\n"
	"}\n"
	"node \"state\" : \"state\"\n"
	"{\n"
	"  x = 1 | -integrated"
	"}";

	CdnNetwork *network = cdn_network_new_from_string (xml, NULL);
	g_assert (network);

	CdnObject *obj = cdn_node_get_child (CDN_NODE (network), "state");
	g_assert (obj);

	CdnVariable *prop = cdn_object_get_variable (obj, "x");
	g_assert (prop);

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)),
	                 ==,
	                 "1");

	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_NONE);
}

static void
test_apply_overload_edge ()
{
	static gchar xml[] = ""
	"templates {"
	"  node \"state\" { x = 0 | integrated }"
	"  edge \"edge\" { x = 1 }"
	"}"
	"node \"state\" : \"state\" {}"
	"edge \"edge\" from \"state\" to \"state\" : \"edge\" { x <= 2 }";

	CdnNetwork *network = cdn_network_new_from_string (xml, NULL);
	g_assert (CDN_IS_NETWORK (network));

	CdnObject *obj = cdn_node_get_child (CDN_NODE (network), "edge");
	g_assert (CDN_IS_OBJECT (obj));

	CdnEdgeAction *action = cdn_edge_get_action (CDN_EDGE (obj), "x");
	g_assert (CDN_IS_EDGE_ACTION (action));

	CdnVariable *target = cdn_edge_action_get_target_variable (action);
	g_assert (CDN_IS_VARIABLE (target));

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_edge_action_get_equation (action)),
	                 ==,
	                 "2");
}

static void
test_apply_overload_node ()
{
	static gchar xml[] = ""
	"templates {"
	"  node \"node\" {"
	"    node \"state\" { x = 0 | integrated }"
	"    edge \"edge\" { x <= 1 }"
	"  }"
	"}"
	"node \"node\" : \"node\" {"
	"  node \"state\" { x = 2 }"
	"}";

	CdnNetwork *network = cdn_network_new_from_string (xml, NULL);
	g_assert (CDN_IS_NETWORK (network));

	CdnObject *obj = cdn_node_get_child (CDN_NODE (network), "node");
	g_assert (CDN_IS_NODE (obj));

	CdnNode *grp = CDN_NODE (obj);
	CdnObject *state = cdn_node_get_child (grp, "state");

	CdnVariable *prop = cdn_object_get_variable (state, "x");
	g_assert (CDN_IS_VARIABLE (prop));

	CdnExpression *expr = cdn_variable_get_expression (prop);
	g_assert_cmpstr (cdn_expression_get_as_string (expr), ==, "2");
	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_INTEGRATED);
}

static void
test_track_modified_state ()
{
	CdnObject *state = CDN_OBJECT (cdn_node_new ("state", NULL));
	GError *error = NULL;

	cdn_object_add_variable (state,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	CdnObject *instance = cdn_object_new_from_template (state, &error);
	cdn_object_set_id (instance, "instance");
	g_assert_no_error (error);

	CdnVariable *prop = cdn_object_get_variable (instance, "x");
	g_assert (prop);

	CdnVariable *orig = cdn_object_get_variable (state, "x");
	cdn_variable_set_expression (orig, cdn_expression_new ("5"));

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)), ==,
	                 "5");

	cdn_variable_set_expression (prop, cdn_expression_new ("2"));
	cdn_variable_set_expression (orig, cdn_expression_new ("3"));

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)), ==,
	                 "2");

	cdn_variable_set_expression (prop, cdn_expression_new ("3"));
	cdn_variable_set_expression (orig, cdn_expression_new ("5"));

	g_assert_cmpstr (cdn_expression_get_as_string (cdn_variable_get_expression (prop)), ==,
	                 "5");

	cdn_variable_set_flags (orig, CDN_VARIABLE_FLAG_IN);
	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_IN);

	cdn_variable_set_flags (prop, CDN_VARIABLE_FLAG_OUT);
	cdn_variable_set_flags (orig, CDN_VARIABLE_FLAG_INTEGRATED);

	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_OUT);

	cdn_variable_set_flags (prop, CDN_VARIABLE_FLAG_INTEGRATED);
	cdn_variable_set_flags (orig, CDN_VARIABLE_FLAG_NONE);

	g_assert_cmpint (cdn_variable_get_flags (prop), ==, CDN_VARIABLE_FLAG_NONE);
}

static void
test_track_modified_edge ()
{
	CdnObject *edge = CDN_OBJECT (cdn_edge_new ("edge", NULL, NULL));
	GError *error = NULL;

	cdn_object_add_variable (edge,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	cdn_edge_add_action (CDN_EDGE (edge),
	                     cdn_edge_action_new ("x", cdn_expression_new ("2")));

	CdnObject *instance = cdn_object_new_from_template (edge, &error);
	g_assert_no_error (error);

	g_assert (CDN_IS_EDGE (instance));

	CdnEdgeAction *orig = cdn_edge_get_action (CDN_EDGE (edge), "x");
	CdnEdgeAction *action = cdn_edge_get_action (CDN_EDGE (instance), "x");

	g_assert (CDN_IS_EDGE_ACTION (orig));
	g_assert (CDN_IS_EDGE_ACTION (action));

	cdn_edge_action_set_equation (orig, cdn_expression_new ("5"));
	g_assert_cmpstr (cdn_expression_get_as_string (cdn_edge_action_get_equation (action)),
	                 ==,
	                 "5");
}

static void
test_unapply_state ()
{
	CdnObject *state = CDN_OBJECT (cdn_node_new ("state", NULL));
	GError *error = NULL;

	cdn_object_add_variable (state,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	CdnObject *instance = cdn_object_new_from_template (state, &error);
	g_assert_no_error (error);

	cdn_object_unapply_template (instance, state, &error);

	g_assert_no_error (error);

	CdnVariable *prop = cdn_object_get_variable (instance, "x");
	g_assert (prop == NULL);
}

static void
test_unapply_modified_state ()
{
	CdnObject *state = CDN_OBJECT (cdn_node_new ("state", NULL));
	GError *error = NULL;

	cdn_object_add_variable (state,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           CDN_VARIABLE_FLAG_INTEGRATED),
	                         NULL);

	CdnObject *instance = cdn_object_new_from_template (state, &error);
	g_assert_no_error (error);

	CdnVariable *orig = cdn_object_get_variable (instance, "x");
	cdn_variable_set_expression (orig, cdn_expression_new ("5"));

	cdn_object_unapply_template (instance, state, &error);

	g_assert_no_error (error);

	CdnVariable *prop = cdn_object_get_variable (instance, "x");
	g_assert (CDN_IS_VARIABLE (prop));
}

static void
test_unapply_edge ()
{
	CdnEdge *edge = cdn_edge_new ("edge", NULL, NULL);
	GError *error = NULL;

	cdn_edge_add_action (edge, cdn_edge_action_new ("x", cdn_expression_new ("1")));

	CdnEdge *instance = CDN_EDGE (cdn_object_new_from_template (CDN_OBJECT (edge), &error));
	g_assert_no_error (error);

	cdn_object_unapply_template (CDN_OBJECT (instance), CDN_OBJECT (edge), &error);

	g_assert_no_error (error);

	CdnEdgeAction *action = cdn_edge_get_action (instance, "x");
	g_assert (action == NULL);
}

static void
test_unapply_modified_edge ()
{
	CdnEdge *edge = cdn_edge_new ("edge", NULL, NULL);
	GError *error = NULL;

	cdn_edge_add_action (edge, cdn_edge_action_new ("x", cdn_expression_new ("1")));

	CdnEdge *instance = CDN_EDGE (cdn_object_new_from_template (CDN_OBJECT (edge), &error));
	g_assert_no_error (error);

	CdnEdgeAction *action = cdn_edge_get_action (instance, "x");
	cdn_edge_action_set_equation (action, cdn_expression_new ("2"));

	cdn_object_unapply_template (CDN_OBJECT (instance), CDN_OBJECT (edge), &error);

	g_assert_no_error (error);

	action = cdn_edge_get_action (instance, "x");
	g_assert (action != NULL);
}

static void
test_apply_multiple_state ()
{
	CdnObject *t1 = CDN_OBJECT (cdn_node_new ("s1", NULL));
	CdnObject *t2 = CDN_OBJECT (cdn_node_new ("s2", NULL));
	GError *error = NULL;

	cdn_object_add_variable (t1,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           0),
	                         NULL);

	cdn_object_add_variable (t2,
	                         cdn_variable_new ("y",
	                                           cdn_expression_new ("2"),
	                                           0),
	                         NULL);

	CdnObject *o = CDN_OBJECT (cdn_node_new ("o1", NULL));

	cdn_object_apply_template (o, t1, &error);
	g_assert_no_error (error);

	cdn_object_apply_template (o, t2, &error);
	g_assert_no_error (error);

	CdnVariable *p1 = cdn_object_get_variable (o, "x");
	CdnVariable *p2 = cdn_object_get_variable (o, "y");

	g_assert (CDN_IS_VARIABLE (p1));
	g_assert (CDN_IS_VARIABLE (p2));
}

static void
test_apply_multiple_state_override ()
{
	CdnObject *t1 = CDN_OBJECT (cdn_node_new ("s1", NULL));
	CdnObject *t2 = CDN_OBJECT (cdn_node_new ("s2", NULL));
	GError *error = NULL;

	cdn_object_add_variable (t1,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           0),
	                         NULL);

	cdn_object_add_variable (t2,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("2"),
	                                           0),
	                         NULL);

	CdnObject *o = CDN_OBJECT (cdn_node_new ("o1", NULL));

	cdn_object_apply_template (o, t1, &error);
	g_assert_no_error (error);

	cdn_object_apply_template (o, t2, &error);
	g_assert_no_error (error);

	CdnVariable *p1 = cdn_object_get_variable (o, "x");
	g_assert (CDN_IS_VARIABLE (p1));

	CdnExpression *e = cdn_variable_get_expression (p1);
	g_assert_cmpstr (cdn_expression_get_as_string (e), ==, "2");
}

static void
test_unapply_multiple_state_override ()
{
	CdnObject *t1 = CDN_OBJECT (cdn_node_new ("s1", NULL));
	CdnObject *t2 = CDN_OBJECT (cdn_node_new ("s2", NULL));
	GError *error = NULL;

	cdn_object_add_variable (t1,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("1"),
	                                           0),
	                         NULL);

	cdn_object_add_variable (t2,
	                         cdn_variable_new ("x",
	                                           cdn_expression_new ("2"),
	                                           0),
	                         NULL);

	CdnObject *o = CDN_OBJECT (cdn_node_new ("o1", NULL));

	cdn_object_apply_template (o, t1, &error);
	g_assert_no_error (error);

	cdn_object_apply_template (o, t2, &error);
	g_assert_no_error (error);

	cdn_object_unapply_template (o, t1, &error);
	g_assert_no_error (error);

	CdnVariable *p1 = cdn_object_get_variable (o, "x");
	g_assert (CDN_IS_VARIABLE (p1));

	CdnExpression *e = cdn_variable_get_expression (p1);
	g_assert_cmpstr (cdn_expression_get_as_string (e), ==, "2");
}

static void
test_apply_multiple_edge ()
{
	CdnEdge *t1 = cdn_edge_new ("l1", NULL, NULL);
	CdnEdge *t2 = cdn_edge_new ("l2", NULL, NULL);
	GError *error = NULL;

	cdn_edge_add_action (t1, cdn_edge_action_new ("x", cdn_expression_new ("1")));
	cdn_edge_add_action (t2, cdn_edge_action_new ("y", cdn_expression_new ("2")));

	CdnEdge *l = cdn_edge_new ("o1", NULL, NULL);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t1), &error);
	g_assert_no_error (error);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t2), &error);
	g_assert_no_error (error);

	CdnEdgeAction *a1 = cdn_edge_get_action (l, "x");
	CdnEdgeAction *a2 = cdn_edge_get_action (l, "y");

	g_assert (CDN_IS_EDGE_ACTION (a1));
	g_assert (CDN_IS_EDGE_ACTION (a2));
}

static void
test_apply_multiple_edge_override ()
{
	CdnEdge *t1 = cdn_edge_new ("l1", NULL, NULL);
	CdnEdge *t2 = cdn_edge_new ("l2", NULL, NULL);
	GError *error = NULL;

	cdn_edge_add_action (t1, cdn_edge_action_new ("x", cdn_expression_new ("1")));
	cdn_edge_add_action (t2, cdn_edge_action_new ("x", cdn_expression_new ("2")));

	CdnEdge *l = cdn_edge_new ("o1", NULL, NULL);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t1), &error);
	g_assert_no_error (error);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t2), &error);
	g_assert_no_error (error);

	CdnEdgeAction *a1 = cdn_edge_get_action (l, "x");
	g_assert (CDN_IS_EDGE_ACTION (a1));

	CdnExpression *e = cdn_edge_action_get_equation (a1);

	g_assert_cmpstr (cdn_expression_get_as_string (e), ==, "2");
}

static void
test_unapply_multiple_edge_override ()
{
	CdnEdge *t1 = cdn_edge_new ("l1", NULL, NULL);
	CdnEdge *t2 = cdn_edge_new ("l2", NULL, NULL);
	GError *error = NULL;

	cdn_edge_add_action (t1, cdn_edge_action_new ("x", cdn_expression_new ("1")));
	cdn_edge_add_action (t2, cdn_edge_action_new ("x", cdn_expression_new ("2")));

	CdnEdge *l = cdn_edge_new ("o1", NULL, NULL);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t1), &error);
	g_assert_no_error (error);

	cdn_object_apply_template (CDN_OBJECT (l), CDN_OBJECT (t2), &error);
	g_assert_no_error (error);

	cdn_object_unapply_template (CDN_OBJECT (l), CDN_OBJECT (t1), &error);
	g_assert_no_error (error);

	CdnEdgeAction *a1 = cdn_edge_get_action (l, "x");
	g_assert (CDN_IS_EDGE_ACTION (a1));

	CdnExpression *e = cdn_edge_action_get_equation (a1);

	g_assert_cmpstr (cdn_expression_get_as_string (e), ==, "2");
}

int
main (int   argc,
      char *argv[])
{
	g_type_init ();
	g_test_init (&argc, &argv, NULL);

	g_type_init ();

	g_test_add_func ("/template/load", test_load);
	g_test_add_func ("/template/apply_state", test_apply_state);
	g_test_add_func ("/template/apply_edge", test_apply_edge);
	g_test_add_func ("/template/apply_node", test_apply_node);
	g_test_add_func ("/template/apply_overload_state", test_apply_overload_state);
	g_test_add_func ("/template/apply_overload_edge", test_apply_overload_edge);
	g_test_add_func ("/template/apply_overload_node", test_apply_overload_node);

	g_test_add_func ("/template/track_modified_state", test_track_modified_state);
	g_test_add_func ("/template/track_modified_edge", test_track_modified_edge);
	/* g_test_add_func ("/templates/track_modified_node", test_track_modified_node); */

	g_test_add_func ("/templates/unapply_state", test_unapply_state);
	g_test_add_func ("/templates/unapply_modified_state", test_unapply_modified_state);

	g_test_add_func ("/templates/unapply_edge", test_unapply_edge);
	g_test_add_func ("/templates/unapply_modified_edge", test_unapply_modified_edge);

	g_test_add_func ("/templates/apply_multiple_state", test_apply_multiple_state);
	g_test_add_func ("/templates/apply_multiple_state_override", test_apply_multiple_state_override);
	g_test_add_func ("/templates/unapply_multiple_state_override", test_unapply_multiple_state_override);

	g_test_add_func ("/templates/apply_multiple_edge", test_apply_multiple_edge);
	g_test_add_func ("/templates/apply_multiple_edge_override", test_apply_multiple_edge_override);
	g_test_add_func ("/templates/unapply_multiple_edge_override", test_unapply_multiple_edge_override);

	g_test_run ();

	return 0;
}
