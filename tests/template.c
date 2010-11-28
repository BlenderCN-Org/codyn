#include <cpg-network/cpg-network.h>
#include <cpg-network/cpg-expression.h>
#include <cpg-network/cpg-object.h>

#include "utils.h"

static gchar simple_xml[] = ""
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<cpg>\n"
"  <network>\n"
"    <templates>\n"
"      <state id=\"template1\">\n"
"        <property name=\"x\">0</property>\n"
"\n"
"        <state id=\"nested1\">\n"
"          <property name=\"y\">0</property>\n"
"        </state>\n"
"      </state>\n"
"    </templates>\n"
"\n"
"    <state id=\"state1\" ref=\"template1\"/>\n"
"  </network>\n"
"</cpg>\n";

static void
test_load ()
{
	CpgNetwork *network;

	network = test_load_network (simple_xml,
	                             CPG_PATH_TEMPLATE_OBJECT, "template1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "template1.x",
	                             CPG_PATH_TEMPLATE_OBJECT, "template1.nested1",
	                             CPG_PATH_TEMPLATE_PROPERTY, "template1.nested1.y",
	                             CPG_PATH_OBJECT, "state1",
	                             CPG_PATH_PROPERTY, "state1.x",
	                             CPG_PATH_OBJECT, "state1.nested1",
	                             CPG_PATH_PROPERTY, "state1.nested1.y",
	                             NULL);

	g_object_unref (network);
}

static void
test_apply_state ()
{
	CpgObject *state = cpg_object_new ("state");
	cpg_object_add_property (state,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	CpgObject *instance = cpg_object_new_from_template (state);

	CpgProperty *prop = cpg_object_get_property (instance, "x");

	g_assert (prop);
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)), ==,
	                 "1");

	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_INTEGRATED);
}

static void
test_apply_link ()
{
	CpgObject *link = CPG_OBJECT (cpg_link_new ("link", NULL, NULL));
	cpg_object_add_property (link,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	cpg_link_add_action (CPG_LINK (link),
	                     cpg_link_action_new ("x", cpg_expression_new ("2")));

	CpgObject *instance = cpg_object_new_from_template (link);

	g_assert (CPG_IS_LINK (instance));

	CpgProperty *prop = cpg_object_get_property (instance, "x");

	g_assert (prop);
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)), ==,
	                 "1");

	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_INTEGRATED);

	CpgLinkAction *action = cpg_link_get_action (CPG_LINK (link), "x");

	g_assert (action);
	g_assert_cmpstr (cpg_link_action_get_target (action), ==, "x");
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_link_action_get_equation (action)), ==, "2");
}

static void
test_apply_group ()
{
	CpgGroup *grp = cpg_group_new ("group", NULL);
	CpgObject *state = cpg_object_new ("state");

	cpg_object_add_property (state,
	                         cpg_property_new ("x", "0", CPG_PROPERTY_FLAG_INTEGRATED));

	CpgObject *link = CPG_OBJECT (cpg_link_new ("link", state, state));
	cpg_link_add_action (CPG_LINK (link),
	                     cpg_link_action_new ("x",
	                                          cpg_expression_new ("1")));

	cpg_group_add (grp, state, NULL);
	cpg_group_add (grp, link, NULL);

	cpg_group_set_proxy (grp, state);

	CpgObject *instance = cpg_object_copy (CPG_OBJECT (grp));

	g_assert (CPG_IS_GROUP (instance));

	grp = CPG_GROUP (instance);

	state = cpg_group_get_child (grp, "state");

	g_assert (state);
	g_assert (state == cpg_group_get_proxy (grp));

	g_assert (cpg_object_get_property (state, "x"));

	link = cpg_group_get_child (grp, "link");

	g_assert (link);

	g_assert (cpg_link_get_action (CPG_LINK (link), "x"));
	g_assert (cpg_link_get_from (CPG_LINK (link)) == state);
	g_assert (cpg_link_get_to (CPG_LINK (link)) == state);
}

static void
test_apply_overload_state ()
{
	static gchar xml[] = ""
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<cpg>\n"
	"  <network>\n"
	"    <templates>\n"
	"      <state id=\"state\">\n"
	"        <property name=\"x\" integrated=\"yes\">0</property>\n"
	"\n"
	"      </state>\n"
	"    </templates>\n"
	"\n"
	"    <state id=\"state\" ref=\"state\">\n"
	"      <property name=\"x\">1</property>\n"
	"    </state>\n"
	"  </network>\n"
	"</cpg>\n";

	CpgNetwork *network = cpg_network_new_from_xml (xml, NULL);
	g_assert (network);

	CpgObject *obj = cpg_group_get_child (CPG_GROUP (network), "state");
	g_assert (obj);

	CpgProperty *prop = cpg_object_get_property (obj, "x");
	g_assert (prop);

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)),
	                 ==,
	                 "1");

	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_NONE);
}

static void
test_apply_overload_link ()
{
	static gchar xml[] = ""
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<cpg>\n"
	"  <network>\n"
	"    <templates>\n"
	"      <state id=\"state\">\n"
	"        <property name=\"x\" integrated=\"yes\">0</property>\n"
	"\n"
	"      </state>\n"
	"      <link id=\"link\">\n"
	"        <action target=\"x\">1</action>\n"
	"      </link>\n"
	"    </templates>\n"
	"\n"
	"    <state id=\"state\" ref=\"state\"/>\n"
	"    <link id=\"link\" ref=\"link\" from=\"state\" to=\"state\">\n"
	"      <action target=\"x\">2</action>"
	"    </link>\n"
	"  </network>\n"
	"</cpg>\n";

	CpgNetwork *network = cpg_network_new_from_xml (xml, NULL);
	g_assert (CPG_IS_NETWORK (network));

	CpgObject *obj = cpg_group_get_child (CPG_GROUP (network), "link");
	g_assert (CPG_IS_OBJECT (obj));

	CpgLinkAction *action = cpg_link_get_action (CPG_LINK (obj), "x");
	g_assert (CPG_IS_LINK_ACTION (action));

	CpgProperty *target = cpg_link_action_get_target_property (action);
	g_assert (CPG_IS_PROPERTY (target));

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_link_action_get_equation (action)),
	                 ==,
	                 "2");
}

static void
test_apply_overload_group ()
{
	static gchar xml[] = ""
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"<cpg>\n"
	"  <network>\n"
	"    <templates>\n"
	"      <state id=\"group\">\n"
	"        <state id=\"state\">\n"
	"          <property name=\"x\" integrated=\"yes\">0</property>\n"
	"\n"
	"        </state>\n"
	"        <link id=\"link\">\n"
	"          <action target=\"x\">1</action>\n"
	"        </link>\n"
	"      </state>\n"
	"    </templates>\n"
	"\n"
	"    <state id=\"state\" ref=\"group\">\n"
	"      <state id=\"state\">\n"
	"        <property name=\"x\">2</property>\n"
	"      </state>\n"
	"    </state>\n"
	"  </network>\n"
	"</cpg>\n";

	CpgNetwork *network = cpg_network_new_from_xml (xml, NULL);
	g_assert (CPG_IS_NETWORK (network));

	CpgObject *obj = cpg_group_get_child (CPG_GROUP (network), "state");
	g_assert (CPG_IS_GROUP (obj));

	CpgGroup *grp = CPG_GROUP (obj);
	CpgObject *state = cpg_group_get_child (grp, "state");

	CpgProperty *prop = cpg_object_get_property (state, "x");
	g_assert (CPG_IS_PROPERTY (prop));

	CpgExpression *expr = cpg_property_get_expression (prop);
	g_assert_cmpstr (cpg_expression_get_as_string (expr), ==, "2");
	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_NONE);
}

static void
test_track_modified_state ()
{
	CpgObject *state = cpg_object_new ("state");
	cpg_object_add_property (state,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	CpgObject *instance = cpg_object_new_from_template (state);

	CpgProperty *prop = cpg_object_get_property (instance, "x");
	g_assert (prop);

	CpgProperty *orig = cpg_object_get_property (state, "x");
	cpg_property_set_expression (orig, cpg_expression_new ("5"));

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)), ==,
	                 "5");

	cpg_property_set_expression (prop, cpg_expression_new ("2"));
	cpg_property_set_expression (orig, cpg_expression_new ("3"));

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)), ==,
	                 "2");

	cpg_property_set_expression (prop, cpg_expression_new ("3"));
	cpg_property_set_expression (orig, cpg_expression_new ("5"));

	g_assert_cmpstr (cpg_expression_get_as_string (cpg_property_get_expression (prop)), ==,
	                 "5");

	cpg_property_set_flags (orig, CPG_PROPERTY_FLAG_IN);
	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_IN);

	cpg_property_set_flags (prop, CPG_PROPERTY_FLAG_OUT);
	cpg_property_set_flags (orig, CPG_PROPERTY_FLAG_INTEGRATED);

	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_OUT);

	cpg_property_set_flags (prop, CPG_PROPERTY_FLAG_INTEGRATED);
	cpg_property_set_flags (orig, CPG_PROPERTY_FLAG_NONE);

	g_assert_cmpint (cpg_property_get_flags (prop), ==, CPG_PROPERTY_FLAG_NONE);
}

static void
test_track_modified_link ()
{
	CpgObject *link = CPG_OBJECT (cpg_link_new ("link", NULL, NULL));
	cpg_object_add_property (link,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	cpg_link_add_action (CPG_LINK (link),
	                     cpg_link_action_new ("x", cpg_expression_new ("2")));

	CpgObject *instance = cpg_object_new_from_template (link);
	g_assert (CPG_IS_LINK (instance));

	CpgLinkAction *orig = cpg_link_get_action (CPG_LINK (link), "x");
	CpgLinkAction *action = cpg_link_get_action (CPG_LINK (instance), "x");

	g_assert (CPG_IS_LINK_ACTION (orig));
	g_assert (CPG_IS_LINK_ACTION (action));

	cpg_link_action_set_equation (orig, cpg_expression_new ("5"));
	g_assert_cmpstr (cpg_expression_get_as_string (cpg_link_action_get_equation (action)),
	                 ==,
	                 "5");
}

static void
test_unapply_state ()
{
	CpgObject *state = cpg_object_new ("state");
	cpg_object_add_property (state,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	CpgObject *instance = cpg_object_new_from_template (state);

	cpg_object_unapply_template (instance, state);

	CpgProperty *prop = cpg_object_get_property (instance, "x");
	g_assert (prop == NULL);
}

static void
test_unapply_modified_state ()
{
	CpgObject *state = cpg_object_new ("state");
	cpg_object_add_property (state,
	                         cpg_property_new ("x", "1", CPG_PROPERTY_FLAG_INTEGRATED));

	CpgObject *instance = cpg_object_new_from_template (state);

	CpgProperty *orig = cpg_object_get_property (instance, "x");
	cpg_property_set_expression (orig, cpg_expression_new ("5"));

	cpg_object_unapply_template (instance, state);

	CpgProperty *prop = cpg_object_get_property (instance, "x");
	g_assert (CPG_IS_PROPERTY (prop));
}

static void
test_unapply_link ()
{
	CpgLink *link = cpg_link_new ("link", NULL, NULL);
	cpg_link_add_action (link, cpg_link_action_new ("x", cpg_expression_new ("1")));

	CpgLink *instance = CPG_LINK (cpg_object_new_from_template (CPG_OBJECT (link)));

	cpg_object_unapply_template (CPG_OBJECT (instance), CPG_OBJECT (link));

	CpgLinkAction *action = cpg_link_get_action (instance, "x");
	g_assert (action == NULL);
}

static void
test_unapply_modified_link ()
{
	CpgLink *link = cpg_link_new ("link", NULL, NULL);
	cpg_link_add_action (link, cpg_link_action_new ("x", cpg_expression_new ("1")));

	CpgLink *instance = CPG_LINK (cpg_object_new_from_template (CPG_OBJECT (link)));

	CpgLinkAction *action = cpg_link_get_action (instance, "x");
	cpg_link_action_set_equation (action, cpg_expression_new ("2"));

	cpg_object_unapply_template (CPG_OBJECT (instance), CPG_OBJECT (link));

	action = cpg_link_get_action (instance, "x");
	g_assert (action != NULL);
}

static void
test_apply_multiple_state ()
{
	CpgObject *t1 = cpg_object_new ("s1");
	CpgObject *t2 = cpg_object_new ("s2");

	cpg_object_add_property (t1, cpg_property_new ("x", "1", 0));
	cpg_object_add_property (t2, cpg_property_new ("y", "2", 0));

	CpgObject *o = cpg_object_new ("o1");

	cpg_object_apply_template (o, t1);
	cpg_object_apply_template (o, t2);

	CpgProperty *p1 = cpg_object_get_property (o, "x");
	CpgProperty *p2 = cpg_object_get_property (o, "y");

	g_assert (CPG_IS_PROPERTY (p1));
	g_assert (CPG_IS_PROPERTY (p2));
}

static void
test_apply_multiple_state_override ()
{
	CpgObject *t1 = cpg_object_new ("s1");
	CpgObject *t2 = cpg_object_new ("s2");

	cpg_object_add_property (t1, cpg_property_new ("x", "1", 0));
	cpg_object_add_property (t2, cpg_property_new ("x", "2", 0));

	CpgObject *o = cpg_object_new ("o1");

	cpg_object_apply_template (o, t1);
	cpg_object_apply_template (o, t2);

	CpgProperty *p1 = cpg_object_get_property (o, "x");
	g_assert (CPG_IS_PROPERTY (p1));

	CpgExpression *e = cpg_property_get_expression (p1);
	g_assert_cmpstr (cpg_expression_get_as_string (e), ==, "2");
}

static void
test_unapply_multiple_state_override ()
{
	CpgObject *t1 = cpg_object_new ("s1");
	CpgObject *t2 = cpg_object_new ("s2");

	cpg_object_add_property (t1, cpg_property_new ("x", "1", 0));
	cpg_object_add_property (t2, cpg_property_new ("x", "2", 0));

	CpgObject *o = cpg_object_new ("o1");

	cpg_object_apply_template (o, t1);
	cpg_object_apply_template (o, t2);

	cpg_object_unapply_template (o, t1);

	CpgProperty *p1 = cpg_object_get_property (o, "x");
	g_assert (CPG_IS_PROPERTY (p1));

	CpgExpression *e = cpg_property_get_expression (p1);
	g_assert_cmpstr (cpg_expression_get_as_string (e), ==, "2");
}

static void
test_apply_multiple_link ()
{
	CpgLink *t1 = cpg_link_new ("l1", NULL, NULL);
	CpgLink *t2 = cpg_link_new ("l2", NULL, NULL);

	cpg_link_add_action (t1, cpg_link_action_new ("x", cpg_expression_new ("1")));
	cpg_link_add_action (t2, cpg_link_action_new ("y", cpg_expression_new ("2")));

	CpgLink *l = cpg_link_new ("o1", NULL, NULL);

	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t1));
	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t2));

	CpgLinkAction *a1 = cpg_link_get_action (l, "x");
	CpgLinkAction *a2 = cpg_link_get_action (l, "y");

	g_assert (CPG_IS_LINK_ACTION (a1));
	g_assert (CPG_IS_LINK_ACTION (a2));
}

static void
test_apply_multiple_link_override ()
{
	CpgLink *t1 = cpg_link_new ("l1", NULL, NULL);
	CpgLink *t2 = cpg_link_new ("l2", NULL, NULL);

	cpg_link_add_action (t1, cpg_link_action_new ("x", cpg_expression_new ("1")));
	cpg_link_add_action (t2, cpg_link_action_new ("x", cpg_expression_new ("2")));

	CpgLink *l = cpg_link_new ("o1", NULL, NULL);

	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t1));
	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t2));

	CpgLinkAction *a1 = cpg_link_get_action (l, "x");
	g_assert (CPG_IS_LINK_ACTION (a1));

	CpgExpression *e = cpg_link_action_get_equation (a1);

	g_assert_cmpstr (cpg_expression_get_as_string (e), ==, "2");
}

static void
test_unapply_multiple_link_override ()
{
	CpgLink *t1 = cpg_link_new ("l1", NULL, NULL);
	CpgLink *t2 = cpg_link_new ("l2", NULL, NULL);

	cpg_link_add_action (t1, cpg_link_action_new ("x", cpg_expression_new ("1")));
	cpg_link_add_action (t2, cpg_link_action_new ("x", cpg_expression_new ("2")));

	CpgLink *l = cpg_link_new ("o1", NULL, NULL);

	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t1));
	cpg_object_apply_template (CPG_OBJECT (l), CPG_OBJECT (t2));

	cpg_object_unapply_template (CPG_OBJECT (l), CPG_OBJECT (t1));

	CpgLinkAction *a1 = cpg_link_get_action (l, "x");
	g_assert (CPG_IS_LINK_ACTION (a1));

	CpgExpression *e = cpg_link_action_get_equation (a1);

	g_assert_cmpstr (cpg_expression_get_as_string (e), ==, "2");
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
	g_test_add_func ("/template/apply_link", test_apply_link);
	g_test_add_func ("/template/apply_group", test_apply_group);
	g_test_add_func ("/template/apply_overload_state", test_apply_overload_state);
	g_test_add_func ("/template/apply_overload_link", test_apply_overload_link);
	g_test_add_func ("/template/apply_overload_group", test_apply_overload_group);

	g_test_add_func ("/template/track_modified_state", test_track_modified_state);
	g_test_add_func ("/template/track_modified_link", test_track_modified_link);
	/* g_test_add_func ("/templates/track_modified_group", test_track_modified_group); */

	g_test_add_func ("/templates/unapply_state", test_unapply_state);
	g_test_add_func ("/templates/unapply_modified_state", test_unapply_modified_state);

	g_test_add_func ("/templates/unapply_link", test_unapply_link);
	g_test_add_func ("/templates/unapply_modified_link", test_unapply_modified_link);

	g_test_add_func ("/templates/apply_multiple_state", test_apply_multiple_state);
	g_test_add_func ("/templates/apply_multiple_state_override", test_apply_multiple_state_override);
	g_test_add_func ("/templates/unapply_multiple_state_override", test_unapply_multiple_state_override);

	g_test_add_func ("/templates/apply_multiple_link", test_apply_multiple_link);
	g_test_add_func ("/templates/apply_multiple_link_override", test_apply_multiple_link_override);
	g_test_add_func ("/templates/unapply_multiple_link_override", test_unapply_multiple_link_override);

	g_test_run ();

	return 0;
}
