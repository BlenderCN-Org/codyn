#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static void
test_import ()
{
	CdnNetwork *network;

	network = test_load_network_from_path ("test_import.cdn",
	                                       CDN_PATH_TEMPLATE_OBJECT, "imported.template1",
	                                       CDN_PATH_TEMPLATE_PROPERTY, "imported.template1.x",
	                                       CDN_PATH_TEMPLATE_OBJECT, "imported.template1.nested1",
	                                       CDN_PATH_TEMPLATE_PROPERTY, "imported.template1.nested1.y",
	                                       CDN_PATH_OBJECT, "imported",
	                                       CDN_PATH_OBJECT, "imported.state1",
	                                       CDN_PATH_PROPERTY, "imported.state1.x",
	                                       CDN_PATH_OBJECT, "imported.state1.nested1",
	                                       CDN_PATH_PROPERTY, "imported.state1.nested1.y",
	                                       NULL);

	g_object_unref (network);
}

static void
test_import_templates ()
{
	CdnNetwork *network;

	network = test_load_network_from_path ("test_import_templates.cdn",
	                                       CDN_PATH_TEMPLATE_OBJECT, "imported.template1",
	                                       CDN_PATH_TEMPLATE_PROPERTY, "imported.template1.x",
	                                       CDN_PATH_TEMPLATE_OBJECT, "imported.template1.nested1",
	                                       CDN_PATH_TEMPLATE_PROPERTY, "imported.template1.nested1.y",
	                                       NULL);
	g_assert (network);
	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/import/import_templates", test_import_templates);
	g_test_add_func ("/import/import", test_import);

	g_test_run ();

	return 0;
}
