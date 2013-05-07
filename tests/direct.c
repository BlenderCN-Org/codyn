#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static void
test_direct ()
{
	cdn_test_variables_with_annotated_output_from_path ("test_direct.cdn");
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/direct/direct", test_direct);

	g_test_run ();

	return 0;
}
