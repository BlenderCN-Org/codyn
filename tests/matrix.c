#include <stdlib.h>
#include <math.h>

#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-debug.h>

#include "utils.h"

static void
test_matrix_parse ()
{
	test_variables_with_annotated_output_from_path ("test_matrix.cdn");
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 36, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	cdn_debug_init ();

	g_test_add_func ("/matrix/parse", test_matrix_parse);
	g_test_run ();

	return 0;
}
