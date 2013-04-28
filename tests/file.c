#include <codyn/codyn.h>
#include <codyn/cdn-expression.h>
#include <codyn/cdn-object.h>

#include "utils.h"

static void
test_input ()
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnVariable *x;
	CdnVariable *y;

	network = test_load_network_from_path_with_objects ("test_file.cdn",
	                                       CDN_PATH_OBJECT, "i", NULL,
	                                       CDN_PATH_PROPERTY, "i.time", NULL,
	                                       CDN_PATH_PROPERTY, "i.x", &x,
	                                       CDN_PATH_PROPERTY, "i.y", &y,
	                                       NULL);

	gdouble xvals[] = {0, 1, 2, 0};
	gdouble yvals[] = {1, 2, -1, 1};

	cdn_network_begin (network, 0, &error);
	g_assert_no_error (error);

	gint i;

	for (i = 0; i < sizeof (xvals) / sizeof (gdouble); ++i)
	{
		cdn_assert_tol (cdn_variable_get_value (x), xvals[i]);
		cdn_assert_tol (cdn_variable_get_value (y), yvals[i]);

		cdn_network_step (network, 0.1);
	}

	cdn_network_end (network, &error);
	g_assert_no_error (error);

	g_object_unref (network);
}

static void
test_input_repeat ()
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnVariable *x;
	CdnVariable *y;

	network = test_load_network_from_path_with_objects ("test_file.cdn",
	                                       CDN_PATH_OBJECT, "i", NULL,
	                                       CDN_PATH_PROPERTY, "i.time", NULL,
	                                       CDN_PATH_PROPERTY, "i.x", &x,
	                                       CDN_PATH_PROPERTY, "i.y", &y,
	                                       NULL);

	gdouble xvals[] = {0, 1, 2, 0, 1, 2};
	gdouble yvals[] = {1, 2, -1, 1, 2, -1};

	cdn_network_begin (network, 0, &error);
	g_assert_no_error (error);

	gint i;

	for (i = 0; i < sizeof (xvals) / sizeof (gdouble); ++i)
	{
		cdn_assert_tol (cdn_variable_get_value (x), xvals[i]);
		cdn_assert_tol (cdn_variable_get_value (y), yvals[i]);

		cdn_network_step (network, 0.1);
	}

	cdn_network_end (network, &error);
	g_assert_no_error (error);

	g_object_unref (network);
}

static void
test_input_no_interpolate ()
{
	CdnNetwork *network;
	GError *error = NULL;
	CdnVariable *x;
	CdnVariable *y;

	network = test_load_network_from_path_with_objects ("test_file.cdn",
	                                       CDN_PATH_OBJECT, "nointerp", NULL,
	                                       CDN_PATH_PROPERTY, "nointerp.time", NULL,
	                                       CDN_PATH_PROPERTY, "nointerp.x", &x,
	                                       CDN_PATH_PROPERTY, "nointerp.y", &y,
	                                       NULL);

	gdouble xvals[] = {0, 0, 1, 1, 1, 1, 2, 2, 2};
	gdouble yvals[] = {1, 1, 2, 2, 2, 2, -1, -1, -1};

	cdn_network_begin (network, 0, &error);
	g_assert_no_error (error);

	gint i;

	for (i = 0; i < sizeof (xvals) / sizeof (gdouble); ++i)
	{
		cdn_assert_tol (cdn_variable_get_value (x), xvals[i]);
		cdn_assert_tol (cdn_variable_get_value (y), yvals[i]);

		cdn_network_step (network, 0.03);
	}

	cdn_network_end (network, &error);
	g_assert_no_error (error);

	g_object_unref (network);
}

int
main (int   argc,
      char *argv[])
{
#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

	g_test_init (&argc, &argv, NULL);

	g_test_add_func ("/file/input", test_input);
	g_test_add_func ("/file/input-repeat", test_input_repeat);
	g_test_add_func ("/file/input-no-interpolate", test_input_no_interpolate);

	g_test_run ();

	return 0;
}
