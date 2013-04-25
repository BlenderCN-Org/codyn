#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#include <glib.h>
#include <math.h>
#include <codyn/codyn.h>

typedef enum
{
	CDN_PATH_NONE,
	CDN_PATH_OBJECT,
	CDN_PATH_PROPERTY,
	CDN_PATH_ACTION,
	CDN_PATH_TEMPLATE_OBJECT,
	CDN_PATH_TEMPLATE_PROPERTY
} CdnPath;

#define cdn_cmp_tol(real, expect) (fabs((real) - (expect)) < 0.00000000001)
#define cdn_assert_tol(real, expect) g_assert_cmpfloat (fabs((real) - (expect)), <, 0.00000000001)
#define cdn_assert_neq_tol(real, expect) g_assert_cmpfloat (fabs((real) - (expect)), >, 0.00000000001)

CdnNetwork *test_load_network (gchar const *network, ...) G_GNUC_NULL_TERMINATED;
CdnNetwork *test_load_network_from_path (gchar const *path, ...) G_GNUC_NULL_TERMINATED;
CdnNetwork *test_load_network_from_path_with_objects (gchar const *path, ...) G_GNUC_NULL_TERMINATED;

CdnEdgeAction *find_action (CdnNode *parent, gchar const *path);

void test_variables_with_annotated_output_from_path (gchar const *path);

#endif /* __TESTS_UTILS_H__ */

