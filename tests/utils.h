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

#define CDN_CMP_TOL 1e-9
#define cdn_cmp_tol(real, expect) (fabs((real) - (expect)) < CDN_CMP_TOL)

#define cdn_assert_tol(real, expect) cdn_assert_float (__FILE__, __PRETTY_FUNCTION__, __LINE__, (real), (expect))
#define cdn_assert_neq_tol(real, expect) cdn_assert_nfloat (__FILE__, __PRETTY_FUNCTION__, __LINE__, (real), (expect))

void cdn_assert_float (gchar const *file, gchar const *func, gint line, gdouble real, gdouble expect);
void cdn_assert_nfloat (gchar const *file, gchar const *func, gint line, gdouble real, gdouble expect);

CdnNetwork *test_load_network (gchar const *network, ...) G_GNUC_NULL_TERMINATED;
CdnNetwork *test_load_network_from_path (gchar const *path, ...) G_GNUC_NULL_TERMINATED;
CdnNetwork *test_load_network_from_path_with_objects (gchar const *path, ...) G_GNUC_NULL_TERMINATED;

CdnEdgeAction *find_action (CdnNode *parent, gchar const *path);

#define test_variables_with_annotated_output_from_path(path) \
	test_variables_with_annotated_output_from_path_impl(__FILE__, __PRETTY_FUNCTION__, __LINE__, (path))

void test_variables_with_annotated_output_from_path_impl (gchar const *file, gchar const *func, gint line, gchar const *path);

#endif /* __TESTS_UTILS_H__ */

