#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#include <glib.h>
#include <math.h>
#include <cpg-network/cpg-network.h>

typedef enum
{
	CPG_PATH_NONE,
	CPG_PATH_OBJECT,
	CPG_PATH_PROPERTY,
	CPG_PATH_ACTION,
	CPG_PATH_TEMPLATE_OBJECT,
	CPG_PATH_TEMPLATE_PROPERTY
} CpgPath;

#define cpg_assert_tol(real, expect) g_assert_cmpfloat (fabs((real) - (expect)), <, 0.00000000001)
#define cpg_assert_neq_tol(real, expect) g_assert_cmpfloat (fabs((real) - (expect)), >, 0.00000000001)

CpgNetwork *test_load_network (gchar const *network, ...) G_GNUC_NULL_TERMINATED;
CpgNetwork *test_load_network_from_path (gchar const *path, ...) G_GNUC_NULL_TERMINATED;

CpgLinkAction *find_action (CpgGroup *parent, gchar const *path);

#endif /* __TESTS_UTILS_H__ */

