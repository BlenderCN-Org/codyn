#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#include <glib.h>
#include <math.h>

#define cpg_assert_tol(real, expect) g_assert_cmpfloat (fabs((real) - (expect)), <, 0.00000000001)

#endif /* __TESTS_UTILS_H__ */

