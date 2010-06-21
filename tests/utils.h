#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#include <glib.h>
#include <math.h>

#define cpg_assert_tol(real, expect) g_assert (fabs ((expect) - (real)) < 0.00000000001)

#endif /* __TESTS_UTILS_H__ */

