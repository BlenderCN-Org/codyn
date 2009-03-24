#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#include <glib.h>

#define assert(real, expect) if (!assert_function(real, expect, __FUNCTION__)) { return 0; }
gboolean assert_function(gdouble expect, gdouble real, gchar const *function);

#endif /* __TESTS_UTILS_H__ */

