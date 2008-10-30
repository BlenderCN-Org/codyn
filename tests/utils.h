#ifndef __TESTS_UTILS_H__
#define __TESTS_UTILS_H__

#define assert(real, expect) if (!assert_function(real, expect, __FUNCTION__)) { return 0; }
int assert_function(double expect, double real, char const *function);

#endif /* __TESTS_UTILS_H__ */

