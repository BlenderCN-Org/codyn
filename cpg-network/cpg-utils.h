#ifndef __CPG_UTILS_H__
#define __CPG_UTILS_H__

#include <stdlib.h>

#define array_resize(Ptr, Type, Num) (Ptr = (Type *)realloc(Ptr, sizeof(Type) * (Num)))

#endif /* __CPG_UTILS_H__ */
