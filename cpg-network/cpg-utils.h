#ifndef __CPG_UTILS_H__
#define __CPG_UTILS_H__

#include <stdlib.h>

#define CPG_FORWARD_DECL(id)    struct _##id
#define array_resize(Ptr, Type, Num) (Ptr = (Type *)realloc(Ptr, sizeof(Type) * (Num)))

#endif /* __CPG_UTILS_H__ */
