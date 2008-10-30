#ifndef __CPG_DEBUG_H__
#define __CPG_DEBUG_H__

#include <stdarg.h>

typedef enum
{
	CPG_DEBUG_TYPE_NONE = 0,
	CPG_DEBUG_TYPE_EXPRESSION = 1 << 0
} CpgDebugType;

#define cpg_debug_message(type, format, ...) cpg_debug_message_function(type, __FUNCTION__, format, __VA_ARGS__)

#define cpg_debug_expression(format, ...) cpg_debug_message(CPG_DEBUG_TYPE_EXPRESSION, format, __VA_ARGS__)

void cpg_debug_add(CpgDebugType type);
void cpg_debug_message_function(CpgDebugType type, char const *function, char const *format, ...);

#endif /* __CPG_DEBUG_H__ */

