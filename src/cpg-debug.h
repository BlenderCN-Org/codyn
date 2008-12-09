#ifndef __CPG_DEBUG_H__
#define __CPG_DEBUG_H__

typedef enum
{
	CPG_DEBUG_TYPE_NONE = 0,
	CPG_DEBUG_TYPE_ERROR = 1 << 0,
	CPG_DEBUG_TYPE_EXPRESSION = 1 << 1,
	CPG_DEBUG_TYPE_EVALUATE = 1 << 2,
} CpgDebugType;

#ifdef RTLINUX
extern int debug_type;
extern char const *debug_types[];
#endif

#define cpg_debug_message(type, ...) cpg_debug_message_function(type, __func__, __VA_ARGS__)

#define cpg_debug_error(...) cpg_debug_message(CPG_DEBUG_TYPE_ERROR, __VA_ARGS__)
#define cpg_debug_expression(...) cpg_debug_message(CPG_DEBUG_TYPE_EXPRESSION, __VA_ARGS__)
#define cpg_debug_evaluate(...) cpg_debug_message(CPG_DEBUG_TYPE_EVALUATE, __VA_ARGS__)

void cpg_debug_add(CpgDebugType type);

#ifndef DISABLE_DEBUG
#ifndef RTLINUX
void cpg_debug_message_function(CpgDebugType type, char const *function, char const *format, ...);
#else
//#include <rtlprint.h>
#include <stdio.h>
#define rtl_printf(...) printf(__VA_ARGS__)
#define cpg_debug_message_function(type, function, ...) 							\
		if (debug_type & type)														\
		{																			\
			rtl_printf(" ** DEBUG %s in %s: ", debug_types[type], function);		\
			rtl_printf(__VA_ARGS__); 												\
			rtl_printf("\n");														\
		}
#endif
#else
#define cpg_debug_message_function(type, function, format, ...) ;
#endif

#endif /* __CPG_DEBUG_H__ */

