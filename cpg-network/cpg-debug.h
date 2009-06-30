#ifndef __CPG_DEBUG_H__
#define __CPG_DEBUG_H__

/**
 * CpgDebugType:
 * @CPG_DEBUG_TYPE_NONE: none
 * @CPG_DEBUG_TYPE_ERROR: error
 * @CPG_DEBUG_TYPE_EXPRESSION: expression
 * @CPG_DEBUG_TYPE_EVALUATE: evaluate
 *
 * Flags used for writing debug messages
 *
 **/
typedef enum
{
	CPG_DEBUG_TYPE_NONE = 0,
	CPG_DEBUG_TYPE_ERROR = 1 << 0,
	CPG_DEBUG_TYPE_EXPRESSION = 1 << 1,
	CPG_DEBUG_TYPE_EVALUATE = 1 << 2,
} CpgDebugType;

#define cpg_debug_message(type, ...) cpg_debug_message_function(type, __func__, __VA_ARGS__)
#define cpg_debug_error(...) cpg_debug_message(CPG_DEBUG_TYPE_ERROR, __VA_ARGS__)
#define cpg_debug_expression(...) cpg_debug_message(CPG_DEBUG_TYPE_EXPRESSION, __VA_ARGS__)
#define cpg_debug_evaluate(...) cpg_debug_message(CPG_DEBUG_TYPE_EVALUATE, __VA_ARGS__)

void cpg_debug_add(CpgDebugType type);

#ifndef DISABLE_DEBUG
void cpg_debug_message_function(CpgDebugType type, char const *function, char const *format, ...);
#else
#define cpg_debug_message_function(type, function, format, ...) ;
#endif

#endif /* __CPG_DEBUG_H__ */

