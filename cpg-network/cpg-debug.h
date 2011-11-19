#ifndef __CPG_DEBUG_H__
#define __CPG_DEBUG_H__

#include <glib.h>

typedef enum
{
	CPG_DEBUG_LINSOLVE = 1 << (G_LOG_LEVEL_USER_SHIFT + 0),
	CPG_DEBUG_DIFF = 1 << (G_LOG_LEVEL_USER_SHIFT + 1),
	CPG_DEBUG_SIMPLIFY = 1 << (G_LOG_LEVEL_USER_SHIFT + 2),
} CpgDebugSection;

#define DEBUG_LINSOLVE      CPG_DEBUG_LINSOLVE, __FILE__, __LINE__, G_STRFUNC
#define DEBUG_DIFF          CPG_DEBUG_DIFF, __FILE__, __LINE__, G_STRFUNC
#define DEBUG_SIMPLIFY      CPG_DEBUG_SIMPLIFY, __FILE__, __LINE__, G_STRFUNC

void cpg_debug_init (void);

void cpg_debug (CpgDebugSection  section,
                gchar const     *file,
                gint             line,
                gchar const     *function);

void cpg_debug_message (CpgDebugSection section,
                        gchar const    *file,
                        gint            line,
                        gchar const    *function,
                        gchar const    *format, ...) G_GNUC_PRINTF (5, 6);

#endif /* __CPG_DEBUG_H__ */

