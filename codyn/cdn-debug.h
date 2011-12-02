#ifndef __CDN_DEBUG_H__
#define __CDN_DEBUG_H__

#include <glib.h>

typedef enum
{
	CDN_DEBUG_LINSOLVE = 1 << (G_LOG_LEVEL_USER_SHIFT + 0),
	CDN_DEBUG_DIFF = 1 << (G_LOG_LEVEL_USER_SHIFT + 1),
	CDN_DEBUG_SIMPLIFY = 1 << (G_LOG_LEVEL_USER_SHIFT + 2),
} CdnDebugSection;

#define DEBUG_LINSOLVE      CDN_DEBUG_LINSOLVE, __FILE__, __LINE__, G_STRFUNC
#define DEBUG_DIFF          CDN_DEBUG_DIFF, __FILE__, __LINE__, G_STRFUNC
#define DEBUG_SIMPLIFY      CDN_DEBUG_SIMPLIFY, __FILE__, __LINE__, G_STRFUNC

void cdn_debug_init (void);

void cdn_debug (CdnDebugSection  section,
                gchar const     *file,
                gint             line,
                gchar const     *function);

void cdn_debug_message (CdnDebugSection section,
                        gchar const    *file,
                        gint            line,
                        gchar const    *function,
                        gchar const    *format, ...) G_GNUC_PRINTF (5, 6);

#endif /* __CDN_DEBUG_H__ */
