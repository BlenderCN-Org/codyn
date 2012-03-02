#include "cdn-debug.h"

static CdnDebugSection debug_level = 0;
static gboolean inited = FALSE;

void
cdn_debug_message (CdnDebugSection  section,
                   const gchar     *file,
                   gint             line,
                   const gchar     *function,
                   const gchar     *format,
                   ...)
{
	if (G_UNLIKELY (debug_level & section))
	{
		gchar *msg;
		va_list ap;

		va_start (ap, format);
		msg = g_strdup_vprintf (format, ap);
		va_end (ap);

		g_log ("Cdn",
		       (GLogLevelFlags)section,
		       "%s:%d (%s) %s",
		       file,
		       line,
		       function,
		       msg);

		g_free (msg);
	}
}

void
cdn_debug (CdnDebugSection  section,
           gchar const     *file,
           gint             line,
           gchar const     *function)
{
	if (G_UNLIKELY (debug_level & section))
	{
		g_log ("Cdn",
		       (GLogLevelFlags)section,
		       "%s:%d (%s)",
		       file,
		       line,
		       function);
	}
}

static void
cdn_debug_log_handler (gchar const    *log_domain,
                       GLogLevelFlags  log_level,
                       gchar const    *message,
                       gpointer        userdata)
{
	g_printerr ("[%s(%p)]: %s\n",
	            log_domain,
	            g_thread_self (),
	            message);
}

void
cdn_debug_init ()
{
	if (G_UNLIKELY (!inited))
	{
		inited = TRUE;

		if (g_getenv ("CODYN_DEBUG_LINSOLVE") != NULL)
		{
			debug_level |= CDN_DEBUG_LINSOLVE;
		}

		if (g_getenv ("CODYN_DEBUG_DIFF") != NULL)
		{
			debug_level |= CDN_DEBUG_DIFF;
		}

		if (g_getenv ("CODYN_DEBUG_SIMPLIFY") != NULL)
		{
			debug_level |= CDN_DEBUG_SIMPLIFY;
		}

		if (g_getenv ("CODYN_DEBUG_IO") != NULL)
		{
			debug_level |= CDN_DEBUG_IO;
		}

		g_log_set_handler ("Cdn",
		                   CDN_DEBUG_DIFF |
		                   CDN_DEBUG_LINSOLVE |
		                   CDN_DEBUG_IO,
		                   cdn_debug_log_handler,
		                   NULL);
	}
}

