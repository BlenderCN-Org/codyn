#include "cdn-debug.h"

static CdnDebugSection debug_level = 0;
static gboolean inited = FALSE;
static gint log_indent = 0;

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

static gchar const *
level_to_string (gint log_level)
{
	switch (log_level)
	{
		case CDN_DEBUG_LINSOLVE:
			return "linsolve";
		case CDN_DEBUG_DIFF:
			return "diff";
		case CDN_DEBUG_SIMPLIFY:
			return "simplify";
		case CDN_DEBUG_IO:
			return "io";
		case CDN_DEBUG_MATH:
			return "math";
		case CDN_DEBUG_INTEGRATOR:
			return "integrator";
		default:
			return "none";
	}
}

static void
cdn_debug_log_handler (gchar const    *log_domain,
                       GLogLevelFlags  log_level,
                       gchar const    *message,
                       gpointer        userdata)
{
	gchar *id;

	id = g_strnfill (log_indent * 2, ' ');

	if (log_level & CDN_DEBUG_SIMPLIFY)
	{
		g_printerr ("%s[cs] %s\n",
		            id,
		            message);
	}
	else if (log_level & CDN_DEBUG_DIFF)
	{
		g_printerr ("%s[df] %s\n",
		            id,
		            message);
	}
	else
	{
		g_printerr ("%s[%s:%s(%p)]: %s\n",
		            id,
		            log_domain,
		            level_to_string (log_level),
		            g_thread_self (),
		            message);
	}

	g_free (id);
}

void
cdn_debug_push_indent ()
{
	++log_indent;
}

void
cdn_debug_pop_indent ()
{
	--log_indent;
}

void
cdn_debug_init ()
{
	if (G_UNLIKELY (!inited))
	{
		inited = TRUE;

		if (g_getenv ("CDN_DEBUG_LINSOLVE") != NULL)
		{
			debug_level |= CDN_DEBUG_LINSOLVE;
		}

		if (g_getenv ("CDN_DEBUG_DIFF") != NULL)
		{
			debug_level |= CDN_DEBUG_DIFF;
		}

		if (g_getenv ("CDN_DEBUG_SIMPLIFY") != NULL)
		{
			debug_level |= CDN_DEBUG_SIMPLIFY;
		}

		if (g_getenv ("CDN_DEBUG_IO") != NULL)
		{
			debug_level |= CDN_DEBUG_IO;
		}

		if (g_getenv ("CDN_DEBUG_MATH") != NULL)
		{
			debug_level |= CDN_DEBUG_MATH;
		}

		if (g_getenv ("CDN_DEBUG_INTEGRATOR") != NULL)
		{
			debug_level |= CDN_DEBUG_INTEGRATOR;
		}

		g_log_set_handler ("Cdn",
		                   CDN_DEBUG_DIFF |
		                   CDN_DEBUG_LINSOLVE |
		                   CDN_DEBUG_IO |
		                   CDN_DEBUG_DIFF |
		                   CDN_DEBUG_MATH |
		                   CDN_DEBUG_INTEGRATOR,
		                   cdn_debug_log_handler,
		                   NULL);
	}
}

gboolean
cdn_debug_is_enabled (CdnDebugSection section)
{
	return (debug_level & section) != 0;
}
