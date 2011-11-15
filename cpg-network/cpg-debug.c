#include "cpg-debug.h"

static CpgDebugSection debug_level = 0;
static gboolean inited = FALSE;

void
cpg_debug_message (CpgDebugSection  section,
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

		g_log ("Cpg",
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
cpg_debug (CpgDebugSection  section,
           gchar const     *file,
           gint             line,
           gchar const     *function)
{
	if (G_UNLIKELY (debug_level & section))
	{
		g_log ("Cpg",
		       (GLogLevelFlags)section,
		       "%s:%d (%s)",
		       file,
		       line,
		       function);
	}
}

void
cpg_debug_init ()
{
	if (G_UNLIKELY (!inited))
	{
		inited = TRUE;

		if (g_getenv ("CPG_DEBUG_LINSOLVE") != NULL)
		{
			debug_level |= CPG_DEBUG_LINSOLVE;
		}
	}
}

