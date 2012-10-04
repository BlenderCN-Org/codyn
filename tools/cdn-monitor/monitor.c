#include "monitor.h"

CdnMonitored *
cdn_monitored_new ()
{
	CdnMonitored *ret = g_slice_new0 (CdnMonitored);

	ret->monitored = g_ptr_array_new_with_free_func ((GDestroyNotify)g_free);
	return ret;
}

static void
monitor_variable_free (CdnMonitorVariable *v)
{
	g_slice_free (CdnMonitorVariable, v);
}

void
cdn_monitored_free (CdnMonitored *monitored)
{
	if (!monitored)
	{
		return;
	}

	if (monitored->monitored)
	{
		g_ptr_array_free (monitored->monitored, TRUE);
	}

	if (monitored->stream)
	{
		g_output_stream_flush (monitored->stream, NULL, NULL);
		g_output_stream_close (monitored->stream, NULL, NULL);
	}

	g_slist_foreach (monitored->monitors, (GFunc)monitor_variable_free, NULL);
	g_slist_free (monitored->monitors);

	g_slist_foreach (monitored->names, (GFunc)g_free, NULL);
	g_slist_free (monitored->names);

	g_free (monitored->output_file);
	g_slice_free (CdnMonitored, monitored);
}

