#include "implementation.h"

static gboolean
default_terminated (CdnMonitorImplementation *implementation)
{
	return FALSE;
}

CdnMonitorImplementation *
cdn_monitor_implementation_new ()
{
	CdnMonitorImplementation *ret;

	ret = g_slice_new0 (CdnMonitorImplementation);

	ret->terminated = default_terminated;

	return ret;
}

void
cdn_monitor_implementation_free (CdnMonitorImplementation *implementation)
{
	if (implementation->free)
	{
		if (implementation->free (implementation))
		{
			return;
		}
	}

	g_slice_free (CdnMonitorImplementation, implementation);
}
