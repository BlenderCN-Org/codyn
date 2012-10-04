#ifndef __CDN_MONITOR_IMPLEMENTATION_H__
#define __CDN_MONITOR_IMPLEMENTATION_H__

#include <codyn/codyn.h>
#include <codyn/cdn-selector.h>
#include "cdn-rawc-types.h"
#include "monitor.h"

typedef struct _CdnMonitorImplementation CdnMonitorImplementation;

struct _CdnMonitorImplementation
{
	CdnNetwork *network;

	void *userdata;

	GSList *(*resolve) (CdnMonitorImplementation *implementation,
	                    CdnSelector              *selector);

	void (*simplify) (CdnMonitorImplementation *implementation);

	gboolean (*free) (CdnMonitorImplementation *implementation);
	gboolean (*terminated) (CdnMonitorImplementation *implementation);

	CdnMonitorVariable *(*get_time) (CdnMonitorImplementation *implementation);

	void (*begin) (CdnMonitorImplementation *implementation, gdouble t, gdouble dt);
	gdouble (*step) (CdnMonitorImplementation *implementation, gdouble t, gdouble dt);
	void (*end) (CdnMonitorImplementation *implementation);
};

void cdn_monitor_implementation_free (CdnMonitorImplementation *implementation);

CdnMonitorImplementation *cdn_monitor_implementation_new ();

CdnMonitorImplementation *cdn_monitor_implementation_codyn_new (gchar const *filename);
CdnMonitorImplementation *cdn_monitor_implementation_rawc_new (gchar const *filename);

#endif /* __CDN_MONITOR_IMPLEMENTATION_H__ */

