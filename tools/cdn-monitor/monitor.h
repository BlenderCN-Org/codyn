#ifndef __CDN_MONITOR_MONITOR_H__
#define __CDN_MONITOR_MONITOR_H__

#include <glib.h>
#include <gio/gio.h>
#include <stdint.h>
#include <codyn/codyn.h>

typedef struct
{
	GPtrArray *monitored;
	GSList *monitors;
	GSList *names;
	gchar *output_file;
	GOutputStream *stream;
} CdnMonitored;

typedef struct _CdnMonitorVariable CdnMonitorVariable;

struct _CdnMonitorVariable
{
	union
	{
		CdnVariable *variable;
		uint32_t     state;
	};

	gpointer userdata;
	CdnDimension dimension;

	gint row;
	gint col;

	gchar *(*get_name) (CdnMonitorVariable *variable);
	gdouble const *(*get_values) (CdnMonitorVariable *variable);
};

CdnMonitored *cdn_monitored_new ();

void cdn_monitored_free (CdnMonitored *monitored);

#endif /* __CDN_MONITOR_MONITOR_H__ */

