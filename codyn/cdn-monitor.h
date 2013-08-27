/*
 * cdn-monitor.h
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CDN_MONITOR_H__
#define __CDN_MONITOR_H__

#include <glib-object.h>
#include <codyn/cdn-object.h>
#include <codyn/cdn-utils.h>
#include <codyn/cdn-forward-decl.h>

G_BEGIN_DECLS

#define CDN_TYPE_MONITOR            (cdn_monitor_get_type ())
#define CDN_MONITOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_MONITOR, CdnMonitor))
#define CDN_MONITOR_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_MONITOR, CdnMonitor const))
#define CDN_MONITOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_MONITOR, CdnMonitorClass))
#define CDN_IS_MONITOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_MONITOR))
#define CDN_IS_MONITOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_MONITOR))
#define CDN_MONITOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_MONITOR, CdnMonitorClass))

typedef struct _CdnMonitor        CdnMonitor;
typedef struct _CdnMonitorClass   CdnMonitorClass;
typedef struct _CdnMonitorPrivate CdnMonitorPrivate;

/**
 * CdnMonitor:
 *
 * Property value monitor.
 *
 * A #CdnMonitor can be used to monitor the value of a certain #CdnVariable
 * while simulating. The monitor will collect the value of the property at
 * each simulation step and provides methods to access these values.
 * Particularly useful is #cdn_monitor_get_data_resampled which retrieves the
 * data resampled at specific times.
 */
struct _CdnMonitor
{
	/*< private >*/
	GObject parent;

	CdnMonitorPrivate *priv;
};

struct _CdnMonitorClass
{
	/*< private >*/
	GObjectClass parent_class;
};

/* forward declaration */

GType          cdn_monitor_get_type           (void) G_GNUC_CONST;

CdnMonitor    *cdn_monitor_new                (CdnNetworkForward *network,
                                               CdnVariable        *property);

const gdouble *cdn_monitor_get_sites          (CdnMonitor         *monitor,
                                               guint              *size);
const gdouble *cdn_monitor_get_data           (CdnMonitor         *monitor,
                                               guint              *size);

gboolean       cdn_monitor_get_data_resampled (CdnMonitor         *monitor,
                                               const gdouble      *sites,
                                               guint               size,
                                               gdouble            *ret);

CdnVariable   *cdn_monitor_get_variable       (CdnMonitor         *monitor);

G_END_DECLS

#endif /* __CDN_MONITOR_H__ */

// vi:ts=4
