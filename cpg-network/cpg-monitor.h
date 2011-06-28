/*
 * cpg-monitor.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_MONITOR_H__
#define __CPG_MONITOR_H__

#include <glib-object.h>
#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

#define CPG_TYPE_MONITOR            (cpg_monitor_get_type ())
#define CPG_MONITOR(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MONITOR, CpgMonitor))
#define CPG_MONITOR_CONST(obj)      (G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MONITOR, CpgMonitor const))
#define CPG_MONITOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_MONITOR, CpgMonitorClass))
#define CPG_IS_MONITOR(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_MONITOR))
#define CPG_IS_MONITOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_MONITOR))
#define CPG_MONITOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_MONITOR, CpgMonitorClass))

typedef struct _CpgMonitor        CpgMonitor;
typedef struct _CpgMonitorClass   CpgMonitorClass;
typedef struct _CpgMonitorPrivate CpgMonitorPrivate;

struct _CpgMonitor
{
	/*< private >*/
	GObject parent;

	CpgMonitorPrivate *priv;
};

struct _CpgMonitorClass
{
	/*< private >*/
	GObjectClass parent_class;
};

/* forward declaration */
CPG_FORWARD_DECL (CpgNetwork);

GType          cpg_monitor_get_type           (void) G_GNUC_CONST;

CpgMonitor    *cpg_monitor_new                (CPG_FORWARD_DECL (CpgNetwork) *network,
                                               CpgProperty        *property);

const gdouble *cpg_monitor_get_sites          (CpgMonitor         *monitor,
                                               guint              *size);
const gdouble *cpg_monitor_get_data           (CpgMonitor         *monitor,
                                               guint              *size);

gboolean       cpg_monitor_get_data_resampled (CpgMonitor         *monitor,
                                               const gdouble      *sites,
                                               guint               size,
                                               gdouble            *ret);

CpgProperty   *cpg_monitor_get_property       (CpgMonitor         *monitor);

G_END_DECLS

#endif /* __CPG_MONITOR_H__ */

// vi:ts=4
