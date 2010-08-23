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

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

/* forward declaration */
CPG_FORWARD_DECL (CpgNetwork);

typedef struct _CpgMonitor CpgMonitor;

GType			  cpg_monitor_get_type				 (void);

CpgMonitor		 *cpg_monitor_new					 (CPG_FORWARD_DECL (CpgNetwork) *network,
													  CpgObject          *object, 
													  const gchar        *property_name);

const gdouble    *cpg_monitor_get_sites 			 (CpgMonitor         *monitor,
													  guint              *size);
const gdouble 	 *cpg_monitor_get_data			 	 (CpgMonitor         *monitor,
												 	  guint              *size);

gboolean		  cpg_monitor_get_data_resampled 	 (CpgMonitor         *monitor,
												      const gdouble      *sites,
												      guint               size,
												      gdouble            *ret);

CpgObject		 *cpg_monitor_get_object			 (CpgMonitor         *monitor);

CpgProperty		 *cpg_monitor_get_property			 (CpgMonitor		 *monitor);

G_END_DECLS

#endif /* __CPG_MONITOR_H__ */

// vi:ts=4
