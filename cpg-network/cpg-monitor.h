#ifndef __CPG_MONITOR_H__
#define __CPG_MONITOR_H__

#include "cpg-object.h"

/* forward declaration */
struct _CpgNetwork;

typedef struct _CpgMonitor CpgMonitor;

GType			  cpg_monitor_get_type				 (void);

CpgMonitor		 *cpg_monitor_new					 (struct _CpgNetwork *network,
													  CpgObject          *object, 
													  gchar const        *property_name);

gdouble const 	 *cpg_monitor_get_data			 	 (CpgMonitor         *monitor,
												 	  guint              *size);

gdouble			 *cpg_monitor_get_data_resampled 	 (CpgMonitor         *monitor,
												      gdouble const      *sites,
												      guint               size);

#endif /* __CPG_MONITOR_H__ */

// vi:ts=4
