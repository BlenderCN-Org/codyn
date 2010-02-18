#ifndef __CPG_MONITOR_H__
#define __CPG_MONITOR_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-utils.h>

G_BEGIN_DECLS

/* forward declaration */
CPG_FORWARD_DECL (CpgNetwork);

typedef struct _CpgMonitor CpgMonitor;

GType			  cpg_monitor_get_type				 (void);

CpgMonitor		 *cpg_monitor_new					 (struct _CpgNetwork *network,
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
