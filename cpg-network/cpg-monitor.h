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
