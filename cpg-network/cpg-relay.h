#ifndef __CPG_RELAY_H__
#define __CPG_RELAY_H__

#include <glib-object.h>
#include "cpg-object.h"

G_BEGIN_DECLS

#define CPG_TYPE_RELAY				(cpg_relay_get_type ())
#define CPG_RELAY(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_RELAY, CpgRelay))
#define CPG_RELAY_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_RELAY, CpgRelay const))
#define CPG_RELAY_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_RELAY, CpgRelayClass))
#define CPG_IS_RELAY(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_RELAY))
#define CPG_IS_RELAY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_RELAY))
#define CPG_RELAY_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_RELAY, CpgRelayClass))

typedef struct _CpgRelay		CpgRelay;
typedef struct _CpgRelayClass	CpgRelayClass;
typedef struct _CpgRelayPrivate	CpgRelayPrivate;

struct _CpgRelay {
	CpgObject parent;
	
	CpgRelayPrivate *priv;
};

struct _CpgRelayClass {
	CpgObjectClass parent_class;
};

GType cpg_relay_get_type(void) G_GNUC_CONST;
CpgRelay *cpg_relay_new(gchar const *id);

G_END_DECLS

#endif /* __CPG_RELAY_H__ */
