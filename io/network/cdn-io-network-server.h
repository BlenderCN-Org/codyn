#ifndef __CDN_IO_NETWORK_SERVER_H__
#define __CDN_IO_NETWORK_SERVER_H__

#include <codyn/cdn-node.h>
#include <gmodule.h>
#include "cdn-io-network-client.h"

G_BEGIN_DECLS

#define CDN_TYPE_IO_NETWORK_SERVER		(cdn_io_network_server_get_type ())
#define CDN_IO_NETWORK_SERVER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_NETWORK_SERVER, CdnIoNetworkServer))
#define CDN_IO_NETWORK_SERVER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_NETWORK_SERVER, CdnIoNetworkServer const))
#define CDN_IO_NETWORK_SERVER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IO_NETWORK_SERVER, CdnIoNetworkServerClass))
#define CDN_IS_IO_NETWORK_SERVER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IO_NETWORK_SERVER))
#define CDN_IS_IO_NETWORK_SERVER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IO_NETWORK_SERVER))
#define CDN_IO_NETWORK_SERVER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IO_NETWORK_SERVER, CdnIoNetworkServerClass))

typedef struct _CdnIoNetworkServer		CdnIoNetworkServer;
typedef struct _CdnIoNetworkServerClass		CdnIoNetworkServerClass;
typedef struct _CdnIoNetworkServerPrivate	CdnIoNetworkServerPrivate;

struct _CdnIoNetworkServer
{
	CdnIoNetworkClient parent;

	CdnIoNetworkServerPrivate *priv;
};

struct _CdnIoNetworkServerClass
{
	CdnIoNetworkClientClass parent_class;
};

GType cdn_io_network_server_get_type (void) G_GNUC_CONST;

void cdn_io_network_server_register (GTypeModule *type_module);

G_END_DECLS

#endif /* __CDN_IO_NETWORK_SERVER_H__ */
