#ifndef __CDN_IO_NETWORK_CLIENT_H__
#define __CDN_IO_NETWORK_CLIENT_H__

#include <codyn/cdn-node.h>
#include <gmodule.h>

G_BEGIN_DECLS

#define CDN_TYPE_IO_NETWORK_CLIENT		(cdn_io_network_client_get_type ())
#define CDN_IO_NETWORK_CLIENT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_NETWORK_CLIENT, CdnIoNetworkClient))
#define CDN_IO_NETWORK_CLIENT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_NETWORK_CLIENT, CdnIoNetworkClient const))
#define CDN_IO_NETWORK_CLIENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IO_NETWORK_CLIENT, CdnIoNetworkClientClass))
#define CDN_IS_IO_NETWORK_CLIENT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IO_NETWORK_CLIENT))
#define CDN_IS_IO_NETWORK_CLIENT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IO_NETWORK_CLIENT))
#define CDN_IO_NETWORK_CLIENT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IO_NETWORK_CLIENT, CdnIoNetworkClientClass))

typedef enum
{
	CDN_NETWORK_PROTOCOL_TCP,
	CDN_NETWORK_PROTOCOL_UNIX,
	CDN_NETWORK_PROTOCOL_UDP
} CdnNetworkProtocol;

typedef struct _CdnIoNetworkClient		CdnIoNetworkClient;
typedef struct _CdnIoNetworkClientClass		CdnIoNetworkClientClass;
typedef struct _CdnIoNetworkClientPrivate	CdnIoNetworkClientPrivate;

struct _CdnIoNetworkClient
{
	CdnNode parent;

	CdnIoNetworkClientPrivate *priv;
};

struct _CdnIoNetworkClientClass
{
	CdnNodeClass parent_class;
};

GType cdn_io_network_client_get_type (void) G_GNUC_CONST;

void cdn_io_network_client_register (GTypeModule *type_module);

G_END_DECLS

#endif /* __CDN_IO_NETWORK_CLIENT_H__ */
