#include "cdn-io-network-client.h"
#include "cdn-client.h"
#include <codyn/cdn-network.h>
#include <gio/gunixsocketaddress.h>
#include "cdn-io-network-enum-types.h"

#define CDN_IO_NETWORK_CLIENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IO_NETWORK_CLIENT, CdnIoNetworkClientPrivate))

struct _CdnIoNetworkClientPrivate
{
	gdouble throttle;
	CdnIoMode mode;
	gchar *host;
	guint port;
	CdnNetworkProtocol protocol;
	CdnClient *client;
};

static void cdn_io_iface_init (gpointer iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnIoNetworkClient,
                                cdn_io_network_client,
                                CDN_TYPE_NODE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (CDN_TYPE_IO,
                                                               cdn_io_iface_init))

enum
{
	PROP_0,
	PROP_THROTTLE,
	PROP_MODE,
	PROP_HOST,
	PROP_PORT,
	PROP_PROTOCOL
};

static gboolean
cdn_io_initialize_impl (CdnIo         *io,
                        GCancellable  *cancellable,
                        GError       **error)
{
	// Create a new socket
	CdnIoNetworkClient *client = CDN_IO_NETWORK_CLIENT (io);
	GSocket *sock;
	GSocketConnectable *conn;
	GSocketAddressEnumerator *enumerator;
	GSocketAddress *sockaddr;
	GError *lasterr = NULL;

	if (client->priv->host == NULL &&
	    client->priv->protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (client));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "Please specify a unix path in the `host' setting of `%s'",
		             id);

		g_free (id);
		return FALSE;
	}

	if (client->priv->protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		sock = g_socket_new (G_SOCKET_FAMILY_UNIX,
		                     G_SOCKET_TYPE_STREAM,
		                     0,
		                     error);
	}
	else if (client->priv->protocol == CDN_NETWORK_PROTOCOL_TCP)
	{
		sock = g_socket_new (G_SOCKET_FAMILY_IPV4,
		                     G_SOCKET_TYPE_STREAM,
		                     0,
		                     error);
	}
	else
	{
		sock = g_socket_new (G_SOCKET_FAMILY_IPV4,
		                     G_SOCKET_TYPE_DATAGRAM,
		                     0,
		                     error);
	}

	if (sock == NULL)
	{
		return FALSE;
	}

	if (client->priv->protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		conn = G_SOCKET_CONNECTABLE (
			g_unix_socket_address_new (client->priv->host));
	}
	else if (client->priv->host != NULL)
	{
		conn = G_SOCKET_CONNECTABLE (
			g_network_address_new (client->priv->host,
			                       client->priv->port));
	}
	else
	{
		GInetAddress *addr;

		addr = g_inet_address_new_any (G_SOCKET_FAMILY_IPV4);

		conn = G_SOCKET_CONNECTABLE (
			g_inet_socket_address_new (addr, client->priv->port));

		g_object_unref (addr);
	}

	enumerator = g_socket_connectable_enumerate (conn);
	g_object_unref (conn);

	while ((sockaddr = g_socket_address_enumerator_next (enumerator,
	                                                     cancellable,
	                                                     error)))
	{
		if (lasterr != NULL)
		{
			g_error_free (lasterr);
		}

		if (client->priv->protocol == CDN_NETWORK_PROTOCOL_UDP ||
		    g_socket_connect (sock,
		                      sockaddr,
		                      cancellable,
		                      &lasterr))
		{
			g_object_ref (sockaddr);
			break;
		}
	}

	g_object_unref (enumerator);

	if (client->priv->protocol != CDN_NETWORK_PROTOCOL_UDP &&
	    !g_socket_is_connected (sock))
	{
		g_object_unref (sock);

		if (lasterr && error && !*error)
		{
			g_propagate_error (error, lasterr);
		}
		else if (lasterr)
		{
			g_error_free (lasterr);
		}

		return FALSE;
	}

	client->priv->client = cdn_client_new (CDN_NODE (client),
	                                       sock,
	                                       sockaddr,
	                                       client->priv->mode,
	                                       client->priv->throttle);

	g_object_unref (sock);
	g_object_unref (sockaddr);

	cdn_client_initialize (client->priv->client);
	return TRUE;
}

static gboolean
cdn_io_finalize_impl (CdnIo         *io,
                      GCancellable  *cancellable,
                      GError       **error)
{
	CdnIoNetworkClient *client = CDN_IO_NETWORK_CLIENT (io);

	if (client->priv->client)
	{
		g_object_unref (client->priv->client);
	}

	return TRUE;
}

static void
cdn_io_update_impl (CdnIo         *io,
                    CdnIntegrator *integrator)
{
	CdnIoNetworkClient *client;

	client = (CdnIoNetworkClient *)io;

	if (client->priv->client)
	{
		cdn_client_update (client->priv->client);
	}
}

static void
cdn_io_iface_init (gpointer iface)
{
	CdnIoInterface *i = iface;

	i->initialize = cdn_io_initialize_impl;
	i->finalize = cdn_io_finalize_impl;
	i->update = cdn_io_update_impl;
}

static void
cdn_io_network_client_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_io_network_client_parent_class)->finalize (object);
}

static void
cdn_io_network_client_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
	CdnIoNetworkClient *self = CDN_IO_NETWORK_CLIENT (object);

	switch (prop_id)
	{
		case PROP_THROTTLE:
			self->priv->throttle = g_value_get_double (value);
			break;
		case PROP_MODE:
			self->priv->mode = g_value_get_flags (value);
			break;
		case PROP_HOST:
			g_free (self->priv->host);
			self->priv->host = g_value_dup_string (value);
			break;
		case PROP_PORT:
			self->priv->port = g_value_get_uint (value);
			break;
		case PROP_PROTOCOL:
			self->priv->protocol = g_value_get_enum (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_network_client_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
	CdnIoNetworkClient *self = CDN_IO_NETWORK_CLIENT (object);

	switch (prop_id)
	{
		case PROP_THROTTLE:
			g_value_set_double (value, self->priv->throttle);
			break;
		case PROP_MODE:
			g_value_set_flags (value, self->priv->mode);
			break;
		case PROP_HOST:
			g_value_set_string (value, self->priv->host);
			break;
		case PROP_PORT:
			g_value_set_uint (value, self->priv->port);
			break;
		case PROP_PROTOCOL:
			g_value_set_enum (value, self->priv->protocol);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_network_client_class_init (CdnIoNetworkClientClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_io_network_client_finalize;

	object_class->get_property = cdn_io_network_client_get_property;
	object_class->set_property = cdn_io_network_client_set_property;

	g_type_class_add_private (object_class, sizeof (CdnIoNetworkClientPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_THROTTLE,
	                                 g_param_spec_double ("throttle",
	                                                      "Throttle",
	                                                      "Throttle",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      0,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_HOST,
	                                 g_param_spec_string ("host",
	                                                      "Host",
	                                                      "Host",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_PORT,
	                                 g_param_spec_uint ("port",
	                                                    "Port",
	                                                    "Port",
	                                                    0,
	                                                    G_MAXUINT,
	                                                    4444,
	                                                    G_PARAM_READWRITE |
	                                                    G_PARAM_CONSTRUCT |
	                                                    G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_PROTOCOL,
	                                 g_param_spec_enum ("protocol",
	                                                    "Protocol",
	                                                    "Protocol",
	                                                    CDN_TYPE_NETWORK_PROTOCOL,
	                                                    CDN_NETWORK_PROTOCOL_TCP,
	                                                    G_PARAM_READWRITE |
	                                                    G_PARAM_CONSTRUCT |
	                                                    G_PARAM_STATIC_STRINGS));

	g_object_class_override_property (object_class,
	                                  PROP_MODE,
	                                  "mode");
}

static void
cdn_io_network_client_class_finalize (CdnIoNetworkClientClass *klass)
{

}

static void
cdn_io_network_client_init (CdnIoNetworkClient *self)
{
	self->priv = CDN_IO_NETWORK_CLIENT_GET_PRIVATE (self);
}

void
cdn_io_network_client_register (GTypeModule *type_module)
{
	cdn_io_network_client_register_type (type_module);
}
