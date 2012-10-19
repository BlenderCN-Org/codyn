#include "cdn-io-network-server.h"
#include <gio/gio.h>
#include <codyn/cdn-io.h>
#include <codyn/cdn-network.h>
#include "cdn-client.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_GIO_UNIX
#include <gio/gunixsocketaddress.h>
#endif

#include "cdn-network-thread.h"

#define CDN_IO_NETWORK_SERVER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IO_NETWORK_SERVER, CdnIoNetworkServerPrivate))

struct _CdnIoNetworkServerPrivate
{
	GSocket *socket;
	GSList *clients;
	GMutex *client_mutex;
	gint wait;
	GCond *wait_condition;
	gint num_wait;
};

static void cdn_io_iface_init (gpointer iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnIoNetworkServer,
                                cdn_io_network_server,
                                CDN_TYPE_IO_NETWORK_CLIENT,
                                0,
                                G_IMPLEMENT_INTERFACE (CDN_TYPE_IO,
                                                       cdn_io_iface_init))

enum
{
	PROP_0,
	PROP_WAIT
};

static void
on_client_closed (CdnClient          *client,
                  CdnIoNetworkServer *server)
{
	GSList *item;

	g_mutex_lock (server->priv->client_mutex);

	item = g_slist_find (server->priv->clients, client);

	if (item)
	{
		g_object_unref (client);

		server->priv->clients =
			g_slist_delete_link (server->priv->clients,
			                     item);
	}

	g_mutex_unlock (server->priv->client_mutex);
}

static CdnClient *
accept_client (CdnIoNetworkServer *server,
               GSocket            *cs)
{
	CdnClient *client;
	CdnIoMode mode;
	gdouble throttle;

	g_object_get (server, "mode", &mode, "throttle", &throttle, NULL);

	client = cdn_client_new (CDN_NODE (server),
	                         cs,
	                         NULL,
	                         mode,
	                         throttle);

	server->priv->clients = g_slist_prepend (server->priv->clients,
	                                         client);

	g_signal_connect (client,
	                  "closed",
	                  G_CALLBACK (on_client_closed),
	                  server);

	cdn_client_initialize (client);

	if (server->priv->num_wait > 0)
	{
		--server->priv->num_wait;

		if (server->priv->num_wait == 0)
		{
			g_cond_signal (server->priv->wait_condition);
		}
	}

	return client;
}

static gboolean
on_accept (GSocket            *socket,
           GIOCondition        condition,
           CdnIoNetworkServer *server)
{
	GSocket *cs;
	CdnIoMode mode;
	gdouble throttle;

	if (!(condition & G_IO_IN))
	{
		return FALSE;
	}

	g_object_get (server, "mode", &mode, "throttle", &throttle, NULL);

	while ((cs = g_socket_accept (socket, NULL, NULL)))
	{
		g_mutex_lock (server->priv->client_mutex);

		accept_client (server, cs);

		g_mutex_unlock (server->priv->client_mutex);

		g_object_unref (cs);
	}

	return TRUE;
}

static gboolean
cdn_io_initialize_impl (CdnIo         *io,
                        GCancellable  *cancellable,
                        GError       **error)
{
	// Create a new socket to start listening on
	CdnIoNetworkServer *server = CDN_IO_NETWORK_SERVER (io);
	GSocket *sock;
	GSocketAddress *sockaddr = NULL;

	gchar *host;
	guint port;
	CdnNetworkProtocol protocol;

	g_object_get (server,
	              "host", &host,
	              "port", &port,
	              "protocol", &protocol,
	              NULL);

#ifdef ENABLE_GIO_UNIX
	if (host == NULL &&
	    protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (server));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "Please specify a unix path in the `host' setting of `%s'",
		             id);

		g_free (id);
		return FALSE;
	}
#endif

#ifdef ENABLE_GIO_UNIX
	if (protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		sock = g_socket_new (G_SOCKET_FAMILY_UNIX,
		                     G_SOCKET_TYPE_STREAM,
		                     0,
		                     error);
	}
	else
#endif
	if (protocol == CDN_NETWORK_PROTOCOL_TCP)
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
		g_free (host);
		return FALSE;
	}

#ifdef ENABLE_GIO_UNIX
	if (protocol == CDN_NETWORK_PROTOCOL_UNIX)
	{
		sockaddr = g_unix_socket_address_new (host);
	}
	else
#endif
	if (host != NULL)
	{
		GInetAddress *addr;

		addr = g_inet_address_new_from_string (host);

		if (addr)
		{
			sockaddr = g_inet_socket_address_new (addr, port);
			g_object_unref (addr);
		}
	}
	else
	{
		GInetAddress *addr;

		addr = g_inet_address_new_any (G_SOCKET_FAMILY_IPV4);

		if (addr)
		{
			sockaddr = g_inet_socket_address_new (addr, port);

			g_object_unref (addr);
		}
	}

	g_free (host);

	if (sockaddr == NULL)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (server));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "Invalid host specification for network server `%s'",
		             id);

		g_free (id);

		g_object_unref (sock);

		return FALSE;
	}

	if (!g_socket_bind (sock, sockaddr, TRUE, error))
	{
		g_object_unref (sockaddr);
		g_object_unref (sock);
		return FALSE;
	}

	g_object_unref (sockaddr);

	if (protocol != CDN_NETWORK_PROTOCOL_UDP)
	{
		if (!g_socket_listen (sock, error))
		{
			g_object_unref (sock);
			return FALSE;
		}

		g_socket_set_blocking (sock, FALSE);
		server->priv->socket = sock;
	}

	g_mutex_lock (server->priv->client_mutex);

	if (protocol != CDN_NETWORK_PROTOCOL_UDP)
	{
		// Add socket to network thread
		cdn_network_thread_register (cdn_network_thread_get_default (),
		                             sock,
		                             (GSocketSourceFunc)on_accept,
		                             server,
		                             NULL);

		if (server->priv->wait > 0)
		{
			server->priv->num_wait = server->priv->wait;

			g_cond_wait (server->priv->wait_condition,
			             server->priv->client_mutex);
		}
		else
		{
			server->priv->num_wait = 0;
		}
	}
	else
	{
		accept_client (server, sock);
		g_object_unref (sock);

		server->priv->num_wait = 0;
	}

	g_mutex_unlock (server->priv->client_mutex);

	return TRUE;
}

static void
remove_client (CdnClient          *client,
               CdnIoNetworkServer *server)
{
	g_signal_handlers_disconnect_by_func (client,
	                                      G_CALLBACK (on_client_closed),
	                                      server);

	cdn_client_close (client);
	g_object_unref (client);
}

static void
do_finalize (CdnIoNetworkServer *server)
{
	g_mutex_lock (server->priv->client_mutex);

	g_slist_foreach (server->priv->clients, (GFunc)remove_client, server);
	g_slist_free (server->priv->clients);

	server->priv->clients = NULL;

	server->priv->num_wait = 0;

	g_mutex_unlock (server->priv->client_mutex);

	if (server->priv->socket)
	{
		cdn_network_thread_unregister (cdn_network_thread_get_default (),
		                               server->priv->socket);

		g_object_unref (server->priv->socket);
		server->priv->socket = NULL;
	}
}

static gboolean
cdn_io_finalize_impl (CdnIo         *io,
                      GCancellable  *cancellable,
                      GError       **error)
{
	do_finalize (CDN_IO_NETWORK_SERVER (io));
	return TRUE;
}

static void
cdn_io_update_impl (CdnIo         *io,
                    CdnIntegrator *integrator)
{
	CdnIoNetworkServer *server;
	GSList *clients;

	server = (CdnIoNetworkServer *)io;

	g_mutex_lock (server->priv->client_mutex);
	clients = g_slist_copy (server->priv->clients);
	g_mutex_unlock (server->priv->client_mutex);

	while (clients)
	{
		cdn_client_update (clients->data);
		clients = g_slist_delete_link (clients, clients);
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
cdn_io_network_server_finalize (GObject *object)
{
	CdnIoNetworkServer *server;

	server = CDN_IO_NETWORK_SERVER (object);

	do_finalize (server);

	g_mutex_free (server->priv->client_mutex);
	g_cond_free (server->priv->wait_condition);

	G_OBJECT_CLASS (cdn_io_network_server_parent_class)->finalize (object);
}

static void
cdn_io_network_server_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
	CdnIoNetworkServer *self = CDN_IO_NETWORK_SERVER (object);

	switch (prop_id)
	{
		case PROP_WAIT:
			self->priv->wait = g_value_get_int (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_network_server_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
	CdnIoNetworkServer *self = CDN_IO_NETWORK_SERVER (object);

	switch (prop_id)
	{
		case PROP_WAIT:
			g_value_set_int (value, self->priv->wait);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_network_server_class_init (CdnIoNetworkServerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_io_network_server_finalize;

	object_class->get_property = cdn_io_network_server_get_property;
	object_class->set_property = cdn_io_network_server_set_property;

	g_type_class_add_private (object_class, sizeof (CdnIoNetworkServerPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_WAIT,
	                                 g_param_spec_int ("wait",
	                                                   "Wait",
	                                                   "Wait",
	                                                   0,
	                                                   G_MAXINT,
	                                                   0,
	                                                   G_PARAM_READWRITE |
	                                                   G_PARAM_STATIC_STRINGS |
	                                                   G_PARAM_CONSTRUCT));
}

static void
cdn_io_network_server_class_finalize (CdnIoNetworkServerClass *klass)
{

}

static void
cdn_io_network_server_init (CdnIoNetworkServer *self)
{
	self->priv = CDN_IO_NETWORK_SERVER_GET_PRIVATE (self);

	self->priv->client_mutex = g_mutex_new ();
	self->priv->wait_condition = g_cond_new ();
}

void
cdn_io_network_server_register (GTypeModule *type_module)
{
	cdn_io_network_server_register_type (type_module);
}
