#include "cdn-network-thread.h"

#define CDN_NETWORK_THREAD_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_NETWORK_THREAD, CdnNetworkThreadPrivate))

G_LOCK_DEFINE_STATIC (instance_mutex);

CdnNetworkThread *instance = NULL;

struct _CdnNetworkThreadPrivate
{
	GThread *thread;
	GMainContext *context;
	GMainLoop *loop;
	GMutex *rmutex;

	GHashTable *socket_table;
};

G_DEFINE_DYNAMIC_TYPE (CdnNetworkThread, cdn_network_thread, G_TYPE_OBJECT)

static void
cdn_network_thread_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_network_thread_parent_class)->finalize (object);
}

static void
cdn_network_thread_class_init (CdnNetworkThreadClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_network_thread_finalize;

	g_type_class_add_private (object_class, sizeof (CdnNetworkThreadPrivate));
}

static void
cdn_network_thread_class_finalize (CdnNetworkThreadClass *klass)
{
}

static void
cdn_network_thread_init (CdnNetworkThread *self)
{
	self->priv = CDN_NETWORK_THREAD_GET_PRIVATE (self);

	self->priv->rmutex = g_mutex_new ();

	self->priv->socket_table =
		g_hash_table_new_full (g_direct_hash,
		                       g_direct_equal,
		                       (GDestroyNotify)g_object_unref,
		                       NULL);
}

static gpointer
run_in_thread (CdnNetworkThread *self)
{
	g_main_context_push_thread_default (self->priv->context);
	g_main_loop_run (self->priv->loop);

	return NULL;
}

void
cdn_network_thread_register (CdnNetworkThread  *self,
                             GSocket           *socket,
                             GSocketSourceFunc  callback,
                             gpointer           user_data,
                             GDestroyNotify     destroy_notify)
{
	GSource *source;
	guint id;

	g_mutex_lock (self->priv->rmutex);

	if (g_hash_table_lookup_extended (self->priv->socket_table,
	                                  socket,
	                                  NULL,
	                                  NULL))
	{
		g_mutex_unlock (self->priv->rmutex);
		return;
	}

	if (self->priv->context == NULL)
	{
		self->priv->context = g_main_context_new ();
		self->priv->loop = g_main_loop_new (self->priv->context,
		                                    FALSE);

		self->priv->thread = g_thread_create ((GThreadFunc)run_in_thread,
		                                      self,
		                                      TRUE,
		                                      NULL);
	}

	source = g_socket_create_source (socket,
	                                 G_IO_IN,
	                                 NULL);

	g_source_set_callback (source,
	                       (GSourceFunc)callback,
	                       user_data,
	                       destroy_notify);

	id = g_source_attach (source, self->priv->context);

	g_hash_table_insert (self->priv->socket_table,
	                     g_object_ref (socket),
	                     GINT_TO_POINTER ((gint)id));

	g_mutex_unlock (self->priv->rmutex);
}

void
cdn_network_thread_unregister (CdnNetworkThread *self,
                               GSocket          *socket)
{
	GSource *source;
	gpointer ptr;
	guint id;

	g_mutex_lock (self->priv->rmutex);

	if (!g_hash_table_lookup_extended (self->priv->socket_table,
	                                   socket,
	                                   NULL,
	                                   &ptr))
	{
		g_mutex_unlock (self->priv->rmutex);
		return;
	}

	id = (guint)GPOINTER_TO_INT (ptr);

	source = g_main_context_find_source_by_id (self->priv->context, id);

	if (source)
	{
		g_source_destroy (source);
	}

	g_hash_table_remove (self->priv->socket_table, socket);

	if (g_hash_table_size (self->priv->socket_table) == 0)
	{
		g_main_loop_quit (self->priv->loop);

		g_thread_join (self->priv->thread);
		self->priv->thread = NULL;

		g_main_loop_unref (self->priv->loop);
		self->priv->loop = NULL;

		g_main_context_unref (self->priv->context);
		self->priv->context = NULL;
	}

	g_mutex_unlock (self->priv->rmutex);
}

CdnNetworkThread *
cdn_network_thread_get_default ()
{
	G_LOCK (instance_mutex);

	if (!instance)
	{
		instance = g_object_new (CDN_TYPE_NETWORK_THREAD, NULL);
		g_object_add_weak_pointer (G_OBJECT (instance),
		                           (gpointer *)&instance);
	}

	G_UNLOCK (instance_mutex);

	return instance;
}

void
_cdn_network_thread_register (GTypeModule *type_module)
{
	cdn_network_thread_register_type (type_module);
}
