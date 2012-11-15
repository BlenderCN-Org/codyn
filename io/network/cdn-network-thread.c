#include "cdn-network-thread.h"

#define CDN_NETWORK_THREAD_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_NETWORK_THREAD, CdnNetworkThreadPrivate))

G_LOCK_DEFINE_STATIC (instance_mutex);

CdnNetworkThread *instance = NULL;

struct _CdnNetworkThreadPrivate
{
	GThread *thread;
	GMainContext *context;
	GMainLoop *loop;

#if GLIB_CHECK_VERSION(2, 32, 0)
	GMutex rmutex;
#else
	GMutex *rmutex;
#endif

	GHashTable *socket_table;
};

G_DEFINE_DYNAMIC_TYPE (CdnNetworkThread, cdn_network_thread, G_TYPE_OBJECT)

static void
cdn_network_thread_finalize (GObject *object)
{
	CdnNetworkThread *self = CDN_NETWORK_THREAD (object);

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_clear (&self->priv->rmutex);
#else
	g_mutex_free (self->priv->rmutex);
#endif

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

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_init (&self->priv->rmutex);
#else
	self->priv->rmutex = g_mutex_new ();
#endif

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

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_lock (&self->priv->rmutex);
#else
	g_mutex_lock (self->priv->rmutex);
#endif

	if (g_hash_table_lookup_extended (self->priv->socket_table,
	                                  socket,
	                                  NULL,
	                                  NULL))
	{
#if GLIB_CHECK_VERSION(2, 32, 0)
		g_mutex_unlock (&self->priv->rmutex);
#else
		g_mutex_unlock (self->priv->rmutex);
#endif

		return;
	}

	if (self->priv->context == NULL)
	{
		self->priv->context = g_main_context_new ();
		self->priv->loop = g_main_loop_new (self->priv->context,
		                                    FALSE);

#if GLIB_CHECK_VERSION(2, 32, 0)
		self->priv->thread = g_thread_new ("cdn-network-thread",
		                                   (GThreadFunc)run_in_thread,
		                                   NULL);
#else
		self->priv->thread = g_thread_create ((GThreadFunc)run_in_thread,
		                                      self,
		                                      TRUE,
		                                      NULL);
#endif
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

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_unlock (&self->priv->rmutex);
#else
	g_mutex_unlock (self->priv->rmutex);
#endif
}

void
cdn_network_thread_unregister (CdnNetworkThread *self,
                               GSocket          *socket)
{
	GSource *source;
	gpointer ptr;
	guint id;

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_lock (&self->priv->rmutex);
#else
	g_mutex_lock (self->priv->rmutex);
#endif

	if (!g_hash_table_lookup_extended (self->priv->socket_table,
	                                   socket,
	                                   NULL,
	                                   &ptr))
	{
#if GLIB_CHECK_VERSION(2, 32, 0)
		g_mutex_unlock (&self->priv->rmutex);
#else
		g_mutex_unlock (self->priv->rmutex);
#endif

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

#if GLIB_CHECK_VERSION(2, 32, 0)
	g_mutex_unlock (&self->priv->rmutex);
#else
	g_mutex_unlock (self->priv->rmutex);
#endif
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
