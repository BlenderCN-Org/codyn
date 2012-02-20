#include "cdn-io-wii-manager.h"
#include "cdn-io-wii-common.h"

#include <gio/gio.h>
#include <codyn/cdn-io.h>

#define CDN_IO_WII_MANAGER_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT_WII_MANAGER, CdnIoWiiManagerPrivate))

typedef struct
{
	CdnObject *object;
	CdnIoMode mode;
	CdnVariable *variables[CDN_IO_WII_VARIABLE_NUM];
} Binding;

typedef struct
{
	guint id;
	gchar *addr;
	gchar *name;

	cwiid_wiimote_t *remote;
	GMutex *mutex;
	gboolean tryconnect;

	GSList *objects;
} WiiMote;

struct _CdnIoWiiManagerPrivate
{
	GDBusConnection *connection;
	GDBusProxy *manager;
	GDBusProxy *adapter;
	GCancellable *cancellable;
	GMainLoop *loop;
	GMutex *request_mutex;

	gdouble last_update;

	GSList *wiimotes;
	gdouble throttle;
};

static CdnIoWiiManager *instance = 0;

enum
{
	ADDED,
	REMOVED,
	NUM_SIGNALS
};

static guint signals[NUM_SIGNALS] = {0,};


G_DEFINE_DYNAMIC_TYPE (CdnIoWiiManager,
                       cdn_io_wii_manager,
                       G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_THROTTLE
};

static Binding *
binding_new (CdnObject *object)
{
	Binding *binding;
	gint i;

	binding = g_slice_new0 (Binding);

	binding->object = object;
	binding->mode = cdn_io_get_mode (CDN_IO (object));

	for (i = 0; i < CDN_IO_WII_VARIABLE_NUM; ++i)
	{
		CdnIoWiiVariableInfo const *info;

		info = cdn_io_wii_common_get_variable_info (i);

		binding->variables[i] = cdn_object_get_variable (object,
		                                                 info->name);
	}

	return binding;
}

static void
binding_free (Binding *binding)
{
	g_slice_free (Binding, binding);
}

static WiiMote *
wii_mote_new (gchar const *addr,
              gchar const *name)
{
	WiiMote *ret;

	ret = g_slice_new0 (WiiMote);

	ret->addr = g_strdup (addr);
	ret->name = g_strdup (name);
	ret->mutex = g_mutex_new ();

	return ret;
}

static void
wii_mote_free (WiiMote *self)
{
	g_free (self->addr);
	g_free (self->name);

	g_mutex_free (self->mutex);

	g_slice_free (WiiMote, self);
}

static void
cdn_io_wii_manager_finalize (GObject *object)
{
	CdnIoWiiManager *manager;

	manager = CDN_IO_WII_MANAGER (object);

	if (manager->priv->cancellable)
	{
		g_cancellable_cancel (manager->priv->cancellable);
		g_object_unref (manager->priv->cancellable);
	}

	if (manager->priv->adapter)
	{
		g_object_unref (manager->priv->adapter);
	}

	if (manager->priv->manager)
	{
		g_object_unref (manager->priv->manager);
	}

	if (manager->priv->connection)
	{
		g_object_unref (manager->priv->connection);
	}

	g_mutex_free (manager->priv->request_mutex);

	g_slist_foreach (manager->priv->wiimotes, (GFunc)wii_mote_free, NULL);
	g_slist_free (manager->priv->wiimotes);

	G_OBJECT_CLASS (cdn_io_wii_manager_parent_class)->finalize (object);
}

static void
cdn_io_wii_manager_class_finalize (CdnIoWiiManagerClass *klass)
{
}

static GObject *
cdn_io_wii_manager_constructor (GType                  type,
                                guint                  n_construct_properties,
                                GObjectConstructParam *construct_properties)
{
	GObjectClass *cls;

	if (instance)
	{
		return g_object_ref (instance);
	}

	cls = G_OBJECT_CLASS (cdn_io_wii_manager_parent_class);

	instance = CDN_IO_WII_MANAGER (cls->constructor (type,
	                                                 n_construct_properties,
	                                                 construct_properties));

	g_object_add_weak_pointer (G_OBJECT (instance),
	                           (gpointer *)(&instance));

	return G_OBJECT (instance);
}

static void
cdn_io_wii_manager_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
	CdnIoWiiManager *self = CDN_IO_WII_MANAGER (object);

	switch (prop_id)
	{
		case PROP_THROTTLE:
			self->priv->throttle = g_value_get_double (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_wii_manager_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
	CdnIoWiiManager *self = CDN_IO_WII_MANAGER (object);

	switch (prop_id)
	{
		case PROP_THROTTLE:
			g_value_set_double (value, self->priv->throttle);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_wii_manager_class_init (CdnIoWiiManagerClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_io_wii_manager_finalize;

	object_class->get_property = cdn_io_wii_manager_get_property;
	object_class->set_property = cdn_io_wii_manager_set_property;

	object_class->constructor = cdn_io_wii_manager_constructor;

	signals[ADDED] =
		g_signal_new ("added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__STRING,
		              G_TYPE_NONE,
		              1,
		              G_TYPE_STRING);

	signals[REMOVED] =
		g_signal_new ("removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              0,
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__STRING,
		              G_TYPE_NONE,
		              1,
		              G_TYPE_STRING);

	g_type_class_add_private (object_class, sizeof(CdnIoWiiManagerPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_THROTTLE,
	                                 g_param_spec_double ("throttle",
	                                                      "Throttle",
	                                                      "Throttle",
	                                                      0,
	                                                      G_MAXDOUBLE,
	                                                      0.1,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static gboolean
is_wii_mote (gchar const *name)
{
	return g_str_has_prefix (name, "Nintendo");
}

static void
add_wiimote (CdnIoWiiManager *manager,
             gchar const        *addr,
             gchar const        *name)
{
	WiiMote *mote;
	GSList *item;

	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		mote = item->data;

		if (g_strcmp0 (mote->addr, addr) == 0)
		{
			g_free (mote->name);
			mote->name = g_strdup (name);

			return;
		}
	}

	mote = wii_mote_new (addr,
	                     name);

	manager->priv->wiimotes = g_slist_append (manager->priv->wiimotes,
	                                          mote);

	g_signal_emit (manager, signals[ADDED], 0, addr);
}

static void
remove_wiimote (CdnIoWiiManager *manager,
                WiiMote            *mote)
{
	gchar *addr;

	addr = g_strdup (mote->addr);

	manager->priv->wiimotes = g_slist_remove (manager->priv->wiimotes,
	                                          mote);

	wii_mote_free (mote);

	g_signal_emit (manager, signals[REMOVED], 0, addr);
	g_free (addr);
}

static void
on_adapter_signal (GDBusProxy         *proxy,
                   gchar const        *sender_name,
                   gchar const        *signal_name,
                   GVariant           *parameters,
                   CdnIoWiiManager *manager)
{
	if (g_strcmp0 (signal_name, "DeviceFound") == 0)
	{
		gchar *addr;
		GVariantIter *iter;
		gchar *key;
		GVariant *value;
		gboolean breakit = FALSE;

		g_variant_get (parameters, "(sa{sv})", &addr, &iter);

		while (g_variant_iter_loop (iter, "{sv}", &key, &value))
		{
			if (g_strcmp0 (key, "Name") == 0 &&
			    is_wii_mote (g_variant_get_string (value, NULL)))
			{
				breakit = TRUE;

				add_wiimote (manager, addr, key);
			}

			g_free (key);
			g_variant_unref (value);

			if (breakit)
			{
				break;
			}
		}

		g_free (addr);
	}
	else if (g_strcmp0 (signal_name, "RemoteDeviceDisappeared") == 0)
	{
		GSList *item;
		gchar *addr;

		g_variant_get (parameters, "(s)", &addr);

		for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
		{
			WiiMote *mote;

			mote = item->data;

			if (g_strcmp0 (mote->addr, addr) == 0)
			{
				remove_wiimote (manager, mote);
				break;
			}
		}

		g_free (addr);
	}
}

static void
on_adapter_property (GDBusProxy         *proxy,
                     GVariant           *changed_properties,
                     GStrv              *invalidated_properties,
                     CdnIoWiiManager *manager)
{
	GVariantIter iter;
	gchar *name;

	if (!manager->priv->loop)
	{
		return;
	}

	g_variant_iter_init (&iter, changed_properties);

	while (g_variant_iter_loop (&iter, "s", &name))
	{
		if (g_strcmp0 (name, "Discovering") == 0)
		{
			GVariant *v;

			v = g_dbus_proxy_get_cached_property (proxy, name);

			if (!g_variant_get_boolean (v))
			{
				g_main_loop_quit (manager->priv->loop);
			}

			g_variant_unref (v);

			g_free (name);
			break;
		}

		g_free (name);
	}
}

static void
discovery_finished (GDBusProxy      *proxy,
                    GAsyncResult    *res,
                    CdnIoWiiManager *manager)
{
	GVariant *ret;
	GError *error = NULL;

	ret = g_dbus_proxy_call_finish (proxy, res, &error);

	if (error)
	{
		g_warning ("%s", error->message);
		g_error_free (error);
	}

	if (ret)
	{
		g_variant_unref (ret);
	}
}

static void
discover_devices (CdnIoWiiManager *manager)
{
	if (!manager->priv->adapter)
	{
		return;
	}

	g_dbus_proxy_call (manager->priv->adapter,
	                   "StartDiscovery",
	                   NULL,
	                   G_DBUS_CALL_FLAGS_NONE,
	                   -1,
	                   manager->priv->cancellable,
	                   (GAsyncReadyCallback)discovery_finished,
	                   manager);
}

static void
adapter_finished (GObject            *source,
                  GAsyncResult       *res,
                  CdnIoWiiManager *manager)
{
	GDBusProxy *proxy;
	GError *error = NULL;

	proxy = g_dbus_proxy_new_finish (res, &error);

	if (!proxy)
	{
		if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
		{
			g_warning ("[CdnIoWiiManager] Could not obtain bluez adapter: %s",
			           error->message);
		}

		g_error_free (error);
	}

	if (manager->priv->adapter)
	{
		g_object_unref (manager->priv->adapter);
	}

	manager->priv->adapter = proxy;

	while (manager->priv->wiimotes)
	{
		WiiMote *mote;

		mote = manager->priv->wiimotes->data;
		remove_wiimote (manager, mote);
	}

	if (proxy)
	{
		g_signal_connect (proxy,
		                  "g-signal",
		                  G_CALLBACK (on_adapter_signal),
		                  manager);

		g_signal_connect (proxy,
		                  "g-properties-changed",
		                  G_CALLBACK (on_adapter_property),
		                  manager);

		discover_devices (manager);
	}
}

static void
get_adapter (CdnIoWiiManager *manager,
             gchar const        *objpath)
{
	g_dbus_proxy_new (manager->priv->connection,
	                  G_DBUS_PROXY_FLAGS_NONE,
	                  NULL,
	                  "org.bluez",
	                  objpath,
	                  "org.bluez.Adapter",
	                  manager->priv->cancellable,
	                  (GAsyncReadyCallback)adapter_finished,
	                  manager);
}

static void
adapter_default_finished (GDBusProxy         *proxy,
                          GAsyncResult       *res,
                          CdnIoWiiManager *manager)
{
	GVariant *ret;
	GError *error = NULL;
	gchar *objpath;

	ret = g_dbus_proxy_call_finish (proxy, res, &error);

	if (!ret)
	{
		if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
		{
			g_warning ("[CdnIoWiiManager] Could not obtain bluez default adapter: %s",
			           error->message);
		}

		g_error_free (error);
		return;
	}

	g_variant_get (ret, "(o)", &objpath);
	get_adapter (manager, objpath);

	g_free (objpath);

	g_variant_unref (ret);
}

static void
on_manager_signal (GDBusProxy         *proxy,
                   gchar const        *sender_name,
                   gchar const        *signal_name,
                   GVariant           *parameters,
                   CdnIoWiiManager *manager)
{
	gchar *objpath;

	if (g_strcmp0 (signal_name, "DefaultAdapterChanged") != 0)
	{
		return;
	}

	g_variant_get (parameters, "(s)", &objpath);
	get_adapter (manager, objpath);

	g_free (objpath);
}

static void
manager_finished (GDBusProxy         *proxy,
                  GAsyncResult       *res,
                  CdnIoWiiManager *manager)
{
	GDBusProxy *ret;
	GError *error = NULL;

	ret = g_dbus_proxy_new_finish (res, &error);

	if (!ret)
	{
		if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
		{
			g_warning ("[CdnIoWiiManager] Could not obtain bluez manager: %s",
			           error->message);
		}

		g_error_free (error);
		return;
	}

	manager->priv->manager = ret;

	g_signal_connect (manager->priv->manager,
	                  "g-signal",
	                  G_CALLBACK (on_manager_signal),
	                  manager);

	g_dbus_proxy_call (proxy,
	                   "DefaultAdapter",
	                   NULL,
	                   G_DBUS_CALL_FLAGS_NONE,
	                   -1,
	                   manager->priv->cancellable,
	                   (GAsyncReadyCallback)adapter_default_finished,
	                   manager);
}

static void
bus_connected (GObject            *source,
               GAsyncResult       *res,
               CdnIoWiiManager *manager)
{
	GDBusConnection *connection;
	GError *error = NULL;

	connection = g_bus_get_finish (res, &error);

	if (!connection)
	{
		if (!g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
		{
			g_warning ("[CdnIoWiiManager] Could not connect to dbus system bus: %s",
			           error->message);
		}

		g_error_free (error);
		return;
	}

	manager->priv->connection = connection;

	g_dbus_proxy_new (manager->priv->connection,
	                  G_DBUS_PROXY_FLAGS_NONE,
	                  NULL,
	                  "org.bluez",
	                  "/",
	                  "org.bluez.Manager",
	                  manager->priv->cancellable,
	                  (GAsyncReadyCallback)manager_finished,
	                  manager);
}

static void
cdn_io_wii_manager_init (CdnIoWiiManager *self)
{
	self->priv = CDN_IO_WII_MANAGER_GET_PRIVATE (self);

	self->priv->cancellable = g_cancellable_new ();
	self->priv->request_mutex = g_mutex_new ();

	g_bus_get (G_BUS_TYPE_SYSTEM,
	           self->priv->cancellable,
	           (GAsyncReadyCallback)bus_connected,
	           self);

}

CdnIoWiiManager *
cdn_io_wii_manager_new ()
{
	return g_object_new (CDN_TYPE_INPUT_WII_MANAGER, NULL);
}

void
cdn_io_wii_manager_register (GTypeModule *module)
{
	cdn_io_wii_manager_register_type (module);
}

gchar **
cdn_io_wii_manager_get_remotes (CdnIoWiiManager *manager)
{
	GPtrArray *ret;
	GSList *item;

	g_return_val_if_fail (CDN_IS_INPUT_WII_MANAGER (manager), NULL);

	ret = g_ptr_array_new ();

	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		WiiMote *mote;

		mote = item->data;

		g_ptr_array_add (ret, g_strdup (mote->addr));
	}

	g_ptr_array_add (ret, NULL);
	return (gchar **)g_ptr_array_free (ret, FALSE);
}

gchar const *
cdn_io_wii_manager_get_remote_name (CdnIoWiiManager *manager,
                                       gchar const        *addr)
{
	GSList *item;

	g_return_val_if_fail (CDN_IS_INPUT_WII_MANAGER (manager), NULL);

	if (!addr)
	{
		return NULL;
	}

	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		WiiMote *mote;

		mote = item->data;

		if (g_strcmp0 (mote->addr, addr) == 0)
		{
			return mote->name;
		}
	}

	return NULL;
}

static WiiMote *
find_by_id (CdnIoWiiManager *manager,
            guint               id)
{
	GSList *item;
	WiiMote *deattached = NULL;

	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		WiiMote *mote;

		mote = item->data;

		if (mote->id == id)
		{
			return mote;
		}

		if (mote->id == 0 && !deattached)
		{
			deattached = mote;
		}
	}

	return deattached;
}

static void
bind_remote (CdnIoWiiManager *manager,
             WiiMote            *mote,
             CdnObject          *object)
{
	mote->objects = g_slist_prepend (mote->objects,
	                                 binding_new (object));
}

gboolean
cdn_io_wii_manager_bind_remote (CdnIoWiiManager *manager,
                                   guint               deviceid,
                                   CdnObject          *object)
{
	WiiMote *mote;
	bdaddr_t *addr;

	g_return_val_if_fail (CDN_IS_INPUT_WII_MANAGER (manager), FALSE);

	g_mutex_lock (manager->priv->request_mutex);
	mote = find_by_id (manager, deviceid);

	if (!mote)
	{
		/* Start a discovery now and block */
		manager->priv->loop = g_main_loop_new (NULL, FALSE);
		discover_devices (manager);

		g_main_loop_run (manager->priv->loop);
		g_object_unref (manager->priv->loop);

		manager->priv->loop = NULL;

		mote = find_by_id (manager, deviceid);
	}

	g_mutex_unlock (manager->priv->request_mutex);

	g_mutex_lock (mote->mutex);

	if (mote->remote != NULL)
	{
		bind_remote (manager, mote, object);
		g_mutex_unlock (mote->mutex);
		return TRUE;
	}

	if (mote->tryconnect)
	{
		g_mutex_unlock (mote->mutex);
		return FALSE;
	}

	/* Try to connect */
	addr = strtoba (mote->addr);
	mote->remote = cwiid_open (addr, CWIID_FLAG_NONBLOCK | CWIID_FLAG_CONTINUOUS);
	g_free (addr);
	mote->tryconnect = TRUE;

	if (mote->remote)
	{
		bind_remote (manager, mote, object);
	}

	g_mutex_unlock (mote->mutex);
	return TRUE;
}

void
cdn_io_wii_manager_reset (CdnIoWiiManager *manager)
{
	GSList *item;

	g_return_if_fail (CDN_IS_INPUT_WII_MANAGER (manager));

	if (!g_mutex_trylock (manager->priv->request_mutex))
	{
		return;
	}

	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		WiiMote *mote;

		mote = item->data;

		mote->tryconnect = FALSE;

		if (mote->remote)
		{
			cwiid_close (mote->remote);
			mote->remote = NULL;
		}

		mote->id = 0;
		g_slist_foreach (mote->objects, (GFunc)binding_free, NULL);
		g_slist_free (mote->objects);
		mote->objects = NULL;
	}

	g_mutex_unlock (manager->priv->request_mutex);
}

static void
set_binding_values (Binding *binding,
                    gint     i,
                    gdouble *vals,
                    gint     numr,
                    gint     numc)
{
	if (binding->variables[i])
	{
		cdn_variable_set_values (binding->variables[i],
		                         vals,
		                         numr,
		                         numc);
	}
}

static void
set_binding_value (Binding *binding,
                   gint     i,
                   gdouble  val)
{
	if (binding->variables[i])
	{
		cdn_variable_set_value (binding->variables[i], val);
	}
}

static void
process_button_message (CdnIoWiiManager    *manager,
                        Binding               *binding,
                        struct cwiid_btn_mesg *msg)
{
	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_1,
	                   msg->buttons & CWIID_BTN_1);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_2,
	                   msg->buttons & CWIID_BTN_2);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_A,
	                   msg->buttons & CWIID_BTN_A);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_B,
	                   msg->buttons & CWIID_BTN_B);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_DOWN,
	                   msg->buttons & CWIID_BTN_DOWN);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_UP,
	                   msg->buttons & CWIID_BTN_UP);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_LEFT,
	                   msg->buttons & CWIID_BTN_RIGHT);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_RIGHT,
	                   msg->buttons & CWIID_BTN_RIGHT);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_HOME,
	                   msg->buttons & CWIID_BTN_HOME);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_PLUS,
	                   msg->buttons & CWIID_BTN_PLUS);

	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BUTTON_MIN,
	                   msg->buttons & CWIID_BTN_MINUS);
}

static void
process_ir_message (CdnIoWiiManager   *manager,
                    Binding              *binding,
                    struct cwiid_ir_mesg *msg)
{
	gint i;
	gdouble ir[CWIID_IR_SRC_COUNT * 2];
	gdouble ir_active[CWIID_IR_SRC_COUNT];
	gdouble ir_size[CWIID_IR_SRC_COUNT];

	for (i = 0; i < CWIID_IR_SRC_COUNT; ++i)
	{
		gdouble x = 0;
		gdouble y = 0;
		gdouble active = 0;
		gdouble size = 0;

		if (msg->src[i].valid)
		{
			x = msg->src[i].pos[0] / (gdouble)CWIID_IR_X_MAX;
			y = msg->src[i].pos[1] / (gdouble)CWIID_IR_Y_MAX;

			active = 1;
			size = msg->src[i].size;
		}

		ir[i] = x;
		ir[i + CWIID_IR_SRC_COUNT] = y;
		ir_active[i] = active;
		ir_size[i] = size;
	}

	set_binding_values (binding,
	                    CDN_IO_WII_VARIABLE_IR_ACTIVE,
	                    ir_active,
	                    CWIID_IR_SRC_COUNT,
	                    2);

	set_binding_values (binding,
	                    CDN_IO_WII_VARIABLE_IR_SIZE,
	                    ir_size,
	                    CWIID_IR_SRC_COUNT,
	                    1);

	set_binding_values (binding,
	                    CDN_IO_WII_VARIABLE_IR,
	                    ir,
	                    CWIID_IR_SRC_COUNT,
	                    1);
}

static void
process_acc_message (CdnIoWiiManager    *manager,
                     Binding               *binding,
                     struct cwiid_acc_mesg *acc)
{
	gint i;
	gdouble values[3];

	for (i = 0; i < 3; ++i)
	{
		values[i] = acc->acc[i] / (gdouble)CWIID_ACC_MAX;
	}

	set_binding_values (binding,
	                    CDN_IO_WII_VARIABLE_ACCELERATION,
	                    values,
	                    3,
	                    1);
}

static void
process_status_message (CdnIoWiiManager       *manager,
                        Binding                  *binding,
                        struct cwiid_status_mesg *status)
{
	set_binding_value (binding,
	                   CDN_IO_WII_VARIABLE_BATTERY,
	                   status->battery / (gdouble)CWIID_ACC_MAX);
}

static void
process_messages (CdnIoWiiManager *manager,
                  WiiMote            *mote,
                  union cwiid_mesg  **messages,
                  gint                num)
{
	gint i;
	GSList *item;

	for (item = mote->objects; item; item = g_slist_next (item))
	{
		Binding *binding;

		binding = item->data;

		if (!(binding->mode & CDN_IO_MODE_INPUT))
		{
			continue;
		}

		for (i = 0; i < num; ++i)
		{
			union cwiid_mesg *msg;

			msg = messages[i];

			switch (msg->type)
			{
				case CWIID_MESG_BTN:
					process_button_message (manager,
					                        binding,
					                        &msg->btn_mesg);
				break;
				case CWIID_MESG_IR:
					process_ir_message (manager,
					                    binding,
					                    &msg->ir_mesg);
				break;
				case CWIID_MESG_ACC:
					process_acc_message (manager,
					                     binding,
					                     &msg->acc_mesg);
				break;
				case CWIID_MESG_STATUS:
					process_status_message (manager,
					                        binding,
					                        &msg->status_mesg);
				break;
				default:
				break;
			}
		}
	}
}

static void
set_led_rumble (CdnIoWiiManager *manager,
                WiiMote            *mote)
{
	GSList *item;

	for (item = mote->objects; item; item = g_slist_next (item))
	{
		Binding *binding;
		gdouble val;

		binding = item->data;

		if (binding->variables[CDN_IO_WII_VARIABLE_LED])
		{
			val = cdn_variable_get_value (binding->variables[CDN_IO_WII_VARIABLE_LED]);
			cwiid_set_led (mote->remote, (uint8_t)MIN (MAX (val, 0), 255));
		}

		if (binding->variables[CDN_IO_WII_VARIABLE_RUMBLE])
		{
			val = cdn_variable_get_value (binding->variables[CDN_IO_WII_VARIABLE_RUMBLE]);
			cwiid_set_rumble (mote->remote, (uint8_t)MIN (MAX (val * 255, 0), 255));
		}
	}
}

void
cdn_io_wii_manager_update (CdnIoWiiManager *manager,
                              CdnIntegrator      *integrator)
{
	GSList *item;
	struct timespec timestamp;

	if (manager->priv->last_update == cdn_integrator_get_time (integrator))
	{
		return;
	}

	/* Omit type check for speed */
	for (item = manager->priv->wiimotes; item; item = g_slist_next (item))
	{
		WiiMote *mote;
		union cwiid_mesg *messages;
		gint num = 0;

		mote = item->data;

		if (!mote->remote)
		{
			continue;
		}

		if (cwiid_get_mesg (mote->remote, &num, &messages, &timestamp) == 0)
		{
			if (num > 0)
			{
				process_messages (manager, mote, &messages, num);
			}
		}

		/* Throttle setting led/rumbling */
		if (cdn_integrator_get_time (integrator) - manager->priv->last_update >=
		    manager->priv->throttle)
		{
			set_led_rumble (manager, mote);
		}
	}

	manager->priv->last_update = cdn_integrator_get_time (integrator);
}
