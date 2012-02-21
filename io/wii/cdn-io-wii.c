#include "cdn-io-wii.h"
#include "cdn-io-wii-manager.h"
#include "cdn-io-wii-common.h"
#include <codyn/cdn-io.h>
#include <gio/gio.h>

#define CDN_IO_WII_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IO_WII, CdnIoWiiPrivate))

struct _CdnIoWiiPrivate
{
	CdnIoWiiManager *manager;
	cwiid_wiimote_t *remote;
	CdnIoMode mode;

	CdnVariable *variables[CDN_IO_WII_VARIABLE_NUM];
};

static void cdn_io_iface_init (gpointer iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnIoWii,
                                cdn_io_wii,
                                CDN_TYPE_NODE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (CDN_TYPE_IO,
                                                               cdn_io_iface_init))

enum
{
	PROP_0,
	PROP_MODE
};

GQuark
cdn_io_wii_error_quark (void)
{
	static GQuark quark = {0,};

	if (G_UNLIKELY (!quark))
	{
		quark = g_quark_from_string ("cpg-input-wii-error");
	}

	return quark;
}

static void
cdn_io_wii_finalize (GObject *object)
{
	CdnIoWii *wii;

	wii = CDN_IO_WII (object);

	g_object_unref (wii->priv->manager);

	G_OBJECT_CLASS (cdn_io_wii_parent_class)->finalize (object);
}

static void
cdn_io_wii_class_finalize (CdnIoWiiClass *klass)
{
}

static gboolean
cdn_io_initialize_impl (CdnIo         *io,
                        GCancellable  *cancellable,
                        GError       **error)
{
	CdnIoWii *wii;
	gint deviceid;

	wii = CDN_IO_WII (io);

	deviceid = (gint)cdn_variable_get_value (wii->priv->variables[CDN_IO_WII_VARIABLE_ID]);

	if (!cdn_io_wii_manager_bind_remote (wii->priv->manager,
	                                     deviceid + 1,
	                                     CDN_OBJECT (io)))
	{
		g_set_error (error,
		             CDN_IO_WII_ERROR,
		             CDN_IO_WII_ERROR_DEVICE_NOT_FOUND,
		             "The WiiMote for input device %d could not be found...",
		             deviceid);

		return FALSE;
	}

	return TRUE;
}

static gboolean
cdn_io_finalize_impl (CdnIo         *io,
                      GCancellable  *cancellable,
                      GError       **error)
{
	CdnIoWii *wii = CDN_IO_WII (io);

	cdn_io_wii_manager_reset (wii->priv->manager);

	return TRUE;
}

static void
cdn_io_update_impl (CdnIo        *io,
                    CdnIntegrator *integrator)
{
	CdnIoWii *wii = CDN_IO_WII (io);

	cdn_io_wii_manager_update (wii->priv->manager, integrator);
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
add_variable (CdnIoWii                   *wii,
              CdnIoMode                   mode,
              guint                       id,
              CdnIoWiiVariableInfo const *info,
              gchar const                *value)
{
	CdnVariable *v;
	CdnExpression *e;

	if (((info->flags & CDN_VARIABLE_FLAG_IN) &&
	    !(mode & CDN_IO_MODE_INPUT)) ||
	    ((info->flags & CDN_VARIABLE_FLAG_OUT) &&
	    !(mode & CDN_IO_MODE_OUTPUT)))
	{
		wii->priv->variables[id] = NULL;
		return;
	}

	if (!value)
	{
		gchar *expr;

		expr = g_strdup_printf ("zeros(%d, %d)",
		                        info->dimension[0],
		                        info->dimension[1]);

		e = cdn_expression_new (expr);

		g_free (expr);
	}
	else
	{
		e = cdn_expression_new (value);
	}

	v = cdn_variable_new (info->name,
	                      e,
	                      info->flags);

	cdn_object_add_variable (CDN_OBJECT (wii), v, NULL);

	wii->priv->variables[id] = v;
}

static void
cdn_io_wii_constructed (GObject *object)
{
	CdnIoWii *self;
	gint i;
	CdnIoMode mode;

	self = CDN_IO_WII (object);

	mode = cdn_io_get_mode (CDN_IO (object));

	self->priv->manager = cdn_io_wii_manager_new ();

	/* Generate fixed properties */
	for (i = 0; i < CDN_IO_WII_VARIABLE_NUM; ++i)
	{
		CdnIoWiiVariableInfo const *info;

		info = cdn_io_wii_common_get_variable_info (i);

		add_variable (self,
		              mode,
		              i,
		              info,
		              i == CDN_IO_WII_VARIABLE_LED ? "id + 1" : NULL);
	}
}

static void
cdn_io_wii_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
	CdnIoWii *self = CDN_IO_WII (object);

	switch (prop_id)
	{
		case PROP_MODE:
			self->priv->mode = g_value_get_flags (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_wii_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
	CdnIoWii *self = CDN_IO_WII (object);

	switch (prop_id)
	{
		case PROP_MODE:
			g_value_set_flags (value, self->priv->mode);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_io_wii_class_init (CdnIoWiiClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_io_wii_finalize;

	object_class->constructed = cdn_io_wii_constructed;

	object_class->get_property = cdn_io_wii_get_property;
	object_class->set_property = cdn_io_wii_set_property;

	g_object_class_override_property (object_class,
	                                  PROP_MODE,
	                                  "mode");

	g_type_class_add_private (object_class, sizeof(CdnIoWiiPrivate));
}

static void
cdn_io_wii_init (CdnIoWii *self)
{
	self->priv = CDN_IO_WII_GET_PRIVATE (self);
}

void
cdn_io_wii_register (GTypeModule *module)
{
	cdn_io_wii_register_type (module);
}
