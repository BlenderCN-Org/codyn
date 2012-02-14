#include "cdn-input-method.h"
#include "cdn-input.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define CDN_INPUT_METHOD_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_INPUT_METHOD, CdnInputMethodPrivate))

static GSList *input_methods = NULL;

struct _CdnInputMethodPrivate
{
	GModule *module;
	gchar *path;
};

G_DEFINE_TYPE (CdnInputMethod, cdn_input_method, G_TYPE_TYPE_MODULE)

enum
{
	PROP_0,
	PROP_PATH
};

static void
cdn_input_method_finalize (GObject *object)
{
	CdnInputMethod *method;

	method = CDN_INPUT_METHOD (object);

	g_free (method->priv->path);

	G_OBJECT_CLASS (cdn_input_method_parent_class)->finalize (object);
}

static gboolean
cdn_input_method_load (GTypeModule *module)
{
	CdnInputMethod *method;
	gpointer sym;

	method = CDN_INPUT_METHOD (module);
	method->priv->module = g_module_open (method->priv->path, 0);

	if (method->priv->module)
	{
		if (g_module_symbol (method->priv->module,
		                     "cdn_input_method_register_types",
		                     &sym))
		{
			CdnInputMethodRegisterTypesFunc func = sym;

			func (module);

			return TRUE;
		}
	}

	return FALSE;
}

static void
cdn_input_method_unload (GTypeModule *module)
{
	CdnInputMethod *method;

	method = CDN_INPUT_METHOD (module);

	if (method->priv->module)
	{
		g_module_close (method->priv->module);
		method->priv->module = NULL;
	}
}

static void
cdn_input_method_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CdnInputMethod *self = CDN_INPUT_METHOD (object);

	switch (prop_id)
	{
		case PROP_PATH:
			self->priv->path = g_value_dup_string (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_input_method_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CdnInputMethod *self = CDN_INPUT_METHOD (object);

	switch (prop_id)
	{
		case PROP_PATH:
			g_value_set_string (value, self->priv->path);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_input_method_class_init (CdnInputMethodClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GTypeModuleClass *module_class = G_TYPE_MODULE_CLASS (klass);

	object_class->finalize = cdn_input_method_finalize;

	object_class->get_property = cdn_input_method_get_property;
	object_class->set_property = cdn_input_method_set_property;

	module_class->load = cdn_input_method_load;
	module_class->unload = cdn_input_method_unload;

	g_type_class_add_private (object_class, sizeof(CdnInputMethodPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_PATH,
	                                 g_param_spec_string ("path",
	                                                      "Path",
	                                                      "Path",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_input_method_init (CdnInputMethod *self)
{
	self->priv = CDN_INPUT_METHOD_GET_PRIVATE (self);
}

CdnInputMethod *
cdn_input_method_new (gchar const *path)
{
	return g_object_new (CDN_TYPE_INPUT_METHOD,
	                     "path", path,
	                     NULL);
}

static GType
find_input_type (gchar const *name,
                 GType        basetype)
{
	GType *children;
	guint num;
	GType ret;
	gchar *comp;
	gint i;

	children = g_type_children (basetype, &num);

	comp = g_strconcat ("cdninput", name, NULL);
	ret = G_TYPE_INVALID;

	for (i = 0; i < num; ++i)
	{
		gchar *low;

		low = g_ascii_strdown (g_type_name (children[i]), -1);

		if (g_strcmp0 (low, comp) == 0)
		{
			ret = children[i];
		}

		g_free (low);

		if (ret == G_TYPE_INVALID)
		{
			ret = find_input_type (name, children[i]);
		}

		if (ret != G_TYPE_INVALID)
		{
			break;
		}
	}

	g_free (comp);
	g_free (children);

	return ret;
}

GType
cdn_input_method_find (gchar const *name)
{
	return find_input_type (name, CDN_TYPE_INPUT);
}

static void
load_input_method (gchar const *path)
{
	CdnInputMethod *method;

	method = cdn_input_method_new (path);

	if (g_type_module_use (G_TYPE_MODULE (method)))
	{
		input_methods = g_slist_prepend (input_methods,
		                                 method);
	}
	else
	{
		g_object_unref (method);
	}
}

static void
load_input_methods (gchar const *dir)
{
	GDir *d;
	gchar const *name;

	d = g_dir_open (dir, 0, NULL);

	if (!d)
	{
		return;
	}

	while ((name = g_dir_read_name (d)) != NULL)
	{
		gchar *path;

		if (!g_str_has_suffix (name, G_MODULE_SUFFIX))
		{
			continue;
		}

		path = g_build_filename (dir, name, NULL);
		load_input_method (path);

		g_free (path);
	}

	g_dir_close (d);
}

static void
load_all_input_methods ()
{
	gchar const *e;
	gchar *path;
	gchar const * const *xdg_dirs;
	gchar const *imdir = "input-methods";
	gchar const *apidir = "codyn-" API_VERSION;

	e = g_getenv ("CODYN_INPUT_METHODS");

	if (e)
	{
		gchar **paths;
		gchar **ptr;

		paths = g_strsplit (e, G_SEARCHPATH_SEPARATOR_S, -1);

		for (ptr = paths; *ptr; ++ptr)
		{
			load_input_methods (*ptr);
		}

		g_strfreev (paths);
	}

	path = g_build_filename (g_get_user_data_dir (),
	                         apidir,
	                         imdir,
	                         NULL);

	load_input_methods (path);
	g_free (path);

	for (xdg_dirs = g_get_system_data_dirs (); xdg_dirs && *xdg_dirs; ++xdg_dirs)
	{
		path = g_build_filename (*xdg_dirs,
		                         apidir,
		                         imdir,
		                         NULL);

		load_input_methods (path);
		g_free (path);
	}

	path = g_build_filename (LIBDIR, apidir, imdir, NULL);
	load_input_methods (path);
	g_free (path);
}

void
cdn_input_method_initialize ()
{
	static gboolean inited = FALSE;

	if (!G_LIKELY (inited))
	{
		inited = TRUE;

		load_all_input_methods ();
	}
}
