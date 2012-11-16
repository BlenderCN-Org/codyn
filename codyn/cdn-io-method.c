#include "cdn-io-method.h"
#include "cdn-io.h"
#include <string.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define CDN_IO_METHOD_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IO_METHOD, CdnIoMethodPrivate))

static GSList *io_methods = NULL;
static GHashTable *io_cache = NULL;

struct _CdnIoMethodPrivate
{
	GModule *module;
	gchar *path;
};

G_DEFINE_TYPE (CdnIoMethod, cdn_io_method, G_TYPE_TYPE_MODULE)

enum
{
	PROP_0,
	PROP_PATH
};

static void
cdn_io_method_finalize (GObject *object)
{
	CdnIoMethod *method;

	method = CDN_IO_METHOD (object);

	g_free (method->priv->path);

	G_OBJECT_CLASS (cdn_io_method_parent_class)->finalize (object);
}

static gboolean
cdn_io_method_load (GTypeModule *module)
{
	CdnIoMethod *method;
	gpointer sym;

	method = CDN_IO_METHOD (module);
	method->priv->module = g_module_open (method->priv->path, 0);

	if (method->priv->module)
	{
		if (g_module_symbol (method->priv->module,
		                     "cdn_io_register_types",
		                     &sym))
		{
			CdnIoMethodRegisterTypesFunc func = sym;

			func (module);

			return TRUE;
		}
	}

	return FALSE;
}

static void
cdn_io_method_unload (GTypeModule *module)
{
	CdnIoMethod *method;

	method = CDN_IO_METHOD (module);

	if (method->priv->module)
	{
		g_module_close (method->priv->module);
		method->priv->module = NULL;
	}
}

static void
cdn_io_method_set_property (GObject      *object,
                            guint         prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
	CdnIoMethod *self = CDN_IO_METHOD (object);

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
cdn_io_method_get_property (GObject    *object,
                            guint       prop_id,
                            GValue     *value,
                            GParamSpec *pspec)
{
	CdnIoMethod *self = CDN_IO_METHOD (object);

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
cdn_io_method_class_init (CdnIoMethodClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GTypeModuleClass *module_class = G_TYPE_MODULE_CLASS (klass);

	object_class->finalize = cdn_io_method_finalize;

	object_class->get_property = cdn_io_method_get_property;
	object_class->set_property = cdn_io_method_set_property;

	module_class->load = cdn_io_method_load;
	module_class->unload = cdn_io_method_unload;

	g_type_class_add_private (object_class, sizeof(CdnIoMethodPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_PATH,
	                                 g_param_spec_string ("path",
	                                                      "Path",
	                                                      "Path",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_io_method_init (CdnIoMethod *self)
{
	self->priv = CDN_IO_METHOD_GET_PRIVATE (self);
}

CdnIoMethod *
cdn_io_method_new (gchar const *path)
{
	return g_object_new (CDN_TYPE_IO_METHOD,
	                     "path", path,
	                     NULL);
}

static gchar *
canon_name (gchar const *s)
{
	gboolean iscap = TRUE;
	GString *ret;

	ret = g_string_sized_new (strlen (s));

	while (*s)
	{
		gchar c = *s;
		++s;

		if (!g_ascii_isalnum (c))
		{
			iscap = FALSE;
			g_string_append_c (ret, '_');
			continue;
		}
		else if (g_ascii_islower (c))
		{
			iscap = FALSE;
		}
		else if (g_ascii_isupper (c) && !iscap)
		{
			g_string_append_c (ret, '_');
			iscap = TRUE;
		}

		g_string_append_c (ret, c);
	}

	return g_string_free (ret, FALSE);
}

typedef GType (*TypeInitFunc) (void);

static void
init_cache ()
{
	if (io_cache == NULL)
	{
		io_cache = g_hash_table_new_full (g_str_hash,
		                                  g_str_equal,
		                                  (GDestroyNotify)g_free,
		                                  NULL);
	}
}

static TypeInitFunc
find_type_init_func (gchar const *name,
                     CdnIoMode    mode)
{
	TypeInitFunc ret = 0;
	GModule *mod;
	gchar *cname;
	gchar *cache_name;

	cname = canon_name (name);

	init_cache ();

	cache_name = g_strdup_printf ("%u_%s", (guint)mode, name);

	if (g_hash_table_lookup_extended (io_cache, cache_name, NULL, (gpointer *)&ret))
	{
		g_free (cname);
		g_free (cache_name);

		return ret;
	}

	mod = g_module_open (NULL, G_MODULE_BIND_LAZY);

	if (mode == CDN_IO_MODE_INPUT)
	{
		gchar *comp;

		comp = g_strconcat ("cdn_input_", cname, "_get_type", NULL);
		g_module_symbol (mod, comp, (gpointer *)&ret);
		g_free (comp);
	}
	else if (mode == CDN_IO_MODE_OUTPUT)
	{
		gchar *comp;

		comp = g_strconcat ("cdn_output_", cname, "_get_type", NULL);
		g_module_symbol (mod, comp, (gpointer *)&ret);
		g_free (comp);
	}

	if (ret == 0)
	{
		gchar *comp;

		comp = g_strconcat ("cdn_io_", cname, "_get_type", NULL);
		g_module_symbol (mod, comp, (gpointer *)&ret);
		g_free (comp);
	}

	g_hash_table_insert (io_cache,
	                     cache_name,
	                     ret);

	g_module_close (mod);
	g_free (cname);

	return ret;
}

GType
cdn_io_method_find (gchar const *name,
                    CdnIoMode    mode)
{
	GType ret;
	TypeInitFunc type_init_func;

	type_init_func = find_type_init_func (name, mode);

	if (type_init_func == 0)
	{
		return G_TYPE_INVALID;
	}

	ret = type_init_func ();

	if (!g_type_is_a (ret, CDN_TYPE_IO))
	{
		ret = G_TYPE_INVALID;
	}

	return ret;
}

static void
load_io_method (gchar const *path)
{
	CdnIoMethod *method;

	method = cdn_io_method_new (path);

	if (g_type_module_use (G_TYPE_MODULE (method)))
	{
		io_methods = g_slist_prepend (io_methods,
		                                 method);
	}
	else
	{
		g_object_unref (method);
	}
}

static void
load_io_methods (gchar const *dir)
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
		load_io_method (path);

		g_free (path);
	}

	g_dir_close (d);
}

static void
load_all_io_methods ()
{
	gchar const *e;
	gchar *path;
	gchar const * const *xdg_dirs;
	gchar const *imdir = "io";
	gchar const *apidir = "codyn-" API_VERSION;
	gchar const *libdir;

	e = g_getenv ("CODYN_IO_METHODS");

	if (e)
	{
		gchar **paths;
		gchar **ptr;

		paths = g_strsplit (e, G_SEARCHPATH_SEPARATOR_S, -1);

		for (ptr = paths; *ptr; ++ptr)
		{
			load_io_methods (*ptr);
		}

		g_strfreev (paths);
	}

	path = g_build_filename (g_get_user_data_dir (),
	                         apidir,
	                         imdir,
	                         NULL);

	load_io_methods (path);
	g_free (path);

	for (xdg_dirs = g_get_system_data_dirs (); xdg_dirs && *xdg_dirs; ++xdg_dirs)
	{
		path = g_build_filename (*xdg_dirs,
		                         apidir,
		                         imdir,
		                         NULL);

		load_io_methods (path);
		g_free (path);
	}

#if ENABLE_OSX_FRAMEWORK
	libdir = "/Library/Frameworks/Codyn.framework/Resources/lib";
#else
	libdir = LIBDIR;
#endif

	path = g_build_filename (libdir, apidir, imdir, NULL);

	load_io_methods (path);
	g_free (path);
}

void
cdn_io_method_initialize ()
{
	static gboolean inited = FALSE;

	if (!G_LIKELY (inited))
	{
		inited = TRUE;
		init_cache ();

		load_all_io_methods ();
	}
}
