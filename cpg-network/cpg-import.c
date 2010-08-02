#include "cpg-import.h"
#include "cpg-debug.h"
#include "cpg-network-deserializer.h"
#include "cpg-import-alias.h"

#include <string.h>

/**
 * SECTION:cpg-import
 * @short_description: Network import object
 *
 * The #CpgImport object can be used to import templates and objects from
 * an external network file.
 *
 **/
#define CPG_IMPORT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_IMPORT, CpgImportPrivate))

#define IMPORT_DIR "cpg-network-2.0"

static gchar **import_search_path = NULL;

struct _CpgImportPrivate
{
	GFile *file;
	gboolean modified;
};

G_DEFINE_TYPE (CpgImport, cpg_import, CPG_TYPE_GROUP)

enum
{
	PROP_0,
	PROP_FILE,
	PROP_MODIFIED,
	PROP_FILENAME
};

static void
cpg_import_finalize (GObject *object)
{
	CpgImport *self = CPG_IMPORT (object);

	g_object_unref (self->priv->file);

	G_OBJECT_CLASS (cpg_import_parent_class)->finalize (object);
}

static void
cpg_import_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
	CpgImport *self = CPG_IMPORT (object);

	switch (prop_id)
	{
		case PROP_FILE:
		{
			GFile *file = g_value_get_object (value);
			self->priv->file = file ? g_file_dup (file) : NULL;
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_import_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
	CpgImport *self = CPG_IMPORT (object);

	switch (prop_id)
	{
		case PROP_FILE:
			g_value_set_object (value, self->priv->file);
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
		break;
		case PROP_FILENAME:
			if (self->priv->file)
			{
				g_value_take_string (value, g_file_get_path (self->priv->file));
			}
			else
			{
				g_value_set_string (value, NULL);
			}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_import_taint (CpgObject *object)
{
	CpgImport *self = CPG_IMPORT (object);

	CPG_OBJECT_CLASS (cpg_import_parent_class)->taint (object);

	self->priv->modified = TRUE;
	g_object_notify (G_OBJECT (object), "modified");
}

static void
cpg_import_reset (CpgObject *object)
{
	CpgImport *self = CPG_IMPORT (object);
	gboolean modified = self->priv->modified;

	CPG_OBJECT_CLASS (cpg_import_parent_class)->reset (object);

	if (modified != self->priv->modified)
	{
		self->priv->modified = modified;
		g_object_notify (G_OBJECT (self), "modified");
	}
}

static gchar **
default_search_path (void)
{
	const gchar * const *xdg_dirs;
	GPtrArray *dirs = g_ptr_array_new ();

	g_ptr_array_add (dirs, g_build_filename (g_get_user_data_dir (),
	                                         IMPORT_DIR,
	                                         "import-library",
	                                         NULL));

	for (xdg_dirs = g_get_system_data_dirs (); xdg_dirs && *xdg_dirs; ++xdg_dirs)
	{
		g_ptr_array_add (dirs, g_build_filename (*xdg_dirs,
		                                         IMPORT_DIR,
		                                         "import-library",
		                                         NULL));
	}

	g_ptr_array_add (dirs, g_build_filename (DATADIR, IMPORT_DIR, "import-library", NULL));

	g_ptr_array_add (dirs, NULL);
	return (gchar **) g_ptr_array_free (dirs, FALSE);
}

static GType
cpg_import_get_copy_type (CpgObject *object)
{
	return CPG_TYPE_IMPORT_ALIAS;
}

static void
cpg_import_copy (CpgObject *object,
                 CpgObject *source)
{
	CpgImport *self = CPG_IMPORT (object);
	CpgImport *source_import = CPG_IMPORT (source);

	self->priv->file = g_file_dup (source_import->priv->file);
	self->priv->modified = source_import->priv->modified;
}

static void
cpg_import_class_init (CpgImportClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	object_class->finalize = cpg_import_finalize;

	object_class->get_property = cpg_import_get_property;
	object_class->set_property = cpg_import_set_property;

	cpg_class->taint = cpg_import_taint;
	cpg_class->reset = cpg_import_reset;
	cpg_class->get_copy_type = cpg_import_get_copy_type;
	cpg_class->copy = cpg_import_copy;

	g_type_class_add_private (object_class, sizeof(CpgImportPrivate));

	/**
	 * CpgImport:file:
	 *
	 * The imported file
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));

	/**
	 * CpgImport:modified:
	 *
	 * Whether any of the imported objects have been modified.
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_MODIFIED,
	                                 g_param_spec_boolean ("modified",
	                                                       "Modified",
	                                                       "Modified",
	                                                       FALSE,
	                                                       G_PARAM_READABLE));

	g_object_class_install_property (object_class,
	                                 PROP_FILENAME,
	                                 g_param_spec_string ("filename",
	                                                      "Filename",
	                                                      "Filename",
	                                                      NULL,
	                                                      G_PARAM_READABLE));
}

static void
cpg_import_init (CpgImport *self)
{
	self->priv = CPG_IMPORT_GET_PRIVATE (self);
}

static gboolean
import_failed (GError     **error,
               gint         code,
               gchar const *format,
               ...)
{
	if (!error)
	{
		return FALSE;
	}

	va_list ap;
	va_start (ap, format);

	gchar *message = g_strdup_vprintf (format, ap);
	va_end (ap);

	if (*error)
	{
		g_error_free (*error);
		*error = NULL;
	}

	cpg_debug_error ("Import error: %s", message);

	g_set_error (error,
	             CPG_NETWORK_LOAD_ERROR,
	             code,
	             "%s",
	             message);

	g_free (message);
	return FALSE;
}

/**
 * cpg_import_new:
 * @network: A #CpgNetwork
 * @parent: (allow-none): A #CpgGroup
 * @id: The import object id
 * @file: The file to import
 * @error: A #GError
 *
 * Import objects from an external file. The import object will automatically
 * be added to the parent group. If the import is done in the normal object
 * tree of the network, templates that are defined in the imported file will
 * be automatically imported in the networks' templates.
 *
 * Returns: A #CpgImport or %NULL if the import failed.
 *
 **/
CpgImport *
cpg_import_new (CpgNetwork   *network,
                CpgGroup     *parent,
                gchar const  *id,
                GFile        *file,
                GError      **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_GROUP (parent), NULL);
	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	CpgImport *obj = g_object_new (CPG_TYPE_IMPORT,
	                               "id", id,
	                               "file", file,
	                                NULL);

	if (!cpg_import_load (obj, network, parent, error))
	{
		g_object_unref (obj);
		obj = NULL;
	}

	return obj;
}

/**
 * cpg_import_new_from_path:
 * @network: A #CpgNetwork
 * @parent: (allow-none): A #CpgGroup
 * @id: The import object id
 * @path: The import file path
 * @error: A #GError
 *
 * Convenience function to create a new import for a path. See #cpg_import_new
 * for more information. Note that the specified path should be an absolute
 * path. The search directories are not used to resolve the full path.
 *
 * Returns: A #CpgImport
 *
 **/
CpgImport *
cpg_import_new_from_path (CpgNetwork   *network,
                          CpgGroup     *parent,
                          gchar const  *id,
                          gchar const  *path,
                          GError      **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (parent == NULL || CPG_IS_GROUP (parent), NULL);
	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CpgImport *ret = cpg_import_new (network, parent, id, file, error);

	g_object_unref (file);

	return ret;
}

static gboolean
object_in_templates (CpgNetwork *network,
                     CpgObject  *obj)
{
	CpgObject *parent;

	while ((parent = cpg_object_get_parent (obj)))
	{
		obj = parent;
	}

	return obj != CPG_OBJECT (network);
}

static void
import_objects (CpgImport *self,
                CpgGroup  *parent)
{
	GSList const *children = cpg_group_get_children (parent);

	while (children)
	{
		cpg_group_add (CPG_GROUP (self), children->data, NULL);
		children = g_slist_next (children);
	}
}

static void
import_templates (CpgImport  *self,
                  CpgNetwork *network)
{
	import_objects (self, cpg_network_get_template_group (network));
}

static void
auto_import_templates (CpgImport  *self,
                       CpgNetwork *source,
                       CpgNetwork *target)
{
	/* Auto-import templates into templates */
	CpgImport *auto_import;

	auto_import = g_object_new (CPG_TYPE_IMPORT,
	                            "id", cpg_object_get_id (CPG_OBJECT (self)),
	                            "file", self->priv->file,
	                            "auto-imported", TRUE,
	                            NULL);

	import_templates (auto_import, source);

	cpg_group_add (cpg_network_get_template_group (target),
	               CPG_OBJECT (auto_import),
	               NULL);
}

static void
auto_import_functions (CpgImport  *import,
                       CpgNetwork *source,
                       CpgNetwork *target)
{
	CpgGroup *function_group = cpg_network_get_function_group (source);
	CpgGroup *target_functions = cpg_network_get_function_group (target);

	GSList const *children = cpg_group_get_children (function_group);

	while (children)
	{
		CpgFunction *function = children->data;
		gchar const *id = cpg_object_get_id (CPG_OBJECT (function));

		if (!cpg_group_find_object (target_functions, id))
		{
			cpg_group_add (target_functions, CPG_OBJECT (function), NULL);
			cpg_object_set_auto_imported (CPG_OBJECT (function),
			                              TRUE);
		}

		children = g_slist_next (children);
	}
}

/**
 * cpg_import_load:
 * @self: A #CpgImport
 * @network: A #CpgNetwork
 * @parent: (allow-none): A #CpgGroup
 * @error: A #GError
 *
 * Perform the actual import. This function is called by #cpg_import_new and
 * should never have to be used manually. It's provided for use in bindings.
 *
 * Returns: %TRUE if the import was successful, %FALSE otherwise.
 *
 **/
gboolean
cpg_import_load (CpgImport   *self,
                 CpgNetwork  *network,
                 CpgGroup    *parent,
                 GError     **error)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (CPG_IS_GROUP (parent), FALSE);

	if (self->priv->file == NULL)
	{
		return import_failed (error,
		                      CPG_NETWORK_LOAD_ERROR_IMPORT,
		                      "Import filename was not specified");
	}

	CpgNetwork *imported = cpg_network_new ();
	CpgNetworkDeserializer *deserializer;

	deserializer = cpg_network_deserializer_new (imported, NULL);

	if (!cpg_network_deserializer_deserialize_file (deserializer,
	                                                self->priv->file,
	                                                error))
	{
		g_object_unref (deserializer);

		return FALSE;
	}

	g_object_unref (deserializer);

	/* Check if importing in templates or normal objects */
	gboolean templ = object_in_templates (network,
	                                      CPG_OBJECT (parent));

	auto_import_functions (self, imported, network);

	if (!templ)
	{
		auto_import_templates (self, imported, network);

		/* Import objects */
		import_objects (self, CPG_GROUP (imported));
	}
	else
	{
		/* Import templates into the import */
		import_templates (self, imported);
	}

	cpg_group_add (parent, CPG_OBJECT (self), NULL);

	self->priv->modified = FALSE;
	g_object_notify (G_OBJECT (self), "modified");

	return TRUE;
}

/**
 * cpg_import_get_file:
 * @self: A #CpgImport
 *
 * Get the file that was imported.
 *
 * Returns: A #GFile
 *
 **/
GFile *
cpg_import_get_file (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), NULL);

	return self->priv->file ? g_file_dup (self->priv->file) : NULL;
}

/**
 * cpg_import_get_modified:
 * @self: A #CpgImport
 *
 * Get whether any of the imported objects were modified after the import.
 *
 * Returns: %TRUE if there were any imported objects modified, %FALSE otherwise
 *
 **/
gboolean
cpg_import_get_modified (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);

	return self->priv->modified;
}

/**
 * cpg_import_get_search_path:
 *
 * Get the search directories used to resolve import file names.
 *
 * Returns: (array zero-terminated=1) (transfer none): a %NULL terminated list of strings
 *
 **/
G_CONST_RETURN gchar * G_CONST_RETURN *
cpg_import_get_search_path (void)
{
	if (import_search_path == NULL)
	{
		import_search_path = default_search_path ();
	}

	return (G_CONST_RETURN gchar * G_CONST_RETURN *)import_search_path;
}

/**
 * cpg_import_set_search_path:
 * @path: (array zero-terminated=1): The search directories
 *
 * Set the search directories used to resolve import file names. @path should
 * be a %NULL terminated list of strings.
 *
 **/
void
cpg_import_set_search_path (gchar **path)
{
	if (import_search_path)
	{
		g_strfreev (import_search_path);
	}

	if (path)
	{
		import_search_path = g_strdupv (path);
	}
	else
	{
		import_search_path = default_search_path ();
	}
}

/**
 * cpg_import_append_search_path:
 * @path: A directory path
 *
 * Append a search directory path to the list of paths to be searched when
 * resolving an import file.
 *
 **/
void
cpg_import_append_search_path (gchar const *path)
{
	guint len = 0;

	g_return_if_fail (path != NULL);

	if (import_search_path == NULL)
	{
		import_search_path = default_search_path ();
	}

	g_return_if_fail (import_search_path != NULL);

	len = g_strv_length (import_search_path);

	import_search_path = g_renew (gchar *,
	                              import_search_path,
	                              len + 2);

	import_search_path[len] = g_strdup (path);
	import_search_path[len + 1] = NULL;
}

/**
 * cpg_import_prepend_search_path:
 * @path: A directory path
 *
 * Prepend a search directory path to the list of paths to be searched when
 * resolving an import file.
 *
 **/
void
cpg_import_prepend_search_path (gchar const *path)
{
	guint len = 0;
	gchar **new_search_path;

	g_return_if_fail (path != NULL);

	if (import_search_path == NULL)
	{
		import_search_path = default_search_path ();
	}

	g_return_if_fail (import_search_path != NULL);

	len = g_strv_length (import_search_path);
	new_search_path = g_new (gchar *, len + 2);
	new_search_path[0] = g_strdup (path);
	memcpy (new_search_path + 1, import_search_path, (len + 1) * sizeof (gchar *));

	g_free (import_search_path);
	import_search_path = new_search_path;
}
