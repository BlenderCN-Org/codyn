#include "cpg-import.h"
#include "cpg-debug.h"
#include "cpg-network-deserializer.h"

/**
 * SECTION:cpg-import
 * @short_description: Network import object
 *
 * The #CpgImport object can be used to import templates and objects from
 * an external network file.
 *
 **/
#define CPG_IMPORT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_IMPORT, CpgImportPrivate))

struct _CpgImportPrivate
{
	GFile *file;
	gboolean auto_imported;
	gboolean modified;
};

G_DEFINE_TYPE (CpgImport, cpg_import, CPG_TYPE_GROUP)

enum
{
	PROP_0,
	PROP_FILE,
	PROP_AUTO_IMPORTED,
	PROP_MODIFIED
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
		case PROP_AUTO_IMPORTED:
			self->priv->auto_imported = g_value_get_boolean (value);
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
		case PROP_AUTO_IMPORTED:
			g_value_set_boolean (value, self->priv->auto_imported);
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
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
	 * CpgImport:auto-imported:
	 *
	 * Whether the import was done automatically due to dependencies of
	 * another import.
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_AUTO_IMPORTED,
	                                 g_param_spec_boolean ("auto-imported",
	                                                       "Auto imported",
	                                                       "Auto Imported",
	                                                       FALSE,
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
 * @parent: A #CpgGroup
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
		cpg_group_add (CPG_GROUP (self), children->data);
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
	               CPG_OBJECT (auto_import));
}

/**
 * cpg_import_load:
 * @self: A #CpgImport
 * @network: A #CpgNetwork
 * @parent: A #CpgGroup
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

	cpg_group_add (parent, CPG_OBJECT (self));

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
 * cpg_import_get_auto_imported:
 * @self: A #CpgImport
 *
 * Get whether the import was done automatically.
 *
 * Returns: %TRUE if the import was automatic, %FALSE otherwise
 *
 **/
gboolean
cpg_import_get_auto_imported (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);

	return self->priv->auto_imported;
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
