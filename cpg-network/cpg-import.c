#include "cpg-import.h"
#include "cpg-debug.h"
#include "cpg-network-deserializer.h"

#define CPG_IMPORT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_IMPORT, CpgImportPrivate))

struct _CpgImportPrivate
{
	gchar *filename;
	gboolean auto_imported;
};

G_DEFINE_TYPE (CpgImport, cpg_import, CPG_TYPE_GROUP)

enum
{
	PROP_0,
	PROP_FILENAME,
	PROP_AUTO_IMPORTED
};

static void
cpg_import_finalize (GObject *object)
{
	CpgImport *self = CPG_IMPORT (object);

	g_free (self->priv->filename);

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
		case PROP_FILENAME:
			g_free (self->priv->filename);
			self->priv->filename = g_value_dup_string (value);
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
		case PROP_FILENAME:
			g_value_set_string (value, self->priv->filename);
		break;
		case PROP_AUTO_IMPORTED:
			g_value_set_boolean (value, self->priv->auto_imported);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_import_class_init (CpgImportClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_import_finalize;

	object_class->get_property = cpg_import_get_property;
	object_class->set_property = cpg_import_set_property;

	g_type_class_add_private (object_class, sizeof(CpgImportPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_FILENAME,
	                                 g_param_spec_string ("filename",
	                                                      "Filename",
	                                                      "Filename",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_CONSTRUCT_ONLY));


	g_object_class_install_property (object_class,
	                                 PROP_AUTO_IMPORTED,
	                                 g_param_spec_boolean ("auto-imported",
	                                                       "Auto imported",
	                                                       "Auto Imported",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT_ONLY));
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

CpgImport *
cpg_import_new (CpgNetwork   *network,
                CpgGroup     *parent,
                gchar const  *id,
                gchar const  *filename,
                GError      **error)
{
	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (filename != NULL, NULL);

	CpgImport *obj = g_object_new (CPG_TYPE_IMPORT,
	                               "id", id,
	                               "filename", filename,
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
	                            "filename", self->priv->filename,
	                            "auto-imported", TRUE,
	                            NULL);

	import_templates (auto_import, source);

	cpg_group_add (cpg_network_get_template_group (target),
	               CPG_OBJECT (auto_import));
}

gboolean
cpg_import_load (CpgImport   *self,
                 CpgNetwork  *network,
                 CpgGroup    *parent,
                 GError     **error)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (CPG_IS_GROUP (parent), FALSE);

	if (self->priv->filename == NULL)
	{
		return import_failed (error,
		                      CPG_NETWORK_LOAD_ERROR_IMPORT,
		                      "Import filename was not specified");
	}

	CpgNetwork *imported = cpg_network_new ();
	CpgNetworkDeserializer *deserializer;
	GFile *file;

	deserializer = cpg_network_deserializer_new (imported, NULL);
	file = g_file_new_for_path (self->priv->filename);

	if (!cpg_network_deserializer_deserialize_file (deserializer,
	                                                file,
	                                                error))
	{
		g_object_unref (file);
		g_object_unref (deserializer);

		return FALSE;
	}

	g_object_unref (file);
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
	return TRUE;
}

gchar const *
cpg_import_get_filename (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), NULL);

	return self->priv->filename;
}

gboolean
cpg_import_get_auto_imported (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);

	return self->priv->auto_imported;
}
