/*
 * cpg-import.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-import.h"
#include "cpg-network-deserializer.h"
#include "cpg-import-alias.h"
#include "cpg-annotatable.h"

#include <string.h>
#include "config.h"

/**
 * SECTION:cpg-import
 * @short_description: Network import object
 *
 * The #CpgImport object can be used to import templates and objects from
 * an external network file.
 *
 **/
#define CPG_IMPORT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_IMPORT, CpgImportPrivate))

#define IMPORT_DIR "cpg-network-" API_VERSION
#define IMPORT_ENV "CPG_NETWORK_IMPORT_PATH"

static gchar **import_search_path = NULL;

struct _CpgImportPrivate
{
	GFile *file;
	gboolean modified;
	CpgGroup *prev_parent;

	GSList *imported_objects;
	gboolean check_remove;
};

static void cpg_modifiable_iface_init (gpointer iface);
static void unregister_imported_property (CpgImport *import, CpgProperty *property);
static void unregister_imported_object (CpgImport *import, CpgObject *object);

G_DEFINE_TYPE_WITH_CODE (CpgImport,
                         cpg_import,
                         CPG_TYPE_GROUP,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_MODIFIABLE,
                                                cpg_modifiable_iface_init));

enum
{
	PROP_0,
	PROP_FILE,
	PROP_MODIFIED,
	PROP_PATH
};

GQuark
cpg_import_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_import_error");
	}

	return quark;
}

static void
cpg_import_finalize (GObject *object)
{
	CpgImport *self = CPG_IMPORT (object);

	g_object_unref (self->priv->file);

	GSList *item;

	for (item = self->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CPG_IS_OBJECT (item->data))
		{
			unregister_imported_object (self, item->data);
		}
		else if (CPG_IS_PROPERTY (item->data))
		{
			unregister_imported_property (self, item->data);
		}
	}

	g_slist_free (self->priv->imported_objects);

	G_OBJECT_CLASS (cpg_import_parent_class)->finalize (object);
}

static void
cpg_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
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
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
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
		case PROP_PATH:
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
	gchar const *ienv;
	GPtrArray *dirs = g_ptr_array_new ();

	ienv = g_getenv (IMPORT_ENV);

	if (ienv)
	{
		gchar **parts;
		gchar **ptr;

		parts = g_strsplit (ienv, ":", -1);

		for (ptr = parts; *ptr; ++ptr)
		{
			if (**ptr)
			{
				g_ptr_array_add (dirs, *ptr);
			}
		}

		g_free (parts);
	}

	g_ptr_array_add (dirs, g_build_filename (g_get_user_data_dir (),
	                                         IMPORT_DIR,
	                                         "library",
	                                         NULL));

	for (xdg_dirs = g_get_system_data_dirs (); xdg_dirs && *xdg_dirs; ++xdg_dirs)
	{
		g_ptr_array_add (dirs, g_build_filename (*xdg_dirs,
		                                         IMPORT_DIR,
		                                         "library",
		                                         NULL));
	}

	g_ptr_array_add (dirs, g_build_filename (DATADIR, IMPORT_DIR, "library", NULL));

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

	g_object_class_install_property (object_class,
	                                 PROP_PATH,
	                                 g_param_spec_string ("path",
	                                                      "Path",
	                                                      "Path",
	                                                      NULL,
	                                                      G_PARAM_READABLE));

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");
}

static gboolean
verify_remove_import (CpgGroup   *group,
                      CpgObject  *child,
                      GError    **error,
                      CpgImport  *import)
{
	if (child != CPG_OBJECT (import))
	{
		return TRUE;
	}

	GSList *item;

	import->priv->check_remove = TRUE;

	for (item = import->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CPG_IS_OBJECT (item->data))
		{
			CpgGroup *parent = cpg_object_get_parent (item->data);

			if (!cpg_group_verify_remove_child (parent,
			                                    item->data,
			                                    error))
			{
				import->priv->check_remove = FALSE;
				return FALSE;
			}
		}
		else if (CPG_IS_PROPERTY (item->data))
		{
			CpgObject *parent = cpg_property_get_object (item->data);

			if (!cpg_object_verify_remove_property (parent,
			                                        item->data,
			                                        error))
			{
				import->priv->check_remove = FALSE;
				return FALSE;
			}
		}
	}

	import->priv->check_remove = TRUE;
	return TRUE;
}

static void
import_removed (CpgImport *import)
{
	GSList *item;

	for (item = import->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CPG_IS_OBJECT (item->data))
		{
			CpgGroup *parent = cpg_object_get_parent (item->data);

			unregister_imported_object (import, item->data);
			cpg_group_remove (parent, item->data, NULL);
		}
		else if (CPG_IS_PROPERTY (item->data))
		{
			CpgObject *parent = cpg_property_get_object (item->data);

			unregister_imported_property (import, item->data);
			cpg_object_remove_property (parent, item->data, NULL);
		}
	}

	g_slist_free (import->priv->imported_objects);
	import->priv->imported_objects = NULL;
}

static void
on_parent_changed (CpgImport *import)
{
	CpgGroup *parent = cpg_object_get_parent (CPG_OBJECT (import));

	if (import->priv->prev_parent != NULL)
	{
		g_signal_handlers_disconnect_by_func (import->priv->prev_parent,
		                                      G_CALLBACK (verify_remove_import),
		                                      import);
		import->priv->prev_parent = NULL;
	}

	import->priv->prev_parent = parent;

	if (parent != NULL)
	{
		g_signal_connect (parent,
		                  "verify-remove-child",
		                  G_CALLBACK (verify_remove_import),
		                  import);
	}
	else
	{
		/* Remove all imported objects and properties as well */
		import_removed (import);
	}
}

static void
cpg_import_init (CpgImport *self)
{
	self->priv = CPG_IMPORT_GET_PRIVATE (self);

	g_signal_connect (self,
	                  "notify::parent",
	                  G_CALLBACK (on_parent_changed),
	                  NULL);
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

	g_warning ("Import error: %s", message);

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
deny_remove_imported_child (CpgGroup   *parent,
                            CpgObject  *child,
                            GError    **error,
                            CpgImport  *import)
{
	if (!import->priv->check_remove &&
	    g_slist_find (import->priv->imported_objects, child))
	{
		if (error)
		{
			g_set_error (error,
			             CPG_IMPORT_ERROR,
			             CPG_IMPORT_ERROR_REMOVE,
			             "The object `%s' cannot be removed because it was automatically imported from `%s'",
			             cpg_object_get_id (child),
			             cpg_object_get_id (CPG_OBJECT (import)));
		}

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static gboolean
deny_remove_imported_property (CpgObject    *parent,
                               CpgProperty  *child,
                               GError      **error,
                               CpgImport    *import)
{
	if (!import->priv->check_remove &&
	    g_slist_find (import->priv->imported_objects, child))
	{
		if (error)
		{
			g_set_error (error,
			             CPG_IMPORT_ERROR,
			             CPG_IMPORT_ERROR_REMOVE,
			             "The property `%s' cannot be removed because it was automatically imported from `%s'",
			             cpg_property_get_name (child),
			             cpg_object_get_id (CPG_OBJECT (import)));
		}

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static void
register_imported_object (CpgImport *import,
                          CpgObject *object)
{
	CpgGroup *parent = cpg_object_get_parent (object);

	g_signal_connect (parent,
	                  "verify-remove-child",
	                  G_CALLBACK (deny_remove_imported_child),
	                  import);
}

static void
unregister_imported_object (CpgImport *import,
                            CpgObject *object)
{
	CpgGroup *parent = cpg_object_get_parent (object);

	g_signal_handlers_disconnect_by_func (parent,
	                                      G_CALLBACK (deny_remove_imported_child),
	                                      import);
}

static void
register_imported_property (CpgImport   *import,
                            CpgProperty *property)
{
	CpgObject *parent = cpg_property_get_object (property);

	g_signal_connect (parent,
	                  "verify-remove-property",
	                  G_CALLBACK (deny_remove_imported_property),
	                  import);
}

static void
unregister_imported_property (CpgImport   *import,
                              CpgProperty *property)
{
	CpgObject *parent = cpg_property_get_object (property);

	g_signal_handlers_disconnect_by_func (parent,
	                                      G_CALLBACK (deny_remove_imported_property),
	                                      import);
}

static void
add_imported_object (CpgImport *import,
                     gpointer   object)
{
	import->priv->imported_objects =
		g_slist_prepend (import->priv->imported_objects, object);

	if (CPG_IS_OBJECT (object))
	{
		register_imported_object (import, CPG_OBJECT (object));
	}
	else if (CPG_IS_PROPERTY (object))
	{
		register_imported_property (import, CPG_PROPERTY (object));
	}
	else
	{
		g_warning ("Unknown imported object: %s",
		           g_type_name (G_TYPE_FROM_INSTANCE (object)));
	}
}

static gboolean
object_in_templates (CpgNetwork *network,
                     CpgObject  *obj)
{
	CpgObject *tg;

	tg = CPG_OBJECT (cpg_network_get_template_group (network));

	while (obj)
	{
		if (obj == tg)
		{
			return TRUE;
		}

		obj = CPG_OBJECT (cpg_object_get_parent (obj));
	}

	return FALSE;
}

static void
import_objects (CpgImport *self,
                CpgGroup  *parent)
{
	GSList const *children;

	children = cpg_group_get_children (parent);

	while (children)
	{
		cpg_group_add (CPG_GROUP (self), children->data, NULL);

		add_imported_object (self, children->data);

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

	add_imported_object (self, auto_import);
}

static void
import_globals (CpgImport  *self,
                CpgNetwork *source,
                CpgNetwork *target)
{
	GSList *properties = cpg_object_get_properties (CPG_OBJECT (source));
	GSList *item;

	for (item = properties; item; item = g_slist_next (item))
	{
		CpgProperty *property = item->data;

		if (!cpg_object_get_property (CPG_OBJECT (target),
		                              cpg_property_get_name (property)))
		{
			CpgProperty *copy = cpg_property_copy (property);

			cpg_object_add_property (CPG_OBJECT (target), copy, NULL);
			add_imported_object (self, copy);
		}
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
	gchar const *annotation;

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

	if (!cpg_network_load_from_file (imported, self->priv->file, error))
	{
		return FALSE;
	}

	annotation = cpg_annotatable_get_annotation (CPG_ANNOTATABLE (imported));

	if (annotation)
	{
		cpg_annotatable_set_annotation (CPG_ANNOTATABLE (self), annotation);
	}

	/* Import globals */
	import_globals (self, network, imported);

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

	cpg_group_add (parent, CPG_OBJECT (self), NULL);

	self->priv->modified = FALSE;
	g_object_notify (G_OBJECT (self), "modified");

	_cpg_network_register_import (network, self);

	g_object_unref (imported);

	return TRUE;
}

/**
 * cpg_import_get_file:
 * @self: A #CpgImport
 *
 * Get the file that was imported.
 *
 * Returns: (transfer full) (allow-none): A #GFile
 *
 **/
GFile *
cpg_import_get_file (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), NULL);

	return self->priv->file ? g_file_dup (self->priv->file) : NULL;
}

/**
 * cpg_import_get_path:
 * @self: A #CpgImport
 *
 * Get the path that was imported.
 *
 * Returns: (transfer full) (allow-none): the path
 *
 **/
gchar *
cpg_import_get_path (CpgImport *self)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), NULL);

	return self->priv->file ? g_file_get_path (self->priv->file) : NULL;
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

gboolean
cpg_import_imports_object (CpgImport *self,
                           CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_IMPORT (self), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	return g_slist_find (self->priv->imported_objects, object) != NULL;
}
