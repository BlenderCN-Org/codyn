/*
 * cdn-import.c
 * This file is part of codyn
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

#include "cdn-import.h"
#include "cdn-import-alias.h"
#include "cdn-annotatable.h"

#include <string.h>
#include "config.h"

#define CDN_IMPORT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_IMPORT, CdnImportPrivate))

#define IMPORT_DIR "codyn-" API_VERSION
#define IMPORT_ENV "CODYN_IMPORT_PATH"

static gchar **import_search_path = NULL;

struct _CdnImportPrivate
{
	GFile *file;
	gboolean modified;
	CdnNode *prev_parent;

	GSList *imported_objects;
	gboolean check_remove;
};

static void cdn_modifiable_iface_init (gpointer iface);
static void unregister_imported_property (CdnImport *import, CdnVariable *property);
static void unregister_imported_object (CdnImport *import, CdnObject *object);

G_DEFINE_TYPE_WITH_CODE (CdnImport,
                         cdn_import,
                         CDN_TYPE_NODE,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_MODIFIABLE,
                                                cdn_modifiable_iface_init));

enum
{
	PROP_0,
	PROP_FILE,
	PROP_MODIFIED,
	PROP_PATH
};

GQuark
cdn_import_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_import_error");
	}

	return quark;
}

static void
cdn_import_finalize (GObject *object)
{
	CdnImport *self = CDN_IMPORT (object);

	g_object_unref (self->priv->file);

	GSList *item;

	for (item = self->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CDN_IS_OBJECT (item->data))
		{
			unregister_imported_object (self, item->data);
		}
		else if (CDN_IS_VARIABLE (item->data))
		{
			unregister_imported_property (self, item->data);
		}
	}

	g_slist_free (self->priv->imported_objects);

	G_OBJECT_CLASS (cdn_import_parent_class)->finalize (object);
}

static void
cdn_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static void
cdn_import_set_property (GObject      *object,
                         guint         prop_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
	CdnImport *self = CDN_IMPORT (object);

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
cdn_import_get_property (GObject    *object,
                         guint       prop_id,
                         GValue     *value,
                         GParamSpec *pspec)
{
	CdnImport *self = CDN_IMPORT (object);

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
cdn_import_taint (CdnObject *object)
{
	CdnImport *self = CDN_IMPORT (object);

	CDN_OBJECT_CLASS (cdn_import_parent_class)->taint (object);

	self->priv->modified = TRUE;
	g_object_notify (G_OBJECT (object), "modified");
}

static void
cdn_import_reset (CdnObject *object)
{
	CdnImport *self = CDN_IMPORT (object);
	gboolean modified = self->priv->modified;

	CDN_OBJECT_CLASS (cdn_import_parent_class)->reset (object);

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
	GPtrArray *dirs;

	dirs = g_ptr_array_new ();

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
cdn_import_get_copy_type (CdnObject *object)
{
	return CDN_TYPE_IMPORT_ALIAS;
}

static void
cdn_import_copy (CdnObject *object,
                 CdnObject *source)
{
	CdnImport *self = CDN_IMPORT (object);
	CdnImport *source_import = CDN_IMPORT (source);

	self->priv->file = g_file_dup (source_import->priv->file);
	self->priv->modified = source_import->priv->modified;
}

static void
cdn_import_class_init (CdnImportClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_import_finalize;

	object_class->get_property = cdn_import_get_property;
	object_class->set_property = cdn_import_set_property;

	cdn_class->taint = cdn_import_taint;
	cdn_class->reset = cdn_import_reset;
	cdn_class->get_copy_type = cdn_import_get_copy_type;
	cdn_class->copy = cdn_import_copy;

	g_type_class_add_private (object_class, sizeof(CdnImportPrivate));

	/**
	 * CdnImport:file:
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
verify_remove_import (CdnNode    *group,
                      CdnObject  *child,
                      GError    **error,
                      CdnImport  *import)
{
	if (child != CDN_OBJECT (import))
	{
		return TRUE;
	}

	GSList *item;

	import->priv->check_remove = TRUE;

	for (item = import->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CDN_IS_OBJECT (item->data))
		{
			CdnNode *parent = cdn_object_get_parent (item->data);

			if (parent && !cdn_node_verify_remove_child (parent,
			                                             item->data,
			                                             error))
			{
				import->priv->check_remove = FALSE;
				return FALSE;
			}
		}
		else if (CDN_IS_VARIABLE (item->data))
		{
			CdnObject *parent = cdn_variable_get_object (item->data);

			if (parent && !cdn_object_verify_remove_variable (parent,
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
import_removed (CdnImport *import)
{
	GSList *item;

	for (item = import->priv->imported_objects; item; item = g_slist_next (item))
	{
		if (CDN_IS_OBJECT (item->data))
		{
			CdnNode *parent = cdn_object_get_parent (item->data);

			unregister_imported_object (import, item->data);

			if (parent)
			{
				cdn_node_remove (parent, item->data, NULL);
			}
		}
		else if (CDN_IS_VARIABLE (item->data))
		{
			CdnObject *parent = cdn_variable_get_object (item->data);

			unregister_imported_property (import, item->data);

			if (parent)
			{
				cdn_object_remove_variable (parent, item->data, NULL);
			}
		}
	}

	g_slist_free (import->priv->imported_objects);
	import->priv->imported_objects = NULL;
}

static void
on_parent_changed (CdnImport *import)
{
	CdnNode *parent = cdn_object_get_parent (CDN_OBJECT (import));

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
		/* Remove all imported objects and variables as well */
		import_removed (import);
	}
}

static void
cdn_import_init (CdnImport *self)
{
	self->priv = CDN_IMPORT_GET_PRIVATE (self);

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
	             CDN_NETWORK_LOAD_ERROR,
	             code,
	             "%s",
	             message);

	g_free (message);
	return FALSE;
}

/**
 * cdn_import_new:
 * @network: A #CdnNetwork
 * @parent: (allow-none): A #CdnNode
 * @id: The import object id
 * @file: The file to import
 * @error: A #GError
 *
 * Import objects from an external file. The import object will automatically
 * be added to the parent group. If the import is done in the normal object
 * tree of the network, templates that are defined in the imported file will
 * be automatically imported in the networks' templates.
 *
 * Returns: A #CdnImport or %NULL if the import failed.
 *
 **/
CdnImport *
cdn_import_new (CdnNetwork   *network,
                CdnNode     *parent,
                gchar const  *id,
                GFile        *file,
                GError      **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);
	g_return_val_if_fail (parent == NULL || CDN_IS_NODE (parent), NULL);
	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	CdnImport *obj = g_object_new (CDN_TYPE_IMPORT,
	                               "id", id,
	                               "file", file,
	                               NULL);

	if (!cdn_import_load (obj, network, parent, error))
	{
		g_object_unref (obj);
		obj = NULL;
	}

	return obj;
}

/**
 * cdn_import_new_from_path:
 * @network: A #CdnNetwork
 * @parent: (allow-none): A #CdnNode
 * @id: The import object id
 * @path: The import file path
 * @error: A #GError
 *
 * Convenience function to create a new import for a path. See #cdn_import_new
 * for more information. Note that the specified path should be an absolute
 * path. The search directories are not used to resolve the full path.
 *
 * Returns: A #CdnImport
 *
 **/
CdnImport *
cdn_import_new_from_path (CdnNetwork   *network,
                          CdnNode     *parent,
                          gchar const  *id,
                          gchar const  *path,
                          GError      **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);
	g_return_val_if_fail (parent == NULL || CDN_IS_NODE (parent), NULL);
	g_return_val_if_fail (id != NULL, NULL);
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CdnImport *ret = cdn_import_new (network, parent, id, file, error);

	g_object_unref (file);

	return ret;
}

static gboolean
deny_remove_imported_child (CdnNode   *parent,
                            CdnObject  *child,
                            GError    **error,
                            CdnImport  *import)
{
	if (!import->priv->check_remove &&
	    g_slist_find (import->priv->imported_objects, child))
	{
		if (error)
		{
			g_set_error (error,
			             CDN_IMPORT_ERROR,
			             CDN_IMPORT_ERROR_REMOVE,
			             "The object `%s' cannot be removed because it was automatically imported from `%s'",
			             cdn_object_get_id (child),
			             cdn_object_get_id (CDN_OBJECT (import)));
		}

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static gboolean
deny_remove_imported_property (CdnObject    *parent,
                               CdnVariable  *child,
                               GError      **error,
                               CdnImport    *import)
{
	if (!import->priv->check_remove &&
	    g_slist_find (import->priv->imported_objects, child))
	{
		if (error)
		{
			g_set_error (error,
			             CDN_IMPORT_ERROR,
			             CDN_IMPORT_ERROR_REMOVE,
			             "The property `%s' cannot be removed because it was automatically imported from `%s'",
			             cdn_variable_get_name (child),
			             cdn_object_get_id (CDN_OBJECT (import)));
		}

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static void
register_imported_object (CdnImport *import,
                          CdnObject *object)
{
	CdnNode *parent = cdn_object_get_parent (object);

	g_signal_connect (parent,
	                  "verify-remove-child",
	                  G_CALLBACK (deny_remove_imported_child),
	                  import);
}

static void
unregister_imported_object (CdnImport *import,
                            CdnObject *object)
{
	CdnNode *parent = cdn_object_get_parent (object);

	if (parent)
	{
		g_signal_handlers_disconnect_by_func (parent,
		                                      G_CALLBACK (deny_remove_imported_child),
		                                      import);
	}
}

static void
register_imported_property (CdnImport   *import,
                            CdnVariable *property)
{
	CdnObject *parent = cdn_variable_get_object (property);

	g_signal_connect (parent,
	                  "verify-remove-variable",
	                  G_CALLBACK (deny_remove_imported_property),
	                  import);
}

static void
unregister_imported_property (CdnImport   *import,
                              CdnVariable *property)
{
	CdnObject *parent = cdn_variable_get_object (property);

	g_signal_handlers_disconnect_by_func (parent,
	                                      G_CALLBACK (deny_remove_imported_property),
	                                      import);
}

static void
add_imported_object (CdnImport *import,
                     gpointer   object)
{
	import->priv->imported_objects =
		g_slist_prepend (import->priv->imported_objects, object);

	if (CDN_IS_OBJECT (object))
	{
		register_imported_object (import, CDN_OBJECT (object));
	}
	else if (CDN_IS_VARIABLE (object))
	{
		register_imported_property (import, CDN_VARIABLE (object));
	}
	else
	{
		g_warning ("Unknown imported object: %s",
		           g_type_name (G_TYPE_FROM_INSTANCE (object)));
	}
}

static gboolean
object_in_templates (CdnNetwork *network,
                     CdnObject  *obj)
{
	CdnObject *tg;

	tg = CDN_OBJECT (cdn_network_get_template_node (network));

	while (obj)
	{
		if (obj == tg)
		{
			return TRUE;
		}

		obj = CDN_OBJECT (cdn_object_get_parent (obj));
	}

	return FALSE;
}

static void
import_objects (CdnImport *self,
                CdnNode  *parent)
{
	GSList *children;

	children = g_slist_copy ((GSList *)cdn_node_get_children (parent));

	while (children)
	{
		g_object_ref (children->data);

		cdn_node_remove (parent, children->data, NULL);
		cdn_node_add (CDN_NODE (self), children->data, NULL);

		g_object_unref (children->data);

		add_imported_object (self, children->data);

		children = g_slist_delete_link (children, children);
	}
}

static void
import_templates (CdnImport  *self,
                  CdnNetwork *network)
{
	import_objects (self, cdn_network_get_template_node (network));
}

static void
auto_import_templates (CdnImport  *self,
                       CdnNetwork *source,
                       CdnNetwork *target)
{
	/* Auto-import templates into templates */
	CdnImport *auto_import;

	auto_import = g_object_new (CDN_TYPE_IMPORT,
	                            "id", cdn_object_get_id (CDN_OBJECT (self)),
	                            "file", self->priv->file,
	                            "auto-imported", TRUE,
	                            NULL);

	import_templates (auto_import, source);

	cdn_node_add (cdn_network_get_template_node (target),
	               CDN_OBJECT (auto_import),
	               NULL);

	add_imported_object (self, auto_import);
	g_object_unref (auto_import);
}

static void
import_globals (CdnImport  *self,
                CdnNetwork *source,
                CdnNetwork *target)
{
	GSList *variables = cdn_object_get_variables (CDN_OBJECT (source));
	GSList *item;

	for (item = variables; item; item = g_slist_next (item))
	{
		CdnVariable *property = item->data;

		if (!cdn_object_get_variable (CDN_OBJECT (target),
		                              cdn_variable_get_name (property)))
		{
			CdnVariable *copy = cdn_variable_copy (property);

			cdn_object_add_variable (CDN_OBJECT (target), copy, NULL);
			add_imported_object (self, copy);
		}
	}
}

/**
 * cdn_import_load:
 * @self: A #CdnImport
 * @network: A #CdnNetwork
 * @parent: (allow-none): A #CdnNode
 * @error: A #GError
 *
 * Perform the actual import. This function is called by #cdn_import_new and
 * should never have to be used manually. It's provided for use in bindings.
 *
 * Returns: %TRUE if the import was successful, %FALSE otherwise.
 *
 **/
gboolean
cdn_import_load (CdnImport   *self,
                 CdnNetwork  *network,
                 CdnNode     *parent,
                 GError     **error)
{
	gchar const *annotation;

	g_return_val_if_fail (CDN_IS_IMPORT (self), FALSE);
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (CDN_IS_NODE (parent), FALSE);

	if (self->priv->file == NULL)
	{
		return import_failed (error,
		                      CDN_NETWORK_LOAD_ERROR_IMPORT,
		                      "Import filename was not specified");
	}

	CdnNetwork *imported = cdn_network_new ();

	if (!cdn_network_load_from_file (imported, self->priv->file, error))
	{
		return FALSE;
	}

	annotation = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (imported));

	if (annotation)
	{
		cdn_annotatable_set_annotation (CDN_ANNOTATABLE (self), annotation);
	}

	/* Import globals */
	import_globals (self, network, imported);

	/* Check if importing in templates or normal objects */
	gboolean templ = object_in_templates (network,
	                                      CDN_OBJECT (parent));

	if (!templ)
	{
		auto_import_templates (self, imported, network);

		/* Import objects */
		import_objects (self, CDN_NODE (imported));
	}
	else
	{
		/* Import templates into the import */
		import_templates (self, imported);
	}

	cdn_node_add (parent, CDN_OBJECT (self), NULL);

	self->priv->modified = FALSE;
	g_object_notify (G_OBJECT (self), "modified");

	_cdn_network_register_import (network, self);

	g_object_unref (imported);

	return TRUE;
}

/**
 * cdn_import_get_file:
 * @self: A #CdnImport
 *
 * Get the file that was imported.
 *
 * Returns: (transfer full) (allow-none): A #GFile
 *
 **/
GFile *
cdn_import_get_file (CdnImport *self)
{
	g_return_val_if_fail (CDN_IS_IMPORT (self), NULL);

	return self->priv->file ? g_file_dup (self->priv->file) : NULL;
}

/**
 * cdn_import_get_path:
 * @self: A #CdnImport
 *
 * Get the path that was imported.
 *
 * Returns: (transfer full) (allow-none): the path
 *
 **/
gchar *
cdn_import_get_path (CdnImport *self)
{
	g_return_val_if_fail (CDN_IS_IMPORT (self), NULL);

	return self->priv->file ? g_file_get_path (self->priv->file) : NULL;
}

/**
 * cdn_import_get_search_path:
 *
 * Get the search directories used to resolve import file names.
 *
 * Returns: (array zero-terminated=1) (transfer none): a %NULL terminated list of strings
 *
 **/
G_CONST_RETURN gchar * G_CONST_RETURN *
cdn_import_get_search_path (void)
{
	if (import_search_path == NULL)
	{
		import_search_path = default_search_path ();
	}

	return (G_CONST_RETURN gchar * G_CONST_RETURN *)import_search_path;
}

/**
 * cdn_import_set_search_path:
 * @path: (array zero-terminated=1): The search directories
 *
 * Set the search directories used to resolve import file names. @path should
 * be a %NULL terminated list of strings.
 *
 **/
void
cdn_import_set_search_path (gchar **path)
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
 * cdn_import_append_search_path:
 * @path: A directory path
 *
 * Append a search directory path to the list of paths to be searched when
 * resolving an import file.
 *
 **/
void
cdn_import_append_search_path (gchar const *path)
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
 * cdn_import_prepend_search_path:
 * @path: A directory path
 *
 * Prepend a search directory path to the list of paths to be searched when
 * resolving an import file.
 *
 **/
void
cdn_import_prepend_search_path (gchar const *path)
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
cdn_import_imports_object (CdnImport *self,
                           CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_IMPORT (self), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	return g_slist_find (self->priv->imported_objects, object) != NULL;
}
