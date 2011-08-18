/*
 * cpg-property-interface.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-property-interface.h"
#include "cpg-group.h"
#include "cpg-tokenizer.h"
#include "cpg-marshal.h"

/**
 * SECTION:cpg-property-interface
 * @short_description: Property interface for #CpgGroup
 *
 * The property interface is a mapping of property aliases to arbitrary
 * properties. Each #CpgGroup has a property interface which manages which
 * properties of children of the group are exposed on the group itself.
 *
 * This allows for controlled accessibility of certain properties of certain
 * children of a group, and ensures data integrity (in a sense).
 *
 */

#define CPG_PROPERTY_INTERFACE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PROPERTY_INTERFACE, CpgPropertyInterfacePrivate))

typedef struct
{
	gchar *child_name;
	gchar *name;
} Property;

static Property *
property_new (gchar const *child_name,
              gchar const *name)
{
	Property *ret;

	ret = g_slice_new0 (Property);

	ret->child_name = g_strdup (child_name);
	ret->name = g_strdup (name);

	return ret;
}

static void
property_free (Property *self)
{
	g_free (self->child_name);
	g_free (self->name);

	g_slice_free (Property, self);
}

struct _CpgPropertyInterfacePrivate
{
	CpgGroup *group;

	GPtrArray *names;

	GHashTable *properties;
};

G_DEFINE_TYPE (CpgPropertyInterface, cpg_property_interface, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_GROUP
};

enum
{
	ADDED,
	REMOVED,
	VERIFY_ADD,
	VERIFY_REMOVE,
	NUM_SIGNALS
};

static gulong signals[NUM_SIGNALS];

GQuark
cpg_property_interface_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_property_interface_error");
	}

	return quark;
}

static void
cpg_property_interface_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_property_interface_parent_class)->finalize (object);
}

static void
cpg_property_interface_dispose (GObject *object)
{
	CpgPropertyInterface *iface;

	iface = CPG_PROPERTY_INTERFACE (object);

	if (iface->priv->properties)
	{
		g_hash_table_destroy (iface->priv->properties);
		iface->priv->properties = NULL;
	}

	if (iface->priv->names)
	{
		g_ptr_array_free (iface->priv->names, TRUE);
		iface->priv->names = NULL;
	}

	G_OBJECT_CLASS (cpg_property_interface_parent_class)->dispose (object);
}

static void
cpg_property_interface_set_property (GObject      *object,
                                     guint         prop_id,
                                     const GValue *value,
                                     GParamSpec   *pspec)
{
	CpgPropertyInterface *self = CPG_PROPERTY_INTERFACE (object);

	switch (prop_id)
	{
		case PROP_GROUP:
			self->priv->group = g_value_get_object (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_property_interface_get_property (GObject    *object,
                                     guint       prop_id,
                                     GValue     *value,
                                     GParamSpec *pspec)
{
	CpgPropertyInterface *self = CPG_PROPERTY_INTERFACE (object);

	switch (prop_id)
	{
		case PROP_GROUP:
			g_value_set_object (value, self->priv->group);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cpg_property_interface_verify_remove_impl (CpgPropertyInterface  *iface,
                                           gchar const           *name,
                                           gchar const           *child_name,
                                           gchar const           *property_name,
                                           GError               **error)
{
	Property *property;

	property = g_hash_table_lookup (iface->priv->properties, name);

	if (!property)
	{
		gchar *id;

		id = cpg_object_get_full_id (CPG_OBJECT (iface->priv->group));

		g_set_error (error,
		             CPG_PROPERTY_INTERFACE_ERROR,
		             CPG_PROPERTY_INTERFACE_ERROR_NOT_FOUND,
		             "The property `%s' does not exist on the interface of `%s'",
		             name,
		             id);

		g_free (id);
		return FALSE;
	}

	return TRUE;

}

static gboolean
cpg_property_interface_verify_add_impl (CpgPropertyInterface  *iface,
                                        gchar const           *name,
                                        gchar const           *child_name,
                                        gchar const           *property_name,
                                        GError               **error)
{
	if (!cpg_tokenizer_validate_identifier (name))
	{
		gchar *id;

		id = cpg_object_get_full_id (CPG_OBJECT (iface->priv->group));

		g_set_error (error,
		             CPG_OBJECT_ERROR,
		             CPG_OBJECT_ERROR_INVALID_PROPERTY_NAME,
		             "Invalid interface property name `%s' for interface of `%s'",
		             name,
		             id);

		g_free (id);

		return FALSE;
	}

	if (g_hash_table_lookup (iface->priv->properties, name))
	{
		gchar *id;

		id = cpg_object_get_full_id (CPG_OBJECT (iface->priv->group));

		g_set_error (error,
		             CPG_PROPERTY_INTERFACE_ERROR,
		             CPG_PROPERTY_INTERFACE_ERROR_EXISTS,
		             "The property `%s' already exists on the interface of `%s'",
		             name,
		             id);

		g_free (id);
		return FALSE;
	}

	return TRUE;
}

static void
cpg_property_interface_class_init (CpgPropertyInterfaceClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_property_interface_finalize;

	object_class->get_property = cpg_property_interface_get_property;
	object_class->set_property = cpg_property_interface_set_property;

	object_class->dispose = cpg_property_interface_dispose;

	klass->verify_add = cpg_property_interface_verify_add_impl;
	klass->verify_remove = cpg_property_interface_verify_remove_impl;

	g_type_class_add_private (object_class, sizeof(CpgPropertyInterfacePrivate));

	signals[ADDED] =
		g_signal_new ("added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgPropertyInterfaceClass,
		                               added),
		              NULL,
		              NULL,
		              cpg_marshal_VOID__STRING_STRING_STRING,
		              G_TYPE_NONE,
		              3,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING);

	signals[REMOVED] =
		g_signal_new ("removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgPropertyInterfaceClass,
		                               removed),
		              NULL,
		              NULL,
		              cpg_marshal_VOID__STRING_STRING_STRING,
		              G_TYPE_NONE,
		              3,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING);

	signals[VERIFY_REMOVE] =
		g_signal_new ("verify-remove",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgPropertyInterfaceClass,
		                               verify_remove),
		              cpg_signal_accumulator_false_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__STRING_STRING_STRING_POINTER,
		              G_TYPE_BOOLEAN,
		              4,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_POINTER);

	signals[VERIFY_ADD] =
		g_signal_new ("verify-add",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgPropertyInterfaceClass,
		                               verify_add),
		              cpg_signal_accumulator_false_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__STRING_STRING_STRING_POINTER,
		              G_TYPE_BOOLEAN,
		              4,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_POINTER);

	g_object_class_install_property (object_class,
	                                 PROP_GROUP,
	                                 g_param_spec_object ("group",
	                                                      "Group",
	                                                      "Group",
	                                                      CPG_TYPE_GROUP,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cpg_property_interface_init (CpgPropertyInterface *self)
{
	self->priv = CPG_PROPERTY_INTERFACE_GET_PRIVATE (self);

	self->priv->names = g_ptr_array_new_with_free_func ((GDestroyNotify)g_free);
	g_ptr_array_add (self->priv->names, NULL);

	self->priv->properties = g_hash_table_new_full (g_str_hash,
	                                                g_str_equal,
	                                                (GDestroyNotify)g_free,
	                                                (GDestroyNotify)property_free);
}

/**
 * cpg_property_interface_new:
 * @object: A #CpgObject
 *
 * Create a new property interface mapping.
 *
 * Returns: A #CpgPropertyInterface
 *
 **/
CpgPropertyInterface *
cpg_property_interface_new (CpgGroup *group)
{
	return g_object_new (CPG_TYPE_PROPERTY_INTERFACE,
	                     "group", group,
	                     NULL);
}

/**
 * cpg_property_interface_lookup:
 * @iface: A #CpgPropertyInterface
 * @name: A mapping name
 * 
 * Get the property corresponding to a certain mapping name.
 *
 * Returns: (transfer none): A #CpgProperty
 *
 **/
CpgProperty *
cpg_property_interface_lookup (CpgPropertyInterface *iface,
                               gchar const          *name)
{
	Property *prop;
	CpgObject *child;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, FALSE);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	child = cpg_group_get_child (iface->priv->group, prop->child_name);

	if (!child)
	{
		return NULL;
	}

	return cpg_object_get_property (child, prop->name);
}

/**
 * cpg_property_interface_add:
 * @iface: A #CpgPropertyInterface
 * @name: The mapping name
 * @property: A #CpgProperty
 * @error: A #GError
 *
 * Add a mapping to the interface.
 *
 * Returns: %TRUE if the mapping was added successfully, %FALSE otherwise
 *
 **/
gboolean
cpg_property_interface_add (CpgPropertyInterface  *iface,
                            gchar const           *name,
                            gchar const           *child_name,
                            gchar const           *property_name,
                            GError               **error)
{
	gboolean ret = FALSE;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);
	g_return_val_if_fail (child_name != NULL, FALSE);
	g_return_val_if_fail (property_name != NULL, FALSE);

	g_signal_emit (iface,
	               signals[VERIFY_ADD],
	               0,
	               name,
	               child_name,
	               property_name,
	               error,
	               &ret);

	if (ret)
	{
		return FALSE;
	}

	g_hash_table_insert (iface->priv->properties,
	                     g_strdup (name),
	                     property_new (child_name, property_name));

	g_ptr_array_remove_index (iface->priv->names, iface->priv->names->len - 1);

	g_ptr_array_add (iface->priv->names, g_strdup (name));
	g_ptr_array_add (iface->priv->names, NULL);

	g_signal_emit (iface, signals[ADDED], 0, name, child_name, property_name);

	return TRUE;
}

/**
 * cpg_property_interface_remove:
 * @iface: A #CpgPropertyInterface
 * @name: The mapping name
 * @error: A #GError
 *
 * Remove a mapping from the interface.
 *
 * Returns: %TRUE if the mapping was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cpg_property_interface_remove (CpgPropertyInterface  *iface,
                               gchar const           *name,
                               GError               **error)
{
	gint i;
	Property *property;
	gboolean ret = FALSE;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	property = g_hash_table_lookup (iface->priv->properties, name);

	if (!property)
	{
		g_set_error (error,
		             CPG_OBJECT_ERROR,
		             CPG_OBJECT_ERROR_PROPERTY_NOT_FOUND,
		             "Interface property `%s' does not exist",
		             name);

		return FALSE;
	}

	g_signal_emit (iface,
	               signals[VERIFY_REMOVE],
	               0,
	               name,
	               property->child_name,
	               property->name,
	               error,
	               &ret);

	if (ret)
	{
		return FALSE;
	}

	/* Make a copy for the signal emission */
	property = property_new (property->child_name, property->name);

	g_hash_table_remove (iface->priv->properties, name);

	for (i = 0; i < iface->priv->names->len; ++i)
	{
		gchar const *ptr;

		ptr = g_ptr_array_index (iface->priv->names, i);

		if (g_strcmp0 (ptr, name) == 0)
		{
			g_ptr_array_remove_index (iface->priv->names, i);
			break;
		}
	}

	g_signal_emit (iface, signals[REMOVED], 0, name, property->child_name, property->name);
	property_free (property);

	return TRUE;
}

/**
 * cpg_property_interface_get_group:
 * @iface: A #CpgPropertyInterface
 *
 * Get the object on which the interface is defined.
 *
 * Returns: (transfer none): A #CpgGroup
 *
 **/
CpgGroup *
cpg_property_interface_get_group (CpgPropertyInterface *iface)
{
	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), NULL);

	return iface->priv->group;
}

/**
 * cpg_property_interface_get_names:
 * @iface: A #CpgPropertyInterface
 *
 * Get the names of the mappings defined on the interface
 *
 * Returns: (transfer full): A %NULL terminated list of strings
 *
 **/
gchar **
cpg_property_interface_get_names (CpgPropertyInterface *iface)
{
	GPtrArray *ptr;
	gint i;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), NULL);

	ptr = g_ptr_array_sized_new (iface->priv->names->len);

	for (i = 0; i < iface->priv->names->len; ++i)
	{
		g_ptr_array_add (ptr, g_strdup (g_ptr_array_index (iface->priv->names, i)));
	}

	return (gchar **)g_ptr_array_free (ptr, FALSE);
}

gboolean
cpg_property_interface_implements (CpgPropertyInterface *iface,
                                   gchar const          *name)
{
	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return g_hash_table_lookup (iface->priv->properties, name) != NULL;
}


gchar const *
cpg_property_interface_lookup_child_name (CpgPropertyInterface  *iface,
                                          gchar const           *name)
{
	Property *prop;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	return prop->child_name;
}

gchar const *
cpg_property_interface_lookup_property_name (CpgPropertyInterface *iface,
                                             gchar const          *name)
{
	Property *prop;

	g_return_val_if_fail (CPG_IS_PROPERTY_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	return prop->name;
}
