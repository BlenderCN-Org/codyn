/*
 * cdn-variable-interface.c
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

#include "cdn-variable-interface.h"
#include "cdn-node.h"
#include "cdn-tokenizer.h"
#include "cdn-marshal.h"

#define CDN_VARIABLE_INTERFACE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_VARIABLE_INTERFACE, CdnVariableInterfacePrivate))

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

struct _CdnVariableInterfacePrivate
{
	CdnNode *node;

	GPtrArray *names;

	GHashTable *properties;
};

G_DEFINE_TYPE (CdnVariableInterface, cdn_variable_interface, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NODE
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

/**
 * cdn_variable_interface_error_quark:
 *
 * Get the error quark for the variable interface error type.
 *
 * Returns: a #GQuark for the variable interface error type
 *
 */
GQuark
cdn_variable_interface_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_variable_interface_error");
	}

	return quark;
}

static void
cdn_variable_interface_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_variable_interface_parent_class)->finalize (object);
}

static void
cdn_variable_interface_dispose (GObject *object)
{
	CdnVariableInterface *iface;

	iface = CDN_VARIABLE_INTERFACE (object);

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

	G_OBJECT_CLASS (cdn_variable_interface_parent_class)->dispose (object);
}

static void
cdn_variable_interface_set_property (GObject      *object,
                                     guint         prop_id,
                                     const GValue *value,
                                     GParamSpec   *pspec)
{
	CdnVariableInterface *self = CDN_VARIABLE_INTERFACE (object);

	switch (prop_id)
	{
		case PROP_NODE:
			self->priv->node = g_value_get_object (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_variable_interface_get_property (GObject    *object,
                                     guint       prop_id,
                                     GValue     *value,
                                     GParamSpec *pspec)
{
	CdnVariableInterface *self = CDN_VARIABLE_INTERFACE (object);

	switch (prop_id)
	{
		case PROP_NODE:
			g_value_set_object (value, self->priv->node);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static gboolean
cdn_variable_interface_verify_remove_impl (CdnVariableInterface  *iface,
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

		id = cdn_object_get_full_id (CDN_OBJECT (iface->priv->node));

		g_set_error (error,
		             CDN_VARIABLE_INTERFACE_ERROR,
		             CDN_VARIABLE_INTERFACE_ERROR_NOT_FOUND,
		             "The property `%s' does not exist on the interface of `%s'",
		             name,
		             id);

		g_free (id);
		return FALSE;
	}

	return TRUE;

}

static gboolean
cdn_variable_interface_verify_add_impl (CdnVariableInterface  *iface,
                                        gchar const           *name,
                                        gchar const           *child_name,
                                        gchar const           *property_name,
                                        GError               **error)
{
	if (!cdn_tokenizer_validate_identifier (name))
	{
		gchar *id;

		id = cdn_object_get_full_id (CDN_OBJECT (iface->priv->node));

		g_set_error (error,
		             CDN_OBJECT_ERROR,
		             CDN_OBJECT_ERROR_INVALID_VARIABLE_NAME,
		             "Invalid interface property name `%s' for interface of `%s'",
		             name,
		             id);

		g_free (id);

		return FALSE;
	}

	if (g_hash_table_lookup (iface->priv->properties, name))
	{
		gchar *id;

		id = cdn_object_get_full_id (CDN_OBJECT (iface->priv->node));

		g_set_error (error,
		             CDN_VARIABLE_INTERFACE_ERROR,
		             CDN_VARIABLE_INTERFACE_ERROR_EXISTS,
		             "The property `%s' already exists on the interface of `%s'",
		             name,
		             id);

		g_free (id);
		return FALSE;
	}

	return TRUE;
}

static void
cdn_variable_interface_class_init (CdnVariableInterfaceClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_variable_interface_finalize;

	object_class->get_property = cdn_variable_interface_get_property;
	object_class->set_property = cdn_variable_interface_set_property;

	object_class->dispose = cdn_variable_interface_dispose;

	klass->verify_add = cdn_variable_interface_verify_add_impl;
	klass->verify_remove = cdn_variable_interface_verify_remove_impl;

	g_type_class_add_private (object_class, sizeof(CdnVariableInterfacePrivate));

	signals[ADDED] =
		g_signal_new ("added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableInterfaceClass,
		                               added),
		              NULL,
		              NULL,
		              cdn_marshal_VOID__STRING_STRING_STRING,
		              G_TYPE_NONE,
		              3,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING);

	signals[REMOVED] =
		g_signal_new ("removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableInterfaceClass,
		                               removed),
		              NULL,
		              NULL,
		              cdn_marshal_VOID__STRING_STRING_STRING,
		              G_TYPE_NONE,
		              3,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING);

	signals[VERIFY_REMOVE] =
		g_signal_new ("verify-remove",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableInterfaceClass,
		                               verify_remove),
		              cdn_signal_accumulator_false_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__STRING_STRING_STRING_POINTER,
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
		              G_STRUCT_OFFSET (CdnVariableInterfaceClass,
		                               verify_add),
		              cdn_signal_accumulator_false_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__STRING_STRING_STRING_POINTER,
		              G_TYPE_BOOLEAN,
		              4,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_STRING,
		              G_TYPE_POINTER);

	g_object_class_install_property (object_class,
	                                 PROP_NODE,
	                                 g_param_spec_object ("node",
	                                                      "Node",
	                                                      "Node",
	                                                      CDN_TYPE_NODE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_variable_interface_init (CdnVariableInterface *self)
{
	self->priv = CDN_VARIABLE_INTERFACE_GET_PRIVATE (self);

	self->priv->names = g_ptr_array_new_with_free_func ((GDestroyNotify)g_free);
	g_ptr_array_add (self->priv->names, NULL);

	self->priv->properties = g_hash_table_new_full (g_str_hash,
	                                                g_str_equal,
	                                                (GDestroyNotify)g_free,
	                                                (GDestroyNotify)property_free);
}

/**
 * cdn_variable_interface_new:
 * @node: A #CdnNode
 *
 * Create a new variable interface mapping.
 *
 * Returns: A #CdnVariableInterface
 *
 **/
CdnVariableInterface *
cdn_variable_interface_new (CdnNode *node)
{
	return g_object_new (CDN_TYPE_VARIABLE_INTERFACE,
	                     "node", node,
	                     NULL);
}

/**
 * cdn_variable_interface_lookup:
 * @iface: A #CdnVariableInterface
 * @name: A mapping name
 *
 * Get the property corresponding to a certain mapping name.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_variable_interface_lookup (CdnVariableInterface *iface,
                               gchar const          *name)
{
	Property *prop;
	CdnObject *child;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, FALSE);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	child = cdn_node_get_child (iface->priv->node, prop->child_name);

	if (!child)
	{
		return NULL;
	}

	return cdn_object_get_variable (child, prop->name);
}

/**
 * cdn_variable_interface_add:
 * @iface: A #CdnVariableInterface
 * @name: The mapping name
 * @variable_name: A variable name
 * @error: A #GError
 *
 * Add a mapping to the interface.
 *
 * Returns: %TRUE if the mapping was added successfully, %FALSE otherwise
 *
 **/
gboolean
cdn_variable_interface_add (CdnVariableInterface  *iface,
                            gchar const           *name,
                            gchar const           *child_name,
                            gchar const           *variable_name,
                            GError               **error)
{
	gboolean ret = FALSE;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);
	g_return_val_if_fail (child_name != NULL, FALSE);
	g_return_val_if_fail (variable_name != NULL, FALSE);

	g_signal_emit (iface,
	               signals[VERIFY_ADD],
	               0,
	               name,
	               child_name,
	               variable_name,
	               error,
	               &ret);

	if (ret)
	{
		return FALSE;
	}

	g_hash_table_insert (iface->priv->properties,
	                     g_strdup (name),
	                     property_new (child_name, variable_name));

	g_ptr_array_remove_index (iface->priv->names, iface->priv->names->len - 1);

	g_ptr_array_add (iface->priv->names, g_strdup (name));
	g_ptr_array_add (iface->priv->names, NULL);

	g_signal_emit (iface, signals[ADDED], 0, name, child_name, variable_name);

	return TRUE;
}

/**
 * cdn_variable_interface_remove:
 * @iface: A #CdnVariableInterface
 * @name: The mapping name
 * @error: A #GError
 *
 * Remove a mapping from the interface.
 *
 * Returns: %TRUE if the mapping was successfully removed, %FALSE otherwise
 *
 **/
gboolean
cdn_variable_interface_remove (CdnVariableInterface  *iface,
                               gchar const           *name,
                               GError               **error)
{
	gint i;
	Property *property;
	gboolean ret = FALSE;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	property = g_hash_table_lookup (iface->priv->properties, name);

	if (!property)
	{
		g_set_error (error,
		             CDN_OBJECT_ERROR,
		             CDN_OBJECT_ERROR_VARIABLE_NOT_FOUND,
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
 * cdn_variable_interface_get_node:
 * @iface: A #CdnVariableInterface
 *
 * Get the object on which the interface is defined.
 *
 * Returns: (transfer none): A #CdnNode
 *
 **/
CdnNode *
cdn_variable_interface_get_node (CdnVariableInterface *iface)
{
	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), NULL);

	return iface->priv->node;
}

/**
 * cdn_variable_interface_get_names:
 * @iface: A #CdnVariableInterface
 *
 * Get the names of the mappings defined on the interface
 *
 * Returns: (transfer full): A %NULL terminated list of strings
 *
 **/
gchar **
cdn_variable_interface_get_names (CdnVariableInterface *iface)
{
	GPtrArray *ptr;
	gint i;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), NULL);

	ptr = g_ptr_array_sized_new (iface->priv->names->len);

	for (i = 0; i < iface->priv->names->len; ++i)
	{
		g_ptr_array_add (ptr, g_strdup (g_ptr_array_index (iface->priv->names, i)));
	}

	return (gchar **)g_ptr_array_free (ptr, FALSE);
}

/**
 * cdn_variable_interface_implements:
 * @iface: the #CdnVariableInterface
 * @name: the name
 *
 * Get whether the variable interface implements a variable with the given name.
 *
 * Returns: %TRUE if implemented, %FALSE otherwise
 *
 */
gboolean
cdn_variable_interface_implements (CdnVariableInterface *iface,
                                   gchar const          *name)
{
	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return g_hash_table_lookup (iface->priv->properties, name) != NULL;
}

/**
 * cdn_variable_interface_lookup_child_name:
 * @iface: the #CdnVariableInterface
 * @name: the variable name
 *
 * Lookup the name of the child for which @name provides an interface.
 *
 * Returns: the child name.
 *
 */
gchar const *
cdn_variable_interface_lookup_child_name (CdnVariableInterface  *iface,
                                          gchar const           *name)
{
	Property *prop;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	return prop->child_name;
}

/**
 * cdn_variable_interface_lookup_variable_name:
 * @iface: the #CdnVariableInterface
 * @name: the variable name
 *
 * Lookup the name of the child variable for which @name provides an interface.
 *
 * Returns: the child variable name.
 *
 */
gchar const *
cdn_variable_interface_lookup_variable_name (CdnVariableInterface *iface,
                                             gchar const          *name)
{
	Property *prop;

	g_return_val_if_fail (CDN_IS_VARIABLE_INTERFACE (iface), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	prop = g_hash_table_lookup (iface->priv->properties, name);

	if (!prop)
	{
		return NULL;
	}

	return prop->name;
}
