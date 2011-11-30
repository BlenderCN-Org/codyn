/*
 * cdn-attribute.c
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

#include "cdn-attribute.h"

/**
 * SECTION:cdn-attribute
 * @short_description: Cdn format attribute
 *
 * This object represents an attribute in the Cdn format.
 *
 **/

#define CDN_ATTRIBUTE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_ATTRIBUTE, CdnAttributePrivate))

struct _CdnAttributePrivate
{
	gchar *id;
	GSList *arguments;
	gint num_arguments;
};

G_DEFINE_TYPE (CdnAttribute, cdn_attribute, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_ID
};

static void
cdn_attribute_finalize (GObject *object)
{
	CdnAttribute *self = CDN_ATTRIBUTE (object);

	g_free (self->priv->id);

	cdn_attribute_set_arguments (self, NULL);

	G_OBJECT_CLASS (cdn_attribute_parent_class)->finalize (object);
}

static void
cdn_attribute_set_property (GObject *object, guint prop_id, const GValue *value, GParamSpec *pspec)
{
	CdnAttribute *self = CDN_ATTRIBUTE (object);

	switch (prop_id)
	{
		case PROP_ID:
			self->priv->id = g_value_dup_string (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_attribute_get_property (GObject *object, guint prop_id, GValue *value, GParamSpec *pspec)
{
	CdnAttribute *self = CDN_ATTRIBUTE (object);

	switch (prop_id)
	{
		case PROP_ID:
			g_value_set_string (value, self->priv->id);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_attribute_class_init (CdnAttributeClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	
	object_class->finalize = cdn_attribute_finalize;

	object_class->get_property = cdn_attribute_get_property;
	object_class->set_property = cdn_attribute_set_property;

	g_type_class_add_private (object_class, sizeof(CdnAttributePrivate));

	g_object_class_install_property (object_class,
	                                 PROP_ID,
	                                 g_param_spec_string ("id",
	                                                      "Id",
	                                                      "Id",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
}

static void
cdn_attribute_init (CdnAttribute *self)
{
	self->priv = CDN_ATTRIBUTE_GET_PRIVATE (self);
}

/**
 * cdn_attribute_new:
 * @id: attribute id
 *
 * Create a new attribute.
 *
 * Returns: A #CdnAttribute
 *
 **/
CdnAttribute *
cdn_attribute_new (gchar const *id)
{
	CdnAttribute *ret;

	ret = g_object_new (CDN_TYPE_ATTRIBUTE,
	                    "id", id,
	                     NULL);

	return ret;
}

/**
 * cdn_attribute_newv:
 * @id: the attribute id
 *
 * Create a new attribute.
 *
 * Returns: (transfer full): the new attribute
 *
 **/
CdnAttribute *
cdn_attribute_newv (gchar const *id,
                    ...)
{
	CdnAttribute *ret;
	va_list ap;
	GObject *obj;

	ret = g_object_new (CDN_TYPE_ATTRIBUTE,
	                    "id", id,
	                     NULL);

	va_start (ap, id);

	while ((obj = G_OBJECT (va_arg (ap, GObject *))) != NULL)
	{
		ret->priv->arguments =
			g_slist_prepend (ret->priv->arguments,
			                 g_object_ref (obj));

		++ret->priv->num_arguments;
	}

	ret->priv->arguments = g_slist_reverse (ret->priv->arguments);

	va_end (ap);

	return ret;
}

/**
 * cdn_attribute_set_arguments:
 * @attribute: A #CdnAttribute
 * @arguments: (element-type GObject) (transfer none): A list of #GObject
 *
 * Set the arguments of the attribute.
 *
 **/
void
cdn_attribute_set_arguments (CdnAttribute *attribute,
                             GSList       *arguments)
{
	g_return_if_fail (CDN_IS_ATTRIBUTE (attribute));

	g_slist_foreach (attribute->priv->arguments, (GFunc)g_object_unref, NULL);
	g_slist_free (attribute->priv->arguments);

	attribute->priv->arguments = NULL;
	attribute->priv->num_arguments = 0;

	while (arguments)
	{
		attribute->priv->arguments =
			g_slist_prepend (attribute->priv->arguments,
			                 g_object_ref (arguments->data));

		++attribute->priv->num_arguments;
		arguments = g_slist_next (arguments);
	}

	attribute->priv->arguments = g_slist_reverse (attribute->priv->arguments);
}

/**
 * cdn_attribute_get_id:
 * @attribute: A #CdnAttribute
 *
 * Get the attribute id.
 *
 * Returns: the attribute id.
 *
 **/
gchar const *
cdn_attribute_get_id (CdnAttribute *attribute)
{
	g_return_val_if_fail (CDN_IS_ATTRIBUTE (attribute), NULL);

	return attribute->priv->id;
}

/**
 * cdn_attribute_get_arguments:
 * @attribute: A #CdnAttribute
 *
 * Get the list of arguments of the attribute.
 *
 * Returns: (element-type GObject) (transfer none): A #GSList of #GObject
 *
 **/
GSList *
cdn_attribute_get_arguments (CdnAttribute *attribute)
{
	g_return_val_if_fail (CDN_IS_ATTRIBUTE (attribute), NULL);

	return attribute->priv->arguments;
}

/**
 * cdn_attribute_get_argument:
 * @attribute: A #CdnAttribute
 * @i: The index of the argument
 *
 * Get an argument.
 *
 * Returns: (transfer none): The argument
 *
 **/
GObject *
cdn_attribute_get_argument (CdnAttribute *attribute,
                            gint          i)
{
	g_return_val_if_fail (CDN_IS_ATTRIBUTE (attribute), NULL);

	return g_slist_nth_data (attribute->priv->arguments, i);
}

/**
 * cdn_attribute_num_arguments:
 * @attribute: A #CdnAttribute
 *
 * Get the number of arguments.
 *
 * Returns: the number of attribute arguments
 *
 **/
gint
cdn_attribute_num_arguments (CdnAttribute *attribute)
{
	g_return_val_if_fail (CDN_IS_ATTRIBUTE (attribute), 0);

	return attribute->priv->num_arguments;
}
