/*
 * cdn-compile-error.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * codyn is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * codyn is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with codyn; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-compile-error.h"

#include <string.h>

/**
 * SECTION:cdn-compile-error
 * @short_description: Compile error message container
 *
 * Object used to store information on expression compile errors.
 *
 */

#define CDN_COMPILE_ERROR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_COMPILE_ERROR, CdnCompileErrorPrivate))

struct _CdnCompileErrorPrivate
{
	GError *error;

	CdnObject *object;
	CdnVariable *property;
	CdnEdgeAction *action;
	CdnExpression *expression;
};

G_DEFINE_TYPE (CdnCompileError, cdn_compile_error, G_TYPE_OBJECT)

GQuark
cdn_compile_error_type_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_compile_error_type");
	}

	return quark;
}

static void
clear_objects (CdnCompileError *error)
{
	if (error->priv->error)
	{
		g_error_free (error->priv->error);
		error->priv->error = NULL;
	}

	if (error->priv->object)
	{
		g_object_unref (error->priv->object);
		error->priv->object = NULL;
	}

	if (error->priv->property)
	{
		g_object_unref (error->priv->property);
		error->priv->property = NULL;
	}

	if (error->priv->action)
	{
		g_object_unref (error->priv->action);
		error->priv->action = NULL;
	}

	if (error->priv->expression)
	{
		g_object_unref (error->priv->expression);
		error->priv->expression = NULL;
	}
}

static void
cdn_compile_error_dispose (GObject *object)
{
	CdnCompileError *error = CDN_COMPILE_ERROR (object);

	clear_objects (error);

	G_OBJECT_CLASS (cdn_compile_error_parent_class)->dispose (object);
}

static void
cdn_compile_error_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_compile_error_parent_class)->finalize (object);
}

static void
cdn_compile_error_class_init (CdnCompileErrorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->dispose = cdn_compile_error_dispose;
	object_class->finalize = cdn_compile_error_finalize;

	g_type_class_add_private (object_class, sizeof(CdnCompileErrorPrivate));
}

static void
cdn_compile_error_init (CdnCompileError *self)
{
	self->priv = CDN_COMPILE_ERROR_GET_PRIVATE (self);
}

/**
 * cdn_compile_error_new:
 *
 * Create new empty compile error
 *
 * Returns: a new #CdnCompileError
 *
 **/
CdnCompileError *
cdn_compile_error_new ()
{
	return g_object_new (CDN_TYPE_COMPILE_ERROR, NULL);
}

/**
 * cdn_compile_error_set:
 * @error: A #CdnCompileError
 * @gerror: A #GError
 * @object: A #CdnObject
 * @property: A #CdnVariable
 * @action: A #CdnEdgeAction
 *
 * Set compile error information.
 *
 **/
void
cdn_compile_error_set (CdnCompileError *error,
                       GError          *gerror,
                       CdnObject       *object,
                       CdnVariable     *property,
                       CdnEdgeAction   *action,
                       CdnExpression   *expression)
{
	g_return_if_fail (CDN_IS_COMPILE_ERROR (error));
	g_return_if_fail (object == NULL || CDN_IS_OBJECT (object));
	g_return_if_fail (property == NULL || CDN_IS_VARIABLE (property));
	g_return_if_fail (action == NULL || CDN_IS_EDGE_ACTION (action));
	g_return_if_fail (expression == NULL || CDN_IS_EXPRESSION (expression));

	if (gerror)
	{
		if (error->priv->error)
		{
			g_error_free (error->priv->error);
		}

		error->priv->error = g_error_copy (gerror);
	}

	if (object)
	{
		if (error->priv->object)
		{
			g_object_unref (error->priv->object);
		}

		error->priv->object = g_object_ref (object);
	}

	if (property)
	{
		if (error->priv->property)
		{
			g_object_unref (error->priv->property);
		}

		error->priv->property = g_object_ref (property);

		if (!expression && !error->priv->expression)
		{
			error->priv->expression = g_object_ref (cdn_variable_get_expression (property));
		}
	}

	if (action)
	{
		if (error->priv->action)
		{
			g_object_unref (error->priv->action);
		}

		error->priv->action = g_object_ref (action);

		if (!expression && !error->priv->expression)
		{
			error->priv->expression = g_object_ref (cdn_edge_action_get_equation (action));
		}
	}

	if (expression)
	{
		if (error->priv->expression)
		{
			g_object_unref (error->priv->expression);
		}

		error->priv->expression = g_object_ref (expression);
	}
}

/**
 * cdn_compile_error_get_error:
 * @error: a #CdnCompileError
 *
 * Get the associated #GError
 *
 * Returns: (transfer none): the associated #GError
 *
 **/
GError *
cdn_compile_error_get_error (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	return error->priv->error;
}

/**
 * cdn_compile_error_get_object:
 * @error: a #CdnCompileError
 *
 * Get the associated #CdnObject
 *
 * Returns: (transfer none): the associated #CdnObject
 *
 **/
CdnObject *
cdn_compile_error_get_object (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	return error->priv->object;
}

/**
 * cdn_compile_error_get_variable:
 * @error: a #CdnCompileError
 *
 * Get the associated #CdnVariable
 *
 * Returns: (transfer none): the associated #CdnVariable
 *
 **/
CdnVariable *
cdn_compile_error_get_variable (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	return error->priv->property;
}

/**
 * cdn_compile_error_get_edge_action:
 * @error: a #CdnCompileError
 *
 * Get the associated #CdnEdgeAction
 *
 * Returns: (transfer none): the associated #CdnEdgeAction
 *
 **/
CdnEdgeAction *
cdn_compile_error_get_edge_action (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	return error->priv->action;
}

/**
 * cdn_compile_error_code_string:
 * @code: the error code
 *
 * Get the string describing an error with error code @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cdn_compile_error_code_string (gint code)
{
	switch (code)
	{
		case CDN_COMPILE_ERROR_VARIABLE_NOT_FOUND:
			return "Property not found";
		break;
		case CDN_COMPILE_ERROR_FUNCTION_NOT_FOUND:
			return "Function not found";
		break;
		case CDN_COMPILE_ERROR_INVALID_TOKEN:
			return "Invalid token";
		break;
		case CDN_COMPILE_ERROR_MAXARG:
			return "Maximum number of arguments";
		break;
		case CDN_COMPILE_ERROR_INVALID_STACK:
			return "Invalid stack";
		break;
		case CDN_COMPILE_ERROR_VARIABLE_RECURSE:
			return "Property recusion";
		break;
		default:
			return "Unknown";
		break;
	}
}

/**
 * cdn_compile_error_string:
 * @error: a #CdnCompileError
 *
 * Get the string describing @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cdn_compile_error_string (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	if (cdn_compile_error_get_error (error)->domain ==
	    CDN_COMPILE_ERROR_TYPE)
	{
		return cdn_compile_error_code_string (cdn_compile_error_get_code (error));
	}
	else
	{
		return "Compile error";
	}
}

/**
 * cdn_compile_error_get_code:
 * @error: a #CdnCompileError
 *
 * Get the error code
 *
 * Returns: the error code
 *
 **/
gint
cdn_compile_error_get_code (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), 0);

	return error->priv->error ? error->priv->error->code : 0;
}

/**
 * cdn_compile_error_get_message:
 * @error: a #CdnCompileError
 *
 * Get the error message
 *
 * Returns: the error message
 *
 **/
gchar const *
cdn_compile_error_get_message (CdnCompileError *error)
{
	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	return error->priv->error ? error->priv->error->message : "Unknown";
}

gchar *
cdn_compile_error_get_formatted_string (CdnCompileError *error)
{
	GString *ret;
	gchar *fullid;
	gchar const *expr;
	gchar *pad;
	gchar *prefix;
	gint start;
	gint end;
	gchar *strip = NULL;

	g_return_val_if_fail (CDN_IS_COMPILE_ERROR (error), NULL);

	ret = g_string_new ("Error while compiling `[");

	if (error->priv->object)
	{
		fullid = cdn_object_get_full_id (error->priv->object);
	}
	else
	{
		fullid = NULL;
	}

	g_string_append (ret, fullid);
	g_free (fullid);

	g_string_append_c (ret, ']');

	if (error->priv->property != NULL)
	{
		g_string_append_c (ret, '.');
		g_string_append (ret, cdn_variable_get_name (error->priv->property));
	}
	else if (error->priv->action != NULL)
	{
		g_string_append (ret, " < ");
		g_string_append (ret, cdn_edge_action_get_target (error->priv->action));
	}

	if (error->priv->expression)
	{
		expr = cdn_expression_get_as_string (error->priv->expression);

		start = cdn_expression_get_error_start (error->priv->expression);
		end = cdn_expression_get_error_at (error->priv->expression);

		start = g_utf8_pointer_to_offset (expr,
		                                  expr + start);

		end = g_utf8_pointer_to_offset (expr,
		                                expr + end);
	}
	else
	{
		expr = "";
		start = 0;
		end = 0;
	}

	if (start != end)
	{
		prefix = g_strdup_printf ("[%d-%d]: ", start, end);
	}
	else
	{
		prefix = g_strdup_printf ("[%d]: ", start);
	}

	pad = g_strnfill (strlen (prefix) + start - 1, ' ');

	if (end - start - 2 > 0)
	{
		gchar *tmp;
		tmp = g_strnfill (end - start - 2, '-');

		strip = g_strconcat (tmp, "^", NULL);
		g_free (tmp);
	}
	else
	{
		strip = g_strdup ("");
	}

	g_string_append_printf (ret,
	                        "':\n%s%s\n%s^%s %s",
	                        prefix,
	                        expr,
	                        pad,
	                        strip,
	                        cdn_compile_error_get_message (error));

	g_free (pad);
	g_free (prefix);
	g_free (strip);

	return g_string_free (ret, FALSE);
}
