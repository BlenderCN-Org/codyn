/*
 * cpg-compile-error.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-compile-error.h"

#include <string.h>

/**
 * SECTION:cpg-compile-error
 * @short_description: Compile error message container
 *
 * Object used to store information on expression compile errors.
 *
 */

#define CPG_COMPILE_ERROR_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_COMPILE_ERROR, CpgCompileErrorPrivate))

struct _CpgCompileErrorPrivate
{
	GError *error;

	CpgObject *object;
	CpgProperty *property;
	CpgLinkAction *action;
	CpgExpression *expression;
};

G_DEFINE_TYPE (CpgCompileError, cpg_compile_error, G_TYPE_OBJECT)

GQuark
cpg_compile_error_type_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_compile_error_type");
	}

	return quark;
}

static void
clear_objects (CpgCompileError *error)
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
cpg_compile_error_dispose (GObject *object)
{
	CpgCompileError *error = CPG_COMPILE_ERROR (object);

	clear_objects (error);

	G_OBJECT_CLASS (cpg_compile_error_parent_class)->dispose (object);
}

static void
cpg_compile_error_finalize (GObject *object)
{
	G_OBJECT_CLASS (cpg_compile_error_parent_class)->finalize (object);
}

static void
cpg_compile_error_class_init (CpgCompileErrorClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->dispose = cpg_compile_error_dispose;
	object_class->finalize = cpg_compile_error_finalize;

	g_type_class_add_private (object_class, sizeof(CpgCompileErrorPrivate));
}

static void
cpg_compile_error_init (CpgCompileError *self)
{
	self->priv = CPG_COMPILE_ERROR_GET_PRIVATE (self);
}

/**
 * cpg_compile_error_new:
 *
 * Create new empty compile error
 *
 * Returns: a new #CpgCompileError
 *
 **/
CpgCompileError *
cpg_compile_error_new ()
{
	return g_object_new (CPG_TYPE_COMPILE_ERROR, NULL);
}

/**
 * cpg_compile_error_set:
 * @error: A #CpgCompileError
 * @gerror: A #GError
 * @object: A #CpgObject
 * @property: A #CpgProperty
 * @action: A #CpgLinkAction
 *
 * Set compile error information.
 *
 **/
void
cpg_compile_error_set (CpgCompileError *error,
                       GError          *gerror,
                       CpgObject       *object,
                       CpgProperty     *property,
                       CpgLinkAction   *action,
                       CpgExpression   *expression)
{
	g_return_if_fail (CPG_IS_COMPILE_ERROR (error));
	g_return_if_fail (object == NULL || CPG_IS_OBJECT (object));
	g_return_if_fail (property == NULL || CPG_IS_PROPERTY (property));
	g_return_if_fail (action == NULL || CPG_IS_LINK_ACTION (action));
	g_return_if_fail (expression == NULL || CPG_IS_EXPRESSION (expression));

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
			error->priv->expression = g_object_ref (cpg_property_get_expression (property));
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
			error->priv->expression = g_object_ref (cpg_link_action_get_equation (action));
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
 * cpg_compile_error_get_error:
 * @error: a #CpgCompileError
 *
 * Get the associated #GError
 *
 * Returns: (transfer none): the associated #GError
 *
 **/
GError *
cpg_compile_error_get_error (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	return error->priv->error;
}

/**
 * cpg_compile_error_get_object:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgObject
 *
 * Returns: (transfer none): the associated #CpgObject
 *
 **/
CpgObject *
cpg_compile_error_get_object (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	return error->priv->object;
}

/**
 * cpg_compile_error_get_property:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgProperty
 *
 * Returns: (transfer none): the associated #CpgProperty
 *
 **/
CpgProperty *
cpg_compile_error_get_property (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	return error->priv->property;
}

/**
 * cpg_compile_error_get_link_action:
 * @error: a #CpgCompileError
 *
 * Get the associated #CpgLinkAction
 *
 * Returns: (transfer none): the associated #CpgLinkAction
 *
 **/
CpgLinkAction *
cpg_compile_error_get_link_action (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	return error->priv->action;
}

/**
 * cpg_compile_error_code_string:
 * @code: the error code
 *
 * Get the string describing an error with error code @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cpg_compile_error_code_string (gint code)
{
	switch (code)
	{
		case CPG_COMPILE_ERROR_PROPERTY_NOT_FOUND:
			return "Property not found";
		break;
		case CPG_COMPILE_ERROR_FUNCTION_NOT_FOUND:
			return "Function not found";
		break;
		case CPG_COMPILE_ERROR_INVALID_TOKEN:
			return "Invalid token";
		break;
		case CPG_COMPILE_ERROR_MAXARG:
			return "Maximum number of arguments";
		break;
		case CPG_COMPILE_ERROR_INVALID_STACK:
			return "Invalid stack";
		break;
		case CPG_COMPILE_ERROR_PROPERTY_RECURSE:
			return "Property recusion";
		break;
		default:
			return "Unknown";
		break;
	}
}

/**
 * cpg_compile_error_string:
 * @error: a #CpgCompileError
 *
 * Get the string describing @error
 *
 * Returns: the error string message
 *
 **/
gchar const *
cpg_compile_error_string (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	if (cpg_compile_error_get_error (error)->domain ==
	    CPG_COMPILE_ERROR_TYPE)
	{
		return cpg_compile_error_code_string (cpg_compile_error_get_code (error));
	}
	else
	{
		return "Compile error";
	}
}

/**
 * cpg_compile_error_get_code:
 * @error: a #CpgCompileError
 *
 * Get the error code
 *
 * Returns: the error code
 *
 **/
gint
cpg_compile_error_get_code (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), 0);

	return error->priv->error ? error->priv->error->code : 0;
}

/**
 * cpg_compile_error_get_message:
 * @error: a #CpgCompileError
 *
 * Get the error message
 *
 * Returns: the error message
 *
 **/
gchar const *
cpg_compile_error_get_message (CpgCompileError *error)
{
	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	return error->priv->error ? error->priv->error->message : "Unknown";
}

gchar *
cpg_compile_error_get_formatted_string (CpgCompileError *error)
{
	GString *ret;
	gchar *fullid;
	gchar const *expr;
	gchar *pad;
	gchar *prefix;
	gint start;
	gint end;
	gchar *strip = NULL;

	g_return_val_if_fail (CPG_IS_COMPILE_ERROR (error), NULL);

	ret = g_string_new ("Error while compiling `[");

	if (error->priv->object)
	{
		fullid = cpg_object_get_full_id (error->priv->object);
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
		g_string_append (ret, cpg_property_get_name (error->priv->property));
	}
	else if (error->priv->action != NULL)
	{
		g_string_append (ret, " < ");
		g_string_append (ret, cpg_link_action_get_target (error->priv->action));
	}

	if (error->priv->expression)
	{
		expr = cpg_expression_get_as_string (error->priv->expression);

		start = cpg_expression_get_error_start (error->priv->expression);
		end = cpg_expression_get_error_at (error->priv->expression);
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
	                        "' (%s):\n%s%s\n%s^%s %s",
	                        cpg_compile_error_string (error),
	                        prefix,
	                        expr,
	                        pad,
	                        strip,
	                        cpg_compile_error_get_message (error));

	g_free (pad);
	g_free (prefix);
	g_free (strip);

	return g_string_free (ret, FALSE);
}
