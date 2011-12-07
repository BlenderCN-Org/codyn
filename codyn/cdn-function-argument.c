/*
 * cdn-function-argument.c
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

#include "cdn-function-argument.h"
#include "cdn-marshal.h"

/**
 * SECTION:cdn-function-argument
 * @short_description: An argument to a custom defined function
 *
 * A #CdnFunctionArgument contains information on an argument (or parameter)
 * to a custom defined function.
 *
 */

#define CDN_FUNCTION_ARGUMENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_FUNCTION_ARGUMENT, CdnFunctionArgumentPrivate))

/* signals */
enum
{
	INVALIDATE_NAME,
	NUM_SIGNALS
};

struct _CdnFunctionArgumentPrivate
{
	gchar *name;
	gboolean isexplicit;

	CdnVariable *property;
};

G_DEFINE_TYPE (CdnFunctionArgument, cdn_function_argument, G_TYPE_INITIALLY_UNOWNED)

enum
{
	PROP_0,
	PROP_NAME,
	PROP_EXPLICIT
};

static guint signals[NUM_SIGNALS] = {0,};

static void
cdn_function_argument_finalize (GObject *object)
{
	CdnFunctionArgument *argument = CDN_FUNCTION_ARGUMENT (object);

	g_free (argument->priv->name);

	G_OBJECT_CLASS (cdn_function_argument_parent_class)->finalize (object);
}

static gboolean
set_name (CdnFunctionArgument *argument,
          gchar const         *name)
{
	if (g_strcmp0 (name, argument->priv->name) == 0)
	{
		return TRUE;
	}

	gboolean invalid = FALSE;

	g_signal_emit (argument, signals[INVALIDATE_NAME], 0, name, &invalid);

	if (!invalid)
	{
		g_free (argument->priv->name);
		argument->priv->name = g_strdup (name);

		g_object_notify (G_OBJECT (argument), "name");
	}

	return !invalid;
}

static void
cdn_function_argument_set_variable (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
	CdnFunctionArgument *self = CDN_FUNCTION_ARGUMENT (object);

	switch (prop_id)
	{
		case PROP_NAME:
			set_name (self, g_value_get_string (value));
		break;
		case PROP_EXPLICIT:
			self->priv->isexplicit = g_value_get_boolean (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_function_argument_get_variable (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
	CdnFunctionArgument *self = CDN_FUNCTION_ARGUMENT (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_value_set_string (value, self->priv->name);
		break;
		case PROP_EXPLICIT:
			g_value_set_boolean (value, self->priv->isexplicit);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_function_argument_class_init (CdnFunctionArgumentClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_function_argument_finalize;

	object_class->get_property = cdn_function_argument_get_variable;
	object_class->set_property = cdn_function_argument_set_variable;

	g_type_class_add_private (object_class, sizeof(CdnFunctionArgumentPrivate));

	/**
	 * CdnFunctionArgument::invalidate-name:
	 * @object: a #CdnFunctionArgument
	 * @name: the new function argument name
	 *
	 * This signal is emitted to validate (or rather, invalidate) a new
	 * name for a function argument. When a signal handler returns %TRUE,
	 * the new name is rejected.
	 *
	 * Returns: %TRUE if the new name should be rejected, %FALSE otherwise
	 *
	 **/
	signals[INVALIDATE_NAME] =
		g_signal_new ("invalidate-name",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnFunctionArgumentClass, invalidate_name),
		              g_signal_accumulator_true_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__STRING,
		              G_TYPE_BOOLEAN,
		              1,
		              G_TYPE_STRING);

	/**
	 * CdnFunctionArgument:name:
	 *
	 * The function argument name
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_NAME,
	                                 g_param_spec_string ("name",
	                                                      "Name",
	                                                      "Name",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_EXPLICIT,
	                                 g_param_spec_boolean ("explicit",
	                                                       "Explicit",
	                                                       "Explicit",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cdn_function_argument_init (CdnFunctionArgument *self)
{
	self->priv = CDN_FUNCTION_ARGUMENT_GET_PRIVATE (self);
}

/**
 * cdn_function_argument_new:
 * @name: The function argument name
 * @isexplicit:
 *
 * Create a new #CdnFunctionArgument. Note that #CdnFunctionArgument derives
 * from #GInitiallyUnowned. This means that initially, the newly created object
 * has a floating reference and does not need to be freed if you add it to
 * a function using #cdn_function_add_argument.
 *
 * Returns: A #CdnFunctionArgument
 *
 **/
CdnFunctionArgument *
cdn_function_argument_new (gchar const   *name,
                           gboolean       isexplicit)
{
	return g_object_new (CDN_TYPE_FUNCTION_ARGUMENT,
	                     "name", name,
	                     "explicit", isexplicit,
	                     NULL);
}

/**
 * cdn_function_argument_copy:
 * @argument: A #CdnFunctionArgument
 *
 * Create a copy of a function argument. Note that #CdnFunctionArgument derives
 * from #GInitiallyUnowned. This means that initially, the newly created object
 * has a floating reference and does not need to be freed if you add it to
 * a function using #cdn_function_add_argument.
 *
 * Returns: (transfer full): A #CdnFunctionArgument
 *
 **/
CdnFunctionArgument *
cdn_function_argument_copy (CdnFunctionArgument *argument)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), NULL);

	return cdn_function_argument_new (argument->priv->name,
	                                  argument->priv->isexplicit);
}

/**
 * cdn_function_argument_get_name:
 * @argument: A #CdnFunctionArgument
 *
 * Get the function name.
 *
 * Returns: the function name
 *
 **/
gchar const *
cdn_function_argument_get_name (CdnFunctionArgument *argument)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), NULL);

	return argument->priv->name;
}

/**
 * cdn_function_argument_set_name:
 * @argument: A #CdnFunctionArgument
 * @name: The argument name
 * 
 * Set the function argument name.
 *
 * Returns: %TRUE if the function argument name could be successfully changed,
 *          %FALSE otherwise.
 *
 **/
gboolean
cdn_function_argument_set_name (CdnFunctionArgument *argument,
                                const gchar         *name)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return set_name (argument, name);
}

/**
 * cdn_function_argument_get_explicit:
 * @argument: A #CdnFunctionArgument
 *
 * Get whether the function argument is explicit.
 *
 * Returns: whether the argument is explicit
 *
 **/
gboolean
cdn_function_argument_get_explicit (CdnFunctionArgument *argument)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), TRUE);

	return argument->priv->isexplicit;
}

/**
 * cdn_function_argument_set_explicit:
 * @argument: A #CdnFunctionArgument
 * @isexplicit: Whether the argument is explicit
 *
 * Set whether a function argument is explicit.
 *
 **/
void
cdn_function_argument_set_explicit (CdnFunctionArgument *argument,
                                    gboolean             isexplicit)
{
	g_return_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument));

	if (argument->priv->isexplicit != isexplicit)
	{
		argument->priv->isexplicit = isexplicit;
		g_object_notify (G_OBJECT (argument), "explicit");
	}
}

void
_cdn_function_argument_set_variable (CdnFunctionArgument *argument,
                                     CdnVariable         *property)
{
	g_return_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument));
	g_return_if_fail (property == NULL || CDN_IS_VARIABLE (property));

	argument->priv->property = property;
}

/**
 * _cdn_function_argument_get_variable:
 * @argument: A #CdnFunctionArgument
 *
 * Get the function argument property.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
_cdn_function_argument_get_variable (CdnFunctionArgument *argument)
{
	g_return_val_if_fail (CDN_IS_FUNCTION_ARGUMENT (argument), NULL);

	return argument->priv->property;
}
