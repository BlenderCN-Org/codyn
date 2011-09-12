/*
 * cpg-function-argument.c
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

#include "cpg-function-argument.h"
#include "cpg-marshal.h"

/**
 * SECTION:cpg-function-argument
 * @short_description: An argument to a custom defined function
 *
 * A #CpgFunctionArgument contains information on an argument (or parameter)
 * to a custom defined function.
 *
 */

#define CPG_FUNCTION_ARGUMENT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_FUNCTION_ARGUMENT, CpgFunctionArgumentPrivate))

/* signals */
enum
{
	INVALIDATE_NAME,
	NUM_SIGNALS
};

struct _CpgFunctionArgumentPrivate
{
	gchar *name;
	CpgExpression *def;
	gboolean isexplicit;

	CpgProperty *property;
};

G_DEFINE_TYPE (CpgFunctionArgument, cpg_function_argument, G_TYPE_INITIALLY_UNOWNED)

enum
{
	PROP_0,
	PROP_NAME,
	PROP_OPTIONAL,
	PROP_DEFAULT_VALUE,
	PROP_EXPLICIT
};

static guint signals[NUM_SIGNALS] = {0,};

static void
cpg_function_argument_finalize (GObject *object)
{
	CpgFunctionArgument *argument = CPG_FUNCTION_ARGUMENT (object);

	g_free (argument->priv->name);

	if (argument->priv->def)
	{
		g_object_unref (argument->priv->def);
	}

	G_OBJECT_CLASS (cpg_function_argument_parent_class)->finalize (object);
}

static gboolean
set_name (CpgFunctionArgument *argument,
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
set_default_value (CpgFunctionArgument *self,
                   CpgExpression       *def)
{
	gboolean isoptional;

	isoptional = self->priv->def != NULL;

	if (self->priv->def == def)
	{
		return;
	}

	if (self->priv->def)
	{
		g_object_unref (self->priv->def);
		self->priv->def = NULL;
	}

	if (def)
	{
		self->priv->def = g_object_ref_sink (def);
	}

	g_object_notify (G_OBJECT (self), "default-value");

	if (isoptional != (self->priv->def != NULL))
	{
		g_object_notify (G_OBJECT (self), "optional");
	}
}

static void
cpg_function_argument_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
	CpgFunctionArgument *self = CPG_FUNCTION_ARGUMENT (object);

	switch (prop_id)
	{
		case PROP_NAME:
			set_name (self, g_value_get_string (value));
		break;
		case PROP_DEFAULT_VALUE:
			set_default_value (self, g_value_get_object (value));
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
cpg_function_argument_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
	CpgFunctionArgument *self = CPG_FUNCTION_ARGUMENT (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_value_set_string (value, self->priv->name);
		break;
		case PROP_OPTIONAL:
			g_value_set_boolean (value, self->priv->def != NULL);
		break;
		case PROP_DEFAULT_VALUE:
			g_value_set_object (value, self->priv->def);
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
cpg_function_argument_class_init (CpgFunctionArgumentClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_function_argument_finalize;

	object_class->get_property = cpg_function_argument_get_property;
	object_class->set_property = cpg_function_argument_set_property;

	g_type_class_add_private (object_class, sizeof(CpgFunctionArgumentPrivate));

	/**
	 * CpgFunctionArgument::invalidate-name:
	 * @object: a #CpgFunctionArgument
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
		              G_STRUCT_OFFSET (CpgFunctionArgumentClass, invalidate_name),
		              g_signal_accumulator_true_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__STRING,
		              G_TYPE_BOOLEAN,
		              1,
		              G_TYPE_STRING);

	/**
	 * CpgFunctionArgument:name:
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

	/**
	 * CpgFunctionArgument:optional:
	 *
	 * Whether or not the function argument is optional
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OPTIONAL,
	                                 g_param_spec_boolean ("optional",
	                                                       "Optional",
	                                                       "Optional",
	                                                       FALSE,
	                                                       G_PARAM_READABLE));

	/**
	 * CpgFunctionArgument:default:
	 *
	 * The default value of an optional function argument
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_DEFAULT_VALUE,
	                                 g_param_spec_object ("default-value",
	                                                      "Default value",
	                                                      "Default Value",
	                                                      CPG_TYPE_EXPRESSION,
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
cpg_function_argument_init (CpgFunctionArgument *self)
{
	self->priv = CPG_FUNCTION_ARGUMENT_GET_PRIVATE (self);
}

/**
 * cpg_function_argument_new:
 * @name: The function argument name
 * @optional: Whether the function argument is optional
 * @def: The default value of an optional function argument
 *
 * Create a new #CpgFunctionArgument. Note that #CpgFunctionArgument derives
 * from #GInitiallyUnowned. This means that initially, the newly created object
 * has a floating reference and does not need to be freed if you add it to
 * a function using #cpg_function_add_argument.
 *
 * Returns: A #CpgFunctionArgument
 *
 **/
CpgFunctionArgument *
cpg_function_argument_new (gchar const   *name,
                           CpgExpression *def,
                           gboolean       isexplicit)
{
	return g_object_new (CPG_TYPE_FUNCTION_ARGUMENT,
	                     "name", name,
	                     "default-value", def,
	                     "explicit", isexplicit,
	                     NULL);
}

/**
 * cpg_function_argument_copy:
 * @argument: A #CpgFunctionArgument
 *
 * Create a copy of a function argument. Note that #CpgFunctionArgument derives
 * from #GInitiallyUnowned. This means that initially, the newly created object
 * has a floating reference and does not need to be freed if you add it to
 * a function using #cpg_function_add_argument.
 *
 * Returns: (transfer full): A #CpgFunctionArgument
 *
 **/
CpgFunctionArgument *
cpg_function_argument_copy (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), NULL);

	return cpg_function_argument_new (argument->priv->name,
	                                  cpg_expression_copy (argument->priv->def),
	                                  argument->priv->isexplicit);
}

/**
 * cpg_function_argument_get_name:
 * @argument: A #CpgFunctionArgument
 *
 * Get the function name.
 *
 * Returns: the function name
 *
 **/
gchar const *
cpg_function_argument_get_name (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), NULL);

	return argument->priv->name;
}

/**
 * cpg_function_argument_set_name:
 * @argument: A #CpgFunctionArgument
 * @name: The argument name
 * 
 * Set the function argument name.
 *
 * Returns: %TRUE if the function argument name could be successfully changed,
 *          %FALSE otherwise.
 *
 **/
gboolean
cpg_function_argument_set_name (CpgFunctionArgument *argument,
                                const gchar         *name)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return set_name (argument, name);
}

/**
 * cpg_function_argument_get_optional:
 * @argument: A #CpgFunctionArgument
 *
 * Get whether the function argument is optional. If the argument is optional
 * its default value can be obtained with
 * #cpg_function_argument_get_default_value
 *
 * Returns: whether the argument is optional
 *
 **/
gboolean
cpg_function_argument_get_optional (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), FALSE);

	return argument->priv->def != NULL;
}

/**
 * cpg_function_argument_get_default_value:
 * @argument: A #CpgFunctionArgument
 *
 * Get the function argument default value.
 *
 * Returns: (transfer none): the default value
 *
 **/
CpgExpression *
cpg_function_argument_get_default_value (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), 0);

	return argument->priv->def;
}

/**
 * cpg_function_argument_set_default_value:
 * @argument: A #CpgFunctionArgument
 * @def: The default argument value
 *
 * Set the default argument value for an optional function argument.
 *
 **/
void
cpg_function_argument_set_default_value (CpgFunctionArgument *argument,
                                         CpgExpression       *def)
{
	g_return_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument));
	g_return_if_fail (def == NULL || CPG_IS_EXPRESSION (def));

	set_default_value (argument, def);
}

/**
 * cpg_function_argument_get_explicit:
 * @argument: A #CpgFunctionArgument
 *
 * Get whether the function argument is explicit.
 *
 * Returns: whether the argument is explicit
 *
 **/
gboolean
cpg_function_argument_get_explicit (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), TRUE);

	return argument->priv->isexplicit;
}

/**
 * cpg_function_argument_set_explicit:
 * @argument: A #CpgFunctionArgument
 * @isexplicit: Whether the argument is explicit
 *
 * Set whether a function argument is explicit.
 *
 **/
void
cpg_function_argument_set_explicit (CpgFunctionArgument *argument,
                                    gboolean             isexplicit)
{
	g_return_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument));

	if (argument->priv->isexplicit != isexplicit)
	{
		argument->priv->isexplicit = isexplicit;
		g_object_notify (G_OBJECT (argument), "explicit");
	}
}

void
_cpg_function_argument_set_property (CpgFunctionArgument *argument,
                                     CpgProperty         *property)
{
	g_return_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument));
	g_return_if_fail (property == NULL || CPG_IS_PROPERTY (property));

	argument->priv->property = property;
}

/**
 * _cpg_function_argument_get_property:
 * @argument: A #CpgFunctionArgument
 *
 * Get the function argument property.
 *
 * Returns: (transfer none): A #CpgProperty
 *
 **/
CpgProperty *
_cpg_function_argument_get_property (CpgFunctionArgument *argument)
{
	g_return_val_if_fail (CPG_IS_FUNCTION_ARGUMENT (argument), NULL);

	return argument->priv->property;
}
