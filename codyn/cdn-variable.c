/*
 * cdn-variable.c
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

#include <string.h>

#include "cdn-variable.h"
#include "cdn-expression.h"
#include "cdn-utils.h"
#include "cdn-enum-types.h"
#include "cdn-object.h"
#include "cdn-marshal.h"
#include "cdn-usable.h"
#include "cdn-modifiable.h"
#include "cdn-annotatable.h"
#include "cdn-selector.h"
#include "cdn-node.h"
#include "cdn-debug.h"

#define CDN_VARIABLE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_VARIABLE, CdnVariablePrivate))

/* signals */
enum
{
	INVALIDATE_NAME,
	EXPRESSION_CHANGED,
	FLAGS_CHANGED,
	NUM_SIGNALS
};

struct _CdnVariablePrivate
{
	guint use_count;
	gchar *name;

	CdnExpression *expression;
	CdnExpression *constraint;
	CdnVariableFlags flags;

	CdnMatrix update;
	CdnObject *object;

	CdnVariable *diff_of;
	CdnVariable *diff_for;

	gchar *annotation;
	guint expression_proxy_id;

	gdouble last_value;
	guint modified : 1;
	guint disposing : 1;
	guint in_constraint : 1;
};

static void cdn_usable_iface_init (gpointer iface);
static void cdn_modifiable_iface_init (gpointer iface);
static void cdn_annotatable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CdnVariable,
                         cdn_variable,
                         G_TYPE_INITIALLY_UNOWNED,
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_USABLE,
                                                cdn_usable_iface_init);
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_MODIFIABLE,
                                                cdn_modifiable_iface_init);
                         G_IMPLEMENT_INTERFACE (CDN_TYPE_ANNOTATABLE,
                                                cdn_annotatable_iface_init));

static guint signals[NUM_SIGNALS] = {0,};

enum
{
	PROP_0,
	PROP_NAME,
	PROP_OBJECT,
	PROP_FLAGS,
	PROP_EXPRESSION,
	PROP_CONSTRAINT,
	PROP_USE_COUNT,
	PROP_MODIFIED,
	PROP_ANNOTATION
};

/**
 * SECTION:cdn-variable
 * @short_description: Property container
 *
 * A #CdnVariable is a container for a specific variable in an object. It
 * consists of a name and a mathematical expression describing its contents.
 *
 */

static void
cdn_variable_use (CdnUsable *usable)
{
	CdnVariable *prop = CDN_VARIABLE (usable);
	++prop->priv->use_count;
}

static gboolean
cdn_variable_unuse (CdnUsable *usable)
{
	CdnVariable *prop = CDN_VARIABLE (usable);

	if (prop->priv->use_count == 0)
	{
		return TRUE;
	}

	return (--(prop->priv->use_count) == 0);
}

static gchar *
cdn_variable_annotatable_get_title (CdnAnnotatable *annotatable)
{
	CdnVariable *variable;

	variable = CDN_VARIABLE (annotatable);

	return cdn_variable_get_full_name_for_display (variable);
}

static void
cdn_annotatable_iface_init (gpointer iface)
{
	CdnAnnotatableInterface *annotatable = iface;

	annotatable->get_title = cdn_variable_annotatable_get_title;
}

static void
cdn_usable_iface_init (gpointer iface)
{
	CdnUsableInterface *usable = iface;

	usable->use = cdn_variable_use;
	usable->unuse = cdn_variable_unuse;
}

static void
cdn_modifiable_iface_init (gpointer iface)
{
	/* Use default implementation */
}

static void
set_object (CdnVariable *variable,
            CdnObject   *object,
            gboolean     notify)
{
	if (variable->priv->object == object)
	{
		return;
	}

	if (variable->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (variable->priv->object),
		                              (gpointer *)&variable->priv->object);
	}

	variable->priv->object = object;

	if (variable->priv->object)
	{
		g_object_add_weak_pointer (G_OBJECT (variable->priv->object),
		                           (gpointer *)&variable->priv->object);
	}

	if (!variable->priv->disposing && notify)
	{
		g_object_notify (G_OBJECT (variable), "object");
	}
}

static gboolean
set_constraint (CdnVariable   *variable,
                CdnExpression *expression)
{
	if (variable->priv->constraint == expression ||
	    (expression && variable->priv->constraint &&
	     cdn_expression_equal (variable->priv->constraint,
	                           expression,
	                           TRUE)))
	{
		if (expression && g_object_is_floating (expression))
		{
			g_object_unref (expression);
		}

		return FALSE;
	}

	if (variable->priv->constraint)
	{
		g_object_unref (variable->priv->constraint);
		variable->priv->constraint = NULL;
	}

	if (expression)
	{
		variable->priv->constraint = g_object_ref_sink (expression);
	}

	if (!variable->priv->disposing &&
	    !variable->priv->modified)
	{
		variable->priv->modified = TRUE;
		g_object_notify (G_OBJECT (variable), "modified");
	}

	return TRUE;
}

static void
on_expression_changed (CdnVariable *variable)
{
	g_object_notify (G_OBJECT (variable), "expression");
	cdn_modifiable_set_modified (CDN_MODIFIABLE (variable), TRUE);
}

static gboolean
set_expression (CdnVariable   *variable,
                CdnExpression *expression,
                gboolean       notify)
{
	if (variable->priv->expression == expression ||
	    (expression && variable->priv->expression &&
	     cdn_expression_equal (variable->priv->expression,
	                           expression,
	                           TRUE)))
	{
		if (expression && g_object_is_floating (expression))
		{
			g_object_unref (expression);
		}

		if (notify)
		{
			g_object_notify (G_OBJECT (variable), "modified");
		}

		return FALSE;
	}

	if (variable->priv->expression)
	{
		_cdn_expression_transfer_dependencies (variable->priv->expression,
		                                       expression);

		g_signal_handler_disconnect (variable->priv->expression,
		                             variable->priv->expression_proxy_id);

		g_object_unref (variable->priv->expression);
		variable->priv->expression = NULL;
	}

	if (expression)
	{
		variable->priv->expression = g_object_ref_sink (expression);

		variable->priv->expression_proxy_id =
			g_signal_connect_swapped (variable->priv->expression,
			                          "notify::expression",
			                          G_CALLBACK (on_expression_changed),
			                          variable);
	}

	if (!variable->priv->disposing)
	{
		variable->priv->modified = TRUE;

		if (notify)
		{
			g_object_notify (G_OBJECT (variable), "modified");
		}
	}

	return TRUE;
}

static void
cdn_variable_finalize (GObject *object)
{
	CdnVariable *variable;

	variable = CDN_VARIABLE (object);

	g_free (variable->priv->name);
	g_free (variable->priv->annotation);
	cdn_matrix_destroy (&variable->priv->update);

	G_OBJECT_CLASS (cdn_variable_parent_class)->finalize (object);
}

static void
set_diff_of (CdnVariable *variable,
             CdnVariable *diff_of)
{
	if (variable == diff_of ||
	    variable->priv->diff_of == diff_of)
	{
		return;
	}

	if (variable->priv->diff_of)
	{
		if (variable->priv->diff_of->priv->diff_for == variable)
		{
			variable->priv->diff_of->priv->diff_for = NULL;
		}

		variable->priv->diff_of = NULL;
	}

	if (diff_of)
	{
		variable->priv->diff_of = diff_of;
		diff_of->priv->diff_for = variable;
	}
}

static void
cdn_variable_dispose (GObject *object)
{
	CdnVariable *variable = CDN_VARIABLE (object);

	variable->priv->disposing = TRUE;

	set_expression (variable, NULL, FALSE);
	set_constraint (variable, NULL);
	set_object (variable, NULL, TRUE);
	set_diff_of (variable, NULL);

	G_OBJECT_CLASS (cdn_variable_parent_class)->dispose (object);
}

static void
set_flags (CdnVariable      *variable,
           CdnVariableFlags  flags,
           gboolean          notify)
{
	if (flags != variable->priv->flags)
	{
		gboolean wasonce = variable->priv->flags & (CDN_VARIABLE_FLAG_ONCE | CDN_VARIABLE_FLAG_IN);
		variable->priv->flags = flags;

		if (flags & (CDN_VARIABLE_FLAG_ONCE | CDN_VARIABLE_FLAG_IN))
		{
			if (!wasonce)
			{
				cdn_expression_set_once (variable->priv->expression,
				                         TRUE);
			}
		}
		else if (wasonce)
		{
			cdn_expression_set_once (variable->priv->expression,
			                         FALSE);
		}

		if (notify)
		{
			g_object_notify (G_OBJECT (variable), "flags");
		}

		variable->priv->modified = TRUE;

		if (notify)
		{
			g_object_notify (G_OBJECT (variable), "modified");
		}
	}
}

static gboolean
set_name (CdnVariable *variable,
          gchar const *name)
{
	if (g_strcmp0 (variable->priv->name, name) == 0)
	{
		return TRUE;
	}

	gboolean invalid = FALSE;

	g_signal_emit (variable, signals[INVALIDATE_NAME], 0, name, &invalid);

	if (!invalid)
	{
		g_free (variable->priv->name);
		variable->priv->name = g_strdup (name);

		g_object_notify (G_OBJECT (variable), "name");
	}

	return !invalid;
}

static void
cdn_variable_set_property (GObject      *object,
                           guint         prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
	CdnVariable *self = CDN_VARIABLE (object);

	switch (prop_id)
	{
		case PROP_NAME:
			set_name (self, g_value_get_string (value));
		break;
		case PROP_OBJECT:
			set_object (self, g_value_get_object (value), TRUE);
		break;
		case PROP_FLAGS:
			set_flags (self, g_value_get_flags (value), TRUE);
		break;
		case PROP_EXPRESSION:
			set_expression (self, g_value_get_object (value), TRUE);
		break;
		case PROP_CONSTRAINT:
			set_constraint (self, g_value_get_object (value));
		break;
		case PROP_MODIFIED:
			self->priv->modified = g_value_get_boolean (value);
		break;
		case PROP_ANNOTATION:
			g_free (self->priv->annotation);
			self->priv->annotation = g_value_dup_string (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_variable_get_property (GObject    *object,
                           guint       prop_id,
                           GValue     *value,
                           GParamSpec *pspec)
{
	CdnVariable *self = CDN_VARIABLE (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_value_set_string (value, self->priv->name);
		break;
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_FLAGS:
			g_value_set_flags (value, self->priv->flags);
		break;
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
		break;
		case PROP_CONSTRAINT:
			g_value_set_object (value, self->priv->constraint);
		break;
		case PROP_USE_COUNT:
			g_value_set_uint (value, self->priv->use_count);
		break;
		case PROP_MODIFIED:
			g_value_set_boolean (value, self->priv->modified);
		break;
		case PROP_ANNOTATION:
			g_value_set_string (value, self->priv->annotation);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_variable_class_init (CdnVariableClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_variable_finalize;
	object_class->dispose = cdn_variable_dispose;

	object_class->get_property = cdn_variable_get_property;
	object_class->set_property = cdn_variable_set_property;

	g_type_class_add_private (object_class, sizeof(CdnVariablePrivate));

	/**
	 * CdnVariable:name:
	 *
	 * The variable name
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_NAME,
	                                 g_param_spec_string ("name",
	                                                      "Name",
	                                                      "Name",
	                                                      NULL,
	                                                      G_PARAM_READWRITE));

	/**
	 * CdnVariable:object:
	 *
	 * The object on which the variable is defined
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CDN_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE));

	/**
	 * CdnVariable:flags:
	 *
	 * The variable flags
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FLAGS,
	                                 g_param_spec_flags ("flags",
	                                                     "Flags",
	                                                     "Flags",
	                                                     CDN_TYPE_VARIABLE_FLAGS,
	                                                     CDN_VARIABLE_FLAG_NONE,
	                                                     G_PARAM_READWRITE));

	/**
	 * CdnVariable:expression:
	 *
	 * The variable expression
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE));

	/**
	 * CdnVariable:constraint:
	 *
	 * The variable constraint expression
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_CONSTRAINT,
	                                 g_param_spec_object ("constraint",
	                                                      "Constraint",
	                                                      "Constraint",
	                                                      CDN_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE));

	g_object_class_override_property (object_class,
	                                  PROP_USE_COUNT,
	                                  "use-count");

	/**
	 * CdnVariable::invalidate-name:
	 * @variable: a #CdnVariable
	 * @name: the new variable name
	 *
	 * This signal is emitted to validate (or rather, invalidate) a new
	 * name for a variable. When a signal handler returns %TRUE,
	 * the new name is rejected.
	 *
	 * Returns: %TRUE if the new name should be rejected, %FALSE otherwise
	 *
	 **/
	signals[INVALIDATE_NAME] =
		g_signal_new ("invalidate-name",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableClass, invalidate_name),
		              g_signal_accumulator_true_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__STRING,
		              G_TYPE_BOOLEAN,
		              1,
		              G_TYPE_STRING);

	signals[EXPRESSION_CHANGED] =
		g_signal_new ("expression-changed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableClass, expression_changed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_EXPRESSION);

	/**
	 * CdnVariable::flags-changed:
	 * @flags: a #CdnVariableFlags
	 *
	 * The ::flags-changed signal is emitted whenever the variable flags
	 * have changed.
	 */
	signals[FLAGS_CHANGED] =
		g_signal_new ("flags-changed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnVariableClass, flags_changed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__FLAGS,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_VARIABLE_FLAGS);

	g_object_class_override_property (object_class,
	                                  PROP_MODIFIED,
	                                  "modified");

	g_object_class_override_property (object_class,
	                                  PROP_ANNOTATION,
	                                  "annotation");
}

static void
cdn_variable_init (CdnVariable *self)
{
	self->priv = CDN_VARIABLE_GET_PRIVATE (self);

	self->priv->update.dimension.rows = 1;
	self->priv->update.dimension.columns = 1;

	self->priv->modified = FALSE;
}

/**
 * cdn_variable_new:
 * @name: the variable name
 * @expression: the value expression
 * @flags: the variable flags
 *
 * Create a new variable object. Property objects are assigned to #CdnObject
 * objects and are of little use on their own. The provided expression will
 * not be parsed initially.
 *
 * Returns: the new #CdnVariable
 *
 **/
CdnVariable *
cdn_variable_new (gchar const      *name,
                  CdnExpression    *expression,
                  CdnVariableFlags  flags)
{
	CdnVariable *ret = (CdnVariable *)g_object_new (CDN_TYPE_VARIABLE, NULL);

	ret->priv->name = g_strdup (name);

	set_expression (ret, expression, FALSE);
	set_flags (ret, flags, FALSE);

	ret->priv->modified = FALSE;

	return ret;
}

/**
 * cdn_variable_get_object:
 * @variable: the #CdnVariable
 *
 * Get the object associated with the variable
 *
 * Returns: (type CdnObject) (transfer none): the object associated with the variable
 **/
CdnObject *
cdn_variable_get_object (CdnVariable *variable)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	return variable->priv->object;
}

/**
 * cdn_variable_set_value:
 * @variable: the #CdnVariable
 * @value: the new value
 *
 * Change the value to a specific number.
 *
 **/
void
cdn_variable_set_value (CdnVariable  *variable,
                        gdouble       value)
{
	/* Omit type check to increase speed */
	cdn_expression_set_value (variable->priv->expression, value);
}

/**
 * cdn_variable_set_values: (skip)
 * @variable: the #CdnVariable
 * @values: the new value
 *
 * Change the value to a specific number.
 *
 **/
void
cdn_variable_set_values (CdnVariable     *variable,
                         CdnMatrix const *values)
{
	/* Omit type check to increase speed */
	cdn_expression_set_values (variable->priv->expression, values);
}

/**
 * cdn_variable_get_value:
 * @variable: the #CdnVariable
 *
 * Get the numerical value of the current value of the variable
 *
 * Return value: the numerical value of the variable's current value
 *
 **/
gdouble
cdn_variable_get_value (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	if (variable->priv->expression)
	{
		gdouble ret;

		if (variable->priv->constraint && !variable->priv->in_constraint)
		{
			// Apply the constraint
			variable->priv->in_constraint = TRUE;

			ret = cdn_expression_evaluate (variable->priv->constraint);

			variable->priv->in_constraint = FALSE;
		}
		else
		{
			ret = cdn_expression_evaluate (variable->priv->expression);
		}

		return ret;
	}
	else
	{
		return 0;
	}
}

/**
 * cdn_variable_get_values:
 * @variable: a #CdnVariable.
 *
 * Get the value of the variable.
 *
 * Returns: the value of the variable.
 *
 **/
CdnMatrix const *
cdn_variable_get_values (CdnVariable *variable)
{
	CdnMatrix const *ret = NULL;

	if (variable->priv->expression)
	{
		if (variable->priv->constraint && !variable->priv->in_constraint)
		{
			variable->priv->in_constraint = TRUE;

			ret = cdn_expression_evaluate_values (variable->priv->constraint);

			variable->priv->in_constraint = FALSE;
		}
		else
		{
			if (cdn_debug_is_enabled (CDN_DEBUG_MATH) &&
			    !cdn_expression_is_cached (variable->priv->expression))
			{
				gchar *s;

				s = cdn_variable_get_full_name_for_display (variable);
				cdn_debug_message (DEBUG_MATH, "Evaluate v: %s", s);
				g_free (s);
			}

			ret = cdn_expression_evaluate_values (variable->priv->expression);
		}
	}

	return ret;
}

/**
 * cdn_variable_get_dimension:
 * @variable: a #CdnVariable.
 * @dim: (out): the number of dimensions.
 *
 * Get the dimension of the variable value.
 *
 **/
void
cdn_variable_get_dimension (CdnVariable  *variable,
                            CdnDimension *dim)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));

	if (variable->priv->expression)
	{
		cdn_expression_get_dimension (variable->priv->expression,
		                              dim);
	}
	else
	{
		if (dim)
		{
			dim->rows = 0;
			dim->columns = 0;
		}
	}
}

/**
 * cdn_variable_get_expression:
 * @variable: a #CdnVariable
 *
 * Get the variable value expression
 *
 * Returns: (transfer none): a #CdnExpression. The expression is owned by the
 *                          variable and should not be freed
 *
 **/
CdnExpression *
cdn_variable_get_expression (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	return variable->priv->expression;
}

/**
 * cdn_variable_set_expression:
 * @variable: a #CdnVariable
 * @expression: the expression
 *
 * Set the variable value from an expression.
 *
 **/
void
cdn_variable_set_expression (CdnVariable   *variable,
                             CdnExpression *expression)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));
	g_return_if_fail (CDN_IS_EXPRESSION (expression));

	if (set_expression (variable, expression, TRUE))
	{
		g_object_notify (G_OBJECT (variable), "expression");
	}
}

/**
 * cdn_variable_get_constraint:
 * @variable: a #CdnVariable
 *
 * Get the variable value constraint expression
 *
 * Returns: (transfer none): a #CdnExpression. The expression is owned by the
 *                          variable and should not be freed
 *
 **/
CdnExpression *
cdn_variable_get_constraint (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	return variable->priv->constraint;
}

/**
 * cdn_variable_set_constraint:
 * @variable: a #CdnVariable
 * @expression: the constraint expression
 *
 * Set the variable constraint from an expression.
 *
 **/
void
cdn_variable_set_constraint (CdnVariable   *variable,
                             CdnExpression *expression)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));
	g_return_if_fail (expression == NULL || CDN_IS_EXPRESSION (expression));

	if (set_constraint (variable, expression))
	{
		g_object_notify (G_OBJECT (variable), "constraint");
	}

}

/**
 * cdn_variable_get_name:
 * @variable: a #CdnVariable
 *
 * Get the variable name
 *
 * Returns: (transfer none): the variable name
 *
 **/
gchar const *
cdn_variable_get_name (CdnVariable *variable)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	return variable->priv->name;
}

/**
 * cdn_variable_set_name:
 * @variable: A #CdnVariable
 * @name: The new variable name
 *
 * Set a new name for a variable.
 *
 * Returns: %TRUE if the name could be successfully changed, %FALSE otherwise
 *
 **/
gboolean
cdn_variable_set_name (CdnVariable *variable,
                       gchar const *name)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return set_name (variable, name);
}

/**
 * cdn_variable_get_integrated:
 * @variable: a #CdnVariable
 *
 * Get whether the variable should be integrated during evaluation or not. This
 * is a convenience function that simply checks if the
 * CDN_VARIABLE_FLAG_INTEGRATED flag is set.
 *
 * Returns: %TRUE if the variable will be integrated, %FALSE otherwise
 *
 **/
gboolean
cdn_variable_get_integrated (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	return variable->priv->flags & CDN_VARIABLE_FLAG_INTEGRATED;
}

/**
 * cdn_variable_set_integrated:
 * @variable: a #CdnVariable
 * @integrated: integrate the variable
 *
 * Set whether the variable should be integrated during evaluation or not. This
 * is a convenience function that simply sets or unsets the
 * CDN_VARIABLE_FLAG_INTEGRATED flag.
 *
 **/
void
cdn_variable_set_integrated (CdnVariable  *variable,
                             gboolean      integrated)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));

	if (integrated)
	{
		cdn_variable_add_flags (variable, CDN_VARIABLE_FLAG_INTEGRATED);
	}
	else
	{
		cdn_variable_remove_flags (variable, CDN_VARIABLE_FLAG_INTEGRATED);
	}
}

/**
 * cdn_variable_reset:
 * @variable: A #CdnVariable
 *
 * Reset the variable. This will reset the value of the variable to the
 * stored string representation.
 *
 **/
void
cdn_variable_reset (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	cdn_expression_set_once (variable->priv->expression,
	                         (variable->priv->flags & CDN_VARIABLE_FLAG_ONCE) != 0);
}

/**
 * cdn_variable_equal:
 * @variable: a #CdnVariable
 * @other: a #CdnVariable
 *
 * Compare two properties for equal values/expressions
 *
 * Returns: %TRUE if the properties are equal, %FALSE otherwise
 *
 **/
gboolean
cdn_variable_equal (CdnVariable *variable,
                    CdnVariable *other,
                    gboolean     asstring)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), FALSE);
	g_return_val_if_fail (CDN_IS_VARIABLE (other), FALSE);

	return variable->priv->flags == other->priv->flags &&
	       cdn_expression_equal (cdn_variable_get_expression (variable),
	                             cdn_variable_get_expression (other),
	                             asstring);
}

/**
 * cdn_variable_get_flags:
 * @variable: A #CdnVariable
 * 
 * The variable flags. The flags contains information on the type of variable (
 * such as in, or out).
 *
 * Returns: A #CdnVariableFlags
 *
 **/
CdnVariableFlags
cdn_variable_get_flags (CdnVariable *variable)
{
	/* Omit type check to increase speed */
	return variable->priv->flags;
}

/**
 * cdn_variable_set_flags:
 * @variable: A #CdnVariable
 * @flags: A #CdnVariableFlags
 * 
 * Set the variable flags.
 *
 **/
void
cdn_variable_set_flags (CdnVariable      *variable,
                        CdnVariableFlags  flags)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));

	set_flags (variable, flags, TRUE);
}

/**
 * cdn_variable_add_flags:
 * @variable: A #CdnVariable
 * @flags: A #CdnVariableFlags
 * 
 * Add a flags flag to the variable flagss.
 *
 **/
void
cdn_variable_add_flags (CdnVariable      *variable,
                        CdnVariableFlags  flags)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));

	set_flags (variable, variable->priv->flags | flags, TRUE);
}

/**
 * cdn_variable_remove_flags:
 * @variable: A #CdnVariable
 * @flags: A #CdnVariableFlags
 * 
 * Remove a flags flag from the variable flagss.
 *
 **/
void
cdn_variable_remove_flags (CdnVariable      *variable,
                           CdnVariableFlags  flags)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));

	set_flags (variable, variable->priv->flags & ~flags, TRUE);
}

void
cdn_variable_clear_update (CdnVariable *variable)
{
	cdn_matrix_clear (&variable->priv->update);
}

/**
 * cdn_variable_get_update:
 * @variable: A #CdnVariable
 *
 * Get the update value of a variable. The update value is used to store the
 * result of differential equations on the variable. You normally do not need
 * to use this function.
 *
 * Returns: (transfer none): The update value
 *
 **/
CdnMatrix *
cdn_variable_get_update (CdnVariable  *variable)
{
	/* Omit type check to increase speed */
	return &variable->priv->update;
}

/**
 * cdn_variable_flags_to_string:
 * @add_flags: Flags to add #CdnVariableFlags
 * @remove_flags: Flags to remove
 *
 * Convert flags to a string representation.
 *
 * Returns: the string representation of the flags
 *
 **/
gchar *
cdn_variable_flags_to_string (CdnVariableFlags add_flags,
                              CdnVariableFlags remove_flags)
{
	GFlagsClass *klass;
	gint i;
	CdnVariableFlags add_building;
	CdnVariableFlags remove_building;
	GPtrArray *attrs;
	GSList *items = NULL;
	GSList *item;

	klass = g_type_class_ref (CDN_TYPE_VARIABLE_FLAGS);
	attrs = g_ptr_array_new ();

	add_building = CDN_VARIABLE_FLAG_NONE;
	remove_building = CDN_VARIABLE_FLAG_NONE;

	for (i = klass->n_values - 1; i >= 0; --i)
	{
		GFlagsValue *value = &(klass->values[i]);
		guint v = value->value;

		if ((add_flags & v) == v && (add_building & v) != v)
		{
			add_building |= value->value;

			items = g_slist_prepend (items,
			                         g_strdup (value->value_nick));
		}

		if ((remove_flags & v) == v && (remove_building & v) != v)
		{
			gchar *n;

			remove_building |= value->value;
			n = g_strconcat ("-", value->value_nick, NULL);

			items = g_slist_prepend (items,
			                         n);
		}
	}

	for (item = items; item; item = g_slist_next (item))
	{
		g_ptr_array_add (attrs, item->data);
	}

	g_slist_free (items);

	g_ptr_array_add (attrs, NULL);

	gchar **vals = (gchar **)g_ptr_array_free (attrs, FALSE);
	gchar *ret = g_strjoinv (" | ", vals);
	g_strfreev (vals);

	g_type_class_unref (klass);

	return ret;
}

/**
 * cdn_variable_flags_from_string:
 * @flags: The flags to parse
 *
 * Parse a string into a set of variable flags. The flags can be specified
 * by their nicks (none, in, out, once, integrated) and separated by any
 * combination of spaces, comma's and/or pipes.
 *
 * Returns: A #CdnVariableFlags
 *
 **/
void
cdn_variable_flags_from_string (gchar const      *flags,
                                CdnVariableFlags *add_flags,
                                CdnVariableFlags *remove_flags)
{
	GFlagsClass *klass = g_type_class_ref (CDN_TYPE_VARIABLE_FLAGS);

	gchar **parts = g_strsplit_set (flags, ",| ", -1);
	gchar **ptr = parts;

	if (add_flags)
	{
		*add_flags = CDN_VARIABLE_FLAG_NONE;
	}

	if (remove_flags)
	{
		*remove_flags = CDN_VARIABLE_FLAG_NONE;
	}

	while (ptr && *ptr)
	{
		gchar const *name;
		CdnVariableFlags *fptr;

		name = *ptr;
		fptr = add_flags;

		if (*name == '-' || *name == '+')
		{
			if (*name == '-')
			{
				fptr = remove_flags;
			}

			++name;
		}

		GFlagsValue *value = g_flags_get_value_by_nick (klass, name);

		if (value && fptr)
		{
			*fptr |= value->value;
		}

		++ptr;
	}

	g_strfreev (parts);
	g_type_class_unref (klass);
}

/**
 * cdn_variable_get_full_name:
 * @variable: A #CdnVariable
 *
 * Get the full name of the variable. This is the name that can be used in the
 * outer most parent to refer to this variable (i.e.
 * <code>cdn_node_find_variable (top_parent, cdn_variable_get_full_name (deep_variable)) == deep_variable</code>)
 *
 * Returns: The full name of the variable. This is a newly allocated string that
 *          should be freed with g_free.
 *
 **/
gchar *
cdn_variable_get_full_name (CdnVariable *variable)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	if (!variable->priv->object)
	{
		return cdn_selector_escape_identifier (variable->priv->name);
	}

	gchar *objid = cdn_object_get_full_id (variable->priv->object);
	gchar *esc = cdn_selector_escape_identifier (variable->priv->name);
	gchar *ret = g_strconcat (objid, ".", esc, NULL);
	g_free (objid);
	g_free (esc);

	return ret;
}

static CdnObject *
find_interfaced (CdnObject    *object,
                 gchar const  *name,
                 gchar       **iname)
{
	CdnNode *parent;
	CdnVariableInterface *iface;

	parent = cdn_object_get_parent (object);

	if (parent == NULL)
	{
		return NULL;
	}

	iface = cdn_node_get_variable_interface (parent);

	gchar **names = cdn_variable_interface_get_names (iface);
	gchar **ptr;

	gchar const *pid = cdn_object_get_id (object);

	for (ptr = names; ptr && *ptr; ++ptr)
	{
		if (g_strcmp0 (cdn_variable_interface_lookup_child_name (iface, *ptr),
		               pid) != 0)
		{
			continue;
		}

		if (g_strcmp0 (cdn_variable_interface_lookup_variable_name (iface, *ptr),
		               name) != 0)
		{
			continue;
		}

		CdnObject *ret;
		ret = find_interfaced (CDN_OBJECT (parent), *ptr, iname);

		if (!ret)
		{
			ret = CDN_OBJECT (parent);

			if (iname)
			{
				*iname = g_strdup (*ptr);
			}
		}

		g_strfreev (names);
		return ret;
	}

	g_strfreev (names);
	return NULL;
}

gchar *
cdn_variable_get_full_name_for_display (CdnVariable *variable)
{
	CdnObject *group;

	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	if (!variable->priv->object)
	{
		return g_strdup (variable->priv->name);
	}

	/* Find out if there is somewhere an interface to us */
	gchar *propid;

	group = find_interfaced (variable->priv->object,
	                         variable->priv->name,
	                         &propid);

	gchar *objid;

	if (group != NULL)
	{
		objid = cdn_object_get_full_id_for_display (group);
	}
	else
	{
		objid = cdn_object_get_full_id_for_display (variable->priv->object);
		propid = g_strdup (variable->priv->name);
	}

	gchar *ret = g_strconcat (objid, ".", propid, NULL);
	g_free (objid);
	g_free (propid);

	return ret;
}

/**
 * cdn_variable_copy:
 * @variable: A #CdnVariable
 *
 * Make a copy of @variable.
 *
 * Returns: (transfer full): A #CdnVariable
 *
 **/
CdnVariable *
cdn_variable_copy (CdnVariable *variable)
{
	CdnVariable *ret;

	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	ret = cdn_variable_new (variable->priv->name,
	                        cdn_expression_copy (variable->priv->expression),
	                        variable->priv->flags);

	cdn_matrix_copy (&ret->priv->update, &variable->priv->update);

	cdn_modifiable_set_modified (CDN_MODIFIABLE (ret),
	                             variable->priv->modified);

	cdn_annotatable_set_annotation (CDN_ANNOTATABLE (ret),
	                                variable->priv->annotation);

	if (variable->priv->constraint)
	{
		set_constraint (ret, cdn_expression_copy (variable->priv->constraint));
	}

	return ret;
}

void
_cdn_variable_set_object (CdnVariable *variable,
                          CdnObject   *object,
                          gboolean     notify)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));
	g_return_if_fail (object == NULL || CDN_IS_OBJECT (object));

	set_object (variable, object, notify);
}

static GSList *
variable_get_actions (CdnNode    *o,
                      CdnVariable *variable,
                      GSList      *ret)
{
	GSList const *l;

	if (!o)
	{
		return ret;
	}

	l = cdn_node_get_edges (o);

	while (l)
	{
		GSList const *actions;

		actions = cdn_edge_get_actions (l->data);

		while (actions)
		{
			if (cdn_edge_action_get_target_variable (actions->data) == variable)
			{
				ret = g_slist_prepend (ret, actions->data);
			}

			actions = g_slist_next (actions);
		}

		l = g_slist_next (l);
	}

	return variable_get_actions (cdn_object_get_parent (CDN_OBJECT (o)),
	                             variable,
	                             ret);
}

/**
 * cdn_variable_get_actions:
 * @variable: A #CdnVariable
 *
 * Get the actions acting on this variable.
 *
 * Returns: (element-type CdnEdgeAction) (transfer full): A #GSList of #CdnEdgeAction
 *
 **/
GSList *
cdn_variable_get_actions (CdnVariable *variable)
{
	CdnObject *obj;

	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	obj = cdn_variable_get_object (variable);

	if (!CDN_IS_NODE (obj))
	{
		return NULL;
	}

	return g_slist_reverse (variable_get_actions (CDN_NODE (cdn_variable_get_object (variable)),
	                                              variable,
	                                              NULL));
}

void
cdn_variable_set_derivative (CdnVariable *variable,
                             CdnVariable *diffprop)
{
	g_return_if_fail (CDN_IS_VARIABLE (variable));
	g_return_if_fail (diffprop == NULL || CDN_IS_VARIABLE (diffprop));

	set_diff_of (variable, diffprop);
}

/**
 * cdn_variable_get_derivative:
 * @variable: A #CdnVariable
 *
 * Get the variable representing the derivative of @variable.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_variable_get_derivative (CdnVariable *variable)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	return variable->priv->diff_of;
}

/**
 * cdn_variable_get_integral:
 * @variable: A #CdnVariable
 *
 * Get the variable representing the integral of @variable.
 *
 * Returns: (transfer none): A #CdnVariable
 *
 **/
CdnVariable *
cdn_variable_get_integral (CdnVariable *variable)
{
	g_return_val_if_fail (CDN_IS_VARIABLE (variable), NULL);

	return variable->priv->diff_for;
}

gboolean
cdn_variable_compile (CdnVariable       *variable,
                      CdnCompileError   *error)
{
	gboolean ret;

	g_return_val_if_fail (CDN_IS_VARIABLE (variable), FALSE);
	g_return_val_if_fail (error == NULL || CDN_IS_COMPILE_ERROR (error), FALSE);

	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (variable->priv->expression)))
	{
		CdnCompileContext *context;

		context = cdn_object_get_compile_context (variable->priv->object, NULL);
		ret = cdn_expression_compile (variable->priv->expression, context, error);
		g_object_unref (context);
	}
	else if (variable->priv->update.values)
	{
		return TRUE;
	}
	else
	{
		ret = TRUE;
	}

	if (ret)
	{
		CdnDimension dim;

		cdn_expression_get_dimension (variable->priv->expression, &dim);

		cdn_matrix_set (&variable->priv->update,
		                NULL,
		                &dim);
	}
	else
	{
		cdn_compile_error_set (error,
		                       NULL,
		                       variable->priv->object,
		                       variable,
		                       NULL,
		                       NULL);
	}

	return ret;
}
