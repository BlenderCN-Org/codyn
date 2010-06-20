#include <string.h>

#include "cpg-property.h"
#include "cpg-expression.h"
#include "cpg-utils.h"
#include "cpg-enum-types.h"
#include "cpg-object.h"

#define CPG_PROPERTY_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CPG_TYPE_PROPERTY, CpgPropertyPrivate))

struct _CpgPropertyPrivate
{
	guint use_count;
	gchar *name;

	CpgExpression *expression;
	gboolean integrated;
	CpgPropertyHint hint;

	gdouble update;
	CpgObject *object;
};

G_DEFINE_TYPE (CpgProperty, cpg_property, G_TYPE_OBJECT)

enum
{
	PROP_0,
	PROP_NAME,
	PROP_OBJECT,
	PROP_HINT,
	PROP_INTEGRATED,
	PROP_EXPRESSION
};

/**
 * SECTION:property
 * @short_description: Property container
 *
 * A #CpgProperty is a container for a specific variable in an object. It
 * consists of a name and a mathematical expression describing its contents.
 *
 */

static void
set_object (CpgProperty *property,
            CpgObject   *object)
{
	if (property->priv->object == object)
	{
		return;
	}

	if (property->priv->object)
	{
		g_object_remove_weak_pointer (G_OBJECT (property->priv->object),
		                              (gpointer *)&property->priv->object);
	}

	property->priv->object = object;

	if (property->priv->object)
	{
		g_object_add_weak_pointer (G_OBJECT (property->priv->object),
		                           (gpointer *)&property->priv->object);
	}

	g_object_notify (G_OBJECT (property), "object");
}

static void
cpg_property_finalize (GObject *object)
{
	CpgProperty *property;

	property = CPG_PROPERTY (object);

	g_free (property->priv->name);

	if (property->priv->expression)
	{
		g_object_unref (property->priv->expression);
	}

	g_object_freeze_notify (object);
	set_object (property, NULL);
	g_object_thaw_notify (object);

	G_OBJECT_CLASS (cpg_property_parent_class)->finalize (object);
}

static void
set_expression (CpgProperty *property,
                CpgExpression *expression)
{
	if (property->priv->expression)
	{
		g_object_unref (property->priv->expression);
	}

	property->priv->expression = g_object_ref (expression);

	g_object_notify (G_OBJECT (property), "expression");
}

static void
set_integrated (CpgProperty *property,
                gboolean     integrated)
{
	if (integrated != property->priv->integrated)
	{
		property->priv->integrated = integrated;

		g_object_notify (G_OBJECT (property), "integrated");
	}
}

static void
set_hint (CpgProperty     *property,
          CpgPropertyHint  hint)
{
	if (hint != property->priv->hint)
	{
		property->priv->hint = hint;

		g_object_notify (G_OBJECT (property), "hint");
	}
}

static void
cpg_property_set_property (GObject      *object,
                           guint         prop_id,
                           const GValue *value,
                           GParamSpec   *pspec)
{
	CpgProperty *self = CPG_PROPERTY (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_free (self->priv->name);
			self->priv->name = g_value_dup_string (value);
		break;
		case PROP_OBJECT:
			set_object (self, g_value_get_object (value));
		break;
		case PROP_HINT:
			set_hint (self, g_value_get_flags (value));
		break;
		case PROP_INTEGRATED:
			set_integrated (self, g_value_get_boolean (value));
		break;
		case PROP_EXPRESSION:
			set_expression (self, g_value_get_object (value));
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_property_get_property (GObject    *object,
                           guint       prop_id,
                           GValue     *value,
                           GParamSpec *pspec)
{
	CpgProperty *self = CPG_PROPERTY (object);

	switch (prop_id)
	{
		case PROP_NAME:
			g_value_set_string (value, self->priv->name);
		break;
		case PROP_OBJECT:
			g_value_set_object (value, self->priv->object);
		break;
		case PROP_HINT:
			g_value_set_flags (value, self->priv->hint);
		break;
		case PROP_INTEGRATED:
			g_value_set_boolean (value, self->priv->integrated);
		break;
		case PROP_EXPRESSION:
			g_value_set_object (value, self->priv->expression);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_property_class_init (CpgPropertyClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_property_finalize;

	object_class->get_property = cpg_property_get_property;
	object_class->set_property = cpg_property_set_property;

	g_type_class_add_private (object_class, sizeof(CpgPropertyPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_NAME,
	                                 g_param_spec_string ("name",
	                                                      "Name",
	                                                      "Name",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));


	g_object_class_install_property (object_class,
	                                 PROP_OBJECT,
	                                 g_param_spec_object ("object",
	                                                      "Object",
	                                                      "Object",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


	g_object_class_install_property (object_class,
	                                 PROP_HINT,
	                                 g_param_spec_flags ("hint",
	                                                     "Hint",
	                                                     "Hint",
	                                                     CPG_TYPE_PROPERTY_HINT,
	                                                     CPG_PROPERTY_HINT_NONE,
	                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


	g_object_class_install_property (object_class,
	                                 PROP_INTEGRATED,
	                                 g_param_spec_boolean ("integrated",
	                                                       "Integrated",
	                                                       "Integrated",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


	g_object_class_install_property (object_class,
	                                 PROP_EXPRESSION,
	                                 g_param_spec_object ("expression",
	                                                      "Expression",
	                                                      "Expression",
	                                                      CPG_TYPE_EXPRESSION,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static void
cpg_property_init (CpgProperty *self)
{
	self->priv = CPG_PROPERTY_GET_PRIVATE (self);
}

/**
 * cpg_property_new:
 * @name: the property name
 * @expression: the value expression
 * @integrated: whether this property should be integated during the simulation
 * @object: the #CpgObject to which the property belongs
 *
 * Create a new property object. Property objects are assigned to #CpgObject
 * objects and are of little use on their own. The provided expression will
 * not be parsed initially.
 *
 * Returns: the new #CpgProperty
 *
 **/
CpgProperty *
cpg_property_new (gchar const  *name,
                  gchar const  *expression,
                  gboolean      integrated,
                  CpgObject    *object)
{
	CpgExpression *expr;
	CpgProperty *ret;

	expr = cpg_expression_new (expression);

	ret = g_object_new (CPG_TYPE_PROPERTY,
	                    "name", name,
	                    "expression", expr,
	                    "integrated", integrated,
	                    "object", object,
	                    NULL);

	g_object_unref (expr);
	return ret;
}

/**
 * cpg_property_get_object:
 * @property: the #CpgProperty
 *
 * Get the object associated with the property
 *
 * Returns: the object associated with the property
 **/
CpgObject *
cpg_property_get_object (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->object;
}

/**
 * cpg_property_set_value:
 * @property: the #CpgProperty
 * @value: the new value
 *
 * Change the value to a specific number. To set the value
 * to any expression, use #cpg_object_set_value.
 *
 **/
void
cpg_property_set_value (CpgProperty  *property,
                        gdouble       value)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	cpg_expression_set_value (property->priv->expression, value);
}

/**
 * cpg_property_get_value:
 * @property: the #CpgProperty
 *
 * Get the numerical value of the current value of the property
 *
 * Return value: the numerical value of the property's current value
 *
 **/
gdouble
cpg_property_get_value (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	if (property->priv->expression)
	{
		return cpg_expression_evaluate (property->priv->expression);
	}
	else
	{
		return 0;
	}
}

/**
 * cpg_property_get_value_expression:
 * @property: a #CpgProperty
 *
 * Get the property value expression
 *
 * Returns: a #CpgExpression. The expression is owned by the property and
 *          should not be freed
 *
 **/
CpgExpression *
cpg_property_get_expression (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->expression;
}

/**
 * cpg_property_set_value_expression:
 * @property: a #CpgProperty
 * @expression: the expression
 *
 * Set the property value from an expression.
 *
 **/
void
cpg_property_set_expression (CpgProperty   *property,
                             CpgExpression *expression)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));
	g_return_if_fail (CPG_IS_EXPRESSION (expression));

	set_expression (property, expression);
}

/**
 * cpg_property_get_name:
 * @property: a #CpgProperty
 *
 * Get the property name
 *
 * Returns: the property name
 *
 **/
gchar const *
cpg_property_get_name (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	return property->priv->name;
}

/**
 * cpg_property_get_integrated:
 * @property: a #CpgProperty
 *
 * Get whether the property should be integrated during evaluation or not
 *
 * Returns: %TRUE if the property will be integrated, %FALSE otherwise
 *
 **/
gboolean
cpg_property_get_integrated (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);

	return property->priv->integrated;
}

/**
 * cpg_property_set_integrated:
 * @property: a #CpgProperty
 * @integrated: integrate the property
 *
 * Set whether the property should be integrated during evaluation or not
 *
 **/
void
cpg_property_set_integrated (CpgProperty  *property,
                             gboolean      integrated)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_integrated (property, integrated);
}

/**
 * cpg_property_reset_cache:
 * @property: a #CpgProperty
 *
 * Reset the cached value of the property expression
 *
 **/
void
cpg_property_reset_cache (CpgProperty *property)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	/* Never reset the cache of something that is only initialized once */
	if (!(property->priv->hint & CPG_PROPERTY_HINT_ONCE))
	{
		cpg_expression_reset_cache (property->priv->expression);
	}
}

/**
 * cpg_property_get_used:
 * @property: A #CpgProperty
 * 
 * Get how many times the property is used currently.
 *
 * Returns: The number of times the property is used
 *
 **/
guint
cpg_property_get_used (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	return property->priv->use_count;
}

/**
 * cpg_property_equal:
 * @property: a #CpgProperty
 * @other: a #CpgProperty
 *
 * Compare two properties for equal values/expressions
 *
 * Returns: %TRUE if the properties are equal, %FALSE otherwise
 *
 **/
gboolean
cpg_property_equal (CpgProperty *property,
                    CpgProperty *other)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);
	g_return_val_if_fail (CPG_IS_PROPERTY (other), FALSE);

	return property->priv->integrated == other->priv->integrated &&
	       property->priv->hint == other->priv->hint &&
	       cpg_expression_equal (cpg_property_get_expression (property),
	                             cpg_property_get_expression (other));
}

/**
 * cpg_property_get_hint:
 * @property: A #CpgProperty
 * 
 * The property hint. The hint contains information on the type of property (
 * such as in, or out).
 *
 * Returns: A #CpgPropertyHint
 *
 **/
CpgPropertyHint
cpg_property_get_hint (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), CPG_PROPERTY_HINT_NONE);

	return property->priv->hint;
}

/**
 * cpg_property_set_hint:
 * @property: A #CpgProperty
 * @hint: A #CpgPropertyHint
 * 
 * Set the property hint.
 *
 **/
void
cpg_property_set_hint (CpgProperty     *property,
                       CpgPropertyHint  hint)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_hint (property, hint);
}

/**
 * cpg_property_add_hint:
 * @property: A #CpgProperty
 * @hint: A #CpgPropertyHint
 * 
 * Add a hint flag to the property hints.
 *
 **/
void
cpg_property_add_hint (CpgProperty     *property,
                       CpgPropertyHint  hint)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_hint (property, property->priv->hint | hint);
}

/**
 * cpg_property_remove_hint:
 * @property: A #CpgProperty
 * @hint: A #CpgPropertyHint
 * 
 * Remove a hint flag from the property hints.
 *
 **/
void
cpg_property_remove_hint (CpgProperty     *property,
                          CpgPropertyHint  hint)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	set_hint (property, property->priv->hint & ~hint);
}

CpgProperty *
_cpg_property_copy (CpgProperty *property)
{
	CpgProperty *ret;

	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	ret = cpg_property_new (property->priv->name,
	                        cpg_expression_get_as_string (property->priv->expression),
	                        property->priv->integrated,
	                        NULL);

	ret->priv->hint = property->priv->hint;
	ret->priv->update = property->priv->update;

	return ret;
}

void
_cpg_property_use (CpgProperty *property)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	++(property->priv->use_count);
}

gboolean
_cpg_property_unuse (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);

	if (property->priv->use_count == 0)
	{
		return TRUE;
	}

	return (--(property->priv->use_count) == 0);
}

void
_cpg_property_set_update (CpgProperty  *property,
                          gdouble       value)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));

	property->priv->update = value;
}

gdouble
_cpg_property_get_update (CpgProperty *property)
{
	g_return_val_if_fail (CPG_IS_PROPERTY (property), 0);

	return property->priv->update;
}

void
_cpg_property_set_object (CpgProperty *property,
                          CpgObject   *object)
{
	g_return_if_fail (CPG_IS_PROPERTY (property));
	g_return_if_fail (CPG_IS_OBJECT (object));

	set_object (property, object);
}

