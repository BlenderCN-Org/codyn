/*
 * cpg-object.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <string.h>

#include "cpg-object.h"

#include "cpg-link.h"
#include "cpg-expression.h"

#include "cpg-compile-error.h"
#include "cpg-utils.h"
#include "cpg-marshal.h"
#include "cpg-usable.h"
#include "cpg-tokenizer.h"
#include "cpg-annotatable.h"
#include "cpg-layoutable.h"
#include "cpg-selector.h"
#include "cpg-taggable.h"

/**
 * SECTION:cpg-object
 * @short_description: Basis for all cpg objects
 *
 * #CpgObject is a base class for all the objects which can be added to a
 * network. It provides property storage and virtual methods which can be
 * implemented that drive the simulation process.
 *
 */

/**
 * CpgForeachExpressionFunc:
 * @expression: a #CpgExpression
 * @userdata: user data
 *
 * A function callback called on each expression found in an object.
 *
 **/

#define CPG_OBJECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_OBJECT, CpgObjectPrivate))

struct _CpgObjectPrivate
{
	guint use_count;
	gchar *id;

	gint x;
	gint y;

	CpgObject *parent;

	/* Properties */
	GSList *properties;
	GHashTable *property_hash;

	/* Links */
	GSList *links;
	GSList *actors;

	/* Templates */
	GSList *templates;
	GSList *templates_reverse_map;

	gchar *annotation;
	GHashTable *tags;

	GSList *when_applied;

	gboolean compiled : 1;
	gboolean auto_imported : 1;
	gboolean has_location : 1;
};

/* Properties */
enum
{
	PROP_0,
	PROP_ID,
	PROP_PARENT,
	PROP_AUTO_IMPORTED,
	PROP_USE_COUNT,
	PROP_ANNOTATION,
	PROP_X,
	PROP_Y,
	PROP_HAS_LOCATION
};

/* Signals */
enum
{
	TAINTED,
	COMPILED,
	RESETTED,
	PROPERTY_ADDED,
	PROPERTY_REMOVED,
	PROPERTY_CHANGED,
	COPIED,
	VERIFY_REMOVE_PROPERTY,
	TEMPLATE_APPLIED,
	TEMPLATE_UNAPPLIED,
	NUM_SIGNALS
};

static void cpg_usable_iface_init (gpointer iface);
static void cpg_annotatable_iface_init (gpointer iface);
static void cpg_layoutable_iface_init (gpointer iface);
static void cpg_taggable_iface_init (gpointer iface);

G_DEFINE_TYPE_WITH_CODE (CpgObject,
                         cpg_object,
                         G_TYPE_OBJECT,
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_USABLE,
                                                cpg_usable_iface_init);
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_ANNOTATABLE,
                                                cpg_annotatable_iface_init);
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_LAYOUTABLE,
                                                cpg_layoutable_iface_init);
                         G_IMPLEMENT_INTERFACE (CPG_TYPE_TAGGABLE,
                                                cpg_taggable_iface_init));

static guint object_signals[NUM_SIGNALS] = {0,};

static GHashTable *
get_tagtable (CpgTaggable *taggable)
{
	return CPG_OBJECT (taggable)->priv->tags;
}

static void
cpg_taggable_iface_init (gpointer iface)
{
	/* Use default implementation */
	CpgTaggableInterface *taggable = iface;

	taggable->get_tagtable = get_tagtable;
}

GQuark
cpg_object_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_object_error");
	}

	return quark;
}

static void
cpg_object_use (CpgUsable *usable)
{
	CpgObject *obj = CPG_OBJECT (usable);
	++obj->priv->use_count;
}

static gboolean
cpg_object_unuse (CpgUsable *usable)
{
	CpgObject *obj = CPG_OBJECT (usable);

	if (obj->priv->use_count == 0)
	{
		return TRUE;
	}

	return (--(obj->priv->use_count) == 0);
}

static void
cpg_layoutable_iface_init (gpointer iface)
{
}

static void
cpg_usable_iface_init (gpointer iface)
{
	CpgUsableInterface *usable = iface;

	usable->use = cpg_object_use;
	usable->unuse = cpg_object_unuse;
}

static gchar *
cpg_object_annotatable_get_title (CpgAnnotatable *annotatable)
{
	CpgObject *obj;

	obj = CPG_OBJECT (annotatable);

	return cpg_object_get_full_id_for_display (obj);
}

static void
cpg_annotatable_iface_init (gpointer iface)
{
	CpgAnnotatableInterface *annotatable = iface;

	annotatable->get_title = cpg_object_annotatable_get_title;
}

static void
free_property (CpgProperty *property,
               CpgObject   *object)
{
	cpg_usable_unuse (CPG_USABLE (property));

	g_signal_handlers_disconnect_by_func (property,
	                                      cpg_object_taint,
	                                      object);

	g_object_unref (property);
}

static void
cpg_object_finalize (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);

	g_slist_foreach (obj->priv->properties, (GFunc)free_property, object);
	g_slist_free (obj->priv->properties);

	g_slist_free (obj->priv->links);
	g_slist_free (obj->priv->actors);

	g_free (obj->priv->id);

	g_free (obj->priv->annotation);

	g_hash_table_destroy (obj->priv->property_hash);
	g_hash_table_destroy (obj->priv->tags);

	g_slist_foreach (obj->priv->when_applied, (GFunc)g_object_unref, NULL);
	g_slist_free (obj->priv->when_applied);

	G_OBJECT_CLASS (cpg_object_parent_class)->finalize (object);
}

/* interface implementations */
static void
cpg_object_reset_impl (CpgObject *object)
{
	g_slist_foreach (object->priv->properties, (GFunc)cpg_property_reset, NULL);

	cpg_object_taint (object);

	g_signal_emit (object, object_signals[RESETTED], 0);
}

static void
set_id (CpgObject   *object,
        const gchar *id)
{
	g_free (object->priv->id);
	object->priv->id = g_strdup (id);
}

static void
get_property (GObject     *object,
              guint        prop_id,
              GValue      *value,
              GParamSpec  *pspec)
{
	CpgObject *obj = CPG_OBJECT (object);

	switch (prop_id)
	{
		case PROP_ID:
			g_value_set_string (value, obj->priv->id);
		break;
		case PROP_PARENT:
			g_value_set_object (value, obj->priv->parent);
		break;
		case PROP_AUTO_IMPORTED:
			g_value_set_boolean (value, obj->priv->auto_imported);
		break;
		case PROP_USE_COUNT:
			g_value_set_uint (value, obj->priv->use_count);
		break;
		case PROP_ANNOTATION:
			g_value_set_string (value, obj->priv->annotation);
		break;
		case PROP_X:
			g_value_set_int (value, obj->priv->x);
		break;
		case PROP_Y:
			g_value_set_int (value, obj->priv->y);
		break;
		case PROP_HAS_LOCATION:
			g_value_set_boolean (value, obj->priv->has_location);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
set_property (GObject       *object,
              guint          prop_id,
              GValue const  *value,
              GParamSpec    *pspec)
{
	CpgObject *obj = CPG_OBJECT (object);

	switch (prop_id)
	{
		case PROP_ID:
			set_id (obj, g_value_get_string (value));
		break;
		case PROP_AUTO_IMPORTED:
			obj->priv->auto_imported = g_value_get_boolean (value);
		break;
		case PROP_ANNOTATION:
			g_free (obj->priv->annotation);
			obj->priv->annotation = g_value_dup_string (value);
		break;
		case PROP_X:
			obj->priv->x = g_value_get_int (value);
		break;
		case PROP_Y:
			obj->priv->y = g_value_get_int (value);
		break;
		case PROP_HAS_LOCATION:
			obj->priv->has_location = g_value_get_boolean (value);
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object,
			                                   prop_id,
			                                   pspec);
		break;
	}
}

static void
link_destroyed (CpgObject  *object,
                CpgLink    *link,
                gboolean    is_last_ref)
{
	if (!is_last_ref)
	{
		return;
	}

	/* Remove link, and toggle ref */
	object->priv->links = g_slist_remove (object->priv->links, link);

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            object);
}

static void
on_template_property_expression_changed (CpgProperty *prop,
                                         GParamSpec  *spec,
                                         CpgObject   *object)
{
	CpgProperty *orig = cpg_object_get_property (object,
	                                             cpg_property_get_name (prop));

	if (!orig)
	{
		return;
	}

	if (cpg_modifiable_get_modified (CPG_MODIFIABLE (orig)))
	{
		return;
	}

	CpgObject *templ = cpg_object_get_property_template (object, orig, FALSE);

	if (templ != cpg_property_get_object (prop))
	{
		return;
	}

	cpg_property_set_expression (orig,
	                             cpg_expression_copy (cpg_property_get_expression (prop)));

	cpg_modifiable_set_modified (CPG_MODIFIABLE (orig), FALSE);
}

static void
on_template_property_flags_changed (CpgProperty *prop,
                                    GParamSpec  *spec,
                                    CpgObject   *object)
{
	/* Check if the current prop had the same, if so, also change
	   it here */
	CpgProperty *orig = cpg_object_get_property (object,
	                                             cpg_property_get_name (prop));

	if (!orig)
	{
		return;
	}

	if (cpg_modifiable_get_modified (CPG_MODIFIABLE (orig)))
	{
		return;
	}

	CpgObject *templ = cpg_object_get_property_template (object, orig, FALSE);

	if (templ != cpg_property_get_object (prop))
	{
		return;
	}

	cpg_property_set_flags (orig, cpg_property_get_flags (prop));
	cpg_modifiable_set_modified (CPG_MODIFIABLE (orig), FALSE);
}

static void
on_template_property_added (CpgObject   *templ,
                            CpgProperty *prop,
                            CpgObject   *object)
{
	CpgProperty *orig =
		cpg_object_get_property (object,
		                         cpg_property_get_name (prop));

	if (orig == NULL ||
	    cpg_object_get_property_template (object, orig, TRUE))
	{
		if (cpg_object_add_property (object,
		                             cpg_property_copy (prop),
		                             NULL))
		{
			orig = cpg_object_get_property (object,
			                                cpg_property_get_name (prop));

			cpg_modifiable_set_modified (CPG_MODIFIABLE (orig), FALSE);
		}
		else
		{
			return;
		}
	}

	g_signal_connect (prop,
	                  "notify::expression",
	                  G_CALLBACK (on_template_property_expression_changed),
	                  object);

	g_signal_connect (prop,
	                  "notify::flags",
	                  G_CALLBACK (on_template_property_flags_changed),
	                  object);
}

static void
disconnect_template_property (CpgObject   *object,
                              CpgObject   *templ,
                              CpgProperty *prop)
{
	g_signal_handlers_disconnect_by_func (prop,
	                                      on_template_property_expression_changed,
	                                      object);

	g_signal_handlers_disconnect_by_func (prop,
	                                      on_template_property_flags_changed,
	                                      object);
}

static void
on_template_property_removed (CpgObject   *templ,
                              CpgProperty *prop,
                              CpgObject   *object)
{
	gchar const *name = cpg_property_get_name (prop);
	CpgProperty *orig = cpg_object_get_property (object, name);

	if (orig && !cpg_modifiable_get_modified (CPG_MODIFIABLE (orig)))
	{
		CpgObject *temp = cpg_object_get_property_template (object, orig, FALSE);

		if (temp == NULL)
		{
			/* Remove the original property as well */
			cpg_object_remove_property (object,
			                            cpg_property_get_name (orig),
			                            NULL);
		}
		else
		{
			/* Then, reupdate the property value from the other
			   template */
			CpgProperty *tempProp = cpg_object_get_property (temp,
			                                                 name);

			cpg_object_add_property (object,
			                         cpg_property_copy (tempProp),
			                         NULL);
		}
	}

	disconnect_template_property (object, templ, prop);
}

static void
disconnect_template (CpgObject *object,
                     CpgObject *templ,
                     gboolean   disconnect_properties)
{
	if (disconnect_properties)
	{
		GSList *item;

		for (item = templ->priv->properties; item; item = g_slist_next (item))
		{
			disconnect_template_property (object, templ, item->data);
		}
	}

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_property_added,
	                                      object);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_property_removed,
	                                      object);

	cpg_usable_unuse (CPG_USABLE (templ));
	g_object_unref (templ);
}

static void
cpg_object_dispose (GObject *object)
{
	CpgObject *obj = CPG_OBJECT (object);

	GSList *templates = obj->priv->templates;
	obj->priv->templates = NULL;

	GSList *item;

	for (item = templates; item; item = g_slist_next (item))
	{
		disconnect_template (obj, item->data, TRUE);
	}

	g_slist_free (templates);

	g_slist_free (obj->priv->templates_reverse_map);
	obj->priv->templates_reverse_map = NULL;

	/* Untoggle ref all links, because we need them destroyed! */
	GSList *copy = g_slist_copy (obj->priv->links);

	for (item = copy; item; item = g_slist_next (item))
	{
		link_destroyed (obj, item->data, TRUE);
	}

	g_slist_free (copy);

	G_OBJECT_CLASS (cpg_object_parent_class)->dispose (object);
}

static void
cpg_object_foreach_expression_impl (CpgObject                *object,
                                    CpgForeachExpressionFunc  func,
                                    gpointer                  userdata)

{
	GSList *item;

	for (item = object->priv->properties; item; item = g_slist_next (item))
	{
		func (cpg_property_get_expression (item->data), userdata);
	}
}

static void
check_modified_for_template (CpgObject   *object,
                             CpgProperty *property)
{
	CpgObject *templ;

	templ = cpg_object_get_property_template (object,
	                                          property,
	                                          TRUE);

	if (templ != NULL)
	{
		cpg_modifiable_set_modified (CPG_MODIFIABLE (property), FALSE);
	}
}

static void
on_property_changed (CpgObject   *object,
                     GParamSpec  *spec,
                     CpgProperty *property)
{
	g_signal_emit (object, object_signals[PROPERTY_CHANGED], 0, property);

	check_modified_for_template (object, property);

	cpg_object_taint (object);
}

static void
on_property_modified (CpgObject   *object,
                      GParamSpec  *spec,
                      CpgProperty *property)
{
	check_modified_for_template (object, property);
}

static gboolean
on_property_invalidate_name (CpgProperty *property,
                             gchar const *name,
                             CpgObject   *object)
{
	CpgProperty *other = cpg_object_get_property (object, name);

	return other && other != property;
}

static void
add_property (CpgObject   *object,
              CpgProperty *property)
{
	g_object_ref_sink (property);

	object->priv->properties = g_slist_append (object->priv->properties,
	                                           property);

	g_hash_table_insert (object->priv->property_hash,
	                     g_strdup (cpg_property_get_name (property)),
	                     property);

	_cpg_property_set_object (property, object);

	cpg_usable_use (CPG_USABLE (property));
	cpg_object_taint (object);

	g_signal_connect_swapped (property,
	                          "notify::expression",
	                          G_CALLBACK (on_property_changed),
	                          object);

	g_signal_connect_swapped (property,
	                          "notify::flags",
	                          G_CALLBACK (on_property_changed),
	                          object);

	g_signal_connect_swapped (property,
	                          "notify::modified",
	                          G_CALLBACK (on_property_modified),
	                          object);

	g_signal_connect (property,
	                  "invalidate-name",
	                  G_CALLBACK (on_property_invalidate_name),
	                  object);

	g_signal_emit (object, object_signals[PROPERTY_ADDED], 0, property);
}

static void
cpg_object_copy_impl (CpgObject *object,
                      CpgObject *source)
{
	/* Copy properties */
	GSList *item;
	gchar *annotation;

	for (item = source->priv->properties; item; item = g_slist_next (item))
	{
		CpgProperty *prop = item->data;

		add_property (object, cpg_property_copy (prop));
	}

	object->priv->templates = g_slist_copy (source->priv->templates);

	for (item = object->priv->templates; item; item = g_slist_next (item))
	{
		cpg_usable_use (item->data);
		g_object_ref (item->data);
	}

	object->priv->templates_reverse_map =
		g_slist_copy (object->priv->templates_reverse_map);

	annotation = cpg_annotatable_get_annotation (CPG_ANNOTATABLE (source));
	cpg_annotatable_set_annotation (CPG_ANNOTATABLE (object),
	                                annotation);

	if (cpg_layoutable_supports_location (CPG_LAYOUTABLE (source)) &&
	    cpg_layoutable_supports_location (CPG_LAYOUTABLE (object)))
	{
		gint x;
		gint y;

		cpg_layoutable_get_location (CPG_LAYOUTABLE (source), &x, &y);
		cpg_layoutable_set_location (CPG_LAYOUTABLE (object), x, y);
	}

	g_free (annotation);
}

static gboolean
cpg_object_unapply_template_impl (CpgObject  *object,
                                  CpgObject  *templ,
                                  GError    **error)
{
	GSList *item;

	object->priv->templates = g_slist_remove (object->priv->templates,
	                                          templ);

	templ->priv->templates_reverse_map =
		g_slist_remove (templ->priv->templates_reverse_map,
		                object);

	for (item = templ->priv->properties; item; item = g_slist_next (item))
	{
		on_template_property_removed (templ, item->data, object);
	}

	/* Keep the template around for the signal emission */
	g_object_ref (templ);
	cpg_usable_use (CPG_USABLE (templ));

	disconnect_template (object, templ, FALSE);

	g_signal_emit (object, object_signals[TEMPLATE_UNAPPLIED], 0, templ);

	cpg_usable_unuse (CPG_USABLE (templ));
	g_object_unref (templ);

	return TRUE;
}

static gboolean
cpg_object_apply_template_impl (CpgObject  *object,
                                CpgObject  *templ,
                                GError    **error)
{
	/* Apply properties */
	GSList *item;

	for (item = templ->priv->properties; item; item = g_slist_next (item))
	{
		on_template_property_added (templ, item->data, object);
	}

	g_signal_connect (templ,
	                  "property-added",
	                  G_CALLBACK (on_template_property_added),
	                  object);

	g_signal_connect (templ,
	                  "property-removed",
	                  G_CALLBACK (on_template_property_removed),
	                  object);

	object->priv->templates = g_slist_append (object->priv->templates,
	                                          g_object_ref (templ));

	templ->priv->templates_reverse_map =
		g_slist_append (templ->priv->templates_reverse_map,
		                object);

	cpg_usable_use (CPG_USABLE (templ));

	g_signal_emit (object, object_signals[TEMPLATE_APPLIED], 0, templ);

	return TRUE;
}

static gboolean
expression_depends_on_real (CpgExpression *expression,
                            CpgProperty   *property,
                            GHashTable    *processed)
{
	GSList const *dependencies;

	dependencies = cpg_expression_get_dependencies (expression);

	while (dependencies)
	{
		if (dependencies->data == property)
		{
			return TRUE;
		}

		/* Don't scan again */
		if (g_hash_table_lookup (processed, dependencies->data))
		{
			return FALSE;
		}

		g_hash_table_insert (processed, dependencies->data, GINT_TO_POINTER (TRUE));

		if (expression_depends_on_real (cpg_property_get_expression (dependencies->data),
		                                property,
		                                processed))
		{
			return TRUE;
		}

		dependencies = g_slist_next (dependencies);
	}

	return FALSE;
}

static gboolean
expression_depends_on (CpgExpression *expression,
                       CpgProperty   *property)
{
	GHashTable *processed;
	gboolean ret;

	processed = g_hash_table_new (g_direct_hash, g_direct_equal);
	ret = expression_depends_on_real (expression, property, processed);

	g_hash_table_destroy (processed);

	return ret;
}

static gboolean
cpg_object_compile_impl (CpgObject         *object,
                         CpgCompileContext *context,
                         CpgCompileError   *error)
{
	if (cpg_object_is_compiled (object))
	{
		/* Don't recompile if not necessary */
		return TRUE;
	}

	/* Compile all the property expressions */
	GSList *properties = object->priv->properties;
	gboolean ret = TRUE;

	/* Prepend the object in the context */
	cpg_compile_context_save (context);
	cpg_compile_context_prepend_object (context, object);

	while (properties)
	{
		CpgProperty *property = (CpgProperty *)properties->data;
		CpgExpression *expr = cpg_property_get_expression (property);
		GError *gerror = NULL;

		if (!cpg_expression_compile (expr,
		                             context,
		                             &gerror))
		{
			g_warning ("Error while parsing expression [%s].%s<%s>: %s",
			           cpg_object_get_id (object),
			           cpg_property_get_name (property),
			           cpg_expression_get_as_string (expr),
			           gerror->message);


			if (error)
			{
				cpg_compile_error_set (error,
				                       gerror,
				                       object,
				                       property,
				                       NULL,
				                       cpg_expression_get_error_at (expr));
			}

			g_error_free (gerror);

			ret = FALSE;
			break;
		}

		if (expression_depends_on (expr, property))
		{
			if (error)
			{
				gerror = g_error_new (CPG_COMPILE_ERROR_TYPE,
				                      CPG_COMPILE_ERROR_PROPERTY_RECURSE,
				                      "Infinite recursion in property expression");

				cpg_compile_error_set (error,
				                       gerror,
				                       object,
				                       property,
				                       NULL,
				                       0);

				g_error_free (gerror);
				ret = FALSE;
				break;
			}
		}

		properties = g_slist_next (properties);
	}

	object->priv->compiled = ret;

	if (ret)
	{
		cpg_object_foreach_expression (object,
		                               (CpgForeachExpressionFunc)cpg_expression_reset_cache,
		                               NULL);

		g_signal_emit (object, object_signals[COMPILED], 0);
	}

	cpg_compile_context_restore (context);
	return ret;
}

static GSList *
cpg_object_get_properties_impl (CpgObject *object)
{
	return g_slist_copy (object->priv->properties);
}

static CpgProperty *
cpg_object_get_property_impl (CpgObject   *object,
                              const gchar *name)
{
	return g_hash_table_lookup (object->priv->property_hash, name);
}

static gboolean
cpg_object_has_property_impl (CpgObject *object,
                              const gchar *name)
{
	return cpg_object_get_property (object, name) != NULL;
}

static gboolean
remove_property (CpgObject   *object,
                 CpgProperty *property,
                 gboolean     check_unuse)
{
	if (!cpg_usable_unuse (CPG_USABLE (property)) && check_unuse)
	{
		cpg_usable_use (CPG_USABLE (property));
		return FALSE;
	}

	object->priv->properties = g_slist_remove (object->priv->properties,
	                                           property);

	_cpg_property_set_object (property, NULL);

	g_hash_table_remove (object->priv->property_hash,
	                     cpg_property_get_name (property));

	g_signal_handlers_disconnect_by_func (property,
	                                      on_property_changed,
	                                      object);

	g_signal_handlers_disconnect_by_func (property,
	                                      on_property_invalidate_name,
	                                      object);

	g_signal_handlers_disconnect_by_func (property,
	                                      on_property_modified,
	                                      object);

	g_signal_emit (object,
	               object_signals[PROPERTY_REMOVED],
	               0,
	               property);

	g_object_unref (property);

	cpg_object_taint (object);
	return TRUE;
}

static gboolean
cpg_object_verify_remove_property_impl (CpgObject    *object,
                                        const gchar  *name,
                                        GError      **error)
{
	if (error)
	{
		*error = NULL;
	}

	CpgProperty *property = cpg_object_get_property (object, name);

	if (property)
	{
		/* Check if the property is still used */
		if (cpg_usable_use_count (CPG_USABLE (property)) > 1)
		{
			if (error)
			{
				g_set_error (error,
				             CPG_OBJECT_ERROR,
				             CPG_OBJECT_ERROR_PROPERTY_IN_USE,
				             "Property `%s' is still in use and can not be removed",
				             name);
			}

			return FALSE;
		}

		/* Check if the property is instantiated from a template */
		CpgObject *temp;
		temp = cpg_object_get_property_template (object, property, FALSE);

		if (temp != NULL)
		{
			if (error)
			{
				g_set_error (error,
				             CPG_OBJECT_ERROR,
				             CPG_OBJECT_ERROR_PROPERTY_FROM_TEMPLATE,
				             "The property `%s' is instantiated from the template `%s' and cannot be removed",
				             name,
				             cpg_object_get_id (temp));
			}

			return FALSE;
		}
	}
	else
	{
		/* Since there is no such property, it cannot be removed */
		if (error)
		{
			g_set_error (error,
			             CPG_OBJECT_ERROR,
			             CPG_OBJECT_ERROR_PROPERTY_NOT_FOUND,
			             "Property %s could not be found for %s",
			             name,
			             cpg_object_get_id (object));
		}

		return FALSE;
	}

	return TRUE;
}

static gboolean
cpg_object_remove_property_impl (CpgObject    *object,
                                 const gchar  *name,
                                 GError      **error)
{
	if (!cpg_object_verify_remove_property (object, name, error))
	{
		return FALSE;
	}

	CpgProperty *property = cpg_object_get_property (object, name);

	remove_property (object, property, FALSE);
	return TRUE;
}

static gboolean
cpg_object_add_property_impl (CpgObject    *object,
                              CpgProperty  *property,
                              GError      **error)
{
	// Check if property already set
	CpgProperty *existing;

	existing = cpg_object_get_property (object,
	                                    cpg_property_get_name (property));

	if (existing)
	{
		cpg_property_set_expression (existing,
		                             cpg_property_get_expression (property));

		cpg_property_set_flags (existing,
		                        cpg_property_get_flags (property));

		if (g_object_is_floating (G_OBJECT (property)))
		{
			g_object_unref (property);
		}

		return TRUE;
	}

	if (!cpg_tokenizer_validate_identifier (cpg_property_get_name (property)))
	{
		gchar *nm;

		nm = cpg_object_get_full_id (object);

		g_set_error (error,
		             CPG_OBJECT_ERROR,
		             CPG_OBJECT_ERROR_INVALID_PROPERTY_NAME,
		             "Invalid property name `%s.%s'",
		             nm,
		             cpg_property_get_name (property));

		g_free (nm);

		return FALSE;
	}

	add_property (object, property);
	return TRUE;
}

static gint
compare_property_dependencies (CpgProperty *prop1,
                               CpgProperty *prop2)
{
	CpgExpression *e1 = cpg_property_get_expression (prop1);
	CpgExpression *e2 = cpg_property_get_expression (prop2);

	GSList *d1 = (GSList *)cpg_expression_get_dependencies (e1);
	GSList *d2 = (GSList *)cpg_expression_get_dependencies (e2);

	if (g_slist_find (d1, prop2) != NULL)
	{
		return 1;
	}
	else if (g_slist_find (d2, prop1) != NULL)
	{
		return -1;
	}
	else
	{
		return 0;
	}
}

static void
cpg_object_clear_impl (CpgObject *object)
{
	GSList *props = g_slist_copy (object->priv->properties);
	GSList *item;

	props = g_slist_sort (props, (GCompareFunc)compare_property_dependencies);

	for (item = props; item; item = g_slist_next (item))
	{
		cpg_object_remove_property (object,
		                            cpg_property_get_name (item->data),
		                            NULL);
	}

	g_slist_free (props);

	cpg_object_taint (object);
}

static void
cpg_object_taint_impl (CpgObject *object)
{
	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	object->priv->compiled = FALSE;

	g_signal_emit (object, object_signals[TAINTED], 0);
}

static gboolean
cpg_object_equal_impl (CpgObject *first,
                       CpgObject *second)
{
	GType tfirst = G_TYPE_FROM_INSTANCE (first);
	GType tsecond = G_TYPE_FROM_INSTANCE (second);

	if (!(tfirst == tsecond || g_type_is_a (tfirst, tsecond)))
	{
		return FALSE;
	}

	/* Compare properties */
	GSList *prop1 = cpg_object_get_properties (first);
	GSList *prop2 = cpg_object_get_properties (second);

	gboolean ret = g_slist_length (prop1) == g_slist_length (prop2);
	g_slist_free (prop2);

	if (ret)
	{
		GSList *item = prop1;

		while (item)
		{
			CpgProperty *prop1 = item->data;
			CpgProperty *prop2 = cpg_object_get_property (second,
			                                              cpg_property_get_name (prop1));

			if (!prop2 || !cpg_property_equal (prop1, prop2))
			{
				ret = FALSE;
				break;
			}

			item = g_slist_next (item);
		}
	}

	g_slist_free (prop1);
	return ret;
}

static GType
cpg_object_get_copy_type_impl (CpgObject *object)
{
	return G_TYPE_FROM_INSTANCE (object);
}

static void
cpg_object_class_init (CpgObjectClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cpg_object_finalize;
	object_class->dispose = cpg_object_dispose;
	object_class->get_property = get_property;
	object_class->set_property = set_property;

	klass->reset = cpg_object_reset_impl;
	klass->foreach_expression = cpg_object_foreach_expression_impl;
	klass->copy = cpg_object_copy_impl;
	klass->compile = cpg_object_compile_impl;
	klass->apply_template = cpg_object_apply_template_impl;
	klass->unapply_template = cpg_object_unapply_template_impl;
	klass->equal = cpg_object_equal_impl;

	klass->get_property = cpg_object_get_property_impl;
	klass->get_properties = cpg_object_get_properties_impl;
	klass->has_property = cpg_object_has_property_impl;
	klass->remove_property = cpg_object_remove_property_impl;
	klass->verify_remove_property = cpg_object_verify_remove_property_impl;
	klass->add_property = cpg_object_add_property_impl;

	klass->clear = cpg_object_clear_impl;
	klass->taint = cpg_object_taint_impl;
	klass->get_copy_type = cpg_object_get_copy_type_impl;

	/**
	 * CpgObject:id:
	 *
	 * The #CpgObject id.
	 *
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_ID,
	                                 g_param_spec_string ("id",
	                                                      "ID",
	                                                      "The object's id",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CpgObject:parent:
	 *
	 * The #CpgObject parent.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_PARENT,
	                                 g_param_spec_object ("parent",
	                                                      "Parent",
	                                                      "Parent",
	                                                      CPG_TYPE_OBJECT,
	                                                      G_PARAM_READABLE));

	g_object_class_override_property (object_class,
	                                  PROP_X,
	                                  "x");

	g_object_class_override_property (object_class,
	                                  PROP_Y,
	                                  "y");

	g_object_class_override_property (object_class,
	                                  PROP_HAS_LOCATION,
	                                  "has-location");

	/**
	 * CpgObject:auto-imported:
	 *
	 * Set to %TRUE when the object was automatically imported.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_AUTO_IMPORTED,
	                                 g_param_spec_boolean ("auto-imported",
	                                                       "Auto Imported",
	                                                       "Auto imported",
	                                                       FALSE,
	                                                       G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	g_object_class_override_property (object_class,
	                                  PROP_USE_COUNT,
	                                  "use-count");

	g_object_class_override_property (object_class,
	                                  PROP_ANNOTATION,
	                                  "annotation");

	/**
	 * CpgObject::tainted:
	 * @object: a #CpgObject
	 *
	 * Emitted when the object is tainted
	 *
	 **/
	object_signals[TAINTED] =
		g_signal_new ("tainted",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               tainted),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CpgObject::compiled:
	 * @object: a #CpgObject
	 *
	 * Emitted when the object is compiled
	 *
	 **/
	object_signals[COMPILED] =
		g_signal_new ("compiled",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               compiled),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CpgObject::resetted:
	 * @object: a #CpgObject
	 *
	 * Emitted when the object is resetted
	 *
	 **/
	object_signals[RESETTED] =
		g_signal_new ("resetted",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               resetted),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CpgObject::copied:
	 * @object: a #CpgObject
	 * @copy: the copy
	 *
	 * Emitted when the object is copied
	 *
	 **/
	object_signals[COPIED] =
		g_signal_new ("copied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               copied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_OBJECT);

	/**
	 * CpgObject::verify-remove-property:
	 * @object: a #CpgObject
	 * @name: the property name
	 * @error: the error
	 *
	 * Emitted when a property is added to the object
	 *
	 * Returns: %TRUE if the property can be removed, %FALSE otherwise
	 *
	 **/
	object_signals[VERIFY_REMOVE_PROPERTY] =
		g_signal_new ("verify-remove-property",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass, verify_remove_property),
		              cpg_signal_accumulator_false_handled,
		              NULL,
		              cpg_marshal_BOOLEAN__STRING_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              G_TYPE_STRING,
		              G_TYPE_POINTER);

	/**
	 * CpgObject::property-added:
	 * @object: a #CpgObject
	 * @property: the added #CpgProperty
	 *
	 * Emitted when a property is added to the object
	 *
	 **/
	object_signals[PROPERTY_ADDED] =
		g_signal_new ("property-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               property_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_PROPERTY);

	/**
	 * CpgObject::property-removed:
	 * @object: a #CpgObject
	 * @property: the removed #CpgProperty
	 *
	 * Emitted when a property is removed from the object
	 *
	 **/
	object_signals[PROPERTY_REMOVED] =
		g_signal_new ("property-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               property_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_PROPERTY);

	/**
	 * CpgObject::property-changed:
	 * @object: a #CpgObject
	 * @property: the changed #CpgProperty
	 *
	 * Emitted when the expression of a property of the object has changed
	 *
	 **/
	object_signals[PROPERTY_CHANGED] =
		g_signal_new ("property-changed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               property_changed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_PROPERTY);

	/**
	 * CpgObject::template-applied:
	 * @object: a #CpgObject
	 * @templ: the applied #CpgObject
	 *
	 * Emitted when a template is applied to the object
	 *
	 **/
	object_signals[TEMPLATE_APPLIED] =
		g_signal_new ("template-applied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               template_applied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_OBJECT);

	/**
	 * CpgObject::template-unapplied:
	 * @object: a #CpgObject
	 * @templ: the unapplied #CpgObject
	 *
	 * Emitted when a template is unapplied from the object
	 *
	 **/
	object_signals[TEMPLATE_UNAPPLIED] =
		g_signal_new ("template-unapplied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgObjectClass,
		                               template_unapplied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_OBJECT);

	g_type_class_add_private (object_class, sizeof (CpgObjectPrivate));
}

static void
cpg_object_init (CpgObject *self)
{
	self->priv = CPG_OBJECT_GET_PRIVATE (self);

	self->priv->property_hash = g_hash_table_new_full (g_str_hash,
	                                                   g_str_equal,
	                                                   (GDestroyNotify)g_free,
	                                                   NULL);

	self->priv->tags = g_hash_table_new_full (g_str_hash,
	                                          g_str_equal,
	                                          (GDestroyNotify)g_free,
	                                          NULL);
}

/**
 * cpg_object_new:
 * @id: the object id
 *
 * Creates a new #CpgObject.
 *
 * Return value: the newly created #CpgObject
 *
 **/
CpgObject *
cpg_object_new (const gchar *id)
{
	return g_object_new (CPG_TYPE_OBJECT, "id", id, NULL);
}

/**
 * cpg_object_new_from_template:
 * @templ: A #CpgObject
 *
 * Create a new #CpgObject based on the template @templ.
 *
 * Returns: A #CpgObject
 *
 **/
CpgObject *
cpg_object_new_from_template (CpgObject  *templ,
                              GError    **error)
{
	CpgObject *obj = g_object_new (G_TYPE_FROM_INSTANCE (templ),
	                               "id", cpg_object_get_id (templ),
	                               NULL);

	if (!cpg_object_apply_template (obj, templ, error))
	{
		g_object_unref (obj);
		return NULL;
	}

	if (CPG_IS_LAYOUTABLE (templ) &&
	    cpg_layoutable_supports_location (CPG_LAYOUTABLE (templ)))
	{
		cpg_layoutable_get_location (CPG_LAYOUTABLE (templ),
		                             &obj->priv->x,
		                             &obj->priv->y);
	}

	return obj;
}

/**
 * cpg_object_add_property:
 * @object: the #CpgObject
 * @property: the #CpgProperty to add
 * @error: a #GError
 *
 * Add a new property to the object. Note that if a property with the same
 * name already exists, the property information is transfered to the existing
 * property instance. This means that the specified @property might not actually
 * be added to the object. Also, since a #CpgProperty is a #GInitiallyUnowned,
 * @property will be destroyed after the call to #cpg_object_add_property in
 * the above described case, unless you explicitly sink the floating reference.
 *
 * In the case that you can not know whether a property is overriding an
 * existing property in @object, never use @property after a call to
 * #cpg_object_add_property. Instead, retrieve the corresponding property
 * using #cpg_object_get_property after the call to #cpg_object_add_property.
 *
 * Returns: %TRUE if the property was added successfully, %FALSE otherwise
 **/
gboolean
cpg_object_add_property (CpgObject    *object,
                         CpgProperty  *property,
                         GError       **error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CPG_IS_PROPERTY (property), FALSE);
	g_return_val_if_fail (cpg_property_get_object (property) == NULL, FALSE);

	if (CPG_OBJECT_GET_CLASS (object)->add_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->add_property (object,
		                                                    property,
		                                                    error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_object_get_property:
 * @object: a #CpgObject
 * @name: a property name
 *
 * Get a #CpgProperty from the object by name
 *
 * Returns: (transfer none) (allow-none): the #CpgProperty with name @name, or
 *          %NULL if no such property could be found
 *
 **/
CpgProperty *
cpg_object_get_property (CpgObject    *object,
                         const gchar  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (CPG_OBJECT_GET_CLASS (object)->get_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->get_property (object,
		                                                    name);
	}
	else
	{
		return NULL;
	}
}

/**
 * cpg_object_has_property:
 * @object: a #CpgObject
 * @name: a property name
 *
 * Get whether @object has a property with name @name
 *
 * Returns: %TRUE if @object has a property with name @name, %FALSE otherwise
 *
 **/
gboolean
cpg_object_has_property (CpgObject    *object,
                         const gchar  *name)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return cpg_object_get_property (object, name) != NULL;
}

/**
 * cpg_object_remove_property:
 * @object: a #CpgObject
 * @name: a property name
 * @error: a #GError
 *
 * Remove the property @name from @object. If the property was not found or
 * could not be removed, @error will be appropriately set
 *
 * Returns: %TRUE if the property could be removed, %FALSE otherwise
 *
 **/
gboolean
cpg_object_remove_property (CpgObject    *object,
                            const gchar  *name,
                            GError      **error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	if (CPG_OBJECT_GET_CLASS (object)->remove_property)
	{
		return CPG_OBJECT_GET_CLASS (object)->remove_property (object,
		                                                       name,
		                                                       error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_object_verify_remove_property:
 * @object: a #CpgObject
 * @name: a property name
 * @error: a #GError
 *
 * Remove the property @name from @object. If the property was not found or
 * could not be removed, @error will be appropriately set
 *
 * Returns: %TRUE if the property could be removed, %FALSE otherwise
 *
 **/
gboolean
cpg_object_verify_remove_property (CpgObject    *object,
                                   const gchar  *name,
                                   GError      **error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	gboolean ret = FALSE;

	g_signal_emit (object,
	               object_signals[VERIFY_REMOVE_PROPERTY],
	               0,
	               name,
	               error,
	               &ret);

	return !ret;
}

/**
 * cpg_object_get_properties:
 * @object: a #CpgObject
 *
 * Gets the object properties.
 *
 * Returns: (element-type CpgProperty) (transfer container): a list of #CpgProperty.
 *
 **/
GSList *
cpg_object_get_properties (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	if (CPG_OBJECT_GET_CLASS (object)->get_properties)
	{
		return CPG_OBJECT_GET_CLASS (object)->get_properties (object);
	}
	else
	{
		return NULL;
	}
}

/**
 * _cpg_object_link:
 * @object: the #CpgObject
 * @link: the #CpgLink which links to this object
 *
 * Adds @link as a link which targets the object.
 *
 **/
void
_cpg_object_link (CpgObject  *object,
                  CpgLink    *link)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_LINK (link));

	object->priv->links = g_slist_append (object->priv->links, link);

	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	g_object_add_toggle_ref (G_OBJECT (link),
	                         (GToggleNotify)link_destroyed,
	                         object);
}

/**
 * _cpg_object_unlink:
 * @object: the #CpgObject
 * @link: the #CpgLink which unlinks from this object
 *
 * Removes @link as a link which targets the object.
 *
 **/
void
_cpg_object_unlink (CpgObject  *object,
                    CpgLink    *link)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_LINK (link));

	GSList *item = g_slist_find (object->priv->links, link);

	if (!item)
	{
		return;
	}

	object->priv->links = g_slist_remove_link (object->priv->links,
	                                           item);

	g_slist_free (object->priv->actors);
	object->priv->actors = NULL;

	g_object_remove_toggle_ref (G_OBJECT (link),
	                            (GToggleNotify)link_destroyed,
	                            object);
}

/**
 * cpg_object_get_actors:
 * @object: A #CpgObject
 *
 * Get the properties which are acted upon by links.
 *
 * Returns: (element-type CpgProperty) (transfer none): A #GSList of #CpgProperty.
 *
 **/
const GSList *
cpg_object_get_actors (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	if (object->priv->actors != NULL)
	{
		return object->priv->actors;
	}

	GSList *ret = NULL;
	GSList *item;

	for (item = object->priv->links; item; item = g_slist_next (item))
	{
		GSList const *actions;

		actions = cpg_link_get_actions (CPG_LINK (item->data));

		while (actions)
		{
			CpgLinkAction *a = actions->data;
			CpgProperty *target = cpg_link_action_get_target_property (a);

			if (!g_slist_find (ret, target))
			{
				ret = g_slist_prepend (ret, target);
			}

			actions = g_slist_next (actions);
		}
	}

	object->priv->actors = g_slist_reverse (ret);
	return object->priv->actors;
}

/**
 * cpg_object_reset:
 * @object: the #CpgObject
 *
 * Reset all properties to their initial values
 *
 **/
void
cpg_object_reset (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->reset)
	{
		CPG_OBJECT_GET_CLASS (object)->reset (object);
	}
}

/**
 * cpg_object_get_id:
 * @object: a #CpgObject
 *
 * Gets the object id
 *
 * Returns: (transfer none): the object id
 *
 **/
const gchar *
cpg_object_get_id (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->id;
}

/**
 * cpg_object_set_id:
 * @object: a #CpgObject
 * @id: the new object id
 *
 * Sets the object id
 *
 **/
void
cpg_object_set_id (CpgObject    *object,
                   const gchar  *id)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (id != NULL);

	set_id (object, id);
	g_object_notify (G_OBJECT (object), "id");
}

/**
 * cpg_object_get_links:
 * @object: A #CpgObject
 *
 * Get a list of links that act on this object.
 *
 * Returns: (element-type CpgLink): A list of #CpgLink
 *
 */
GSList const *
cpg_object_get_links (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->links;
}

/**
 * cpg_object_compile:
 * @object: A #CpgObject
 * @context: A #CpgCompileContext
 * @error: (type CpgCompileError): A #CpgCompileError
 *
 * Compile the object.
 *
 * Returns: %TRUE if the object compiled successfully, %FALSE otherwise. If the
 *          compilation failed and @error was set, the reason for the compile
 *          failure is set in @error.
 *
 **/
gboolean
cpg_object_compile (CpgObject         *object,
                    CpgCompileContext *context,
                    CpgCompileError   *error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	if (CPG_OBJECT_GET_CLASS (object)->compile)
	{
		return CPG_OBJECT_GET_CLASS (object)->compile (object,
		                                               context,
		                                               error);
	}

	return TRUE;
}

/**
 * cpg_object_taint:
 * @object: a #CpgObject
 *
 * Mark the object as tainted. This emits the "tainted" signal. The #CpgNetwork
 * in which the object is added acts on this signal to mark the network tainted
 * and as such the object will be properly recompiled when the network needs
 * to be simulated
 *
 **/
void
cpg_object_taint (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->taint)
	{
		CPG_OBJECT_GET_CLASS (object)->taint (object);
	}
}

/**
 * cpg_object_clear:
 * @object: A #CpgObject
 *
 * Clear all properties from the object.
 *
 **/
void
cpg_object_clear (CpgObject *object)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	if (CPG_OBJECT_GET_CLASS (object)->clear)
	{
		CPG_OBJECT_GET_CLASS (object)->clear (object);
	}
}

/**
 * cpg_object_equal:
 * @first: A #CpgObject
 * @second: A #CpgObject
 *
 * Check if two objects are equal.
 *
 * Returns: %TRUE if the objects are equal, %FALSE otherwise
 *
 **/
gboolean
cpg_object_equal (CpgObject *first,
                  CpgObject *second)
{
	g_return_val_if_fail (CPG_IS_OBJECT (first), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (second), FALSE);

	if (CPG_OBJECT_GET_CLASS (first)->equal)
	{
		return CPG_OBJECT_GET_CLASS (first)->equal (first, second);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cpg_object_get_applied_templates:
 * @object: A #CpgObject
 *
 * Get the list of applied templates.
 *
 * Returns: (element-type CpgObject) (transfer none): A #GSList of #CpgObject
 *
 **/
const GSList *
cpg_object_get_applied_templates (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->templates;
}

/**
 * cpg_object_get_template_applies_to:
 * @object: A #CpgObject
 *
 * Get the list objects that this object (as a template) applies to
 *
 * Returns: (element-type CpgObject) (transfer none): A #GSList of #CpgObject
 *
 **/
const GSList *
cpg_object_get_template_applies_to (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->templates_reverse_map;
}

/**
 * cpg_object_get_parent:
 * @object: A #CpgObject
 *
 * Get the parent of the object.
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_object_get_parent (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->parent;
}

/**
 * cpg_object_is_compiled:
 * @object: A #CpgObject
 *
 * Get whether the object is compiled.
 *
 * Returns: %TRUE if the object is compiled, %FALSE otherwise
 *
 **/
gboolean
cpg_object_is_compiled (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	return object->priv->compiled;
}

static gboolean
run_when_applied (CpgObject  *object,
                  CpgObject  *templ,
                  GError    **error)
{
	GSList *applied;

	applied = templ->priv->when_applied;

	while (applied)
	{
		if (!cpg_when_applied_run (applied->data,
		                           object,
		                           error))
		{
			return FALSE;
		}

		applied = g_slist_next (applied);
	}

	return TRUE;
}

/**
 * cpg_object_apply_template:
 * @object: A #CpgObject
 * @templ: The template
 * @error: The #GError
 *
 * Apply a template to the object. This will apply all of the characteristics
 * of the template to the object. Note that @object should be of the same type,
 * or inheriting from, the type of @templ.
 *
 * Returns: %TRUE if the template could be successfully applied,
 *          %FALSE otherwise
 **/
gboolean
cpg_object_apply_template (CpgObject  *object,
                           CpgObject  *templ,
                           GError    **error)
{
	gboolean ret;

	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (templ), FALSE);
	g_return_val_if_fail (g_type_is_a (G_TYPE_FROM_INSTANCE (object),
	                                   G_TYPE_FROM_INSTANCE (templ)), FALSE);

	if (g_slist_find (object->priv->templates, templ))
	{
		g_set_error (error,
		             CPG_OBJECT_ERROR,
		             CPG_OBJECT_ERROR_TEMPLATE_ALREADY_APPLIED,
		             "The template `%s' is already applied to `%s'",
		             cpg_object_get_id (object),
		             cpg_object_get_id (templ));

		return FALSE;
	}

	ret = CPG_OBJECT_GET_CLASS (object)->apply_template (object,
	                                                     templ,
	                                                     error);

	if (ret)
	{
		ret = run_when_applied (object, templ, error);
	}

	return ret;
}

/**
 * cpg_object_unapply_template:
 * @object: A #CpgObject
 * @templ: The template
 * @error: The #GError
 *
 * Unapply a template from the object.
 *
 * Returns: %TRUE if the template could be successfully unapplied,
 *          %FALSE otherwise
 *
 **/
gboolean
cpg_object_unapply_template (CpgObject  *object,
                             CpgObject  *templ,
                             GError    **error)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CPG_IS_OBJECT (templ), FALSE);

	if (!g_slist_find (object->priv->templates, templ))
	{
		g_set_error (error,
		             CPG_OBJECT_ERROR,
		             CPG_OBJECT_ERROR_TEMPLATE_NOT_FOUND,
		             "The template `%s' is not applied to `%s'",
		             cpg_object_get_id (object),
		             cpg_object_get_id (templ));

		return FALSE;
	}

	return CPG_OBJECT_GET_CLASS (object)->unapply_template (object,
	                                                        templ,
	                                                        error);
}

/**
 * cpg_object_get_property_template:
 * @object: A #CpgObject
 * @property: A #CpgProperty
 * @match_full: How to match the property
 *
 * Get the template on which @property is defined, if any. If @match_full is
 * %TRUE, the template will only be possitively matched if both properties are
 * equal (i.e. if a property originated from a template, but was later modified,
 * this function will not return the original template object).
 *
 * Returns: (transfer none): A #CpgObject
 *
 **/
CpgObject *
cpg_object_get_property_template (CpgObject   *object,
                                  CpgProperty *property,
                                  gboolean     match_full)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CPG_IS_PROPERTY (property), NULL);

	GSList *templates = g_slist_copy ((GSList *)cpg_object_get_applied_templates (object));
	templates = g_slist_reverse (templates);
	GSList *item;

	gchar const *name = cpg_property_get_name (property);

	for (item = templates; item; item = g_slist_next (item))
	{
		CpgProperty *tprop;
		CpgObject *templ = item->data;

		tprop = cpg_object_get_property (templ, name);

		if (tprop && (!match_full || cpg_property_equal (property, tprop)))
		{
			g_slist_free (templates);
			return templ;
		}
	}

	g_slist_free (templates);

	return NULL;
}

/**
 * cpg_object_copy:
 * @object: The source object
 *
 * Create a copy of the given object. This will create a new object with the
 * same id and with a copy of all the properties defined on the object.
 * The copied object will not have the same links, nor will it have a parent.
 * See the documentation of specific subclasses of #CpgObject to see the
 * copy semantics for those classes.
 *
 * Returns: (transfer full): A #CpgObject
 *
 **/
CpgObject *
cpg_object_copy (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	GType gtype;

	if (CPG_OBJECT_GET_CLASS (object)->get_copy_type)
	{
		gtype = CPG_OBJECT_GET_CLASS (object)->get_copy_type (object);
	}
	else
	{
		gtype = G_TYPE_FROM_INSTANCE (object);
	}

	CpgObject *ret = g_object_new (gtype,
	                               "id", cpg_object_get_id (object),
	                               NULL);

	if (CPG_OBJECT_GET_CLASS (ret)->copy)
	{
		CPG_OBJECT_GET_CLASS (ret)->copy (ret, object);
	}

	g_signal_emit (object, object_signals[COPIED], 0, ret);

	return ret;
}

/**
 * cpg_object_get_auto_imported:
 * @object: A #CpgObject
 *
 * Get whether the object was automatically imported.
 *
 * Returns: %TRUE if the object was automatically imported, %FALSE otherwise
 *
 **/
gboolean
cpg_object_get_auto_imported (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), FALSE);

	return object->priv->auto_imported;
}

/**
 * cpg_object_set_auto_imported:
 * @object: A #CpgObject
 * @auto_imported: a boolean
 *
 * Set whether the object was automatically imported.
 *
 **/
void
cpg_object_set_auto_imported (CpgObject *object,
                              gboolean   auto_imported)
{
	g_return_if_fail (CPG_IS_OBJECT (object));

	object->priv->auto_imported = auto_imported;
}

void
_cpg_object_set_parent (CpgObject *object,
                        CpgObject *parent)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (parent == NULL || CPG_IS_OBJECT (parent));

	object->priv->parent = parent;

	g_object_notify (G_OBJECT (object), "parent");
}

/**
 * cpg_object_get_full_id:
 * @object: A #CpgObject
 *
 * Get the full id of the object. This is the id that can be used in the outer
 * most parent to refer to this object (i.e.
 * <code>cpg_group_find_object (top_parent, cpg_object_get_full_id (deep_child)) == deep_child</code>)
 *
 * Returns: The full id of the object. This is a newly allocated string that
 *          should be freed with g_free.
 *
 **/
gchar *
cpg_object_get_full_id (CpgObject *object)
{
	CpgObject *parent;

	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	parent = cpg_object_get_parent (object);

	if (!parent ||
	    (CPG_IS_NETWORK (parent) && object == CPG_OBJECT (cpg_network_get_template_group (CPG_NETWORK (parent)))))
	{
		return cpg_selector_escape_identifier (object->priv->id);
	}

	while (parent->priv->parent)
	{
		if ((CPG_IS_NETWORK (parent->priv->parent) && parent == CPG_OBJECT (cpg_network_get_template_group (CPG_NETWORK (parent->priv->parent)))))
		{
			break;
		}

		parent = parent->priv->parent;
	}

	return cpg_object_get_relative_id (object, parent);
}

gchar *
cpg_object_get_full_id_for_display (CpgObject *object)
{
	CpgObject *parent;

	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	parent = cpg_object_get_parent (object);

	if (!parent ||
	    (CPG_IS_NETWORK (parent) && object == CPG_OBJECT (cpg_network_get_template_group (CPG_NETWORK (parent)))))
	{
		return g_strdup (object->priv->id);
	}

	while (parent->priv->parent)
	{
		if ((CPG_IS_NETWORK (parent->priv->parent) && parent == CPG_OBJECT (cpg_network_get_template_group (CPG_NETWORK (parent->priv->parent)))))
		{
			break;
		}

		parent = parent->priv->parent;
	}

	return cpg_object_get_relative_id_for_display (object, parent);
}

/**
 * cpg_object_foreach_expression:
 * @object: A #CpgObject
 * @func: (scope call): A #CpgForeachExpressionFunc
 * @userdata: The userdata that will be supplied to @func
 *
 * Iterate over each expression defined in the object. For normal objects, this
 * means all the property expressions. Classes that inherit from #CpgObject
 * can implement their own iteration for additional expressions (see for
 * example #CpgGroup).
 *
 **/
void
cpg_object_foreach_expression (CpgObject                *object,
                               CpgForeachExpressionFunc  func,
                               gpointer                  userdata)
{
	/* Omit type check to increase speed */
	if (!func)
	{
		return;
	}

	CPG_OBJECT_GET_CLASS (object)->foreach_expression (object, func, userdata);
}

static gchar *
get_relative_id (CpgObject *object,
                 CpgObject *parent,
                 gboolean   for_display)
{
	gchar *ret;
	gchar *par = NULL;

	if (object == parent)
	{
		return g_strdup ("");
	}

	if (cpg_object_get_parent (object) == NULL)
	{
		return for_display ?
		       g_strdup (object->priv->id) :
		       cpg_selector_escape_identifier (object->priv->id);
	}

	if (object->priv->parent)
	{
		if (for_display)
		{
			par = cpg_object_get_relative_id_for_display (object->priv->parent, parent);
		}
		else
		{
			par = cpg_object_get_relative_id (object->priv->parent, parent);
		}
	}

	if (par && *par)
	{
		gchar *esc;

		if (for_display)
		{
			esc = g_strdup (object->priv->id);
		}
		else
		{
			esc = cpg_selector_escape_identifier (object->priv->id);
		}

		ret = g_strconcat (par, ".", esc, NULL);
		g_free (esc);
	}
	else
	{
		if (for_display)
		{
			ret = g_strdup (object->priv->id);
		}
		else
		{
			ret = cpg_selector_escape_identifier (object->priv->id);
		}
	}

	g_free (par);
	return ret;
}

gchar *
cpg_object_get_relative_id (CpgObject *object,
                            CpgObject *parent)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);

	return get_relative_id (object, parent, FALSE);
}

gchar *
cpg_object_get_relative_id_for_display (CpgObject *object,
                                        CpgObject *parent)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CPG_IS_OBJECT (parent), NULL);

	return get_relative_id (object, parent, TRUE);
}

void
cpg_object_add_when_applied (CpgObject      *object,
                             CpgWhenApplied *applied)
{
	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_WHEN_APPLIED (applied));

	if (g_slist_find (object->priv->when_applied, applied))
	{
		return;
	}

	object->priv->when_applied =
		g_slist_append (object->priv->when_applied,
		                g_object_ref (applied));
}

void
cpg_object_remove_when_applied (CpgObject      *object,
                                CpgWhenApplied *applied)
{
	GSList *item;

	g_return_if_fail (CPG_IS_OBJECT (object));
	g_return_if_fail (CPG_IS_WHEN_APPLIED (applied));

	item = g_slist_find (object->priv->when_applied, applied);

	if (item)
	{
		g_object_unref (item->data);

		object->priv->when_applied =
			g_slist_delete_link (object->priv->when_applied,
			                     item);
	}
}

/**
 * cpg_object_get_when_applied:
 * @object: A #CpgObject
 *
 * Get the list of #CpgWhenApplied which are run when the object is applied
 * as a template.
 *
 * Returns: (element-type CpgWhenApplied): A #GSList
 *
 **/
GSList const *
cpg_object_get_when_applied (CpgObject *object)
{
	g_return_val_if_fail (CPG_IS_OBJECT (object), NULL);

	return object->priv->when_applied;
}
