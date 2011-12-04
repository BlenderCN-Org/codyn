/*
 * cdn-object.c
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

#include <stdlib.h>
#include <string.h>

#include "cdn-object.h"

#include "cdn-edge.h"
#include "cdn-expression.h"

#include "cdn-compile-error.h"
#include "cdn-utils.h"
#include "cdn-marshal.h"
#include "cdn-usable.h"
#include "cdn-tokenizer.h"
#include "cdn-annotatable.h"
#include "cdn-layoutable.h"
#include "cdn-selector.h"
#include "cdn-taggable.h"
#include "cdn-node.h"

/**
 * SECTION:cdn-object
 * @short_description: Basis for all cdn objects
 *
 * #CdnObject is a base class for all the objects which can be added to a
 * network. It provides property storage and virtual methods which can be
 * implemented that drive the simulation process.
 *
 */

/**
 * CdnForeachExpressionFunc:
 * @expression: a #CdnExpression
 * @userdata: user data
 *
 * A function callback called on each expression found in an object.
 *
 **/

#define CDN_OBJECT_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CDN_TYPE_OBJECT, CdnObjectPrivate))

struct _CdnObjectPrivate
{
	guint use_count;
	gchar *id;

	gint x;
	gint y;

	CdnNode *parent;

	/* Properties */
	GSList *variables;
	GHashTable *property_hash;

	/* Templates */
	GSList *templates;
	GSList *templates_reverse_map;

	gchar *annotation;
	GHashTable *tags;

	GSList *events;
	CdnEvent *last_event;

	guint compiled : 1;
	guint auto_imported : 1;
	guint has_location : 1;
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
	VARIABLE_ADDED,
	VARIABLE_REMOVED,
	VARIABLE_CHANGED,
	COPIED,
	VERIFY_REMOVE_VARIABLE,
	TEMPLATE_APPLIED,
	TEMPLATE_UNAPPLIED,
	NUM_SIGNALS
};

static void cdn_usable_iface_init (gpointer iface);
static void cdn_annotatable_iface_init (gpointer iface);
static void cdn_layoutable_iface_init (gpointer iface);
static void cdn_taggable_iface_init (gpointer iface);

G_DEFINE_ABSTRACT_TYPE_WITH_CODE (CdnObject,
                                  cdn_object,
                                  G_TYPE_OBJECT,
                                  G_IMPLEMENT_INTERFACE (CDN_TYPE_USABLE,
                                                         cdn_usable_iface_init);
                                  G_IMPLEMENT_INTERFACE (CDN_TYPE_ANNOTATABLE,
                                                         cdn_annotatable_iface_init);
                                  G_IMPLEMENT_INTERFACE (CDN_TYPE_LAYOUTABLE,
                                                         cdn_layoutable_iface_init);
                                  G_IMPLEMENT_INTERFACE (CDN_TYPE_TAGGABLE,
                                                         cdn_taggable_iface_init));

static guint object_signals[NUM_SIGNALS] = {0,};

static GHashTable *
get_tag_table (CdnTaggable *taggable)
{
	return CDN_OBJECT (taggable)->priv->tags;
}

static void
cdn_taggable_iface_init (gpointer iface)
{
	/* Use default implementation */
	CdnTaggableInterface *taggable = iface;

	taggable->get_tag_table = get_tag_table;
}

GQuark
cdn_object_error_quark (void)
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_object_error");
	}

	return quark;
}

static void
cdn_object_use (CdnUsable *usable)
{
	CdnObject *obj = CDN_OBJECT (usable);
	++obj->priv->use_count;
}

static gboolean
cdn_object_unuse (CdnUsable *usable)
{
	CdnObject *obj = CDN_OBJECT (usable);

	if (obj->priv->use_count == 0)
	{
		return TRUE;
	}

	return (--(obj->priv->use_count) == 0);
}

static void
cdn_layoutable_iface_init (gpointer iface)
{
}

static void
cdn_usable_iface_init (gpointer iface)
{
	CdnUsableInterface *usable = iface;

	usable->use = cdn_object_use;
	usable->unuse = cdn_object_unuse;
}

static gchar *
cdn_object_annotatable_get_title (CdnAnnotatable *annotatable)
{
	CdnObject *obj;

	obj = CDN_OBJECT (annotatable);

	return cdn_object_get_full_id_for_display (obj);
}

static void
cdn_annotatable_iface_init (gpointer iface)
{
	CdnAnnotatableInterface *annotatable = iface;

	annotatable->get_title = cdn_object_annotatable_get_title;
}

static void
free_property (CdnVariable *property,
               CdnObject   *object)
{
	cdn_usable_unuse (CDN_USABLE (property));

	g_signal_handlers_disconnect_by_func (property,
	                                      cdn_object_taint,
	                                      object);

	g_object_unref (property);
}

static void
cdn_object_finalize (GObject *object)
{
	CdnObject *obj = CDN_OBJECT (object);

	g_slist_foreach (obj->priv->variables, (GFunc)free_property, object);
	g_slist_free (obj->priv->variables);

	g_free (obj->priv->id);

	g_free (obj->priv->annotation);

	g_hash_table_destroy (obj->priv->property_hash);
	g_hash_table_destroy (obj->priv->tags);

	g_slist_foreach (obj->priv->events, (GFunc)g_object_unref, NULL);
	g_slist_free (obj->priv->events);

	G_OBJECT_CLASS (cdn_object_parent_class)->finalize (object);
}

static void
reset_expression (CdnExpression *expression)
{
	cdn_expression_reset (expression);
}

/* interface implementations */
static void
cdn_object_reset_impl (CdnObject *object)
{
	cdn_object_foreach_expression (object,
	                               (CdnForeachExpressionFunc)reset_expression,
	                               NULL);

	g_slist_foreach (object->priv->variables, (GFunc)cdn_variable_reset, NULL);

	cdn_object_taint (object);

	g_signal_emit (object, object_signals[RESETTED], 0);
}

static void
set_id (CdnObject   *object,
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
	CdnObject *obj = CDN_OBJECT (object);

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
	CdnObject *obj = CDN_OBJECT (object);

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
on_template_property_expression_changed (CdnVariable *prop,
                                         GParamSpec  *spec,
                                         CdnObject   *object)
{
	CdnVariable *orig = cdn_object_get_variable (object,
	                                             cdn_variable_get_name (prop));

	if (!orig)
	{
		return;
	}

	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (orig)))
	{
		return;
	}

	CdnObject *templ = cdn_object_get_variable_template (object, orig, FALSE);

	if (templ != cdn_variable_get_object (prop))
	{
		return;
	}

	cdn_variable_set_expression (orig,
	                             cdn_expression_copy (cdn_variable_get_expression (prop)));

	cdn_modifiable_set_modified (CDN_MODIFIABLE (orig), FALSE);
}

static void
on_template_property_flags_changed (CdnVariable *prop,
                                    GParamSpec  *spec,
                                    CdnObject   *object)
{
	/* Check if the current prop had the same, if so, also change
	   it here */
	CdnVariable *orig = cdn_object_get_variable (object,
	                                             cdn_variable_get_name (prop));

	if (!orig)
	{
		return;
	}

	if (cdn_modifiable_get_modified (CDN_MODIFIABLE (orig)))
	{
		return;
	}

	CdnObject *templ = cdn_object_get_variable_template (object, orig, FALSE);

	if (templ != cdn_variable_get_object (prop))
	{
		return;
	}

	cdn_variable_set_flags (orig, cdn_variable_get_flags (prop));
	cdn_modifiable_set_modified (CDN_MODIFIABLE (orig), FALSE);
}

static void
on_template_variable_added (CdnObject   *templ,
                            CdnVariable *prop,
                            CdnObject   *object)
{
	CdnVariable *df;

	CdnVariable *orig =
		cdn_object_get_variable (object,
		                         cdn_variable_get_name (prop));

	if (orig == NULL ||
	    cdn_object_get_variable_template (object, orig, TRUE))
	{
		if (cdn_object_add_variable (object,
		                             cdn_variable_copy (prop),
		                             NULL))
		{
			orig = cdn_object_get_variable (object,
			                                cdn_variable_get_name (prop));

			cdn_modifiable_set_modified (CDN_MODIFIABLE (orig), FALSE);
		}
		else
		{
			return;
		}
	}

	df = cdn_variable_get_derivative (prop);

	if (df)
	{
		cdn_variable_set_derivative (cdn_object_get_variable (object,
		                                                      cdn_variable_get_name (prop)),
		                             cdn_object_get_variable (object,
		                                                      cdn_variable_get_name (df)));
	}

	df = cdn_variable_get_integral (prop);

	if (df)
	{
		cdn_variable_set_derivative (cdn_object_get_variable (object,
		                                                      cdn_variable_get_name (df)),
		                             cdn_object_get_variable (object,
		                                                      cdn_variable_get_name (prop)));
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
disconnect_template_property (CdnObject   *object,
                              CdnObject   *templ,
                              CdnVariable *prop)
{
	g_signal_handlers_disconnect_by_func (prop,
	                                      on_template_property_expression_changed,
	                                      object);

	g_signal_handlers_disconnect_by_func (prop,
	                                      on_template_property_flags_changed,
	                                      object);
}

static void
on_template_variable_removed (CdnObject   *templ,
                              CdnVariable *prop,
                              CdnObject   *object)
{
	gchar const *name = cdn_variable_get_name (prop);
	CdnVariable *orig = cdn_object_get_variable (object, name);

	if (orig && !cdn_modifiable_get_modified (CDN_MODIFIABLE (orig)))
	{
		CdnObject *temp = cdn_object_get_variable_template (object, orig, FALSE);

		if (temp == NULL)
		{
			/* Remove the original property as well */
			cdn_object_remove_variable (object,
			                            cdn_variable_get_name (orig),
			                            NULL);
		}
		else
		{
			/* Then, reupdate the property value from the other
			   template */
			CdnVariable *tempProp = cdn_object_get_variable (temp,
			                                                 name);

			cdn_object_add_variable (object,
			                         cdn_variable_copy (tempProp),
			                         NULL);
		}
	}

	disconnect_template_property (object, templ, prop);
}

static void
disconnect_template (CdnObject *object,
                     CdnObject *templ,
                     gboolean   disconnect_variables)
{
	if (disconnect_variables)
	{
		GSList *item;

		for (item = templ->priv->variables; item; item = g_slist_next (item))
		{
			disconnect_template_property (object, templ, item->data);
		}
	}

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_variable_added,
	                                      object);

	g_signal_handlers_disconnect_by_func (templ,
	                                      on_template_variable_removed,
	                                      object);

	cdn_usable_unuse (CDN_USABLE (templ));
	g_object_unref (templ);
}

static void
cdn_object_dispose (GObject *object)
{
	CdnObject *obj = CDN_OBJECT (object);

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

	G_OBJECT_CLASS (cdn_object_parent_class)->dispose (object);
}

static void
cdn_object_foreach_expression_impl (CdnObject                *object,
                                    CdnForeachExpressionFunc  func,
                                    gpointer                  userdata)

{
	GSList *item;

	for (item = object->priv->variables; item; item = g_slist_next (item))
	{
		CdnExpression *cons;

		func (cdn_variable_get_expression (item->data), userdata);

		cons = cdn_variable_get_constraint (item->data);

		if (cons)
		{
			func (cons, userdata);
		}
	}
}

static void
check_modified_for_template (CdnObject   *object,
                             CdnVariable *property)
{
	CdnObject *templ;

	templ = cdn_object_get_variable_template (object,
	                                          property,
	                                          TRUE);

	if (templ != NULL)
	{
		cdn_modifiable_set_modified (CDN_MODIFIABLE (property), FALSE);
	}
}

static void
on_variable_modified (CdnObject   *object,
                      GParamSpec  *spec,
                      CdnVariable *property)
{
	check_modified_for_template (object, property);

	cdn_object_taint (object);
}

static gboolean
on_variable_invalidate_name (CdnVariable *property,
                             gchar const *name,
                             CdnObject   *object)
{
	CdnVariable *other = cdn_object_get_variable (object, name);

	return other && other != property;
}

static void
add_variable (CdnObject   *object,
              CdnVariable *property)
{
	g_object_ref_sink (property);

	object->priv->variables = g_slist_append (object->priv->variables,
	                                           property);

	g_hash_table_insert (object->priv->property_hash,
	                     g_strdup (cdn_variable_get_name (property)),
	                     property);

	_cdn_variable_set_object (property, object, object->priv->use_count > 1);

	cdn_usable_use (CDN_USABLE (property));
	cdn_object_taint (object);

	g_signal_connect_swapped (property,
	                          "notify::modified",
	                          G_CALLBACK (on_variable_modified),
	                          object);

	g_signal_connect (property,
	                  "invalidate-name",
	                  G_CALLBACK (on_variable_invalidate_name),
	                  object);

	g_signal_emit (object, object_signals[VARIABLE_ADDED], 0, property);
}

static void
cdn_object_copy_impl (CdnObject *object,
                      CdnObject *source)
{
	/* Copy variables */
	GSList *item;
	gchar *annotation;

	for (item = source->priv->variables; item; item = g_slist_next (item))
	{
		CdnVariable *prop = item->data;

		add_variable (object, cdn_variable_copy (prop));
	}

	for (item = source->priv->variables; item; item = g_slist_next (item))
	{
		CdnVariable *prop = item->data;
		CdnVariable *df;

		df = cdn_variable_get_derivative (prop);

		if (df)
		{
			cdn_variable_set_derivative (cdn_object_get_variable (object, cdn_variable_get_name (prop)),
			                             cdn_object_get_variable (object, cdn_variable_get_name (df)));
		}
	}

	object->priv->templates = g_slist_copy (source->priv->templates);

	for (item = object->priv->templates; item; item = g_slist_next (item))
	{
		cdn_usable_use (item->data);
		g_object_ref (item->data);
	}

	object->priv->templates_reverse_map =
		g_slist_copy (object->priv->templates_reverse_map);

	annotation = cdn_annotatable_get_annotation (CDN_ANNOTATABLE (source));
	cdn_annotatable_set_annotation (CDN_ANNOTATABLE (object),
	                                annotation);

	if (cdn_layoutable_supports_location (CDN_LAYOUTABLE (source)) &&
	    cdn_layoutable_supports_location (CDN_LAYOUTABLE (object)))
	{
		gint x;
		gint y;

		cdn_layoutable_get_location (CDN_LAYOUTABLE (source), &x, &y);
		cdn_layoutable_set_location (CDN_LAYOUTABLE (object), x, y);
	}

	cdn_taggable_copy_to (CDN_TAGGABLE (source),
	                      object->priv->tags);

	g_free (annotation);
}

static void
remove_template_tag (CdnTaggable *templ,
                     gchar const *key,
                     gchar const *value,
                     CdnTaggable *object)
{
	GSList const *tt;

	if (g_strcmp0 (cdn_taggable_get_tag (object, key),
	               value) != 0)
	{
		return;
	}

	cdn_taggable_remove_tag (object, key);

	for (tt = cdn_object_get_applied_templates (CDN_OBJECT (object));
	     tt;
	     tt = g_slist_next (tt))
	{
		gchar const *oval;

		if (cdn_taggable_try_get_tag (tt->data, key, &oval))
		{
			cdn_taggable_add_tag (object,
			                      key,
			                      oval);
		}
	}
}

static gboolean
cdn_object_unapply_template_impl (CdnObject  *object,
                                  CdnObject  *templ,
                                  GError    **error)
{
	GSList *item;

	object->priv->templates = g_slist_remove (object->priv->templates,
	                                          templ);

	templ->priv->templates_reverse_map =
		g_slist_remove (templ->priv->templates_reverse_map,
		                object);

	for (item = templ->priv->variables; item; item = g_slist_next (item))
	{
		on_template_variable_removed (templ, item->data, object);
	}

	cdn_taggable_foreach (CDN_TAGGABLE (templ),
	                      (CdnTaggableForeachFunc)remove_template_tag,
	                      object);

	/* Keep the template around for the signal emission */
	g_object_ref (templ);
	cdn_usable_use (CDN_USABLE (templ));

	disconnect_template (object, templ, FALSE);

	g_signal_emit (object, object_signals[TEMPLATE_UNAPPLIED], 0, templ);

	cdn_usable_unuse (CDN_USABLE (templ));
	g_object_unref (templ);

	return TRUE;
}

static void
foreach_tag_copy (CdnTaggable *source,
                  gchar const *key,
                  gchar const *value,
                  CdnTaggable *target)
{
	cdn_taggable_add_tag (target, key, value);
}

static gboolean
cdn_object_apply_template_impl (CdnObject  *object,
                                CdnObject  *templ,
                                GError    **error)
{
	/* Apply variables */
	GSList *item;

	for (item = templ->priv->variables; item; item = g_slist_next (item))
	{
		on_template_variable_added (templ, item->data, object);
	}

	cdn_taggable_foreach (CDN_TAGGABLE (templ),
	                      (CdnTaggableForeachFunc)foreach_tag_copy,
	                      object);

	g_signal_connect (templ,
	                  "variable-added",
	                  G_CALLBACK (on_template_variable_added),
	                  object);

	g_signal_connect (templ,
	                  "variable-removed",
	                  G_CALLBACK (on_template_variable_removed),
	                  object);

	object->priv->templates = g_slist_append (object->priv->templates,
	                                          g_object_ref (templ));

	templ->priv->templates_reverse_map =
		g_slist_append (templ->priv->templates_reverse_map,
		                object);

	cdn_usable_use (CDN_USABLE (templ));

	g_signal_emit (object, object_signals[TEMPLATE_APPLIED], 0, templ);

	return TRUE;
}

static CdnCompileContext *
cdn_object_get_compile_context_impl (CdnObject         *object,
                                     CdnCompileContext *context)
{
	if (!context)
	{
		if (object->priv->parent)
		{
			context = cdn_object_get_compile_context (CDN_OBJECT (object->priv->parent),
			                                          NULL);
		}
		else
		{
			context = cdn_compile_context_new ();
		}
	}

	cdn_compile_context_prepend_object (context, object);
	return context;
}

static gboolean
cdn_object_compile_impl (CdnObject         *object,
                         CdnCompileContext *context,
                         CdnCompileError   *error)
{
	if (cdn_object_is_compiled (object))
	{
		/* Don't recompile if not necessary */
		return TRUE;
	}

	/* Compile all the property expressions */
	GSList *variables = object->priv->variables;
	gboolean ret = TRUE;

	/* Prepend the object in the context */
	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	context = cdn_object_get_compile_context_impl (object, context);

	while (variables)
	{
		CdnVariable *property = (CdnVariable *)variables->data;
		CdnExpression *expr = cdn_variable_get_expression (property);
		CdnExpression *cons;

		if (!cdn_expression_compile (expr,
		                             context,
		                             error))
		{
			if (error)
			{
				cdn_compile_error_set (error,
				                       NULL,
				                       object,
				                       property,
				                       NULL,
				                       NULL);
			}

			ret = FALSE;
			break;
		}

		cons = cdn_variable_get_constraint (property);

		if (cons && !cdn_expression_compile (cons, context, error))
		{
			if (error)
			{
				cdn_compile_error_set (error,
				                       NULL,
				                       object,
				                       property,
				                       NULL,
				                       NULL);
			}

			ret = FALSE;
			break;

		}

		if (cdn_expression_depends_on (expr,
		                               cdn_variable_get_expression (property)))
		{
			if (error)
			{
				GError *gerror = NULL;

				gerror = g_error_new (CDN_COMPILE_ERROR_TYPE,
				                      CDN_COMPILE_ERROR_VARIABLE_RECURSE,
				                      "Infinite recursion in property expression");

				cdn_compile_error_set (error,
				                       gerror,
				                       object,
				                       property,
				                       NULL,
				                       expr);

				g_error_free (gerror);
				ret = FALSE;
				break;
			}
		}

		variables = g_slist_next (variables);
	}

	if (ret)
	{
		GSList *event;

		for (event = object->priv->events; event; event = g_slist_next (event))
		{
			if (!cdn_event_compile (event->data, context, error))
			{
				if (error)
				{
					cdn_compile_error_set (error,
					                       NULL,
					                       object,
					                       NULL,
					                       NULL,
					                       NULL);
				}

				ret = FALSE;
				break;
			}
		}
	}

	object->priv->compiled = ret;

	if (ret)
	{
		cdn_object_foreach_expression (object,
		                               (CdnForeachExpressionFunc)cdn_expression_reset_cache,
		                               NULL);

		g_signal_emit (object, object_signals[COMPILED], 0);
	}

	cdn_compile_context_restore (context);
	g_object_unref (context);

	return ret;
}

static GSList *
cdn_object_get_variables_impl (CdnObject *object)
{
	return g_slist_copy (object->priv->variables);
}

static CdnVariable *
cdn_object_get_variable_impl (CdnObject   *object,
                              const gchar *name)
{
	return g_hash_table_lookup (object->priv->property_hash, name);
}

static gboolean
cdn_object_has_variable_impl (CdnObject *object,
                              const gchar *name)
{
	return cdn_object_get_variable (object, name) != NULL;
}

static gboolean
remove_variable (CdnObject   *object,
                 CdnVariable *property,
                 gboolean     check_unuse)
{
	if (!cdn_usable_unuse (CDN_USABLE (property)) && check_unuse)
	{
		cdn_usable_use (CDN_USABLE (property));
		return FALSE;
	}

	object->priv->variables = g_slist_remove (object->priv->variables,
	                                           property);

	_cdn_variable_set_object (property, NULL, object->priv->use_count > 1);

	g_hash_table_remove (object->priv->property_hash,
	                     cdn_variable_get_name (property));

	g_signal_handlers_disconnect_by_func (property,
	                                      on_variable_invalidate_name,
	                                      object);

	g_signal_handlers_disconnect_by_func (property,
	                                      on_variable_modified,
	                                      object);

	g_signal_emit (object,
	               object_signals[VARIABLE_REMOVED],
	               0,
	               property);

	g_object_unref (property);

	cdn_object_taint (object);
	return TRUE;
}

static gboolean
cdn_object_verify_remove_variable_impl (CdnObject    *object,
                                        const gchar  *name,
                                        GError      **error)
{
	if (error)
	{
		*error = NULL;
	}

	CdnVariable *property = cdn_object_get_variable (object, name);

	if (property)
	{
		/* Check if the property is still used */
		if (cdn_usable_use_count (CDN_USABLE (property)) > 1)
		{
			if (error)
			{
				g_set_error (error,
				             CDN_OBJECT_ERROR,
				             CDN_OBJECT_ERROR_VARIABLE_IN_USE,
				             "Property `%s' is still in use and can not be removed",
				             name);
			}

			return FALSE;
		}

		/* Check if the property is instantiated from a template */
		CdnObject *temp;
		temp = cdn_object_get_variable_template (object, property, FALSE);

		if (temp != NULL)
		{
			if (error)
			{
				g_set_error (error,
				             CDN_OBJECT_ERROR,
				             CDN_OBJECT_ERROR_VARIABLE_FROM_TEMPLATE,
				             "The property `%s' is instantiated from the template `%s' and cannot be removed",
				             name,
				             cdn_object_get_id (temp));
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
			             CDN_OBJECT_ERROR,
			             CDN_OBJECT_ERROR_VARIABLE_NOT_FOUND,
			             "Property %s could not be found for %s",
			             name,
			             cdn_object_get_id (object));
		}

		return FALSE;
	}

	return TRUE;
}

static gboolean
cdn_object_remove_variable_impl (CdnObject    *object,
                                 const gchar  *name,
                                 GError      **error)
{
	if (!cdn_object_verify_remove_variable (object, name, error))
	{
		return FALSE;
	}

	CdnVariable *property = cdn_object_get_variable (object, name);

	remove_variable (object, property, FALSE);
	return TRUE;
}

static gboolean
cdn_object_add_variable_impl (CdnObject    *object,
                              CdnVariable  *property,
                              GError      **error)
{
	// Check if property already set
	CdnVariable *existing;

	existing = cdn_object_get_variable (object,
	                                    cdn_variable_get_name (property));

	if (existing)
	{
		cdn_variable_set_expression (existing,
		                             cdn_variable_get_expression (property));

		cdn_variable_set_flags (existing,
		                        cdn_variable_get_flags (property));

		if (g_object_is_floating (G_OBJECT (property)))
		{
			g_object_unref (property);
		}

		return TRUE;
	}

	if (!cdn_tokenizer_validate_identifier (cdn_variable_get_name (property)))
	{
		gchar *nm;

		nm = cdn_object_get_full_id (object);

		g_set_error (error,
		             CDN_OBJECT_ERROR,
		             CDN_OBJECT_ERROR_INVALID_VARIABLE_NAME,
		             "Invalid property name `%s.%s'",
		             nm,
		             cdn_variable_get_name (property));

		g_free (nm);

		return FALSE;
	}

	add_variable (object, property);
	return TRUE;
}

static gint
compare_property_dependencies (CdnVariable *prop1,
                               CdnVariable *prop2)
{
	CdnExpression *e1 = cdn_variable_get_expression (prop1);
	CdnExpression *e2 = cdn_variable_get_expression (prop2);

	if (cdn_expression_depends_on (e1, e2))
	{
		return -1;
	}
	else if (cdn_expression_depends_on (e2, e1))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

static void
cdn_object_clear_impl (CdnObject *object)
{
	GSList *props = g_slist_copy (object->priv->variables);
	GSList *item;

	props = g_slist_sort (props, (GCompareFunc)compare_property_dependencies);

	for (item = props; item; item = g_slist_next (item))
	{
		cdn_object_remove_variable (object,
		                            cdn_variable_get_name (item->data),
		                            NULL);
	}

	g_slist_free (props);

	cdn_object_taint (object);
}

static void
cdn_object_taint_impl (CdnObject *object)
{
	if (object->priv->compiled)
	{
		object->priv->compiled = FALSE;
		g_signal_emit (object, object_signals[TAINTED], 0);
	}
}

static gboolean
cdn_object_equal_impl (CdnObject *first,
                       CdnObject *second)
{
	GType tfirst = G_TYPE_FROM_INSTANCE (first);
	GType tsecond = G_TYPE_FROM_INSTANCE (second);

	if (!(tfirst == tsecond || g_type_is_a (tfirst, tsecond)))
	{
		return FALSE;
	}

	/* Compare variables */
	GSList *prop1 = cdn_object_get_variables (first);
	GSList *prop2 = cdn_object_get_variables (second);

	gboolean ret = g_slist_length (prop1) == g_slist_length (prop2);
	g_slist_free (prop2);

	if (ret)
	{
		GSList *item = prop1;

		while (item)
		{
			CdnVariable *prop1 = item->data;
			CdnVariable *prop2 = cdn_object_get_variable (second,
			                                              cdn_variable_get_name (prop1));

			if (!prop2 || !cdn_variable_equal (prop1, prop2))
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
cdn_object_get_copy_type_impl (CdnObject *object)
{
	return G_TYPE_FROM_INSTANCE (object);
}

static void
cdn_object_class_init (CdnObjectClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_object_finalize;
	object_class->dispose = cdn_object_dispose;
	object_class->get_property = get_property;
	object_class->set_property = set_property;

	klass->reset = cdn_object_reset_impl;
	klass->foreach_expression = cdn_object_foreach_expression_impl;
	klass->copy = cdn_object_copy_impl;
	klass->compile = cdn_object_compile_impl;
	klass->get_compile_context = cdn_object_get_compile_context_impl;
	klass->apply_template = cdn_object_apply_template_impl;
	klass->unapply_template = cdn_object_unapply_template_impl;
	klass->equal = cdn_object_equal_impl;

	klass->get_variable = cdn_object_get_variable_impl;
	klass->get_variables = cdn_object_get_variables_impl;
	klass->has_variable = cdn_object_has_variable_impl;
	klass->remove_variable = cdn_object_remove_variable_impl;
	klass->verify_remove_variable = cdn_object_verify_remove_variable_impl;
	klass->add_variable = cdn_object_add_variable_impl;

	klass->clear = cdn_object_clear_impl;
	klass->taint = cdn_object_taint_impl;
	klass->get_copy_type = cdn_object_get_copy_type_impl;

	/**
	 * CdnObject:id:
	 *
	 * The #CdnObject id.
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
	 * CdnObject:parent:
	 *
	 * The #CdnObject parent.
	 *
	 */
	g_object_class_install_property (object_class,
	                                 PROP_PARENT,
	                                 g_param_spec_object ("parent",
	                                                      "Parent",
	                                                      "Parent",
	                                                      CDN_TYPE_OBJECT,
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
	 * CdnObject:auto-imported:
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
	 * CdnObject::tainted:
	 * @object: a #CdnObject
	 *
	 * Emitted when the object is tainted
	 *
	 **/
	object_signals[TAINTED] =
		g_signal_new ("tainted",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               tainted),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CdnObject::compiled:
	 * @object: a #CdnObject
	 *
	 * Emitted when the object is compiled
	 *
	 **/
	object_signals[COMPILED] =
		g_signal_new ("compiled",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               compiled),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CdnObject::resetted:
	 * @object: a #CdnObject
	 *
	 * Emitted when the object is resetted
	 *
	 **/
	object_signals[RESETTED] =
		g_signal_new ("resetted",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               resetted),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__VOID,
		              G_TYPE_NONE,
		              0);

	/**
	 * CdnObject::copied:
	 * @object: a #CdnObject
	 * @copy: the copy
	 *
	 * Emitted when the object is copied
	 *
	 **/
	object_signals[COPIED] =
		g_signal_new ("copied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               copied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_OBJECT);

	/**
	 * CdnObject::verify-remove-property:
	 * @object: a #CdnObject
	 * @name: the property name
	 * @error: the error
	 *
	 * Emitted when a property is added to the object
	 *
	 * Returns: %TRUE if the property can be removed, %FALSE otherwise
	 *
	 **/
	object_signals[VERIFY_REMOVE_VARIABLE] =
		g_signal_new ("verify-remove-variable",
		              G_TYPE_FROM_CLASS (klass),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass, verify_remove_variable),
		              cdn_signal_accumulator_false_handled,
		              NULL,
		              cdn_marshal_BOOLEAN__STRING_POINTER,
		              G_TYPE_BOOLEAN,
		              2,
		              G_TYPE_STRING,
		              G_TYPE_POINTER);

	/**
	 * CdnObject::property-added:
	 * @object: a #CdnObject
	 * @property: the added #CdnVariable
	 *
	 * Emitted when a property is added to the object
	 *
	 **/
	object_signals[VARIABLE_ADDED] =
		g_signal_new ("variable-added",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               variable_added),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_VARIABLE);

	/**
	 * CdnObject::property-removed:
	 * @object: a #CdnObject
	 * @property: the removed #CdnVariable
	 *
	 * Emitted when a property is removed from the object
	 *
	 **/
	object_signals[VARIABLE_REMOVED] =
		g_signal_new ("variable-removed",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               variable_removed),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_VARIABLE);

	/**
	 * CdnObject::template-applied:
	 * @object: a #CdnObject
	 * @templ: the applied #CdnObject
	 *
	 * Emitted when a template is applied to the object
	 *
	 **/
	object_signals[TEMPLATE_APPLIED] =
		g_signal_new ("template-applied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               template_applied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_OBJECT);

	/**
	 * CdnObject::template-unapplied:
	 * @object: a #CdnObject
	 * @templ: the unapplied #CdnObject
	 *
	 * Emitted when a template is unapplied from the object
	 *
	 **/
	object_signals[TEMPLATE_UNAPPLIED] =
		g_signal_new ("template-unapplied",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnObjectClass,
		                               template_unapplied),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__OBJECT,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_OBJECT);

	g_type_class_add_private (object_class, sizeof (CdnObjectPrivate));
}

static void
cdn_object_init (CdnObject *self)
{
	self->priv = CDN_OBJECT_GET_PRIVATE (self);

	self->priv->property_hash = g_hash_table_new_full (g_str_hash,
	                                                   g_str_equal,
	                                                   (GDestroyNotify)g_free,
	                                                   NULL);

	self->priv->tags = cdn_taggable_create_table ();
	self->priv->compiled = FALSE;
}

/**
 * cdn_object_new:
 * @id: the object id
 *
 * Creates a new #CdnObject.
 *
 * Return value: the newly created #CdnObject
 *
 **/
CdnObject *
cdn_object_new (const gchar *id)
{
	return g_object_new (CDN_TYPE_OBJECT, "id", id, NULL);
}

/**
 * cdn_object_new_from_template:
 * @templ: A #CdnObject
 *
 * Create a new #CdnObject based on the template @templ.
 *
 * Returns: A #CdnObject
 *
 **/
CdnObject *
cdn_object_new_from_template (CdnObject  *templ,
                              GError    **error)
{
	CdnObject *obj = g_object_new (G_TYPE_FROM_INSTANCE (templ),
	                               "id", cdn_object_get_id (templ),
	                               NULL);

	if (!cdn_object_apply_template (obj, templ, error))
	{
		g_object_unref (obj);
		return NULL;
	}

	if (CDN_IS_LAYOUTABLE (templ) &&
	    cdn_layoutable_supports_location (CDN_LAYOUTABLE (templ)))
	{
		cdn_layoutable_get_location (CDN_LAYOUTABLE (templ),
		                             &obj->priv->x,
		                             &obj->priv->y);
	}

	return obj;
}

/**
 * cdn_object_add_variable:
 * @object: the #CdnObject
 * @property: the #CdnVariable to add
 * @error: a #GError
 *
 * Add a new property to the object. Note that if a property with the same
 * name already exists, the property information is transfered to the existing
 * property instance. This means that the specified @property might not actually
 * be added to the object. Also, since a #CdnVariable is a #GInitiallyUnowned,
 * @property will be destroyed after the call to #cdn_object_add_variable in
 * the above described case, unless you explicitly sink the floating reference.
 *
 * In the case that you can not know whether a property is overriding an
 * existing property in @object, never use @property after a call to
 * #cdn_object_add_variable. Instead, retrieve the corresponding property
 * using #cdn_object_get_variable after the call to #cdn_object_add_variable.
 *
 * Returns: %TRUE if the property was added successfully, %FALSE otherwise
 **/
gboolean
cdn_object_add_variable (CdnObject    *object,
                         CdnVariable  *property,
                         GError       **error)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CDN_IS_VARIABLE (property), FALSE);
	g_return_val_if_fail (cdn_variable_get_object (property) == NULL, FALSE);

	if (CDN_OBJECT_GET_CLASS (object)->add_variable)
	{
		return CDN_OBJECT_GET_CLASS (object)->add_variable (object,
		                                                    property,
		                                                    error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_object_get_variable:
 * @object: a #CdnObject
 * @name: a property name
 *
 * Get a #CdnVariable from the object by name
 *
 * Returns: (transfer none) (allow-none): the #CdnVariable with name @name, or
 *          %NULL if no such property could be found
 *
 **/
CdnVariable *
cdn_object_get_variable (CdnObject    *object,
                         const gchar  *name)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);
	g_return_val_if_fail (name != NULL, NULL);

	if (CDN_OBJECT_GET_CLASS (object)->get_variable)
	{
		return CDN_OBJECT_GET_CLASS (object)->get_variable (object,
		                                                    name);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_object_has_variable:
 * @object: a #CdnObject
 * @name: a property name
 *
 * Get whether @object has a property with name @name
 *
 * Returns: %TRUE if @object has a property with name @name, %FALSE otherwise
 *
 **/
gboolean
cdn_object_has_variable (CdnObject    *object,
                         const gchar  *name)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	return cdn_object_get_variable (object, name) != NULL;
}

/**
 * cdn_object_remove_variable:
 * @object: a #CdnObject
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
cdn_object_remove_variable (CdnObject    *object,
                            const gchar  *name,
                            GError      **error)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	if (CDN_OBJECT_GET_CLASS (object)->remove_variable)
	{
		return CDN_OBJECT_GET_CLASS (object)->remove_variable (object,
		                                                       name,
		                                                       error);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_object_verify_remove_variable:
 * @object: a #CdnObject
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
cdn_object_verify_remove_variable (CdnObject    *object,
                                   const gchar  *name,
                                   GError      **error)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (name != NULL, FALSE);

	gboolean ret = FALSE;

	g_signal_emit (object,
	               object_signals[VERIFY_REMOVE_VARIABLE],
	               0,
	               name,
	               error,
	               &ret);

	return !ret;
}

/**
 * cdn_object_get_variables:
 * @object: a #CdnObject
 *
 * Gets the object variables.
 *
 * Returns: (element-type CdnVariable) (transfer container): a list of #CdnVariable.
 *
 **/
GSList *
cdn_object_get_variables (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	if (CDN_OBJECT_GET_CLASS (object)->get_variables)
	{
		return CDN_OBJECT_GET_CLASS (object)->get_variables (object);
	}
	else
	{
		return NULL;
	}
}

/**
 * cdn_object_reset:
 * @object: the #CdnObject
 *
 * Reset all variables to their initial values
 *
 **/
void
cdn_object_reset (CdnObject *object)
{
	g_return_if_fail (CDN_IS_OBJECT (object));

	if (CDN_OBJECT_GET_CLASS (object)->reset)
	{
		CDN_OBJECT_GET_CLASS (object)->reset (object);
	}
}

/**
 * cdn_object_get_id:
 * @object: a #CdnObject
 *
 * Gets the object id
 *
 * Returns: (transfer none): the object id
 *
 **/
const gchar *
cdn_object_get_id (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return object->priv->id;
}

/**
 * cdn_object_set_id:
 * @object: a #CdnObject
 * @id: the new object id
 *
 * Sets the object id
 *
 **/
void
cdn_object_set_id (CdnObject    *object,
                   const gchar  *id)
{
	g_return_if_fail (CDN_IS_OBJECT (object));
	g_return_if_fail (id != NULL);

	set_id (object, id);
	g_object_notify (G_OBJECT (object), "id");
}

/**
 * cdn_object_compile:
 * @object: A #CdnObject
 * @context: A #CdnCompileContext
 * @error: (type CdnCompileError): A #CdnCompileError
 *
 * Compile the object.
 *
 * Returns: %TRUE if the object compiled successfully, %FALSE otherwise. If the
 *          compilation failed and @error was set, the reason for the compile
 *          failure is set in @error.
 *
 **/
gboolean
cdn_object_compile (CdnObject         *object,
                    CdnCompileContext *context,
                    CdnCompileError   *error)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	if (CDN_OBJECT_GET_CLASS (object)->compile)
	{
		return CDN_OBJECT_GET_CLASS (object)->compile (object,
		                                               context,
		                                               error);
	}

	return TRUE;
}

/**
 * cdn_object_taint:
 * @object: a #CdnObject
 *
 * Mark the object as tainted. This emits the "tainted" signal. The #CdnNetwork
 * in which the object is added acts on this signal to mark the network tainted
 * and as such the object will be properly recompiled when the network needs
 * to be simulated
 *
 **/
void
cdn_object_taint (CdnObject *object)
{
	g_return_if_fail (CDN_IS_OBJECT (object));

	if (CDN_OBJECT_GET_CLASS (object)->taint)
	{
		CDN_OBJECT_GET_CLASS (object)->taint (object);
	}
}

/**
 * cdn_object_clear:
 * @object: A #CdnObject
 *
 * Clear all variables from the object.
 *
 **/
void
cdn_object_clear (CdnObject *object)
{
	g_return_if_fail (CDN_IS_OBJECT (object));

	if (CDN_OBJECT_GET_CLASS (object)->clear)
	{
		CDN_OBJECT_GET_CLASS (object)->clear (object);
	}
}

/**
 * cdn_object_equal:
 * @first: A #CdnObject
 * @second: A #CdnObject
 *
 * Check if two objects are equal.
 *
 * Returns: %TRUE if the objects are equal, %FALSE otherwise
 *
 **/
gboolean
cdn_object_equal (CdnObject *first,
                  CdnObject *second)
{
	g_return_val_if_fail (CDN_IS_OBJECT (first), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (second), FALSE);

	if (CDN_OBJECT_GET_CLASS (first)->equal)
	{
		return CDN_OBJECT_GET_CLASS (first)->equal (first, second);
	}
	else
	{
		return FALSE;
	}
}

/**
 * cdn_object_get_applied_templates:
 * @object: A #CdnObject
 *
 * Get the list of applied templates.
 *
 * Returns: (element-type CdnObject) (transfer none): A #GSList of #CdnObject
 *
 **/
const GSList *
cdn_object_get_applied_templates (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return object->priv->templates;
}

/**
 * cdn_object_get_template_applies_to:
 * @object: A #CdnObject
 *
 * Get the list objects that this object (as a template) applies to
 *
 * Returns: (element-type CdnObject) (transfer none): A #GSList of #CdnObject
 *
 **/
const GSList *
cdn_object_get_template_applies_to (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return object->priv->templates_reverse_map;
}

/**
 * cdn_object_get_parent:
 * @object: A #CdnObject
 *
 * Get the parent of the object.
 *
 * Returns: (transfer none): A #CdnNode
 *
 **/
CdnNode *
cdn_object_get_parent (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return object->priv->parent;
}

/**
 * cdn_object_is_compiled:
 * @object: A #CdnObject
 *
 * Get whether the object is compiled.
 *
 * Returns: %TRUE if the object is compiled, %FALSE otherwise
 *
 **/
gboolean
cdn_object_is_compiled (CdnObject *object)
{
	/* Omit check to speed up */
	return object->priv->compiled ? TRUE : FALSE;
}

/**
 * cdn_object_apply_template:
 * @object: A #CdnObject
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
cdn_object_apply_template (CdnObject  *object,
                           CdnObject  *templ,
                           GError    **error)
{
	gboolean ret;

	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (templ), FALSE);
	g_return_val_if_fail (g_type_is_a (G_TYPE_FROM_INSTANCE (object),
	                                   G_TYPE_FROM_INSTANCE (templ)), FALSE);

	if (g_slist_find (object->priv->templates, templ))
	{
		return TRUE;
	}

	ret = CDN_OBJECT_GET_CLASS (object)->apply_template (object,
	                                                     templ,
	                                                     error);

	return ret;
}

/**
 * cdn_object_unapply_template:
 * @object: A #CdnObject
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
cdn_object_unapply_template (CdnObject  *object,
                             CdnObject  *templ,
                             GError    **error)
{
	gboolean ret;

	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);
	g_return_val_if_fail (CDN_IS_OBJECT (templ), FALSE);

	if (!g_slist_find (object->priv->templates, templ))
	{
		g_set_error (error,
		             CDN_OBJECT_ERROR,
		             CDN_OBJECT_ERROR_TEMPLATE_NOT_FOUND,
		             "The template `%s' is not applied to `%s'",
		             cdn_object_get_id (object),
		             cdn_object_get_id (templ));

		return FALSE;
	}

	ret = CDN_OBJECT_GET_CLASS (object)->unapply_template (object,
	                                                       templ,
	                                                       error);

	return ret;
}

/**
 * cdn_object_get_variable_template:
 * @object: A #CdnObject
 * @property: A #CdnVariable
 * @match_full: How to match the property
 *
 * Get the template on which @property is defined, if any. If @match_full is
 * %TRUE, the template will only be possitively matched if both variables are
 * equal (i.e. if a property originated from a template, but was later modified,
 * this function will not return the original template object).
 *
 * Returns: (transfer none): A #CdnObject
 *
 **/
CdnObject *
cdn_object_get_variable_template (CdnObject   *object,
                                  CdnVariable *property,
                                  gboolean     match_full)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CDN_IS_VARIABLE (property), NULL);

	GSList *templates = g_slist_copy ((GSList *)cdn_object_get_applied_templates (object));
	templates = g_slist_reverse (templates);
	GSList *item;

	gchar const *name = cdn_variable_get_name (property);

	for (item = templates; item; item = g_slist_next (item))
	{
		CdnVariable *tprop;
		CdnObject *templ = item->data;

		tprop = cdn_object_get_variable (templ, name);

		if (tprop && (!match_full || cdn_variable_equal (property, tprop)))
		{
			g_slist_free (templates);
			return templ;
		}
	}

	g_slist_free (templates);

	return NULL;
}

/**
 * cdn_object_copy:
 * @object: The source object
 *
 * Create a copy of the given object. This will create a new object with the
 * same id and with a copy of all the variables defined on the object.
 * The copied object will not have the same links, nor will it have a parent.
 * See the documentation of specific subclasses of #CdnObject to see the
 * copy semantics for those classes.
 *
 * Returns: (transfer full): A #CdnObject
 *
 **/
CdnObject *
cdn_object_copy (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	GType gtype;

	if (CDN_OBJECT_GET_CLASS (object)->get_copy_type)
	{
		gtype = CDN_OBJECT_GET_CLASS (object)->get_copy_type (object);
	}
	else
	{
		gtype = G_TYPE_FROM_INSTANCE (object);
	}

	CdnObject *ret = g_object_new (gtype,
	                               "id", cdn_object_get_id (object),
	                               NULL);

	if (CDN_OBJECT_GET_CLASS (ret)->copy)
	{
		CDN_OBJECT_GET_CLASS (ret)->copy (ret, object);
	}

	g_signal_emit (object, object_signals[COPIED], 0, ret);

	return ret;
}

/**
 * cdn_object_get_auto_imported:
 * @object: A #CdnObject
 *
 * Get whether the object was automatically imported.
 *
 * Returns: %TRUE if the object was automatically imported, %FALSE otherwise
 *
 **/
gboolean
cdn_object_get_auto_imported (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), FALSE);

	return object->priv->auto_imported;
}

/**
 * cdn_object_set_auto_imported:
 * @object: A #CdnObject
 * @auto_imported: a boolean
 *
 * Set whether the object was automatically imported.
 *
 **/
void
cdn_object_set_auto_imported (CdnObject *object,
                              gboolean   auto_imported)
{
	g_return_if_fail (CDN_IS_OBJECT (object));

	object->priv->auto_imported = auto_imported;
}

void
_cdn_object_set_parent (CdnObject *object,
                        CdnNode  *parent)
{
	g_return_if_fail (CDN_IS_OBJECT (object));
	g_return_if_fail (parent == NULL || CDN_IS_NODE (parent));

	object->priv->parent = parent;

	g_object_notify (G_OBJECT (object), "parent");
}

/**
 * cdn_object_get_full_id:
 * @object: A #CdnObject
 *
 * Get the full id of the object. This is the id that can be used in the outer
 * most parent to refer to this object (i.e.
 * <code>cdn_node_find_object (top_parent, cdn_object_get_full_id (deep_child)) == deep_child</code>)
 *
 * Returns: The full id of the object. This is a newly allocated string that
 *          should be freed with g_free.
 *
 **/
gchar *
cdn_object_get_full_id (CdnObject *object)
{
	CdnNode *parent;

	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	parent = cdn_object_get_parent (object);

	if (!parent ||
	    (CDN_IS_NETWORK (parent) && object == CDN_OBJECT (cdn_network_get_template_node (CDN_NETWORK (parent)))))
	{
		return cdn_selector_escape_identifier (object->priv->id);
	}

	while (CDN_OBJECT (parent)->priv->parent)
	{
		CdnNode *par = CDN_OBJECT (parent)->priv->parent;

		if ((CDN_IS_NETWORK (par) && parent == cdn_network_get_template_node (CDN_NETWORK (par))))
		{
			break;
		}

		parent = par;
	}

	return cdn_object_get_relative_id (object, parent);
}

gchar *
cdn_object_get_full_id_for_display (CdnObject *object)
{
	CdnNode *parent;

	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	parent = cdn_object_get_parent (object);

	if (!parent ||
	    (CDN_IS_NETWORK (parent) && object == CDN_OBJECT (cdn_network_get_template_node (CDN_NETWORK (parent)))))
	{
		return g_strdup (object->priv->id);
	}

	while (CDN_OBJECT (parent)->priv->parent)
	{
		CdnNode *par = CDN_OBJECT (parent)->priv->parent;

		if ((CDN_IS_NETWORK (par) && parent == cdn_network_get_template_node (CDN_NETWORK (par))))
		{
			break;
		}

		parent = par;
	}

	return cdn_object_get_relative_id_for_display (object, parent);
}

/**
 * cdn_object_foreach_expression:
 * @object: A #CdnObject
 * @func: (scope call): A #CdnForeachExpressionFunc
 * @userdata: The userdata that will be supplied to @func
 *
 * Iterate over each expression defined in the object. For normal objects, this
 * means all the property expressions. Classes that inherit from #CdnObject
 * can implement their own iteration for additional expressions (see for
 * example #CdnNode).
 *
 **/
void
cdn_object_foreach_expression (CdnObject                *object,
                               CdnForeachExpressionFunc  func,
                               gpointer                  userdata)
{
	/* Omit type check to increase speed */
	if (!func)
	{
		return;
	}

	CDN_OBJECT_GET_CLASS (object)->foreach_expression (object, func, userdata);
}

static gchar *
get_relative_id (CdnObject *object,
                 CdnNode  *parent,
                 gboolean   for_display)
{
	gchar *ret;
	gchar *par = NULL;

	if (object == CDN_OBJECT (parent))
	{
		return g_strdup ("");
	}

	if (cdn_object_get_parent (object) == NULL)
	{
		return for_display ?
		       g_strdup (object->priv->id) :
		       cdn_selector_escape_identifier (object->priv->id);
	}

	if (object->priv->parent)
	{
		if (for_display)
		{
			par = cdn_object_get_relative_id_for_display (CDN_OBJECT (object->priv->parent),
			                                              parent);
		}
		else
		{
			par = cdn_object_get_relative_id (CDN_OBJECT (object->priv->parent),
			                                  parent);
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
			esc = cdn_selector_escape_identifier (object->priv->id);
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
			ret = cdn_selector_escape_identifier (object->priv->id);
		}
	}

	g_free (par);
	return ret;
}

gchar *
cdn_object_get_relative_id (CdnObject *object,
                            CdnNode  *parent)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CDN_IS_NODE (parent), NULL);

	return get_relative_id (object, parent, FALSE);
}

gchar *
cdn_object_get_relative_id_for_display (CdnObject *object,
                                        CdnNode  *parent)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);
	g_return_val_if_fail (CDN_IS_NODE (parent), NULL);

	return get_relative_id (object, parent, TRUE);
}

CdnCompileContext *
cdn_object_get_compile_context (CdnObject         *object,
                                CdnCompileContext *context)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);
	g_return_val_if_fail (context == NULL || CDN_IS_COMPILE_CONTEXT (context), NULL);

	return CDN_OBJECT_GET_CLASS (object)->get_compile_context (object, context);
}

void
cdn_object_add_event (CdnObject *object,
                      CdnEvent  *event)
{
	g_return_if_fail (CDN_IS_OBJECT (object));
	g_return_if_fail (CDN_IS_EVENT (event));

	object->priv->events = g_slist_append (object->priv->events,
	                                       g_object_ref_sink (event));

	object->priv->last_event = event;
}

CdnEvent *
cdn_object_get_last_event (CdnObject *object)
{
	g_return_val_if_fail (CDN_IS_OBJECT (object), NULL);

	return object->priv->last_event;
}

GSList const *
cdn_object_get_events (CdnObject *object)
{
	return object->priv->events;
}
