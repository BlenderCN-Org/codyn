/*
 * cdn-mini-object.c
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

#include "cdn-mini-object.h"

#include <gobject/gvaluecollector.h>

static void cdn_mini_object_class_init (gpointer g_class, gpointer class_data);

static void cdn_value_mini_object_init (GValue *value);
static void cdn_value_mini_object_free (GValue *value);
static void cdn_value_mini_object_copy (GValue const *src_value,
                                        GValue *dest_value);

static gpointer cdn_value_mini_object_peek_pointer (GValue const *value);
static gchar *cdn_value_mini_object_collect (GValue *value,
                                             guint n_collect_values,
                                             GTypeCValue *collect_values,
                                             guint collect_flags);

static gchar *cdn_value_mini_object_lcopy (GValue const *value,
                                           guint n_collect_values,
                                           GTypeCValue *collect_values,
                                           guint collect_flags);

static CdnMiniObject *cdn_mini_object_copy_default (CdnMiniObject *obj);
static void cdn_mini_object_finalize (CdnMiniObject *obj);
static void cdn_mini_object_init (CdnMiniObject *obj);

GType
cdn_mini_object_get_type (void)
{
	static volatile GType _cdn_mini_object_type = 0;

	if (g_once_init_enter (&_cdn_mini_object_type))
	{
		GType _type;

		static const GTypeValueTable value_table =
		{
			cdn_value_mini_object_init,
			cdn_value_mini_object_free,
			cdn_value_mini_object_copy,
			cdn_value_mini_object_peek_pointer,
			(char *) "p",
			cdn_value_mini_object_collect,
			(char *) "p",
			cdn_value_mini_object_lcopy
		};

		static const GTypeInfo mini_object_info =
		{
			sizeof (CdnMiniObjectClass),
			NULL,
			NULL,
			cdn_mini_object_class_init,
			NULL,
			NULL,
			sizeof (CdnMiniObject),
			0,
			(GInstanceInitFunc)cdn_mini_object_init,
			&value_table
		};

		static const GTypeFundamentalInfo mini_object_fundamental_info =
		{
			(G_TYPE_FLAG_CLASSED | G_TYPE_FLAG_INSTANTIATABLE |
			 G_TYPE_FLAG_DERIVABLE | G_TYPE_FLAG_DEEP_DERIVABLE)
		};

		_type = g_type_fundamental_next ();
		g_type_register_fundamental (_type,
		                             "CdnMiniObject",
		                             &mini_object_info,
		                             &mini_object_fundamental_info,
		                             G_TYPE_FLAG_ABSTRACT);

		g_once_init_leave (&_cdn_mini_object_type, _type);
	}

	return _cdn_mini_object_type;
}

static void
cdn_mini_object_class_init (gpointer g_class, gpointer class_data)
{
	CdnMiniObjectClass *mo_class = CDN_MINI_OBJECT_CLASS (g_class);

	mo_class->copy = cdn_mini_object_copy_default;
	mo_class->finalize = cdn_mini_object_finalize;
}

static void
cdn_mini_object_init (CdnMiniObject *obj)
{
	obj->ref_count = 1;
}

static CdnMiniObject *
cdn_mini_object_copy_default (CdnMiniObject *obj)
{
	return cdn_mini_object_new (G_TYPE_FROM_INSTANCE (obj));
}

static void
cdn_mini_object_finalize (CdnMiniObject * obj)
{
	g_type_free_instance ((GTypeInstance *)obj);
}

CdnMiniObject *
cdn_mini_object_new (GType type)
{
	return CDN_MINI_OBJECT_CAST (g_type_create_instance (type));
}

/**
 * cdn_mini_object_copy:
 * @obj: A #CdnMiniObject
 *
 * Copy a mini object.
 *
 * Returns: (transfer full): A #CdnMiniObject
 *
 **/
gpointer
cdn_mini_object_copy (gpointer obj)
{
	CdnMiniObjectClass *mo_class;

	if (!obj)
	{
		return NULL;
	}

	g_return_val_if_fail (CDN_IS_MINI_OBJECT (obj), NULL);

	mo_class = CDN_MINI_OBJECT_GET_CLASS (obj);

	return mo_class->copy (obj);
}

/**
 * cdn_mini_object_ref:
 * @obj: a #CdnMiniObject.
 *
 * Increase the ref count on the mini object.
 *
 * Returns: (transfer full): @obj
 *
 **/
gpointer
cdn_mini_object_ref (gpointer obj)
{
	CdnMiniObject *mobj;

	if (!obj)
	{
		return NULL;
	}

	g_return_val_if_fail (CDN_IS_MINI_OBJECT (obj), NULL);

	mobj = obj;

	g_atomic_int_inc (&mobj->ref_count);
	return obj;
}

/**
 * cdn_mini_object_unref:
 * @obj: a #CdnMiniObject.
 *
 * Decrease the ref count of the mini object. If the ref count drops to 0, the
 * mini object will be finalized.
 *
 **/
void
cdn_mini_object_unref (gpointer obj)
{
	CdnMiniObject *mobj;
	CdnMiniObjectClass *mo_class;

	if (!obj)
	{
		return;
	}

	g_return_if_fail (CDN_IS_MINI_OBJECT (obj));

	mobj = obj;

	if (g_atomic_int_dec_and_test (&mobj->ref_count))
	{
		mo_class = CDN_MINI_OBJECT_GET_CLASS (obj);
		mo_class->finalize (obj);
	}
}

static void
cdn_value_mini_object_init (GValue *value)
{
	value->data[0].v_pointer = NULL;
}

static void
cdn_value_mini_object_free (GValue *value)
{
	if (value->data[0].v_pointer)
	{
		cdn_mini_object_unref (value->data[0].v_pointer);
	}
}

static void
cdn_value_mini_object_copy (GValue const *src,
                            GValue       *dest)
{
	if (src->data[0].v_pointer)
	{
		dest->data[0].v_pointer =
			cdn_mini_object_ref (src->data[0].v_pointer);
	}
	else
	{
		dest->data[0].v_pointer = NULL;
	}
}

static gpointer
cdn_value_mini_object_peek_pointer (GValue const *value)
{
	return value->data[0].v_pointer;
}

static gchar *
cdn_value_mini_object_collect (GValue      *value,
                               guint        n_collect_values,
                               GTypeCValue *collect_values,
                               guint        collect_flags)
{
	if (collect_values[0].v_pointer)
	{
		value->data[0].v_pointer =
			cdn_mini_object_ref (collect_values[0].v_pointer);
	}
	else
	{
		value->data[0].v_pointer = NULL;
	}

	return NULL;
}

static gchar *
cdn_value_mini_object_lcopy (GValue const *value,
                             guint         n_collect_values,
                             GTypeCValue  *collect_values,
                             guint         collect_flags)
{
	gpointer *mini_object_p = collect_values[0].v_pointer;

	if (!mini_object_p)
	{
		return g_strdup_printf ("value location for '%s' passed as NULL",
		                        G_VALUE_TYPE_NAME (value));
	}

	if (!value->data[0].v_pointer)
	{
		*mini_object_p = NULL;
	}
	else if (collect_flags & G_VALUE_NOCOPY_CONTENTS)
	{
		*mini_object_p = value->data[0].v_pointer;
	}
	else
	{
		*mini_object_p = cdn_mini_object_ref (value->data[0].v_pointer);
	}

	return NULL;
}

void
cdn_value_set_mini_object (GValue        *value,
                           CdnMiniObject *mini_object)
{
	g_return_if_fail (CDN_VALUE_HOLDS_MINI_OBJECT (value));
	g_return_if_fail (mini_object == NULL || CDN_IS_MINI_OBJECT (mini_object));

	if (value->data[0].v_pointer)
	{
		cdn_mini_object_unref (value->data[0].v_pointer);
	}

	if (mini_object == NULL)
	{
		value->data[0].v_pointer = NULL;
	}
	else
	{
		value->data[0].v_pointer = cdn_mini_object_ref (mini_object);
	}
}

void
cdn_value_take_mini_object (GValue        *value,
                            CdnMiniObject *mini_object)
{
	g_return_if_fail (CDN_VALUE_HOLDS_MINI_OBJECT (value));
	g_return_if_fail (mini_object == NULL || CDN_IS_MINI_OBJECT (mini_object));

	if (value->data[0].v_pointer)
	{
		cdn_mini_object_unref (value->data[0].v_pointer);
	}

	if (mini_object == NULL)
	{
		value->data[0].v_pointer = NULL;
	}
	else
	{
		value->data[0].v_pointer = mini_object;
	}
}

/**
 * cdn_value_get_mini_object:
 * @value: A #GValue
 *
 * Get the mini object from the value. The returned object is owned by the
 * value and should not be freed.
 *
 * Returns: (transfer none): A #CdnMiniObject
 *
 **/
CdnMiniObject *
cdn_value_get_mini_object (GValue const *value)
{
	g_return_val_if_fail (CDN_VALUE_HOLDS_MINI_OBJECT (value), NULL);

	return value->data[0].v_pointer;
}

/**
 * cdn_value_dup_mini_object:
 * @value: A #GValue
 *
 * Duplicate a mini object.
 *
 * Returns: (transfer full): A #CdnMiniObject
 *
 **/
CdnMiniObject *
cdn_value_dup_mini_object (GValue const *value)
{
	g_return_val_if_fail (CDN_VALUE_HOLDS_MINI_OBJECT (value), NULL);

	return cdn_mini_object_ref (value->data[0].v_pointer);
}

static void
param_mini_object_init (GParamSpec *pspec)
{
}

static void
param_mini_object_set_default (GParamSpec *pspec, GValue *value)
{
	value->data[0].v_pointer = NULL;
}

static gboolean
param_mini_object_validate (GParamSpec * pspec, GValue * value)
{
	CdnMiniObject *mini_object = value->data[0].v_pointer;
	gboolean changed = FALSE;

	if (mini_object && !g_value_type_compatible (G_OBJECT_TYPE (mini_object), pspec->value_type))
	{
		cdn_mini_object_unref (mini_object);
		value->data[0].v_pointer = NULL;
		changed = TRUE;
	}

	return changed;
}

static gint
param_mini_object_values_cmp (GParamSpec   *pspec,
                              GValue const *value1,
                              GValue const *value2)
{
	guint8 *p1 = value1->data[0].v_pointer;
	guint8 *p2 = value2->data[0].v_pointer;

	return p1 < p2 ? -1 : p1 > p2;
}

GType
cdn_param_spec_mini_object_get_type (void)
{
	static GType type;

	if (G_UNLIKELY (type) == 0)
	{
		static const GParamSpecTypeInfo pspec_info =
		{
			sizeof (CdnParamSpecMiniObject),  /* instance_size */
			16,                               /* n_preallocs */
			param_mini_object_init,           /* instance_init */
			G_TYPE_OBJECT,                    /* value_type */
			NULL,                             /* finalize */
			param_mini_object_set_default,    /* value_set_default */
			param_mini_object_validate,       /* value_validate */
			param_mini_object_values_cmp,     /* values_cmp */
		};

		type = g_param_type_register_static ("CdnParamSpecMiniObject", &pspec_info);
	}

	return type;
}

/**
 * cdn_param_spec_mini_object: (skip)
 * @name: the name
 * @nick: the nick
 * @blurb: the blurb
 * @object_type: the object type
 * @flags: the flags
 *
 * Returns: (transfer full): a new #GParamSpec
 *
 **/
GParamSpec *
cdn_param_spec_mini_object (const char  *name,
                            const char  *nick,
                            const char  *blurb,
                            GType        object_type,
                            GParamFlags  flags)
{
	CdnParamSpecMiniObject *ospec;

	g_return_val_if_fail (g_type_is_a (object_type, CDN_TYPE_MINI_OBJECT), NULL);

	ospec = g_param_spec_internal (CDN_TYPE_PARAM_MINI_OBJECT,
	                               name,
	                               nick,
	                               blurb,
	                               flags);

	G_PARAM_SPEC (ospec)->value_type = object_type;

	return G_PARAM_SPEC (ospec);
}
