/*
 * cpg-mini-object.c
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cpg-mini-object.h"

#include <gobject/gvaluecollector.h>

static void cpg_mini_object_class_init (gpointer g_class, gpointer class_data);

static void cpg_value_mini_object_init (GValue *value);
static void cpg_value_mini_object_free (GValue *value);
static void cpg_value_mini_object_copy (GValue const *src_value,
                                        GValue *dest_value);

static gpointer cpg_value_mini_object_peek_pointer (GValue const *value);
static gchar *cpg_value_mini_object_collect (GValue *value,
                                             guint n_collect_values,
                                             GTypeCValue *collect_values,
                                             guint collect_flags);

static gchar *cpg_value_mini_object_lcopy (GValue const *value,
                                           guint n_collect_values,
                                           GTypeCValue *collect_values,
                                           guint collect_flags);

static CpgMiniObject *cpg_mini_object_copy_default (CpgMiniObject const *obj);
static void cpg_mini_object_finalize (CpgMiniObject *obj);

GType
cpg_mini_object_get_type (void)
{
	static volatile GType _cpg_mini_object_type = 0;

	if (g_once_init_enter (&_cpg_mini_object_type))
	{
		GType _type;

		static const GTypeValueTable value_table =
		{
			cpg_value_mini_object_init,
			cpg_value_mini_object_free,
			cpg_value_mini_object_copy,
			cpg_value_mini_object_peek_pointer,
			(char *) "p",
			cpg_value_mini_object_collect,
			(char *) "p",
			cpg_value_mini_object_lcopy
		};

		static const GTypeInfo mini_object_info =
		{
			sizeof (CpgMiniObjectClass),
			NULL,
			NULL,
			cpg_mini_object_class_init,
			NULL,
			NULL,
			sizeof (CpgMiniObject),
			0,
			NULL,
			&value_table
		};

		static const GTypeFundamentalInfo mini_object_fundamental_info =
		{
			(G_TYPE_FLAG_CLASSED | G_TYPE_FLAG_INSTANTIATABLE |
			 G_TYPE_FLAG_DERIVABLE | G_TYPE_FLAG_DEEP_DERIVABLE)
		};

		_type = g_type_fundamental_next ();
		g_type_register_fundamental (_type,
		                             "CpgMiniObject",
		                             &mini_object_info,
		                             &mini_object_fundamental_info,
		                             G_TYPE_FLAG_ABSTRACT);

		g_once_init_leave (&_cpg_mini_object_type, _type);
	}

	return _cpg_mini_object_type;
}

static void
cpg_mini_object_class_init (gpointer g_class, gpointer class_data)
{
	CpgMiniObjectClass *mo_class = CPG_MINI_OBJECT_CLASS (g_class);

	mo_class->copy = cpg_mini_object_copy_default;
	mo_class->finalize = cpg_mini_object_finalize;
}

static CpgMiniObject *
cpg_mini_object_copy_default (CpgMiniObject const *obj)
{
	return cpg_mini_object_new (G_TYPE_FROM_INSTANCE (obj));
}

static void
cpg_mini_object_finalize (CpgMiniObject * obj)
{
}

CpgMiniObject *
cpg_mini_object_new (GType type)
{
	return CPG_MINI_OBJECT_CAST (g_type_create_instance (type));
}

/**
 * cpg_mini_object_copy:
 * @obj: A #CpgMiniObject
 *
 * Copy a mini object.
 *
 * Returns: (transfer full): A #CpgMiniObject
 *
 **/
CpgMiniObject *
cpg_mini_object_copy (CpgMiniObject const *obj)
{
	CpgMiniObjectClass *mo_class;

	mo_class = CPG_MINI_OBJECT_GET_CLASS (obj);

	return mo_class->copy(obj);
}

void
cpg_mini_object_free (CpgMiniObject *obj)
{
	CpgMiniObjectClass *mo_class;

	mo_class = CPG_MINI_OBJECT_GET_CLASS (obj);
	mo_class->finalize (obj);

	g_type_free_instance ((GTypeInstance *)obj);
}

static void
cpg_value_mini_object_init (GValue *value)
{
	value->data[0].v_pointer = NULL;
}

static void
cpg_value_mini_object_free (GValue *value)
{
	if (value->data[0].v_pointer)
	{
		cpg_mini_object_free (CPG_MINI_OBJECT_CAST (value->data[0].v_pointer));
	}
}

static void
cpg_value_mini_object_copy (GValue const *src,
                            GValue       *dest)
{
	if (src->data[0].v_pointer)
	{
		dest->data[0].v_pointer =
			cpg_mini_object_copy (CPG_MINI_OBJECT_CAST (src->data[0].v_pointer));
	}
	else
	{
		dest->data[0].v_pointer = NULL;
	}
}

static gpointer
cpg_value_mini_object_peek_pointer (GValue const *value)
{
	return value->data[0].v_pointer;
}

static gchar *
cpg_value_mini_object_collect (GValue      *value,
                               guint        n_collect_values,
                               GTypeCValue *collect_values,
                               guint        collect_flags)
{
	if (collect_values[0].v_pointer)
	{
		value->data[0].v_pointer =
			cpg_mini_object_copy (collect_values[0].v_pointer);
	}
	else
	{
		value->data[0].v_pointer = NULL;
	}

	return NULL;
}

static gchar *
cpg_value_mini_object_lcopy (GValue const *value,
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
		*mini_object_p = cpg_mini_object_copy (value->data[0].v_pointer);
	}

	return NULL;
}

void
cpg_value_set_mini_object (GValue        *value,
                           CpgMiniObject *mini_object)
{
	g_return_if_fail (CPG_VALUE_HOLDS_MINI_OBJECT (value));
	g_return_if_fail (mini_object == NULL || CPG_IS_MINI_OBJECT (mini_object));

	if (value->data[0].v_pointer)
	{
		cpg_mini_object_free (value->data[0].v_pointer);
	}

	if (mini_object == NULL)
	{
		value->data[0].v_pointer = NULL;
	}
	else
	{
		value->data[0].v_pointer = cpg_mini_object_copy (mini_object);
	}
}

void
cpg_value_take_mini_object (GValue        *value,
                            CpgMiniObject *mini_object)
{
	g_return_if_fail (CPG_VALUE_HOLDS_MINI_OBJECT (value));
	g_return_if_fail (mini_object == NULL || CPG_IS_MINI_OBJECT (mini_object));

	if (value->data[0].v_pointer)
	{
		cpg_mini_object_free (value->data[0].v_pointer);
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
 * cpg_value_get_mini_object:
 * @value: A #GValue
 *
 * Get the mini object from the value. The returned object is owned by the
 * value and should not be freed.
 *
 * Returns: (transfer none): A #CpgMiniObject
 *
 **/
CpgMiniObject *
cpg_value_get_mini_object (GValue const *value)
{
	g_return_val_if_fail (CPG_VALUE_HOLDS_MINI_OBJECT (value), NULL);

	return value->data[0].v_pointer;
}

/**
 * cpg_value_dup_mini_object:
 * @value: A #GValue
 *
 * Duplicate a mini object.
 *
 * Returns: (transfer full): A #CpgMiniObject
 *
 **/
CpgMiniObject *
cpg_value_dup_mini_object (GValue const *value)
{
	g_return_val_if_fail (CPG_VALUE_HOLDS_MINI_OBJECT (value), NULL);

	return cpg_mini_object_copy (value->data[0].v_pointer);
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
	CpgMiniObject *mini_object = value->data[0].v_pointer;
	gboolean changed = FALSE;

	if (mini_object && !g_value_type_compatible (G_OBJECT_TYPE (mini_object), pspec->value_type))
	{
		cpg_mini_object_free (mini_object);
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
cpg_param_spec_mini_object_get_type (void)
{
	static GType type;

	if (G_UNLIKELY (type) == 0)
	{
		static const GParamSpecTypeInfo pspec_info =
		{
			sizeof (CpgParamSpecMiniObject),  /* instance_size */
			16,                               /* n_preallocs */
			param_mini_object_init,           /* instance_init */
			G_TYPE_OBJECT,                    /* value_type */
			NULL,                             /* finalize */
			param_mini_object_set_default,    /* value_set_default */
			param_mini_object_validate,       /* value_validate */
			param_mini_object_values_cmp,     /* values_cmp */
		};

		type = g_param_type_register_static ("CpgParamSpecMiniObject", &pspec_info);
	}

	return type;
}

/**
 * cpg_param_spec_mini_object:
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
cpg_param_spec_mini_object (const char  *name,
                            const char  *nick,
                            const char  *blurb,
                            GType        object_type,
                            GParamFlags  flags)
{
	CpgParamSpecMiniObject *ospec;

	g_return_val_if_fail (g_type_is_a (object_type, CPG_TYPE_MINI_OBJECT), NULL);

	ospec = g_param_spec_internal (CPG_TYPE_PARAM_MINI_OBJECT,
	                               name,
	                               nick,
	                               blurb,
	                               flags);

	G_PARAM_SPEC (ospec)->value_type = object_type;

	return G_PARAM_SPEC (ospec);
}