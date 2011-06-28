/*
 * cpg-mini-object.h
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

#ifndef __CPG_MINI_OBJECT_H__
#define __CPG_MINI_OBJECT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_MINI_OBJECT		(cpg_mini_object_get_type ())
#define CPG_MINI_OBJECT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MINI_OBJECT, CpgMiniObject))
#define CPG_MINI_OBJECT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_MINI_OBJECT, CpgMiniObject const))
#define CPG_MINI_OBJECT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_MINI_OBJECT, CpgMiniObjectClass))
#define CPG_IS_MINI_OBJECT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_MINI_OBJECT))
#define CPG_IS_MINI_OBJECT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_MINI_OBJECT))
#define CPG_MINI_OBJECT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_MINI_OBJECT, CpgMiniObjectClass))
#define CPG_MINI_OBJECT_CAST(obj)	((CpgMiniObject *)obj)

typedef struct _CpgMiniObject		CpgMiniObject;
typedef struct _CpgMiniObjectClass	CpgMiniObjectClass;

/**
 * CpgMiniObjectCopyFunction:
 * @obj: a #CpgMiniObject
 *
 * Returns: (transfer full):
 *
 **/
typedef CpgMiniObject *(*CpgMiniObjectCopyFunction)     (CpgMiniObject const *obj);
typedef void           (*CpgMiniObjectFinalizeFunction) (CpgMiniObject *obj);

struct _CpgMiniObject
{
	/*< private >*/
	GTypeInstance parent;
};

struct _CpgMiniObjectClass
{
	/*< private >*/
	GTypeClass parent_class;

	CpgMiniObjectCopyFunction copy;
	CpgMiniObjectFinalizeFunction finalize;
};

GType cpg_mini_object_get_type (void) G_GNUC_CONST;

CpgMiniObject *cpg_mini_object_new  (GType                type);
CpgMiniObject *cpg_mini_object_copy (CpgMiniObject const *obj);
void           cpg_mini_object_free (CpgMiniObject       *obj);

/* GParamSpec */

#define	CPG_TYPE_PARAM_MINI_OBJECT	        (cpg_param_spec_mini_object_get_type())
#define CPG_IS_PARAM_SPEC_MINI_OBJECT(pspec)    (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), \
                                                 CPG_TYPE_PARAM_MINI_OBJECT))
#define CPG_PARAM_SPEC_MINI_OBJECT(pspec)       (G_TYPE_CHECK_INSTANCE_CAST ((pspec), \
                                                 CPG_TYPE_PARAM_MINI_OBJECT, \
                                                 CpgParamSpecMiniObject))

typedef struct _CpgParamSpecMiniObject CpgParamSpecMiniObject;

struct _CpgParamSpecMiniObject
{
	GParamSpec parent_instance;
};


GType cpg_param_spec_mini_object_get_type (void);

GParamSpec *cpg_param_spec_mini_object (const char *name,
                                        const char *nick,
                                        const char *blurb,
                                        GType       object_type,
                                        GParamFlags flags);

/* GValue stuff */
#define CPG_VALUE_HOLDS_MINI_OBJECT(value)  (G_VALUE_HOLDS(value, CPG_TYPE_MINI_OBJECT))

void           cpg_value_set_mini_object  (GValue        *value,
                                           CpgMiniObject *mini_object);
void           cpg_value_take_mini_object (GValue        *value,
                                           CpgMiniObject *mini_object);
CpgMiniObject *cpg_value_get_mini_object  (GValue const  *value);
CpgMiniObject *cpg_value_dup_mini_object  (GValue const  *value);

G_END_DECLS

#endif /* __CPG_MINI_OBJECT_H__ */
