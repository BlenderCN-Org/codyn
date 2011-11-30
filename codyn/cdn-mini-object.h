/*
 * cdn-mini-object.h
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

#ifndef __CDN_MINI_OBJECT_H__
#define __CDN_MINI_OBJECT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define CDN_TYPE_MINI_OBJECT		(cdn_mini_object_get_type ())
#define CDN_MINI_OBJECT(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_MINI_OBJECT, CdnMiniObject))
#define CDN_MINI_OBJECT_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_MINI_OBJECT, CdnMiniObject const))
#define CDN_MINI_OBJECT_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_MINI_OBJECT, CdnMiniObjectClass))
#define CDN_IS_MINI_OBJECT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_MINI_OBJECT))
#define CDN_IS_MINI_OBJECT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_MINI_OBJECT))
#define CDN_MINI_OBJECT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_MINI_OBJECT, CdnMiniObjectClass))
#define CDN_MINI_OBJECT_CAST(obj)	((CdnMiniObject *)obj)

typedef struct _CdnMiniObject		CdnMiniObject;
typedef struct _CdnMiniObjectClass	CdnMiniObjectClass;

/**
 * CdnMiniObjectCopyFunction:
 * @obj: a #CdnMiniObject
 *
 * Returns: (transfer full):
 *
 **/
typedef CdnMiniObject *(*CdnMiniObjectCopyFunction)     (CdnMiniObject const *obj);
typedef void           (*CdnMiniObjectFinalizeFunction) (CdnMiniObject *obj);

struct _CdnMiniObject
{
	/*< private >*/
	GTypeInstance parent;
};

struct _CdnMiniObjectClass
{
	/*< private >*/
	GTypeClass parent_class;

	CdnMiniObjectCopyFunction copy;
	CdnMiniObjectFinalizeFunction finalize;
};

GType cdn_mini_object_get_type (void) G_GNUC_CONST;

CdnMiniObject *cdn_mini_object_new  (GType                type);
CdnMiniObject *cdn_mini_object_copy (CdnMiniObject const *obj);
void           cdn_mini_object_free (CdnMiniObject       *obj);

/* GParamSpec */

#define	CDN_TYPE_PARAM_MINI_OBJECT	        (cdn_param_spec_mini_object_get_type())
#define CDN_IS_PARAM_SPEC_MINI_OBJECT(pspec)    (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), \
                                                 CDN_TYPE_PARAM_MINI_OBJECT))
#define CDN_PARAM_SPEC_MINI_OBJECT(pspec)       (G_TYPE_CHECK_INSTANCE_CAST ((pspec), \
                                                 CDN_TYPE_PARAM_MINI_OBJECT, \
                                                 CdnParamSpecMiniObject))

typedef struct _CdnParamSpecMiniObject CdnParamSpecMiniObject;

struct _CdnParamSpecMiniObject
{
	GParamSpec parent_instance;
};


GType cdn_param_spec_mini_object_get_type (void);

GParamSpec *cdn_param_spec_mini_object (const char *name,
                                        const char *nick,
                                        const char *blurb,
                                        GType       object_type,
                                        GParamFlags flags);

/* GValue stuff */
#define CDN_VALUE_HOLDS_MINI_OBJECT(value)  (G_VALUE_HOLDS(value, CDN_TYPE_MINI_OBJECT))

void           cdn_value_set_mini_object  (GValue        *value,
                                           CdnMiniObject *mini_object);
void           cdn_value_take_mini_object (GValue        *value,
                                           CdnMiniObject *mini_object);
CdnMiniObject *cdn_value_get_mini_object  (GValue const  *value);
CdnMiniObject *cdn_value_dup_mini_object  (GValue const  *value);

G_END_DECLS

#endif /* __CDN_MINI_OBJECT_H__ */
