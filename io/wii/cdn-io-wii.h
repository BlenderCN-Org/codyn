/*
 * cdn-io-wii.h
 * This file is part of codyn
 *
 * Copyright (C) 2012 - Jesse van den Kieboom
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

#ifndef __CDN_IO_WII_H__
#define __CDN_IO_WII_H__

#include <codyn/cdn-node.h>

G_BEGIN_DECLS

#define CDN_TYPE_IO_WII			(cdn_io_wii_get_type ())
#define CDN_IO_WII(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_WII, CdnIoWii))
#define CDN_IO_WII_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO_WII, CdnIoWii const))
#define CDN_IO_WII_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_IO_WII, CdnIoWiiClass))
#define CDN_IS_IO_WII(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IO_WII))
#define CDN_IS_IO_WII_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_IO_WII))
#define CDN_IO_WII_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_IO_WII, CdnIoWiiClass))

#define CDN_IO_WII_ERROR (cdn_io_wii_error_quark ())

typedef enum
{
	CDN_IO_WII_ERROR_DEVICE_NOT_FOUND
} CdnIoWiiError;

typedef struct _CdnIoWii		CdnIoWii;
typedef struct _CdnIoWiiClass		CdnIoWiiClass;
typedef struct _CdnIoWiiPrivate		CdnIoWiiPrivate;

struct _CdnIoWii
{
	/*< private >*/
	CdnNode parent;

	CdnIoWiiPrivate *priv;
};

struct _CdnIoWiiClass
{
	/*< private >*/
	CdnNodeClass parent_class;
};

GType  cdn_io_wii_get_type    (void) G_GNUC_CONST;
GQuark cdn_io_wii_error_quark (void);

void   cdn_io_wii_register    (GTypeModule *module);

G_END_DECLS

#endif /* __CDN_IO_WII_H__ */
