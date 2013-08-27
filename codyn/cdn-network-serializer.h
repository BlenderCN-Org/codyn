/*
 * cdn-network-serializer.h
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

#ifndef __CDN_NETWORK_SERIALIZER_H__
#define __CDN_NETWORK_SERIALIZER_H__

#include <gio/gio.h>
#include <codyn/cdn-network.h>

G_BEGIN_DECLS

#define CDN_TYPE_NETWORK_SERIALIZER		(cdn_network_serializer_get_type ())
#define CDN_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_SERIALIZER, CdnNetworkSerializer))
#define CDN_NETWORK_SERIALIZER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_SERIALIZER, CdnNetworkSerializer const))
#define CDN_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NETWORK_SERIALIZER, CdnNetworkSerializerClass))
#define CDN_IS_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NETWORK_SERIALIZER))
#define CDN_IS_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NETWORK_SERIALIZER))
#define CDN_NETWORK_SERIALIZER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NETWORK_SERIALIZER, CdnNetworkSerializerClass))

typedef struct _CdnNetworkSerializer		CdnNetworkSerializer;
typedef struct _CdnNetworkSerializerClass	CdnNetworkSerializerClass;
typedef struct _CdnNetworkSerializerPrivate	CdnNetworkSerializerPrivate;

/**
 * CdnNetworkSerializer:
 *
 * Network to XML serializer.
 *
 * This can be used to serialize a #CdnNetwork to XML.
 */
struct _CdnNetworkSerializer
{
	/*< private >*/
	GObject parent;

	CdnNetworkSerializerPrivate *priv;
};

struct _CdnNetworkSerializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_network_serializer_get_type (void) G_GNUC_CONST;

CdnNetworkSerializer *cdn_network_serializer_new (CdnNetwork *network,
                                                  CdnNode   *root);

gboolean cdn_network_serializer_serialize (CdnNetworkSerializer  *serializer,
                                           GOutputStream         *stream,
                                           GError               **error);

gboolean cdn_network_serializer_serialize_file (CdnNetworkSerializer  *serializer,
                                                GFile                 *file,
                                                GError               **error);

gboolean cdn_network_serializer_serialize_path (CdnNetworkSerializer  *serializer,
                                                const gchar           *path,
                                                GError               **error);

gchar *cdn_network_serializer_serialize_memory (CdnNetworkSerializer  *serializer,
                                                GError               **error);

G_END_DECLS

#endif /* __CDN_NETWORK_SERIALIZER_H__ */
