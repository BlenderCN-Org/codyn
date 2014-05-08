/*
 * cdn-network-deserializer.h
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

#ifndef __CDN_NETWORK_DESERIALIZER_H__
#define __CDN_NETWORK_DESERIALIZER_H__

#include <gio/gio.h>
#include <codyn/cdn-network.h>

G_BEGIN_DECLS

#define CDN_TYPE_NETWORK_DESERIALIZER			(cdn_network_deserializer_get_type ())
#define CDN_NETWORK_DESERIALIZER(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_DESERIALIZER, CdnNetworkDeserializer))
#define CDN_NETWORK_DESERIALIZER_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_DESERIALIZER, CdnNetworkDeserializer const))
#define CDN_NETWORK_DESERIALIZER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NETWORK_DESERIALIZER, CdnNetworkDeserializerClass))
#define CDN_IS_NETWORK_DESERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NETWORK_DESERIALIZER))
#define CDN_IS_NETWORK_DESERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NETWORK_DESERIALIZER))
#define CDN_NETWORK_DESERIALIZER_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NETWORK_DESERIALIZER, CdnNetworkDeserializerClass))

typedef struct _CdnNetworkDeserializer		CdnNetworkDeserializer;
typedef struct _CdnNetworkDeserializerClass	CdnNetworkDeserializerClass;
typedef struct _CdnNetworkDeserializerPrivate	CdnNetworkDeserializerPrivate;

/**
 * CdnNetworkDeserializer:
 *
 * XML to Network deserializer.
 *
 * Use this to deserialize an XML description of a network to a #CdnNetwork.
 */
struct _CdnNetworkDeserializer
{
	/*< private >*/
	GObject parent;

	CdnNetworkDeserializerPrivate *priv;
};

struct _CdnNetworkDeserializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_network_deserializer_get_type (void) G_GNUC_CONST;
CdnNetworkDeserializer *cdn_network_deserializer_new (CdnNetwork *network,
                                                      CdnNode   *root);

gboolean cdn_network_deserializer_deserialize (CdnNetworkDeserializer  *deserializer,
                                               GFile                   *file,
                                               GInputStream            *stream,
                                               GError                 **error);

gboolean cdn_network_deserializer_deserialize_path (CdnNetworkDeserializer  *deserializer,
                                                    const gchar             *path,
                                                    GError                 **error);

G_END_DECLS

#endif /* __CDN_NETWORK_DESERIALIZER_H__ */
