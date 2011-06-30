/*
 * cpg-network-deserializer.h
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

#ifndef __CPG_NETWORK_DESERIALIZER_H__
#define __CPG_NETWORK_DESERIALIZER_H__

#include <gio/gio.h>
#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK_DESERIALIZER			(cpg_network_deserializer_get_type ())
#define CPG_NETWORK_DESERIALIZER(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializer))
#define CPG_NETWORK_DESERIALIZER_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializer const))
#define CPG_NETWORK_DESERIALIZER_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializerClass))
#define CPG_IS_NETWORK_DESERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK_DESERIALIZER))
#define CPG_IS_NETWORK_DESERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK_DESERIALIZER))
#define CPG_NETWORK_DESERIALIZER_GET_CLASS(obj)		(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK_DESERIALIZER, CpgNetworkDeserializerClass))

typedef struct _CpgNetworkDeserializer		CpgNetworkDeserializer;
typedef struct _CpgNetworkDeserializerClass	CpgNetworkDeserializerClass;
typedef struct _CpgNetworkDeserializerPrivate	CpgNetworkDeserializerPrivate;

struct _CpgNetworkDeserializer
{
	/*< private >*/
	GObject parent;

	CpgNetworkDeserializerPrivate *priv;
};

struct _CpgNetworkDeserializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_network_deserializer_get_type (void) G_GNUC_CONST;
CpgNetworkDeserializer *cpg_network_deserializer_new (CpgNetwork *network,
                                                      CpgGroup   *root);

gboolean cpg_network_deserializer_deserialize (CpgNetworkDeserializer  *deserializer,
                                               GFile                   *file,
                                               GInputStream            *stream,
                                               GError                 **error);

gboolean cpg_network_deserializer_deserialize_path (CpgNetworkDeserializer  *deserializer,
                                                    const gchar             *path,
                                                    GError                 **error);

G_END_DECLS

#endif /* __CPG_NETWORK_DESERIALIZER_H__ */
