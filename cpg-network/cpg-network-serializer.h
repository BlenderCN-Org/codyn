/*
 * cpg-network-serializer.h
 * This file is part of cpg-network
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

#ifndef __CPG_NETWORK_SERIALIZER_H__
#define __CPG_NETWORK_SERIALIZER_H__

#include <gio/gio.h>
#include <cpg-network/cpg-network.h>

G_BEGIN_DECLS

#define CPG_TYPE_NETWORK_SERIALIZER		(cpg_network_serializer_get_type ())
#define CPG_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializer))
#define CPG_NETWORK_SERIALIZER_CONST(obj)	(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializer const))
#define CPG_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializerClass))
#define CPG_IS_NETWORK_SERIALIZER(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_NETWORK_SERIALIZER))
#define CPG_IS_NETWORK_SERIALIZER_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_NETWORK_SERIALIZER))
#define CPG_NETWORK_SERIALIZER_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_NETWORK_SERIALIZER, CpgNetworkSerializerClass))

typedef struct _CpgNetworkSerializer		CpgNetworkSerializer;
typedef struct _CpgNetworkSerializerClass	CpgNetworkSerializerClass;
typedef struct _CpgNetworkSerializerPrivate	CpgNetworkSerializerPrivate;

struct _CpgNetworkSerializer
{
	/*< private >*/
	GObject parent;

	CpgNetworkSerializerPrivate *priv;
};

struct _CpgNetworkSerializerClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cpg_network_serializer_get_type (void) G_GNUC_CONST;

CpgNetworkSerializer *cpg_network_serializer_new (CpgNetwork *network,
                                                  CpgGroup   *root);

gboolean cpg_network_serializer_serialize (CpgNetworkSerializer  *serializer,
                                           GOutputStream         *stream,
                                           GError               **error);

gboolean cpg_network_serializer_serialize_file (CpgNetworkSerializer  *serializer,
                                                GFile                 *file,
                                                GError               **error);

gboolean cpg_network_serializer_serialize_path (CpgNetworkSerializer  *serializer,
                                                const gchar           *path,
                                                GError               **error);

gchar *cpg_network_serializer_serialize_memory (CpgNetworkSerializer  *serializer,
                                                GError               **error);

G_END_DECLS

#endif /* __CPG_NETWORK_SERIALIZER_H__ */
