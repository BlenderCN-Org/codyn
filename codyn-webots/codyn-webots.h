/*
 * codyn-webots.h
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

#ifndef __CDN_NETWORK_WEBOTS_H__
#define __CDN_NETWORK_WEBOTS_H__

#include <glib-object.h>
#include <codyn/codyn.h>

G_BEGIN_DECLS

#define CDN_TYPE_NETWORK_WEBOTS			(cdn_network_webots_get_type ())
#define CDN_NETWORK_WEBOTS(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_WEBOTS, CdnNetworkWebots))
#define CDN_NETWORK_WEBOTS_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_NETWORK_WEBOTS, CdnNetworkWebots const))
#define CDN_NETWORK_WEBOTS_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_NETWORK_WEBOTS, CdnNetworkWebotsClass))
#define CDN_IS_NETWORK_WEBOTS(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_NETWORK_WEBOTS))
#define CDN_IS_NETWORK_WEBOTS_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_NETWORK_WEBOTS))
#define CDN_NETWORK_WEBOTS_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_NETWORK_WEBOTS, CdnNetworkWebotsClass))

typedef struct _CdnNetworkWebots	CdnNetworkWebots;
typedef struct _CdnNetworkWebotsClass	CdnNetworkWebotsClass;
typedef struct _CdnNetworkWebotsPrivate	CdnNetworkWebotsPrivate;

struct _CdnNetworkWebots
{
	/*< private >*/
	GObject parent;

	CdnNetworkWebotsPrivate *priv;
};

struct _CdnNetworkWebotsClass
{
	/*< private >*/
	GObjectClass parent_class;
};

GType cdn_network_webots_get_type (void) G_GNUC_CONST;

CdnNetworkWebots *cdn_network_webots_new           (CdnNetwork       *network,
                                                    guint             basic_time_step);

void              cdn_network_webots_read_inputs (CdnNetworkWebots *webots);
void              cdn_network_webots_write_outputs (CdnNetworkWebots *webots);

G_END_DECLS

#endif /* __CDN_NETWORK_WEBOTS_H__ */

