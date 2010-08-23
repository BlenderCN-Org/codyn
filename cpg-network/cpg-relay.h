/*
 * cpg-relay.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2010 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_RELAY_H__
#define __CPG_RELAY_H__

#include <glib-object.h>
#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_RELAY				(cpg_relay_get_type ())
#define CPG_RELAY(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_RELAY, CpgRelay))
#define CPG_RELAY_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_RELAY, CpgRelay const))
#define CPG_RELAY_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_RELAY, CpgRelayClass))
#define CPG_IS_RELAY(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_RELAY))
#define CPG_IS_RELAY_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_RELAY))
#define CPG_RELAY_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_RELAY, CpgRelayClass))

typedef struct _CpgRelay		CpgRelay;
typedef struct _CpgRelayClass	CpgRelayClass;
typedef struct _CpgRelayPrivate	CpgRelayPrivate;

struct _CpgRelay {
	/*< private >*/
	CpgObject parent;
	
	CpgRelayPrivate *priv;
};

struct _CpgRelayClass {
	/*< private >*/
	CpgObjectClass parent_class;
};

GType cpg_relay_get_type(void) G_GNUC_CONST;
CpgRelay *cpg_relay_new(const gchar *id);

G_END_DECLS

#endif /* __CPG_RELAY_H__ */
