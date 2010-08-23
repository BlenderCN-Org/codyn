/*
 * cpg-state.h
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

#ifndef __CPG_STATE_H__
#define __CPG_STATE_H__

#include <cpg-network/cpg-object.h>

G_BEGIN_DECLS

#define CPG_TYPE_STATE				(cpg_state_get_type ())
#define CPG_STATE(obj)				(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_STATE, CpgState))
#define CPG_STATE_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_STATE, CpgState const))
#define CPG_STATE_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_STATE, CpgStateClass))
#define CPG_IS_STATE(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_STATE))
#define CPG_IS_STATE_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_STATE))
#define CPG_STATE_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_STATE, CpgStateClass))

typedef struct _CpgState		CpgState;
typedef struct _CpgStateClass	CpgStateClass;
typedef struct _CpgStatePrivate	CpgStatePrivate;

struct _CpgState {
	/*< private >*/
	CpgObject parent;
	
	CpgStatePrivate *priv;
};

struct _CpgStateClass {
	/*< private >*/
	CpgObjectClass parent_class;
};

GType cpg_state_get_type(void) G_GNUC_CONST;
CpgState *cpg_state_new(gchar const *id);

G_END_DECLS

#endif /* __CPG_STATE_H__ */
