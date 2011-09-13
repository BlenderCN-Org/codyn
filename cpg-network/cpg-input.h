/*
 * cpg-input.h
 * This file is part of cpg-network
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * cpg-network is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * cpg-network is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with cpg-network; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#ifndef __CPG_INPUT_H__
#define __CPG_INPUT_H__

#include <cpg-network/cpg-object.h>
#include <cpg-network/cpg-integrator.h>

G_BEGIN_DECLS

#define CPG_TYPE_INPUT			(cpg_input_get_type ())
#define CPG_INPUT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INPUT, CpgInput))
#define CPG_INPUT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_INPUT, CpgInput const))
#define CPG_INPUT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_INPUT, CpgInputClass))
#define CPG_IS_INPUT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_INPUT))
#define CPG_IS_INPUT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_INPUT))
#define CPG_INPUT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_INPUT, CpgInputClass))

typedef struct _CpgInput	CpgInput;
typedef struct _CpgInputClass	CpgInputClass;
typedef struct _CpgInputPrivate	CpgInputPrivate;

struct _CpgInput
{
	/*< private >*/
	CpgObject parent;

	CpgInputPrivate *priv;

	/*< public >*/
};

struct _CpgInputClass
{
	/*< private >*/
	CpgObjectClass parent_class;

	/*< public >*/
	void (*update) (CpgInput *input, CpgIntegrator *integrator);
};

GType cpg_input_get_type (void) G_GNUC_CONST;

void cpg_input_update (CpgInput *input, CpgIntegrator *integrator);

G_END_DECLS

#endif /* __CPG_INPUT_H__ */
