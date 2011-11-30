/*
 * cdn-input.h
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

#ifndef __CDN_INPUT_H__
#define __CDN_INPUT_H__

#include <codyn/cdn-object.h>
#include <codyn/integrators/cdn-integrator.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT			(cdn_input_get_type ())
#define CDN_INPUT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT, CdnInput))
#define CDN_INPUT_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT, CdnInput const))
#define CDN_INPUT_CLASS(klass)		(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_INPUT, CdnInputClass))
#define CDN_IS_INPUT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT))
#define CDN_IS_INPUT_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_INPUT))
#define CDN_INPUT_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_INPUT, CdnInputClass))

typedef struct _CdnInput	CdnInput;
typedef struct _CdnInputClass	CdnInputClass;
typedef struct _CdnInputPrivate	CdnInputPrivate;

struct _CdnInput
{
	/*< private >*/
	CdnObject parent;

	CdnInputPrivate *priv;

	/*< public >*/
};

struct _CdnInputClass
{
	/*< private >*/
	CdnObjectClass parent_class;

	/*< public >*/
	void (*update) (CdnInput *input, CdnIntegrator *integrator);
};

GType cdn_input_get_type (void) G_GNUC_CONST;

void cdn_input_update (CdnInput *input, CdnIntegrator *integrator);

G_END_DECLS

#endif /* __CDN_INPUT_H__ */
