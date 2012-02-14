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

#include <glib-object.h>
#include <codyn/cdn-object.h>
#include <codyn/integrators/cdn-integrator.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define CDN_TYPE_INPUT			(cdn_input_get_type ())
#define CDN_INPUT(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_INPUT, CdnInput))
#define CDN_IS_INPUT(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_INPUT))
#define CDN_INPUT_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_INPUT, CdnInputInterface))

typedef struct _CdnInput		CdnInput;
typedef struct _CdnInputInterface	CdnInputInterface;

struct _CdnInputInterface
{
	GTypeInterface parent;

	gboolean (*initialize)   (CdnInput            *input,
	                          GCancellable        *cancellable,
	                          GError             **error);

	void (*initialize_async) (CdnInput            *input,
	                          GCancellable        *cancellable,
	                          GAsyncReadyCallback  callback,
	                          gpointer             user_data);

	gboolean (*finalize)     (CdnInput            *input,
	                          GCancellable        *cancellable,
	                          GError             **error);

	void (*finalize_async)   (CdnInput            *input,
	                          GCancellable        *cancellable,
	                          GAsyncReadyCallback  callback,
	                          gpointer             user_data);

	void (*update)           (CdnInput            *input,
	                          CdnIntegrator       *integrator);
};

GType    cdn_input_get_type          (void) G_GNUC_CONST;

void     cdn_input_update            (CdnInput             *input,
                                      CdnIntegrator        *integrator);

gboolean cdn_input_initialize        (CdnInput             *input,
                                      GCancellable         *cancellable,
                                      GError              **error);

void     cdn_input_initialize_async  (CdnInput             *input,
                                      GCancellable         *cancellable,
                                      GAsyncReadyCallback   callback,
                                      gpointer              user_data);

gboolean cdn_input_initialize_finish (CdnInput             *input,
                                      GAsyncResult         *result,
                                      GError              **error);

gboolean cdn_input_finalize          (CdnInput             *input,
                                      GCancellable         *cancellable,
                                      GError              **error);

void     cdn_input_finalize_async    (CdnInput             *input,
                                      GCancellable         *cancellable,
                                      GAsyncReadyCallback   callback,
                                      gpointer              user_data);

gboolean cdn_input_finalize_finish   (CdnInput             *input,
                                      GAsyncResult         *result,
                                      GError              **error);

G_END_DECLS

#endif /* __CDN_INPUT_H__ */
