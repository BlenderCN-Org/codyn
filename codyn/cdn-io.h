/*
 * cdn-io.h
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

#ifndef __CDN_IO_H__
#define __CDN_IO_H__

#include <glib-object.h>
#include <codyn/cdn-object.h>
#include <codyn/integrators/cdn-integrator.h>
#include <gio/gio.h>

G_BEGIN_DECLS

#define CDN_TYPE_IO			(cdn_io_get_type ())
#define CDN_IO(obj)			(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_IO, CdnIo))
#define CDN_IS_IO(obj)			(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_IO))
#define CDN_IO_GET_INTERFACE(obj)	(G_TYPE_INSTANCE_GET_INTERFACE ((obj), CDN_TYPE_IO, CdnIoInterface))

typedef struct _CdnIo		CdnIo;
typedef struct _CdnIoInterface	CdnIoInterface;

typedef enum
{
	CDN_IO_MODE_INPUT = 1 << 0,
	CDN_IO_MODE_OUTPUT = 1 << 1,
	CDN_IO_MODE_INPUT_OUTPUT = CDN_IO_MODE_INPUT | CDN_IO_MODE_OUTPUT
} CdnIoMode;

struct _CdnIoInterface
{
	GTypeInterface parent;

	gboolean (*initialize)   (CdnIo               *io,
	                          GCancellable        *cancellable,
	                          GError             **error);

	void (*initialize_async) (CdnIo               *io,
	                          GCancellable        *cancellable,
	                          GAsyncReadyCallback  callback,
	                          gpointer             user_data);

	gboolean (*finalize)     (CdnIo               *io,
	                          GCancellable        *cancellable,
	                          GError             **error);

	void (*finalize_async)   (CdnIo               *io,
	                          GCancellable        *cancellable,
	                          GAsyncReadyCallback  callback,
	                          gpointer             user_data);

	void (*update)           (CdnIo               *io,
	                          CdnIntegrator       *integrator);
};

GType    cdn_io_get_type          (void) G_GNUC_CONST;

void     cdn_io_update            (CdnIo                *io,
                                   CdnIntegrator        *integrator);

gboolean cdn_io_initialize        (CdnIo                *io,
                                   GCancellable         *cancellable,
                                   GError              **error);

void     cdn_io_initialize_async  (CdnIo                *io,
                                   GCancellable         *cancellable,
                                   GAsyncReadyCallback   callback,
                                   gpointer              user_data);

gboolean cdn_io_initialize_finish (CdnIo                *io,
                                   GAsyncResult         *result,
                                   GError              **error);

gboolean cdn_io_finalize          (CdnIo                *io,
                                   GCancellable         *cancellable,
                                   GError              **error);

void     cdn_io_finalize_async    (CdnIo                *io,
                                   GCancellable         *cancellable,
                                   GAsyncReadyCallback   callback,
                                   gpointer              user_data);

gboolean cdn_io_finalize_finish   (CdnIo                *io,
                                   GAsyncResult         *result,
                                   GError              **error);

CdnIoMode cdn_io_get_mode         (CdnIo                *io);

G_END_DECLS

#endif /* __CDN_IO_H__ */
