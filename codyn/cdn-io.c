/*
 * cdn-io.c
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

#include "cdn-io.h"
#include "cdn-enum-types.h"

G_DEFINE_INTERFACE (CdnIo, cdn_io, CDN_TYPE_OBJECT)

static void
initialize_async_thread (GSimpleAsyncResult *res,
                         GObject            *object,
                         GCancellable       *cancellable)
{
	CdnIoInterface *iface;
	GError *error = NULL;

	iface = CDN_IO_GET_INTERFACE (object);

	if (iface->initialize == NULL)
	{
		g_set_error_literal (&error,
		                     G_IO_ERROR,
		                     G_IO_ERROR_NOT_SUPPORTED,
		                     "Operation not supported");

		g_simple_async_result_set_from_error (res, error);
		g_error_free (error);
		return;
	}

	if (!iface->initialize (CDN_IO (object), cancellable, &error))
	{
		g_simple_async_result_set_from_error (res, error);
		g_error_free (error);
	}
	else
	{
		g_simple_async_result_set_op_res_gboolean (res, TRUE);
	}
}

static void
initialize_async_real (CdnIo               *io,
                       GCancellable        *cancellable,
                       GAsyncReadyCallback  callback,
                       gpointer             user_data)
{
	CdnIoInterface *iface;
	GSimpleAsyncResult *res;

	iface = CDN_IO_GET_INTERFACE (io);

	res = g_simple_async_result_new (G_OBJECT (io),
	                                 callback,
	                                 user_data,
	                                 cdn_io_initialize_async);

	g_simple_async_result_run_in_thread (res,
	                                     initialize_async_thread,
	                                     G_PRIORITY_DEFAULT,
	                                     cancellable);

	g_object_unref (res);
}

static void
finalize_async_thread (GSimpleAsyncResult *res,
                       GObject            *object,
                       GCancellable       *cancellable)
{
	CdnIoInterface *iface;
	GError *error = NULL;

	iface = CDN_IO_GET_INTERFACE (object);

	if (iface->finalize == NULL)
	{
		g_set_error_literal (&error,
		                     G_IO_ERROR,
		                     G_IO_ERROR_NOT_SUPPORTED,
		                     "Operation not supported");

		g_simple_async_result_set_from_error (res, error);
		g_error_free (error);
		return;
	}

	if (!iface->finalize (CDN_IO (object), cancellable, &error))
	{
		g_simple_async_result_set_from_error (res, error);
		g_error_free (error);
	}
	else
	{
		g_simple_async_result_set_op_res_gboolean (res, TRUE);
	}
}

static void
finalize_async_real (CdnIo               *io,
                     GCancellable        *cancellable,
                     GAsyncReadyCallback  callback,
                     gpointer             user_data)
{
	CdnIoInterface *iface;
	GSimpleAsyncResult *res;

	iface = CDN_IO_GET_INTERFACE (io);

	res = g_simple_async_result_new (G_OBJECT (io),
	                                 callback,
	                                 user_data,
	                                 cdn_io_finalize_async);

	g_simple_async_result_run_in_thread (res,
	                                     finalize_async_thread,
	                                     G_PRIORITY_DEFAULT,
	                                     cancellable);

	g_object_unref (res);
}

static void
cdn_io_default_init (CdnIoInterface *iface)
{
	static gboolean initialized = FALSE;

	iface->initialize_async = initialize_async_real;
	iface->finalize_async = finalize_async_real;

	if (G_UNLIKELY (!initialized))
	{
		initialized = TRUE;

		g_object_interface_install_property (iface,
		                                     g_param_spec_flags ("mode",
		                                                         "Mode",
		                                                         "Mode",
		                                                         CDN_TYPE_IO_MODE,
		                                                         CDN_IO_MODE_INPUT,
		                                                         G_PARAM_READWRITE |
		                                                         G_PARAM_CONSTRUCT_ONLY |
		                                                         G_PARAM_STATIC_STRINGS));
	}
}

gboolean
cdn_io_initialize (CdnIo         *io,
                   GCancellable  *cancellable,
                   GError       **error)
{
	CdnIoInterface *iface;

	g_return_val_if_fail (CDN_IS_IO (io), FALSE);

	if (g_cancellable_set_error_if_cancelled (cancellable, error))
	{
		return FALSE;
	}

	iface = CDN_IO_GET_INTERFACE (io);

	if (iface->initialize == NULL)
	{
		g_set_error_literal (error,
		                     G_IO_ERROR,
		                     G_IO_ERROR_NOT_SUPPORTED,
		                     "Operation not supported");

		return FALSE;
	}

	return iface->initialize (io, cancellable, error);
}

void
cdn_io_initialize_async (CdnIo            *io,
                            GCancellable        *cancellable,
                            GAsyncReadyCallback  callback,
                            gpointer             user_data)
{
	CdnIoInterface *iface;

	g_return_if_fail (CDN_IS_IO (io));
	g_return_if_fail (cancellable == NULL || G_IS_CANCELLABLE (cancellable));
	g_return_if_fail (callback != NULL);

	iface = CDN_IO_GET_INTERFACE (io);

	iface->initialize_async (io, cancellable, callback, user_data);
}

gboolean
cdn_io_initialize_finish (CdnIo      *io,
                             GAsyncResult  *result,
                             GError       **error)
{
	g_return_val_if_fail (CDN_IS_IO (io), FALSE);
	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (g_simple_async_result_is_valid (result,
	                                                      G_OBJECT (io),
	                                                      cdn_io_finalize_async),
	                      FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result),
	                                           error))
	{
		return FALSE;
	}

	return TRUE;
}

gboolean
cdn_io_finalize (CdnIo      *io,
                    GCancellable  *cancellable,
                    GError       **error)
{
	CdnIoInterface *iface;

	g_return_val_if_fail (CDN_IS_IO (io), FALSE);

	if (g_cancellable_set_error_if_cancelled (cancellable, error))
	{
		return FALSE;
	}

	iface = CDN_IO_GET_INTERFACE (io);

	if (iface->finalize == NULL)
	{
		g_set_error_literal (error,
		                     G_IO_ERROR,
		                     G_IO_ERROR_NOT_SUPPORTED,
		                     "Operation not supported");

		return FALSE;
	}

	return iface->finalize (io, cancellable, error);
}

void
cdn_io_finalize_async (CdnIo            *io,
                          GCancellable        *cancellable,
                          GAsyncReadyCallback  callback,
                          gpointer             user_data)
{
	CdnIoInterface *iface;

	g_return_if_fail (CDN_IS_IO (io));
	g_return_if_fail (cancellable == NULL || G_IS_CANCELLABLE (cancellable));
	g_return_if_fail (callback != NULL);

	iface = CDN_IO_GET_INTERFACE (io);

	iface->finalize_async (io, cancellable, callback, user_data);
}

gboolean
cdn_io_finalize_finish (CdnIo      *io,
                           GAsyncResult  *result,
                           GError       **error)
{
	g_return_val_if_fail (CDN_IS_IO (io), FALSE);
	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (g_simple_async_result_is_valid (result,
	                                                      G_OBJECT (io),
	                                                      cdn_io_finalize_async),
	                      FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result),
	                                           error))
	{
		return FALSE;
	}

	return TRUE;
}

void
cdn_io_update (CdnIo      *io,
                  CdnIntegrator *integrator)
{
	CdnIoInterface *iface;

	g_return_if_fail (CDN_IS_IO (io));
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));

	iface = CDN_IO_GET_INTERFACE (io);

	if (iface->update)
	{
		iface->update (io, integrator);
	}
}
