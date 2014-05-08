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

/**
 * CdnIo:
 *
 * Io module interface.
 *
 * #CdnIo is an interface which can be implemented to add new types
 * of IO to codyn. Once implemented and registered, the new IO type
 * is also available in the codyn modeling language and can be easily
 * instantiated. This is useful for direct integration of external
 * input/output in codyn, such that all existing codyn tools can still
 * be used.
 *
 * A #CdnIo interface implementation must be a subclass of #CdnObject.
 * Any input or output values must be represented by #CdnVariable
 * in the object, which allows them to be accessed from the network.
 * The #CdnIo implementation is thus simply another node in the network,
 * but one which updates its contents based on external IO.
 *
 * Usually, custom IO is implemented in a dynamically loaded module.
 * Such modules are compiled as shared libraries and export at least
 * one of the following methods:
 *
 * 1. cdn_input_<name>_get_type
 * 2. cdn_output_<name>_get_type
 * 3. cdn_io_<name>_get_type
 *
 * depending on whether the io module implements only input, only output
 * or both input and output. Multiple io types can be defined within the
 * same dynamic module and are identified by their name. The returned
 * #GType must be a concrete class which implements #CdnIo.
 *
 * Io objects are initialized before every run of a network and finalized
 * afterwards. Initialization and finalization is usually done asynchronously
 * such that all custom IO instantiations can be initialized and finalized
 * in parallel. The default implementation of #CdnIo::initialize_async and
 * #CdnIo::finalize_async will create a new thread in which #CdnIo::initialize and
 * #CdnIo::finalize are respectively executed. If needed, these default implementations
 * can be overridden to implement your own means of asynchronous initialization
 * and finalization.
 *
 * During numerical integration, #CdnIo::update is called which should update
 * any of the #CdnVariable that the io object exposes based on the current
 * integration time. It is up to the implementation whether or not it is required
 * to perform the actual updates asynchronously in the background.
 *
 */

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
	GSimpleAsyncResult *res;

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
	GSimpleAsyncResult *res;

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
		                                                         G_PARAM_CONSTRUCT |
		                                                         G_PARAM_STATIC_STRINGS));
	}
}

/**
 * cdn_io_initialize:
 * @io: the #CdnIo
 * @cancellable: a #GCancellable
 * @error: a #GError or %NULL
 *
 * Initialize the IO module. This will call the initialize method on the
 * #CdnIo interface.
 *
 * Returns: %TRUE if the io module has been initialized, %FALSE otherwise.
 *
 */
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

/**
 * cdn_io_initialize_async:
 * @io: the #CdnIo
 * @cancellable: a #GCancellable
 * @callback: a callback
 * @user_data: user data
 *
 * Initialize the IO module asynchronously. This will call the initialize_async
 * method on the #CdnIo interface. Once initialized, @callback will be called
 * with the specified @user_data. Call #cdn_io_initialize_finish from @callback
 * to finalize the asynchronous initialization and check for possible errors.
 *
 */
void
cdn_io_initialize_async (CdnIo               *io,
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

/**
 * cdn_io_initialize_finish:
 * @io: the #CdnIo
 * @result: the result
 * @error: a #GError or %NULL
 *
 * Finish the asynchronous initialization of the io.
 *
 * Returns: %TRUE if the io module has been initialized, %FALSE otherwise.
 *
 */
gboolean
cdn_io_initialize_finish (CdnIo         *io,
                          GAsyncResult  *result,
                          GError       **error)
{
	g_return_val_if_fail (CDN_IS_IO (io), FALSE);
	g_return_val_if_fail (G_IS_ASYNC_RESULT (result), FALSE);
	g_return_val_if_fail (g_simple_async_result_is_valid (result,
	                                                      G_OBJECT (io),
	                                                      cdn_io_initialize_async),
	                      FALSE);

	if (g_simple_async_result_propagate_error (G_SIMPLE_ASYNC_RESULT (result),
	                                           error))
	{
		return FALSE;
	}

	return TRUE;
}

/**
 * cdn_io_finalize:
 * @io: the #CdnIo
 * @cancellable: a #GCancellable
 * @error: a #GError or %NULL
 *
 * Finalize the IO module. This will call the finalize method on the
 * #CdnIo interface.
 *
 * Returns: %TRUE if finalization was successful, %FALSE otherwise
 *
 */
gboolean
cdn_io_finalize (CdnIo         *io,
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

/**
 * cdn_io_finalize_async:
 * @io: the #CdnIo
 * @cancellable: a #GCancellable
 * @callback: a callback
 * @user_data: user data
 *
 * Finalize the IO module asynchronously. This will call the finalize_async
 * method on the #CdnIo interface. Once finalized, @callback will be called
 * with the specified @user_data. Call #cdn_io_finalize_finish from @callback
 * to finalize the asynchronous finalization and check for possible errors.
 *
 */
void
cdn_io_finalize_async (CdnIo               *io,
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

/**
 * cdn_io_finalize_finish:
 * @io: the #CdnIo
 * @result: the result
 * @error: a #GError or %NULL
 *
 * Finish the asynchronous finalization of the io.
 *
 * Returns: %TRUE if the io module has been finalized, %FALSE otherwise.
 *
 */
gboolean
cdn_io_finalize_finish (CdnIo         *io,
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

/**
 * cdn_io_update:
 * @io: the #CdnIo
 * @integrator: the current #CdnIntegrator
 *
 * Update the IO. This will call the update method on the
 * #CdnIo interface.
 *
 */
void
cdn_io_update (CdnIo         *io,
               CdnIntegrator *integrator)
{
	CdnIoInterface *iface;

	iface = CDN_IO_GET_INTERFACE (io);

	if (iface->update)
	{
		iface->update (io, integrator);
	}
}

/**
 * cdn_io_get_mode:
 * @io: the #CdnIo
 *
 * Get the io mode of @io.
 *
 * Returns: the io mode
 *
 */
CdnIoMode
cdn_io_get_mode (CdnIo *io)
{
	CdnIoMode mode;

	g_return_val_if_fail (CDN_IS_IO (io), CDN_IO_MODE_INPUT);

	mode = CDN_IO_MODE_INPUT;

	g_object_get (io, "mode", &mode, NULL);
	return mode;
}
