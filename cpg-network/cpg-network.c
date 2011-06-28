/*
 * cpg-network.c
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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#include "cpg-network.h"
#include "cpg-expression.h"
#include "cpg-object.h"
#include "cpg-link.h"
#include "cpg-integrator-euler.h"
#include "cpg-network-deserializer.h"
#include "cpg-operator-delayed.h"
#include "cpg-import.h"
#include "cpg-parser-context.h"

/**
 * SECTION:cpg-network
 * @short_description: The main CPG network object
 *
 * The cpg network is the main component of the cpg-network library. The network
 * consists of #CpgObject and #CpgLink objects which combined make
 * up the network.
 *
 * The easiest way of using the library is to write the network using the
 * XML representation (see #xml-specification). You then create the network
 * from file using #cpg_network_new_from_file. To simulate the network, use
 * #cpg_network_run or for running single steps #cpg_network_step.
 *
 * For more information, see
 * <link linkend='making-a-network'>Making a network</link>.
 *
 */

#define CPG_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CPG_TYPE_NETWORK, CpgNetworkPrivate))

/* Properties */
enum
{
	PROP_0,
	PROP_INTEGRATOR,
	PROP_FILE,
	PROP_FILENAME
};

struct _CpgNetworkPrivate
{
	GFile *file;

	CpgIntegrator *integrator;
	CpgIntegratorState *integrator_state;

	CpgGroup *template_group;
	CpgGroup *function_group;

	GSList *operators;

	GHashTable *imports;
};

enum
{
	COMPILE_ERROR,
	NUM_SIGNALS
};

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CpgNetwork, cpg_network, CPG_TYPE_GROUP)

static void on_import_parent_changed (CpgImport  *import,
                                      GParamSpec *spec,
                                      CpgNetwork *network);

GQuark
cpg_network_load_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_network_load_error");
	}

	return quark;
}

GQuark
cpg_network_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cpg_network_error");
	}

	return quark;
}

static gboolean
remove_destroyed_import (gpointer key,
                         gpointer value,
                         gpointer data)
{
	return value == data;
}

static void
on_registered_import_destroyed (CpgNetwork *network,
                                gpointer    data)
{
	g_hash_table_foreach_remove (network->priv->imports,
	                             (GHRFunc)remove_destroyed_import,
	                             data);
}

static void
unregister_import (CpgNetwork *network,
                   CpgImport  *import)
{
	g_object_weak_unref (G_OBJECT (import),
	                     (GWeakNotify)on_registered_import_destroyed,
	                     network);

	g_signal_handlers_disconnect_by_func (import,
	                                      G_CALLBACK (on_import_parent_changed),
	                                      network);
}

static void
unregister_all_imports (GFile      *file,
                        CpgImport  *import,
                        CpgNetwork *network)
{
	unregister_import (network, import);
}

static void
cpg_network_finalize (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (network->priv->file)
	{
		g_object_unref (network->priv->file);
	}

	cpg_object_clear (CPG_OBJECT (network));

	g_object_unref (network->priv->template_group);
	g_object_unref (network->priv->function_group);

	g_slist_foreach (network->priv->operators, (GFunc)g_object_unref, NULL);
	g_slist_free (network->priv->operators);

	g_hash_table_foreach (network->priv->imports,
	                      (GHFunc)unregister_all_imports,
	                      network);

	g_hash_table_destroy (network->priv->imports);

	G_OBJECT_CLASS (cpg_network_parent_class)->finalize (object);
}

static void
cpg_network_get_property (GObject     *object,
                          guint        prop_id,
                          GValue      *value,
                          GParamSpec  *pspec)
{
	CpgNetwork *self = CPG_NETWORK (object);

	switch (prop_id)
	{
		case PROP_INTEGRATOR:
			g_value_set_object (value, self->priv->integrator);
		break;
		case PROP_FILE:
			g_value_set_object (value, self->priv->file);
		break;
		case PROP_FILENAME:
			if (self->priv->file)
			{
				g_value_take_string (value, g_file_get_path (self->priv->file));
			}
			else
			{
				g_value_set_string (value, NULL);
			}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
set_integrator (CpgNetwork    *network,
                CpgIntegrator *integrator)
{
	if (network->priv->integrator == integrator)
	{
		return;
	}

	if (network->priv->integrator)
	{
		g_object_unref (network->priv->integrator);
		network->priv->integrator = NULL;
	}

	if (integrator)
	{
		network->priv->integrator = g_object_ref (integrator);

		g_object_set (network->priv->integrator,
		              "object", network,
		              NULL);

		cpg_integrator_set_state (integrator,
		                          network->priv->integrator_state);
	}

	cpg_object_reset (CPG_OBJECT (network));

	g_object_notify (G_OBJECT (network), "integrator");

}

static void
set_file (CpgNetwork *network,
          GFile      *file)
{
	if (network->priv->file)
	{
		g_object_unref (network->priv->file);
		network->priv->file = NULL;
	}

	if (file)
	{
		network->priv->file = g_file_dup (file);
	}

	g_object_notify (G_OBJECT (network), "file");
}

static void
cpg_network_set_property (GObject       *object,
                          guint          prop_id,
                          const GValue  *value,
                          GParamSpec    *pspec)
{
	CpgNetwork *self = CPG_NETWORK (object);

	switch (prop_id)
	{
		case PROP_INTEGRATOR:
			set_integrator (self, CPG_INTEGRATOR (g_value_get_object (value)));
		break;
		case PROP_FILE:
		{
			set_file (self, G_FILE (g_value_get_object (value)));
		}
		break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cpg_network_dispose (GObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (network->priv->integrator)
	{
		g_object_unref (network->priv->integrator);
		network->priv->integrator = NULL;
	}

	if (network->priv->integrator_state)
	{
		g_object_unref (network->priv->integrator_state);
		network->priv->integrator_state = NULL;
	}

	G_OBJECT_CLASS (cpg_network_parent_class)->dispose (object);
}

static gboolean
template_exists (CpgGroup  *group,
                 CpgObject *template)
{
	GSList const *children;

	if (CPG_OBJECT (group) == template)
	{
		return TRUE;
	}

	children = cpg_group_get_children (group);

	while (children)
	{
		CpgObject *child;

		child = children->data;

		if (child == template)
		{
			return TRUE;
		}

		if (CPG_IS_GROUP (child))
		{
			if (template_exists (CPG_GROUP (child), template))
			{
				return TRUE;
			}
		}

		children = g_slist_next (children);
	}

	return FALSE;
}

static gboolean
cpg_network_add_impl (CpgGroup   *group,
                      CpgObject  *object,
                      GError    **error)
{
	CpgNetwork *network = CPG_NETWORK (group);

	/* Check if the network owns all the templates */
	GSList const *templates = cpg_object_get_applied_templates (object);

	while (templates)
	{
		CpgObject *template = templates->data;

		if (!template_exists (network->priv->template_group, template))
		{
			g_set_error (error,
			             CPG_NETWORK_ERROR,
			             CPG_NETWORK_ERROR_UNOWNED_TEMPLATE,
			             "The object `%s' contains template `%s' which is not part of the network",
			             cpg_object_get_id (object),
			             cpg_object_get_id (template));

			return FALSE;
		}

		templates = g_slist_next (templates);
	}

	if (CPG_GROUP_CLASS (cpg_network_parent_class)->add (group, object, error))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

static void
cpg_network_reset_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	cpg_object_reset (CPG_OBJECT (network->priv->function_group));

	CPG_OBJECT_CLASS (cpg_network_parent_class)->reset (object);
}

typedef struct
{
	CpgNetwork *network;
	CpgCompileContext *context;
	CpgCompileError *error;
	gboolean failed;
} CompileInfo;

static gboolean
cpg_network_compile_impl (CpgObject         *object,
                          CpgCompileContext *context,
                          CpgCompileError   *error)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (!context)
	{
		context = cpg_compile_context_new ();
	}
	else
	{
		g_object_ref (context);
	}

	cpg_compile_context_prepend_object (context, CPG_OBJECT (network->priv->integrator));
	cpg_compile_context_prepend_object (context, object);

	cpg_compile_context_set_functions (context,
	                                   cpg_group_get_children (network->priv->function_group));

	gboolean ret = cpg_object_compile (CPG_OBJECT (network->priv->function_group),
	                                   context,
	                                   error);

	if (ret)
	{
		ret = CPG_OBJECT_CLASS (cpg_network_parent_class)->compile (object,
		                                                            context,
		                                                            error);
	}

	g_object_unref (context);

	if (!ret)
	{
		if (error)
		{
			g_signal_emit (network,
			               network_signals[COMPILE_ERROR],
			               0,
			               error);
		}
	}

	return ret;
}

static void
cpg_network_clear_impl (CpgObject *object)
{
	CpgNetwork *network = CPG_NETWORK (object);

	CPG_OBJECT_CLASS (cpg_network_parent_class)->clear (object);

	cpg_object_clear (CPG_OBJECT (network->priv->template_group));
	cpg_object_clear (CPG_OBJECT (network->priv->function_group));
}

static void
cpg_network_foreach_expression_impl (CpgObject                *object,
                                     CpgForeachExpressionFunc  func,
                                     gpointer                  userdata)
{
	CpgNetwork *network = CPG_NETWORK (object);

	if (CPG_OBJECT_CLASS (cpg_network_parent_class)->foreach_expression)
	{
		CPG_OBJECT_CLASS (cpg_network_parent_class)->foreach_expression (object,
		                                                                 func,
		                                                                 userdata);
	}

	cpg_object_foreach_expression (CPG_OBJECT (network->priv->function_group),
	                               func,
	                               userdata);
}

static void
cpg_network_class_init (CpgNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CpgGroupClass *group_class = CPG_GROUP_CLASS (klass);
	CpgObjectClass *cpg_class = CPG_OBJECT_CLASS (klass);

	object_class->finalize = cpg_network_finalize;
	object_class->dispose = cpg_network_dispose;
	object_class->get_property = cpg_network_get_property;
	object_class->set_property = cpg_network_set_property;

	cpg_class->reset = cpg_network_reset_impl;
	cpg_class->compile = cpg_network_compile_impl;
	cpg_class->clear = cpg_network_clear_impl;
	cpg_class->foreach_expression = cpg_network_foreach_expression_impl;

	group_class->add = cpg_network_add_impl;

	/**
	 * CpgNetwork::compile-error:
	 * @network: a #CpgNetwork
	 * @error: a #CpgCompileError
	 *
	 * Emitted when there is a compile error
	 *
	 **/
	network_signals[COMPILE_ERROR] =
		g_signal_new ("compile-error",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CpgNetworkClass,
		                               compile_error),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__BOXED,
		              G_TYPE_NONE,
		              1,
		              CPG_TYPE_COMPILE_ERROR);

	g_object_class_install_property (object_class,
	                                 PROP_INTEGRATOR,
	                                 g_param_spec_object ("integrator",
	                                                      "Integrator",
	                                                      "Integrator",
	                                                      CPG_TYPE_INTEGRATOR,
	                                                      G_PARAM_READWRITE));

	g_type_class_add_private (object_class, sizeof (CpgNetworkPrivate));


	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));


	g_object_class_install_property (object_class,
	                                 PROP_FILENAME,
	                                 g_param_spec_string ("filename",
	                                                      "Filename",
	                                                      "Filename",
	                                                      NULL,
	                                                      G_PARAM_READABLE));
}

static void
cpg_network_init (CpgNetwork *network)
{
	network->priv = CPG_NETWORK_GET_PRIVATE (network);

	network->priv->template_group = cpg_group_new ("templates", NULL);
	network->priv->function_group = cpg_group_new ("functions", NULL);

	g_signal_connect_swapped (network->priv->template_group,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          network);

	g_signal_connect_swapped (network->priv->function_group,
	                          "tainted",
	                          G_CALLBACK (cpg_object_taint),
	                          network);

	network->priv->integrator_state = cpg_integrator_state_new (CPG_OBJECT (network));

	/* Create default integrator */
	CpgIntegratorEuler *integrator = cpg_integrator_euler_new ();
	cpg_network_set_integrator (network, CPG_INTEGRATOR (integrator));
	g_object_unref (integrator);

	network->priv->operators = g_slist_prepend (network->priv->operators,
	                                            cpg_operator_delayed_new ());

	network->priv->imports = g_hash_table_new_full (g_file_hash,
	                                                (GEqualFunc)g_file_equal,
	                                                (GDestroyNotify)g_object_unref,
	                                                NULL);
}

/**
 * cpg_network_new:
 *
 * Create a new empty CPG network
 *
 * Return value: the newly created CPG network
 *
 **/
CpgNetwork *
cpg_network_new ()
{
	return g_object_new (CPG_TYPE_NETWORK, "id", "(cpg)", NULL);
}

static CpgNetworkFormat
format_from_seekable_stream (GInputStream *stream)
{
	gchar buffer[64];
	CpgNetworkFormat ret;

	ret = CPG_NETWORK_FORMAT_UNKNOWN;

	while (TRUE)
	{
		gssize r;
		gssize i;

		r = g_input_stream_read (stream,
		                         buffer,
		                         sizeof (buffer),
		                         NULL,
		                         NULL);

		if (!r)
		{
			break;
		}

		for (i = 0; i < r; ++i)
		{
			if (!g_ascii_isspace (buffer[i]))
			{
				ret = (buffer[i] == '<' ? CPG_NETWORK_FORMAT_XML
				                        : CPG_NETWORK_FORMAT_CPG);

				break;
			}
		}

		if (ret != CPG_NETWORK_FORMAT_UNKNOWN)
		{
			break;
		}
	}

	g_seekable_seek (G_SEEKABLE (stream),
	                 0,
	                 G_SEEK_SET,
	                 NULL,
	                 NULL);

	return ret;
}

static CpgNetworkFormat
format_from_buffered_stream (GBufferedInputStream *stream)
{
	/* Get the buffer size */
	gsize bufsize;
	gsize start;
	CpgNetworkFormat fmt;

	start = 0;
	bufsize = g_buffered_input_stream_get_buffer_size (stream);

	if (bufsize == 0)
	{
		/* Make sure initial buffer size is not 0 */
		bufsize = 64;
		g_buffered_input_stream_set_buffer_size (stream, bufsize);
	}

	fmt = CPG_NETWORK_FORMAT_UNKNOWN;

	while (TRUE)
	{
		gchar const *buf;
		gsize count;
		gsize ret;

		ret = g_buffered_input_stream_fill (stream,
		                                    bufsize,
		                                    NULL,
		                                    NULL);

		if (ret <= 0)
		{
			/* Either an error or EOF */
			break;
		}

		buf = g_buffered_input_stream_peek_buffer (stream, &count);

		while (start < count)
		{
			if (!g_ascii_isspace (buf[start]))
			{
				fmt = (buf[start] == '<' ? CPG_NETWORK_FORMAT_XML
				                         : CPG_NETWORK_FORMAT_CPG);

				break;
			}

			++start;
		}

		if (fmt != CPG_NETWORK_FORMAT_UNKNOWN)
		{
			break;
		}

		start = count;

		/* Read 64 more bytes */
		bufsize += 64;

		g_buffered_input_stream_set_buffer_size (stream, bufsize);
	}

	return fmt;
}

/**
 * cpg_network_format_from_stream:
 * @stream: A #GInputStream
 *
 * Determine the type of CPG format from a stream. This only works if either
 * the stream is seekable (see #GSeekable), or if the stream is a
 * #GBufferedInputStream. If needed, you can wrap your stream in a
 * #GBufferedInputStream before passing it.
 *
 * Returns: A #CpgNetworkFormat
 *
 **/
CpgNetworkFormat
cpg_network_format_from_stream (GInputStream *stream)
{
	if (G_IS_SEEKABLE (stream) &&
	    g_seekable_can_seek (G_SEEKABLE (stream)))
	{
		return format_from_seekable_stream (stream);
	}

	if (G_IS_BUFFERED_INPUT_STREAM (stream))
	{
		return format_from_buffered_stream (G_BUFFERED_INPUT_STREAM (stream));
	}

	return CPG_NETWORK_FORMAT_UNKNOWN;
}

/**
 * cpg_network_format_from_file:
 * @file: A #GFile
 *
 * Determine the type of CPG format from a file. If the type of the file
 * could not be determined, #CPG_NETWORK_FORMAT_UNKNOWN is returned. This
 * function only uses the mime type of a file. Use
 * #cpg_network_format_from_stream to determine the format from the contents.
 *
 * Returns: A #CpgNetworkFormat
 *
 **/
CpgNetworkFormat
cpg_network_format_from_file (GFile *file)
{
	GFileInfo *info;
	gchar const *ctype;
	CpgNetworkFormat ret;

	info = g_file_query_info (file,
	                          G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE,
	                          0,
	                          NULL,
	                          NULL);

	if (!info)
	{
		return CPG_NETWORK_FORMAT_UNKNOWN;
	}

	ctype = g_file_info_get_content_type (info);

	if (g_content_type_is_a (ctype, "application/xml"))
	{
		ret = CPG_NETWORK_FORMAT_XML;
	}
	else if (g_content_type_is_a (ctype, "text/x-cpg"))
	{
		ret = CPG_NETWORK_FORMAT_CPG;
	}
	else
	{
		ret = CPG_NETWORK_FORMAT_UNKNOWN;
	}

	g_object_unref (info);
	return ret;
}

/**
 * cpg_network_load_from_stream:
 * @network: A #CpgNetwork
 * @stream: The stream to load
 * @error: A #GError
 *
 * Load a network from a stream
 *
 * Returns: %TRUE if the stream could be loaded, %FALSE otherwise
 *
 **/
gboolean
cpg_network_load_from_stream (CpgNetwork    *network,
                              GInputStream  *stream,
                              GError       **error)
{
	gboolean ret;
	CpgNetworkFormat fmt;
	GInputStream *wrapped;

	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_INPUT_STREAM (stream), FALSE);

	cpg_object_clear (CPG_OBJECT (network));

	if (!(G_IS_SEEKABLE (stream) && g_seekable_can_seek (G_SEEKABLE (stream))) &&
	    !G_IS_BUFFERED_INPUT_STREAM (stream))
	{
		wrapped = g_buffered_input_stream_new (stream);
	}
	else
	{
		wrapped = g_object_ref (stream);
	}

	fmt = cpg_network_format_from_stream (wrapped);

	if (fmt == CPG_NETWORK_FORMAT_XML)
	{
		CpgNetworkDeserializer *deserializer;

		deserializer = cpg_network_deserializer_new (network,
		                                             NULL);

		ret = cpg_network_deserializer_deserialize (deserializer,
		                                            NULL,
		                                            wrapped,
		                                            error);

		g_object_unref (deserializer);
	}
	else
	{
		CpgParserContext *ctx;

		ctx = cpg_parser_context_new (network);
		cpg_parser_context_push_input (ctx, NULL, wrapped);

		ret = cpg_parser_context_parse (ctx, error);

		g_object_unref (ctx);
	}

	g_object_unref (wrapped);

	return ret;
}

/**
 * cpg_network_load_from_file:
 * @network: A #CpgNetwork
 * @file: The file to load
 * @error: A #GError
 *
 * Load a network from a file
 *
 * Returns: %TRUE if the file could be loaded, %FALSE otherwise
 *
 **/
gboolean
cpg_network_load_from_file (CpgNetwork  *network,
                            GFile       *file,
                            GError     **error)
{
	gboolean ret;
	CpgNetworkFormat fmt;
	GInputStream *stream;
	GFileInputStream *bstream;

	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_FILE (file), FALSE);

	set_file (network, file);
	stream = NULL;

	cpg_object_clear (CPG_OBJECT (network));

	bstream = g_file_read (file, NULL, NULL);

	fmt = CPG_NETWORK_FORMAT_UNKNOWN;

	if (bstream)
	{
		stream = g_buffered_input_stream_new (G_INPUT_STREAM (bstream));
		g_object_unref (bstream);

		fmt = cpg_network_format_from_stream (stream);
	}

	if (fmt == CPG_NETWORK_FORMAT_UNKNOWN)
	{
		fmt = cpg_network_format_from_file (file);
	}

	if (fmt == CPG_NETWORK_FORMAT_XML)
	{
		CpgNetworkDeserializer *deserializer;

		deserializer = cpg_network_deserializer_new (network,
		                                             NULL);

		ret = cpg_network_deserializer_deserialize (deserializer,
		                                            file,
		                                            stream,
		                                            error);

		g_object_unref (deserializer);
	}
	else
	{
		CpgParserContext *ctx;

		ctx = cpg_parser_context_new (network);
		cpg_parser_context_push_input (ctx, file, stream);

		ret = cpg_parser_context_parse (ctx, error);

		g_object_unref (ctx);
	}

	if (stream)
	{
		g_object_unref (stream);
	}

	return ret;
}

/**
 * cpg_network_load_from_path:
 * @network: A #CpgNetwork
 * @path: The filename of the file to load
 * @error: A #GError
 * 
 * Load a network from a path into an existing network instance.
 *
 * Returns: %TRUE if the path could be loaded, %FALSE otherwise
 *
 **/
gboolean
cpg_network_load_from_path (CpgNetwork   *network,
                            const gchar  *path,
                            GError      **error)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	GFile *file = g_file_new_for_path (path);
	gboolean ret = cpg_network_load_from_file (network, file, error);
	g_object_unref (file);

	return ret;
}

/**
 * cpg_network_load_from_string:
 * @network: A #CpgNetwork
 * @xml: The network to load
 * @error: A #GError
 * 
 * Load a network from text into an existing network instance.
 *
 * Returns: %TRUE if the text could be loaded, %FALSE otherwise
 *
 **/
gboolean
cpg_network_load_from_string (CpgNetwork   *network,
                              const gchar  *s,
                              GError      **error)
{
	GInputStream *stream;
	gboolean ret;

	g_return_val_if_fail (CPG_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (s != NULL, FALSE);

	cpg_object_clear (CPG_OBJECT (network));

	stream = g_memory_input_stream_new_from_data (s, -1, NULL);
	ret = cpg_network_load_from_stream (network, stream, error);
	g_object_unref (stream);

	return ret;
}

/**
 * cpg_network_new_from_stream:
 * @stream: the stream containing the network definition
 * @error: error return value
 *
 * Create a new CPG network by reading the network definition from a stream
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the stream
 *
 **/
CpgNetwork *
cpg_network_new_from_stream (GInputStream  *stream,
                             GError       **error)
{
	g_return_val_if_fail (G_IS_INPUT_STREAM (stream), NULL);

	CpgNetwork *network = g_object_new (CPG_TYPE_NETWORK,
	                                    "id", "(cpg)",
	                                    NULL);

	if (!cpg_network_load_from_stream (network, stream, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cpg_network_new_from_file:
 * @file: the file containing the network definition
 * @error: error return value
 *
 * Create a new CPG network by reading the network definition from file
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error reading the file
 *
 **/
CpgNetwork *
cpg_network_new_from_file (GFile   *file,
                           GError **error)
{
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	CpgNetwork *network = g_object_new (CPG_TYPE_NETWORK,
	                                    "id", "(cpg)",
	                                    NULL);

	if (!cpg_network_load_from_file (network, file, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cpg_network_new_from_path:
 * @path: The network file path
 * @error: A #GError
 * 
 * Create a new CPG network by reading the network definition from a file path.
 * See #cpg_network_new_from_file for more information.
 *
 * Returns: A #CpgNetwork
 *
 **/
CpgNetwork *
cpg_network_new_from_path (gchar const  *path,
                           GError      **error)
{
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CpgNetwork *network = cpg_network_new_from_file (file, error);
	g_object_unref (file);

	return network;
}

/**
 * cpg_network_new_from_string:
 * @s: definition of the network
 * @error: error return value
 *
 * Create a new CPG network from the network definition
 *
 * Return value: the newly created CPG network or %NULL if there was an
 * error
 *
 **/
CpgNetwork *
cpg_network_new_from_string (gchar const  *s,
                             GError      **error)
{
	g_return_val_if_fail (s != NULL, NULL);

	CpgNetwork *network = cpg_network_new ();

	if (!cpg_network_load_from_string (network, s, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cpg_network_step:
 * @network: a #CpgNetwork
 * @timestep: the integration timestep
 *
 * Perform one step of simulation given the specified @timestep.
 *
 **/
void
cpg_network_step (CpgNetwork  *network,
                  gdouble      timestep)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (timestep > 0);

	cpg_integrator_step (network->priv->integrator,
	                     cpg_integrator_get_time (network->priv->integrator),
	                     timestep);
}

/**
 * cpg_network_run:
 * @network: a #CpgNetwork
 * @from: the simulation start time
 * @timestep: the integration time step to simulate with
 * @to: the simulation end time
 *
 * Perform a period of simulation. The period is determined by from, timestep
 * and to as described above.
 *
 **/
void
cpg_network_run (CpgNetwork  *network,
                 gdouble      from,
                 gdouble      timestep,
                 gdouble      to)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (from < to);
	g_return_if_fail (timestep > 0);

	cpg_integrator_run (network->priv->integrator,
	                    from,
	                    timestep,
	                    to);
}

/**
 * cpg_network_merge:
 * @network: a #CpgNetwork
 * @other: a #CpgNetwork to merge
 *
 * Merges all the globals, templates and objects from @other into @network.
 *
 **/
void
cpg_network_merge (CpgNetwork  *network,
                   CpgNetwork  *other)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_NETWORK (other));

	GSList *props = cpg_object_get_properties (CPG_OBJECT (other));
	GSList *item;

	for (item = props; item; item = g_slist_next (item))
	{
		CpgProperty *property = item->data;

		if (!cpg_object_get_property (CPG_OBJECT (network),
		                              cpg_property_get_name (property)))
		{
			cpg_object_add_property (CPG_OBJECT (network),
			                         cpg_property_copy (property),
			                         NULL);
		}
	}

	g_slist_free (props);

	/* Copy over templates */
	CpgGroup *template_group = cpg_network_get_template_group (other);
	GSList const *templates = cpg_group_get_children (template_group);

	while (templates)
	{
		CpgObject *template = templates->data;

		if (!cpg_group_get_child (network->priv->template_group,
		                          cpg_object_get_id (template)))
		{
			cpg_group_add (network->priv->template_group,
			               template,
			               NULL);
		}
	}

	/* Copy over children */
	GSList const *children = cpg_group_get_children (CPG_GROUP (other));

	while (children)
	{
		cpg_group_add (CPG_GROUP (network), children->data, NULL);
		children = g_slist_next (children);
	}

	CpgGroup *function_group = cpg_network_get_template_group (other);
	GSList const *functions = cpg_group_get_children (function_group);

	/* Copy over functions */
	while (functions)
	{
		CpgObject *function = functions->data;

		if (!cpg_group_get_child (network->priv->function_group,
		                          cpg_object_get_id (function)))
		{
			cpg_group_add (network->priv->function_group,
			               function,
			               NULL);
		}
	}
}

/**
 * cpg_network_merge_from_file:
 * @network: a #CpgNetwork
 * @file: network file
 * @error: error return value
 *
 * Merges the network defined in the file @file into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
void
cpg_network_merge_from_file (CpgNetwork  *network,
                             GFile       *file,
                             GError     **error)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (G_IS_FILE (file));

	CpgNetwork *other;

	other = cpg_network_new_from_file (file, error);

	if (other != NULL)
	{
		cpg_network_merge (network, other);
		g_object_unref (other);
	}
}

/**
 * cpg_network_merge_from_path:
 * @network: a #CpgNetwork
 * @path: network path
 * @error: error return value
 *
 * Merges the network defined in the file @path into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
void
cpg_network_merge_from_path (CpgNetwork  *network,
                             const gchar *path,
                             GError     **error)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (path != NULL);

	CpgNetwork *other;

	other = cpg_network_new_from_path (path, error);

	if (other != NULL)
	{
		cpg_network_merge (network, other);
		g_object_unref (other);
	}
}

/**
 * cpg_network_merge_from_string:
 * @network: a #CpgNetwork
 * @s: a string describing the network
 * @error: error return value
 *
 * Merges the network defined in @s into @network. This is
 * similar to creating a network from xml and merging it with @network.
 *
 **/
void
cpg_network_merge_from_string (CpgNetwork   *network,
                               gchar const  *s,
                               GError      **error)
{
	CpgNetwork *other;

	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (s != NULL);

	other = cpg_network_new_from_string (s, error);

	if (other != NULL)
	{
		cpg_network_merge (network, other);
		g_object_unref (other);
	}
}

/**
 * cpg_network_set_integrator:
 * @network: A #CpgNetwork
 * @integrator: A #CpgIntegrator
 *
 * Set the integrator used to integrate the network. Note that the network
 * is automatically reset when the integrator is changed.
 *
 **/
void
cpg_network_set_integrator (CpgNetwork    *network,
                            CpgIntegrator *integrator)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_INTEGRATOR (integrator));

	set_integrator (network, integrator);
}

/**
 * cpg_network_get_integrator:
 * @network: A #CpgNetwork
 *
 * Get the integrator currently associated with the network.
 *
 * Returns: (transfer none): A #CpgIntegrator
 *
 **/
CpgIntegrator *
cpg_network_get_integrator (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->integrator;
}

/**
 * cpg_network_get_template_group:
 * @network: A #CpgNetwork
 * 
 * Get the group containing the templates.
 *
 * Returns: (transfer none): A #CpgGroup
 *
 **/
CpgGroup *
cpg_network_get_template_group (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->template_group;
}

/**
 * cpg_network_get_function_group:
 * @network: A #CpgNetwork
 * 
 * Get the group containing the user defined functions.
 *
 * Returns: (transfer none): A #CpgGroup
 *
 **/
CpgGroup *
cpg_network_get_function_group (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->function_group;
}

/**
 * cpg_network_get_file:
 * @network: A #CpgNetwork
 *
 * Get the file with which the network was loaded.
 *
 * Returns: (transfer full) (allow-none): The file or %NULL if the network was not loaded from file.
 *
 **/
GFile *
cpg_network_get_file (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->file ? g_file_dup (network->priv->file) : NULL;
}

/**
 * cpg_network_get_path:
 * @network: A #CpgNetwork
 *
 * Get the path with which the network was loaded.
 *
 * Returns: (transfer full) (allow-none): The path or %NULL if the network was not loaded from file.
 *
 **/
gchar *
cpg_network_get_path (CpgNetwork *network)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);

	return network->priv->file ? g_file_get_path (network->priv->file) : NULL;
}

static void
on_import_parent_changed (CpgImport  *import,
                          GParamSpec *spec,
                          CpgNetwork *network)
{
	if (cpg_object_get_parent (CPG_OBJECT (import)) == NULL)
	{
		unregister_import (network, import);
	}
}

void
_cpg_network_register_import (CpgNetwork *network,
                              CpgImport  *import)
{
	g_return_if_fail (CPG_IS_NETWORK (network));
	g_return_if_fail (CPG_IS_IMPORT (import));

	GFile *file = cpg_import_get_file (import);

	if (g_hash_table_lookup (network->priv->imports, file))
	{
		g_object_unref (file);
		return;
	}

	g_hash_table_insert (network->priv->imports,
	                     file,
	                     import);

	g_signal_connect (import,
	                  "notify::parent",
	                  G_CALLBACK (on_import_parent_changed),
	                  network);

	g_object_weak_ref (G_OBJECT (import),
	                   (GWeakNotify)on_registered_import_destroyed,
	                   network);
}

/**
 * cpg_network_get_import:
 * @network: A #CpgNetwork
 * @file: A #GFile
 *
 * Get a registered import which imports the given file.
 *
 * Returns: (transfer none): A #CpgImport
 *
 **/
CpgImport *
cpg_network_get_import (CpgNetwork   *network,
                        GFile        *file)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	return g_hash_table_lookup (network->priv->imports, file);
}

/**
 * cpg_network_get_import_from_path:
 * @network: A #CpgNetwork
 * @path: A file path
 *
 * Get a registered import which imports the given path.
 *
 * Returns: (transfer none): A #CpgImport
 *
 **/
CpgImport *
cpg_network_get_import_from_path  (CpgNetwork   *network,
                                   const gchar  *path)
{
	g_return_val_if_fail (CPG_IS_NETWORK (network), NULL);
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CpgImport *ret = cpg_network_get_import (network, file);
	g_object_unref (file);

	return ret;
}
