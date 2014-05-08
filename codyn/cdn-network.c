/*
 * codyn.c
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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <sys/time.h>

#include "cdn-network.h"
#include "cdn-network-xml.h"
#include "cdn-expression.h"
#include "cdn-object.h"
#include "cdn-edge.h"
#include "cdn-integrators.h"
#include "cdn-network-deserializer.h"
#include "cdn-operators.h"
#include "cdn-import.h"
#include "cdn-parser-context.h"
#include "cdn-debug.h"
#include "cdn-io-method.h"
#include "instructions/cdn-instruction-rand.h"

#define CDN_NETWORK_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE ((object), CDN_TYPE_NETWORK, CdnNetworkPrivate))

/**
 * CdnNetwork:
 *
 * The main codyn network object.
 *
 * The #CdnNetwork class is a #CdnNode subclass representing the top-level
 * node in which all other nodes are defined. Variables defined on the network
 * are therefore automatically available to all the nodes in your network and
 * can be considered as global variables. Please see #CdnNode for useful API
 * for finding child nodes, and adding/removing nodes from the network.
 *
 * A #CdnNetwork can be loaded from either an XML format or the newer Codyn
 * language format. Please see #cdn_network_new_from_file,
 * #cdn_network_new_from_path, #cdn_network_new_from_stream and
 * #cdn_network_new_from_string for more information.
 *
 * Networks are mainly composed of two types of objects, nodes and edges. A
 * node (see #CdnNode) is a container of variables which can be either state
 * variables or normal variables. Edges (see #CdnEdge) on the other hand
 * represent connections between two nodes. Conceptually, they define the
 * relationship between two nodes in terms of how variables in one node
 * influence variables in another node. A typical example of the use of an
 * edge is to implement coupling between two dynamical subsystems.
 *
 * To simulate a network, you can either use #cdn_network_run, which
 * simulates the network for a given amount of time in one time.
 * #cdn_network_run blocks until the simulation is done. Alternatively, you
 * can use #cdn_network_begin, #cdn_network_step and #cdn_network_end to do
 * the same without blocking. The simulation will use the integrater which can
 * be set using #cdn_network_set_integrator.
 *
 * Templates in a network (useful when loading a network from a file) can be
 * accessed through a special template node. This node only serves as a
 * container for the templates and can be accessed from a network using
 * #cdn_network_get_template_node.
 *
 * A network also governs the random number generator that is used to implement
 * the `rand()` function. You can set the seed that is used for the random
 * number generator with #cdn_network_set_random_seed. The default random seed
 * is read from `/dev/urandom` on UNIX like systems. On non-UNIX systems, or when
 * reading from `/dev/urandom` fails, the initial seed is set to the current
 * system time (milliseconds since epoch).
 *
 */

/* Properties */
enum
{
	PROP_0,
	PROP_INTEGRATOR,
	PROP_FILE,
	PROP_FILENAME
};

struct _CdnNetworkPrivate
{
	GFile *file;

	CdnIntegrator *integrator;
	CdnIntegratorState *integrator_state;

	CdnNode *template_group;

	GHashTable *imports;
	GSList *linked_libraries;
	CdnParserContext *parser_context;

	guint seed;
};

enum
{
	COMPILE_ERROR,
	NUM_SIGNALS
};

static guint network_signals[NUM_SIGNALS] = {0,};

G_DEFINE_TYPE (CdnNetwork, cdn_network, CDN_TYPE_NODE)

static void on_import_parent_changed (CdnImport  *import,
                                      GParamSpec *spec,
                                      CdnNetwork *network);

/**
 * cdn_network_load_error_quark:
 *
 * Get the error quark for the network load error type.
 *
 * Returns: a #GQuark for the network load error type
 *
 */
GQuark
cdn_network_load_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_network_load_error");
	}

	return quark;
}

/**
 * cdn_network_error_quark:
 *
 * Get the error quark for the network error type.
 *
 * Returns: a #GQuark for the network error type
 *
 */
GQuark
cdn_network_error_quark ()
{
	static GQuark quark = 0;

	if (G_UNLIKELY (quark == 0))
	{
		quark = g_quark_from_static_string ("cdn_network_error");
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
on_registered_import_destroyed (CdnNetwork *network,
                                gpointer    data)
{
	g_hash_table_foreach_remove (network->priv->imports,
	                             (GHRFunc)remove_destroyed_import,
	                             data);
}

static void
unregister_import (CdnNetwork *network,
                   CdnImport  *import)
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
                        CdnImport  *import,
                        CdnNetwork *network)
{
	unregister_import (network, import);
}

static void
cdn_network_finalize (GObject *object)
{
	CdnNetwork *network = CDN_NETWORK (object);

	if (network->priv->file)
	{
		g_object_unref (network->priv->file);
	}

	g_object_unref (network->priv->template_group);

	g_hash_table_foreach (network->priv->imports,
	                      (GHFunc)unregister_all_imports,
	                      network);

	g_hash_table_destroy (network->priv->imports);

	if (network->priv->parser_context)
	{
		g_object_unref (network->priv->parser_context);
	}

	G_OBJECT_CLASS (cdn_network_parent_class)->finalize (object);
}

static void
cdn_network_get_property (GObject     *object,
                          guint        prop_id,
                          GValue      *value,
                          GParamSpec  *pspec)
{
	CdnNetwork *self = CDN_NETWORK (object);

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
set_integrator (CdnNetwork    *network,
                CdnIntegrator *integrator)
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

		cdn_integrator_set_state (integrator,
		                          network->priv->integrator_state);
	}

	cdn_object_reset (CDN_OBJECT (network));

	g_object_notify (G_OBJECT (network), "integrator");

}

static void
set_file (CdnNetwork *network,
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
cdn_network_set_property (GObject       *object,
                          guint          prop_id,
                          const GValue  *value,
                          GParamSpec    *pspec)
{
	CdnNetwork *self = CDN_NETWORK (object);

	switch (prop_id)
	{
		case PROP_INTEGRATOR:
			set_integrator (self, CDN_INTEGRATOR (g_value_get_object (value)));
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
cdn_network_dispose (GObject *object)
{
	CdnNetwork *network = CDN_NETWORK (object);

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

	while (network->priv->linked_libraries)
	{
		g_module_close (network->priv->linked_libraries->data);

		network->priv->linked_libraries =
			g_slist_delete_link (network->priv->linked_libraries,
			                     network->priv->linked_libraries);
	}

	cdn_object_clear (CDN_OBJECT (network));

	G_OBJECT_CLASS (cdn_network_parent_class)->dispose (object);
}

static gboolean
template_exists (CdnNode  *group,
                 CdnObject *template)
{
	GSList const *children;

	if (CDN_OBJECT (group) == template)
	{
		return TRUE;
	}

	children = cdn_node_get_children (group);

	while (children)
	{
		CdnObject *child;

		child = children->data;

		if (child == template)
		{
			return TRUE;
		}

		if (CDN_IS_NODE (child))
		{
			if (template_exists (CDN_NODE (child), template))
			{
				return TRUE;
			}
		}

		children = g_slist_next (children);
	}

	return FALSE;
}

static gboolean
cdn_network_add_impl (CdnNode   *group,
                      CdnObject  *object,
                      GError    **error)
{
	CdnNetwork *network = CDN_NETWORK (group);

	/* Check if the network owns all the templates */
	GSList const *templates = cdn_object_get_applied_templates (object);

	while (templates)
	{
		CdnObject *template = templates->data;

		if (!template_exists (network->priv->template_group, template))
		{
			g_set_error (error,
			             CDN_NETWORK_ERROR,
			             CDN_NETWORK_ERROR_UNOWNED_TEMPLATE,
			             "The object `%s' contains template `%s' which is not part of the network",
			             cdn_object_get_id (object),
			             cdn_object_get_id (template));

			return FALSE;
		}

		templates = g_slist_next (templates);
	}

	if (CDN_NODE_CLASS (cdn_network_parent_class)->add (group, object, error))
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

typedef struct
{
	CdnNetwork *network;
	CdnCompileContext *context;
	CdnCompileError *error;
	gboolean failed;
} CompileInfo;

static CdnCompileContext *
cdn_network_get_compile_context_impl (CdnObject         *object,
                                      CdnCompileContext *context)
{
	CdnNetwork *network = CDN_NETWORK (object);

	/* Note: we repeat this logic here from cdn-object because we need
	   to prepend the 'integrator' object before the real object... */
	if (!context)
	{
		if (cdn_object_get_parent (object))
		{
			context = cdn_object_get_compile_context (CDN_OBJECT (cdn_object_get_parent (object)),
			                                          NULL);
		}
		else
		{
			context = cdn_compile_context_new ();
		}
	}

	cdn_compile_context_prepend_object (context, CDN_OBJECT (network->priv->integrator));
	CDN_OBJECT_CLASS (cdn_network_parent_class)->get_compile_context (object, context);

	return context;
}

static void
reset_rands (CdnNetwork *network)
{
	CdnIntegratorState *state;
	GSList const *rands;

	state = cdn_integrator_get_state (network->priv->integrator);
	rands = cdn_integrator_state_rand_instructions (state);

	srand (network->priv->seed);

	while (rands)
	{
		cdn_instruction_rand_reset (rands->data);
		rands = g_slist_next (rands);
	}
}

static gboolean
cdn_network_compile_impl (CdnObject         *object,
                          CdnCompileContext *context,
                          CdnCompileError   *error)
{
	CdnNetwork *network = CDN_NETWORK (object);
	gboolean ret;

	if (context)
	{
		cdn_compile_context_save (context);
		g_object_ref (context);
	}

	context = cdn_network_get_compile_context_impl (object, context);

	ret = CDN_OBJECT_CLASS (cdn_network_parent_class)->compile (object,
	                                                            context,
	                                                            error);

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
	else
	{
		reset_rands (network);
	}

	return ret;
}

static void
cdn_network_clear_impl (CdnObject *object)
{
	CdnNetwork *network = CDN_NETWORK (object);

	CDN_OBJECT_CLASS (cdn_network_parent_class)->clear (object);

	cdn_object_clear (CDN_OBJECT (network->priv->template_group));
}

static void
cdn_network_reset_impl (CdnObject *object)
{
	CdnNetwork *network;

	network = CDN_NETWORK (object);

	// Reset the integrator
	if (network->priv->integrator)
	{
		cdn_object_reset (CDN_OBJECT (network->priv->integrator));
	}

	// Reset the object
	CDN_OBJECT_CLASS (cdn_network_parent_class)->reset (object);

	reset_rands (network);
}

static void
cdn_network_constructed (GObject *object)
{
	CdnNetwork *network;

	network = CDN_NETWORK (object);

	if (cdn_object_get_id (CDN_OBJECT (network)) == NULL)
	{
		cdn_object_set_id (CDN_OBJECT (network), "(cdn)");
	}
}

static void
cdn_network_class_init (CdnNetworkClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	CdnNodeClass *group_class = CDN_NODE_CLASS (klass);
	CdnObjectClass *cdn_class = CDN_OBJECT_CLASS (klass);

	object_class->finalize = cdn_network_finalize;
	object_class->dispose = cdn_network_dispose;
	object_class->get_property = cdn_network_get_property;
	object_class->set_property = cdn_network_set_property;
	object_class->constructed = cdn_network_constructed;

	cdn_class->compile = cdn_network_compile_impl;
	cdn_class->get_compile_context = cdn_network_get_compile_context_impl;
	cdn_class->clear = cdn_network_clear_impl;
	cdn_class->reset = cdn_network_reset_impl;

	group_class->add = cdn_network_add_impl;

	/**
	 * CdnNetwork::compile-error:
	 * @network: a #CdnNetwork
	 * @error: a #CdnCompileError
	 *
	 * Emitted when there is a compile error
	 *
	 **/
	network_signals[COMPILE_ERROR] =
		g_signal_new ("compile-error",
		              G_OBJECT_CLASS_TYPE (object_class),
		              G_SIGNAL_RUN_LAST,
		              G_STRUCT_OFFSET (CdnNetworkClass,
		                               compile_error),
		              NULL,
		              NULL,
		              g_cclosure_marshal_VOID__BOXED,
		              G_TYPE_NONE,
		              1,
		              CDN_TYPE_COMPILE_ERROR);

	/**
	 * CdnNetwork:integrator:
	 *
	 * The integrator used to simulate the network.
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_INTEGRATOR,
	                                 g_param_spec_object ("integrator",
	                                                      "Integrator",
	                                                      "Integrator",
	                                                      CDN_TYPE_INTEGRATOR,
	                                                      G_PARAM_READWRITE));

	g_type_class_add_private (object_class, sizeof (CdnNetworkPrivate));

	/**
	 * CdnNetwork:file:
	 *
	 * The file from which the network was read (may be %NULL).
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FILE,
	                                 g_param_spec_object ("file",
	                                                      "File",
	                                                      "File",
	                                                      G_TYPE_FILE,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

	/**
	 * CdnNetwork:filename:
	 *
	 * The filename from which the network was read (may be %NULL).
	 **/
	g_object_class_install_property (object_class,
	                                 PROP_FILENAME,
	                                 g_param_spec_string ("filename",
	                                                      "Filename",
	                                                      "Filename",
	                                                      NULL,
	                                                      G_PARAM_READABLE));
}

static void
init_seed_from_time (CdnNetwork *network)
{
	struct timeval tv;

	gettimeofday (&tv, NULL);
	network->priv->seed = tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

static void
cdn_network_init (CdnNetwork *network)
{
	cdn_init ();

	network->priv = CDN_NETWORK_GET_PRIVATE (network);

#ifdef G_OS_UNIX
{
	GFile *randomf;
	GFileInputStream *istream;

	randomf = g_file_new_for_path ("/dev/urandom");
	istream = g_file_read (randomf, NULL, NULL);

	if (istream)
	{
		GDataInputStream *dstream;
		GError *err = NULL;

		dstream = g_data_input_stream_new (G_INPUT_STREAM (istream));

		network->priv->seed = g_data_input_stream_read_uint32 (dstream,
		                                                       NULL,
		                                                       &err);

		if (err != NULL)
		{
			init_seed_from_time (network);
		}

		g_object_unref (dstream);
		g_object_unref (istream);
	}
	else
	{
		init_seed_from_time (network);
	}

	g_object_unref (randomf);
}
#else
	init_seed_from_time (network);
#endif

	network->priv->template_group = cdn_node_new ("templates");
	_cdn_object_set_parent (CDN_OBJECT (network->priv->template_group),
	                        CDN_NODE (network));

	g_signal_connect_swapped (network->priv->template_group,
	                          "tainted",
	                          G_CALLBACK (cdn_object_taint),
	                          network);

	network->priv->integrator_state = cdn_integrator_state_new (CDN_OBJECT (network));

	/* Create default integrator */
	CdnIntegratorEuler *integrator = cdn_integrator_euler_new ();
	cdn_network_set_integrator (network, CDN_INTEGRATOR (integrator));
	g_object_unref (integrator);

	network->priv->imports = g_hash_table_new_full (g_file_hash,
	                                                (GEqualFunc)g_file_equal,
	                                                (GDestroyNotify)g_object_unref,
	                                                NULL);
}

/**
 * cdn_network_set_random_seed:
 * @network: a #CdnNetwork.
 * @seed: the new seed.
 *
 * Set the random seed used to initialize the random number generator. The
 * network will be reset when calling this function which will reset the random
 * number generator with the given seed.
 */
void
cdn_network_set_random_seed (CdnNetwork *network,
                             guint       seed)
{
	g_return_if_fail (CDN_IS_NETWORK (network));

	network->priv->seed = seed;
	cdn_object_reset (CDN_OBJECT (network));
}

/**
 * cdn_network_get_random_seed:
 * @network: a #CdnNetwork.
 *
 * Get the random seed used to initialize the random number generator.
 *
 * Returns: the seed.
 */
guint
cdn_network_get_random_seed (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), 1);

	return network->priv->seed;
}

/**
 * cdn_network_new:
 *
 * Create a new empty CDN network
 *
 * Return value: the newly created CDN network
 *
 **/
CdnNetwork *
cdn_network_new ()
{
	cdn_init ();

	return g_object_new (CDN_TYPE_NETWORK, NULL);
}

static CdnNetworkFormat
format_from_seekable_stream (GInputStream *stream)
{
	gchar buffer[64];
	CdnNetworkFormat ret;

	ret = CDN_NETWORK_FORMAT_UNKNOWN;

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
				ret = (buffer[i] == '<' ? CDN_NETWORK_FORMAT_XML
				                        : CDN_NETWORK_FORMAT_CDN);

				break;
			}
		}

		if (ret != CDN_NETWORK_FORMAT_UNKNOWN)
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

static CdnNetworkFormat
format_from_buffered_stream (GBufferedInputStream *stream)
{
	/* Get the buffer size */
	gsize bufsize;
	gsize start;
	CdnNetworkFormat fmt;

	start = 0;
	bufsize = g_buffered_input_stream_get_buffer_size (stream);

	if (bufsize == 0)
	{
		/* Make sure initial buffer size is not 0 */
		bufsize = 64;
		g_buffered_input_stream_set_buffer_size (stream, bufsize);
	}

	fmt = CDN_NETWORK_FORMAT_UNKNOWN;

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
				fmt = (buf[start] == '<' ? CDN_NETWORK_FORMAT_XML
				                         : CDN_NETWORK_FORMAT_CDN);

				break;
			}

			++start;
		}

		if (fmt != CDN_NETWORK_FORMAT_UNKNOWN)
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
 * cdn_network_format_from_stream:
 * @stream: A #GInputStream
 *
 * Determine the type of CDN format from a stream. This only works if either
 * the stream is seekable (see #GSeekable), or if the stream is a
 * #GBufferedInputStream. If needed, you can wrap your stream in a
 * #GBufferedInputStream before passing it.
 *
 * Returns: A #CdnNetworkFormat
 *
 **/
CdnNetworkFormat
cdn_network_format_from_stream (GInputStream *stream)
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

	return CDN_NETWORK_FORMAT_UNKNOWN;
}

/**
 * cdn_network_format_from_file:
 * @file: A #GFile
 *
 * Determine the type of CDN format from a file. If the type of the file
 * could not be determined, #CDN_NETWORK_FORMAT_UNKNOWN is returned. This
 * function only uses the mime type of a file. Use
 * #cdn_network_format_from_stream to determine the format from the contents.
 *
 * Returns: A #CdnNetworkFormat
 *
 **/
CdnNetworkFormat
cdn_network_format_from_file (GFile *file)
{
	GFileInfo *info;
	gchar const *ctype;
	CdnNetworkFormat ret;

	info = g_file_query_info (file,
	                          G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE,
	                          0,
	                          NULL,
	                          NULL);

	if (!info)
	{
		return CDN_NETWORK_FORMAT_UNKNOWN;
	}

	ctype = g_file_info_get_content_type (info);

	if (g_content_type_is_a (ctype, "application/xml"))
	{
		ret = CDN_NETWORK_FORMAT_XML;
	}
	else if (g_content_type_is_a (ctype, "text/x-cdn"))
	{
		ret = CDN_NETWORK_FORMAT_CDN;
	}
	else
	{
		ret = CDN_NETWORK_FORMAT_UNKNOWN;
	}

	g_object_unref (info);
	return ret;
}

static gboolean
load_from_stream (CdnNetwork    *network,
                  GInputStream  *stream,
                  gboolean       clearfirst,
                  GError       **error)
{
	gboolean ret;
	CdnNetworkFormat fmt;
	GInputStream *wrapped;

	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_INPUT_STREAM (stream), FALSE);

	if (clearfirst)
	{
		cdn_object_clear (CDN_OBJECT (network));
	}

	if (!(G_IS_SEEKABLE (stream) && g_seekable_can_seek (G_SEEKABLE (stream))) &&
	    !G_IS_BUFFERED_INPUT_STREAM (stream))
	{
		wrapped = g_buffered_input_stream_new (stream);
	}
	else
	{
		wrapped = g_object_ref (stream);
	}

	fmt = cdn_network_format_from_stream (wrapped);

	if (fmt == CDN_NETWORK_FORMAT_XML)
	{
		CdnNetworkDeserializer *deserializer;

		deserializer = cdn_network_deserializer_new (network,
		                                             NULL);

		ret = cdn_network_deserializer_deserialize (deserializer,
		                                            NULL,
		                                            wrapped,
		                                            error);

		g_object_unref (deserializer);
	}
	else
	{
		CdnParserContext *ctx;

		ctx = cdn_parser_context_new (network);
		cdn_parser_context_push_input (ctx, NULL, wrapped, FALSE);

		ret = cdn_parser_context_parse (ctx, TRUE, error);

		g_object_unref (ctx);
	}

	g_object_unref (wrapped);

	return ret;
}

/**
 * cdn_network_load_from_stream:
 * @network: A #CdnNetwork
 * @stream: The stream to load
 * @error: A #GError
 *
 * Load a network from a stream
 *
 * Returns: %TRUE if the stream could be loaded, %FALSE otherwise
 *
 **/
gboolean
cdn_network_load_from_stream (CdnNetwork    *network,
                              GInputStream  *stream,
                              GError       **error)
{
	return load_from_stream (network, stream, TRUE, error);
}

static gboolean
load_from_file (CdnNetwork  *network,
                GFile       *file,
                gboolean     clearfirst,
                GError     **error)
{
	gboolean ret;
	CdnNetworkFormat fmt;
	GInputStream *stream;
	GFileInputStream *bstream;

	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_FILE (file), FALSE);

	set_file (network, file);
	stream = NULL;

	if (clearfirst)
	{
		cdn_object_clear (CDN_OBJECT (network));
	}

	bstream = g_file_read (file, NULL, error);

	if (!bstream)
	{
		return FALSE;
	}

	fmt = CDN_NETWORK_FORMAT_UNKNOWN;

	stream = g_buffered_input_stream_new (G_INPUT_STREAM (bstream));
	g_object_unref (bstream);

	fmt = cdn_network_format_from_stream (stream);

	if (fmt == CDN_NETWORK_FORMAT_UNKNOWN)
	{
		fmt = cdn_network_format_from_file (file);
	}

	if (fmt == CDN_NETWORK_FORMAT_XML)
	{
		CdnNetworkDeserializer *deserializer;

		deserializer = cdn_network_deserializer_new (network,
		                                             NULL);

		ret = cdn_network_deserializer_deserialize (deserializer,
		                                            file,
		                                            stream,
		                                            error);

		g_object_unref (deserializer);
	}
	else
	{
		CdnParserContext *ctx;

		ctx = cdn_parser_context_new (network);
		cdn_parser_context_push_input (ctx, file, stream, FALSE);

		if (network->priv->parser_context != NULL)
		{
			g_object_unref (network->priv->parser_context);
			network->priv->parser_context = NULL;
		}

		ret = cdn_parser_context_parse (ctx, TRUE, error);

		if (ret)
		{
			g_object_unref (ctx);
		}
		else
		{
			network->priv->parser_context = ctx;
			g_object_set (ctx, "network", NULL, NULL);
		}
	}

	if (stream)
	{
		g_object_unref (stream);
	}

	return ret;
}

/**
 * cdn_network_load_from_file:
 * @network: A #CdnNetwork
 * @file: The file to load
 * @error: A #GError
 *
 * Load a network from a file
 *
 * Returns: %TRUE if the file could be loaded, %FALSE otherwise
 *
 **/
gboolean
cdn_network_load_from_file (CdnNetwork  *network,
                            GFile       *file,
                            GError     **error)
{
	return load_from_file (network, file, TRUE, error);
}

static gboolean
load_from_path (CdnNetwork   *network,
                const gchar  *path,
                gboolean      clearfirst,
                GError      **error)
{
	GFile *file;
	gboolean ret;

	file = g_file_new_for_path (path);
	ret = load_from_file (network, file, clearfirst, error);
	g_object_unref (file);

	return ret;
}

/**
 * cdn_network_load_from_path:
 * @network: A #CdnNetwork
 * @path: The filename of the file to load
 * @error: A #GError
 *
 * Load a network from a path into an existing network instance.
 *
 * Returns: %TRUE if the path could be loaded, %FALSE otherwise
 *
 **/
gboolean
cdn_network_load_from_path (CdnNetwork   *network,
                            const gchar  *path,
                            GError      **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	return load_from_path (network, path, TRUE, error);
}

static gboolean
load_from_string (CdnNetwork   *network,
                  const gchar  *s,
                  gboolean      clearfirst,
                  GError      **error)
{
	GInputStream *stream;
	gboolean ret;

	stream = g_memory_input_stream_new_from_data (s, -1, NULL);
	ret = load_from_stream (network, stream, clearfirst, error);
	g_object_unref (stream);

	return ret;
}

/**
 * cdn_network_load_from_string:
 * @network: A #CdnNetwork
 * @s: The network to load
 * @error: A #GError
 *
 * Load a network from text into an existing network instance.
 *
 * Returns: %TRUE if the text could be loaded, %FALSE otherwise
 *
 **/
gboolean
cdn_network_load_from_string (CdnNetwork   *network,
                              const gchar  *s,
                              GError      **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (s != NULL, FALSE);

	return load_from_string (network, s, TRUE, error);
}

/**
 * cdn_network_new_from_stream:
 * @stream: the stream containing the network definition
 * @error: error return value
 *
 * Create a new CDN network by reading the network definition from a stream
 *
 * Return value: the newly created CDN network or %NULL if there was an
 * error reading the stream
 *
 **/
CdnNetwork *
cdn_network_new_from_stream (GInputStream  *stream,
                             GError       **error)
{
	cdn_init ();

	g_return_val_if_fail (G_IS_INPUT_STREAM (stream), NULL);

	CdnNetwork *network = g_object_new (CDN_TYPE_NETWORK,
	                                    "id", "(cdn)",
	                                    NULL);

	if (!cdn_network_load_from_stream (network, stream, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cdn_network_new_from_file:
 * @file: the file containing the network definition
 * @error: error return value
 *
 * Create a new CDN network by reading the network definition from file
 *
 * Return value: the newly created CDN network or %NULL if there was an
 * error reading the file
 *
 **/
CdnNetwork *
cdn_network_new_from_file (GFile   *file,
                           GError **error)
{
	cdn_init ();

	g_return_val_if_fail (G_IS_FILE (file), NULL);

	CdnNetwork *network = g_object_new (CDN_TYPE_NETWORK,
	                                    "id", "(cdn)",
	                                    NULL);

	if (!cdn_network_load_from_file (network, file, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cdn_network_new_from_path:
 * @path: The network file path
 * @error: A #GError
 *
 * Create a new CDN network by reading the network definition from a file path.
 * See #cdn_network_new_from_file for more information.
 *
 * Returns: A #CdnNetwork
 *
 **/
CdnNetwork *
cdn_network_new_from_path (gchar const  *path,
                           GError      **error)
{
	cdn_init ();

	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CdnNetwork *network = cdn_network_new_from_file (file, error);
	g_object_unref (file);

	return network;
}

/**
 * cdn_network_new_from_string:
 * @s: definition of the network
 * @error: error return value
 *
 * Create a new CDN network from the network definition
 *
 * Return value: the newly created CDN network or %NULL if there was an
 * error
 *
 **/
CdnNetwork *
cdn_network_new_from_string (gchar const  *s,
                             GError      **error)
{
	cdn_init ();

	g_return_val_if_fail (s != NULL, NULL);

	CdnNetwork *network = cdn_network_new ();

	if (!cdn_network_load_from_string (network, s, error))
	{
		g_object_unref (network);
		network = NULL;
	}

	return network;
}

/**
 * cdn_network_step:
 * @network: a #CdnNetwork
 * @timestep: the integration timestep
 *
 * Perform one step of simulation given the specified @timestep. Note that you
 * have to call #cdn_network_begin before doing the first step, and you should
 * call #cdn_network_end after the last step.
 *
 **/
gdouble
cdn_network_step (CdnNetwork  *network,
                  gdouble      timestep)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), 0);
	g_return_val_if_fail (timestep > 0, 0);

	return cdn_integrator_step (network->priv->integrator,
	                            cdn_integrator_get_time (network->priv->integrator),
	                            timestep);
}

/**
 * cdn_network_run:
 * @network: a #CdnNetwork
 * @from: the simulation start time
 * @timestep: the integration time step to simulate with
 * @to: the simulation end time
 * @error: a #GError
 *
 * Perform a period of simulation. The period is determined by from, timestep
 * and to as described above.
 *
 * Returns: %TRUE if the network has been successfully integrated, %FALSE otherwise.
 *
 **/
gboolean
cdn_network_run (CdnNetwork  *network,
                 gdouble      from,
                 gdouble      timestep,
                 gdouble      to,
                 GError     **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);

	return cdn_integrator_run (network->priv->integrator,
	                           from,
	                           timestep,
	                           to,
	                           error);
}

/**
 * cdn_network_begin:
 * @network: A #CdnNetwork
 * @start: the simulation start time
 * @error: a #GError
 *
 * Initialize the network to be integrated. Note that you do not need to
 * use this if you are using #cdn_network_run since it will call
 * #cdn_network_begin (and #cdn_network_end) for you.
 *
 * Returns: %TRUE if the network is ready to be integrated, %FALSE otherwise.
 *
 **/
gboolean
cdn_network_begin (CdnNetwork  *network,
                   gdouble      start,
                   GError     **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);

	return cdn_integrator_begin (network->priv->integrator, start, error);
}

/**
 * cdn_network_end:
 * @network: a #CdnNetwork
 * @error: a #GError
 *
 * Finalize the network after integration. Note that you do not need to use
 * this if you are using #cdn_network_run since it will call
 * #cdn_network_end for you.
 *
 * Returns: %TRUE if the network has been finalized successfully, %FALSE otherwise.
 *
 **/
gboolean
cdn_network_end (CdnNetwork  *network,
                 GError     **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);

	return cdn_integrator_end (network->priv->integrator, error);
}

/**
 * cdn_network_merge_from_file:
 * @network: a #CdnNetwork
 * @file: network file
 * @error: error return value
 *
 * Merges the network defined in the file @file into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
gboolean
cdn_network_merge_from_file (CdnNetwork  *network,
                             GFile       *file,
                             GError     **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_FILE (file), FALSE);

	return load_from_file (network, file, FALSE, error);
}

/**
 * cdn_network_merge_from_path:
 * @network: a #CdnNetwork
 * @path: network path
 * @error: error return value
 *
 * Merges the network defined in the file @path into @network. This is
 * similar to creating a network from a file and merging it with @network.
 *
 **/
gboolean
cdn_network_merge_from_path (CdnNetwork  *network,
                             const gchar *path,
                             GError     **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	return load_from_path (network, path, FALSE, error);
}


/**
 * cdn_network_merge_from_stream:
 * @network: a #CdnNetwork
 * @stream: network stream
 * @error: error return value
 *
 * Merges the network defined in the stream @stream into @network.
 *
 **/
gboolean
cdn_network_merge_from_stream (CdnNetwork    *network,
                             GInputStream  *stream,
                             GError       **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (G_IS_INPUT_STREAM (stream), FALSE);

	return load_from_stream (network, stream, FALSE, error);
}


/**
 * cdn_network_merge_from_string:
 * @network: a #CdnNetwork
 * @s: a string describing the network
 * @error: error return value
 *
 * Merges the network defined in @s into @network. This is
 * similar to creating a network from xml and merging it with @network.
 *
 **/
gboolean
cdn_network_merge_from_string (CdnNetwork   *network,
                               gchar const  *s,
                               GError      **error)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (s != NULL, FALSE);

	return load_from_string (network, s, FALSE, error);
}

/**
 * cdn_network_set_integrator:
 * @network: A #CdnNetwork
 * @integrator: A #CdnIntegrator
 *
 * Set the integrator used to integrate the network. Note that the network
 * is automatically reset when the integrator is changed.
 *
 **/
void
cdn_network_set_integrator (CdnNetwork    *network,
                            CdnIntegrator *integrator)
{
	g_return_if_fail (CDN_IS_NETWORK (network));
	g_return_if_fail (CDN_IS_INTEGRATOR (integrator));

	set_integrator (network, integrator);
}

/**
 * cdn_network_get_integrator:
 * @network: A #CdnNetwork
 *
 * Get the integrator currently associated with the network.
 *
 * Returns: (transfer none): A #CdnIntegrator
 *
 **/
CdnIntegrator *
cdn_network_get_integrator (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	return network->priv->integrator;
}

/**
 * cdn_network_get_template_node:
 * @network: A #CdnNetwork
 *
 * Get the group containing the templates.
 *
 * Returns: (transfer none): A #CdnNode
 *
 **/
CdnNode *
cdn_network_get_template_node (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	return network->priv->template_group;
}

/**
 * cdn_network_get_file:
 * @network: A #CdnNetwork
 *
 * Get the file with which the network was loaded.
 *
 * Returns: (transfer full) (allow-none): The file or %NULL if the network was not loaded from file.
 *
 **/
GFile *
cdn_network_get_file (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	return network->priv->file ? g_file_dup (network->priv->file) : NULL;
}

/**
 * cdn_network_get_path:
 * @network: A #CdnNetwork
 *
 * Get the path with which the network was loaded.
 *
 * Returns: (transfer full) (allow-none): The path or %NULL if the network was not loaded from file.
 *
 **/
gchar *
cdn_network_get_path (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	return network->priv->file ? g_file_get_path (network->priv->file) : NULL;
}

static void
on_import_parent_changed (CdnImport  *import,
                          GParamSpec *spec,
                          CdnNetwork *network)
{
	if (cdn_object_get_parent (CDN_OBJECT (import)) == NULL)
	{
		g_hash_table_remove (network->priv->imports,
		                     cdn_import_get_file (import));

		unregister_import (network, import);
	}
}

void
_cdn_network_register_import (CdnNetwork *network,
                              CdnImport  *import)
{
	g_return_if_fail (CDN_IS_NETWORK (network));
	g_return_if_fail (CDN_IS_IMPORT (import));

	GFile *file = cdn_import_get_file (import);

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
 * cdn_network_get_import:
 * @network: A #CdnNetwork
 * @file: A #GFile
 *
 * Get a registered import which imports the given file.
 *
 * Returns: (transfer none): A #CdnImport
 *
 **/
CdnImport *
cdn_network_get_import (CdnNetwork   *network,
                        GFile        *file)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);
	g_return_val_if_fail (G_IS_FILE (file), NULL);

	return g_hash_table_lookup (network->priv->imports, file);
}

/**
 * cdn_network_get_import_from_path:
 * @network: A #CdnNetwork
 * @path: A file path
 *
 * Get a registered import which imports the given path.
 *
 * Returns: (transfer none): A #CdnImport
 *
 **/
CdnImport *
cdn_network_get_import_from_path  (CdnNetwork   *network,
                                   const gchar  *path)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);
	g_return_val_if_fail (path != NULL, NULL);

	GFile *file = g_file_new_for_path (path);
	CdnImport *ret = cdn_network_get_import (network, file);
	g_object_unref (file);

	return ret;
}

static void
cdn_init_real ()
{
	cdn_debug_init ();
	cdn_io_method_initialize ();
	cdn_network_xml_init ();
}

/**
 * cdn_init:
 *
 * Global codyn library initialization. This initializes the gobject type
 * system, debugging and input/output modules. Note that you normally do not need
 * to call this as it codyn initializes automatically on first use. This function
 * can be called multiple times and will only initialize once.
 */
void
cdn_init ()
{
	static gboolean inited = FALSE;

#if !GLIB_CHECK_VERSION(2, 35, 0)
	g_type_init ();
#endif

#if !GLIB_CHECK_VERSION(2, 31, 0)
	g_thread_init (NULL);
#endif

	if (G_UNLIKELY (!inited))
	{
		inited = TRUE;
		cdn_init_real ();
	}
}


typedef void (*CdnLinkedLibraryInit) (CdnNetwork *network);

/**
 * cdn_network_link_library:
 * @network a #CdnNetwork.
 * @path path to the dynamic library to link.
 * @error a #GError
 *
 * Link a dynamic library (.so, .dylib or .dll) at runtime. This method is
 * mainly useful for internal use. It's used by the codyn format which has
 * a link_library keyword to dynamically link a library when the network is
 * loaded. The loaded library typically installs new math functions or defines
 * a new type of integrator.
 *
 * When the library is loaded, a symbol with the name "cdn_linked_library_init"
 * is resolved. If found, the symbol (interpreted as a function) is called with
 * @network as a parameter.
 *
 * Returns: %TRUE if the library was linked successfully, %FALSE otherwise.
 *
 */
gboolean
cdn_network_link_library (CdnNetwork   *network,
                          gchar const  *path,
                          GError      **error)
{
	GModule *mod;
	CdnLinkedLibraryInit ptr;

	g_return_val_if_fail (CDN_IS_NETWORK (network), FALSE);
	g_return_val_if_fail (path != NULL, FALSE);

	mod = g_module_open (path, G_MODULE_BIND_LAZY);

	if (!mod)
	{
		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_LINK_LIBRARY,
		             "Failed to link library `%s'",
		             path);

		return FALSE;
	}

	if (g_module_symbol (mod, "cdn_linked_library_init", (gpointer *)&ptr))
	{
		ptr (network);
	}

	network->priv->linked_libraries =
		g_slist_prepend (network->priv->linked_libraries,
		                 mod);

	return TRUE;
}

/**
 * cdn_network_get_parser_context:
 * @network the #CdnNetwork
 *
 * Returns: (type CdnParserContext) (transfer none): the parser context.
 */
CdnParserContext *
cdn_network_get_parser_context (CdnNetwork *network)
{
	g_return_val_if_fail (CDN_IS_NETWORK (network), NULL);

	return network->priv->parser_context;
}
