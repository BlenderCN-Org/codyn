#include "cdn-cfile-stream.h"
#include <errno.h>
#include <string.h>

#define CDN_CFILE_STREAM_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_CFILE_STREAM, CdnCfileStreamPrivate))

/**
 * CdnCfileStream:
 *
 * An output stream implementation for C FILE streams.
 *
 * This class implements a GOutputStream wrapped around the C streaming FILE
 * API.
 *
 **/

struct _CdnCfileStreamPrivate
{
	FILE *cfile;
};

G_DEFINE_TYPE (CdnCfileStream, cdn_cfile_stream, G_TYPE_OUTPUT_STREAM)

enum
{
	PROP_0,
	PROP_CFILE
};

static void
cdn_cfile_stream_finalize (GObject *object)
{
	G_OBJECT_CLASS (cdn_cfile_stream_parent_class)->finalize (object);
}

static gssize
cdn_cfile_stream_write_fn (GOutputStream  *ostream,
                           const void     *buffer,
                           gsize           count,
                           GCancellable   *cancellable,
                           GError        **error)
{
	gssize ret;
	CdnCfileStream *stream;

	stream = CDN_CFILE_STREAM (ostream);

	if (!stream->priv->cfile)
	{
		return -1;
	}

	ret = fwrite (buffer, 1, count, stream->priv->cfile);

	if (ferror (stream->priv->cfile) != 0)
	{
		g_set_error (error,
		             G_IO_ERROR,
		             g_io_error_from_errno (errno),
		             "%s",
		             strerror (errno));
	}

	return ret;
}

static gboolean
cdn_cfile_stream_flush (GOutputStream  *ostream,
                        GCancellable   *cancellable,
                        GError        **error)
{
	CdnCfileStream *stream;

	stream = CDN_CFILE_STREAM (ostream);

	if (!stream->priv->cfile)
	{
		return FALSE;
	}

	if (fflush (stream->priv->cfile) != 0)
	{
		g_set_error (error,
		             G_IO_ERROR,
		             g_io_error_from_errno (errno),
		             "%s",
		             strerror (errno));

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static gboolean
cdn_cfile_stream_close_fn (GOutputStream  *ostream,
                           GCancellable   *cancellable,
                           GError        **error)
{
	CdnCfileStream *stream;

	stream = CDN_CFILE_STREAM (ostream);

	if (!stream->priv->cfile)
	{
		return FALSE;
	}

	if (fclose (stream->priv->cfile) != 0)
	{
		g_set_error (error,
		             G_IO_ERROR,
		             g_io_error_from_errno (errno),
		             "%s",
		             strerror (errno));

		return FALSE;
	}
	else
	{
		return TRUE;
	}
}

static void
cdn_cfile_stream_set_property (GObject      *object,
                               guint         prop_id,
                               const GValue *value,
                               GParamSpec   *pspec)
{
	CdnCfileStream *self = CDN_CFILE_STREAM (object);

	switch (prop_id)
	{
		case PROP_CFILE:
			self->priv->cfile = g_value_get_pointer (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_cfile_stream_get_property (GObject    *object,
                               guint       prop_id,
                               GValue     *value,
                               GParamSpec *pspec)
{
	CdnCfileStream *self = CDN_CFILE_STREAM (object);

	switch (prop_id)
	{
		case PROP_CFILE:
			g_value_set_pointer (value, self->priv->cfile);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_cfile_stream_class_init (CdnCfileStreamClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GOutputStreamClass *stream_class = G_OUTPUT_STREAM_CLASS (klass);

	object_class->finalize = cdn_cfile_stream_finalize;

	object_class->get_property = cdn_cfile_stream_get_property;
	object_class->set_property = cdn_cfile_stream_set_property;

	stream_class->write_fn = cdn_cfile_stream_write_fn;
	stream_class->flush = cdn_cfile_stream_flush;
	stream_class->close_fn = cdn_cfile_stream_close_fn;

	g_type_class_add_private (object_class, sizeof(CdnCfileStreamPrivate));

	/**
	 * CdnCfileStream:cfile:
	 *
	 * A FILE pointer to write to.
	 */
	g_object_class_install_property (object_class,
	                                 PROP_CFILE,
	                                 g_param_spec_pointer ("cfile",
	                                                       "Cfile",
	                                                       "Cfile",
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_CONSTRUCT_ONLY |
	                                                       G_PARAM_STATIC_STRINGS));
}

static void
cdn_cfile_stream_init (CdnCfileStream *self)
{
	self->priv = CDN_CFILE_STREAM_GET_PRIVATE (self);
}

/**
 * cdn_cfile_stream_new:
 * @cfile: the FILE to write to.
 *
 * Create a new #CdnCfileStream from a FILE.
 *
 * Returns: a new #CdnCfileStream.
 */
GOutputStream *
cdn_cfile_stream_new (FILE *cfile)
{
	return g_object_new (CDN_TYPE_CFILE_STREAM, "cfile", cfile, NULL);
}
