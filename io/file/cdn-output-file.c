#include "cdn-output-file.h"
#include <codyn/cdn-io.h>
#include <codyn/cdn-network.h>
#include <string.h>

#define CDN_OUTPUT_FILE_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_OUTPUT_FILE, CdnOutputFilePrivate))

struct _CdnOutputFilePrivate
{
	GFile *file;
	gchar *path;
	GOutputStream *stream;
	gchar *delimiter;

	GPtrArray *outputs;
	gboolean header;

	CdnIoMode mode;
};

static void cdn_io_iface_init (gpointer iface);

G_DEFINE_DYNAMIC_TYPE_EXTENDED (CdnOutputFile,
                                cdn_output_file,
                                CDN_TYPE_NODE,
                                0,
                                G_IMPLEMENT_INTERFACE_DYNAMIC (CDN_TYPE_IO,
                                                               cdn_io_iface_init))

enum
{
	PROP_0,
	PROP_PATH,
	PROP_HEADER,
	PROP_DELIMITER,
	PROP_MODE
};

static void
cdn_output_file_finalize (GObject *object)
{
	CdnOutputFile *output;

	output = CDN_OUTPUT_FILE (object);

	if (output->priv->file)
	{
		g_object_unref (output->priv->file);
	}

	if (output->priv->stream)
	{
		g_object_unref (output->priv->stream);
	}

	if (output->priv->outputs)
	{
		g_ptr_array_free (output->priv->outputs, TRUE);
	}

	g_free (output->priv->delimiter);
	g_free (output->priv->path);

	G_OBJECT_CLASS (cdn_output_file_parent_class)->finalize (object);
}

static void
extract_output (CdnOutputFile *f)
{
	GSList *vars;

	vars = cdn_object_get_variables (CDN_OBJECT (f));

	f->priv->outputs = g_ptr_array_sized_new (10);

	while (vars)
	{
		CdnVariable *v;

		v = vars->data;

		if (cdn_variable_get_flags (v) & CDN_VARIABLE_FLAG_OUT)
		{
			g_ptr_array_add (f->priv->outputs, v);
		}

		vars = g_slist_delete_link (vars, vars);
	}
}

static gboolean
write_header (CdnOutputFile  *output,
              GCancellable   *cancellable,
              GError        **error)
{
	GString *s;
	gboolean ret;
	gint i;

	if (!output->priv->header)
	{
		return TRUE;
	}

	s = g_string_sized_new (1024);

	g_string_append (s, "time");

	for (i = 0; i < output->priv->outputs->len; ++i)
	{
		CdnVariable *v;

		v = g_ptr_array_index (output->priv->outputs, i);

		g_string_append (s, output->priv->delimiter);
		g_string_append (s, cdn_variable_get_name (v));
	}

	g_string_append_c (s, '\n');

	ret = g_output_stream_write_all (output->priv->stream,
	                                 s->str,
	                                 s->len,
	                                 NULL,
	                                 cancellable,
	                                 error);

	g_string_free (s, TRUE);
	return ret;
}

static gboolean
cdn_output_file_initialize_impl (CdnIo         *io,
                                 GCancellable  *cancellable,
                                 GError       **error)
{
	CdnOutputFile *f;
	GFileOutputStream *stream;

	f = CDN_OUTPUT_FILE (io);

	extract_output (f);

	if (f->priv->file == NULL)
	{
		gchar *id;

		id = cdn_object_get_full_id_for_display (CDN_OBJECT (f));

		g_set_error (error,
		             CDN_NETWORK_LOAD_ERROR,
		             CDN_NETWORK_LOAD_ERROR_IO,
		             "No path was set for output file `%s'",
		             id);

		g_free (id);
		return FALSE;
	}

	stream = g_file_replace (f->priv->file,
	                         NULL,
	                         FALSE,
	                         G_FILE_CREATE_NONE,
	                         cancellable,
	                         error);

	if (!stream)
	{
		return FALSE;
	}

	f->priv->stream = G_OUTPUT_STREAM (stream);

	return write_header (f, cancellable, error);
}

static gboolean
cdn_output_file_finalize_impl (CdnIo         *io,
                               GCancellable  *cancellable,
                               GError       **error)
{
	gboolean ret = TRUE;
	CdnOutputFile *f;

	f = CDN_OUTPUT_FILE (io);

	g_ptr_array_free (f->priv->outputs, TRUE);
	f->priv->outputs = NULL;

	if (f->priv->stream)
	{
		ret = g_output_stream_flush (f->priv->stream,
		                             cancellable,
		                             error);

		if (ret)
		{
			ret = g_output_stream_close (f->priv->stream,
			                             cancellable,
			                             error);
		}

		g_object_unref (f->priv->stream);
		f->priv->stream = NULL;
	}

	return ret;
}

static void
cdn_output_file_update (CdnIo         *io,
                        CdnIntegrator *integrator)
{
	CdnOutputFile *output;
	gchar buffer[G_ASCII_DTOSTR_BUF_SIZE];
	gint i;

	output = CDN_OUTPUT_FILE (io);

	g_ascii_dtostr (buffer,
	                G_ASCII_DTOSTR_BUF_SIZE,
	                cdn_integrator_get_time (integrator));

	g_output_stream_write_all (output->priv->stream,
	                           buffer,
	                           strlen (buffer),
	                           NULL,
	                           NULL,
	                           NULL);

	for (i = 0; i < output->priv->outputs->len; ++i)
	{
		CdnVariable *v;

		v = g_ptr_array_index (output->priv->outputs, i);

		g_output_stream_write_all (output->priv->stream,
		                           output->priv->delimiter,
		                           strlen (output->priv->delimiter),
		                           NULL,
		                           NULL,
		                           NULL);

		g_ascii_dtostr (buffer,
		                G_ASCII_DTOSTR_BUF_SIZE,
		                cdn_variable_get_value (v));

		g_output_stream_write_all (output->priv->stream,
		                           buffer,
		                           strlen (buffer),
		                           NULL,
		                           NULL,
		                           NULL);
	}

	g_output_stream_write_all (output->priv->stream,
	                           "\n",
	                           1,
	                           NULL,
	                           NULL,
	                           NULL);
}

static void
cdn_io_iface_init (gpointer iface)
{
	CdnIoInterface *i = iface;

	i->update = cdn_output_file_update;

	i->initialize = cdn_output_file_initialize_impl;
	i->finalize = cdn_output_file_finalize_impl;
}

static CdnObject *
top_parent (CdnObject *o)
{
	CdnNode *p;

	p = cdn_object_get_parent (o);

	if (!p)
	{
		return o;
	}

	return top_parent (CDN_OBJECT (p));
}

static GFile *
resolve_path (CdnOutputFile *output,
              gchar const   *path)
{
	CdnObject *p;
	GFile *pf;
	gchar *cwd;
	GFile *f;

	if (!path)
	{
		return NULL;
	}

	if (g_path_is_absolute (path))
	{
		return g_file_new_for_path (path);
	}

	p = top_parent (CDN_OBJECT (output));

	if (CDN_IS_NETWORK (p))
	{
		pf = cdn_network_get_file (CDN_NETWORK (p));

		if (pf)
		{
			GFile *f;
			GFile *par;

			par = g_file_get_parent (pf);
			f = g_file_resolve_relative_path (par, path);

			g_object_unref (par);
			g_object_unref (pf);

			return f;
		}
	}

	cwd = g_get_current_dir ();

	pf = g_file_new_for_path (cwd);
	g_free (cwd);

	f = g_file_resolve_relative_path (pf, path);

	g_object_unref (pf);

	return f;
}

static void
set_path (CdnOutputFile *output,
          gchar const   *path)
{
	GFile *f = NULL;

	if (path != NULL)
	{
		f = resolve_path (output, path);
	}

	if (output->priv->file == NULL && f == NULL)
	{
		return;
	}

	if (output->priv->file && f && g_file_equal (output->priv->file, f))
	{
		g_object_unref (f);
		return;
	}

	if (output->priv->file)
	{
		g_object_unref (output->priv->file);
		output->priv->file = NULL;

		g_free (output->priv->path);
		output->priv->path = NULL;
	}

	if (f)
	{
		output->priv->file = g_file_dup (f);
		output->priv->path = g_strdup (path);
	}

	g_object_notify (G_OBJECT (output), "path");
}

static void
cdn_output_file_set_property (GObject      *object,
                              guint         prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
	CdnOutputFile *self = CDN_OUTPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_PATH:
			set_path (self, g_value_get_string (value));
			break;
		case PROP_MODE:
			self->priv->mode = g_value_get_flags (value);
			break;
		case PROP_HEADER:
			self->priv->header = g_value_get_boolean (value);
			break;
		case PROP_DELIMITER:
			g_free (self->priv->delimiter);
			self->priv->delimiter = g_value_dup_string (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_output_file_get_property (GObject    *object,
                              guint       prop_id,
                              GValue     *value,
                              GParamSpec *pspec)
{
	CdnOutputFile *self = CDN_OUTPUT_FILE (object);

	switch (prop_id)
	{
		case PROP_PATH:
			g_value_set_string (value, self->priv->path);
			break;
		case PROP_MODE:
			g_value_set_flags (value, self->priv->mode);
			break;
		case PROP_HEADER:
			g_value_set_boolean (value, self->priv->header);
			break;
		case PROP_DELIMITER:
			g_value_set_string (value, self->priv->delimiter);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_output_file_class_init (CdnOutputFileClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);

	object_class->finalize = cdn_output_file_finalize;

	object_class->get_property = cdn_output_file_get_property;
	object_class->set_property = cdn_output_file_set_property;

	g_type_class_add_private (object_class, sizeof(CdnOutputFilePrivate));

	g_object_class_install_property (object_class,
	                                 PROP_PATH,
	                                 g_param_spec_string ("path",
	                                                      "Path",
	                                                      "Path",
	                                                      NULL,
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS));

	g_object_class_install_property (object_class,
	                                 PROP_HEADER,
	                                 g_param_spec_boolean ("header",
	                                                       "Header",
	                                                       "Header",
	                                                       TRUE,
	                                                       G_PARAM_READWRITE |
	                                                       G_PARAM_STATIC_STRINGS |
	                                                       G_PARAM_CONSTRUCT));

	g_object_class_install_property (object_class,
	                                 PROP_DELIMITER,
	                                 g_param_spec_string ("delimiter",
	                                                      "Delimiter",
	                                                      "Delimiter",
	                                                      "\t",
	                                                      G_PARAM_READWRITE |
	                                                      G_PARAM_STATIC_STRINGS |
	                                                      G_PARAM_CONSTRUCT));

	g_object_class_override_property (object_class,
	                                  PROP_MODE,
	                                  "mode");
}

static void
cdn_output_file_class_finalize (CdnOutputFileClass *klass)
{
}

static void
cdn_output_file_init (CdnOutputFile *self)
{
	self->priv = CDN_OUTPUT_FILE_GET_PRIVATE (self);
}

void
cdn_output_file_register (GTypeModule *type_module)
{
	cdn_output_file_register_type (type_module);
}
