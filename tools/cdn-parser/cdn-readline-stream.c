/*
 * cdn-readline-stream.c
 * This file is part of codyn
 *
 * Copyright (C) 2011 - Jesse van den Kieboom
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, 
 * Boston, MA  02110-1301  USA
 */

#include "cdn-readline-stream.h"
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <string.h>

#define CDN_READLINE_STREAM_GET_PRIVATE(object)(G_TYPE_INSTANCE_GET_PRIVATE((object), CDN_TYPE_READLINE_STREAM, CdnReadlineStreamPrivate))

struct _CdnReadlineStreamPrivate
{
	gchar *prompt;
	GString *buffer;
	gchar *indentation;
};

G_DEFINE_TYPE (CdnReadlineStream, cdn_readline_stream, G_TYPE_INPUT_STREAM)

enum
{
	PROP_0,
	PROP_PROMPT
};

static void
cdn_readline_stream_finalize (GObject *object)
{
	CdnReadlineStream *stream;

	stream = CDN_READLINE_STREAM (object);

	g_free (stream->priv->prompt);
	g_string_free (stream->priv->buffer, TRUE);

	g_free (stream->priv->indentation);

	G_OBJECT_CLASS (cdn_readline_stream_parent_class)->finalize (object);
}

static gchar *
get_indentation (gchar const *s)
{
	gchar const *ptr = s;
	gchar const *end;
	gchar *ret;

	while (g_ascii_isspace (*ptr))
	{
		++ptr;
	}

	end = s + strlen (s) - 1;

	while (end > s && g_ascii_isspace (*end))
	{
		--end;
	}

	ret = g_strndup (s, ptr - s);

	if (*end == '{')
	{
		gchar *tmp;

		tmp = g_strconcat (ret, "    ", NULL);

		g_free (ret);
		ret = tmp;
	}

	return ret;
}

static gssize
input_readfn (GInputStream  *stream,
              void          *buffer,
              gsize          count,
              GCancellable  *cancellable,
              GError       **error)
{
	CdnReadlineStream *rl;

	rl = CDN_READLINE_STREAM (stream);

	if (rl->priv->buffer->len == 0)
	{
		while (TRUE)
		{
			gchar *ret;
			gchar const *ptr = rl->priv->indentation;

			while (*ptr)
			{
				rl_stuff_char (*ptr++);
			}

			ret = readline (rl->priv->prompt);

			if (ret == NULL)
			{
				break;
			}

			g_free (rl->priv->indentation);
			rl->priv->indentation = get_indentation (ret);

			if (!*ret)
			{
				g_free (ret);
				continue;
			}

			add_history (ret);

			g_string_append (rl->priv->buffer, ret);

			g_free (ret);
		}
	}

	if (rl->priv->buffer->len > 0)
	{
		gssize written;

		strncpy (buffer, rl->priv->buffer->str, count);

		if (rl->priv->buffer->len > count)
		{
			written = count;

			g_string_erase (rl->priv->buffer,
			                0,
			                rl->priv->buffer->len - count);
		}
		else
		{
			written = rl->priv->buffer->len;
			g_string_assign (rl->priv->buffer, "");
		}

		return written;
	}
	else
	{
		return 0;
	}
}

static void
cdn_readline_stream_set_property (GObject      *object,
                                  guint         prop_id,
                                  const GValue *value,
                                  GParamSpec   *pspec)
{
	CdnReadlineStream *self = CDN_READLINE_STREAM (object);

	switch (prop_id)
	{
		case PROP_PROMPT:
			g_free (self->priv->prompt);
			self->priv->prompt = g_value_dup_string (value);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_readline_stream_get_property (GObject    *object,
                                  guint       prop_id,
                                  GValue     *value,
                                  GParamSpec *pspec)
{
	CdnReadlineStream *self = CDN_READLINE_STREAM (object);

	switch (prop_id)
	{
		case PROP_PROMPT:
			g_value_set_string (value, self->priv->prompt);
			break;
		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
cdn_readline_stream_class_init (CdnReadlineStreamClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GInputStreamClass *stream_class = G_INPUT_STREAM_CLASS (klass);

	object_class->finalize = cdn_readline_stream_finalize;

	object_class->get_property = cdn_readline_stream_get_property;
	object_class->set_property = cdn_readline_stream_set_property;

	stream_class->read_fn = input_readfn;

	g_type_class_add_private (object_class, sizeof(CdnReadlineStreamPrivate));

	g_object_class_install_property (object_class,
	                                 PROP_PROMPT,
	                                 g_param_spec_string ("prompt",
	                                                      "Prompt",
	                                                      "Prompt",
	                                                      NULL,
	                                                      G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}

static int
handle_tab (int count, int key)
{
	rl_insert_text ("    ");
	return 0;
}

static int
startup_rl ()
{
	rl_bind_key ('\t', handle_tab);
	return 0;
}

static void
cdn_readline_stream_init (CdnReadlineStream *self)
{
	self->priv = CDN_READLINE_STREAM_GET_PRIVATE (self);

	self->priv->buffer = g_string_sized_new (255);

	rl_startup_hook = startup_rl;

	self->priv->indentation = g_strdup ("");
}

GInputStream *
cdn_readline_stream_new (gchar const *prompt)
{
	return g_object_new (CDN_TYPE_READLINE_STREAM,
	                     "prompt", prompt,
	                     NULL);
}
