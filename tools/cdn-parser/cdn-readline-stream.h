/*
 * cdn-readline-stream.h
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

#ifndef __CDN_READLINE_STREAM_H__
#define __CDN_READLINE_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define CDN_TYPE_READLINE_STREAM		(cdn_readline_stream_get_type ())
#define CDN_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_READLINE_STREAM, CdnReadlineStream))
#define CDN_READLINE_STREAM_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CDN_TYPE_READLINE_STREAM, CdnReadlineStream const))
#define CDN_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CDN_TYPE_READLINE_STREAM, CdnReadlineStreamClass))
#define CDN_IS_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CDN_TYPE_READLINE_STREAM))
#define CDN_IS_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CDN_TYPE_READLINE_STREAM))
#define CDN_READLINE_STREAM_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CDN_TYPE_READLINE_STREAM, CdnReadlineStreamClass))

typedef struct _CdnReadlineStream		CdnReadlineStream;
typedef struct _CdnReadlineStreamClass		CdnReadlineStreamClass;
typedef struct _CdnReadlineStreamPrivate	CdnReadlineStreamPrivate;

struct _CdnReadlineStream
{
	/*< private >*/
	GInputStream parent;

	CdnReadlineStreamPrivate *priv;
};

struct _CdnReadlineStreamClass
{
	/*< private >*/
	GInputStreamClass parent_class;
};

GType cdn_readline_stream_get_type (void) G_GNUC_CONST;

GInputStream *cdn_readline_stream_new (gchar const *prompt);

G_END_DECLS

#endif /* __CDN_READLINE_STREAM_H__ */
