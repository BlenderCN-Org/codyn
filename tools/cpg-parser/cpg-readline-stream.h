/*
 * cpg-readline-stream.h
 * This file is part of cpg-network
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

#ifndef __CPG_READLINE_STREAM_H__
#define __CPG_READLINE_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define CPG_TYPE_READLINE_STREAM		(cpg_readline_stream_get_type ())
#define CPG_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStream))
#define CPG_READLINE_STREAM_CONST(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStream const))
#define CPG_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), CPG_TYPE_READLINE_STREAM, CpgReadlineStreamClass))
#define CPG_IS_READLINE_STREAM(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), CPG_TYPE_READLINE_STREAM))
#define CPG_IS_READLINE_STREAM_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), CPG_TYPE_READLINE_STREAM))
#define CPG_READLINE_STREAM_GET_CLASS(obj)	(G_TYPE_INSTANCE_GET_CLASS ((obj), CPG_TYPE_READLINE_STREAM, CpgReadlineStreamClass))

typedef struct _CpgReadlineStream		CpgReadlineStream;
typedef struct _CpgReadlineStreamClass		CpgReadlineStreamClass;
typedef struct _CpgReadlineStreamPrivate	CpgReadlineStreamPrivate;

struct _CpgReadlineStream
{
	/*< private >*/
	GInputStream parent;

	CpgReadlineStreamPrivate *priv;
};

struct _CpgReadlineStreamClass
{
	/*< private >*/
	GInputStreamClass parent_class;
};

GType cpg_readline_stream_get_type (void) G_GNUC_CONST;

GInputStream *cpg_readline_stream_new (gchar const *prompt);

G_END_DECLS

#endif /* __CPG_READLINE_STREAM_H__ */
